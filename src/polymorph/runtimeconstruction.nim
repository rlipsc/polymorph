# SPDX-License-Identifier: Apache-2.0

# Copyright (c) 2020 Ryan Lipscombe
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import macros, private/[utils, ecsstatedb, statechangeutils, statechangegen, eventutils, mutationtracking]
import sharedtypes, tables, sets


type
  SystemReqSets* = Table[SystemIndex, tuple[req, neg: ComponentSet]]


proc allSysReq*(id: EcsIdentity): SystemReqSets =
  ## Precalculate sets for system requirements and negations.
  let
    allSys = id.allUnsealedSystems
  
  for index in allSys:
    result[index] = (
      req: id.ecsSysRequirements(index).toHashSet,
      neg: id.ecsSysNegations(index).toHashSet
    )


proc checkForEvent(id: EcsIdentity, eventContext: EventContext, eventKind: EventKind, contextEntity: NimNode): NimNode =
  ## Parses an entity for components to invoke `eventKind`.
  
  let
    entity = eventContext.entity
    fetched = genSym(nskLet, "fetched")

  var
    fetchStatement = nnkCall.newTree(ident "fetch", entity)
    componentsToFetch: seq[ComponentBuildInfo]
    conditionalExecs = newStmtList()

  for typeId in id.allUnsealedComponents:
    let
      c = getInfo(id, typeId)
      curContext = newEventContext(entity, c, nil)

    if id.read(eventKind, curContext).len > 0:
      componentsToFetch.add c
      fetchStatement.add ident(c.name)

  if componentsToFetch.len == 1:
    # Single component fetches return the instance directly.
    var
      c = componentsToFetch[0]
      event = newStmtList()
      curContext = newEventContext(entity, c, fetched)
      
    event.invokeEvent(id, curContext, eventKind)

    conditionalExecs.add(
      quote do:
        if `fetched`.valid:
          `event`
    )
  else:
    # Multiple component fetches return a tuple.
    for c in componentsToFetch:
      let
        compField = ident c.lcName
        fetchedInstance = newDotExpr(fetched, compField)
      var
        event = newStmtList()
        curContext = newEventContext(entity, c, fetchedInstance)
        
      event.invokeEvent(id, curContext, eventKind)
      
      assert event.len > 0

      conditionalExecs.add(quote do:
        if `fetchedInstance`.valid:
          `event`
      )

  if componentsToFetch.len > 0:
    let
      contextEntityAccess =
        if not contextEntity.isNil:
          # The context entity is only available for construct.
          let context = ident "context"
          quote do:
            let `context` {.inject, used.} = `contextEntity`
        else:
          newStmtList()

    quote do:
      block:
        `contextEntityAccess`
        let `fetched` = `fetchStatement`
        `conditionalExecs`
  else:
    newStmtList()


proc buildConstructionCaseStmt(
    id: EcsIdentity,
    sysReq: SystemReqSets,
    kind: StateChangeDetailsKind,
    entityIdent, sysProcessed, getTypeIdent, getCompIdx: NimNode,
    compAccess, compValue: ComponentAccessProc, compValid: ComponentValidProc): tuple[sysCase, eventCase: NimNode] =

  ## Build a case statement to handle updating systems linked to any component
  ## found in the input list.
  
  let
    allComponents = id.allUnsealedComponents
    suffix = sysProcessed.getSuffix

  result.sysCase = nnkCaseStmt.newTree(getTypeIdent)
  result.eventCase = nnkCaseStmt.newTree(getTypeIdent)

  proc compHasKey(typeInfo: ComponentBuildInfo, suffix: string): NimNode {.locks:0.} =
    # Output code to check the scanned types for the current type id.
    let id = newLit typeInfo.typeId.int
    quote do:
      types.hasKey(`id`)

  var
    deferredEvents = newStmtList()

  # Populate case statement with state changes for each component.
  for c in id.building allComponents:

    var details = newStateChangeDetails(kind, compAccess, compHasKey)    
    details.values = nnkBracket.newTree()
    details.passed = @[c.typeId]

    if c.isOwned:
      # Pass values for owned construction.
      details.values.add compValue(c, suffix)
      
      var required = id.inclDependents(sysReq[c.owner].req)
      required.excl c.typeId
      for owned in id.building(required):
        details.passed.add owned.typeId
        details.values.add compValue(owned, suffix)
    
    let instTy = c.instanceTy
    details.iterating = quote do:
      `instTy`(`getCompIdx`)
    
    details.passedSet = details.passed.toHashSet
    details.suffix = suffix

    details.sysProcessed = sysProcessed

    for sys in id.building(id.linked(c.typeId)):
      id.applyChange(entityIdent, details, SystemChange(
        kind: sckAdd,
        sys: sys.index,
        checkIncl: sysReq[sys.index].req,
        checkExcl: sysReq[sys.index].neg,
        )
      )
      
    # Generate code for adding this component.
    id.buildStateChange entityIdent, details

    let
      sysUpdates = details.systemUpdates
      postFetches = details.postFetches

    if details.systemUpdates.len > 0:
      result.sysCase.add nnkOfBranch.newTree(
        newLit c.typeId.int,
        quote do:
          `postFetches`     # Includes run time ownership checks.
          `sysUpdates`
      )
    
    if details.allEvents.len > 0:
      result.eventCase.add nnkOfBranch.newTree(
        newLit c.typeId.int,
        details.allEvents
      )

  let
    disc = nnkDiscardStmt.newTree(newEmptyNode())

  if result.eventCase.len > 1:
    result.eventCase.add nnkElse.newTree(disc)
  
  if result.sysCase.len > 1:
    result.sysCase.add nnkElse.newTree(disc)


proc makeRuntimeConstruction*(id: EcsIdentity): NimNode =
  ## Builds procedures for creating entities from component lists
  ## at run time.

  let
    cList = ident "componentList"
    construction = ident "construction"
    cloneEntity = ident "entity"
    amountParam = ident "amount"
    ctAmountParam = ident "amount"
    context = ident "context"
    contextEntity = ident "contextEntity" # Corrected to the current entity if context is not supplied.
    visitedConstruct = genSym(nskVar, "visited")
    visitedClone = genSym(nskVar, "visited")
    types = ident "types"
    compIndexInfo = ident "curCompInfo"

    reference = ident "reference"
    res = ident "result"
    componentStorageFormat = id.componentStorageFormat
    addToEnt = addComponentRef(res, reference, componentStorageFormat)
    tcIdent = ident typeClassName()
    iterVar = ident "i"
    component = ident "component"
    compIdx = ident "compIdx"
    entity = ident "entity"
    typeId = ident "typeId"
    callback = ident "callback"
    systemReqSets = id.allSysReq() # Precalculate system requirements.

  # These procedures change the 'read' and 'exists' operations for
  # 'newStateChangeDetails' to access the 'types' table.
  
  proc compAccessConstruct(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
    let
      typeId = newLit typeInfo.typeId.int
      instTy = typeInfo.instanceTy
    quote do:
      `instTy`(`types`[`typeId`][1])

  proc compValueConstruct(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
    let
      typeId = newLit typeInfo.typeId.int
      refTy = ident refTypeName(typeInfo.name)
    quote do:
      `refTy`(`types`[`typeId`][0]).value

  proc compHasKey(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
    let
      typeId = newLit typeInfo.typeId.int
    quote do:
      `types`.hasKey `typeId`

  proc compAccessClone(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
    let
      typeId = newLit typeInfo.typeId.int
      instTy = typeInfo.instanceTy
    quote do:
      `instTy`(`types`[`typeId`])

  proc compValueClone(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
    compAccessClone(typeInfo, suffix).newDotExpr ident"access"

  # Create case statements for construct and clone.

  let
    sysSet = ident systemsEnumName()

  var
    entId = quote do: `entity`.entityId
    entCompCount = componentRefsLen(entId, componentStorageFormat)
    entContext = newEventContext(res) # Entity events.
  
  # Find component type bounds for the current set of unsealed components.

  var
    lowCompId = id.ecsComponentsToBeSealed[^1].int
    highCompId: int

  for typeId in id.unsealedComponents:
    if typeId.int < lowCompId:
      lowCompId = typeId.int
    if typeId.int > highCompId:
      highCompId = typeId.int


  result = newStmtList()

  # Utilities are added first for constructors to use.

  result.add(quote do:
    proc toTemplate*(`entity`: EntityRef): seq[Component] =
      ## Creates a list of components ready to be used for construction.
      assert `entity`.alive
      let length = `entCompCount`
      `res` = newSeq[Component](length)
      for i, compRef in `entity`.entityId.pairs:
        caseComponent(compRef.typeId):
          `res`[i] = componentInstanceType()(compRef.index).makeContainer()

    proc toTemplate*(entities: Entities): ConstructionTemplate =
      result.setLen entities.len
      for i, e in entities:
        result[i] = e.toTemplate
  )

  let
    disabledOps = id.getDisabledOps
    useDynamicConstruction = id.runtimeConstructionHooks

  # Add forward declarations to allow calling constructs and clones together.

  if not(edoClone in disabledOps):
    result.add(quote do:
      proc clone*(`cloneEntity`: EntityRef): EntityRef
    )
  if not(edoConstruct in disabledOps):
    result.add(quote do:
      proc construct*(`construction`: ComponentList, `amountParam`: int, `context` = NO_ENTITY_REF): seq[EntityRef] {.discardable.}
      proc construct*(`construction`: ConstructionTemplate): seq[EntityRef]
    )

  # Build

  if not(edoConstruct in disabledOps):
    let
      # Construct's case statement reads parsed info from a ComponentList.
      constructCaseInfo = id.buildConstructionCaseStmt(
        systemReqSets,
        scdkConstruct,
        res,
        visitedConstruct,
        quote do:
          `compIndexInfo`[1].`component`.typeId.int,
        quote do:
          `compIndexInfo`[1].`compIdx`,
        compAccessConstruct,
        compValueConstruct,
        compHasKey,
      )

      # Build the core construct loop.
      constructCase = constructCaseInfo.sysCase
      constructLoop =
        if constructCase.len > 1:
          quote do:
            for `compIndexInfo` in `types`.pairs:
              `constructCase`
            `visitedConstruct` = {}
        else:
          newStmtList()

      eventCase = constructCaseInfo.eventCase

      curComp = ident "curComp"

      # defaultAdd updates the entity with the component type given in the component list.
      defaultConstructAdd = quote do:
        when owningSystemIndex == InvalidSystemIndex:
          `reference` = newInstance(componentRefType()(`curComp`).value).toRef            
        else:
          # This reference isn't valid until we add the item row in the second pass.
          let
            c = owningSystem.count
            nextGen = 
              if c < componentGenerations().len:
                (componentGenerations()[c].int + 1).ComponentGeneration
              else:
                1.ComponentGeneration
          
          `reference` = (
            componentId,
            owningSystem.count.ComponentIndex,
            nextGen
          )
        `addToEnt`
        `types`[`curComp`.typeId.int] = (`curComp`, `reference`.index)

      # Names of the arrays holding the different construction callbacks.
      manualConstruct = ident "manualConstruct"
      postConstruct = ident "postConstruct"

      constructInner =
        if useDynamicConstruction:
          ## Allow run time hooks to dynamically transform component lists during construction.
          quote do:
            # trigger construction callback if present
            let cb = `manualConstruct`[`curComp`.typeId.int]
            if cb != nil:
              # The callback is responsible for adding reference(s) from the source entity.
              # Callbacks may add multiple sets of components or none at all.
              let compsAdded = cb(`res`, `curComp`, `contextEntity`)
              # We must now include manually added components to the index so
              # that systems are updated in the second stage below.
              # It is still possible to add owned components, and their
              # dependencies will be checked in the second pass.

              # We now have to work out which types have been returned.
              for comp in compsAdded:
                assert comp.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $comp.typeId.int
                assert not `types`.hasKey(comp.typeId.int), "Cannot construct: Entity has duplicate components for " & $comp.typeId
                caseComponent comp.typeId:
                  when owningSystemIndex == InvalidSystemIndex:
                    `reference` = newInstance(componentRefType()(comp).value).toRef
                  else:
                    # This reference isn't valid until we add the item row in the second pass.
                    let
                      c = owningSystem.count
                      nextGen = 
                        if c < componentGenerations().len:
                          (componentGenerations()[c].int + 1).ComponentGeneration
                        else:
                          1.ComponentGeneration

                    `reference` = (
                      componentId,
                      owningSystem.count.ComponentIndex,
                      nextGen
                    )
                  `addToEnt`
                  `types`[comp.typeId.int] = (comp, `reference`.index)
            else:
              # No hooks registered for this component.
              `defaultConstructAdd`
        else:
          # No dynamic hooks.
          defaultConstructAdd


    var constructEvents = newStmtList()

    # Build a loop to process user construct events.
    if eventCase.len > 1:
      constructEvents.add(
        quote do:
          for `compIndexInfo` in `types`.pairs:
            `eventCase`
      )

    # Construct events.

    constructEvents.invokeEvent(id, entContext, ekEntityConstruct)
    if constructEvents.len > 0:
      constructEvents.trackMutation(id, ekEntityConstruct, id.allUnsealedComponents, announce = false)

    constructEvents.invokeEvent(id, entContext, ekConstruct)
    constructEvents.add id.checkForEvent(entContext, ekConstructComp, contextEntity)

    var
      bindConstructs = newStmtList()
      curEnt = genSym(nskLet, "entity")
      bindContext = newEventContext(curEnt)

    let
      runBindEvents = id.checkForEvent(bindContext, ekBindConstruct, contextEntity)
    
    # TODO: use 'types' instead of reparsing the entity? May be less resilient to self modification.
    if runBindEvents.len > 0:
      bindConstructs.add(quote do:
        block:
          template entities: Entities {.used.} = `res`
          var i: int
          while i < `res`.len:
            let `curEnt` = `res`[i]
            `runBindEvents`
            i.inc
      )

    if useDynamicConstruction:
      # Add variables and utilities for run time dynamic construction hooks.
      result.add(
        quote do:
          var
            # Note: As zero is an invalid component, a component count of eg 5 indicates valid indices of 0..5, not 0..4.
            `manualConstruct`: array[`lowCompId`..`highCompId`, ConstructorProc]
            # Post constructors are called after an entity template has been constructed.
            `postConstruct`: array[`lowCompId`..`highCompId`, PostConstructorProc]

          # Note: do not rely on the order a callback is invoked when constructing templates.
          proc registerConstructor*(`typeId`: ComponentTypeId, `callback`: ConstructorProc) = `manualConstruct`[`typeId`.int] = `callback`
          template registerConstructor*(t: typedesc[`tcIdent`], `callback`: ConstructorProc) = registerConstructor(t.typeId, `callback`)
          proc registerPostConstructor*(`typeId`: ComponentTypeId, `callback`: PostConstructorProc) = `postConstruct`[`typeId`.int] = `callback`
          template registerPostConstructor*(t: typedesc[`tcIdent`], `callback`: PostConstructorProc) = registerPostConstructor(t.typeId, `callback`)
      )
      # Invoke post-construction callbacks.
      bindConstructs.add(
        quote do:
          # Re-parse components to check for and activate post construction callbacks.
          # This post-step allows the user to do any multi-entity work that may be required
          # such as physics linkage and so on.
          # You can add/remove components within callbacks here.
          var `iterVar`: int
          while `iterVar` < `res`.len:
            let `entity` = `res`[`iterVar`]
            `iterVar` += 1

            for compRef in `entity`:
              let
                tId = compRef.typeId
                pc = `postConstruct`[tId.int]
              
              if pc != nil:
                # Callback passes this entity and the fully constructed result array.
                pc(`entity`, compRef, `res`)
      )

    else:
      # Assert failure if run time hooks are used.

      let cannotHook = quote do:
        assert false, "This ECS doesn't include run time construction hooking - enable by generating with 'ECSEntityOptions.runtimeConstructionHooks = true'"

      result.add(
        quote do:
          proc registerConstructor*(`typeId`: ComponentTypeId, `callback`: ConstructorProc) = `cannotHook`
          template registerConstructor*(t: typedesc[`tcIdent`], `callback`: ConstructorProc) = `cannotHook`
          proc registerPostConstructor*(`typeId`: ComponentTypeId, `callback`: PostConstructorProc) = `cannotHook`
          template registerPostConstructor*(t: typedesc[`tcIdent`], `callback`: PostConstructorProc) = `cannotHook`
      )

    # Emit construct procedures.

    result.add(quote do:

      proc construct*(`cList`: ComponentList, `context`: EntityRef = NO_ENTITY_REF): EntityRef =
        ## Create a runtime entity from a list of components.
        ## 
        ## The user may use `registerCallback` to control construction of
        ## particular types.
        ## 
        ## `context` contains the constructing entity when not passed.
        ## 
        ## When this called from a `ConstructionTemplate`, `context` will
        ## be the first entity to be constructed.

        static: startOperation(`id`, "construct")
        `res` = newEntity()
        let
          `contextEntity` =
            if `context`.entityId != NO_ENTITY:
              `context`
            else:
              `res`
        
        # Build an index to component list, which helps speed things
        # up when checking systems.
        var
          `types`: Table[int, tuple[`component`: Component, `compIdx`: ComponentIndex]]
          `visitedConstruct` {.used.}: set[`sysSet`]

        for `curComp` in `cList`:
          assert `curComp`.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $`curComp`.typeId.int
          assert not `types`.hasKey(`curComp`.typeId.int), "Cannot construct: Entity has duplicate components for " & $`curComp`.typeId
          var `reference`: ComponentRef

          # Index and unowned component generation.
          caseComponent `curComp`.typeId:
            `constructInner`

        `constructLoop`
        `constructEvents`

        static:
          endOperation(`id`)


      proc construct*(`construction`: ComponentList, `amountParam`: int, `context` = NO_ENTITY_REF): seq[EntityRef] {.discardable.} =
        for i in 0 ..< `amountParam`:
          result.add construct(`construction`, `context`)


      proc construct*(`construction`: ConstructionTemplate): seq[EntityRef] =
        ## Constructs multiple entities and returns their entity ids.
        ## 
        ## The first entity in the list is passed to the others as the `context`.
        ## This same entity is also passed to each individual component's constructor,
        ## this allows components to have some reference to their construction environment.
        ## 
        ## For example, the first entity can contain a physics body component that others may
        ## reference.
        ## 
        ## No other structure is assumed, and the meaning of `context` is defined by the user.
        ## Components are constructed in order, calling manual construction code per type,
        ## then a second pass calls post construction procs with reference to the completed component
        ## lists.
        ## 
        ## Post construction procs are fed the fully constructed entity and its existing component,
        ## along with the rest of the constructed entities in this template.
        ## This allows fetching components to read/modify initialised values.
        if `construction`.len > 0:
          result.setLen(`construction`.len)
          result[0] = `construction`[0].construct(NO_ENTITY_REF)

          for `iterVar` in 1 ..< `construction`.len:
            result[`iterVar`] = `construction`[`iterVar`].construct(result[0])
    
          let `contextEntity` {.inject, used.} = result[0]
          
          `bindConstructs`


      proc construct*(`construction`: ConstructionTemplate, `ctAmountParam`: int): seq[Entities] {.discardable.} =
        result.setLen `ctAmountParam`
        for i in 0 ..< `ctAmountParam`:
          result[i] = construct(`construction`)

      )
  
  if not(edoClone in disabledOps):
    let
      # Clone's case statement reads parsed info from an existing entity.
      cloneCaseInfo = id.buildConstructionCaseStmt(
        systemReqSets,
        scdkClone,
        res,
        visitedClone,
        quote do: `compIndexInfo`[0],
        quote do: `compIndexInfo`[1],
        compAccessClone,
        compValueClone,
        compHasKey
      )
      cloneCase = cloneCaseInfo.sysCase
      userCloneEvents = cloneCaseInfo.eventCase
      cloneConstruct = ident "cloneConstruct"
      curComp = ident "curComp"

      # Build the core clone loop.
      cloneLoop =
        if cloneCase.len > 1:
          quote do:
            for `compIndexInfo` in `types`.pairs:
              `cloneCase`
        else:
          newStmtList()

      defaultCloneAdd = quote do:
        when owningSystemIndex == InvalidSystemIndex:
          `reference` = newInstance(componentInstanceType()(`curComp`.index).access).toRef
        else:
          # This reference isn't valid until we add the item row in the second pass.
          `reference` = `curComp`
        `addToEnt`
        `types`[`curComp`.typeId.int] = `reference`.index

      cloneInner =
        if useDynamicConstruction:
          quote do:
            # trigger construction callback if present
            let cb = `cloneConstruct`[`curComp`.typeId.int]
            if cb != nil:
              # The callback is responsible for adding reference(s) from the source entity.
              # Callbacks may add multiple sets of components or none at all.
              let compsAdded = cb(`res`, `curComp`)
              # We must now include manually added components to the index so
              # that systems are updated in the second stage below.
              # It is still possible to add owned components, and their
              # dependencies will be checked in the second pass.

              # We now have to work out which types have been returned.
              for comp in compsAdded:
                assert comp.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $comp.typeId.int
                assert not `types`.hasKey(comp.typeId.int), "Cannot construct: Entity has duplicate components for " & $comp.typeId
                caseComponent comp.typeId:
                  when owningSystemIndex == InvalidSystemIndex:
                    `reference` = newInstance(componentRefType()(comp).value).toRef
                  else:
                    # This reference isn't valid until we add the item row in the second pass.
                    `reference` = (
                      componentId,
                      owningSystem.count.ComponentIndex,
                      1.ComponentGeneration # TODO: Update generation.
                    )

                  `addToEnt`
                  `types`[comp.typeId.int] = `reference`.index
            else:
              `defaultCloneAdd`
        else:
          # No dynamic hooks.
          defaultCloneAdd

    # Create clone events.

    var cloneEvents = newStmtList()
    cloneEvents.invokeEvent(id, entContext, ekEntityClone)
    if cloneEvents.len > 0:
      cloneEvents.trackMutation(id, ekEntityClone, id.allUnsealedComponents, announce = false)
    cloneEvents.invokeEvent(id, entContext, ekClone)
    cloneEvents.add id.checkForEvent(entContext, ekCloneComp, nil)

    # Build a loop to process user clone events.
    if userCloneEvents.len > 1:
      cloneEvents.add(
        quote do:
          for `compIndexInfo` in `types`.pairs:
            `userCloneEvents`
      )

    # Emit clone procedure.

    result.add(quote do:
      var
        # Clone constructors allow custom handling of components by type when `clone` is called on an entity.
        `cloneConstruct`: array[`lowCompId`..`highCompId`, CloneConstructorProc]

      proc registerCloneConstructor*(`typeId`: ComponentTypeId, `callback`: CloneConstructorProc) = `cloneConstruct`[`typeId`.int] = `callback`
      template registerCloneConstructor*(t: typedesc[`tcIdent`], `callback`: CloneConstructorProc) = registerCloneConstructor(t.typeId, `callback`)

      proc clone*(`cloneEntity`: EntityRef): EntityRef =
        ## Copy an entity's components to a new entity.
        ## 
        ## Note that copying objects with pointers/references can have undesirable results.
        ## 
        ## For special setup, use `registerCloneConstructor` for the type. This gets passed
        ## the clone type it would have added.
        ## 
        ## You can then add a modified component or entirely different set
        ## of components, or ignore it by not adding anything.

        let `entity` = `cloneEntity`
        assert `entity`.alive, "Cloning a dead entity"
        static: startOperation(`id`, "clone")

        `res` = newEntity()
        # Build an index of component type to its instantiated instance.
        var
          `types`: Table[int, ComponentIndex]
          `visitedClone` {.used.}: set[`sysSet`]

        for `curComp` in `entity`.components:
          assert `curComp`.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $`curComp`.typeId.int
          assert not `types`.hasKey(`curComp`.typeId.int), "Cannot construct: Entity has duplicate components for " & $`curComp`.typeId

          var `reference`: ComponentRef

          # Index and unowned component generation.
          caseComponent `curComp`.typeId:
            `cloneInner`

        # Update systems.
        `cloneLoop`
        `cloneEvents`
        static: endOperation(`id`)
    )
