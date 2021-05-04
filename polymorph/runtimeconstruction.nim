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

import macros, private/[utils, ecsstatedb], sharedtypes, tables
from strutils import toLowerAscii

proc buildConstructionCaseStmt(id: EcsIdentity, entity: NimNode, cloning: bool): tuple[core, userDecls, userCode: NimNode] =
  ## Build a case statement to handle updating systems linked to any component
  ## found in the input list.
  let
    compIndexInfo = ident "curCompInfo"
    visited = ident "visited"
    types = ident "types"

  var
    compCase = nnkCaseStmt.newTree()
    userCode = newStmtList()
    userCodeBoolDecls: Table[SystemIndex, NimNode]

  # Select on typeId.
  compCase.add(quote do: `compIndexInfo`[0].int)

  for typeId in id.unsealedComponents:

    let
      linkedSystems = id.linked typeId
      typeName = id.typeName(typeId)
      ofInstType = ident typeName.instanceTypeName
    var
      addToSystems = newStmtList()
      ofBranch = nnkOfBranch.newTree()
      ofStmts = newStmtList()
      onAddCallback = id.onAddCallbackNode(typeId)
      onAddToEntCode = id.onAddToEntCodeNode(typeId)
      
      userAddComponent = newStmtList()

    ofBranch.add newLit typeId.int

    let
      source = 
        if cloning:
          quote do: `compIndexInfo`[1]
        else:
          quote do: `compIndexInfo`[1][1]
      typeOwner = id.systemOwner(typeId)

    # User callback.
    if onAddCallback.len > 0:
      # Check for this type's initialiser.
      let cbProcName = ident addCallbackName(typeName)
      userAddComponent.add(quote do:
        `cbProcName`(`entity`, `ofInstType`(`source`))
      )

    if typeOwner == InvalidSystemIndex:
      # TODO: This needs to be removed to a collected event processing proc.
      # Component add user code.
      if onAddToEntCode.len > 0:
        userAddComponent.add(quote do:
          block:
            ## Current component being added to entity. 
            template curComponent: untyped {.used.} = `ofInstType`(`source`)
            `onAddToEntCode`
        )

    for sys in linkedSystems:
      let
        requiredCompsLen = id.len_ecsSysRequirements sys
        systemStr = id.getSystemName sys
        sysTupleType = ident tupleName(systemStr)
        sysTupleVar = ident tupleName(systemStr).toLowerAscii
        systemNode = id.instantiation sys
        newRow = ident "newRow"
        # This is recorded in the match block and used for user events.
        sysUserCodeIdx = ident "run" & systemStr & "UserCodeIdx"

      var
        matchList = newSeqOfCap[NimNode](requiredCompsLen)
        sysFields = newStmtList()
        stateUpdates = newStmtList()
        assertions = newStmtList()
        userSysAddCode = newStmtList()

      if requiredCompsLen > 0:
        # Generate code to check the system is satisfied,
        # and code to do the assignment of the tuple elements.

        for comp in id.ecsSysRequirements(sys):
          let
            typeStr = id.typeName(comp)
            typeField = ident typeStr.toLowerAscii
            instType = ident typeStr.instanceTypeName
            refType = ident typeStr.refTypeName
            ownedByThisSystem = id.systemOwner(comp) == sys
            neededByOwnedSystem = typeOwner == sys and comp != typeId
            compId = comp.int

          if neededByOwnedSystem and not cloning:
            # It's a run time error to not specify all the owned components.
            # We can elide the check for cloned entities since existing
            # entities must already have satisfied owner systems.
            assertions.add quote do:
              assert `types`.hasKey(`comp`),
                "Cannot construct: Specified owned component \"" & `typeName` &
                "\" also needs owned component \"" & `typeStr` & "\""
          
          if comp != typeId:
            # Fragment of if statement clause.
            matchList.add quote do:
              `types`.hasKey(`comp`)

          let source =
            if comp == typeId:
              quote do: `compIndexInfo`[1]
            else:
              quote do: `types`[`compId`]

          id.addUserCompAddToSys(userSysAddCode, entity, sys, comp, quote do:
            `systemNode`.groups[`sysUserCodeIdx`].`typeField`)

          # Assignment of the tuple field for this component.
          if not cloning:
            # Construct.
            if ownedByThisSystem:
              # Owned fields also need their state initialised.
              stateUpdates.add updateOwnedComponentState(id, comp, sys, newRow)
              # Fetch any user init for owned types.
              let
                typeAccess = quote do:
                  `refType`(`source`[0]).value
                (ownedOnAddEvent, ownedUserInitEvent, ownedUpdateEvent) =
                  id.ownedUserInitEvents(sys, sysTupleVar, typeAccess, comp, entity, typeAccess)
              sysFields.add(quote do:
                `sysTupleVar`.`typeField` = `typeAccess`)
              if ownedByThisSystem:
                sysFields.add(quote do:
                  `ownedOnAddEvent`
                  `ownedUserInitEvent`
                  `ownedUpdateEvent`)
            else:
              # We already have the Instance type stored for unowned components.
              sysFields.add(quote do:
                `sysTupleVar`.`typeField` = `instType`(`source`[1]))
          
          else:
            # Clone.
            if ownedByThisSystem:
              # Owned fields also need their state initialised.
              stateUpdates.add updateOwnedComponentState(id, comp, sys, newRow)
              # Fetch any user init for owned types.
              let
                typeInst = newCall(instType, source)
                typeAccess = newDotExpr(typeInst, ident "access")
                (ownedOnAddEvent, ownedUserInitEvent, ownedUpdateEvent) =
                  id.ownedUserInitEvents(sys, sysTupleVar, typeInst, comp, entity, typeAccess)
              sysFields.add(quote do:
                `sysTupleVar`.`typeField` = `typeAccess`)
              if ownedByThisSystem:
                sysFields.add(quote do:
                  `ownedOnAddEvent`
                  `ownedUserInitEvent`
                  `ownedUpdateEvent`)
            else:
              # We already have the Instance type stored for unowned components.
              sysFields.add(quote do:
                `sysTupleVar`.`typeField` = `instType`(`source`))

      let
        sysOpts = id.getOptions sys
        updateIndex =id.updateIndex(entity, sys, newRow)
        addToSystem = addToSystemTuple(systemNode, sysTupleVar, sysOpts)
        hasUserAddToSysCode = userSysAddCode.len > 0
        hasUserAddedCode = id.len_onAdded(sys) > 0
        hasUserSystemCode = hasUserAddToSysCode or hasUserAddedCode

        setUserCodeIdx =
          if hasUserSystemCode: quote do:
            `sysUserCodeIdx` = `newRow`
          else: newStmtList()
        
        matchCode = quote do:
          if not `visited`[`sys`.int]:
            `visited`[`sys`.int] = true
            var `sysTupleVar`: `sysTupleType`
            `sysTupleVar`.entity = `entity`
            `sysFields`
            `addToSystem`
            let `newRow` = `systemNode`.high
            `updateIndex`
            `stateUpdates`
            `setUserCodeIdx`

      if hasUserSystemCode and sys notin userCodeBoolDecls:
        # Both events for adding to systems and the `added:` block within a system are invoked
        # in their own block, after state construction.
        userCodeBoolDecls[sys] = newIdentDefs(sysUserCodeIdx, ident "int", newLit -1)

        let addedEvent = id.userSysAdded(sys, sysUserCodeIdx)

        userCode.add(quote do:
          if `sysUserCodeIdx` >= 0:
            `addedEvent`
            `userSysAddCode`
        )

      addToSystems.add assertions

      if matchList.len > 0:
        # TODO: Pare down checks so only one hash per system set.
        # Wrap matchCode with if statement so we only add when the system is satisfied.
        let sysMatchClause = genInfixes(matchList, "and")
        addToSystems.add(newIfStmt( (sysMatchClause, matchCode) ))
        
      else:
        # No conditions, always add to this system.
        addToSystems.add matchCode
    
    let
      hasSystems = addToSystems.len > 0
      hasUserComponentEvents = userAddComponent.len > 0

    # Only create an of branch for components with events.
    if hasSystems or hasUserComponentEvents:
      if hasSystems:
        ofStmts.add addToSystems
      if hasUserComponentEvents:
        ofStmts.add userAddComponent
      ofBranch.add ofStmts
      compCase.add ofBranch

  compCase.add nnkElse.newTree(quote do: discard)

  var decls = nnkVarSection.newTree()
  for node in userCodeBoolDecls.values:
    decls.add node

  (compCase, decls, userCode)

proc makeRuntimeConstruction*(id: EcsIdentity): NimNode =
  ## Builds procedures for creating entities from component lists
  ## at run time.
  let
    cList = ident "componentList"
    construction = ident "construction"
    master = ident "master"
    visited = ident "visited"
    types = ident "types"
    compIndexInfo = ident "curCompInfo"

    maxSys = id.len_systems
    reference = ident "reference"
    res = ident "result"
    entOpts = id.entityOptions
    addToEnt = addComponentRef(res, reference, entOpts)
    tcIdent = ident typeClassName()
    iterVar = ident "i"
    component = ident "component"
    compIdx = ident "compIdx"
    entity = ident "entity"
    typeId = ident "typeId"
    callback = ident "callback"

    constructCaseInfo = buildConstructionCaseStmt(id, res, false)
    userConstructEvents = constructCaseInfo.userCode
    userDecls = constructCaseInfo.userDecls
    constructCase = constructCaseInfo.core

    cloneCaseInfo = buildConstructionCaseStmt(id, res, true)
    cloneCase = cloneCaseInfo.core
    userCloneEvents = cloneCaseInfo.userCode
    userCloneDecls = constructCaseInfo.userDecls

    entId = quote do: `entity`.entityId
    entCompCount = componentRefsLen(entId, entOpts)

    curEntTempl = 
      quote do:
        template curEntity: EntityRef {.used.} = `res`

  # Find component type bounds for the current set of components.
  var
    lowCompId = id.ecsComponentsToBeSealed[^1].int
    highCompId: int

  for typeId in id.unsealedComponents:

    if typeId.int < lowCompId:
      lowCompId = typeId.int
    
    if typeId.int > highCompId:
      highCompId = typeId.int

  let
    userConstructStateChangeEvent = id.userStateChange(res, eceConstruct)
    userCloneStateChangeEvent = id.userStateChange(res, eceClone)

    # Names of the arrays holding the different construction callbacks.
    manualConstruct = ident "manualConstruct"
    postConstruct = ident "postConstruct"
    cloneConstruct = ident "cloneConstruct"

    postConstruction =
      case entOpts.componentStorageFormat
      of csSeq, csArray:
        quote do:
          var compIdx: int
          while compIdx < `entCompCount`:
            let
              compRef = entityData(`entId`).componentRefs[compIdx]
              tId = compRef.typeId
              pc = `postConstruct`[tId.int]
            if pc != nil:
              # Callback passes this entity and the fully constructed result array.
              pc(`entity`, compRef, `res`)
            compIdx += 1
      of csTable:
        quote do:
          for compRef in `entity`:
            let
              tId = compRef.typeId
              pc = `postConstruct`[tId.int]
            if pc != nil:
              # Callback passes this entity and the fully constructed result array.
              pc(`entity`, compRef, `res`)

  result = quote do:
    var
      # Note: As zero is an invalid component, a component count of eg 5 indicates valid indices of 0..5, not 0..4.
      `manualConstruct`: array[`lowCompId`..`highCompId`, ConstructorProc]
      # Post constructors are called after an entity template has been constructed.
      `postConstruct`: array[`lowCompId`..`highCompId`, PostConstructorProc]
      # Clone constructors allow custom handling of components by type when `clone` is called on an entity.
      `cloneConstruct`: array[`lowCompId`..`highCompId`, CloneConstructorProc]

    # Do not rely on the order a callback is invoked when constructing templates

    proc registerConstructor*(`typeId`: ComponentTypeId, `callback`: ConstructorProc) = `manualConstruct`[`typeId`.int] = `callback`
    template registerConstructor*(t: typedesc[`tcIdent`], `callback`: ConstructorProc) = registerConstructor(t.typeId, `callback`)

    proc registerPostConstructor*(`typeId`: ComponentTypeId, `callback`: PostConstructorProc) = `postConstruct`[`typeId`.int] = `callback`
    template registerPostConstructor*(t: typedesc[`tcIdent`], `callback`: PostConstructorProc) = registerPostConstructor(t.typeId, `callback`)

    proc registerCloneConstructor*(`typeId`: ComponentTypeId, `callback`: CloneConstructorProc) = `cloneConstruct`[`typeId`.int] = `callback`
    template registerCloneConstructor*(t: typedesc[`tcIdent`], `callback`: CloneConstructorProc) = registerCloneConstructor(t.typeId, `callback`)

    proc construct*(`cList`: ComponentList, `master`: EntityRef = NO_ENTITY_REF): EntityRef =
      ## Create a runtime entity from a list of components.
      ## The user may use `registerCallback` to control construction of particular types.
      ## When called from a `ConstructionList`, `master` is set to the first entity constructed.

      static: startOperation(EcsIdentity(`id`), "construct")
      `res` = newEntity()
      # master defaults to the current entity if nothing specified
      let
        masterRef = if `master`.entityId != NO_ENTITY:
          `master`
        else:
          `res`
      # Build an index to component list, which helps speed things
      # up when checking systems.
      var
        `types`: Table[int, tuple[`component`: Component, `compIdx`: ComponentIndex]]
        `visited`: array[`maxSys`, bool]
      `curEntTempl`

      for compRef in `cList`:
        assert compRef.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $compRef.typeId.int
        assert not `types`.hasKey(compRef.typeId.int), "Cannot construct: Entity has duplicate components for " & $compRef.typeId
        var `reference`: ComponentRef

        # Index and unowned component generation.
        caseComponent compRef.typeId:
          # trigger construction callback if present
          let cb = `manualConstruct`[compRef.typeId.int]
          if cb != nil:
            # The callback is responsible for adding reference(s) from the source entity.
            # Callbacks may add multiple sets of components or none at all.
            let compsAdded = cb(`res`, compRef, masterRef)
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
                  # This reference isn't valid until we add the tuple in the second pass.
                  `reference` = (
                    componentId(),
                    owningSystem.count.ComponentIndex,
                    1.ComponentGeneration
                  )
                `addToEnt`
                `types`[comp.typeId.int] = (comp, `reference`.index)
          else:
            when owningSystemIndex == InvalidSystemIndex:
              `reference` = newInstance(componentRefType()(compRef).value).toRef            
            else:
              # This reference isn't valid until we add the tuple in the second pass.
              `reference` = (
                componentId(),
                owningSystem.count.ComponentIndex,
                1.ComponentGeneration
              )
            `addToEnt`
            `types`[compRef.typeId.int] = (compRef, `reference`.index)

      # Update systems.
      `userDecls`
      for `compIndexInfo` in `types`.pairs:
        `constructCase`
      `userConstructEvents`
      `userConstructStateChangeEvent`
      static: endOperation(EcsIdentity(`id`))

    proc construct*(`construction`: ConstructionTemplate): seq[EntityRef] =
      ## Constructs multiple entities and returns their entity ids.
      ## The first entity in the list is passed to the others as the "master".
      ## This same entity is also passed to each individual component's constructor,
      ## this allows components to have some reference to their construction environment.
      ## For example, the first entity can contain a physics body component that others may
      ## reference.
      ## No other structure is assumed, and the meaning of 'master' is defined by the user.
      ## Components are constructed in order, calling manual construction code per type,
      ## then a second pass calls post construction procs with reference to the completed component
      ## lists.
      ## Post construction procs are fed the fully constructed entity and its existing component,
      ## along with the rest of the constructed entities in this template.
      ## This allows fetching components to read/modify initialised values.
      if `construction`.len > 0:
        result.setLen(`construction`.len)
        result[0] = `construction`[0].construct(NO_ENTITY_REF)

        for `iterVar` in 1 ..< `construction`.len:
          result[`iterVar`] = `construction`[`iterVar`].construct(result[0])

        # Re-parse components to check for and activate post construction callbacks.
        # This post-step allows the user to do any multi-entity work that may be required
        # such as physics linkage and so on.
        # You can add/remove components within callbacks here.
        var `iterVar`: int
        while `iterVar` < `res`.len:
          let `entity` = `res`[`iterVar`]
          `postConstruction`
          `iterVar` += 1

    proc toTemplate*(`entity`: EntityRef): seq[Component] =
      ## Creates a list of components ready to be used for construction.
      assert `entity`.alive
      let length = `entCompCount`
      `res` = newSeq[Component](length)
      for i, compRef in `entity`.entityId.pairs:
        caseComponent(compRef.typeId):
          `res`[i] = componentInstanceType()(compRef.index).makeContainer()

    proc clone*(`entity`: EntityRef): EntityRef =
      ## Copy an entity's components to a new entity.
      ## Note that copying objects with pointers/references can have undesirable results.
      ## For special setup, use `registerCloneConstructor` for the type. This gets passed
      ## the clone type it would have added. You can then add a modified component or 
      ## entirely different set of components, or ignore it by not adding anything.
      assert `entity`.alive, "Cloning a dead entity"
      static: startOperation(EcsIdentity(`id`), "clone")

      `res` = newEntity()
      # Build an index of component type to its instantiated instance.
      var
        `types`: Table[int, ComponentIndex]
        `visited`: array[`maxSys`, bool]
      `curEntTempl`

      for compRef in `entity`.components:
        assert compRef.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $compRef.typeId.int
        assert not `types`.hasKey(compRef.typeId.int), "Cannot construct: Entity has duplicate components for " & $compRef.typeId

        var `reference`: ComponentRef

        # Index and unowned component generation.
        caseComponent compRef.typeId:
          # trigger construction callback if present
          let cb = `cloneConstruct`[compRef.typeId.int]
          if cb != nil:
            # The callback is responsible for adding reference(s) from the source entity.
            # Callbacks may add multiple sets of components or none at all.
            let compsAdded = cb(`res`, compRef)
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
                  # This reference isn't valid until we add the tuple in the second pass.
                  `reference` = compRef
                `addToEnt`
                `types`[comp.typeId.int] = `reference`.index
          else:
            when owningSystemIndex == InvalidSystemIndex:
              `reference` = newInstance(componentInstanceType()(compRef.index).access).toRef
            else:
              # This reference isn't valid until we add the tuple in the second pass.
              `reference` = compRef
            `addToEnt`
            `types`[compRef.typeId.int] = `reference`.index
            
      # Update systems.
      `userCloneDecls`
      for `compIndexInfo` in `types`.pairs:
        `cloneCase`
      `userCloneEvents`
      `userCloneStateChangeEvent`
      static: endOperation(EcsIdentity(`id`))
