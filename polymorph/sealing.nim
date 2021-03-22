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

import
  macros, sharedtypes, private/[utils, ecsstatedb], components, entities,
  statechanges, runtimeconstruction
import tables, strutils, stats, sets
from typetraits import tupleLen

export
  onAddCallback, onRemoveCallback,
  onAdd, onRemove, onInit, onInterceptUpdate, onUpdate,
  onSystemAdd, onSystemAddTo, onSystemRemove, onSystemRemoveFrom,
  onEntityChange,
  ecsstatedb

proc makeEntityState(options: ECSEntityOptions): NimNode =
  ## Instantiate the variable that holds the entity component state information,
  ## and generate entity utility procs.
  let
    # TODO: Allow multiple entity states by parametrising `entityStorageTypeName`.
    ecStateVarIdent = ident(entityStorageVarName())
    eqIdent = nnkAccQuoted.newTree(ident "==")
    initEntityStorageType = ident initEntityStorageTypeName()
    storageType = ident entityStorageTypeName()
    entIdNode = ident "entityId"
    entAccess = entAccess(options, entIdNode)
    typeClass = ident typeClassName()
    maxEntId = entityIdUpperBound(options.entityStorageFormat)

  result = quote do:
    # State definition
    var `ecStateVarIdent`: `storageType`
    `initEntityStorageType`(`ecStateVarIdent`)

    # Quick access to the entity's object without implying a copy
    template entityData*(`entIdNode`: EntityId): untyped = `entAccess`
    proc lastEntityId*: EntityId = (`ecStateVarIdent`.nextEntityId.IdBaseType - 1.IdBaseType).EntityId

    proc `eqIdent`*(eRef: EntityRef, e: EntityId): bool {.inline.} =
      eRef.entityId.IdBaseType == e.IdBaseType and
        eRef.instance.IdBaseType == entityData(e).instance.IdBaseType
    proc isCurrent*(eRef: EntityRef): bool =
      # Only checks instances
      eRef.instance.IdBaseType == entityData(eRef.entityId).instance.IdBaseType
    template `eqIdent`*(live: EntityId, eRef: EntityRef): bool =
      # Useful if you need to compare a reference to the live entity
      eRef.entityId.IdBaseType == live.IdBaseType and
        eRef.instance.IdBaseType == entityData(live).instance.IdBaseType
    proc instance*(e: EntityId): EntityInstance {.inline.} = entityData(e).instance
    proc instance*(e: EntityRef): EntityInstance {.inline.} = entityData(e.entityId).instance
    ## Generate a reference to a particular instance of an entity.
    proc makeRef*(entityId: EntityId): EntityRef {.inline.} = (entityId, entityData(entityId).instance)
    proc entityCount*: int =
      ## Returns the number of alive entities.
      `ecStateVarIdent`.entityCounter
    proc high*(entityType: typedesc[EntityId] | typedesc[EntityRef]): int = `maxEntId`
    template alive*(entity: EntityId): bool =
      ## Checks the entity id (the slot, not instance) is valid
      ## (not NO_ENTITY) and that its index has been initialised.
      entity.valid and entity.int >= 1 and
        entity.int <= `maxEntId` and
        entityData(entity).setup
    ## For an EntityRef, alive checks that the instance matches the referenced entity, ie; if
    ## the entity has been deleted/recreated since the reference was made, as well as checking
    ## if the entity itself is valid and initialised.
    template alive*(entRef: EntityRef): bool =
      entRef.entityId.alive and entityData(entRef.entityId).instance.int == entRef.instance.int
    template components*(entity: EntityRef, index: int): untyped =
      ## Access to entity's components.
      assert entity.alive
      entityData(entityId).componentRefs[index]
    template withComponent*(entity: EntityRef, t: typedesc[`typeClass`], actions: untyped): untyped =
      block:
        let component {.inject.} = entity.fetchComponent(t)
        actions

proc makeEntitySupport(entOpts: ECSEntityOptions): NimNode =
  let
    entity = ident "entity"
    entIdNode = ident "entityId"
    componentTypeId = ident "componentTypeId"
    typeClass = ident typeClassName()
    hasCore = 
      if entOpts.useSet:
        let setType = ident enumName()
        quote do:
          return `componentTypeId`.`setType` in entityData(`entIdNode`).exists
      else:
        case entOpts.componentStorageFormat
        of csTable:
          quote do:
            return entityData(`entIdNode`).componentRefs.hasKey(`componentTypeId`)
        of csSeq:
          quote do:
            for c in entityData(`entIdNode`).componentRefs:
              if c.typeId == `componentTypeId`:
                return true
        of csArray:
          quote do:
            for i in 0 ..< entityData(`entIdNode`).nextCompIdx:
              let c = entityData(`entIdNode`).componentRefs[i]
              if c.typeId == `componentTypeId`:
                return true
  result = newStmtList()
  result.add(quote do:
    proc hasComponent*(`entity`: EntityRef, `componentTypeId`: ComponentTypeId): bool =
      let `entIdNode` = `entity`.entityId
      
      if not `entity`.alive:
        var str = "hasComponent on dead entity: " & $`entIdNode`.int & " instance " & $(`entIdNode`.instance.int)
        if `entIdNode` != `entity`:
          str &= " expected instance " & $`entity`.instance.int &
            " type " & $`componentTypeId`.int
        assert false, str
      
      if entityData(`entIdNode`).setup:
        `hasCore`
    
    template hasComponent*(entity: EntityRef, t: typedesc[`typeClass`]): untyped =
      # TODO: doesn't support components that aren't objects, eg `registerComponent: MyComp = seq[T]`.
      entity.hasComponent t.typeId
    
    template has*(entity: EntityRef, t: typedesc[`typeClass`]): untyped =
      entity.hasComponent t

    template contains*(entity: EntityRef, `componentTypeId`: ComponentTypeId): bool = entity.hasComponent(`componentTypeId`)
    
    template contains*(entity: EntityRef, t: typedesc[`typeClass`]): untyped =
      entity.hasComponent(t.typeId)
  )

  case entOpts.componentStorageFormat
  of csSeq:
    result.add(quote do:
      
      iterator components*(entityId: EntityId): ComponentRef =
        # Iterate through components. Different methods are required for different storage strategies.
        for item in entityData(entityId).componentRefs:
          yield item
      
      iterator pairs*(entityId: EntityId): (int, ComponentRef) =
        # Iterate through components. Different methods are required for different storage strategies.
        for i, item in entityData(entityId).componentRefs.pairs:
          yield (i, item)

      proc componentCount*(entityId: EntityId): int = entityData(entityId).componentRefs.len
      
      proc componentCount*(entityRef: EntityRef): int = entityData(entityRef.entityId).componentRefs.len
    )
  of csArray:
    result.add(quote do:
      
      iterator components*(entityId: EntityId): ComponentRef =
        # Iterate through components. Different methods are required for different storage strategies.
        let length = entityData(entityId).nextCompIdx
        for i in 0 ..< length:
          yield entityData(entityId).componentRefs[i]
      
      iterator pairs*(entityId: EntityId): (int, ComponentRef) =
        # Iterate through components. Different methods are required for different storage strategies.
        let length = entityData(entityId).nextCompIdx
        for i in 0 ..< length:
          yield (i, entityData(entityId).componentRefs[i])
      
      proc componentCount*(entityId: EntityId): int = entityData(entityId).nextCompIdx
      
      proc componentCount*(entityRef: EntityRef): int = entityData(entityRef.entityId).nextCompIdx
    )
  of csTable:
    result.add(quote do:

      iterator components*(entityId: EntityId): ComponentRef =
        # Iterate through components. Different methods are required for different storage strategies.
        for item in entityData(entityId).componentRefs.values:
          yield item
      
      iterator pairs*(entityId: EntityId): (int, ComponentRef) =
        # Iterate through components. Different methods are required for different storage strategies.
        var i: int
        for item in entityData(entityId).componentRefs.values:
          yield (i, item)
          i += 1
      
      proc componentCount*(entityId: EntityId): int = entityData(entityId).componentRefs.len
      
      proc componentCount*(entityRef: EntityRef): int = entityData(entityRef.entityId).componentRefs.len
    )

  result.add(quote do:

    template components*(entity: EntityRef): untyped = entity.entityId.components
    
    iterator items*(entity: EntityRef): ComponentRef =
      for comp in entity.entityId.components: yield comp
    
    template pairs*(entity: EntityRef): (int, ComponentRef) =
      entity.entityId.pairs

    template forAllEntities*(actions: untyped) =
      ## Walk all active entities.
      var found, pos: int
      while found < entityCount():
        if entityData(pos.EntityId).setup:
          let
            index {.inject, used.} = found
            storageIndex {.inject, used.} = pos
            entity {.inject.}: EntityRef =
              (pos.EntityId, entityData(pos.EntityId).instance)
          actions
          found += 1
        pos += 1
  )

proc recyclerHasData(ecStateNode: NimNode, options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.len > 0
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      `ecStateNode`.`rLen` > 0

proc recyclerGet(ecStateNode: NimNode, options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.pop
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      let curIdx = `ecStateNode`.recycleLen - 1
      let r = `ecStateNode`.entityRecycler[curIdx]
      `ecStateNode`.`rLen` = curIdx
      r

proc makeFindComponent(id: EcsIdentity, entityId: NimNode, componentTypeId: ComponentTypeId): NimNode =
  ## Generate code fragment for fetching a ComponentTypeId depending on entity options.
  let
    returnType = ident instanceTypeName(id.typeName componentTypeId)
    res = genSym(nskVar, "res")
    options = id.entityOptions
  case options.componentStorageFormat
  of csTable:
    quote do: `returnType`(entityData(`entityId`).componentRefs.getOrDefault(`componentTypeId`.ComponentTypeId).index)
  of csSeq, csArray:
    # We have to search the component list one by one. For small lists, this may be faster than a table
    # due to data locality. Seq is usually in heap, is more indirected than array but is more memory efficient.
    # Ideally, we would have the most commonly looked up components closer to the start of the list (added first).
    # TODO: Further investigate the performance of ordering. initial investigations with a binary insert
    # seems to be slower in the overall case, but potentially could be improved by ordering type ids by
    # use or offering some priority tagging to keep those components closer to the start of lists.
    let
      length =
        if options.componentStorageFormat == csSeq:
          quote: entityData(`entityId`).componentRefs.len
        else:
          quote: entityData(`entityId`).nextCompIdx
    quote do:
      var `res` = InvalidComponentRef
      for i in 0 ..< `length`:
        if entityData(`entityId`).componentRefs[i].typeId == `componentTypeId`.ComponentTypeId:
          `res` = entityData(`entityId`).componentRefs[i]
          break
      `returnType`(`res`.index)

proc makeFetchComponent(id: EcsIdentity): NimNode =
  ## Generate `fetchComponent` for all types not processed so far.
  result = newStmtList()
  
  for typeId in id.unsealedComponents:

    var
      tyName = id.typeName typeId
      typeNode = ident tyName
      instanceNode = ident instanceTypeName(tyName)
      tid = typeStringToId(id, tyName)
      entIdent = ident "entity"
      entIdIdent = ident "entityId"
      findComp = makeFindComponent(id, entIdIdent, tid)
      # Needs to be an ident (rather than a sym if typed in the `quote`) to bind later.
      alive = ident "alive"

    assert tid != InvalidComponent
    result.add(quote do:
      proc fetchComponent*(`entIdent`: EntityRef, t: typedesc[`typeNode`]): `instanceNode` =
        ## Looks up and returns the instance of the component, which allows direct field access.
        ## Returns default no component index if the component cannot be found.
        ## Eg;
        ##   let comp = entity.fetchComponent CompType  # Will be of type CompTypeInstance
        ##   comp.x = 3 # Edit some supposed fields for this component.
        assert `entIdent`.`alive`, "Fetch component on a dead entity. Entity ID: " & $`entIdent`.entityId.int &
          ", Instance: " & $`entIdent`.instance.int
        let `entIdIdent` = `entIdent`.entityId
        `findComp`
      
      template fetch*(`entIdent`: EntityRef, t: typedesc[`typeNode`]): `instanceNode` =
        fetchComponent(`entIdent`, t)

    )

proc makeNewEntity(options: ECSEntityOptions): NimNode =
  result = newStmtList()

  # Perform relevant setup for the entity's component storage.
  let
    entStorage = ident entityStorageVarName()
    entityId = ident "entityId"
    compInit =
      case options.componentStorageFormat
      of csSeq:
        if options.entityStorageFormat == esPtrArray:
          quote do:
            GC_Ref(entityData(`entityId`).componentRefs)
        else:
          newEmptyNode()
      of csArray:
        quote do:
          entityData(`entityId`).nextCompIdx = 0
      of csTable:
        if options.entityStorageFormat == esPtrArray:
          quote do:
            GC_Ref(entityData(`entityId`).componentRefs)
            entityData(`entityId`).componentRefs.clear
        else:
          quote do:
            entityData(`entityId`).componentRefs.clear

  let
    maxEntLen = maxEntLen(options)

    maxLenCheck =
      case options.entityStorageFormat
      of esArray, esPtrArray:
        case options.errors.errEntityOverflow:
        of erAssert:
          quote do:
            assert `entStorage`.nextEntityId.IdBaseType < `maxEntLen`, "Exceeded entity limit: newEntity's maximum entity count is " & $(`maxEntLen` - 1)
        of erRaise:
          quote do:
            if `entStorage`.nextEntityId.IdBaseType >= `maxEntLen`:
              raise newException(EntityOverflow, "Exceeded entity limit: newEntity's maximum entity count is " & $(`maxEntLen` - 1))
      of esSeq:
        newStmtList()

    updateEntStorage =
      case options.entityStorageFormat
      of esSeq:
        quote do:
          `entStorage`.entityComponents.setLen `entStorage`.nextEntityId.int + 1
          `entStorage`.nextEntityId = `entStorage`.entityComponents.len.EntityId

      of esArray, esPtrArray:
        quote do:
          `entStorage`.nextEntityId = (`entStorage`.nextEntityId.IdBaseType + 1).EntityId

    recyclerData = entStorage.recyclerHasData(options)
    recyclerGet = entStorage.recyclerGet(options)
    setOp =
      if options.useSet: quote do:
        entityData(`entityId`).exists = {}        
      else: newEmptyNode()

  result.add(quote do:
    proc newEntity*: EntityRef =
      var `entityId`: EntityId
      if `recyclerData`:
        `entityId` = `recyclerGet`
      else:
        `maxLenCheck`
        `entityId` = `entStorage`.nextEntityId
        `updateEntStorage`
      
      # Check we're not overwriting a live entity
      assert entityData(`entityId`).setup == false,
        "Overwriting EntityId = " & $`entityId`.int &
        " counter = " & $`entStorage`.entityCounter

      `entStorage`.entityCounter += 1
      # set up the new entry with this component
      `setOp`
      `compInit`

      entityData(`entityId`).setup = true
      # TODO: Handle overflow.
      let i = (entityData(`entityId`).instance.IdBaseType + 1).EntityInstance
      entityData(`entityId`).instance = i
      (`entityId`, i)
  )


import algorithm

proc getComponentUpdatePerformance(id: EcsIdentity): seq[ComponentUpdatePerfTuple] {.compileTime.} =
  ## Internal procedure to return the number of systems each component accesses during an update.
  ## The result is sorted.
  var r: seq[ComponentUpdatePerfTuple]

  for typeId in id.unsealedComponents:
    # Each component gets a direct access addComponent
    let
      tyNameStr = id.typeName typeId
      relevantSystems = id.linked typeId
    r.add((tyNameStr, relevantSystems.len))

  r.sort do (x, y: ComponentUpdatePerfTuple) -> int:
    cmp(y.systemsUpdated, x.systemsUpdated)
  r

macro componentUpdatePerformance*(id: static[EcsIdentity]): untyped =
  ## Generates a static sequence of components by the number of systems they update,
  ## ordered by increasing system accesses.
  ## 
  ## This can act as a guide of the cost of performing `addComponent`, `removeComponent` and `newEntityWith`.
  var items = getComponentUpdatePerformance(id)

  var bracket = nnkBracket.newTree
  for item in items:
    bracket.add(quote do: `item`)
  result = newStmtList(nnkPrefix.newTree(ident "@", bracket))

proc makeEntities(id: EcsIdentity): NimNode =
  # Create entity state.
  var r = newStmtList()
  let entOpts = id.entityOptions
  if entOpts.entityStorageFormat != esSeq and entOpts.maxEntities <= 0:
    error "Cannot generate entities with a max count of zero as the entity storage format is non-resizable"
  
  let entTypes = makeEntityItems(id)
  r.add entTypes
  r.add makeEntityState(entOpts)
  r.add makeEntitySupport(entOpts)
  r.add makeNewEntity(entOpts)
  r

# Runtime systems

proc makeRuntimeStrOutput(id: EcsIdentity): NimNode =
  let
    res = ident "result"
    strOp = nnkAccQuoted.newTree(ident "$")
    # compName matches the template provided by caseComponent.
    compName = ident "componentName"
  var
    componentCount: int
  
  result = newStmtList()

  # String operator for instance types.
  for c in id.unsealedComponents:
    let
      instTypeName = id.typeName c
      instType = ident instanceTypeName(instTypeName)
      invalidStr = newLit "<Invalid " & instTypeName & ">"
      deletedPrefix = newLit "<Deleted " & instTypeName

    result.add(quote do:
      proc `strOp`*(val: `instType`): string =
        if val.valid:
          try:
            `res` = val.access.repr
          except:
            `res` = "<Error accessing>\n"
        else:
          if val.int == InvalidComponentIndex.int:
            `res` = `invalidStr`
          else:
            `res` = `deletedPrefix` & " (index: " & $val.int & ")>"
      )

    componentCount += 1

  let
    compCount = newIntLitNode componentCount
    tc = ident instanceTypeClassName()

  result.add(quote do:
    # ------
    # Tools
    # ------

    proc `strOp`*[T: `tc`](val: T): string =
      ## Generic `$` for component indexes.
      if val.valid:
        try:
          for field, value in val.access.fieldPairs:
            when value isnot `tc`:
              `res` &= field & ": " & value.repr
            else:
              `res` &= field & ": " & val.access.repr
            
            if `res`[^1] != '\n': `res` &= '\n'
        except:
          `res` = "<Error accessing " & $T & ": " & getCurrentExceptionMsg() & ">"
      else:
        if val.int == InvalidComponentIndex.int:
          `res` = "<Invalid " & $T & ">"
        else:
          `res` = "<Out of bounds instance of " & $T & " (index: " & $val.int & ")>"

    proc `strOp`*(componentId: ComponentTypeId): string =
      ## Display the name and id for a component type.
      componentId.caseComponent:
        `res` = `compName`() & " (" & `strOp`(int(componentId)) & ")"

    func typeName*(componentId: ComponentTypeId): string = 
      componentId.caseComponent:
        `res` = `compName`()

    proc toString*(componentRef: ComponentRef, showData: bool = true): string =
      ## Display the name, type and data for a component reference.
      let tId = componentRef.typeId
      tId.caseComponent:
        `res` = `compName`() & " (id: " & `strOp`(int(tId)) & ", index: " & `strOp`(componentRef.index.int) & ", generation: " & `strOp`(componentRef.generation.int) & ")"
        if showData:
          `res` &= ":\n"
          try:
            `res` &= `strOp`(componentInstanceType()(componentRef.index.int))
          except:
            `res` &= "<ERROR ACCESSING (index: " & `strOp`(componentRef.index.int) & ", count: " & `strOp`(componentInstanceType().componentCount) & ")>\n"

    proc `strOp`*(componentRef: ComponentRef, showData: bool = true): string = componentRef.toString(showData)

    proc toString*(comp: Component, showData = true): string =
      ## `$` function for dynamic component superclass.
      ## Displays the sub-class data according to the component's `typeId`.
      caseComponent comp.typeId:
        result &= `compName`()
        if showData:
          result &= ":\n" & $componentRefType()(comp).value.repr & "\n"
    
    proc `strOp`*(comp: Component): string = comp.toString

    proc toString*(componentList: ComponentList, showData: bool = true): string =
      ## `$` for listing construction templates.
      let maxIdx = componentList.high
      for i, item in componentList:
        caseComponent item.typeId:
          let s = componentRefType()(item).toString(showData)
          if i < maxIdx and not showData:
            result &= s & ", "
          else:
            result &= s
    
    proc `strOp`*(componentList: ComponentList): string = componentList.toString

    proc toString*(construction: ConstructionTemplate, showData: bool = true): string =
      for i, item in construction:
        `res` &= `strOp`(i) & ": " & item.toString(showData) & "\n"

    proc `strOp`*(construction: ConstructionTemplate): string = construction.toString

    ## Count of components defined for this ECS.
    proc componentCount*: int = `compCount`
  )

proc makeRuntimeTools(id: EcsIdentity): NimNode =
  result = newStmtList( quote do:

    template matchToSystems*(componentTypeId: ComponentTypeId, actions: untyped): untyped =
      # Match a runtime componentTypeId with its systems. Has to check all systems at runtime, so is slow.
      # This is intended for aiding debugging.
      forAllSystems:
        if componentTypeId in system.requirements:
          actions

    type EntityTransitionType* = enum ettUpdate, ettRemoveAdd

    template transition*(entity: EntityRef,
        prevState, newState: ComponentList,
        transitionType: static[EntityTransitionType]) =
      ## Removes components in `prevState` that aren't in `newState` and
      ## adds or updates components in `newState`.
      ## 
      ## `transitionType` controls whether to just update components that
      ## are in both states, or to always remove components in
      ## `prevState` and add `newState`.
      ## A transition type of `ettRemoveAdd` will always trigger events
      ## such as `onAdd`, but does more work if many components are
      ## shared between `prevState` and `newState`.
      ## 
      ## Note: be aware when invoking this in systems that removing
      ## components can invalidate the current `item` row.
      {.line.}:
        if prevState.len > 0:
          var prevIds = newSeq[ComponentTypeId](prevState.len)
          for i, c in prevState:
            prevIds[i] = c.typeId
          # Remove components first so we don't invoke a state with both old
          # and new components.
          when transitionType == ettUpdate:
            for c in newState:
              let tyId = c.typeId
              if tyId notin prevIds:
                caseComponent tyId:
                  entity.removeComponent componentType()
          elif transitionType == ettRemoveAdd:
            for c in prevState:
              caseComponent c.typeId:
                entity.removeComponent componentType()
          else:
            {.fatal: "Unknown transition type '" & $transitionType & "'".}
        entity.addOrUpdate newState

    template transition*(entity: EntityRef, prevState, newState: ComponentList) =
      ## Removes components in `prevState` that aren't in `newState` and
      ## adds or updates components in `newState`.
      transition(entity, prevState, newState, ettUpdate)
  )

proc makeRuntimeDebugOutput(id: EcsIdentity): NimNode =
  let
    res = ident("result")
    entity = ident("entity")
    totalCount = id.len_systems - 1 # Don't count InvalidSystem entry.
    tsc = ident("totalSystemCount")
    strOp = nnkAccQuoted.newTree(ident "$")
    componentIndexTC = ident instanceTypeClassName()

    # Bind these symbols here so the user doesn't have to import them.
    statsPush = bindSym("push", brClosed)
    tupleLen = bindSym("tupleLen", brClosed)
    fmtFloat = bindSym("formatFloat", brClosed)
    fmtSize = bindSym("formatSize", brClosed)
    spaces = bindSym("spaces", brClosed)
    trimZeros = bindSym("trimZeros", brClosed)
    entOpts = id.entityOptions
    maxEntId = entityIdUpperBound(entOpts.entityStorageFormat)
    sysStr = bindSym "systemStr"

  # Build static array of system components.
  var ownedComponents = nnkBracket.newTree()
  for typeId in id.allComponentsSeq:
    if id.systemOwner(typeId) != InvalidSystemIndex:
      ownedComponents.add newDotExpr(newLit typeId.int, ident "ComponentTypeId")

  result = newStmtList()

  result.add(quote do:
    proc listComponents*(entity: EntityRef, showData = true): string =
      ## List all components attached to an entity.
      ## The parameter `showData` controls whether the component's data is included in the output.
      if entity.alive:
        let entityId = entity.entityId
        for compRef in entityId.components:
          let compDesc = `strOp`(compRef, showData)
          var
            owned: bool
            genMax: int
            genStr: string
          try:
            caseComponent compRef.typeId:
              genMax = componentGenerations().len
              let gen = componentGenerations()[compRef.index.int]
              genStr = `strOp`(gen)
              owned = componentInstanceType().isOwnedComponent
          except:
            genStr = " ERROR ACCESSING generations (index: " &
              `strOp`(compRef.index.int) &
              ", count: " & `strOp`(genMax) & ")"

          `res` &= compDesc

          # $typeId returns the string of the storage type for this component.
          if owned:
            if not compRef.alive:
              `res` &= " <DEAD OWNED COMPONENT Type: " & `strOp`(compRef.typeId) & ", generation: " & genStr & ">\n"

          else:
            if not compRef.valid:
              `res` &= " <INVALID COMPONENT Type: " & `strOp`(compRef.typeId) & ", generation: " & genStr & ">\n"

          let needsNL = `res`[^1] != '\n'
          if needsNL:
            `res` &= "\n"
          if showData:
            `res` &= "\n"

      else: `res` &= "[Entity not alive, no component item entry]\n"

    proc `strOp`*(`entity`: EntityRef, showData = true): string =
      ## `$` function for `EntityRef`.
      ## List all components and what systems the entity uses.
      ## By default adds data inside components with `repr`.
      ## Set `showData` to false to just display the component types.
      let id = `entity`.entityId.int
      `res` = "[EntityId: " & $(id)

      if id < 1 or id > `maxEntId`:
        `res` &= " Out of bounds!]"
      else:
        let
          comps = `entity`.listComponents(showData)
          systems = `entity`.listSystems()
          sys = if systems == "": "<No systems used>\n" else: systems
          invalidStr = if not `entity`.entityId.valid: " INVALID/NULL ENTITY ID" else: ""
        `res` &=
          " (generation: " & $(`entity`.instance.int) & ")" &
          invalidStr & "\nAlive: " & $`entity`.alive &
          "\nComponents:\n" & comps &
          "Systems:\n" & $sys & "]"

    proc `strOp`*(`entity`: EntityId): string =
      ## Display the entity currently instantiated for this `EntityId`.
      `strOp`(`entity`.makeRef)

    proc `strOp`*(sysIdx: SystemIndex): string =
      ## Outputs the system name passed to `sysIdx`.
      caseSystem sysIdx:
        `sysStr`(sys.name)

    ## Total number of systems defined
    const `tsc`* = `totalCount`
  
    proc analyseSystem*[T](sys: T, jumpThreshold: Natural = 0): SystemAnalysis =
      ## Analyse a system for sequential component access by measuring
      ## the difference between consecutively accessed memory addresses.
      ## 
      ## Address deltas greater than `jumpThreshold` are counted in the field
      ## `forwardJumps`.
      ## 
      ## If `jumpThreshold` is zero, each component's `jumpThreshold` is set
      ## to the size of the component type in bytes.
      ## 
      ## The ideal access pattern is generally forward sequential memory access,
      ## minimising jumps and not moving backwards.
      ## 
      ## Passing `SystemAnalysis` to `$` outputs a string that includes
      ## fragmentation metrics and distribution data.
      ## 
      ## Note that different systems may have different memory access patterns
      ## to the same components. Systems and component storage generally takes
      ## a "first come, first served" approach with regard to processing.
      ## 
      ## For example, if the first entity in a system list has component(s)
      ## binding it to the system removed, then re-added, its position in the
      ## system's processing order is likely to change even if the component(s)
      ## added back have the same memory address as before, leading to
      ## non-sequential access.

      mixin name
      `res`.name = sys.name

      template getAddressInt(value: untyped): int =
        var address: pointer
        when value is `componentIndexTC`:
          # Standard index components.
          address = value.access.addr
        else:
          # Owned components.
          address = value.unsafeAddr
        cast[int](address)

      const
        # Even if groups.len == 0, we can still retrieve the type.
        tupleLen = `tupleLen`(sys.groups[0].type)
        # The tuple's entity field isn't included.
        compCount = tupleLen - 1
      `res`.components.setLen compCount
      `res`.entities = sys.count

      template component(idx): untyped = `res`.components[idx]

      var
        # Dummy tuple to iterate field details.
        sysTuple: sys.tupleType
        fieldIdx = 0

      # Gather system tuple field info.
      for field, value in sysTuple.fieldPairs:
        when not(value is EntityRef):
          when value is `componentIndexTC`:
            # Indirection to component.
            type valueType = value.access.type
          else:
            # Owned components.
            type valueType = value.type

          # We cannot necessarily use const here because the type's
          # size may be run-time dependent, for instance {.importc.}
          # types that are not marked with {.completeStruct.}.
          let valueSize = valueType.sizeOf

          component(fieldIdx).name = field
          component(fieldIdx).valueSize = valueSize
          component(fieldIdx).jumpThreshold =
            if jumpThreshold == 0:
              # For owned components, the minimum jump size is the
              # next group item.
              if value.isOwnedComponent: sysTuple.sizeOf
              else: valueSize
            else:
              jumpThreshold

          fieldIdx += 1

      var lastAddresses: array[compCount, int]
      let systemItems = sys.count

      # Init lastAddresses with first row.
      if systemItems > 1:

        const startIdx =
          # Owner systems skip the first item.
          if sys.isOwner: 2
          else: 1

        fieldIdx = 0
        for value in sys.groups[startIdx - 1].fields:
          when not(value is EntityRef):
            lastAddresses[fieldIdx] = value.getAddressInt
            fieldIdx += 1

        # Get address diffs.
        for i in startIdx ..< systemItems:
          fieldIdx = 0
          for value in sys.groups[i].fields:
            when not(value is EntityRef):
              let
                thresh = component(fieldIdx).jumpThreshold
                address = getAddressInt(value)
                diff = address - lastAddresses[fieldIdx]
              var tagged: bool

              component(fieldIdx).allData.`statsPush` diff

              if diff < 0:
                component(fieldIdx).backwardsJumps += 1
                tagged = true
              elif diff > thresh:
                component(fieldIdx).forwardJumps += 1
                tagged = true

              if tagged:
                component(fieldIdx).taggedData.`statsPush` diff

              lastAddresses[fieldIdx] = address
              fieldIdx += 1

        for i, c in `res`.components:
          component(i).fragmentation =
            (c.backwardsJumps + c.forwardJumps).float / systemItems.float

    proc `strOp`*(analysis: SystemAnalysis): string =
      ## Outputs a string detailing a system analysis.
      ## 
      ## For each component in the system, fragmentation is calculated
      ## as the ratio of non-consecutive vs consecutive address accesses
      ## that the system makes.
      ## 
      ## A fragmentation of 0.0 means the system accesses this component
      ## sequentially forward no greater than `jumpThreshold` per item.
      ## 
      ## A fragmentation of 1.0 means every consecutive address
      ## accessed by the system for this component was greater than
      ## the `jumpThreshold`, or travelling backwards in memory. 
      ## 
      ## The shape of the distribution of address accesses by this system
      ## is described in the "Address deltas" section.
      const
        alignPos = 70
        decimals = 4

      `res` = "Analysis for " & analysis.name &
        " (" & $analysis.entities & " rows of " & $analysis.components.len & " components):\n"
      
      if analysis.components.len == 0:
        `res` &= "<No components found>\n"
      
      else:
        
        func numStr(value: float, precision = decimals): string =
          `res` = `fmtFloat`(value, ffDecimal, precision)
          `trimZeros`(`res`)
        
        func numStr(value: SomeInteger): string =
          let
            strVal = $value
            digits = strVal.len
          result.setLen digits + (digits div 3)
          var pos: int
          for d in 0 ..< digits:
            if d > 0 and ((digits - d) mod 3 == 0):
              `res`[pos] = ','
              `res`[pos + 1] = strVal[d]
              pos += 2
            else:
              `res`[pos] = strVal[d]
              pos += 1

        func pad(s1, s2: string): string =
          if s2.len > 0:
            s1 & `spaces`(max(1, alignPos - s1.len)) & s2
          else:
            s1

        for c in analysis.components:

          proc dataStr(data: RunningStat): string =

            func eqTol(a, b: float, tol = 0.001): bool = abs(a - b) < tol
            
            let
              exKurt = data.kurtosis - 3.0
              dataRange = data.max - data.min
            const
              cont = -6/5
              indent = "      "
            `res` =
              pad(
                indent & "Min: " & data.min.numStr &
                  ", max: " & $data.max.numStr &
                  ", sum: " & $data.sum.numStr,
                "Range: " & `fmtSize`(dataRange.int64, includeSpace = true)
              ) & "\n" &

              indent & "Mean: " & $data.mean.numStr & "\n" &

              pad(indent & "Std dev: " & data.standardDeviation.numStr,
                "CoV: " &
                  (if data.mean != 0.0:
                    numStr(data.standardDeviation / data.mean)
                  else:
                    "inf"
                  ) & "\n"
               ) &

              indent & "Variance: " & $data.variance.numStr & "\n" &
              
              pad(
                indent & "Kurtosis/spread (normal = 3.0): " &
                $data.kurtosis.numStr &
                " (excess: " & exKurt.numStr & ")",
                if exKurt > 2.0: "Many outliers"
                  elif exKurt.eqTol 0.0: "Normally distributed"
                  elif exKurt.eqTol cont: "Continuous/no outliers"
                  elif exKurt < -2.0: "Few outliers"
                  else: ""
              ) & "\n" &

              pad(indent & "Skewness: " & $data.skewness.numStr,
                if data.skewness < 0: "Outliers trend backwards"
                elif data.skewness > 0: "Outliers trend forwards"
                else: ""
              ) & "\n"
          let
            jt = c.jumpThreshold.float
            n = c.allData.n
            fwdPerc =
              if n > 0: numStr((c.forwardJumps / n) * 100.0)
              else: "N/A"
            bkdPerc =
              if n > 0: numStr((c.backwardsJumps / n) * 100.0)
              else: "N/A"
            indent = "    "
          
          `res` &= "  " & c.name & ":\n" &

            indent & "Value size: " & `fmtSize`(c.valueSize, includeSpace = true) &
              ", jump threshold: " & `fmtSize`(c.jumpThreshold, includeSpace = true) & "\n" &

            pad(
              indent & "Jumps over threshold : " & $c.forwardJumps.numStr,
              "Jump ahead: " & fwdPerc & " %") & "\n" &

            pad(
              indent & "Backwards jumps      : " & $c.backwardsJumps.numStr,
              "Jump back: " & bkdPerc & " %") & "\n" &

            indent & "Fragmentation: " & numStr(c.fragmentation * 100.0) & " %" &
              " (n = " & $c.taggedData.n & "):\n"

          if c.taggedData.n > 0:
            `res` &= indent & "  Mean scale: " &
              numStr(c.taggedData.mean / jt) &
              " times threshold\n" &
              c.taggedData.dataStr
          else:
            `res` &= indent & "  <No fragmented indirections>\n"

          `res` &= indent & "All address deltas (n = " & $n & "):\n"
          if n > 0:
            `res` &= c.allData.dataStr
          else: `res` &=
            indent & "  <No data>\n"
  )

proc makeListSystem(id: EcsIdentity): NimNode =
  var
    res = ident "result"
    innards = newStmtList()
    entIdent = ident "entity"
  innards.add(quote do: `res` = "")
  for sysId in id.unsealedSystems:
    let
      options = id.getOptions sysId
      sysName = id.getSystemName(sysId)
      sysStr = newLit systemStr(sysName)
      sys = id.instantiation sysId
      entId = entIdent.newDotExpr(ident "entityId")
      hasKey = indexHasKey(sys, entId, options.indexFormat)
      reqs = id.ecsSysRequirements(sysId)

    innards.add(quote do:
      var inSys = true
      for req in `reqs`:
        if req.ComponentTypeId notin `entIdent`:
          inSys = false
          break
      if inSys:
        if `hasKey`:
          `res` &= `sysStr` & " \n"
        else:
          `res` &= `sysStr` & " Sync issue: entity contains components but is missing from this system's index\n"
      )

  result = quote do:
    proc listSystems*(`entIdent`: EntityRef): string =
      if `entIdent`.alive:
        `innards`
      else:
        if `entIdent` == NO_ENTITY_REF:
          `res` = "<Entity is NO_ENTITY_REF>"
        else:
          `res` = "<Entity is not alive>"

proc doStartLog(id: EcsIdentity): NimNode =
  if not id.logInitialised:
    id.set_logInitialised true
    quote do: startGenLog(`defaultGenLogFilename`)
  else:
    newStmtList()


proc makeCaseComponent(id: EcsIdentity): NimNode =
  ## Generate caseComponent for the current component set.
  let
    actions = ident "actions"
    idIdent = ident "id"
  var caseStmt = nnkCaseStmt.newTree()
  caseStmt.add quote do: `idIdent`.int

  for component in id.unsealedComponents:

    var
      ofNode = nnkOfBranch.newTree()
      compVal = newIntLitNode(component.int)
    ofNode.add compVal
    let
      tyStr = id.typeName component
      ty = newIdentNode tyStr
      tyRef = newIdentNode refTypeName(tyStr)
      tyInstance = newIdentNode instanceTypeName(tyStr)
      tyInit = newIdentNode createInstanceName(tyStr)
      tyRefInit = newIdentNode refInitName(id.refInitPrefix(component), tyStr)
      tyDel = newIdentNode deleteInstanceName()
      # reference the alive variable for this type.
      aliveStateIdent = newIdentNode aliveStateInstanceName(tyStr)
      # reference the storage object
      storageFieldIdent = newIdentNode storageFieldName(tyStr)
      tyInstanceIds = newIdentNode instanceIdsName(tyStr)
      sysOwner = id.systemOwner(component)
      sysIdx = newDotExpr(newLit sysOwner.int, ident "SystemIndex")
    var
      accessOwningSystem = newStmtList()
      isOwnedComponent: NimNode
    
    if sysOwner != InvalidSystemIndex:
      isOwnedComponent = newLit true
      let sysOwner = id.instantiation sysOwner
      accessOwningSystem.add(quote do:
        template owningSystem: untyped {.used.} = `sysOwner`
      )
    else:
      isOwnedComponent = newLit false

    # Following templates are available for use within the case statement.
    # These aren't compiled in unless the invoker uses them.
    ofNode.add(quote do:
      template componentId: untyped {.used.} = `component`.ComponentTypeId
      template componentName: untyped {.used.} = `tyStr`
      template componentType: untyped {.used.} = `ty`
      template componentRefType: untyped {.used.} = `tyRef`
      template componentInit: untyped {.used.} = `tyInit`
      template componentRefInit: untyped {.used.} = `tyRefInit`
      template componentDel(index: `tyInstance`): untyped {.used.} = `tyDel`(index)
      template componentAlive: untyped {.used.} = `aliveStateIdent`
      template componentGenerations: untyped {.used.} = `tyInstanceIds`
      template componentInstanceType: untyped {.used.} = `tyInstance`
      # Component data is similar to `access` but provides the whole array.
      # Eg; `echo componentData[myComponentRef.index.int].repr`
      template componentData: untyped {.used.} = `storageFieldIdent`
      template isOwned: bool {.used.} = `isOwnedComponent`
      template owningSystemIndex: SystemIndex {.used.} = `sysIdx`
      `accessOwningSystem`
      `actions`
    )
    caseStmt.add ofNode

  let elseCode =
    case id.errCaseComponent
    of erAssert:
      quote do:
        assert false, "Invalid component type id: " & $(`idIdent`.toInt)
    of erRaise:
      quote do:
        raise newException(ValueError, "Invalid component type id: " & $(`idIdent`.toInt))
  caseStmt.add(nnkElse.newTree(elseCode))

  result = newStmtList()
  result.add(quote do:
    template caseComponent*(`idIdent`: ComponentTypeId, `actions`: untyped): untyped =
      ## Creates a case statement that matches `id` with its component.
      ##
      ## Note:
      ## * Has no concept of entity, this is a static case statement with injected
      ##   actions
      ## * the same action block is compiled for every choice, but you can use the
      ##   local `component` template to fetch any outer scope entities you wish
      ##   on the branch qualified type.
      ##
      ## For example, the following will display the name of a run-time component type id.
      ##
      ## ```
      ## myCompId.caseComponent:
      ##   echo "Component Name: ", componentName
      ## ```
      ##
      ## Within `actions`, the following templates provide typed access to the runtime index.
      ##
      ##   * componentId: the ComponentTypeId of the component
      ##   * componentName: string name
      ##   * componentType: static type represented by `id`
      ##   * componentInstanceType: index type, eg; MyComponentInstance
      ##   * componentRefType: ref type for this component, eg: MyComponentRef
      ##   * componentInit: initialiser procedure for this type
      ##   * componentRefInit: Ref initialiser procedure for this type
      ##   * componentDel: delete procedure for this type
      ##   * componentAlive: direct access to proc to test if this component is alive
      ##   * componentGenerations: direct access to the generation values for this type
      `caseStmt`
  )

proc makeMatchSystem*(id: EcsIdentity): NimNode =
  ## Generate caseSystem and forAllSystems for current systems.
  let
    actions = ident "actions"
    index = ident "index"
    unsealedSystems = id.allUnsealedSystems
  var body = newStmtList()
  var caseStmt = nnkCaseStmt.newTree()
  
  caseStmt.add quote do: `index`.int

  for sysId in unsealedSystems:

    let sysIdx = sysId.int
    var ofNode = nnkOfBranch.newTree()

    ofNode.add newIntLitNode(sysIdx)

    var
      curSys = id.instantiation sysId
      curSysTuple = ident(id.getSystemName(sysId).tupleName)

    ofNode.add(quote do:
      template sys: untyped {.used.} = `curSys`
      template SystemTupleType: typedesc {.used.} = `curSysTuple`
      `actions`
    )

    caseStmt.add ofNode

  let elseCode = quote do: raise newException(ValueError, "Invalid system index: " & $`index`.int)
  caseStmt.add(nnkElse.newTree(elseCode))
  body.add caseStmt

  var allSysBody = newStmtList()
  for sys in unsealedSystems:
    let
      curSys = id.instantiation sys
      curSysTuple = ident(id.getSystemName(sys).tupleName)
    allSysBody.add(quote do:
      block:
        template sys: untyped {.used.} = `curSys`
        template SystemTupleType: typedesc {.used.} = `curSysTuple`
        `actions`
    )

  result = quote do:
    template caseSystem*(`index`: SystemIndex, `actions`: untyped): untyped =
      ## Creates a case statement that matches a `SystemIndex` with its instantiation.
      ## This generates a runtime case statement that will perform `actions`
      ## for all systems like so:
      ##  case index
      ##    of 0: actions
      ##    of 1: actions
      ##    ... and so on for each system index
      ## `actions` is therefore executed using the correct `system` context
      ## for the runtime system. IE, if `index` = 7 then `system` will be 
      ## the instantiated variable for the seventh system.
      ## This allows you to write generic code that dynamically applies to any system
      ## chosen at runtime.
      ## Use the `sys` template to access to the system variable the index represents.
      `body`

    template forAllSystems*(`actions`: untyped): untyped =
      ## This will perform `actions` for every system.
      ## Injects the `sys` template for easier operation.
      `allSysBody`

proc makeCompRefAlive: NimNode =
  quote do:
    template alive*(compRef: ComponentRef): bool =
      ## Check if this component ref's index is still valid and active.
      ## Requires use of run-time case statement to match against type id.
      let index = compRef.index.int
      var r: bool
      caseComponent compRef.typeId:
        r = componentAlive()[index] and compRef.generation.int == componentGenerations()[index]
      r

proc sealComps(id: EcsIdentity): NimNode =
  assert id.unsealedComponentCount > 0, "No components defined"

  result = newStmtList()
  result.add generateTypeStorage(id)
  result.add genTypeAccess(id)

proc addPerformanceLog(id: EcsIdentity) {.compileTime.} =
  ## Append system operations per component to the log.
  let commentStart = "# "
  let perf: seq[ComponentUpdatePerfTuple] = getComponentUpdatePerformance(id)
  for item in perf:
    if item.systemsUpdated == 0:
      genLog commentStart & item.componentType & ": <No systems using this component>"
    else:
      genLog commentStart & item.componentType & ": " & $item.systemsUpdated & " systems"
  genLog ""

proc sealEntities(id: EcsIdentity): NimNode =
  result = newStmtList()
  result.add makeEntities(id)
  result.add makeCompRefAlive()
  result.add makeFetchComponent(id)
  result.add makeCaseComponent(id)
  result.add makeMatchSystem(id)

proc sealStateChanges(id: EcsIdentity): NimNode =
  result = newStmtList()
  result.add makeDelete(id)
  result.add makeNewEntityWith(id)
  result.add makeAddComponents(id)
  result.add makeRemoveComponentDirect(id)

proc sealRuntimeDebugging(id: EcsIdentity): NimNode =
  result = newStmtList()
  result.add makeRuntimeStrOutput(id)
  result.add makeListSystem(id)
  result.add makeRuntimeDebugOutput(id)

proc invalidSystemAdds(id: EcsIdentity): (bool, string) =
  for sysId in id.allSystemsSeq:
    let
      sysReqs = id.ecsSysRequirements sysId
      sysName = id.getSystemName sysId

    for compId in id.onAddToSystemComp(sysId):

      if compId notin sysReqs:
        let
          compStr = id.typeName compId
          sysStr = "\"" & sysName & "\""
          compList = id.commaSeparate sysReqs
        return (true, "Trying to set a user event for component \"" &
          compStr & "\" being added to system \"" & sysStr &
          "\", but this system does not use the component. \"" &
          sysStr & "\" uses [" & compList & "]")

    for compId in id.onRemoveFromSystemComp(sysId):
      if compId notin sysReqs:
        let
          compStr = id.typeName compId
          sysStr = "\"" & sysName & "\""
          compList = id.commaSeparate sysReqs
        return (true, "Trying to set a user event for component \"" &
          compStr & "\" being removed from system \"" & sysStr &
          "\", but this system does not use the component. \"" &
          sysStr & "\" uses [" & compList & "]")

macro systemsUsed*(id: static[EcsIdentity], components: openarray[typedesc]): untyped =
  ## Output a string listing systems that an entity with the parameter
  ## components would use.
  var
    notFound: seq[string]
    res: seq[SystemIndex]
    comps: seq[ComponentTypeId]
  for c in components:
    let
      cStr = $c
      # TODO: Use a single place to define "ref" postfix.
      tId = 
        if cStr.len > 3 and cStr[^3..^1].toLowerAscii == "ref":
          id.findCompId(cStr[0..^4])
        else:
          id.findCompId(cStr)
    if tId == InvalidComponent:
      notFound.add cStr
    else:
      comps.add tId
  
  for c in comps:
    for sys in id.systems(c):

      if sys notin res:
        var match = true
        for sysComp in id.ecsSysRequirements(sys):
          if sysComp notin comps:
            match = false
            break
    
        if match:
          res.add sys

  if notFound.len > 0:
    result = newLit id.commaSeparate(res) & " (unknown component type '" & join(notFound, ", ") & "')"
  else:
    result = newLit id.commaSeparate(res)

template systemsUsed*(components: openarray[typedesc]): untyped =
  ## Output a string listing systems that an entity with the parameter
  ## components would use.
  defaultIdentity.systemsUsed components

macro expectSystems*(id: static[EcsIdentity], entity: EntityRef, systems: openArray[string]): untyped =
  ## Generates code to fail assertion if an entity isn't part of one or more systems.
  var systemIdxs: HashSet[SystemIndex]

  for sysName in systems:
    let sysIdx = id.findSystemIndex(sysName.strVal)
    if not sysIdx.found:
      error "Cannot find system \"" & sysName.strVal & "\""
    else:
      systemIdxs.incl sysIdx.index
  
  let
    missingSystems = genSym(nskVar, "missingSystems")
    missingCompsStr = genSym(nskVar, "missingComps")
    totalMissing = genSym(nskVar, "totalMissing")
    allCompsComma = genSym(nskVar, "acComma")
    joinComma = genSym(nskTemplate, "joinComma")

  var
    checks = newStmtList()
    allSystemsStr: string
    checkedComps: HashSet[ComponentTypeId]
  # Fixed checks are built using the parameter systems.
  for sys in systemIdxs:
    let
      sysName = id.getSystemName sys
      sysVar = id.instantiation sys
      sysMissingStr = newLit(systemStr(sysName) & " uses " &
        id.commaSeparate(id.ecsSysRequirements(sys)) & ": ")
    
    var
      compCheck = newStmtList()
      first = true
    let
      sysCompComma = genSym(nskVar, "cComma")
    
    for compId in id.ecsSysRequirements(sys):
      checkedComps.incl compId
      
      let
        tyName = id.typeName(compId)
        compStr =
          if first: tyName
          else:
            first = false
            ", " & tyName
        compTy = ident tyName
    
      compCheck.add(quote do:
        if not(`entity`.hasComponent `compTy`):
          `joinComma`(`missingCompsStr`, `compStr`, `sysCompComma`)
          if `compId`.ComponentTypeId notin `totalMissing`:
            `totalMissing`.add `compId`.ComponentTypeId
      )
    
    # "incInt" (uses: AddOne, IntCont) is missing component(s): AddOne
    checks.add(quote do:
      if `entity` notin `sysVar`:
        var
          `missingCompsStr` = "missing "
          `sysCompComma`: bool
        `compCheck`
        `missingSystems` &= `sysMissingStr` & `missingCompsStr` & "\n"
    )

    #"System \"" & sys.name & "\" (" & $int(sysIdx) & ")"

    allSystemsStr &= systemStr(sysName) & "\n"

  result = quote do:
    block:
      {.line.}:
        template `joinComma`(str1, str2, comma: untyped) =
          if comma:
            str1 &= ", " & str2
          else:
            comma = true
            str1 &= str2
        
        var
          `missingSystems`: string
          `totalMissing`: seq[ComponentTypeId]

        `checks`
        if `missingSystems`.len > 0:
          var
            `allCompsComma`: bool
            allMissingStr: string
            existingSystems = `entity`.listSystems
          if existingSystems.len == 0:
            existingSystems = "<No systems used>\n"
          for c in `totalMissing`:
            let tn = c.typeName
            `joinComma`(allMissingStr, tn, `allCompsComma`)
            
          doAssert false, "\nEntityId " & $`entity`.entityId.int & 
            " expected systems:\n" & `allSystemsStr` &
            "\nCurrent systems:\n" & existingSystems & "\nMissing systems:\n" &
            $`missingSystems` & "\nCurrent components:\n" &
            `entity`.listComponents(false) & "\nComponents required: " &
            allMissingStr & "\n"

template expectSystems*(entity: EntityRef, systems: openArray[string]): untyped =
  expectSystems(defaultIdentity, entity, systems)

macro onEcsBuiltId*(id: static[EcsIdentity], code: NimNode) =
  ## Includes `code` immediately after makeEcs has finished.
  ## 
  ## Useful for initialisations and support routines using the ECS (for
  ## example for use within systems).
  id.add_onEcsBuiltCode code
  newStmtList()

macro onEcsBuilt*(code: untyped): untyped =
  ## Includes `code` immediately after makeEcs has finished.
  ## 
  ## Useful for initialisations and support routines using the ECS (for
  ## example for use within systems).
  defaultIdentity.add_onEcsBuiltCode code
  newStmtList()

proc makeEcs(id: EcsIdentity, entityOptions: EcsEntityOptions): NimNode =
  ## This macro seals and fully defines the ECS state.
  ## 
  ## Once completed you can:
  ## 
  ## * Create new entities,
  ## * Add/remove components from entities,
  ## * Systems are updated when components are added or removed.
  ## 
  ## To create the system execution procedures, use `commitSystems`.
  when defined(ecsLog) and not defined(ecsLogDetails):
    echo "Building ECS \"" & id.string & "\"..."

  id.set_entityOptions entityOptions

  when defined(ecsLogDetails):
    echo "[ Entity generation for \"" & id.string & "\"]"
    echo "Entity options:\n", entityOptions.repr, "\n"
  
  id.startOperation "Building ECS \"" & id.string & "\""

  result = newStmtList()

  result.add id.doStartLog()

  let
    unsealedSystems = id.allUnsealedSystems
    unsealedComponents = id.allUnsealedComponents

  id.ecsBuildOperation "component processing":
    for typeId in unsealedComponents:

      let
        # Walk the ownership for this component.
        deps = id.calcDependentOwners typeId
        systems = id.systems typeId

      # All systems that need to be involved in state
      # changes with this component.
      for sys in systems:
        id.add_linked typeId, sys

      for dep in deps:
        id.add_dependentOwners typeId, dep
        if dep notin systems:
          id.add_linked typeId, dep

      # Component event callbacks.
      let
        onAddCallbackForwardDecl = id.onAddCallbackForwardDeclNode(typeId)
        onRemoveCallbackForwardDecl = id.onRemoveCallbackForwardDeclNode(typeId)

      if onAddCallbackForwardDecl.len > 0:
        result.add onAddCallbackForwardDecl
      if onRemoveCallbackForwardDecl.len > 0:
        result.add onRemoveCallbackForwardDecl

  # Write the number of systems using each component to the log.
  addPerformanceLog(id)

  # Forward declarations for system added/removed callbacks.
  for sys in unsealedSystems:
    let
      addedDecl = id.onAddedCallbackDeclNode(sys)
      removedDecl = id.onRemovedCallbackDeclNode(sys)
    if addedDecl.len > 0:
      result.add addedDecl
    if removedDecl.len > 0:
      result.add removedDecl

  id.ecsBuildOperation "seal components":
    result.add sealComps(id)

  id.ecsBuildOperation "seal entities":
    result.add sealEntities(id)

  id.ecsBuildOperation "run time debug output":
    result.add sealRuntimeDebugging(id)

  id.ecsBuildOperation "state changes":
    result.add sealStateChanges(id)
  
  id.ecsBuildOperation "run time tools":
    result.add makeRuntimeTools(id)

  let invalidEvents = invalidSystemAdds(id)
  if invalidEvents[0]:
    error invalidEvents[1]

  id.ecsBuildOperation "run time construction":
    result.add makeRuntimeConstruction(id)

  id.ecsBuildOperation "user component event callbacks":
    for typeId in unsealedComponents:

      let
        onAddCallback = id.onAddCallbackNode(typeId)
        onRemoveCallback = id.onRemoveCallbackNode(typeId)

      if onAddCallback.len > 0:
        result.add onAddCallback
      if onRemoveCallback.len > 0:
        result.add onRemoveCallback

      id.add_ecsSealedComponents typeId

  id.ecsBuildOperation "user system event callbacks":
    for sys in unsealedSystems:

      let
        onSysAddedCallback = id.onAddedCallbackNode(sys)
        onSysRemovedCallback = id.onRemovedCallbackNode(sys)

      if onSysAddedCallback.len > 0:
        result.add onSysAddedCallback

      if onSysRemovedCallback.len > 0:
        result.add onSysRemovedCallback

  # Flag systems as generated.
  for sys in unsealedSystems:
    id.set_sealed sys, true

  # User code to run after everything's defined.
  result.add id.onEcsBuiltCodeNode

  when defined(ecsLogCode):
    id.ecsBuildOperation "Generate makeEcs() log":
      genLog("\n# makeEcs() code generation output:\n" & result.repr)
      result.add id.flushGenLog(defaultGenLogFilename)
  
  id.endOperation()
  when defined(ecsLog) or defined(ecsLogDetails):
    echo "ECS \"" & id.string & "\" built."

macro makeEcs*(id: static[EcsIdentity], entityOptions: static[EcsEntityOptions]): untyped =
  id.makeEcs(entityOptions)

macro makeEcs*(maxEnts: static[int] = defaultMaxEntities): untyped =
  let entOpts = ECSEntityOptions(maxEntities: maxEnts)
  defaultIdentity.makeEcs(entOpts)

macro makeEcs*(options: static[EcsEntityOptions]): untyped =
  defaultIdentity.makeEcs(options)

macro flushGenLog*: untyped =
  defaultIdentity.flushGenLog(defaultGenLogFilename)

import strutils

proc genRunProc(id: EcsIdentity, name: string): NimNode =
  ## Create a proc to call systems that haven't been committed.
  # The current order is stored as a macrocache seq of SystemIndex.
  let
    procName = ident name.toLowerAscii
    start = id.systemOrderStart()
    commitOrder = id.systemOrder()
    currentCommits = commitOrder[start .. ^1]

  #  
  var sysCalls = newStmtList()

  when defined(ecsLog) or defined(ecsLogDetails):
    echo "Wrapper proc `" & name & "()` execution order:"
  
  for system in currentCommits:
    let sysName = id.getSystemName(system)
    sysCalls.add nnkCall.newTree(ident doProcName(sysName))
    
    when defined(ecsLog) or defined(ecsLogDetails):
      echo "  " & sysName

  result = quote do:
    proc `procName`* =
      `sysCalls`
  
  # Set the starting point to take for the next set of commits.
  id.set_systemOrderStart commitOrder.len

macro commitSystems*(id: static[EcsIdentity], procName: static[string]): untyped =
  ## The macro will output the system execution procedures, and should be run after `makeEcs`.
  ## 
  ## Each procedure is defined as the system's name prefixed with "do", eg; a system "foo" generates `doFoo()`.
  ## 
  ## These procedures perform all the actions within `makeSystem`, and can be considered as "polling" or "ticking" the ECS state for this system.
  ## 
  ## If `procName` is given, a wrapper proc is generated that includes all the procedures since the last `commitSystems` was invoked.
  result = newStmtList()

  let
    commitHeader = "for \"" & id.string & "\""
    logTitle =
      if procName != "":
        commitHeader & ", wrapped to proc `" & procName & "()`"
      else:
        commitHeader

  when defined(ecsLog) and not defined(ecsLogDetails):
    echo "Committing systems " & commitHeader
  
  id.startOperation "Commit systems " & commitHeader
  
  var committedBodies = id.ecsSysBodiesAdded
  for sys in id.ecsSysUncommitted:
    if sys notin committedBodies:
      let definition = id.definition sys
      if definition.len > 0:

        id.startOperation "Adding system proc for \"" & id.getSystemName(sys) & "\""
        
        result.add definition
        id.add_ecsSysBodiesAdded sys
        committedBodies.add sys

        id.endOperation

  if procName != "":
    # Generate wrapper proc.
    result.add id.genRunProc(procName)

  # Check for defined but not committed systems.
  var noBodies: seq[string]
  let
    bodiesAdded = id.ecsSysBodiesAdded
    sysDefined = id.ecsSysDefined
    
  for system in sysDefined:
    if system notin bodiesAdded:
      noBodies.add "\"" & id.getSystemName(system) & "\""
  
  if noBodies.len > 0:
    var outputStr = noBodies[0]
    for i in 1 ..< noBodies.len:
      outputStr &= ", " & noBodies[i]
    let noBodyStr = "Systems are defined that do not have bodies: " & `outputStr`
    result.add(quote do:
      {.hint: `noBodyStr`.}
    )

  let logCodeComment = "Commit systems " & logTitle & "\n"
  genLog  "\n# " & logCodeComment &
          "# " & '-'.repeat(logCodeComment.len) & "\n" &
          result.repr
  
  when defined(ecsLog) or defined(ecsLogDetails):
    echo "Systems committed " & logTitle & "."

  result.add id.flushGenLog(defaultGenLogFilename)
  
  id.endOperation

template commitSystems*(procName: static[string]): untyped =
  defaultIdentity.commitSystems(procName)
