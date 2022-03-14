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

import macros, sharedtypes, components
import private/[statechangeutils, statechangegen, utils, ecsstatedb, eventutils, mutationtracking]
import strutils, tables, sets, macrocache

macro componentToSysRequirements*(id: static[EcsIdentity], varName: untyped): untyped =
  ## Create a static sequence that matches componentTypeId to an array of indexes into systemNodes
  ## The result is an array that returns a seq of SystemIndex by ComponentTypeId
  ##   `array[ComponentTypeId, seq[SystemIndex]]`
  ## This means we can check at run time what systems are required per component.
  ## Note this must only be used after seal has been called, otherwise the compile-time lists will be
  ## incomplete.
  var compSys = newSeqOfCap[seq[SystemIndex]](id.components.len)
  for ty in id.allComponentsSeq:
    compSys.add id.systems(ty)

  let res = compSys.genStaticArray()

  result = quote do:
    const `varName` = `res`

#---------------
# User event API
#---------------

proc setupOnAddCallback(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "
  
  id.add_onAddCallback(typeIndex, actions)

  newStmtList()

macro onAddCallback*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when this component type is added.
  ## Each invocation will append to the code that will be inserted.
  id.setupOnAddCallback(typeToUse, actions)

macro onAddCallback*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when this component type is added.
  ## Each invocation will append to the code that will be inserted.
  defaultIdentity.setupOnAddCallback(typeToUse, actions)


proc setupOnRemoveCallback(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "

  id.add_onRemoveCallback(typeIndex, actions)
  
  newStmtList()

macro onRemoveCallback*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when this component type is removed.
  ## Each invocation will append to the code that will be inserted.
  defaultIdentity.setupOnRemoveCallback(typeToUse, actions)

macro onRemoveCallback*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when this component type is removed.
  ## Each invocation will append to the code that will be inserted.
  id.setupOnRemoveCallback(typeToUse, actions)


# The following events are inserted inline during code generation.

proc setupOnInit(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "
  
  id.add_onInitCode(typeIndex, actions)
  result = newStmtList()

macro onInit*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated.
  ## Each invocation will append to the code that will be inserted.
  id.setupOnInit(typeToUse, actions)

macro onInit*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated.
  ## Each invocation will append to the code that will be inserted.
  defaultIdentity.setupOnInit(typeToUse, actions)


proc setupOnInterceptUpdate(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "

  id.add_onInterceptUpdate(typeIndex, actions)

  result = newStmtList()

macro onInterceptUpdate*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## The data being added can be accessed in `curComponent`, and is of
  ## the native type, not the instance type.
  ## Each invocation will append to the code that will be inserted.
  ## Note: When this is hooked, the user must call `commit` if they don't
  ## want the update parameters to be ignored.
  id.setupOnInterceptUpdate(typeToUse, actions)

macro onInterceptUpdate*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## The data being added can be accessed in `curComponent`, and is of
  ## the native type, not the instance type.
  ## Each invocation will append to the code that will be inserted.
  ## Note: When this is hooked, the user must call `commit` if they don't
  ## want the update parameters to be ignored.
  defaultIdentity.setupOnInterceptUpdate(typeToUse, actions)


macro onUpdate*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## The data being added can be accessed in `curComponent`, and is of
  ## the native type, not the instance type.
  ## Each invocation will append to the code that will be inserted.
  ## Note: When this is hooked, the user must call `commit` if they don't
  ## want the update parameters to be ignored.
  id.setupOnInterceptUpdate(typeToUse, actions)

macro onUpdate*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## The data being added can be accessed in `curComponent`, and is of
  ## the native type, not the instance type.
  ## Each invocation will append to the code that will be inserted.
  ## Note: When this is hooked, the user must call `commit` if they don't
  ## want the update parameters to be ignored.
  defaultIdentity.setupOnInterceptUpdate(typeToUse, actions)


proc setupOnDelete(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "

  id.add_onFinalisationCode(typeIndex, actions)

  result = newStmtList()

macro onDelete*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component is deleted.
  ## Each invocation will append to the code that will be inserted.
  id.setupOnDelete(typeToUse, actions)

macro onDelete*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component is deleted.
  ## Each invocation will append to the code that will be inserted.
  defaultIdentity.setupOnDelete(typeToUse, actions)


proc setupOnAdd(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "

  id.add_onAddToEntCode(typeIndex, actions)

  result = newStmtList()

macro onAdd*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is added to an entity.
  ## Each invocation will append to the code that will be inserted.
  id.setupOnAdd(typeToUse, actions)

macro onAdd*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is added to an entity.
  ## Each invocation will append to the code that will be inserted.
  defaultIdentity.setupOnAdd(typeToUse, actions)


proc setupOnRemove(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "

  id.add_onRemoveFromEntCode(typeIndex, actions)

  result = newStmtList()

macro onRemove*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from an entity.
  ## Each invocation will append to the code that will be inserted.
  id.setupOnRemove(typeToUse, actions)

macro onRemove*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from an entity.
  ## Each invocation will append to the code that will be inserted.
  defaultIdentity.setupOnRemove(typeToUse, actions)


proc setupOnSystemAdd(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  ## Add some code to be executed when a component of this type is added to any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "

  id.add_onAddAnySystemCode(typeIndex, actions)

  result = newStmtList()

macro onSystemAdd*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnSystemAdd(typeToUse, actions)

macro onSystemAdd*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnSystemAdd(typeToUse, actions)


proc setupOnSystemAddTo(id: EcsIdentity, typeToUse: NimNode, systemName: string, actions: NimNode): NimNode =
  let
    sysIndex = findSystemIndex(id, systemName)
    typeIndex = id.typeStringToId($typeToUse)

  if typeIndex.int == 0:
    error "Cannot find type " & $typeIndex.int & " in registered components "
  if not sysIndex.found:
    error "Cannot find system \"" & systemName & "\" in defined systems: " & id.commaSeparate(id.allSystemsSeq)

  id.add_onAddToCode(sysIndex.index, typeIndex, actions)

  # Record the request to use this event.
  # This is checked during sealing to ensure it is allowed.
  id.add_onAddToSystemComp(sysIndex.index, typeIndex)

  result = newStmtList()

macro onSystemAddTo*(id: static[EcsIdentity], typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  id.setupOnSystemAddTo(typeToUse, systemName, actions)

macro onSystemAddTo*(typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  defaultIdentity.setupOnSystemAddTo(typeToUse, systemName, actions)


proc setupOnSystemRemove(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "
  
  id.add_onRemoveAnySystemCode(typeIndex, newBlockStmt(actions))

  result = newStmtList()

macro onSystemRemove*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code to be inserted.
  id.setupOnSystemRemove(typeToUse, actions)

macro onSystemRemove*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code to be inserted.
  defaultIdentity.setupOnSystemRemove(typeToUse, actions)


proc setupOnSystemRemoveFrom(id: EcsIdentity, typeToUse: NimNode, systemName: string, actions: NimNode): NimNode =
  let
    sysFound = findSystemIndex(id, systemName)
    typeIndex = id.typeStringToId($typeToUse)
  
  if typeIndex.int == 0:
    error "Cannot find type " & $typeToUse & " in registered components "
  if not sysFound.found:
    error "Cannot find system \"" & systemName & "\" in defined systems: " & id.commaSeparate(id.allSystemsSeq)
  
  id.add_onRemoveFromCode(sysFound.index, typeIndex, actions)

  # Record the request to use this event.
  # This is checked during sealing to ensure it is allowed.
  id.add_onRemoveFromSystemComp(sysFound.index, typeIndex)

  result = newStmtList()

macro onSystemRemoveFrom*(id: static[EcsIdentity], typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code to be inserted.
  id.setupOnSystemRemoveFrom(typeToUse, systemName, actions)

macro onSystemRemoveFrom*(typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code to be inserted.
  defaultIdentity.setupOnSystemRemoveFrom(typeToUse, systemName, actions)


proc addEntityStateChange(id: EcsIdentity, actions: NimNode) =
  var
    curCode = id.onEntityStateChange

  if curCode.isNil:
    curCode = newStmtList()
  
  curCode.add newBlockStmt(actions)

  id.set_onEntityStateChange curCode

macro onEntityChangeId*(id: static[EcsIdentity], actions: untyped): untyped =
  ## This event is called for an entity when components are added or
  ## removed.
  ## 
  ## The following component changes are distinguished:
  ##   newEntityWith, add, construct, clone, remove, and delete
  ##
  ## Within the event you have access to the following injected variables:
  ##  - `entity`: The current entity the state change applies to.
  ##  - `state`: The `EntityChangeEvent` enum value representing the state
  ##    change.
  ##  - `types`: A list of `ComponentTypeId` for each component involved.
  ## 
  ## Each invocation of `onEntityChange` appends `actions` to the output.
  ## 
  ## For states that add components, this event occurs after the
  ## components have been added.
  ##
  ## For states that remove components, this event occurs before the
  ## components have been removed.
  ## 
  ## Example use:
  ## 
  ##    onEntityChange:
  ##      # Output the details of the entity's state change.
  ##      echo "Change: ", entity.entityId.int, ": ", state, ": ", types
  ## 
  id.addEntityStateChange actions
  newStmtList()

macro onEntityChange*(actions: untyped): untyped =
  ## This event is called for an entity when components are added or
  ## removed.
  ## 
  ## The following component changes are distinguished:
  ##   newEntityWith, add, construct, clone, remove, and delete
  ##
  ## Within the event you have access to the following injected variables:
  ##  - `entity`: The current entity the state change applies to.
  ##  - `state`: The `EntityChangeEvent` enum value representing the state
  ##    change.
  ##  - `types`: A list of `ComponentTypeId` for each component involved.
  ## 
  ## Each invocation of `onEntityChange` appends `actions` to the output.
  ## 
  ## For states that add components, this event occurs after the
  ## components have been added.
  ##
  ## For states that remove components, this event occurs before the
  ## components have been removed.
  ## 
  ## Example use:
  ## 
  ##    onEntityChange:
  ##      # Output the details of the entity's state change.
  ##      echo "Change: ", entity.entityId.int, ": ", state, ": ", types
  ## 
  defaultIdentity.addEntityStateChange actions
  newStmtList()


#--------------
# State changes
#--------------


proc doNewEntityWith(id: EcsIdentity, passedValues: NimNode): NimNode {.compileTime.} =
  ## Output updates to the entity and relevant systems.
  
  result = newStmtList()

  let
    entitySym = genSym(nskLet, "entity")

  var
    details = id.newStateChangeDetails(scdkNewEntity, passedValues)

  details.suffix =
    if defined(ecsNoMangle): ""
    else: signatureHash entitySym
  
  var
    satisfied: SystemSet
  
  for sys in id.satisfiedSystems(details.passed):
    # Update systems that are full satisfied by the parameters.
    satisfied.incl sys
    id.applyChange entitySym, details, SystemChange(sys: sys, kind: sckAdd)

  var
    unsatisfiedComps: seq[ComponentTypeId]
    needComps: seq[ComponentTypeId]

  # Check any owned components passed fully satisfy their systems.

  for c in details.passed:
    let
      owner = id.systemOwner c

    if owner != InvalidSystemIndex and owner notin satisfied:
      unsatisfiedComps.add c

      for owned in id.ecsSysRequirements(owner):
        if owned notin details.passedSet:
          needComps.add owned

  if unsatisfiedComps.len > 0:
    error "Cannot instantiate this entity as the components [" &
      id.commaSeparate(unsatisfiedComps) & "] needs [" &
      id.commaSeparate(needComps) & "] to satisfy their owning systems"

  # Generate system state changes and events.

  id.buildStateChange entitySym, details

  # Assemble generated code.

  let
    newDecls = details.newDecls
    sysUpdates = details.systemUpdates
    addToEnt = id.addToEntityList(entitySym, details.passed, details.suffix)

    allEvents = details.allEvents
    defEvents = details.deferredEvents

    opStr =  "newEntityWith: " & id.commaSeparate(details.passed)

  var
    newEntEvent = newStmtList()
    newEntCtx = newEventContext(entitySym, details.passed)

  newEntEvent.invokeEvent(id, newEntCtx, ekNewEntityWith)
  
  var
    body = quote do:
      let `entitySym` {.inject.} = newEntity()
      `newDecls`
      `sysUpdates`
      `addToEnt`
      `newEntEvent`
      `allEvents`
      `defEvents`
  let
    compInts = details.passed.toInts
  
  body.trackMutation(id, ekNewEntityWith, compInts, announce = false)

  result = quote do:
    block:
      static: startOperation(EcsIdentity(`id`), `opStr`)
      `body`
      static: endOperation(EcsIdentity(`id`))
      `entitySym`

  genLog "\n# macro newEntityWith(" & id.commaSeparate(details.passed) & "):\n", result.repr


proc doAddComponents(id: EcsIdentity, entity: NimNode, componentValues: NimNode): NimNode =
  ## Output minimised code to add the components in `componentList` to
  ## an entity.

  let
    entitySym = genSym(nskLet, "entity")
    defineEntity =
      quote do:
        let `entitySym` = `entity`

  var
    details = id.newStateChangeDetails(scdkAdd, componentValues)
  
  details.suffix =
    if defined(ecsNoMangle): ""
    else: signatureHash entitySym

  var
    returnType = nnkPar.newTree()
  
  for i in 0 ..< details.passed.len:
    let
      c = id.getInfo details.passed[i]
      typeStr = c.name
      fieldStr = c.fetchedName details.suffix
      typeIdent = ident typeStr
    
    # Assert component is not already in the entity.
    details.asserts.add(quote do:
      {.line.}:
        assert `typeIdent` notin `entitySym`,
          "Entity already contains component '" & `typeStr` & "'"
    )

    # Add instances to the return type.
    returnType.add nnkExprColonExpr.newTree(
      ident c.lcName,
      ident fieldStr
    )

  var
    negatedSystems: SystemSet

  for change in id.addStateChanges(details.passed, details.passedSet):
    if change.kind == sckRemove:
      negatedSystems.incl change.sys
    id.applyChange entitySym, details, change

  id.buildStateChange entitySym, details
  
  var
    op = newStmtList()
    opStr = "Add components " & id.commaSeparate(details.passed)
    addEntEvent = newStmtList()
    addEntCtx = newEventContext(entitySym, details.passed)
  let
    compInts = details.passed.toInts
    negatedSystemsSeq = negatedSystems.toSeq
  
  addEntEvent.invokeEvent(id, addEntCtx, ekAddComponents)

  op.add defineEntity
  op.add details.asserts
  op.add details.newDecls
  op.add details.fetches
  op.add details.systemUpdates
  op.add id.addToEntityList(entitySym, details.passed, details.suffix)

  op.add addEntEvent
  op.add details.allEvents
  op.add details.deferredEvents

  op.trackMutation(id, ekAddComponents, compInts, announce = false)

  let
    cacheId = quote do: EcsIdentity(`id`)
  result = quote do:
    block:
      static:
        startOperation(`cacheId`, `opStr`)
        if `cacheId`.ecsSysIterating > 0:
          if `cacheId`.inSystemIndex in `negatedSystemsSeq`:
            # This addComponent negates the currently iterating system,
            # and therefore acts as a remove operation, so we need to
            # add in extra loop checking logic.
            `cacheId`.set_sysRemoveAffectedThisSystem true
      
      `op`
      
      static:
        endOperation(`cacheId`)
      
      `returnType`

  genLog "\n# macro addComponents(" & id.commaSeparate(details.passed) & "):\n", result.repr


proc doRemoveComponents(id: EcsIdentity, entity: NimNode, componentList: NimNode): NimNode =
  #[
    Important note!
    If you call removeComponent whilst in a system using that component, the current `item` will change!
    In this case, item.entity.removeComponent will cause item.entity and its components to be different.
    This happens because `entity.removeComponent` and `entity.delete` remove items from systems by swapping
    the item with the last one in the list and reducing the list length.
  ]#

  var
    details = id.newStateChangeDetails(scdkRemove, componentList)
  let
    entitySym = genSym(nskLet, "entity")
    defineEntity =
      quote do:
        let `entitySym` = `entity`

  # Extend to include dependent owned components.
  details.passed = id.inclDependents(details.passed)
  
  details.passedSet = details.passed.toHashSet
  details.suffix =
    if defined(ecsNoMangle): ""
    else: signatureHash entitySym

  var
    relevantSystems: SystemSet

  for change in id.removeStateChanges(details.passed):
    relevantSystems.incl change.sys
    id.applyChange entitySym, details, change

  id.buildStateChange entitySym, details

  let
    opStr = "removeComponents: " & id.commaSeparate(details.passed)

    cacheId = quote do:
      EcsIdentity(`id`)
    
    relevantSystemsSeq = relevantSystems.toSeq
    relevantSystemInts = relevantSystemsSeq.toIntList
    
    catchUseInSystem = quote do:
      static:
        startOperation(`cacheId`, `opStr`)

        if `cacheId`.ecsSysIterating > 0:
          if `cacheId`.inSystemIndex in `relevantSystemsSeq`:
            # Calling removeComponent from within a system that uses the component.
            # We don't know if its the current row's entity or some other entity.
            `cacheId`.set_sysRemoveAffectedThisSystem true
        else:
          if `cacheId`.ecsEventEnv.len > 0:
            if eventOccurred(`cacheId`, {ekAddCB, ekRowAddedCB, ekRemoveCB, ekRowRemovedCB},
                `relevantSystemInts`):
              # Callback events set sysRemoveAffectedThisSystem to ensure
              # 'item' catches use after remove.
              `cacheId`.set_sysRemoveAffectedThisSystem true


    endOperation = quote do:
      static: endOperation(`cacheId`)

  var
    op = newStmtList()
    remEntEvent = newStmtList()
    remEntCtx = newEventContext(entitySym, details.passed)
  
  remEntEvent.invokeEvent(id, remEntCtx, ekRemoveComponents)

  op.unpack catchUseInSystem
  op.unpack defineEntity
  op.unpack details.asserts
  op.unpack details.fetches
  op.unpack remEntEvent
  op.unpack details.allEvents
  op.unpack id.removeFromEntityList(entitySym, details.passedSet + details.compRemoves, details.suffix)
  op.unpack details.systemUpdates
  op.unpack details.deferredEvents
  op.unpack endOperation

  let
    compInts = details.passed.toInts
  
  op.trackMutation(id, ekRemoveComponents, compInts, announce = false)

  result = newBlockStmt(op)

  genLog "\n# macro removeComponents(" & id.commaSeparate(details.passed) & "):\n", result.repr


proc makeStateChanges*(id: EcsIdentity): NimNode =
  let
    entity = ident "entity"
    identity = quote do: EcsIdentity(`id`)
    res = ident "result"
    componentList = ident "componentList"

  result = quote do:
    
    macro newEntityWith*(`componentList`: varargs[typed]): untyped =
      ## Create an entity with the parameter components.
      ## This macro statically generates updates for only systems
      ## entirely contained within the parameters and ensures no
      ## run time component list iterations and associated checks.
      doNewEntityWith(`identity`, `componentList`)

    macro addComponents*(id: static[EcsIdentity], `entity`: EntityRef, `componentList`: varargs[typed]): untyped =
      ## Add components to a specific identity.
      doAddComponents(id, `entity`, `componentList`)

    macro addComponents*(`entity`: EntityRef, `componentList`: varargs[typed]): untyped =
      ## Add components to an entity and return a tuple containing
      ## the instances.
      doAddComponents(`identity`, `entity`, `componentList`)

    macro add*(`entity`: EntityRef, `componentList`: varargs[typed]): untyped =
      ## Add components to an entity and return a tuple containing
      ## the instances.
      doAddComponents(`identity`, `entity`, `componentList`)

    macro removeComponents*(`entity`: EntityRef, `componentList`: varargs[typed]): untyped =
      ## Remove components from an entity.
      doRemoveComponents(`identity`, `entity`, `componentList`)
    
    macro remove*(`entity`: EntityRef, `componentList`: varargs[typed]): untyped =
      ## Remove a component from an entity.
      doRemoveComponents(`identity`, `entity`, `componentList`)

    macro removeComponent*(`entity`: EntityRef, component: typed) =
      ## Remove a component from an entity.
      doRemoveComponents(`identity`, `entity`, component)

    template removeComponents*(entity: EntityRef, compList: ComponentList) =
      ## Remove a run time list of components from the entity.
      for c in compList:
        assert c.typeId != InvalidComponent
        caseComponent c.typeId:
          removeComponent(entity, componentType())

    # Generic state change utilities.

    template add*(`entity`: EntityRef, component: ComponentTypeclass) =
      `entity`.addComponent component


  if id.private:
    # Note: functionality here needs to be discardable and therefore
    # must be made with procedures.
    #
    # However it's important not to force generation of these procedures
    # for every type because 1) owned component dependencies might not
    # make this possible and 2) extra time is spent compiling operations
    # even if not used. Therefore, generics are used to allow on-demand
    # generation by type.
    #
    # Unfortunately, currently generics don't resolve component types
    # correctly within `block:` statements across module boundaries.
    # See: https://github.com/nim-lang/Nim/issues/13747
    #
    # This means 'private ECS' generation can fail and report that
    # component types are not declared.
    #
    # To work around this, a template wraps these procs for each type to
    # allow on demand generation for singular components and support
    # `{.discardable.}`, at the cost of extra compilation time and
    # code duplication.

    for c in id.building(id.allUnsealedComponents):
      let
        cType = ident c.name
        cInst = c.instanceTy

      if not c.isOwned or (c.isOwned and id.ecsOwnedComponents(c.owner).len == 1):
        # Components owned by systems with other owned components cannot be
        # added as single operations.
        result.add(
          quote do:
            template addComponent*(`entity`: EntityRef, component: `cType`): `cInst` =
              ## Add a single component to `entity` and return the instance.
              block:
                proc ac(e: EntityRef, c: `cType`): `cInst` {.discardable.} =
                  e.addComponents(c)[0]
                `entity`.ac(component)

            template addOrUpdate*(`entity`: EntityRef, component: `cType`): `cInst` =
              ## Add `component` to `entity`, or if `component` already exists, overwrite it.
              ## Returns the component instance.
              proc aou(e: EntityRef, c: `cType`): `cInst` {.discardable.} =
                let
                  fetched = e.fetchComponent typedesc[`cType`]
                if fetched.valid:
                  # Replace original. No further work is required as the types or indexes have not been updated.
                  update(fetched, c)
                  `res` = fetched
                else:
                  # Add as normal.
                  `res` = addComponent(e, c)
              aou(`entity`, component)

            template addIfMissing*(`entity`: EntityRef, component: `cType`): `cInst` =
              ## Add a component only if it isn't already present.
              ## If the component is already present, no changes are made and an invalid result is returned.
              ## If the component isn't present, it will be added and the instance is returned.
              proc aim(e: EntityRef, c: `cType`): `cInst` {.discardable.} =
                if not e.hasComponent `cType`:
                  `res` = addComponent(e, c)
              aim(`entity`, component)

            template fetchOrAdd*(`entity`: EntityRef, component: typedesc[`cType`]): `cInst` =
              ## Fetch an existing component type if present, otherwise add
              ## the component type and return the instance.
              ## 
              ## This is useful when you always want a valid component
              ## instance returned, but don't want to overwrite existing
              ## data.
              proc foa(e: EntityRef, c: typedesc[`cType`]): `cInst` {.discardable.} =
                `res` = e.fetchComponent typedesc[`cType`]
                if not `res`.valid:
                  `res` = addComponent(e, c())
              foa(`entity`, component)
        )
  else:
    # Public ECS outputs can rely on generics.

    result.add(
      quote do:
        proc addComponent*[T: ComponentTypeclass](`entity`: EntityRef, component: T): auto {.discardable.} =
          ## Add a single component to `entity` and return the instance.
          `entity`.addComponents(component)[0]

        proc addOrUpdate*[T: ComponentTypeclass](`entity`: EntityRef, component: T): auto {.discardable.} =
          ## Add `component` to `entity`, or if `component` already exists, overwrite it.
          ## Returns the component instance.
          let
            fetched = `entity`.fetchComponent typedesc[T]
          
          if fetched.valid:
            # Replace original. No further work is required as the types or indexes have not been updated.
            update(fetched, component)
            `res` = fetched
          else:
            # Add as normal.
            `res` = addComponent(`entity`, component)

        proc addIfMissing*[T: ComponentTypeclass](`entity`: EntityRef, component: T): auto {.discardable.} =
          ## Add a component only if it isn't already present.
          ## If the component is already present, no changes are made and an invalid result is returned.
          ## If the component isn't present, it will be added and the instance is returned.

          if not `entity`.hasComponent typedesc[T]:
            `res` = addComponent(`entity`, component)

        proc fetchOrAdd*[T: ComponentTypeclass](`entity`: EntityRef, component: typedesc[T]): auto {.discardable.} =
          ## Fetch an existing component type if present, otherwise add
          ## the component type and return the instance.
          ## 
          ## This is useful when you always want a valid component
          ## instance returned, but don't want to overwrite existing
          ## data.
          `res` = `entity`.fetchComponent typedesc[T]
          if not `res`.valid:
            `res` = addComponent(`entity`, component())
    )

  result.add(quote do:
    template addComponents*(`entity`: EntityRef, components: ComponentList) =
      ## Add components from a list.
      ## Each component is assembled by it's run time `typeId`.
      static: startOperation(`identity`, "Add components from ref list")
      {.line.}:
        for c in components:
          caseComponent c.typeId:
            discard `entity`.addComponent componentRefType()(c).value
      static: endOperation(`identity`)


    template add*(`entity`: EntityRef, components: ComponentList) =
      ## Add components from a list.
      ## Each component is assembled by its run time `typeId`.
      addComponents(`entity`, components)


    template addIfMissing*(entity: EntityRef, components: ComponentList) =
      ## Add components from a list if they're not already present.
      {.line.}:
        for c in components:
          caseComponent c.typeId:
            entity.addIfMissing componentRefType()(c).value


    template addOrUpdate*(entity: EntityRef, components: ComponentList) =
      ## Add or update components from a list.
      {.line.}:
        for c in components:
          caseComponent c.typeId:
            discard addOrUpdate(entity, componentRefType()(c).value)


    template updateComponents*(entity: EntityRef, components: ComponentList) =
      ## Updates existing components from a list.
      ## Components not found on the entity exist are ignored.
      {.line.}:
        for c in components:
          caseComponent c.typeId:
            let inst = entity.fetchComponent componentType()
            if inst.valid:
              inst.update componentRefType()(c).value


    template update*(entity: EntityRef, components: ComponentList) =
      ## Updates existing components from a list.
      ## Components not found on the entity are ignored.
      updateComponents(entity, components)

  )
  
  genLog "\n# State changes operations:\n", result.repr


proc doFetchComponents(id: EcsIdentity, entity: NimNode, components: NimNode): NimNode =
  ## Generate code to look up a list of components from an entity,
  ## returning a tuple of fetched instances for the component.
  ## 
  ## Components that were not found will be `InvalidComponent`.
  ## 
  ## Example:
  ## 
  ##   let results = entity.fetch Comp1, Comp2
  ##   echo results.comp1
  ##   echo results.comp2

  var
    resultTuple = nnkTupleConstr.newTree()
    resultSym = genSym(nskLet, "fetch")
  let
    suffix =
      if defined(ecsNoMangle): ""
      else: signatureHash resultSym
    
    passedTypes = toTypeList(id, components)

  for c in building(id, passedTypes):
    let
      field = ident c.lcName
      value = c.fetchedIdent suffix
  
    resultTuple.add newColonExpr(field, value)
  
  var
    fetchOp = newStmtList()

  when compileOption("assertions"):
    fetchOp.add(
      quote do:
        {.line.}:
          assert `entity`.alive,
            "Fetch component on a dead entity. Entity ID: " & $`entity`.entityId.int &
            ", Instance: " & $`entity`.instance.int    
    )
  fetchOp.buildFetchComponents(id, entity, passedTypes, suffix, earlyExit = true, byFetchedIdent)
  fetchOp.add resultTuple
  
  result =
    quote do:
      block:
        let `resultSym` = `fetchOp`
        `resultSym`

  genLog "\n## macro fetchComponents(" & id.commaSeparate(passedTypes) & "):\n", result.repr


proc makeFetchComponents*(id: EcsIdentity): NimNode =
  let
    entity = ident "entity"
    ecsId = quote do: EcsIdentity(`id`)
  
  quote do:
    macro fetchComponents*(`entity`: EntityRef, components: varargs[typed]): untyped =
      ## Generate code to look up a list of components from an entity,
      ## returning a tuple of fetched instances for the component.
      ## 
      ## Components that were not found will be `InvalidComponent`.
      ## 
      ## Example:
      ## 
      ##   let results = entity.fetch Comp1, Comp2
      ##   echo results.comp1
      ##   echo results.comp2
      doFetchComponents(`ecsId`, `entity`, components)

    template fetch*(`entity`: EntityRef, components: varargs[typed]): untyped =
      fetchComponents(`entity`, components)

    template fetchComponent*(`entity`: EntityRef, t: typedesc): auto =
      ## Looks up and returns the instance of the component, which allows direct field access.
      ## Returns default no component index if the component cannot be found.
      ## Eg;
      ##   let comp = entity.fetchComponent CompType  # Will be of type CompTypeInstance
      ##   comp.x = 3 # Edit some supposed fields for this component.
      fetchComponents(`entity`, t)[0]

    template fetch*(`entity`: EntityRef, component: typedesc): auto =
      fetchComponent(`entity`, component)


proc clearAllEntComponentRefs(entityId: NimNode, componentStorageFormat: ECSCompStorage): NimNode =
  case componentStorageFormat
  of csSeq:
    quote do:
      for compRef in entityData(`entityId`).componentRefs:
        caseComponent compRef.typeId:
          componentDel(componentInstanceType()(compRef.index))
      entityData(`entityId`).componentRefs.setLen(0)
  of csArray:
    quote do:
      for compIdx in 0 ..< entityData(`entityId`).nextCompIdx:
        let curCompRef = entityData(`entityId`).componentRefs[compIdx]
        caseComponent curCompRef.typeId:
          componentDel(componentInstanceType()(curCompRef.index))
      entityData(`entityId`).nextCompIdx = 0
  of csTable:
    quote do:
      for compPair in entityData(`entityId`).componentRefs.pairs:
        caseComponent compPair[1].typeId:
          componentDel(componentInstanceType()(compPair[1].index))
      entityData(`entityId`).componentRefs.clear


proc recyclerAdd(ecStateNode, entIdNode: NimNode, recyclerFormat: ECSRecyclerFormat): NimNode =
  case recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.add `entIdNode`
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      let nextIdx = `ecStateNode`.`rLen`
      assert nextIdx < `ecStateNode`.entityRecycler.len
      
      `ecStateNode`.entityRecycler[nextIdx] = `entIdNode`
      `ecStateNode`.`rLen` += 1


proc recyclerClear*(ecStateNode: NimNode, recyclerFormat: ECSRecyclerFormat): NimNode =
  case recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.setLen 0
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      `ecStateNode`.`rLen` = 0


proc makeDelete*(id: EcsIdentity): NimNode =
  ## Generates delete procedures for the current entity.
  ## Delete will be created with all the systems that have been seen since the last
  ## `makeEcs` invocation.
  let
    entitySym = ident "entity"

  # This operation parses the entity's components by building 'case'
  # statements that process associated systems.
  # with each component.

  let
    allComponents = id.allUnsealedComponents
    curComp = genSym(nskForVar, "curComp")
    sysProcessed = genSym(nskVar, "sysProcessed")

    suffix =
      if defined(ecsNoMangle): ""
      else: signatureHash sysProcessed

  var
    deleteCore = newStmtList()
    caseSysRemoves = nnkCaseStmt.newTree(quote do: `curComp`.typeId.int)
    caseEvents = nnkCaseStmt.newTree(quote do: `curComp`.typeId.int)
    hasSysOps, hasUserEvents: bool
    sysEnum = ident systemsEnumName()

  deleteCore.add(
    quote do:
      var `sysProcessed`: set[`sysEnum`]
  )

  for c in id.building allComponents:
    # Populate case statements for events and each component.

    var
      details = newStateChangeDetails(scdkDelete)
    let
      instTy = c.instanceTy
    
    details.passed = @[c.typeId]
    details.passedSet = details.passed.toHashSet
    details.suffix = suffix
    details.iterating = quote do:
      `instTy`(`curComp`.index)
    details.sysProcessed = sysProcessed

    # Calculate the effect of removing this component.
    for sys in id.linked(c.typeId):
      id.applyChange(entitySym, details, SystemChange(kind: sckRemove, sys: sys))

    # Generate code to remove the component.
    id.buildStateChange entitySym, details

    if details.deferredEvents.len > 0 or details.allEvents.len > 0:
      let
        allEvents = details.allEvents
        defEvents = details.deferredEvents
        events = quote do:
          `allEvents`
          `defEvents`
      
      # Events are parsed separately in their own case statement.
      hasUserEvents = true

      caseEvents.add nnkOfBranch.newTree(
        newLit c.typeId.int,
        events
      )

    # The state transition parse.
    if details.systemUpdates.len > 0:
      hasSysOps = true

      caseSysRemoves.add nnkOfBranch.newTree(
        newLit c.typeId.int,
        details.systemUpdates
      )

  let
    disc = nnkDiscardStmt.newTree(newEmptyNode())
  
  if hasUserEvents:
    caseEvents.add nnkElse.newTree(disc)
    deleteCore.add entitySym.iterateComponents(curComp, caseEvents)
    deleteCore.add(quote do:
      `sysProcessed` = {}
      )
  
  if hasSysOps:
    caseSysRemoves.add nnkElse.newTree(disc)
    deleteCore.add entitySym.iterateComponents(curComp, caseSysRemoves)

  let
    cacheId = quote do: EcsIdentity(`id`)
    
    storageVar = ident entityStorageVarName()
    entIdIdent = ident "entityId"
    clearComponents = clearAllEntComponentRefs(entIdIdent, id.componentStorageFormat)

    entOpts = id.entityOptions
    gcCleanup =
      case entOpts.entityStorageFormat
      of esSeq, esArray: newStmtList()
      of esPtrArray:
        if entOpts.componentStorageFormat in [csSeq, csTable]:
          quote do:
            GC_Unref(entityData(`entIdIdent`).componentRefs)  # TODO: this needs testing and might need a rework.
        else:
          newStmtList()
    recFormat = entOpts.recyclerFormat
    recyclerAdd = storageVar.recyclerAdd(entIdIdent, recFormat)
    recyclerClear = storageVar.recyclerClear(recFormat)
    initSet =
      if entOpts.useSet:
        quote do:
          entityData(`entIdIdent`).exists = {}        
      else: newEmptyNode()
    deleteEntParam = ident "entity"

  var
    entContext = newEventContext(deleteEntParam)
    userStateChangeEvent = newStmtList()
  
  userStateChangeEvent.invokeEvent(id, entContext, ekDeleteEnt)

  userStateChangeEvent.trackMutation(id, ekDeleteEnt, [0], announce = false)

  result = quote do:
    proc doDelete(`entitySym`: EntityRef) =
      static: startOperation(`cacheId`, "delete")

      let
        `entIdIdent` = `entitySym`.entityId
      
      if not `entitySym`.alive or not(entityData(`entIdIdent`).setup):
        return
      
      `userStateChangeEvent`
      `deleteCore`

      `initSet`
      `clearComponents`
      `gcCleanup`

      entityData(`entIdIdent`).setup = false
      `storageVar`.entityCounter -= 1
      `recyclerAdd`

      if `storageVar`.entityCounter == 0:
        # Helps against nextEntityId going out of range after repeated add/delete.
        `recyclerClear`
        `storageVar`.nextEntityId = FIRST_ENTITY_ID

      static: endOperation(`cacheId`)


    proc deleteAll*(entities: var Entities, resize = true) =
      for i in 0 ..< entities.len:
        entities[i].delete
      if resize: entities.setLen 0


    proc resetEntityStorage* =
      ## This deletes all entities, removes them from associated systems and resets next entity.
      for i in 0 ..< `storageVar`.nextEntityId.int:
        let ent = (i.EntityId).makeRef
        ent.delete
      `recyclerClear`
      `storageVar`.nextEntityId = FIRST_ENTITY_ID
      `storageVar`.entityCounter = 0

