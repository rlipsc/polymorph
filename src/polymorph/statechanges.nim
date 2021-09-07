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

import macros, sharedtypes, components, private/[statechangeutils, utils, ecsstatedb, parseparams]
import strutils, tables, typetraits, sets, macrocache

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
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  let
    tyName = id.typeName(typeIndex)
    instTypeName = ident instanceTypeName(tyName)
    cc = ident "curComponent"
    ce = ident "curEntity"
    cbProcName = ident addCallbackName(tyName)

  id.add_onAddCallback(typeIndex, quote do:
      proc `cbProcName`*(`ce`: EntityRef, `cc`: `instTypeName`) =
        let `ce` {.inject.} = `ce`
        `actions`
  )

  id.add_onAddCallbackForwardDecl(typeIndex, quote do:
      proc `cbProcName`*(`ce`: EntityRef, `cc`: `instTypeName`)
  )

  result = newStmtList()

macro onAddCallback*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnAddCallback(typeToUse, actions)

macro onAddCallback*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnAddCallback(typeToUse, actions)


proc setupOnRemoveCallback(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  let
    tyName = id.typeName(typeIndex)
    instType = ident instanceTypeName(tyName)
    cc = ident "curComponent"
    ce = ident "curEntity"
    cbProcName = ident removeCallbackName(tyName)

  id.add_onRemoveCallback(typeIndex, quote do:
      proc `cbProcName`*(`ce`: EntityRef, `cc`: `instType`) =
        `actions`
  )
  id.add_onRemoveCallbackForwardDecl(typeIndex, quote do:
      proc `cbProcName`*(`ce`: EntityRef, `cc`: `instType`)
  )

  result = newStmtList()

macro onRemoveCallback*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnRemoveCallback(typeToUse, actions)

macro onRemoveCallback*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnRemoveCallback(typeToUse, actions)


# The following events are inserted inline during code generation.

proc setupOnInit(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  
  id.add_onInitCode(typeIndex, actions)
  
  result = newStmtList()

macro onInit*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnInit(typeToUse, actions)

macro onInit*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnInit(typeToUse, actions)


proc setupOnInterceptUpdate*(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## The data being added can be accessed in `curComponent`, and is of
  ## the native type, not the instance type.
  ## Each invocation will append to the code that will be inserted.
  ## Note: When this is hooked, the user must call `commit` if they don't
  ## want the update parameters to be ignored.
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "

  id.add_onInterceptValueInitCode(typeIndex, actions)

  result = newStmtList()


macro onInterceptUpdate*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnInterceptUpdate(typeToUse, actions)

macro onInterceptUpdate*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnInterceptUpdate(typeToUse, actions)

macro onUpdate*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnInterceptUpdate(typeToUse, actions)

macro onUpdate*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnInterceptUpdate(typeToUse, actions)

proc setupOnDelete*(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  ## Add some code to be executed when a component is deleted.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "

  id.add_onFinalisationCode(typeIndex, actions)

  result = newStmtList()

macro onDelete*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnDelete(typeToUse, actions)

macro onDelete*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnDelete(typeToUse, actions)


proc setupOnAdd*(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  ## Add some code to be executed when a component of this type is added to an entity.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "

  id.add_onAddToEntCode(typeIndex, actions)

  result = newStmtList()

macro onAdd*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnAdd(typeToUse, actions)

macro onAdd*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnAdd(typeToUse, actions)


proc setupOnRemove*(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  ## Add some code to be executed when a component of this type is removed from an entity.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "

  id.add_onRemoveFromEntCode(typeIndex, actions)

  result = newStmtList()

macro onRemove*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnRemove(typeToUse, actions)

macro onRemove*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnRemove(typeToUse, actions)


proc setupOnSystemAdd(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  ## Add some code to be executed when a component of this type is added to any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "

  id.add_onAddAnySystemCode(typeIndex, actions)

  result = newStmtList()

macro onSystemAdd*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnSystemAdd(typeToUse, actions)

macro onSystemAdd*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnSystemAdd(typeToUse, actions)


proc setupOnSystemAddTo(id: EcsIdentity, typeToUse: NimNode, systemName: string, actions: NimNode): NimNode =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let
    sysIndex = findSystemIndex(id, systemName)
    typeIndex = id.typeStringToId($typeToUse)

  doAssert typeIndex.int != 0, "Cannot find type " & $typeIndex.int & " in registered components "
  doAssert sysIndex.found, "Cannot find system \"" & systemName & "\" in defined systems: " & id.commaSeparate(id.allSystemsSeq)

  id.add_onAddToCode(sysIndex.index, typeIndex, actions)
  id.add_onAddToSystemComp(sysIndex.index, typeIndex)

  result = newStmtList()

macro onSystemAddTo*(id: static[EcsIdentity], typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  id.setupOnSystemAddTo(typeToUse, systemName, actions)

macro onSystemAddTo*(typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  defaultIdentity.setupOnSystemAddTo(typeToUse, systemName, actions)


proc setupOnSystemRemove*(id: EcsIdentity, typeToUse: NimNode, actions: NimNode): NimNode =
  ## Add some code to be executed when a component of this type is removed from any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code to be inserted.
  let typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  
  id.add_onRemoveAnySystemCode(typeIndex, newBlockStmt(actions))

  result = newStmtList()

macro onSystemRemove*(id: static[EcsIdentity], typeToUse: typedesc, actions: untyped): untyped =
  id.setupOnSystemRemove(typeToUse, actions)

macro onSystemRemove*(typeToUse: typedesc, actions: untyped): untyped =
  defaultIdentity.setupOnSystemRemove(typeToUse, actions)


proc setupOnSystemRemoveFrom*(id: EcsIdentity, typeToUse: NimNode, systemName: string, actions: NimNode): NimNode =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code to be inserted.
  let
    sysFound = findSystemIndex(id, systemName)
    typeIndex = id.typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  doAssert sysFound.found, "Cannot find system \"" & systemName & "\" in defined systems: " & id.commaSeparate(id.allSystemsSeq)
  
  id.add_onRemoveFromCode(sysFound.index, typeIndex, actions)
  id.add_onRemoveFromSystemComp(sysFound.index, typeIndex)

  result = newStmtList()

macro onSystemRemoveFrom*(id: static[EcsIdentity], typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  id.setupOnSystemRemoveFrom(typeToUse, systemName, actions)

macro onSystemRemoveFrom*(typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  defaultIdentity.setupOnSystemRemoveFrom(typeToUse, systemName, actions)


proc addEntityStateChange(id: EcsIdentity, actions: NimNode) =
  id.add_onEntityStateChange newBlockStmt(actions)

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

#--------------
# State changes
#--------------


proc doNewEntityWith(id: EcsIdentity, componentList: NimNode): NimNode {.compileTime.} =
  # Note: Currently does not generate a container and writes out the innards
  # within a block statement in the caller scope.
  # Consequences: All ECS mechanics must be exposed to the user.
  #   - Pros:
  #     - No caller overhead,
  #     - The user can create features that access any part of the ECS state.
  #   - Cons:
  #     - The user can potentially corrupt the ECS with low level access.
  result = newStmtList()

  let
    entity = genSym(nskVar, "entity")
    entIdNode = quote: `entity`.entityId
  var
    componentDecl = nnkVarSection.newTree()
    addToEntity = newStmtList()
    compIds: seq[ComponentTypeId]
    statements = newStmtList()
    userCompAddCode = newStmtList()

  for component in componentList:
    let
      # Search for the type name for this node
      tyName = component.findType
      # Convert to `ComponentTypeId`
      typeId = id.typeStringToId(tyName)
      fieldName = tyName.toLowerAscii & instPostfix
      fieldIdent = ident fieldName
      instTypeIdent = ident instanceTypeName(tyName)

    if typeId in compIds: error "newEntityWith has been passed more than one component of type " & tyName

    # For later use in updateSystems
    compIds.add typeId

    # Create storage for this component
    let compOwner = id.systemOwner typeId
    if compOwner != InvalidSystemIndex:
      # Owned component access is part of the system row and cannot be accessed
      # until a new row is added.
      let sysNode = id.instantiation compOwner
      # This should correspond to the next index for groups, which is added just below.
      componentDecl.add genFieldAssignment(fieldName, false, quote do:
        `sysNode`.count.`instTypeIdent`
      )
    else:
      # Create storage and assign the parameter value.
      componentDecl.add genFieldAssignment(fieldName, false, quote do:
        newInstance(`component`))

    let
      compRef = newDotExpr(fieldIdent, ident "toRef")
    
    addToEntity.add addComponentRef(entity, compRef, id.entityOptions)

    # Component add user inline event.
    let userAddToEnt = id.onAddToEntCodeNode(typeId)
    if userAddToEnt.len > 0:
      userCompAddCode.add(quote do:
        block:
          ## Current component being added to entity. 
          template curComponent: untyped {.used.} = `fieldIdent`
          `userAddToEnt`
      )

    # Component add user callback event.
    let onAddCallback = id.onAddCallbackNode(typeId)
    if onAddCallback.len > 0:
      # Check for this type's initialiser.
      let cbProcName = ident addCallbackName(tyName)
      userCompAddCode.add(quote do:
        `cbProcName`(`entity`, `fieldIdent`)
      )

  let setOp =
    if id.useSet:
      let
        setType = ident enumName()
        setVal = componentListToSet(id, compIds, setType)
      entSetIncl(entIdNode, setVal)
    else:
      newEmptyNode()

  var processed: HashSet[SystemIndex]

  statements.add(quote do:
    var `entity` = newEntity()
    `componentDecl`
    `setOp`
  )
  
  var
    missingComps, ownedComps: seq[ComponentTypeId]
    userAddedEvents = newStmtList()

  for compId in compIds:      
    let ownerSystem = id.systemOwner compId
    var ownershipSatisfied = true

    # Parameters that include owned components must fully satisfy their owning system.
    if ownerSystem != InvalidSystemIndex:
      ownedComps.add compId
      for comp in id.ecsSysRequirements(ownerSystem):

        if comp != compId and comp notin compIds and comp notin missingComps:
          ownershipSatisfied = false
          missingComps.add comp

    if ownershipSatisfied:
      
      for sys in id.systems(compId):
        if sys notin processed:
          processed.incl sys

          if id.satisfied(sys, compIds):
            # This system matches the components we've been given.

            let
              (sysUpdate, event, eventsExist) = addSysTuple(id, entity, sys, compIds, componentList)

            statements.add sysUpdate

            if eventsExist:
              userAddedEvents.add event


  if missingComps.len > 0:
    error "Owned component(s) [" & id.commaSeparate(ownedComps) &
      "] need their owner systems completed with component(s): [" & id.commaSeparate(missingComps) & "]"

  let
    stateChangeEvent = id.userStateChange(entity, eceNewEntityWith, compIds)
    opStr =  "newEntityWith: " & id.commaSeparate(compIds)
  statements.add(quote do:
    {.line.}:
      template curEntity: EntityRef {.used.} = `entity`
      static: startOperation(EcsIdentity(`id`), `opStr`)
      `addToEntity`
      `userCompAddCode`
      `userAddedEvents`
      `stateChangeEvent`
      static: endOperation(EcsIdentity(`id`))
      `entity`
  )

  result = newBlockStmt(statements)

  genLog "\n# macro newEntityWith(" & id.commaSeparate(compIds) & "):\n", result.repr


proc makeNewEntityWith*(id: EcsIdentity): NimNode =
  let componentList = ident "componentList"
  quote do:
    macro newEntityWith*(`componentList`: varargs[typed]): untyped =
      ## Create an entity with the parameter components.
      ## This macro statically generates updates for only systems
      ## entirely contained within the parameters and ensures no
      ## run time component list iterations and associated checks.
      doNewEntityWith(EcsIdentity(`id`), `componentList`)


proc genComponents*(id: EcsIdentity, entity: NimNode, compInfo: ComponentParamInfo): NimNode =
  ## Create storage instances for passed components.
  ## Only handles non-owned components. 
  result = newStmtList()
  var
    componentDecl = nnkVarSection.newTree()
    userEventDecls = newStmtList()
    interceptPrecursor = newStmtList()

  for idx in compInfo.generateIdx:
    let
      typeId = compInfo.passed[idx]
      typeName = id.typeName(typeId)
      typeStr = typeName & instPostfix
      instVarStr = typeStr.toLower
      instVar = ident instVarStr
      paramVal = compInfo.values[idx]

    let
      onAddToEntCode = id.onAddToEntCodeNode(typeId)
      onAddCallback = id.onAddCallbackNode(typeId)
      hasUserAddCode = onAddToEntCode.len > 0
      hasUserCallback = onAddCallback.len > 0

    if hasUserAddCode:
      userEventDecls.add(quote do:
        block:
          template curComponent: untyped {.used.} = `instVar`
          template curEntity: untyped {.used.} = `entity`
          `onAddToEntCode`
      )
    if hasUserCallback:
      let cbProcName = ident addCallbackName(typeName)
      userEventDecls.add(quote do:
        `cbProcName`(`entity`, `instVar`)
      )

    componentDecl.add genFieldAssignment(instVarStr, false, quote do: newInstance(`paramVal`))
  
  if interceptPrecursor.len > 0:
    result.add interceptPrecursor
  result.add componentDecl
  result.add userEventDecls


proc checkRequired(id: EcsIdentity, compInfo: ComponentParamInfo): NimNode =
  ## Check fetched components that satisfy owned component systems are valid.
  result = newStmtList()
  if compInfo.requiredFetches.len > 0:
    var checkSystem: seq[NimNode]

    for typeId in compInfo.requiredFetches:
      let
        typeStr = id.typeName typeId
        typeField = ident typeStr.toLower & instPostfix
      checkSystem.add newDotExpr(typeField, ident "alive")

    let
      matchesSystem = genInfixes(checkSystem, "and")
      unsatisfiedErrStr =
        newLit "Cannot complete this add operation because systems that own the parameter components are not fully satisfied, missing: " & id.commaSeparate(compInfo.requiredFetches)
      errorResponse =
        case id.errIncompleteOwned
        of erAssert:
          quote do:
            assert `matchesSystem`, `unsatisfiedErrStr`
        of erRaise:
          quote do:
            if not(`matchesSystem`): raise newException(ValueError, `unsatisfiedErrStr`)
    result.add errorResponse


proc addConditionalSystems(id: EcsIdentity, entity: NimNode, compInfo: ComponentParamInfo): NimNode =
  ## Add components to systems that have no owned components.
  ## These systems may or may not be updated depending on fetched instances.
  result = newStmtList()
  var
    addedEvents = newStmtList()
    conditionalAddedEvents = newStmtList()

  for sys in compInfo.unownedSystems:
    let
      sysTupleStr = id.getSystemName(sys).tupleName
      sysTupleVar = ident(sysTupleStr.toLower)
    
    var
      checkSystem: seq[NimNode]
      updateTupleFields = newStmtList()

    updateTupleFields.add(quote do:
      `sysTupleVar`.entity = `entity`)

    # Generate the code that adds this component instance to this system.
    # This includes user events and special handling for owned components.
    let (updateSystem, addedEvent, eventsExist) = id.addSysTuple(entity, sys, compInfo.passed, compInfo.values)

    # Build the checks to see if this system matches.
    for typeId in id.ecsSysRequirements(sys):
      let
        typeStr = id.typeName typeId
        typeField = ident typeStr.toLower & instPostfix

      if typeId in compInfo.lookFor:
        checkSystem.add(quote do: `typeField`.valid)

    if checkSystem.len > 0:
      let
        matchesSystem = genInfixes(checkSystem, "and")
        conditionalAdd = quote do:
          if `matchesSystem`:
            `updateSystem`
            `addedEvent`
      if eventsExist:
        conditionalAddedEvents.add conditionalAdd
      else:
        result.add conditionalAdd
    else:
      addedEvents.add addedEvent
      result.add updateSystem
  if addedEvents.len > 0:
    result.add addedEvents
  if conditionalAddedEvents.len > 0:
    result.add conditionalAddedEvents


proc addOwned(id: EcsIdentity, entity: NimNode, compParamInfo: ComponentParamInfo): NimNode =
  ## Assumes we have everything we need to add to owned systems.
  ## Any systems updated here must be fully qualified with their components.
  ## Where they are not given as parameters, their presence must be enforced
  ## at run-time.
  result = newStmtList()

  for sys in compParamInfo.ownedSystems:
    let
      sysName = id.getSystemName sys
      systemNode = id.instantiation sys
      sysTupleStr = sysName.tupleName
      sysTupleType = ident sysTupleStr
      sysTupleVar = ident(sysTupleStr.toLower)
      tupleSetup = newStmtList()
      sysOpts = id.getOptions(sys)

    # Components are assigned to variables with the same name.
    var
      assignCompRefs = nnkLetSection.newTree()
      userSysAddCode = newStmtList()
      stateUpdates = newStmtList()
    let
      sysHighVar = genSym(nskLet, "sysHigh")
      getSysHigh = quote do: `systemNode`.high
      
    # Retrieve the last inserted system row for the component index.
    assignCompRefs.add newIdentDefs(sysHighVar, newEmptyNode(), getSysHigh)

    # TODO: Migrate to addSysTuple

    for typeId in id.ecsSysRequirements(sys):
      # Populate system tuple.
      let
        typeStr = id.typeName typeId
        lcTypeStr = typeStr.toLower
        typeField = ident lcTypeStr
        instType = ident typeStr.instanceTypeName
        instField = ident lcTypeStr & instPostfix
        ownedByThisSystem = id.systemOwner(typeId) == sys

      if ownedByThisSystem:
        # When a component is owned, it must be given as a parameter.
        let index = compParamInfo.passed.find(typeId)
        if index < 0:
          error "Cannot find owned component \"" & typeStr &
            "\" when adding to system \"" & sysName & "\""

        let
          value = compParamInfo.values[index]
          sysTupleVar = ident(sysTupleStr.toLower)
          instField = ident lcTypeStr & instPostfix

          (onAddCode, userInitCode, updateCode) = id.ownedUserInitEvents(sys, sysTupleVar, instField, typeId, entity, value)

        # Updating state lists is usually done in the component's
        # creation procs for non-owned components.
        stateUpdates.add updateOwnedComponentState(id, typeId, sys, sysHighVar)

        tupleSetup.add(quote do:
          `sysTupleVar`.`typeField` = `value`
          `updateCode`
        )

        stateUpdates.add(quote do:
          `userInitCode`
          `onAddCode`
        )
        
        # Owned components reference the group index of their owning system.
        # Equivalent to: typeField = InstanceType(sys.high)
        assignCompRefs.add newIdentDefs(instField, newEmptyNode(), newDotExpr(sysHighVar, instType))

      else:
        # Assign found reference.
        let instanceField = ident typeStr.toLower & instPostfix
        tupleSetup.add(quote do:
          `sysTupleVar`.`typeField` = `instanceField`
        )

      # TODO: Note: this is normally performed in addSysTuple. Refactor required.
      id.addUserCompAddToSys(userSysAddCode, entity, sys, typeId, instField)

    let updateIndex = id.updateIndex(entity, sys, sysHighVar)

    result.add(quote do:
      var `sysTupleVar`: `sysTupleType`
      `sysTupleVar`.entity = `entity`
    )
    if userSysAddCode.len > 0: result.add(quote do:
      template curEntity: EntityRef {.used.} = `entity`
    )
    result.add tupleSetup
    result.add addToSystemTuple(systemNode, sysTupleVar, sysOpts)
    result.add assignCompRefs
    result.add updateIndex
    result.add stateUpdates
    result.add userSysAddCode


proc doAddComponents(id: EcsIdentity, entity: NimNode, componentList: NimNode): NimNode =
  ## Output minimised code to add the components in `componentList` to
  ## an entity.
  var inner = newStmtList()

  let componentInfo = id.parseComponentParameters(componentList)

  when defined(ecsPerformanceHints):
    let paramStr = id.commaSeparate(componentInfo.passed)
    inner.add(quote do:
      static: startOperation(EcsIdentity(`id`), "Add components: " & `paramStr`)
    )

  for typeId in componentInfo.passed:
    let
      typeStr = id.typeName typeId
      typeIdent = ident typeStr
    inner.add.add(quote do:
      assert `typeIdent` notin `entity`, "Component \"" & `typeStr` & "\" already exists in entity"
    )

  inner.add genComponents(id, entity, componentInfo)
  inner.add buildFetch(id, entity, componentInfo.lookFor)
  inner.add checkRequired(id, componentInfo)
  inner.add addOwned(id, entity, componentInfo)
  inner.add addToEntityList(id, entity, componentInfo.passed)
  inner.add addConditionalSystems(id, entity, componentInfo)
  if id.useSet:
    let
      setType = ident enumName()
      setVal = componentListToSet(id, componentInfo.passed, setType)
    inner.add entSetIncl(quote do: `entity`.entityId, setVal)
  inner.add id.userStateChange(entity, eceAddComponents, componentInfo.passed)
  
  # Build return tuple.
  var returnType = nnkPar.newTree()
  for typeId in componentInfo.passed:
    let
      typeStr = id.typeName typeId
      fieldIdent = ident typeStr.toLower
      typeIdent = ident typeStr.toLower & instPostfix
    returnType.add nnkExprColonExpr.newTree(fieldIdent, typeIdent)

  when defined(ecsPerformanceHints):
    inner.add(quote do:
      static: endOperation(EcsIdentity(`id`))
    )

  inner.add returnType

  result = quote do:
    block:
      assert `entity`.alive, "Adding to a dead entity"
      `inner`

  genLog "\n# macro addComponents(" & id.commaSeparate(componentInfo.passed) & "):\n", result.repr


proc makeAddComponents*(id: EcsIdentity): NimNode =
  let
    entity = ident "entity"
    identity = quote do: EcsIdentity(`id`)
    res = ident "result"
    componentList = ident "componentList"
  
  result = newStmtList()

  # Core 'add' macros.

  result.add(
    quote do:
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
  )

  # Per-component utilities.

  for typeId in id.unsealedComponents:
    let
      tyStr = id.typeName(typeId)
      ty = ident tyStr
      inst = ident instanceTypeName(tyStr)
      owner = id.systemOwner(typeId)

      # These single component operations can't perform state updates
      # to systems with multiple owned components.
      skip = owner != InvalidSystemIndex and id.len_ecsOwnedComponents(owner) > 1

    if not skip:
      result.add(quote do:
        proc addComponent*[T: ComponentTypeclass](`entity`: EntityRef, component: T): auto {.discardable.} =
          ## Add a component to an entity and return the instance.
          `entity`.addComponents(component)[0]

        proc addComponent*(`entity`: EntityRef, component: `ty`): `inst` {.discardable.} =
          ## Add a component to an entity and return the instance.
          addComponents(`entity`, component)[0]

        proc add*(`entity`: EntityRef, component: `ty`): `inst` {.discardable.} =
          ## Add a component to an entity and return the instance.
          addComponents(`entity`, component)[0]

        # TODO: addOrUpdate that allows updating a selection of fields rather than the whole component item.
        proc addOrUpdate*(`entity`: EntityRef, component: `ty`): `inst` {.discardable.} =
          ## Add `component` to `entity`, or if `component` already exists, overwrite it.
          ## Returns the component instance.
          let fetched = `entity`.fetchComponent component.type
          if fetched.valid:
            # Replace original. No further work is required as the types or indexes have not been updated.
            update(fetched, component)
            
            `res` = fetched
          else:
            # Add as normal.
            `res` = addComponent(`entity`, component)
        
        proc addIfMissing*(`entity`: EntityRef, component: `ty`): `inst` {.discardable.} =
          ## Add a component only if it isn't already present.
          ## If the component is already present, no changes are made and an invalid result is returned.
          ## If the component isn't present, it will be added and the instance is returned.
          if not `entity`.hasComponent component.type:
            `res` = addComponent(`entity`, component)
        
        proc fetchOrAdd*(`entity`: EntityRef, component: typedesc[`ty`]): `inst` {.discardable.} =
          ## Fetch an existing component type if present, otherwise add
          ## the component type and return the instance.
          ## 
          ## This is useful when you always want a valid component
          ## instance returned, but don't want to overwrite existing
          ## data.
          `res` = `entity`.fetchComponent component.type
          if not `res`.valid:
            `res` = addComponent(`entity`, component())
      )

  result.add(
    quote do:

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
        for c in components:
          caseComponent c.typeId:
            let inst = entity.fetchComponent componentType()
            if inst.valid:
              inst.update componentRefType()(c).value

      template update*(entity: EntityRef, components: ComponentList) =
        ## Updates existing components from a list.
        ## Components not found on the entity exist are ignored.
        updateComponents(entity, components)
  )


proc doRemoveComponents(id: EcsIdentity, entity: NimNode, componentList: NimNode): NimNode =
  #[
    Important note!
    If you call removeComponent whilst in a system using that component, the current `item` will change!
    In this case, item.entity.removeComponent will cause item.entity and its components to be different.
    This happens because `entity.removeComponent` and `entity.delete` remove items from systems by swapping
    the item with the last one in the list and reducing the list length.
  ]#
  var types = id.toTypeList(componentList)

  let
    componentStorageFormat = id.componentStorageFormat
    index = ident "index"
    entityIdIdent = ident "entityId"
    componentLen = componentRefsLen(entityIdIdent, id.entityOptions)
    alive = ident "alive" # Late bind for this template
    rowIdent = ident "row"
    foundComp = ident "found"
    # System indexes that use the component being removed.
    removeIdxIdent = ident "compIdx"
    compsDeleted = ident "compsDeleted"

  result = newStmtList()

  var
    updateSystems = newStmtList()
    userUpdates = newStmtList()
    findSysCode = newStmtList()
    foundDecl = nnkVarSection.newTree()
    componentsToRemove: HashSet[ComponentTypeId]
    relevantSystems: HashSet[SystemIndex]
    setOp = newStmtList()

    # Set up the remove operation.
    (removeRef, caseStmt) =
      case componentStorageFormat
      of csTable:
        # Table storages fetch components and check each one individually,
        # it doesn't need a case statement.
        (newStmtList(), newStmtList())
      of csSeq, csArray:
        # Prelude the current component to use within a case statement,
        # and set up the case statement itself with the typeId of the
        # current component as the discriminator.
        (
          newStmtList(quote do:
            `foundComp` = entityData(`entityIdIdent`).componentRefs[`removeIdxIdent`]
          ),
          nnkCaseStmt.newTree(
            newDotExpr(foundComp, ident "typeId")
          )
        )

    # The number of components may be extended if owner systems are affected.
    numComponentsToRemove = types.len
    compIdx: int

  for typeId in types:
    # Always remove parameter components.
    componentsToRemove.incl typeId
    for sys in id.linked(typeId):
      relevantSystems.incl sys



  # Generate setup code for this operation.
  if relevantSystems.len > 0:
    foundDecl = id.buildFindSystemVars(relevantSystems)
    findSysCode = id.buildFindSystems(entity, relevantSystems)
    userUpdates = id.buildRemoveSystemEvents(entity, relevantSystems)
    updateSystems = id.buildRemoveSystems(entity, relevantSystems)

  # Process involved systems.
  for systemIndex in relevantSystems:
    # When removing a component from the system that owns it, we must
    # also remove the other components the system owns as well, as their
    # storage is being invalidated
    for ownedComp in id.ecsOwnedComponents(systemIndex):
      # Increment search counter to include owned components.
      if ownedComp notin componentsToRemove:
        componentsToRemove.incl ownedComp
        numComponentsToRemove += 1
        types.add ownedComp

  # Total components to remove, including linked owned components.
  let delCompCount = newLit numComponentsToRemove

  for delComp in componentsToRemove:

    let
      typeName = id.typeName delComp
      delInstanceType = ident instanceTypeName(typeName)
      isOwned = id.systemOwner(delComp) != InvalidSystemIndex
      eventsRemoveFromEntity = userRemoveFromEntity(id, entity, foundComp, delComp)

      deleteInstance =
        if not isOwned:
          let tyDelete = ident deleteInstanceName()
          quote do:
            `tyDelete`(`delInstanceType`(`foundComp`.index))
        else:
          # Owner systems perform deletion events in the system pass.
          newStmtList()

      removeCompFromEntity = removeComponentRef(entityIdIdent, removeIdxIdent, delComp.int, id.componentStorageFormat)

      updateOwnedAliveState =
        if isOwned:
          let aliveIdent = ident aliveStateInstanceName(typeName)
          quote do:
            `aliveIdent`[`foundComp`.index.int] = false
        else:
          newStmtList()

      coreDelete = quote do:
        `eventsRemoveFromEntity`
        `deleteInstance`
        `removeCompFromEntity`
        `updateOwnedAliveState`
    
    case componentStorageFormat
    of csTable:
      # Add a fetch and check for this component.
      # There's no possibility of an early exit, as only required
      # components are checked.
      removeRef.add(quote do:
        `foundComp` = entityData(`entityIdIdent`).componentRefs.getOrDefault(`delComp`.ComponentTypeId)
        if `foundComp`.typeId == `delComp`.ComponentTypeId:
          `coreDelete`
        )
    of csSeq, csArray:
      # Append the case statement for this component.
      # If we've hit our target number of components we can break early,
      # as no duplicate component types are allowed.
      var ofBranch = nnkOfBranch.newTree()
      ofBranch.add newDotExpr(newLit(delComp.int), ident "ComponentTypeId")
      ofBranch.add(quote do:
        `coreDelete`
        `compsDeleted` = `compsDeleted` + 1
        if `compsDeleted` == `delCompCount`:
          break
      )
      caseStmt.add ofBranch
    compIdx += 1

    let
      setVal = ident "ce" & typeName
    if id.useSet:
      setOp.add(entSetExcl(entityIdIdent, setVal))

  let
    cacheId = quote do: EcsIdentity(`id`)
    opStr = "removeComponents: " & id.commaSeparate(types)
    relevantSystemsSeq = relevantSystems.toSeq

  var userStateChangeEvents = newStmtList()
  
  for typeId in types:
    userStateChangeEvents.add id.userStateChange(entity, eceRemoveComponents, @[typeId])

  let perfHint =
    when defined(ecsPerformanceHints):
      let paramStr = id.commaSeparate(types)
      quote do:
        startOperation(EcsIdentity(`id`), "Remove components: " & `paramStr`)
    else:
      newStmtList()

  case componentStorageFormat
  of csSeq, csArray:
    # Finish off the case statement and add to the remove code.
    caseStmt.add nnkElse.newTree(quote do: discard)
    removeRef.add caseStmt
  else:
    discard

  # Output final code.
  result.add(quote do:
    block:
      static:
        `perfHint`

        if `cacheId`.inSystemAll and `cacheId`.inSystemIndex in `relevantSystemsSeq`:
          # Calling removeComponent from within a system that uses the component.
          # We don't know if its the current row's entity or some other entity.
          `cacheId`.set_sysRemoveAffectedThisSystem true
        static: startOperation(`cacheId`, `opStr`)

      assert `entity`.`alive`
      let `entityIdIdent` = `entity`.entityId

      if entityData(`entityIdIdent`).setup:

        `userStateChangeEvents`

        ## Access to currently updating entity.
        template curEntity: untyped {.used.} = `entity`

        var
          # RowIdent is used by updateSystems.
          `rowIdent` {.used.}: int
          `foundComp`: ComponentRef
          `removeIdxIdent` = `componentLen` - 1
          `compsDeleted` = 0
        `foundDecl`
        `findSysCode`
        `userUpdates`
        while `removeIdxIdent` >= 0:
          `removeRef`
          `removeIdxIdent` = `removeIdxIdent` - 1
        # Remove this entity from all relevant systems.
        `updateSystems`
        # Update set if required.
        `setOp`
      static: endOperation(`cacheId`)
  )

  genLog "\n# macro removeComponents(" & id.commaSeparate(types) & "):\n", result.repr


proc makeRemoveComponents*(id: EcsIdentity): NimNode =
  let
    componentList = ident "componentList"
    entity = ident "entity"
    identity = quote do: EcsIdentity(`id`)

  result = quote do:
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
    ent = ident("entity")
    delProcName = ident("delete")
    compRefIdent = ident("compRef")
    storageVar = ident entityStorageVarName()
    totalSystemCount = id.systems.len
    rowIdent = ident "row"
    entIdIdent = ident "entityId"
    visitedIdent = ident "visited"
    clearComponents = clearAllEntComponentRefs(entIdIdent, id.componentStorageFormat)
    foundDecls = nnkVarSection.newTree()
    foundChecks = newStmtList()

  var
    updateSystems = newStmtList()
    caseStmtRemove = nnkCaseStmt.newTree()
    caseStmtUserCode = nnkCaseStmt.newTree()

  # Here we build a case statement executed for each of the entity's component indexes
  # so that each branch contains the appropriate free code for all systems that use
  # that component.
  # A system may have a requirement for multiple components that aren't satisfied though,
  # so we must also check if there's a row to be removed.
  caseStmtRemove.add newDotExpr(compRefIdent, ident "typeId")
  caseStmtUserCode.add newDotExpr(compRefIdent, ident "typeId")
  
  var
    includeEntityTmpl: bool
    processedSystems: HashSet[SystemIndex]
    userCodeExists, userVisitedRequired: bool
  let
    unsealedSystems = id.allUnsealedSystems

  for compId in id.unsealedComponents:

    let tyName = id.typeName compId

    var
      removeBody = newStmtList()
      userBody = newStmtList()

    # Update systems references for each component.
    for sysIdx in id.systems(compId):

      # Process only new systems we haven't seen before for this set of entities.
      if sysIdx in unsealedSystems:
        let
          sysOpts = id.getOptions sysIdx
          sysName = id.getSystemName sysIdx
          sysNameUpper = sysName.capitalizeAscii
          sysNameLower = sysName.toLowerAscii
          sysIdent = ident systemVarName(sysNameUpper)
          sysNode = id.instantiation sysIdx
          tryGetSys = sysNode.indexTryGet(entIdIdent, rowIdent, sysOpts.indexFormat)
          foundSys = ident sysNameLower & "Found"
          foundSysRow = ident sysNameLower & "Row"

          removeSystemEntry = removeSysReference(id, sysIdx, sysIdent, foundSys, foundSysRow, entIdIdent, ent)
          userSysRemove = userSysRemoved(id, sysIdx, sysIdent, foundSysRow, entIdIdent, ent, sysOpts)

        if sysIdx notin processedSystems:
          processedSystems.incl sysIdx
          foundDecls.add newIdentDefs(foundSys, ident "bool")
          foundDecls.add newIdentDefs(foundSysRow, ident "int")
          foundChecks.add(quote do:
            if `tryGetSys`:
              `foundSys` = true
              `foundSysRow` = `rowIdent`
            )

        if id.systemOwner(compId) == sysIdx:
          # Update alive state for owned components.
          let
            aliveIdent = ident aliveStateInstanceName(tyName)
            cIdx = compId.int
          updateSystems.add(quote do:
            `aliveIdent`[`cIdx`] = false
          )

        removeBody.add(
          quote do:
            if not `visitedIdent`[`sysIdx`]:
              `visitedIdent`[`sysIdx`] = true
              `removeSystemEntry`
        )

        if userSysRemove.len > 0:
          userVisitedRequired = true
          userBody.add(
            quote do:
              if not `visitedIdent`[`sysIdx`]:
                `visitedIdent`[`sysIdx`] = true
                if `foundSys`:
                  `userSysRemove`
          )

    if removeBody.len > 0:
      var ofNodeRemove = nnkOfBranch.newTree()
      ofNodeRemove.add newDotExpr(newIntLitNode(compId.int), ident "ComponentTypeId")
      ofNodeRemove.add newStmtList(removeBody)
      caseStmtRemove.add(ofNodeRemove)

    let removeEvents = userRemoveFromEntity(id, ent, compRefIdent, compId)
    if removeEvents.len > 0:
      userBody.add removeEvents
    
    if userBody.len > 0:
      includeEntityTmpl = true

    if userBody.len > 0:
      userCodeExists = true
      var ofNodeUser = nnkOfBranch.newTree()
      ofNodeUser.add newDotExpr(newIntLitNode(compId.int), ident "ComponentTypeId")
      ofNodeUser.add newStmtList(userBody)
      caseStmtUserCode.add(ofNodeUser)

  caseStmtRemove.add nnkElse.newTree(quote do: discard)
  caseStmtUserCode.add nnkElse.newTree(quote do: discard)
  
  updateSystems.add caseStmtRemove

  let
    # For pointer arrays, the GC needs to be informed about the componentRef sequence.
    entOpts = id.entityOptions
    gcCleanup =
      case entOpts.entityStorageFormat
      of esSeq, esArray: newEmptyNode()
      of esPtrArray:
        if entOpts.componentStorageFormat in [csSeq, csTable]:
          quote do:
            GC_Unref(entityData(`entIdIdent`).componentRefs)
        else:
          newEmptyNode()
    recFormat = entOpts.recyclerFormat
    recyclerAdd = storageVar.recyclerAdd(entIdIdent, recFormat)
    recyclerClear = storageVar.recyclerClear(recFormat)
    initSet =
      if entOpts.useSet:
        quote do:
          entityData(`entIdIdent`).exists = {}        
      else: newEmptyNode()

  let
    visitedArray = quote do:
      var `visitedIdent` {.used.}: array[0 .. `totalSystemCount`, bool]

    clearRecycler = storageVar.recyclerClear(recFormat)

    curEntTmpl =
      if includeEntityTmpl:
        quote do:
          ## Access to currently updating entity.
          template curEntity: untyped {.used.} = `ent`
      else: newEmptyNode()

    userCode =
      if userCodeExists:
        var userOnRemove = newStmtList()
        if userVisitedRequired: userOnRemove.add(visitedArray)
        userOnRemove.add(quote do:
          for `compRefIdent` in `entIdIdent`.components:
            `caseStmtUserCode`
        )
        newBlockStmt(userOnRemove)
      else:
        newStmtList()

  let
    cacheId = quote do: EcsIdentity(`id`)
    userStateChangeEvent = id.userStateChange(ent, eceDelete)    

  result = quote do:
    proc doDelete(`ent`: EntityRef) =
      static: startOperation(`cacheId`, "delete")

      if not `ent`.alive: return
      
      `userStateChangeEvent`

      let `entIdIdent` = `ent`.entityId
      if entityData(`entIdIdent`).setup:
        var `rowIdent` {.used.}: int

        `foundDecls`
        `foundChecks`
        `curEntTmpl`

        `userCode`

        `visitedArray`
        for `compRefIdent` in `entIdIdent`.components:
          `caseStmtRemove`
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

    template `delProcName`*(`ent`: EntityRef) =
      static:
        if `cacheId`.inSystem and (not `cacheId`.inSystemDeleteRow):
          `cacheId`.set_systemCalledDelete true
      doDelete(`ent`)

    proc deleteAll*(entities: var Entities, resize = true) =
      for i in 0 ..< entities.len:
        entities[i].delete
      if resize: entities.setLen 0

    proc resetEntityStorage* =
      ## This deletes all entities, removes them from associated systems and resets next entity.
      for i in 0 ..< `storageVar`.nextEntityId.int:
        let ent = (i.EntityId).makeRef
        ent.delete
      `clearRecycler`
      `storageVar`.nextEntityId = FIRST_ENTITY_ID
      `storageVar`.entityCounter = 0

