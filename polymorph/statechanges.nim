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

import macros, sharedtypes, private/[utils, ecsstateinfo], components, strutils, tables, typetraits, sets

macro componentToSysRequirements*(varName: untyped): untyped =
  ## Create a static sequence that matches componentTypeId to an array of indexes into systemNodes
  ## The result is an array that returns a seq of indexes into allSystemsNode by componentTypeId
  ##   `array[ComponentTypeId, seq[SystemIndex]]`
  ## This means we can check at run time what systems are required per component.
  ## Note this must only be used after seal has been called, otherwise the compile-time lists will be
  ## incomplete.
  var compSys = newSeq[seq[SystemIndex]](typeInfo.len)
  for ty in typeInfo:
    compSys.add ty.systems
  let res = compSys.genStaticArray()
  result = quote do:
    const `varName` = `res`
  genLog "# componentToSysRequirements:\n", result.repr

proc findType(compNode: NimNode): string =
  ## Expects a typed node and tries to extract the type name.
  case compNode.kind
  of nnkObjConstr:
    # Defined inline
    compNode.expectMinLen 1

    if compNode[0].kind == nnkDotExpr:
      compNode[0][1].strVal
    else:
      compNode[0].strVal
  of nnkSym:
    let tyImpl = compNode.getTypeInst()
    tyImpl.repr
  of nnkCall:
    let caller = compNode[0].getImpl()
    caller.expectKind nnkProcDef
    let callerTypeStr = $caller[3][0]
    callerTypeStr
  else:
    $(compNode.getTypeInst())

proc componentListToSet(componentIds: seq[ComponentTypeId], setType: NimNode): NimNode =
  # Add to entity set in one go.
  result = nnkCurly.newTree()
  # Add the exists flags, cast to the components enum
  for id in componentIds:
    result.add ident "ce" & typeInfo.typeName id

macro onAddCallback*(typeToUse: typedesc, actions: untyped): untyped =
  let typeIndex = typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  let
    tyName = typeInfo.typeName typeIndex
    instTypeName = ident instanceTypeName(tyName)
    cc = ident "curComponent"
    ce = ident "curEntity"
    cbProcName = ident addCallbackName(tyName)
  typeInfo[typeIndex.int].onAddCallback = quote do:
      proc `cbProcName`*(`ce`: EntityRef, `cc`: `instTypeName`) =
        `actions`
  typeInfo[typeIndex.int].onAddCallbackForwardDecl = quote do:
      proc `cbProcName`*(`ce`: EntityRef, `cc`: `instTypeName`)

  result = newStmtList()

macro onRemoveCallback*(typeToUse: typedesc, actions: untyped): untyped =
  let typeIndex = typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  let
    tyName = typeInfo.typeName typeIndex
    instType = ident instanceTypeName(tyName)
    cc = ident "curComponent"
    ce = ident "curEntity"
    cbProcName = ident removeCallbackName(tyName)
  typeInfo[typeIndex.int].onRemoveCallback = quote do:
      proc `cbProcName`*(`ce`: EntityRef, `cc`: `instType`) =
        `actions`
  typeInfo[typeIndex.int].onRemoveCallbackForwardDecl = quote do:
      proc `cbProcName`*(`ce`: EntityRef, `cc`: `instType`)

  result = newStmtList()

# These inline hooks insert code at generation, and are therefore more limited in what they can do.

macro onInit*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  typeInfo[typeIndex].onInitCode.add actions
  result = newStmtList()

macro onInterceptUpdate*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## The data being added can be accessed in `curComponent`, and is of
  ## the native type, not the instance type.
  ## Each invocation will append to the code that will be inserted.
  ## Note: When this is hooked, the user must call `commit` if they don't
  ## want the update parameters to be ignored.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  typeInfo[typeIndex].onInterceptValueInitCode.add actions
  result = newStmtList()

macro onDelete*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component is deleted.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  typeInfo[typeIndex].onFinalisationCode.add actions
  result = newStmtList()

macro onAdd*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is added to an entity.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  typeInfo[typeIndex].onAddToEntCode.add actions
  result = newStmtList()

macro onRemove*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from an entity.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  typeInfo[typeIndex].onRemoveFromEntCode. add actions
  result = newStmtList()

macro onSystemAdd*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is added to any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  typeInfo[typeIndex].onAddAnySystemCode.add actions
  result = newStmtList()

macro onSystemAddTo*(typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let
    sysIndex = findSystemIndex(systemName)
    typeIndex = typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  doAssert sysIndex.found, "Cannot find system \"" & systemName & "\" in defined systems: " & systemInfo.allSystemNames
  if systemInfo[sysIndex.index].onAddToCode.hasKey(typeIndex): 
    systemInfo[sysIndex.index].onAddToCode[typeIndex].add newBlockStmt(actions)
  else:
    systemInfo[sysIndex.index].onAddToCode[typeIndex] = newStmtList(newBlockStmt(actions))
  result = newStmtList()

macro onSystemRemove*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code to be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  
  typeInfo[typeIndex].onRemoveAnySystemCode.add newBlockStmt(actions)
  result = newStmtList()

macro onSystemRemoveFrom*(typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code to be inserted.
  let
    sysFound = findSystemIndex(systemName)
    typeIndex = typeStringToId($typeToUse)
  doAssert typeIndex.int != 0, "Cannot find type " & $typeToUse & " in registered components "
  doAssert sysFound.found, "Cannot find system \"" & systemName & "\" in defined systems: " & systemInfo.allSystemNames
  
  let sysIndex = sysFound.index

  if systemInfo[sysIndex].onRemoveFromCode.hasKey(typeIndex): 
    systemInfo[sysIndex].onRemoveFromCode[typeIndex].add newBlockStmt(actions)
  else:
    systemInfo[sysIndex].onRemoveFromCode[typeIndex] = newStmtList(newBlockStmt(actions))
  result = newStmtList()

proc doNewEntityWith(entOpts: ECSEntityOptions, componentList: NimNode): NimNode {.compileTime.} =
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
      typeId = tyName.typeStringToId
      fieldName = tyName.toLowerAscii & instPostfix
      fieldIdent = ident fieldName
      instTypeIdent = ident instanceTypeName(tyName)
      info = typeInfo[typeId.int]

    if typeId in compIds: error "newEntityWith has been passed more than one component of type " & tyName

    # For later use in updateSystems
    compIds.add typeId

    # Create storage for this component
    let compOwner = info.systemOwner
    if compOwner != InvalidSystemIndex:
      # Owned component access is part of the system row and cannot be accessed
      # until a new row is added.
      let sysNode = systemInfo[compOwner.int].instantiation
      # This should correspond to the next index for groups, which is added just below.
      componentDecl.add genFieldAssignment(fieldName, false, quote do:
        `sysNode`.count.`instTypeIdent`
      )
    else:
      # Create storage and assign the parameter value.
      componentDecl.add genFieldAssignment(fieldName, false, quote do:
        newInstance(`component`))

    let compRef = newDotExpr(fieldIdent, ident "toRef")
    addToEntity.add addComponentRef(entity, compRef, entOpts)

    # Component add user inline event.
    let userAddToEnt = info.onAddToEntCode
    if userAddToEnt.len > 0:
      userCompAddCode.add(quote do:
        block:
          ## Current component being added to entity. 
          template curComponent: untyped {.used.} = `fieldIdent`
          `userAddToEnt`
      )

    # Component add user callback event.
    if info.onAddCallback.len > 0:
      # Check for this type's initialiser.
      let cbProcName = ident addCallbackName(tyName)
      userCompAddCode.add(quote do:
        `cbProcName`(`entity`, `fieldIdent`)
      )

  let setOp =
    if entOpts.useSet:
      let
        setType = ident enumName()
        setVal = componentListToSet(compIds, setType)
      entSetIncl(entOpts, entIdNode, setVal)
    else:
      newEmptyNode()

  var
    processed: HashSet[SystemIndex]
    userSysAddCode = newStmtList()

  statements.add(quote do:
    var `entity` = newEntity()
    `componentDecl`
    `setOp`
  )
  
  var
    missingComps, ownedComps: seq[ComponentTypeId]
    userAddedEvents = newStmtList()

  for compId in compIds:      
    let ownerSystem = typeInfo[compId.int].systemOwner
    var satisfied = true

    # Parameters that include owned components must fully satisfy their owning system.
    if ownerSystem != InvalidSystemIndex:
      ownedComps.add compId
      for comp in systemInfo[ownerSystem.int].requirements:
        if comp != compId and comp notin compIds and comp notin missingComps:
          satisfied = false
          missingComps.add comp

    if satisfied:
      for sys in typeInfo[compId.int].systems:
        if sys notin processed:
          processed.incl sys

          # We only need to update systems that are fully qualified by componentList.
          var discarded: bool
          for req in systemInfo[sys.int].requirements:
            if req notin compIds:
              discarded = true

          if not discarded:
            # Add component to system.
            let (sysUpdate, event, eventsExist) = genSystemUpdate(entity, sys, compIds, componentList)
            statements.add sysUpdate
            if eventsExist:
              userAddedEvents.add event


  if missingComps.len > 0:
    error "Owned component(s) [" & ownedComps.commaSeparate &
      "] need their owner systems completed with component(s): [" & missingComps.commaSeparate & "]"

  statements.add(quote do:
    template curEntity: EntityRef {.used.} = `entity`
    `addToEntity`
    `userSysAddCode`
    `userCompAddCode`
    `userAddedEvents`
    `entity`
  )

  result = newBlockStmt(statements)
  
  genLog "# newEntityWith " & compIds.commaSeparate & ":\n", result.repr

proc makeNewEntityWith*(entOpts: ECSEntityOptions): NimNode =
  let componentList = ident "componentList"
  quote do:
    macro newEntityWith*(`componentList`: varargs[typed]): untyped =
      ## Create an entity with the parameter components.
      ## This macro statically generates updates for only systems
      ## entirely contained within the parameters and ensures no
      ## run time component list iterations and associated checks.
      doNewEntityWith(`entOpts`, `componentList`)

## This structure is used at compile-time to convert a list of components to a
## map of tasks for code generation to use.
type ComponentParamInfo* = ref object
  ## The component types of arguments in their given order.
  passed*:          seq[ComponentTypeId]
  ## The actual code/variable/data given for the component matching `passed`.
  values*:          seq[NimNode]
  ## Non-owned components belonging to owner systems that haven't been passed.
  ## These must be fetched at run-time, and must exist for the owner system row
  ## to be created.
  requiredFetches*: seq[ComponentTypeId]
  ## These components form the union of non-owned components that share
  ## a system with one or more of the components in `passed`.
  ## These are fetched at run-time and trigger system updates when they exist.
  ## For example given system `sysA` uses components [A, B, C], if you
  ## `addComponent A()` on its own, `B` and `C` will be in `lookFor` to see
  ## if the entity satisfies `sysA` at run-time, and a row can be added.
  lookFor*:         seq[ComponentTypeId]
  ## Components that require storage generation, stored as an index into `passed`.
  generateIdx*:     seq[int]
  ## Owned components, their owning systems, and index into `passed`.
  owned*:           seq[tuple[id: ComponentTypeId, sys: SystemIndex, passedIdx: int]]
  ## Non-owning system that may or may not be satisfied.
  unownedSystems*:  seq[SystemIndex]
  ## Systems that own passed components and must be fully satisfied.
  ownedSystems*:    seq[SystemIndex]

proc processComponentParameters(componentList: NimNode): ComponentParamInfo =
  ## Collect information from parameters passed as part of a state change
  ## and process into targetted lists for each code generation job.

  # Generate a list of SystemIndexes used by each ComponentTypeId.
  result = ComponentParamInfo()

  # Initial parse of parameters to component ids and value nodes.
  for compNode in componentList:
    let tyName = compNode.findType
    doAssert tyName != "", "Cannot determine type name of argument:\n" & compNode.treeRepr & "\ngetType:\n" & compNode.getType.repr
    
    # Find the ComponentTypeId for this type.
    let typeId = tyName.typeStringToId
    if typeId in result.passed: error "Passed more than one component of type " & tyName

    if typeId in result.passed:
      error "Component type " & tyName & " appears more than once in parameters"
    
    result.passed.add typeId
    result.values.add compNode

  # Process parameters for dependent requirements.
  for i, typeId in result.passed:
    let
      ownerSystem = typeInfo[typeId.int].systemOwner
      isOwned = ownerSystem != InvalidSystemIndex
    
    if isOwned:
      if ownerSystem notin result.ownedSystems:
        # Owning systems must have all their components fully satisfied.
        result.owned.add (typeId, ownerSystem, i)
        result.ownedSystems.add ownerSystem

        # Add component instances that must be valid to support the owner system for a component passed.
        for comp in systemInfo[ownerSystem.int].requirements:
          if comp notin result.passed:
            if typeInfo.isOwned comp:
              let
                curName = typeInfo[comp.int].typeName
                typeStr = typeInfo[typeId.int].typeName
              error "Cannot add " & typeStr & ", missing required owned component " & curName
            elif comp notin result.requiredFetches:
              result.requiredFetches.add comp
              result.lookFor.add comp

    else:
      # Passed components that are not owned must generate a storage slot index.
      result.generateIdx.add i

    # Create a list of components we're missing that would potentially
    # satisfy all systems that use our parameters.
    # We only need to check systems directly using each component,
    # not dependent owner systems, as we can assume incomplete
    # ownership has been disallowed above.
    for sys in typeInfo[typeId.int].systems:

      let sysIsOwner = systemInfo[sys.int].ownedComponents.len > 0
      if not sysIsOwner:
        # Owner systems that weren't in the parameters are ignored since
        # they cannot exist in isolation.

        if sys notin result.unownedSystems:
          result.unownedSystems.add sys

        for typeId in systemInfo[sys.int].requirements:
          if typeId notin result.passed and typeId notin result.lookFor:
            result.lookFor.add typeId

proc genComponents*(entity: NimNode, compInfo: ComponentParamInfo): NimNode =
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
      info = typeInfo[typeId.int]
      typeStr = info.typeName & instPostfix
      instVarStr = typeStr.toLower
      instVar = ident instVarStr
      paramVal = compInfo.values[idx]

    let
      hasUserAddCode = info.onAddToEntCode.len > 0
      hasUserCallback = info.onAddCallback.len > 0

    if hasUserAddCode:
      let userCode = info.onAddToEntCode
      userEventDecls.add(quote do:
        block:
          template curComponent: untyped {.used.} = `instVar`
          template curEntity: untyped {.used.} = `entity`
          `userCode`
      )
    if hasUserCallback:
      let cbProcName = ident addCallbackName(info.typeName)
      userEventDecls.add(quote do:
        `cbProcName`(`entity`, `instVar`)
      )

    componentDecl.add genFieldAssignment(instVarStr, false, quote do: newInstance(`paramVal`))
  
  if interceptPrecursor.len > 0:
    result.add interceptPrecursor
  result.add componentDecl
  result.add userEventDecls

proc buildFetch(entity: NimNode, compInfo: ComponentParamInfo, entOpts: ECSEntityOptions): NimNode =
  ## Fetch the `compInfo.lookFor` components from the entity's component list into instance variables.
  result = newStmtList()
  var
    compDecl = nnkVarSection.newTree()
    fetch = newStmtList()
    fieldCounter = ident "fieldCounter"
    targetHigh = compInfo.lookFor.len

  if targetHigh > 0:
    let multipleFetches = targetHigh > 1

    if multipleFetches:
      result.add(quote do:
        var `fieldCounter`: int
      )

    case entOpts.componentStorageFormat
    of csSeq, csArray:
      # Loop through components and extract with a case statement.
      var fetchCase = nnkCaseStmt.newTree()
      let
        fetchCompIdent = ident "curComp"
        distinguisher = quote do: `fetchCompIdent`.typeId.int
      fetchCase.add distinguisher

      for typeId in compInfo.lookFor:
        let
          typeStr = typeInfo[typeId.int].typeName
          fieldName = typeStr.toLowerAscii & instPostfix
          fieldIdent = ident fieldName
          instTypeIdent = ident typeStr.instanceTypeName

        # Create component variable to be populated by the fetch.
        compDecl.add genField(fieldName, false, instTypeIdent)

        let
          getInstance = quote do:
            `fetchCompIdent`.index
          ofBranch = nnkOfBranch.newTree(newIntLitNode(typeId.int))
          ofStmts = 
            if not multipleFetches:
              # With only one component to fetch we don't need to track count.
              newStmtList(quote do:
                `fieldIdent` = `instTypeIdent`(`getInstance`)
                break
              )
            else:
              newStmtList(quote do:
                `fieldIdent` = `instTypeIdent`(`getInstance`)
                `fieldCounter` += 1
                if `fieldCounter` == `targetHigh`: break
              )
        ofBranch.add ofStmts
        fetchCase.add ofBranch

      fetchCase.add nnkElse.newTree(newStmtList(quote do: discard))
      fetch.add(quote do:
        for `fetchCompIdent` in entityData(`entity`.entityId).componentRefs:
          `fetchCase`
      )

    of csTable:
      ## Tables can directly fetch components.
      for comp in compInfo.lookFor:
        let
          typeStr = typeInfo[comp.int].typeName
          instTypeIdent = ident(typeStr.instanceTypeName)
          fieldName = typeStr.toLowerAscii & instPostfix
          fieldIdent = ident fieldName
        fetch.add(quote do:
          let `fieldIdent` = `instTypeIdent`(entityData(`entity`.entityId).componentRefs.getOrDefault(`comp`.ComponentTypeId).index)
        )

    result.add compDecl
    result.add fetch

proc checkRequired(options: ECSEntityOptions, compInfo: ComponentParamInfo): NimNode =
  ## Check fetched components that satisfy owned component systems are valid.
  result = newStmtList()
  if compInfo.requiredFetches.len > 0:
    var checkSystem: seq[NimNode]

    for typeId in compInfo.requiredFetches:
      let
        typeStr = typeInfo[typeId.int].typeName
        typeField = ident typeStr.toLower & instPostfix
      checkSystem.add newDotExpr(typeField, ident "alive")

    let
      matchesSystem = genInfixes(checkSystem, "and")
      unsatisfiedErrStr =
        newLit "Cannot complete this add operation because systems that own the parameter components are not fully satisfied, missing: " & compInfo.requiredFetches.commaSeparate
      errorResponse =
        case options.errors.incompleteOwned
        of erAssert:
          quote do:
            assert `matchesSystem`, `unsatisfiedErrStr`
        of erRaise:
          quote do:
            if not(`matchesSystem`): raise newException(ValueError, `unsatisfiedErrStr`)
    result.add errorResponse

proc addConditionalSystems(entity: NimNode, compInfo: ComponentParamInfo): NimNode =
  ## Add components to systems that have no owned components.
  ## These systems may or may not be updated depending on fetched instances.
  result = newStmtList()
  var
    addedEvents = newStmtList()
    conditionalAddedEvents = newStmtList()
  for sys in compInfo.unownedSystems:
    template sysInfo: untyped = systemInfo[sys.int]
    let
      sysTupleStr = sysInfo.systemName.tupleName
      sysTupleVar = ident(sysTupleStr.toLower)
    
    var
      checkSystem: seq[NimNode]
      updateTupleFields = newStmtList()

    updateTupleFields.add(quote do:
      `sysTupleVar`.entity = `entity`)

    # Generate the code that adds this component instance to this system.
    # This includes user events and special handling for owned components.
    let (updateSystem, addedEvent, eventsExist) = genSystemUpdate(entity, sys, compInfo.passed, compInfo.values)

    # Build the checks to see if this system matches.
    for typeId in sysInfo.requirements:
      let
        typeStr = typeInfo[typeId.int].typeName
        typeField = ident typeStr.toLower & instPostfix

      if typeId in compInfo.lookFor or typeId in compInfo.passed:
        if typeId notin compInfo.passed:
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

proc addOwned(entity: NimNode, compParamInfo: ComponentParamInfo): NimNode =
  ## Assumes we have everything we need to add to owned systems.
  ## Any systems updated here must be fully qualified with their components.
  ## Where they are not given as parameters, their presence must be enforced
  ## at run-time.
  result = newStmtList()

  for sys in compParamInfo.ownedSystems:
    template sysInfo: untyped = systemInfo[sys.int]
    let
      sysName = sysInfo.systemName
      systemNode = sysInfo.instantiation
      sysTupleStr = sysInfo.systemName.tupleName
      sysTupleType = ident sysTupleStr
      sysTupleVar = ident(sysTupleStr.toLower)
      tupleSetup = newStmtList()
      sysOpts = sysInfo.options

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

    # TODO: Migrate to genSystemUpdate

    for typeId in sysInfo.requirements:
      # Populate system tuple.
      let
        compInfo = typeInfo[typeId.int]
        typeStr = compInfo.typeName
        lcTypeStr = typeStr.toLower
        typeField = ident lcTypeStr
        instType = ident typeStr.instanceTypeName
        instField = ident lcTypeStr & instPostfix
        ownedByThisSystem = compInfo.systemOwner == sys

      if ownedByThisSystem:
        # When a component is owned, it must be given as a parameter.
        let index = compParamInfo.passed.find(typeId)
        doAssert index > -1,
          "Cannot find owned component \"" & typeStr &
          "\" when adding to system \"" & sysName & "\""

        let
          value = compParamInfo.values[index]
          sysTupleVar = ident(sysTupleStr.toLower)
          instField = ident lcTypeStr & instPostfix

          (onAddCode, userInitCode, updateCode) = sys.ownedUserInitEvents(sysTupleVar, instField, typeId, entity, value)

        # Updating state lists is usually done in the component's
        # creation procs for non-owned components.
        stateUpdates.add updateOwnedComponentState(typeId, sys, sysHighVar)

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

      userSysAddCode.addUserSysCode(entity, sys, typeId, instField)

    let updateIndex = entity.updateIndex(sys, sysHighVar, sysOpts)

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

proc doAddComponents(entOpts: ECSEntityOptions, entity: NimNode, componentList: NimNode): NimNode =
  var inner = newStmtList()

  let componentInfo = processComponentParameters(componentList)

  for typeId in componentInfo.passed:
    let
      typeStr = typeInfo[typeId.int].typeName
      typeIdent = ident typeStr
    inner.add.add(quote do:
      assert `typeIdent` notin `entity`, "Component \"" & `typeStr` & "\" already exists in entity"
    )

  inner.add genComponents(entity, componentInfo)
  inner.add buildFetch(entity, componentInfo, entOpts)
  inner.add checkRequired(entOpts, componentInfo)
  inner.add addOwned(entity, componentInfo)
  inner.add addToEntityList(entity, componentInfo.passed, entOpts)
  inner.add addConditionalSystems(entity, componentInfo)
  
  # Build return tuple.
  var returnType = nnkPar.newTree()
  for typeId in componentInfo.passed:
    let
      typeStr = typeInfo[typeId.int].typeName
      fieldIdent = ident typeStr.toLower
      typeIdent = ident typeStr.toLower & instPostfix
    returnType.add nnkExprColonExpr.newTree(fieldIdent, typeIdent)
  inner.add returnType

  result = quote do:
    block:
      `inner`

  debugPerformance "AddComponents for " & componentInfo.passed.commaSeparate & " complete."

  genLog "\n# macro addComponents(" & componentInfo.passed.commaSeparate & "):\n", result.repr

proc makeAddComponents*(entOpts: ECSEntityOptions): NimNode =
  let
    componentList = ident "componentList"
    entity = ident "entity"
  quote do:
    macro addComponents*(`entity`: EntityRef, `componentList`: varargs[typed]): untyped =
      ## Add components to an entity and update systems in one pass.
      doAddComponents(`entOpts`, `entity`, `componentList`)

    proc addComponent*[T: ComponentTypeclass](entity: EntityRef, component: T): T.instanceType {.discardable.} =
      ## Add a component to an entity and return the instance.
      entity.addComponents(component)[0]

    # TODO: addOrUpdate that allows granular updating fields rather than whole component item.
    proc addOrUpdate*[T: ComponentTypeclass](entity: EntityRef, component: T): T.instanceType {.discardable.} =
      ## Add `component` to `entity`, or if `component` already exists, overwrite it.
      ## Returns the component instance.
      let fetched = entity.fetchComponent T.type
      if fetched.valid:
        # Replace original. No further work is required as the types or indexes have not been updated.
        fetched.update component
        fetched
      else:
        # Add as normal.
        entity.addComponent component
    
    proc addIfMissing*[T: ComponentTypeclass](entity: EntityRef, component: T): T.instanceType {.discardable.} =
      ## Add a component only if it isn't already present.
      ## If the component is already present, no changes are made and an invalid result is returned.
      ## If the component isn't present, it will be added and the instance is returned.
      if not entity.hasComponent T.type:
        result = entity.addComponent component

proc removeComponentRef(entityId, index: NimNode, componentTypeId: int, options: ECSEntityOptions): NimNode = 
  # Removes a component from entity storage.
  # * Doesn't touch systems
  # * Doesn't update the intset for hasComponent.

  let remIdx =
    if options.componentStorageFormat == csTable:
      quote do: `componentTypeId`.ComponentTypeId
    else:
      newIntLitNode(componentTypeId)

  case options.componentStorageFormat
  of csTable:
    # No error if key doesn't exist.
    quote do:
      entityData(`entityId`).componentRefs.del(`remIdx`)
  of csArray:
    quote do:
      let curHigh = entityData(`entityId`).nextCompIdx - 1
    
      if `index` < curHigh:
        # Swap last item with this one.
        entityData(`entityId`).componentRefs[`index`] = entityData(`entityId`).componentRefs[curHigh]
      # Cull length
      entityData(`entityId`).nextCompIdx -= 1
  of csSeq:
    quote do:
      # Cull length
      entityData(`entityId`).componentRefs.del(`index`)

proc userRemoveCode(systemIndex: int, system, rowIdent, entIdIdent, entity: NimNode, sysOpts: ECSSysOptions): NimNode =
  # Add user defined system remove code.
  result = newStmtList()
  for typeId in systemInfo[systemIndex].requirements:
    template info: untyped = typeInfo[typeId.int]
    let
      typeName = info.typeName
      instType = ident instanceTypeName(typeName)
      fieldIdent = ident typeName.toLowerAscii()
      curCompTemplate =
        if info.systemOwner.int == systemIndex:
          quote do:
            template curComponent: untyped {.used.} = `instType`(`rowIdent`)
        else:
          quote do:
            template curComponent: untyped {.used.} = `system`.groups[`rowIdent`].`fieldIdent`

    template sysInfo: untyped = systemInfo[systemIndex]
    let userSysRemFrom = sysInfo.onRemoveFromCode.getOrDefault typeId 
    if userSysRemFrom != nil:
      result.add(quote do:
        block:
          ## Access to the current row's entity.
          template curEntity: untyped {.used.} = `system`.groups[`rowIdent`].entity
          ## Access to the current system's row.
          template curRow: untyped {.used.} = `system`.groups[`rowIdent`]
          ## Access to current updating system variable.
          template curSystem: untyped {.used.} = `system`
          ## Component being removed from this system.
          `curCompTemplate`
          `userSysRemFrom`
      )

    if info.onRemoveAnySystemCode.len > 0:
      let sysRem = info.onRemoveAnySystemCode
      if sysRem != nil:
        result.add(quote do:
          block:
            ## Access to the current row's entity.
            template curEntity: untyped {.used.} = `system`.groups[`rowIdent`].entity
            ## Access to the current system's row.
            template curRow: untyped {.used.} = `system`.groups[`rowIdent`]
            ## Access to current updating system variable.
            template curSystem: untyped {.used.} = `system`
            ## Component being removed from this system.
            `curCompTemplate`
            `sysRem`
        )

proc removeSysReference(systemIndex: int, sys, sysRowExists, rowIdent, entIdIdent, entity: NimNode, sysOpts: ECSSysOptions): NimNode =
  # Remove an entity's tuple from a system.
  # * Does not update the entity's storage

  let
    topIdxIdent = ident "topIdx"
    updatedRowEntIdent = ident "updatedRowEnt"
    updatedEntId = newDotExpr(updatedRowEntIdent, ident "entityId")
    # updates index with entity id to group row.
    setIndex = sys.indexWrite(updatedEntId, rowIdent, sysOpts)
    sysInfo = systemInfo[systemIndex.int]
    sysName = sysInfo.systemName
  
  # TODO: If this system contains an owned component, don't swap the row on deletion.

  let
    trimGroup = 
      case sysOpts.storageFormat
      of ssSeq:
        quote do:
          let `topIdxIdent` = `sys`.high
          if `rowIdent` < `topIdxIdent`:
            `sys`.groups[`rowIdent`] = `sys`.groups[`topIdxIdent`]
            # Get entity that's been moved.
            let `updatedRowEntIdent` = `sys`.groups[`rowIdent`][0]
            # Update the index for the moved row
            `setIndex`

          assert `sys`.groups.len > 0, " System " & `sys`.name & " has empty group but is scheduled to delete from row " & $`rowIdent` & ". Top row is " & $`topIdxIdent`
          `sys`.groups.setLen(`sys`.groups.len - 1)
      of ssArray:
        quote do:
          let `topIdxIdent` = `sys`.high
          if `rowIdent` < `topIdxIdent`:
            `sys`.groups[`rowIdent`] = `sys`.groups[`topIdxIdent`]
            # Get entity that's been moved.
            let `updatedRowEntIdent` = `sys`.groups[`rowIdent`][0]
            # Update the index for the moved row
            `setIndex`

          `sys`.nextFreeIdx -= 1

  # Get code defined in the system's `removed:` section.
  var userRemovedEvent = newStmtList()
  if sysInfo.onRemoved.len > 0:
    let
      doEcho =
        if sysOpts.echoRunning != seNone:
          quote do:
            echo `sysName` & " removing entry"
        else: newEmptyNode()

      userRemovedEventCode = sysInfo.onRemoved
    userRemovedEvent.add(quote do:
      block:
        template item: untyped {.used.} = `sys`.groups[`rowIdent`]
        template sys: untyped {.used.} = `sys`
        `doEcho`
        `userRemovedEventCode`
    )

  let delIndex = sys.indexDel(entIdIdent, sysOpts)
  let r = quote do:
    if `sysRowExists`:
      `userRemovedEvent`
      `delIndex`
      `trimGroup`
  r

proc makeRemoveComponentDirect*(entOpts: ECSEntityOptions): NimNode =
  #[
    Important note!
    If you call removeComponent whilst in a system using that component, the current `item` will change!
    In this case, item.entity.removeComponent will cause item.entity and its components to be different.
    This happens because `entity.removeComponent` and `entity.delete` remove items from systems by swapping
    the item with the last one in the list and reducing the list length.
  ]#
  let
    index = ident "index"
    entityIdent = ident "entity"
    entityIdIdent = ident "entityId"
    componentLen = componentRefsLen(entityIdIdent, entOpts)
    alive = ident "alive" # Late bind for this template
    rowIdent = ident "row"

  result = newStmtList()

  for typeId in ecsComponentsToBeSealed:
    let
      typeName = typeInfo[typeId.int].typeName
      tyDelete = ident deleteInstanceName()

      compTypedesc = nnkBracketExpr.newTree(ident "typedesc", ident typeName)
      # System indexes that use the component being removed
      relevantSystems = typeInfo[typeId.int].linked
      removeIdxIdent = ident "compIdx"
      foundComp = ident "found"

    var
      updateSystems = newStmtList()
      userUpdates = newStmtList()
      findSysCode = newStmtList()
      foundDecl = nnkVarSection.newTree()
      componentsToRemove: set[uint16]
      # TODO: Could use the system ground truth to determine the number of components we need to
      # remove from the component list.
      # ie; `if aFound: numComponentsToRemove += 1` etc.
      # This would mean we wouldn't keep searching the component list for, say, owned components
      # as we know how many to look for, especially as we've already determined whether the system
      # is populated.
      numComponentsToRemove = 1

    # Always remove parameter component.
    componentsToRemove.incl typeId.uint16
    
    # Gather components that will be affected.
    for systemIndex in relevantSystems:
      template sysInfo: untyped = systemInfo[systemIndex.int]
      
      # We must remove all references to every relevant system here,
      # as we're removing a required component for the system to run.
      let
        sysOpts = sysInfo.options
        sysName = sysInfo.systemName
        sysIdent = ident systemVarName(sysName)
        foundSys = ident sysName.toLower & "Found"
        foundSysRow = ident sysName.toLower & "FoundRow"

      foundDecl.add newIdentDefs(foundSys, ident "bool")
      foundDecl.add newIdentDefs(foundSysRow, ident "int")

      # Add a check to see if the entity is in this system.
      let
        sysNode = systemInfo[systemIndex.int].instantiation
        tryGetIndex = sysNode.indexTryGet(entityIdIdent, rowIdent, sysOpts)
      findSysCode.add(quote do:
        if `tryGetIndex`:
          `foundSys` = true
          `foundSysRow` = `rowIdent`
        )
      
      updateSystems.add(removeSysReference(systemIndex.int, sysIdent, foundSys, foundSysRow, entityIdIdent, entityIdent, sysOpts))

      # When removing a component that's also part of an owned system, we must also remove any owned components
      # for that system, since we are effectively invalidating the owned component's storage.
      for ownedComp in systemInfo[systemIndex.int].ownedComponents:
        if ownedComp != typeId:
          # Increment search counter to include owned components.
          if ownedComp.uint16 notin componentsToRemove:
            numComponentsToRemove += 1
            componentsToRemove.incl ownedComp.uint16

      # Add event handlers for remove.
      let userSysRemove = userRemoveCode(systemIndex.int, sysIdent, foundSysRow, entityIdIdent, entityIdent, sysOpts)
      if userSysRemove.len > 0:
        userUpdates.add(quote do:
          if `foundSys`:
            `userSysRemove`
        )

    var removeRefCore = newStmtList()
    let
      compsDeleted = ident "compsDeleted"
      delCompCount = newLit numComponentsToRemove
    
    var compIdx = 0

    # Build code to remove the selected components.
    for delComp in componentsToRemove:
      template delCompInfo: untyped = typeInfo[delComp.int]
      let
        typeName = delCompInfo.typeName
        delInstanceType = ident instanceTypeName(typeName)
      var
        userCompRemove = newStmtList()
        updateOwnedAliveState = newStmtList()

      if delCompInfo.onRemoveFromEntCode.len > 0:
        let compRem = delCompInfo.onRemoveFromEntCode
        if compRem != nil:
          userCompRemove.add(quote do:
            block:
              template curComponent: untyped {.used.} = `delInstanceType`(`foundComp`.index)
              `compRem`
          )

      if delCompInfo.systemOwner != InvalidSystemIndex:
        # Update alive state.
        let aliveIdent = ident aliveStateInstanceName(typeName)
        updateOwnedAliveState.add(quote do:
          `aliveIdent`[`foundComp`.index.int] = false
        )

      let
        removeCompFromEntity = removeComponentRef(entityIdIdent, removeIdxIdent, delComp.int, entOpts)

        coreDelete = quote do:
          `userCompRemove`
          `tyDelete`(`delInstanceType`(`foundComp`.index))
          `removeCompFromEntity`
          `updateOwnedAliveState`
      
      case entOpts.componentStorageFormat
      of csTable:
        removeRefCore.add(quote do:
          `foundComp` = entityData(`entityIdIdent`).componentRefs.getOrDefault(`delComp`.ComponentTypeId)
          if `foundComp`.typeId == `delComp`.ComponentTypeId:
            `coreDelete`
          )
      of csSeq, csArray:
        # TODO: Replace if statements with case when looping components.
        removeRefCore.add(quote do:
          if `foundComp`.typeId == `delComp`.ComponentTypeId:
            `coreDelete`
            `compsDeleted` = `compsDeleted` + 1
            if `compsDeleted` == `delCompCount`:
              break
            `removeIdxIdent` = `removeIdxIdent` - 1
            continue
        )
      compIdx += 1

    let removeRef =
      case entOpts.componentStorageFormat
      of csTable:
        quote do:
          var `foundComp`: ComponentRef
          `removeRefCore`
      of csSeq, csArray:
        quote do:
          var
            `foundComp`: ComponentRef
            `removeIdxIdent` = `componentLen` - 1
            `compsDeleted` = 0
          while `removeIdxIdent` >= 0:
            `foundComp` = entityData(`entityIdIdent`).componentRefs[`removeIdxIdent`]
            `removeRefCore`
            `removeIdxIdent` = `removeIdxIdent` - 1

    let
      setVal = ident "ce" & typeName
      setOp = entSetExcl(entOpts, entityIdIdent, setVal)
      doRemoveName = ident "doRemove" & typeName

    result.add(quote do:
      proc `doRemoveName`(`entityIdent`: EntityRef) =
        assert `entityIdent`.`alive`
        let `entityIdIdent` = `entityIdent`.entityId

        if entityData(`entityIdIdent`).setup:
          ## Access to currently updating entity.
          template curEntity: untyped {.used.} = `entityIdent`
          # RowIdent is used by updateSystems.
          var `rowIdent` {.used.}: int
          `foundDecl`
          `findSysCode`
          `userUpdates`
          # Update set if required.
          `setOp`
          `removeRef`
          # Remove this entity from all relevant systems.
          `updateSystems`
            
      template removeComponent*(`entityIdent`: EntityRef, compType: `compTypedesc`) =
        ## Remove the component.
        static:
          if inSystemAll and inSystemIndex in `relevantSystems`:
            # Calling removeComponent from within a system that uses the component.
            # We don't know if it's the current row's entity or some other entity.
            sysRemoveAffectedThisSystem = true
        `doRemoveName`(`entityIdent`)
    )
  genLog "# Remove component:\n", result.repr

proc clearAllEntComponentRefs(entityId: NimNode, options: ECSEntityOptions): NimNode =
  case options.componentStorageFormat
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

proc recyclerAdd(ecStateNode, entIdNode: NimNode, options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.add `entIdNode`
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      let nextIdx = `ecStateNode`.`rLen`
      assert nextIdx < `ecStateNode`.entityRecycler.len
      
      `ecStateNode`.entityRecycler[nextIdx] = `entIdNode`
      `ecStateNode`.`rLen` += 1

proc recyclerClear*(ecStateNode: NimNode, options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.setLen 0
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      `ecStateNode`.`rLen` = 0

proc makeDelete*(options: ECSEntityOptions): NimNode =
  ## Generates delete procedures for the current entity.
  ## Delete will be created with all the systems that have been seen since the last
  ## `makeEcs` invocation.
  let
    ent = ident("entity")
    delProcName = ident("delete")
    compRefIdent = ident("compRef")
    storageVar = ident entityStorageVarName()
    totalSystemCount = systemInfo.len
    rowIdent = ident "row"
    entIdIdent = ident "entityId"
    visitedIdent = ident "visited"
    clearComponents = clearAllEntComponentRefs(entIdIdent, options)
    foundDecls = nnkVarSection.newTree()
    foundChecks = newStmtList()
    visitedArray = quote do:
      var `visitedIdent`: array[0 .. `totalSystemCount`, bool]

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
    processedSystems: set[int16]
    userCodeExists, userVisitedRequired: bool

  for compId in ecsComponentsToBeSealed:
    let
      compIdx = compId.int
      tyName = typeInfo[compIdx.int].typeName
      tyInstance = newIdentNode instanceTypeName(tyName)

    template compInfo: untyped =  typeInfo[compIdx.int]

    var
      removeBody = newStmtList()
      userBody = newStmtList()

    # For each component, update any systems referenced.  
    for sysIdx in typeInfo[compIdx.int].systems:
      # Process only new systems we haven't seen before for this set of entities.
      if sysIdx in ecsSystemsToBeSealed:
        template sysInfo: untyped =  systemInfo[sysIdx.int]
        let
          sysOpts = sysInfo.options
          sysNameUpper = sysInfo.systemName.capitalizeAscii
          sysNameLower = sysInfo.systemName.toLowerAscii
          sysIdent = ident systemVarName(sysNameUpper)
          sysNode = sysInfo.instantiation
          tryGetSys = sysNode.indexTryGet(entIdIdent, rowIdent, sysOpts)
          foundSys = ident sysNameLower & "Found"
          foundSysRow = ident sysNameLower & "Row"

          removeSystemEntry = removeSysReference(sysIdx.int, sysIdent, foundSys, foundSysRow, entIdIdent, ent, sysOpts)
          userSysRemove = userRemoveCode(sysIdx.int, sysIdent, foundSysRow, entIdIdent, ent, sysOpts)
        
        if sysIdx.int16 notin processedSystems:
          processedSystems.incl sysIdx.int16
          foundDecls.add newIdentDefs(foundSys, ident "bool")
          foundDecls.add newIdentDefs(foundSysRow, ident "int")
          foundChecks.add(quote do:
            if `tryGetSys`:
              `foundSys` = true
              `foundSysRow` = `rowIdent`
            )

        if compInfo.systemOwner == sysIdx:
          # Update alive state for owned components.
          let
            aliveIdent = ident aliveStateInstanceName(tyName)
            cIdx = compIdx.int
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

    # Remove code by component.
    if compInfo.onRemoveFromEntCode.len > 0:
      let compRem = compInfo.onRemoveFromEntCode
      if compRem != nil:
        includeEntityTmpl = true
        userBody.add(quote do:
          block:
            ## Current component being removed from entity. 
            template curComponent: untyped {.used.} = `tyInstance`(`compRefIdent`.index)
            `compRem`
        )
    
    if removeBody.len > 0:
      var ofNodeRemove = nnkOfBranch.newTree()
      ofNodeRemove.add newDotExpr(newIntLitNode(compIdx), ident "ComponentTypeId")
      ofNodeRemove.add newStmtList(removeBody)
      caseStmtRemove.add(ofNodeRemove)

    if compInfo.onRemoveCallback.len > 0:
      includeEntityTmpl = true
      let cbProcName = ident removeCallbackName(tyName)
      userBody.add(quote do:
        `cbProcName`(`ent`, `tyInstance`(`compRefIdent`.index)))

    if userBody.len > 0:
      userCodeExists = true
      var ofNodeUser = nnkOfBranch.newTree()
      ofNodeUser.add newDotExpr(newIntLitNode(compIdx), ident "ComponentTypeId")
      ofNodeUser.add newStmtList(userBody)
      caseStmtUserCode.add(ofNodeUser)

  caseStmtRemove.add nnkElse.newTree(newStmtList(quote do: discard))
  caseStmtUserCode.add nnkElse.newTree(newStmtList(quote do: discard))
  
  updateSystems.add caseStmtRemove

  let
    # For pointer arrays, the GC needs to be informed about the componentRef sequence.
    gcCleanup =
      case options.entityStorageFormat
      of esSeq, esArray: newEmptyNode()
      of esPtrArray:
        if options.componentStorageFormat in [csSeq, csTable]:
          quote do:
            GC_Unref(entityData(`entIdIdent`).componentRefs)
        else:
          newEmptyNode()
    recyclerAdd = storageVar.recyclerAdd(entIdIdent, options)
    recyclerClear = storageVar.recyclerClear(options)
    initSet =
      if options.useSet:
        quote do:
          entityData(`entIdIdent`).exists = {}        
      else: newEmptyNode()

  let
    clearRecycler = storageVar.recyclerClear(options)
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
      else: newStmtList()

  result = quote do:
    proc doDelete(`ent`: EntityRef) =

      if not `ent`.alive: return
      let `entIdIdent` = `ent`.entityId
      if entityData(`entIdIdent`).setup:
        var `rowIdent`: int

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

    template `delProcName`*(`ent`: EntityRef) =
      static:
        if inSystem and (not inSystemDeleteRow): systemCalledDelete = true
      doDelete(`ent`)

    proc deleteAll*(entities: Entities) =
      for i in 0 ..< entities.len:
        entities[i].delete

    proc resetEntityStorage* =
      ## This deletes all entities, removes them from associated systems and resets next entity.
      # TODO: More efficient to dispense with the book keeping and just clear systems & storage manually.
      # Good for testing for issues with delete though.
      # NOTE: ENTITY INSTANCE FIELD SHOULD REMAIN INTACT!
      for i in 0 ..< `storageVar`.nextEntityId.int:
        let ent = (i.EntityId).makeRef
        ent.delete
      `clearRecycler`
      `storageVar`.nextEntityId = FIRST_ENTITY_ID
      `storageVar`.entityCounter = 0

  genLog "# Delete entity:\n", result.repr

