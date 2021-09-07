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

import macros, strutils, strformat, sequtils, times, sharedtypes,
  private/[ecsstatedb, utils, debugging], tables
import random

template newEntityTemplate*: ComponentList = @[]
proc initComponents*: ComponentList = @[]

proc indexInit(sysNode: NimNode, options: ECSSysOptions): NimNode =
  let
    initTab = bindSym "initTable"

  case options.indexFormat
  of sifTable:
    quote do:
      `sysNode`.index = `initTab`[EntityId, int]() 
  of sifArray:
    newEmptyNode()
  of sifAllocatedSeq:
    # Requires + 1 for array as 0 is invalid.
    let initSize = newIntLitNode(options.maxEntities + 1)
    quote do:
      `sysNode`.index = newSeq[tuple[exists: bool, row: Natural]](`initSize`)

proc makeSystemType(id: EcsIdentity, sysIndex: SystemIndex, componentTypes: NimNode, extraFields: seq[NimNode]): NimNode =
  ## Generates the type declaration for this system.
  let
    name = id.getSystemName sysIndex
    sysTypeName = systemTypeName(name)
    sysIdent = ident sysTypeName
    tupleTypeIdent = ident(tupleName(name))
    options = id.getOptions(sysIndex)
  result = newStmtList()

  # Generate the type for this system
  result.add(quote do:
    type
      `sysIdent`* = object
        id*: SystemIndex
        lastIndex*: int           ## Records the last item position processed for streaming.
        streamRate*: Natural      ## Rate at which this system streams items by default, overridden if defined using `stream x:`.
        # TODO: Currently writable, use sys.name to get a generated constant by system type.
        systemName*: string       ## Name is automatically set up at code construction in defineSystem
        disabled*: bool           ## Doesn't run doProc if true, no work is done.
        paused*: bool             ## Pauses this system's entity processing, but still runs init & finish. 
        initialised*: bool        ## Automatically set to true after an `init` body is called.
        deleteList*: seq[EntityRef] ## Anything added to this list is deleted after the `finish` block. This avoids affecting the main loop when iterating.
  )

  var
    # Extract field list so we can edit them.
    fields = result.recList(sysTypeName)
    highIdx, count: NimNode
  doAssert fields.kind != nnkEmpty, "Internal error: Cannot retrieve fields from system type `" & name & "`"

  # Create requirements array
  let reqCount = id.len_ecsSysRequirements(sysIndex)
  fields.add genField("requirements", false, genArray(reqCount, ident "ComponentTypeId"))

  # Append groups field to the system type, depending on options.
  case options.storageFormat
  of ssSeq:
    fields.add genField("groups", true, genSeq(tupleTypeIdent))
    highIdx = quote do:
      template high*(system: `sysIdent`): int = system.groups.high
    count = quote do:
      template count*(system: `sysIdent`): int = system.groups.len
  of ssArray:
    let maxEntities = options.maxEntities
    fields.add genField("groups", true, genArray(maxEntities + 1, tupleTypeIdent))
    # TODO: Make nextFreeIdx private. This is exposed due to `newEntityWith` not building a proc, so needs internal access.
    fields.add genField("nextFreeIdx", true, ident "int")
    highIdx = quote do:
      template high*(system: `sysIdent`): int = system.nextFreeIdx - 1
    count = quote do:
      template count*(system: `sysIdent`): int = system.nextFreeIdx

  # Define the `index` field, used to translate from entityId to the groups row. 
  case options.indexFormat
  of sifTable:
    fields.add genField("index", true, genTable(ident "EntityId", ident "int"))
  of sifArray:
    let existsTuple = quote: tuple[exists: bool, row: Natural]
    # Requires + 1 for array as 0 is invalid.
    fields.add genField("index", true, genArray(options.maxEntities + 1, existsTuple))
  of sifAllocatedSeq:
    let existsTuple = quote: tuple[exists: bool, row: Natural]
    fields.add genField("index", true, genSeq(existsTuple))

  # Add fields to support timing the system.
  if options.timings == stProfiling:
    # Time to run one tick's worth of items based on past performance
    fields.add genField("timePerGroupRun", true, ident "float")
    # Calculate time taken to perform one row item based on past performance
    fields.add genField("timePerGroupItem", true, ident "float")
    fields.add genField("minTimePerGroupItem", true, ident "float")
    fields.add genField("maxTimePerGroupItem", true, ident "float")
    fields.add genField("minTimePerGroupRun", true, ident "float")
    fields.add genField("maxTimePerGroupRun", true, ident "float")
  if options.timings in [stRunEvery, stProfiling]:
    # Keeps track of the last time a tick was issued
    fields.add genField("lastTick", true, ident "float")
    # In theory, inputting a negative curTime() + x in runEvery would allow no trigger until x
    # had passed, then act as if runEvery is zero (ie; always run)
    ## If runEvery is non-zero, this system's do proc will only trigger after this delay
    fields.add genField("runEvery", true, ident "float")

  # Finally, add extra fields (provided by the user) to the type.
  if extraFields.len > 0:
    fields.add(extraFields)

  result.add(quote do:
    `highIdx`
    `count`
  )

proc instantiateSystem(id: EcsIdentity, sysIndex: SystemIndex, sysName: string, fieldSetup: seq[tuple[fieldName, value: NimNode]]): NimNode =
  ## Generates an init proc that instantiates the system and initialises variables and instantiates the system variable.
  let
    typeName = ($sysName).capitalizeAscii
    sysType = ident systemTypeName(typeName)
    initParam = ident "value"
    initIdent = ident systemInitName(sysName)
    options = id.getOptions(sysIndex)
    initIndex = initParam.indexInit(options)
    sysTypeName = systemTypeName(sysName)
    sysIdent = ident sysTypeName

  result = newStmtList()
  var fieldInits = newStmtList()

  for setupState in fieldSetup:
    # `value.myVar = myValue`.
    fieldInits.add newAssignment(newDotExpr(initParam, setupState.fieldName), setupState.value)

  # Build this system's requirements array for assignment in init.
  var reqsConst = nnkBracket.newTree()
  for i, req in id.ecsSysRequirements(sysIndex):
    reqsConst.add newDotExpr(newLit req.int, ident "ComponentTypeId")

  # Create variable for this system.
  let
    sysVar = ident systemVarName(typeName)
    typeIdent = ident tupleName(typeName)
    systemVarDecl =
      if options.useThreadVar:
        quote:
          var `sysVar`* {.threadVar.}: `sysIdent`
      else:
        quote:
          var `sysVar`*: `sysIdent`

  let docCmt = newCommentStmtNode "Returns the tuple type for the " & sysName & " system."
  result.add(quote do:
    `systemVarDecl`
    `docCmt`
    template tupleType*(system: `sysType`): untyped = `typeIdent`
  )

  # Add timing utilities for this system.
  let timingsProcs =
    if options.timings == stProfiling:
      quote do:
        proc timePerItem*(sys: `sysIdent`): float {.inline.} = sys.timePerGroupItem
        proc minTimePerItem*(sys: `sysIdent`): float {.inline.} = sys.minTimePerGroupItem
        proc maxTimePerItem*(sys: `sysIdent`): float {.inline.} = sys.maxTimePerGroupItem
        proc minTimePerRun*(sys: `sysIdent`): float {.inline.} = sys.minTimePerGroupRun
        proc maxTimePerRun*(sys: `sysIdent`): float {.inline.} = sys.maxTimePerGroupRun
        proc resetMinMax*(sys: var `sysIdent`) {.inline.} =
          sys.minTimePerGroupItem = 0.0
          sys.maxTimePerGroupItem = 0.0
          sys.minTimePerGroupRun = 0.0
          sys.maxTimePerGroupRun = 0.0
        proc timePerRun*(sys: `sysIdent`): float {.inline.} = sys.timePerGroupRun
    else: newEmptyNode()

  let
    entity = ident "entity"
    sysHasEntity = sysVar.indexHasKey(quote do: `entity`.entityId, options.indexFormat)
  result.add(quote do:
    proc `initIdent`*(`initParam`: var `sysIdent`) =
      ## Initialise the system.

      ## The sys template represents the system variable being passed.
      template sys: untyped {.used.} = `initParam`
      ## The self template represents the system variable being passed.
      template self: untyped {.used.} = `initParam`

      `initIndex`
      `initParam`.streamRate = 1  # Default items to process per frame when using `stream:`.
      `initParam`.requirements = `reqsConst`
      `initParam`.systemName = `sysName`
      `fieldInits`
      sys.id = `sysIndex`.SystemIndex

    `timingsProcs`

    func name*(sys: `sysIdent`): string = `sysName`
    
    # Call init proc to set up the system's variable.
    # Initialising in-place guarantees no copies.
    `sysVar`.`initIdent`()

    proc contains*(sys: `sysIdent`, `entity`: EntityRef): bool =
      `sysHasEntity`
    
  )

proc processPragma(identNode, valueType: NimNode): tuple[ident: NimNode, def: NimNode] =
  ## Process and remove `public` pragma if present.
  identNode[1].expectKind nnkPragma
  for pIdx in countDown(identNode[1].len - 1, 0):
    if identNode[1][pIdx].kind == nnkIdent:
      let pragmaStr = identNode[1][pIdx].strVal

      if pragmaStr == "public" or pragmaStr == "pub":
        identNode[1].del(pIdx)
        # Mark as exported.
        identNode[0] = postFix(identNode[0], "*")
        break

  var a, b: NimNode

  if identNode[0].len > 1:
    a = identNode[0][1]
  else:
    a = identNode[0]

  if identNode[1].len == 0:
    # Public was the only pragma so remove the pragma tree entirely.
    b = newIdentDefs(identNode[0], valueType)
  else:
    b = newIdentDefs(identNode, valueType)
  (a, b)

proc parsePublicPragma(identNode, valueType: NimNode): (NimNode, NimNode) =
  ## Parse the ident node for an ident def and if it finds a {.public.}
  ## annotation, postfix the ident with `*` so the field is exported.
  ## This is necessary because `ident*` invalid syntax in an
  ## untyped block.
  case identNode.kind
  of nnkPragmaExpr:
    # Strip out public pragma (but keep others).
    identNode.processPragma(valueType)
  of nnkInfix:
    if identNode.len >= 1:
      if identNode[1].kind == nnkPragmaExpr:
        identNode[1].processPragma(valueType)
      else:
        (identNode[1], newIdentDefs(identNode[1], valueType))
    else:
      (identNode, newIdentDefs(identNode, valueType))
  else:
    (identNode, newIdentDefs(identNode, valueType))

proc matchesTypeProvided(node: NimNode): bool =
  ## Tests to see if we're doing `field -> type`.
  node.kind == nnkInfix and node[0].kind == nnkIdent and node[0].strVal == "->"


proc unpackComponentArg(id: EcsIdentity, node: NimNode): tuple[
    typeName: string,
    typeId: ComponentTypeId,
    negate: bool] =

  # Parse <typedesc> and <not typedesc>

  const
    sysPre = "System components "

  case node.kind
    
    of nnkIdent, nnkSym:
      (
        typeName: node.strVal,
        typeId: id.typeStringToId node.strVal,
        negate: false
      )
    
    of nnkPrefix:
      if node[1].kind != nnkIdent:
        error sysPre & "expected a type ident for negation but got " & $node.kind & ": '" & node.repr & "'"
      
      if node[0].strVal != "not":
        error sysPre & "cannot process a type modifier of '" & node[0].repr & "'"
      
      (
        typeName: node[1].strVal,
        typeId: id.typeStringToId node[1].strVal,
        negate: true
      )

    else:
      error sysPre & "expected 'type' or 'not type' but got " &
        $node.kind & ": '" & node.repr & "'"
      (
        typeName: "",
        typeId: InvalidComponent,
        negate: false
      )


proc createSysTuple(id: EcsIdentity, sysName: string, componentTypes, ownedComponents: NimNode, extraFields: NimNode, sysOptions: ECSSysOptions): NimNode =
  ## Create a tuple to hold the required combination of types for this system.
  result = newStmtList()

  if id.findSystemIndex(sysName).found:
    error "System \"" & sysName & "\" has already been defined"
  
  if componentTypes.len == 0:
    error "Systems require at least one component type to operate on but none have been provided"
  
  if sysOptions.indexFormat in [sifArray, sifAllocatedSeq] and sysOptions.maxEntities == 0:
    error "System \"" & sysName & "\", system options: maxEntities cannot be zero when using the fixed size option '" &
      $sysOptions.indexFormat & "'"

  if sysOptions.storageFormat in [ssArray] and sysOptions.maxEntities == 0:
    error "System \"" & sysName & "\", system options: maxEntities cannot be zero when using the fixed size option '" &
      $sysOptions.storageFormat & "'"

  var
    passedComponentIds: seq[ComponentTypeId]
    passedNegationIds: seq[ComponentTypeId]

  for item in componentTypes:
    let
      uArg = id.unpackComponentArg(item)
      typeId = uArg.typeId
      typeName = id.typeName(typeId)

    if typeId in passedComponentIds:
      error "Component '" & typeName & "' is included multiple times for system \"" & sysName & "\""

    if uArg.negate:
      passedNegationIds.add typeId
    else:
      # Check the component isn't already part of a sealed ECS.

      if typeId in id.ecsSealedComponents:
        error "Component " & typeName &
          " has already been sealed with makeEcs and cannot be extended to system \"" &
          sysName &
          "\". Use 'defineSystem' to forward declare the system or place this 'makeSystem' before 'makeEcs'."

      passedComponentIds.add typeId

  if passedComponentIds.len == 0:
    error "Systems must use at least one component"

  # Process owned components.

  var
    ownedComponentIds: seq[ComponentTypeId]

  for comp in ownedComponents:
    let
      typeId = id.typeStringToId($comp)

    if typeId.int == -1:
      error "This type's id can't be found in registered components: " & $comp

    if typeId notin passedComponentIds:
      error "Requesting to own component " & id.typeName(typeId) &
        " which is not part of system \"" & sysName &
        "\". System components: " & id.commaSeparate(passedComponentIds)

    ownedComponentIds.add typeId

  for typeId in passedComponentIds:
    let
      curOwner = id.systemOwner typeId

    if curOwner != InvalidSystemIndex and typeId in ownedComponentIds:
      error "Component " & id.typeName(typeId) & " is already owned by system \"" & id.getSystemName(curOwner) & "\"" 

  # Conditions have now been met to add the system.

  when defined(ecsLog) or defined(ecsLogDetails):
    echo "[ System generation for \"" & sysName & "\"]"
  when defined(ecsLogDetails):
    echo "System \"", sysName, "\" options:\n", sysOptions.repr, "\n"

  let typeName = ($sysName).capitalizeAscii
  # Create the node for the variable for this system.
  let inst = ident(systemVarName(typeName))

  # Create a new system index.
  let
    sysIndex = id.addSystem sysName

  id.setOptions(sysIndex, sysOptions)

  # Store variable ident for this system.
  id.set_Instantiation(sysIndex, inst)
  
  # Append to the commitSystems order.
  id.add_systemOrder sysIndex

  assert id.len_ecsOwnedComponents(sysIndex) == 0, "Owned components are already initialised"

  for sys in ownedComponentIds:
    id.add_ecsOwnedComponents sysIndex, sys

  for compId in passedNegationIds:
    id.add_ecsSysNegations sysIndex, compId

  # Build tuple type of components for this system.
  var
    elements = nnkTupleTy.newTree()
  
  # Add entity field.
  elements.add(nnkIdentDefs.newTree(ident("entity"), ident("EntityRef"), newEmptyNode()))
  
  # Add component fields to the tuple.
  for typeId in passedComponentIds:
    let
      tyName = id.typeName typeId

    if typeId in ownedComponentIds:
      # Owned component storage is the system itself.

      id.set_systemOwner typeId, sysIndex
      id.set_isOwned typeId, true
      
      elements.add(nnkIdentDefs.newTree(
        ident((tyName).toLowerAscii), ident(tyName), newEmptyNode())
      )

    else:
      # If not owned the component instance type is used.
      
      elements.add(nnkIdentDefs.newTree(
        ident(tyName.toLowerAscii), ident(tyName.instanceTypeName()), newEmptyNode())
      )

    # Update component to system links.
    id.add_systems typeId, sysIndex
    id.add_ecsSysRequirements sysIndex, typeId
  
  # Build system tuple type and access variable.
  let typeIdent = ident(tupleName(typeName))

  # This type defines the elements of the `groups` field.
  result.add(quote do:
    type `typeIdent`* = `elements`)

  # Add user defined fields to this System's type.
  # These can be defined as `Field: Value`, `Field = Value`, or `Field -> Type = Value`.
  var
    fieldSetup: seq[tuple[fieldName, value: NimNode]]
    extraFieldDefs: seq[NimNode]

  if extraFields.kind != nnkEmpty:
    
    # Store extra fields for this system.
    # This allows checking later `fields:` components match the original
    # definition.
    id.set_extraFields(sysIndex, extraFields)

    for tyDef in extraFields:
      case tyDef.kind
      of nnkCall:
        # Already in the right format.
        tyDef[1].expectKind nnkStmtList
        tyDef[1].expectMinLen 1
        let
          valueType = tyDef[1][0]
          (_, identDef) = parsePublicPragma(tyDef[0], valueType)

        extraFieldDefs.add(identDef)
  
      of nnkAsgn:
        # Because it's invalid syntax to use `field: type = statement`
        # you can manually set the type using `field -> type = statement`
        # Otherwise, use `field: type` and set it up with `init:` in the system.
        var valueType: NimNode
        let value = tyDef[1]
        if tyDef[0].matchesTypeProvided:
          # Pull out user's type request.
          valueType = tyDef[0][2]
        else:
          # Try and work out what type we have.
          valueType = value.assignmentType
        let (ident, identDef) = parsePublicPragma(tyDef[0], valueType)

        # Add assignment to the init procedure.
        fieldSetup.add (ident, value)
        # Record the field def too.
        extraFieldDefs.add(identDef)
      else:
        error "Unhandled kind for extra field in system \"" & sysName &
          "\", expected `field: type` or `field = value`, got:\n" & tyDef.treeRepr

  # Generate the system type according to provided options.
  result.add makeSystemType(id, sysIndex, componentTypes, extraFieldDefs)
  result.add instantiateSystem(id, sysIndex, sysName, fieldSetup)

  # Static access to the state of ownership in the system.
  let sysType = ident systemTypeName(sysName)
  if ownedComponentIds.len > 0:
    result.add(quote do:
      template isOwner*(sys: `sysType`): bool = true
      template ownedComponents*(sys: `sysType`): seq[ComponentTypeId] = `ownedComponents`
      )
  else:
    result.add(quote do:
      template isOwner*(sys: `sysType`): bool = false
      template ownedComponents*(sys: `sysType`): seq[ComponentTypeId] = []
      )

  id.add_ecsSysDefined sysIndex

  if id.private:
    result.deExport

  genLog "\n# System \"" & sysName & "\":\n" & result.repr

macro defineSystemOwner*(id: static[EcsIdentity], name: static[string], componentTypes: untyped, ownedComponents: openarray[typedesc], options: static[ECSSysOptions], extraFields: untyped): untyped =
  ## Define a system using an ECS identity, declaring types that are owned by this system and providing extra fields.
  result = id.createSysTuple(name, componentTypes, ownedComponents, extraFields, options)

macro defineSystemOwner*(id: static[EcsIdentity], name: static[string], componentTypes: untyped, ownedComponents: openarray[typedesc], options: static[ECSSysOptions]): untyped =
  ## Define a system using an ECS identity, declaring types that are owned by this system.
  result = id.createSysTuple(name, componentTypes, ownedComponents, nil, options)

macro defineSystem*(id: static[EcsIdentity], name: static[string], componentTypes: untyped, options: static[ECSSysOptions], extraFields: untyped): untyped =
  ## Define a system and its types using an ECS identity, providing extra fields to incorporate into the resultant system instance.
  result = id.createSysTuple(name, componentTypes, nil, extraFields, options)

template defineSystem*(id: static[EcsIdentity], name: static[string], componentTypes: untyped, options: static[ECSSysOptions]): untyped =
  ## Define a system and its types using an ECS identity with specific options.
  defineSystem(id, name, componentTypes, options, nil)

template defineSystem*(id: static[EcsIdentity], name: static[string], componentTypes: untyped): untyped =
  ## Define a system and its types using an ECS identity with default system options.
  defineSystem(id, name, componentTypes, defaultSystemOptions, nil)

#---------------------------------------------------
# Convenience templates for using a default identity
#---------------------------------------------------

template defineSystem*(name: static[string], componentTypes: untyped, options: static[ECSSysOptions], extraFields: untyped): untyped =
  ## Define a system and its types using the default ECS identity, providing extra fields to incorporate into the resultant system instance.
  defaultIdentity.defineSystem(name, componentTypes, options, extraFields)

template defineSystem*(name: static[string], componentTypes: untyped, options: static[ECSSysOptions]): untyped =
  ## Define a system and its types using the default ECS identity with specific options.
  defaultIdentity.defineSystem(name, componentTypes, options)

template defineSystem*(name: static[string], componentTypes: untyped): untyped =
  ## Define a system and its types using the default ECS identity with default system options.
  defaultIdentity.defineSystem(name, componentTypes)

template defineSystemOwner*(name: static[string], componentTypes: untyped, ownedComponents: untyped, options: static[ECSSysOptions], extraFields: untyped): untyped =
  ## Define a system using the default ECS identity, declaring types that are owned by this system and providing extra fields.
  defaultIdentity.defineSystemOwner(name, componentTypes, ownedComponents, options, extraFields)

template defineSystemOwner*(name: static[string], componentTypes: untyped, ownedComponents: untyped, options: static[ECSSysOptions]): untyped =
  ## Define a system using the default ECS identity, declaring types that are owned by this system and providing extra fields.
  defaultIdentity.defineSystemOwner(name, componentTypes, ownedComponents, options)

# ----------------
# Grouping systems
# ----------------

macro defineGroup*(id: static[EcsIdentity], group: static[string], systems: openarray[string]): untyped =
  ## Assign specific systems to a group using an ECS identity.
  ## 
  ## These systems will not be output as part of `commitSystems` and must be output with `commitGroup`.
  ## 
  ## Systems may be part of multiple groups.
  let
    lcGroup = group.toLowerAscii

  for systemName in systems:
    let
      nameStr = systemName.strVal
      lookupSys = findSystemIndex(id, nameStr)

    if lookupSys.found:
      let
        sys = lookupSys.index

      # Add to list of systems for this group.
      if sys notin id.groupSystems(lcGroup):
        id.add_groupSystems(lcGroup, sys)

      # Add the group to this system.
      if lcGroup notin id.systemGroups(sys):
        id.add_systemGroups(sys, lcGroup)

    else:
      error "Cannot find system \"" & nameStr & "\""

template defineGroup*(group: static[string], systems: openarray[string]): untyped =
  ## Assign specific systems to a group using the default ECS identity.
  ## 
  ## These systems will not be output as part of `commitSystems` and must be output with `commitGroup`.
  ## 
  ## Systems may be part of multiple groups.
  defaultIdentity.defineGroup(group, systems)

macro defineGroup*(id: static[EcsIdentity], group: static[string]): untyped =
  ## Assign previously defined and ungrouped systems to a group using an ECS identity.
  ## 
  ## These systems will not be output as part of `commitSystems` and must be output with `commitGroup`.
  ## 
  ## Systems may be part of multiple groups.
  var
    order = id.systemOrder()
  let
    lcGroup = group.toLowerAscii

  for sys in order:
    # Ignore systems that already belong to groups.
    if id.systemGroups(sys).len == 0:

      # Add to list of systems for this group.
      if sys notin id.groupSystems(lcGroup):
        id.add_groupSystems(lcGroup, sys)

      # Add the group to this system.
      if lcGroup notin id.systemGroups(sys):
        id.add_systemGroups(sys, lcGroup)

template defineGroup*(group: static[string]): untyped =
  ## Assign previously defined and ungrouped systems to a group using the default ECS identity.
  ## 
  ## These systems will not be output as part of `commitSystems` and must be output with `commitGroup`.
  ## 
  ## Systems may be part of multiple groups.
  defaultIdentity.defineGroup(group)

macro commitGroup*(id: static[EcsIdentity], group, runProc: static[string]): untyped =
  ## Output uncommitted system definitions for a group using an ECS identity and wrap in an execution procedure.
  ## 
  ## The order of execution matches the order these systems have been defined.
  ## 
  ## It is a compile time error to commit groups with no systems or that contain systems with no body.
  result = newStmtList()

  let
    systems = id.groupSystems(group.toLowerAscii)

  if systems.len == 0:
    error "No system bodies defined for group \"" & group & "\""
  else:
    result.add id.commitSystemList(systems, runProc)
  
  if id.private:
    result.deExport

template commitGroup*(group, runProc: static[string]): untyped =
  ## Output uncommitted system definitions for a group using the default ECS identity and wrap in an execution procedure.
  ## 
  ## The order of execution matches the order these systems have been defined.
  ## 
  ## It is a compile time error to commit groups with no systems or that contain systems with no body.
  defaultIdentity.commitGroup(group, runProc)


#-----------------
# System utilities
#-----------------

template updateTimings*(sys: untyped): untyped =
  # per item
  # max
  if sys.timePerGroupItem > sys.maxTimePerGroupItem:
    sys.maxTimePerGroupItem = sys.timePerGroupItem
  # min
  if sys.minTimePerGroupItem <= 0.0: 
    sys.minTimePerGroupItem = sys.timePerGroupItem
  else:
    if sys.timePerGroupItem < sys.minTimePerGroupItem:
      sys.minTimePerGroupItem = sys.timePerGroupItem
  # per run
  # max
  if sys.timePerGroupRun > sys.maxTimePerGroupRun:
    sys.maxTimePerGroupRun = sys.timePerGroupRun
  # min
  if sys.minTimePerGroupRun <= 0.0: 
    sys.minTimePerGroupRun = sys.timePerGroupRun
  else:
    if sys.timePerGroupRun < sys.minTimePerGroupRun:
      sys.minTimePerGroupRun = sys.timePerGroupRun

proc getAssertItem(assertItem: bool, sys, itemIdx: NimNode, rowIdx: NimNode = nil): NimNode =
  if assertItem:
    if rowIdx != nil:
      quote do:
        if `itemIdx` != `rowIdx`:
          assert false,
            "'item' in " & `sys`.name & " is being used after a " &
            "remove or delete affected this system"
        elif `itemIdx` > `sys`.high:
          assert false,
            "'item' in " & `sys`.name & " is out of bounds. " &
            "Use of 'item' after remove/delete affected this system?"
    else:
      quote do:
        if `itemIdx` > `sys`.high:
          assert false,
            "'item' in " & `sys`.name & " is out of bounds. " &
            "Use of 'item' after remove/delete affected this system?"
  else:
    newStmtList()

macro removeComponents*(sys: object, types: varargs[typed]) =
  ## Remove the components in `types` from all entities in this system.
  
  # This macro builds a `removeComponents` with `types` and executes it
  # for each entity in the system, from the last entity backwards to the
  # first.
  let entity = ident "entity"
  var `removeInner` = nnkCall.newTree(ident "remove", entity)
  
  for ty in types:
    case ty.kind
    of nnkSym:
      `removeInner`.add ty
    of nnkHiddenStdConv:
      ty.expectMinLen 2
      ty[1].expectKind nnkBracket
      for compTy in ty[1]:
        `removeInner`.add compTy
    else:
      `removeInner`.add ty

  quote do:
    for i in countDown(`sys`.count - 1, 0):
      let `entity` = `sys`.groups[i].entity
      `removeInner`

template remove*(sys: object, types: varargs[typed]) =
  ## Remove the components in `types` from all entities in this system.
  removeComponents(sys, types)

template clear*(sys: object) =
  ## Remove all entities in this system.
  if `sys`.count > 0:
    for i in countDown(`sys`.count - 1, 0):
      `sys`.groups[i].entity.delete

type SysEventOp = enum seAdded = "onAdded", seRemoved = "onRemoved"

proc checkEventCycle(id: EcsIdentity, name: string, sysIndex: SystemIndex, op: SysEventOp): NimNode =
  # Inline system events can add/remove components, which may
  # trigger one or more other inline system events.
  # 
  # Since these events are inlined at compile-time during a state
  # change, cycles can form when they're expanded.
  # 
  # This applies even though you may be performing state changes
  # on *separate* entities that will never create an event cycle
  # at run time, because within the compile time context each
  # state changes must include code for all *potential* changes,
  # and this can combinatoric explode with multi-component changes
  # more than a few levels deep.
  # 
  # Of particular note, `construct` must statically expand all
  # possible inline events for every component in the identity.
  # 
  # System events such as `added` and `removed` are therefore
  # checked to halt compilation when nested state changes try to
  # invoke the same event for the same system again.
  # 
  # To avoid the possibility of compile time cycles within system
  # events, use `addedCallback` and `removedCallback`.
  # 
  # Future work: it may be possible to isolate state change
  # branches by inserting ident tracking code using the
  # macrocache to make this check more lenient.
  
  let
    # Build access idents for onAdded and onRemoved.
    opStr = $op
    chain = opStr & "Chain"
    source = opStr & "Source"
    start = opStr & "Start"

    getSource = ident source
    setSource = ident "set_" & source

    getStartIdx = ident start
    setStartIdx = ident "set_" & start

    getChain = ident chain
    addChain = ident "add_" & chain
    lenChain = ident "len_" & chain

  # Insert static code for the next expansion level to catch closed
  # event loops.
  quote do:
    static:
      const
        identity = EcsIdentity(`id`)
        # When `getSource` is empty it returns default(SystemIndex),
        # which is expected to == InvalidSystemIndex.
        #
        # Once the event has been fully expanded by the compiler
        # the source is set back to InvalidSystemIndex for the next
        # system event.
        chainSource = identity.`getSource`
        curSys = `sysIndex`.SystemIndex
      
      startOperation(identity, `opStr` & " \"" & identity.getSystemName(curSys) & "\"")

      if chainSource == InvalidSystemIndex:
        # The first system to invoke the event records the path.
        # Initialise the chain from this system.
        identity.`setSource` curSys
        identity.`setStartIdx`(curSys, identity.`lenChain`(curSys))
        identity.`addChain` curSys, curSys

      else:
        # This event has been generated by another inline event.
        # If this system has been seen before then compilation is
        # halted, otherwise appends this system to the chain.
        const curChain = identity.`getChain`(chainSource)

        assert curChain.len > 0, "Internal error: chain desync - current chain is empty, expected start system \"" &
          identity.getSystemName(chainSource) & "\""
        
        when curChain.len > 0:

          const chainIdx = identity.`getStartIdx` curSys
          assert chainIdx < curChain.len
          
          const
            # Trim current chain before the last known starting point.
            activeChain = curChain[chainIdx .. max(chainIdx, curChain.high)]
          
          when defined(ecsLogDetails):
            const potentialChain = activeChain & curSys
            debugPerformance(identity, `opStr` & " event chain: " & identity.commaSeparate(potentialChain))

          when curSys in activeChain:
            # This system is already in the chain and part of a cycle.
            # TODO: We could also do this perhaps with more flexibility using a recursion count.

            proc outputMsg(sysSource: SystemIndex, postFix = ""): string =
              let
                sysReqs = identity.ecsSysRequirements(sysSource)
                compListStr = identity.commaSeparate(sysReqs)
              "  \"" & identity.getSystemName(sysSource) & "\" [" & compListStr & "]" & postFix & "\n"

            proc chainMsg: string = 
              const cycleIndicator = " <- Start of cycle"

              when activeChain.len > 1:
                for i in 0 ..< activeChain.len - 1:
                  result.add activeChain[i].outputMsg()
              result.add activeChain[^1].outputMsg(cycleIndicator)
              result.add curSys.outputMsg(" ...\n")
              result.add "Use a callback event to avoid cycles." 

            error "System \"" & `name` &
              "\" inline event '" & `opStr` & "'" &
              " causes circular code generation:\n" & chainMsg()

          identity.`addChain` chainSource, curSys

proc strictCatchCheck(cacheId: NimNode): NimNode =
  let
    undefinedItemMsg = "Cannot access 'item' here as the current row may be " &
      "undefined until the next iteration due to a previous removal of " &
      "components or deletion of an entity within this system iteration. " &
      "Use the injected 'entity' variable or the system's 'deleteList' " &
      "to avoid this error whilst using 'ecsStrict'"

  if defined(ecsStrict):
    quote do:
      static:
        when `cacheId`.sysRemoveAffectedThisSystem or `cacheId`.systemCalledDelete:
          error `undefinedItemMsg`
  else: newStmtList()

proc timeWrapper(sys: NimNode, options: EcsSysOptions, core: NimNode): NimNode =
  ## Wraps `core` with timing code if `timings` is true, otherwise passes through `core` unchanged.
  case options.timings
  of stNone:
    `core`
  of stRunEvery:
    quote do:
      let startTime = cpuTime()
      `core`
      # record last tick time
      `sys`.lastTick = startTime
  of stProfiling:
    quote do:
      let startTime = cpuTime()
      `core`
      let endTime = cpuTime()
      `sys`.timePerGroupRun = endTime - startTime
      if `sys`.count > 0:
        `sys`.timePerGroupItem = `sys`.timePerGroupRun / `sys`.count.float
      else:
        `sys`.timePerGroupItem = 0.0
      # Note that the min/max timers are user handled with `system.resetMinMax`.
      `sys`.updateTimings()
      # record last tick time
      `sys`.lastTick = startTime

proc wrapAllBlock(id: EcsIdentity, name: string, sysIndex: SystemIndex, options: EcsSysOptions, code: NimNode): NimNode =
  let
    # `sys` is the system variable parameter passed to the doProc.
    sys = ident "sys"
    cacheId = quote do: EcsIdentity(`id`)
    groupIndex = ident "groupIndex"
    idx = genSym(nskVar, "i")
    sysLen = ident "sysLen"

    # if `entity` != `item.entity` then this row has been removed.
    rowEnt = ident "entity"

    typeName = name.capitalizeAscii
    typeIdent = ident(tupleName(typeName))

    strictCatch = strictCatchCheck(cacheId)
    assertItem = id.assertItem(sysIndex)
    assertCheck = getAssertItem(assertItem, sys, groupIndex)

  result = timeWrapper(sys, options, quote do:
    block:
      static:
        `cacheId`.set_inSystemAll true

      var
        `sysLen` = `sys`.count()

      if `sysLen` > 0:
        var
          `idx`: int

        while `idx` < `sysLen`:
          ## The entity this row started execution with.
          let
            `rowEnt` {.used, inject.} = `sys`.groups[`idx`].entity
            ## Read only index into `groups`.
            `groupIndex` {.used, inject.} = `idx`

          ## Current system item being processed.
          template item: `typeIdent` {.used.} =
            `strictCatch`
            `assertCheck`
            `sys`.groups[`idx`]

          # Inject the user's statements from `all:`
          `code`

          when `cacheId`.systemCalledDelete or
            `cacheId`.sysRemoveAffectedThisSystem:
              `sysLen` = `sys`.count()
              if `sysLen` > 0 and (`idx` < `sysLen` and `sys`.groups[`idx`].entity == `rowEnt`):
                # This row wasn't deleted so move forward.
                `idx` = `idx` + 1
          else:
            `idx` = `idx` + 1
      static:
        `cacheId`.set_inSystemAll false
  )

type
  Command = enum
    cmdNone,
    cmdMultiPass = "multipass",
    cmdStochastic = "stochastic"

proc getCommand(node: NimNode): Command =
  if node.kind == nnkIdent:
    case node.strVal.toLowerAscii
    of $cmdMultiPass:
      result = cmdMultiPass
    of $cmdStochastic:
      result = cmdStochastic

proc parseStreamCommands(code: NimNode): tuple[amount: NimNode, command: Command, body: NimNode] =
  # Stream commands:
  #   stream multipass <optional count>: <code block>
  #     - forces a minimum of `count` rows to be processed even if repeated.
  #     - omitting count uses system.streamRate.
  #   stream stochastic <optional count>: <code block>
  #     - selects rows to process at random.
  #     - omitting count uses system.streamRate.


  let formatError =
      "Expected one of:\n" &
      "  'stream:'\n" &
      "  'stream <command|parameter>:'\n" &
      "  'stream <parameter|parameter> <command|parameter>:'\n" &
      "Passed:\n" & code.repr & "\n"

  if code.len notin [2, 3]:
    error formatError

  case code.kind
    of nnkCommand:
      result.body = code[2]

      case code[1].kind
        of nnkIntLit:
          # `stream N:`
          result.amount = code[1]
        
        of nnkIdent:
          # `stream x:`
          result.command = code[1].getCommand

          if result.command == cmdNone:
            # Pass through user's identifier.
            result.amount = code[1]
        
        of nnkCommand:
          # `stream x y:`

          result.command = code[1][0].getCommand

          if result.command != cmdNone:
            # `stream command x:`
            result.amount = code[1][1]

          else:
            # Check for `stream x command:`
            result.command = code[1][1].getCommand

            if result.command == cmdNone:
              # No commands found.
              error formatError
            else:
              result.amount = code[1][0]

        else:
          error formatError

    of nnkCall:
      # `stream:`
      if code[1].kind != nnkStmtList:
        error formatError
      else:
        result.body = code[1]
    else:
      error formatError

proc wrapStreamBlock(id: EcsIdentity, name: string, sysIndex: SystemIndex, options: EcsSysOptions, code: NimNode): NimNode =
  let
    # `sys` is the system variable parameter passed to the doProc.
    sys = ident "sys"
    cacheId = quote do: EcsIdentity(`id`)
    groupIndex = ident "groupIndex"
    sysLen = ident "sysLen"

    # if `entity` != `item.entity` then this row has been removed.
    rowEnt = ident "entity"

    typeName = name.capitalizeAscii
    typeIdent = ident(tupleName(typeName))

    strictCatch = strictCatchCheck(cacheId)
    assertItem = id.assertItem(sysIndex)
    streamAssertCheck = getAssertItem(assertItem, sys, groupIndex)
    streamDetails = parseStreamCommands(code)
    streamBody = streamDetails.body

    streamAmount =
      if streamDetails.amount == nil:
        # Use default stream rate if not provided.
        quote do:
          `sys`.streamRate
      else:
        streamDetails.amount

  if streamDetails.body.len == 0:
    error "Stream cannot be empty"

  let
    processed = ident "sysProcessed"
    finished = genSym(nskVar, "finished")
    randomValue = bindSym "rand"

    processNext =
      case streamDetails.command
      of cmdNone:
        quote do:
          `sys`.lastIndex = `sys`.lastIndex + 1
          `finished` = (`processed` >= `streamAmount`) or (`sys`.lastIndex >= `sysLen`)
      of cmdMultiPass:
        quote do:
          `sys`.lastIndex = (`sys`.lastIndex + 1) mod `sysLen`
          `finished` = `processed` >= `streamAmount`
      of cmdStochastic:
        quote do:
          `sys`.lastIndex = `randomValue`(`sys`.high)
          `finished` = `processed` >= `streamAmount`

    initFirstRun =
      case streamDetails.command
      of cmdNone, cmdMultiPass:
        quote do:
          `sys`.lastIndex = `sys`.lastIndex mod `sysLen`
      of cmdStochastic:
        quote do:
          if not `finished`:
            `sys`.lastIndex = `randomValue`(`sys`.high)

  result = timeWrapper(sys, options, quote do:
    static:
      `cacheId`.set_inSystemStream  true
    block:
      # loop per entity in system

      # Note that streaming bodies always check the length each iteration so
      # there's no need to modify generation when a delete is called.

      var
        `sysLen` = `sys`.count()
        `processed`: int
        `finished` = `sysLen` == 0
      `initFirstRun`
      while not `finished`:
        let
          ## Current index into `groups`.
          `groupIndex` {.used, inject.} = `sys`.lastIndex
          ## The entity this row started execution with.
          `rowEnt` {.used, inject.} = `sys`.groups[`groupIndex`].entity
        ## Current system item being processed.
        template item: `typeIdent` {.used.} =
          `strictCatch`
          `streamAssertCheck`
          `sys`.groups[`groupIndex`]
        
        # Inject stream statements.
        `streamBody`
        
        `processed` = `processed` + 1
        `sysLen` = `sys`.count()

        # processing based on commands.
        `processNext`
      
      static:
        `cacheId`.set_inSystemStream false
  )

#---------------------------
# Generating the system proc
#---------------------------

proc generateSystem(id: EcsIdentity, name: string, componentTypes: NimNode, options: ECSSysOptions, systemBody: NimNode, ownedFields: NimNode): NimNode =
  ## Create the system proc to 'tick' the system.

  if systemBody == nil:
    error "makeSystem needs a `body`"

  systemBody.expectKind nnkStmtList

  type
    SystemBlock = enum
      sbFields = "fields"
      sbInit = "init",
      sbStart = "start",
      sbAll = "all",
      sbStream = "stream"
      sbFinish = "finish"
      sbAdded = "added"
      sbRemoved = "removed"
      sbAddedCallback = "addedcallback"
      sbRemovedCallback = "removedcallback"

  let
    sysId = ident(systemVarName(name))
    sys = ident "sys"
    sysType = ident systemTypeName(name)

  var
    sysIdxSearch = id.findSystemIndex(name)
    sysIndex = sysIdxSearch[1]

  var
    extraFields: NimNode
    hasFieldsBlock: bool

  # Extract definitions `fields:` blocks for system variable creation.
  # If the system is previously defined, fields are checked to match.
  for item in systemBody:
    if item.kind == nnkCall and
        item[0].kind in [nnkIdent, nnkOpenSymChoice] and
        ($item[0]).toLowerAscii == $sbFields:

      if not hasFieldsBlock:
        hasFieldsBlock = true
        extraFields = newStmtList()

      for fieldDef in item[1]:
        extraFields.add fieldDef

  result = newStmtList()

  # `repr` may give mangled type names when creating a private ECS
  # inside a block.
  var
    componentTypesStr: string
  
  for t in componentTypes:
  
    let
      uArg = id.unpackComponentArg(t)
      tName =
        if uArg.negate:
          "not " & uArg.typeName
        else:
          uArg.typeName

    if uArg.typeName.len == 0:
      error "Cannot resolve type from " & t.repr

    if componentTypesStr.len > 0:
      componentTypesStr &= ", " & tName
    else:
      componentTypesStr = tName


  if sysIdxSearch.found:
    # This system has already been defined.
    # Check the state passed here matches the previous definition.
    let
      existingSysIdx = sysIdxSearch.index
      existingExtraFields = id.extraFields(existingSysIdx)
    
    if hasFieldsBlock and not(existingExtraFields == extraFields):
      var
        fStr1 = existingExtraFields.repr
        fStr2 = extraFields.repr
      if fStr1 == "nil": fStr1 = "<None>"
      if fStr2 == "nil": fStr2 = "<None>"

      error "Fields block doesn't match the original system definition:\n" &
        "Original fields:\n" & fStr1 &
        "\n\nPassed fields:" & fStr2

    if id.bodyDefined(existingSysIdx):
      error "System \"" & name & "\" already has a body defined"
  
    let
      expectedTypes = id.ecsSysRequirements existingSysIdx
      existingOpts = id.getOptions existingSysIdx
      negations = id.ecsSysNegations(existingSysIdx)


    if componentTypes.len > 0:
      # Check the components given to makeSystem match defineSystem.
      
      template errMsg = error "Components passed to makeSystem \"" & name &
        "\" [" & componentTypesStr &
        "] in conflict with previous definition in defineSystem: [" &
        id.commaSeparate(expectedTypes) & "]" &
        (if negations.len > 0: " and not [" & id.commaSeparate(negations) & "]"
          else: "")
      
      # Check given types match the original definition.

      var
        expIdx: int
      
      for givenType in componentTypes:
        let
          uArg = id.unpackComponentArg(givenType)

        if uArg.negate and uArg.typeId notin negations:
          errMsg()
        elif expIdx < expectedTypes.len:
          if uArg.typeId != expectedTypes[expIdx]:
            errMsg()
          else:
            expIdx += 1

    # Options must match with the original definition.
    if existingOpts != options:
      error "Options don't match with previous definition for system \"" &
        name & "\":\nOriginal options:\n" & $existingOpts &
        "\nPassed options:\n" & $options

    when defined(ecsLog) or defined(ecsLogDetails):
      echo "Adding body to pre-defined system \"", name, "\" with types ",
        id.commaSeparate(expectedTypes)
  else:
    # This is an inline makeSystem.
    when defined(ecsLog) or defined(ecsLogDetails):
      echo "Defining body for system \"", name, "\" with types ", componentTypesStr

    result.add createSysTuple(id, name, componentTypes, ownedFields, extraFields, options)

    sysIdxSearch = id.findSystemIndex(name)
    assert sysIdxSearch.found, "Internal error: cannot find system \"" & name & "\" after adding it"
    
    # Use the new SystemIndex.
    sysIndex = sysIdxSearch.index


  var
    initBodies = newStmtList()

    startBodies = newStmtList()   
    finishBodies = newStmtList()

  let assertItem = id.assertItem(sysIndex)

  var
    activeBlocks: set[SystemBlockKind]
    bodyIndex: int
    blockChoices: array[SystemBlock, string]
  
  for b in SystemBlock.low .. SystemBlock.high:
    blockChoices[b] = $b

  const
    descriptorNodeKinds = [nnkIdent, nnkSym, nnkOpenSymChoice]

  while bodyIndex < systemBody.len:
    # Step through top level nodes and expand block descriptors.
  
    let item = systemBody[bodyIndex]

    if item.len < 1 or
        item.kind notin [nnkCall, nnkCommand] or
        item[0].kind notin descriptorNodeKinds:
      # Not something we need to process.
      bodyIndex.inc
      continue

    let
      sysBlock = ($item[0]).toLowerAscii

    if sysBlock notin blockChoices:
      # Not a known block descriptor.
      bodyIndex.inc
      continue

    let
      code = item[1]

      staticCloseAdded = quote do:
        static:
          const
            identity = EcsIdentity(`id`)

          identity.set_onAddedSource InvalidSystemIndex
          endOperation(identity)
      
      staticCloseRemoved = quote do:
        static:
          const
            identity = EcsIdentity(`id`)
          
          identity.set_onRemovedSource InvalidSystemIndex
          endOperation(identity)

    const
      msgSealed = "This ECS has been sealed with makeEcs() and system '$1' events cannot be changed"
      msgSealedAdded = msgSealed % "added"
      msgSealedRemoved = msgSealed % "removed"

    var
      removeBlock: bool

    case sysBlock.toLowerAscii
      of $sbFields:
        # This block's fields have already been used to create the system
        # variable, this block can be removed from the system body.
        removeBlock = true
        activeBlocks.incl sbkFields

      of $sbInit:
        item[1].expectKind nnkStmtList
        initBodies.add(code)

        removeBlock = true
        activeBlocks.incl sbkInit

      of $sbStart:
        item[1].expectKind nnkStmtList
        startBodies.add(code)

        removeBlock = true
        activeBlocks.incl sbkStart

      of $sbAll:
        item[1].expectKind nnkStmtList
        systemBody[bodyIndex] = id.wrapAllBlock(name, sysIndex, options, code)

        activeBlocks.incl sbkAll

      of $sbFinish:
        item[1].expectKind nnkStmtList
        finishBodies.add(code)

        removeBlock = true
        activeBlocks.incl sbkFinish

      of $sbAdded:

        if id.sealed sysIndex:
          error msgSealedAdded

        if id.len_onAdded(sysIndex) == 0:
          id.add_onAdded(sysIndex, id.checkEventCycle(name, sysIndex, seAdded))
        id.add_onAdded(sysIndex, code)
        id.add_onAdded(sysIndex, staticCloseAdded)
        
        removeBlock = true
        activeBlocks.incl sbkAdded
      
      of $sbRemoved:

        if id.sealed sysIndex:
          error msgSealedRemoved

        if id.len_onRemoved(sysIndex) == 0:
          id.add_onRemoved(sysIndex, id.checkEventCycle(name, sysIndex, seRemoved))

        id.add_onRemoved(sysIndex, code)
        id.add_onRemoved(sysIndex, staticCloseRemoved)
        
        removeBlock = true
        activeBlocks.incl sbkRemoved

      of $sbAddedCallback:

        if id.sealed sysIndex:
          error msgSealedAdded
        let
          eventProcName = ident systemAddedCBName(name)
          gi = ident "groupIndex"
          assertCheck = getAssertItem(assertItem, sys, gi)

        # Callback definitions are added during makeEcs.
        id.add_onAddedCallback(sysIndex, quote do:
          proc `eventProcName`(`sys`: var `sysType`, `gi`: int) =
            template item: untyped =
              `assertCheck`
              `sys`.groups[`gi`]
            `code`
        )
        # Record forward declaration.
        id.add_onAddedCallbackDecl(sysIndex, quote do:
          proc `eventProcName`(`sys`: var `sysType`, `gi`: int)
        )

        removeBlock = true
        activeBlocks.incl sbkAddedCallback

      of $sbRemovedCallback:

        if id.sealed sysIndex:
          error msgSealedRemoved
        let
          eventProcName = ident name & "RemovedCallback"
          gi = ident "groupIndex"
          assertCheck = getAssertItem(assertItem, sys, gi)

        # Callback definitions are added during makeEcs.
        id.add_onRemovedCallback(sysIndex, quote do:
          proc `eventProcName`(`sys`: var `sysType`, `gi`: int) =
            template item: untyped =
              `assertCheck`
              `sys`.groups[`gi`]
            `code`
        )
        # Record forward declaration.
        id.add_onRemovedCallbackDecl(sysIndex, quote do:
          proc `eventProcName`(`sys`: var `sysType`, `gi`: int)
        )

        removeBlock = true
        activeBlocks.incl sbkRemovedCallback

      of $sbStream:

        # Note: Passing `item` rather than `code` as stream has to
        # perform further processing.
        systemBody[bodyIndex] = wrapStreamBlock(id, name, sysIndex, options, item)
        activeBlocks.incl sbkStream

    if removeBlock:
      systemBody.del bodyIndex
    else:
      bodyIndex.inc

  # Set up debug echo statements.
  var echoRun, echoInit, echoAll, echoFinish, echoCompleted = newEmptyNode()
  case options.echoRunning:
  of seNone: discard
  of seEchoUsed, seEchoUsedAndRunning, seEchoUsedAndRunningAndFinished:
    if options.echoRunning in [seEchoUsedAndRunning, seEchoUsedAndRunningAndFinished]:
      echoRun = quote do:
        echo `name` & " running..."
    if sbkInit in activeBlocks:
      echoInit = quote do:
        echo `name` & " initialising"
    if sbkAll in activeBlocks:
      echoAll = quote do:
        echo `name` & " run all"
    if sbkFinish in activeBlocks:
      echoFinish = quote do:
        echo `name` & " run finish"
    if options.echoRunning == seEchoUsedAndRunningAndFinished:
      echoCompleted = quote do:
        echo `name` & " completed"
  of seEchoAll:
    echoRun = quote do:
      echo `name` & " running..."
    echoInit = quote do:
      echo `name` & " initialising"
    echoAll = quote do:
      echo `name` & " run all"
    echoFinish = quote do:
      echo `name` & " run finish"
    echoCompleted = quote do:
      echo `name` & " completed"

  ##
  ## All body
  ##

  let
    cacheId = quote do: EcsIdentity(`id`)

    staticInit = quote do:
      `cacheId`.set_inSystem  true
      `cacheId`.set_inSystemIndex `sysIndex`.SystemIndex
      `cacheId`.set_sysRemoveAffectedThisSystem false
      `cacheId`.set_systemCalledDelete false
      const
        errPrelude = "Internal error: "
        internalError = " macrocache storage is unexpectedly populated for system \"" & `name` & "\""
      assert `cacheId`.readsFrom(`sysIndex`.SystemIndex).len == 0, errPrelude & "readsFrom" & internalError
      assert `cacheId`.writesTo(`sysIndex`.SystemIndex).len == 0, errPrelude & "writesTo" & internalError

    reportPerformance =
      when defined(ecsPerformanceHints):
        quote do:
          # Reports system component access and performance hints.
          # Each component access is displayed in order of access
          # within the system
          const prefix {.used.} = "System \"" & `name` & "\""
          when `cacheId`.len_readsFrom(`sysIndex`.SystemIndex) > 0:
            debugPerformance `cacheId`, prefix & ": Reads from: " &
              `cacheId`.commaSeparate(`cacheId`.readsFrom(`sysIndex`.SystemIndex))
          when `cacheId`.len_writesTo(`sysIndex`.SystemIndex) > 0:
            debugPerformance `cacheId`, prefix & ": Writes to: " &
              `cacheId`.commaSeparate(`cacheId`.writesTo(`sysIndex`.SystemIndex))
          when `cacheId`.systemCalledDelete:
            debugPerformance `cacheId`, prefix & " uses an arbitrary delete, length must be checked each iteration"
          elif `cacheId`.sysRemoveAffectedThisSystem:
            debugPerformance `cacheId`, prefix & " calls a remove that affects this system, length must be checked each iteration"
      else:
        newStmtList()

    staticTearDown = quote do:
      `reportPerformance`

      `cacheId`.set_inSystem false
      `cacheId`.set_inSystemIndex InvalidSystemIndex

      `cacheId`.set_sysRemoveAffectedThisSystem false
      `cacheId`.set_systemCalledDelete false
      
      # Reset event chaining.
      when `cacheId`.onAddedSource != InvalidSystemIndex:
        `cacheId`.set_onAddedSource InvalidSystemIndex
      when `cacheId`.onRemovedSource != InvalidSystemIndex:
        `cacheId`.set_onRemovedSource InvalidSystemIndex      

  # Generate list of types for system comment.
  var sysTypeNames: string
  for typeName in id.systemTypesStr(sysIndex):
    if sysTypeNames != "": sysTypeNames &= ", "
    sysTypeNames &= typeName

  let
    doSystem = ident(doProcName(name))
    systemComment = newCommentStmtNode("System \"" & name & "\", using components: " & sysTypeNames)
    runCheck =
      case options.timings
      of stNone:
        quote do: not `sys`.disabled
      of stRunEvery, stProfiling:
        quote do: (not `sys`.disabled) and ((`sys`.runEvery == 0.0) or (cpuTime() - `sys`.lastTick >= `sys`.runEvery))
    initWrapper =
      if initBodies.len > 0:
        quote do:
          if unlikely(not `sys`.initialised):
            `echoInit`
            `initBodies`
            `sys`.initialised = true
      else:
        newStmtList()

  let
    # Assemble the final proc.
    systemProc = quote do:
      proc `doSystem`*(`sys`: var `sysType`) =
        `systemComment`
        static:
          `staticInit`

        `echoRun`
        if `runCheck`:
          `initWrapper`
          `startBodies`
          if not `sys`.paused:
            `echoAll`
            `systemBody`

          `echoFinish`
          `finishBodies`
          for ent in `sys`.deleteList:
            ent.delete
          `sys`.deleteList.setLen 0
        `echoCompleted`
        
        static:
          `staticTearDown`
      
      template `doSystem`*: untyped =
        `doSystem`(`sysId`)

  # Store the body of the do proc.
  # The procs themselves are only accessible after commitSystem is called.
  id.set_definition(sysIndex, systemProc)

  # Add to the list of systems with a defined body.
  # This allows detection of trying to set a body multiple times.
  id.set_bodyDefined(sysIndex, true)

  # Add to the current list of systems with uncommitted bodies.
  id.addUncommitted sysIndex

## Options specified

macro makeSystemOpts*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[untyped], options: static[ECSSysOptions], systemBody: untyped): untyped =
  ## Define a system and/or add a system code body using an ECS identity with specific options.
  generateSystem(id, name, componentTypes, options, systemBody, newEmptyNode())

macro makeSystemOpts*(name: static[string], componentTypes: openarray[untyped], options: static[ECSSysOptions], systemBody: untyped): untyped =
  ## Define a system and/or add a system code body using the default ECS identity with specific options.
  generateSystem(defaultIdentity, name, componentTypes, options, systemBody, newEmptyNode())

## No options specified

proc sysOptionsOrDefault(id: EcsIdentity, name: string): tuple[found: bool, options: EcsSysOptions] {.compileTime.} =
  let
    sysIdxSearch = id.findSystemIndex(name)

  if sysIdxSearch.found:
    (true, id.getOptions sysIdxSearch.index)
  else:
    (false, defaultSysOpts)

macro makeSystem*(id: static[EcsIdentity], name: static[string], componentTypes: untyped, systemBody: untyped): untyped =
  ## Define a system and/or add a system code body using an ECS identity.
  ## 
  ## Previously defined systems carry their options over, otherwise `defaultSystemOptions` is used.
  let opts = id.sysOptionsOrDefault name
  generateSystem(id, name, componentTypes, opts.options, systemBody, newEmptyNode())

template makeSystem*(name: static[string], componentTypes: untyped, systemBody: untyped): untyped =
  ## Define a system and/or add a system code body using the default ECS identity.
  ## 
  ## Previously defined systems carry their options over, otherwise `defaultSystemOptions` is used.
  defaultIdentity.makeSystem(name, componentTypes, systemBody)

macro makeSystemBody*(id: static[EcsIdentity], name: static[string], systemBody: untyped): untyped =
  ## Define the code body for a previously defined system using an ECS identity.
  let opts = id.sysOptionsOrDefault name

  if not opts.found:
    error "`makeSystemBody` requires a 'defineSystem' for \"" & name & "\""

  generateSystem(id, name, newEmptyNode(), opts.options, systemBody, newEmptyNode())

template makeSystemBody*(name: static[string], systemBody: untyped): untyped =
  ## Define the code body for a previously defined system using the default ECS identity.
  defaultIdentity.makeSystemBody(name, systemBody)

macro forSystemsUsing*(id: static[EcsIdentity], typeIds: static[openarray[ComponentTypeId]], actions: untyped): untyped =
  ## Statically perform `actions` only for systems defined for these types.
  ## Note that typeIds must be known at compile time.
  var processed: seq[SystemIndex]
  result = newStmtList()
  for compId in typeIds:
    for sys in id.systems(compId):
      if sys notin processed:
        processed.add sys
        let
          sysName = id.getSystemName sys
          curSysVar = id.instantiation sys
          curTupType = ident sysName.tupleName
        result.add(quote do:
          block:
            template sys: untyped = `curSysVar`
            template sysType: untyped = `curTupType`
            actions
        )
  genLog "# forSystemsUsing:\n", result.repr

macro forSystemsUsing*(id: static[EcsIdentity], types: openarray[typedesc], actions: untyped): untyped =
  ## Statically perform `actions` only for systems defined for these types.
  ## Systems may have other types defined but must include all of `types`.
  ## Note that types must be known at compile time.
  var typeIds = newSeq[ComponentTypeId]()
  for paramType in types:
    let paramTypeId = typeStringToId(id, $paramType)
    typeIds.add paramTypeId
  #
  result = quote do:
    forSystemsUsing(`typeIds`, `actions`)

