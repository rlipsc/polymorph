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
  case options.indexFormat
  of sifTable:
    quote do:
      `sysNode`.index = initTable[EntityId, int]() 
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

  if options.indexFormat == sifTable:
    result.add(quote do:
      when not declared(tables):
        import tables
    )

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
  if options.timings in [strunEvery, stProfiling]:
    # Keeps track of the last time a tick was issued
    fields.add genField("lastTick", true, ident "float")
    # In theory, inputting a negative curTime() + x in runEvery would allow no trigger until x
    # had passed, then act as if runEvery is zero (ie; always run)
    ## If runEvery is non-zero, this system's do proc will only trigger after this delay
    fields.add genField("runEvery", true, ident "float")
    fields.add genField("lastRun", true, ident "float")

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

proc createSysTuple(id: EcsIdentity, sysName: string, componentTypes, ownedComponents: NimNode, extraFields: NimNode, sysOptions: ECSSysOptions): NimNode =
  ## Create a tuple to hold the required combination of types for this system.
  result = newStmtList()

  let existingSysIndex = id.findSystemIndex sysName
  doAssert (not existingSysIndex.found) or
    (existingSysIndex.index notin id.ecsSystemsToBeSealed),
    "System \"" & sysName & "\" has already been defined"
  doAssert componentTypes.len > 0, "Systems require at least one type to operate on but none have been provided (missing a defineSystem?)"
  if sysOptions.indexFormat in [sifArray, sifAllocatedSeq] and sysOptions.maxEntities == 0:
    error "System \"" & sysName & "\", system options: maxEntities cannot be zero when using a fixed size type"

  var
    typeIdents: seq[NimNode]
    passedComponentIds: seq[ComponentTypeId]

  for item in componentTypes:
    let typeId = id.typeStringToId($item)
    doAssert typeId notin id.ecsSealedComponents,
      "Component " & id.typeName(typeId) &
      " has already been sealed with makeEcs and cannot be extended to system \"" &
      sysName &
      "\". Use defineSystem to forward declare the system or place this makeSystem before makeEcs."
    typeIdents.add item
    passedComponentIds.add typeId

  # Process owned components.
  var ownedComponentIds: seq[ComponentTypeId]
  for comp in ownedComponents:
    let typeId = id.typeStringToId($comp)
    
    doAssert typeId.int != -1, "This type's id can't be found in registered components: " & $comp
    if typeId notin passedComponentIds:
      error "Asking to own component " & id.typeName(typeId) &
        " that is not part of system \"" & sysName &
        "\". System components are: " & id.commaSeparate(passedComponentIds)
    
    ownedComponentIds.add typeId

  for typeId in passedComponentIds:
    let curOwner = id.systemOwner typeId
    
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
  let sysIndex = id.addSystem sysName
  id.setOptions(sysIndex, sysOptions)
  id.set_Instantiation(sysIndex, inst)
  assert id.len_ecsOwnedComponents(sysIndex) == 0

  for sys in ownedComponentIds:
    id.add_ecsOwnedComponents(sysIndex, sys)

  # Build tuple type of components for this system.
  var elements = nnkTupleTy.newTree()
  # Add entity field.
  elements.add(nnkIdentDefs.newTree(ident("entity"), ident("EntityRef"), newEmptyNode()))
  # Add component fields to the tuple.
  for typeId in passedComponentIds:
    let tyName = id.typeName typeId
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
      else: error "Unhandled kind for extra field in system \"" & sysName & "\", expected `field: type` or `field = value`, got:\n" & tyDef.treeRepr
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

  id.add_ecsSystemsToBeSealed sysIndex
  id.add_ecsSysUncommitted sysIndex
  id.add_ecsSysDefined sysIndex

  genLog "\n# System \"" & sysName & "\":\n" & result.repr

macro defineSystemOwner*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[typedesc], ownedComponents: openarray[typedesc], options: static[ECSSysOptions], extraFields: untyped): untyped =
  result = id.createSysTuple(name, componentTypes, ownedComponents, extraFields, options)

macro defineSystemOwner*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[typedesc], ownedComponents: openarray[typedesc], options: static[ECSSysOptions]): untyped =
  result = id.createSysTuple(name, componentTypes, ownedComponents, nil, options)

macro defineSystem*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[typedesc], options: static[ECSSysOptions], extraFields: untyped): untyped =
  ## Forward-define a system and its types, providing extra fields to incorporate into the resultant system instance.
  result = id.createSysTuple(name, componentTypes, nil, extraFields, options)

template defineSystem*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[typedesc], options: static[ECSSysOptions]): untyped =
  ## Forward-define a system and its types using options.
  defineSystem(id, name, componentTypes, options, nil)

template defineSystem*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[typedesc]): untyped =
  ## Forward-define a system and its types using the default system options.
  defineSystem(id, name, componentTypes, defaultSystemOptions, nil)

#---------------------------------------------------
# Convenience templates for using a default identity
#---------------------------------------------------

template defineSystem*(name: static[string], componentTypes: openarray[typedesc], options: static[ECSSysOptions], extraFields: untyped): untyped =
  defaultIdentity.defineSystem(name, componentTypes, options, extraFields)

template defineSystem*(name: static[string], componentTypes: openarray[typedesc], options: static[ECSSysOptions]): untyped =
  defaultIdentity.defineSystem(name, componentTypes, options)

template defineSystem*(name: static[string], componentTypes: openarray[typedesc]): untyped =
  defaultIdentity.defineSystem(name, componentTypes)

template defineSystemOwner*(name: static[string], componentTypes: openarray[typedesc], ownedComponents: openarray[typedesc], options: static[ECSSysOptions], extraFields: untyped): untyped =
  defaultIdentity.defineSystemOwner(name, componentTypes, ownedComponents, options, extraField)

template defineSystemOwner*(name: static[string], componentTypes: openarray[typedesc], ownedComponents: openarray[typedesc], options: static[ECSSysOptions]): untyped =
  defaultIdentity.defineSystemOwner(name, componentTypes, ownedComponents, options)

#---------------------------
# Generating the system proc
#---------------------------

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

proc generateSystem(id: EcsIdentity, name: string, componentTypes: NimNode, options: ECSSysOptions, extraFields: NimNode, systemBody: NimNode, ownedFields: NimNode): NimNode =
  ## Create the system proc to 'tick' the system.

  let
    typeName = name.capitalizeAscii
    typeIdent = ident(tupleName(typeName))
    sysId = ident(systemVarName(name))
    sys = ident "sys"
    sysType = ident systemTypeName(name)
  var
    sysIdxSearch = id.findSystemIndex(name)

  result = newStmtList()

  if sysIdxSearch.found:
    # This system has already been defined.
    let
      existingSysIdx = sysIdxSearch.index
      expectedTypes = id.ecsSysRequirements(existingSysIdx)
    
    if componentTypes.len > 0:
      # Check the components given to makeSystem match defineSystem.
      template errMsg =
        error "Components passed to makeSystem \"" & name &
          "\" " & componentTypes.repr &
          " in conflict with previous definition in defineSystem: [" &
          id.commaSeparate(expectedTypes) & "]"
      
      if componentTypes.len != expectedTypes.len:
        errMsg()

      for i, givenType in componentTypes:
        # Types must be given in the same order.
        if typeStringToId(id, $givenType) != expectedTypes[i]:
          errMsg()

    if sysIdxSearch.index in id.ecsSysBodiesAdded:
      error "System \"" & name & "\" already has a body defined"
  
    when defined(ecsLog) or defined(ecsLogDetails):
      echo "Adding body to pre-defined system \"", name, "\" with types ",
        id.commaSeparate(expectedTypes)
  else:
    # This is an inline makeSystem.
    when defined(ecsLog) or defined(ecsLogDetails):
      echo "Defining body for system \"", name, "\" with types ", componentTypes.repr
    result.add createSysTuple(id, name, componentTypes, ownedFields, extraFields, options)
    sysIdxSearch = id.findSystemIndex(name)
    assert sysIdxSearch.found, "Internal error: cannot find system \"" & name & "\" after adding it"

  var
    initBody = newStmtList()
    startBody = newStmtList()
    finishBody = newStmtList()
    allBody = newStmtList()
    allWrapper = newStmtList()
    streamBody = newStmtList()
    streamWrapper = newStmtList()

  let
    sysIndex = sysIdxSearch[1]
    options = id.getOptions sysIndex

  # The sequence of generateSystem invocations.
  # This is used by commitSystems to build the combined doProc.
  id.add_systemOrder sysIndex

  if systemBody == nil:
    error "makeSystem needs a `body`"

  systemBody.expectKind nnkStmtList

  let assertItem = id.assertItem(sysIndex)

  type
    Command = enum cmNone, cmMultiPass, cmStochastic
    SystemBlock = enum
      sbInit = "init",
      sbStart = "start",
      sbAll = "all",
      sbStream = "stream"
      sbFinish = "finish"
      sbAdded = "added"
      sbRemoved = "removed"
      sbAddedCallback = "addedcallback"
      sbRemovedCallback = "removedcallback"

  const
    commandStrs = [
      cmMultiPass: "multipass",
      cmStochastic: "stochastic"
      ]

  var verbChoices = $SystemBlock.low
  for verb in 1 .. SystemBlock.high.ord:
    verbChoices &= ", " & $SystemBlock(verb)

  var
    command: Command
    streamAmount = newEmptyNode()

  # Step through top level nodes and extract verbs and commands.
  for item in systemBody:
    if item.kind notin [nnkCall, nnkCommand]: continue

    doAssert item[0].kind in [nnkIdent, nnkSym],
      "In system \"" & name & "\" expected command: " & verbChoices & " but got " & item[0].repr
    let
      verb = $item[0]
      code = item[1]
      tcn = ident typeClassName()

    type SysEventOp = enum seAdded = "onAdded", seRemoved = "onRemoved"

    proc checkEventCycle(op: SysEventOp): NimNode =
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
      # System events such as "onAdded" and "onRemoved" are therefore
      # checked to halt compilation when nested state changes try to
      # invoke the same event for the same system again.
      # 
      # To avoid the possibility of compile time cycles within system
      # events, use `onAddedCallback` and `onRemoveCallback`.
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
              
              when defined(ecsPerformanceHints):
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

    let
      staticCloseAdded = quote do:
        static:
          const identity = EcsIdentity(`id`)
          identity.set_onAddedSource InvalidSystemIndex
          endOperation(identity)
      staticCloseRemoved = quote do:
        static:
          const identity = EcsIdentity(`id`)
          identity.set_onRemovedSource InvalidSystemIndex
          endOperation(identity)
    const
      msgSealed = "This ECS has been sealed with makeEcs() and system '$1' events cannot be changed"
      msgSealedAdded = msgSealed % "added"
      msgSealedRemoved = msgSealed % "removed"

    case verb.toLowerAscii

      of $sbInit:
        item[1].expectKind nnkStmtList
        initBody.add(code)
        
      of $sbStart:
        item[1].expectKind nnkStmtList
        startBody.add(code)

      of $sbAll:
        item[1].expectKind nnkStmtList
        allBody = code

      of $sbFinish:
        item[1].expectKind nnkStmtList
        finishBody.add(code)

      of $sbAdded:

        if id.sealed sysIndex:
          error msgSealedAdded

        if id.len_onAdded(sysIndex) == 0:
          id.add_onAdded(sysIndex, checkEventCycle(seAdded))
        id.add_onAdded(sysIndex, code)
        id.add_onAdded(sysIndex, staticCloseAdded)
      
      of $sbRemoved:

        if id.sealed sysIndex:
          error msgSealedRemoved

        if id.len_onRemoved(sysIndex) == 0:
          id.add_onRemoved(sysIndex, checkEventCycle(seRemoved))

        id.add_onRemoved(sysIndex, code)
        id.add_onRemoved(sysIndex, staticCloseRemoved)

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

      of $sbStream:

        # Stream commands:
        #   stream multipass <optional count>: <code block>
        #     - forces a minimum of `count` rows to be processed even if repeated.
        #     - omitting count uses system.streamRate.
        #   stream stochastic <optional count>: <code block>
        #     - selects rows to process at random.
        #     - omitting count uses system.streamRate.

        case code.kind
        of nnkIntLit:
          # Provided a stream amount `stream 10:` or `stream expression:`.
          item.expectLen 3

          streamAmount = item[1]
          item[2].expectKind nnkStmtList
          streamBody = item[2]

        of nnkCommand:
          # Stream is followed by a command and parameter, eg `stream stochastic 10:`.
          item[1].expectLen 2
          item[1][0].expectKind nnkIdent

          let userCommand = item[1][0].strVal.toLowerAscii

          case userCommand
          of commandStrs[cmMultipass]:
            command = cmMultiPass
          of commandStrs[cmStochastic]:
            command = cmStochastic
          else:
            error "Unknown command for streaming: \"" & userCommand & "\""

          streamAmount = item[1][1]

          item[2].expectKind nnkStmtList
          streamBody = item[2]

        of nnkIdent:
          # Stream is followed by an ident which could be a command or parameter.
          let userCommand = item[1].strVal.toLowerAscii

          # When a command is provided here the system.streamRate will be used.
          case userCommand
          of commandStrs[cmMultipass]:
            command = cmMultiPass
          of commandStrs[cmStochastic]:
            command = cmStochastic
          else:
            # Allow `stream myVar:`
            streamAmount = item[1]

          item[2].expectKind nnkStmtList
          streamBody = item[2]
        else:
          # No stream parameters
          code.expectKind nnkStmtList
          streamBody = code
      else: error(&"makeSystem: Unknown verb \"{verb}\", expected {verbChoices}")

  var activeBlocks: set[SystemBlockKind]

  if initBody.len > 0: activeBlocks.incl sbkInit
  if startBody.len > 0: activeBlocks.incl sbkStart
  if allBody.len > 0: activeBlocks.incl sbkAll
  if streamBody.len > 0: activeBlocks.incl sbkStream
  if finishBody.len > 0: activeBlocks.incl sbkFinish
  if id.len_onAdded(sysIndex) > 0: activeBlocks.incl sbkAdded
  if id.len_onRemoved(sysIndex) > 0: activeBlocks.incl sbkRemoved
  if id.len_onAddedCallback(sysIndex) > 0: activeBlocks.incl sbkAddedCallback
  if id.len_onRemovedCallback(sysIndex) > 0: activeBlocks.incl sbkRemovedCallback

  if activeBlocks == {}:
    error("Systems must do something within " & verbChoices)

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

  proc timeWrapper(core: NimNode): NimNode =
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

  ##
  ## All body
  ##

  let
    cacheId = quote do: EcsIdentity(`id`)
    groupIndex = ident "groupIndex"
    idx = genSym(nskVar, "i")
    sysLen = ident "sysLen"
    # if `entity` != `item.entity` then this row has been removed.
    rowEnt = ident "entity"
    defineIdx =
      if id.len_ecsOwnedComponents(sysIndex) > 0:
        # Owner systems forgo the first item to keep parity
        # with component storage and the mechanics of `valid`.
        # TODO: This needs more thought.
        quote do:
          var `idx` = 1
      else:
        quote do:
          var `idx`: int
    undefinedItemMsg = "Cannot access 'item' here as the current row is undefined " &
      "until the next iteration, due to a previous removal of components " &
      "or deletion of entities within this system iteration. Use the injected " &
      "'entity' variable or make a note of data in 'item' before " &
      "removing components or deleting entities."

    staticInit = quote do:
      `cacheId`.set_inSystem  true
      `cacheId`.set_inSystemIndex `sysIndex`.SystemIndex
      `cacheId`.set_sysRemoveAffectedThisSystem false
      `cacheId`.set_systemCalledDelete false
      `cacheId`.set_systemCalledDeleteEntity false
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
          elif `cacheId`.systemCalledDeleteEntity:
            debugPerformance `cacheId`, prefix & " calls deleteEntity, length must be checked each iteration"
      else:
        newStmtList()

    staticTearDown = quote do:
      `reportPerformance`

      `cacheId`.set_inSystem false
      `cacheId`.set_inSystemIndex InvalidSystemIndex

      `cacheId`.set_sysRemoveAffectedThisSystem false
      `cacheId`.set_systemCalledDelete false
      `cacheId`.set_systemCalledDeleteEntity false
      
      # Reset event chaining.
      when `cacheId`.onAddedSource != InvalidSystemIndex:
        `cacheId`.set_onAddedSource InvalidSystemIndex
      when `cacheId`.onRemovedSource != InvalidSystemIndex:
        `cacheId`.set_onRemovedSource InvalidSystemIndex      

    assertCheck = getAssertItem(assertItem, sys, idx)

    strictCatch =
      if defined(ecsStrict):
        quote do:
          static:
            when `cacheId`.sysRemoveAffectedThisSystem:
              error `undefinedItemMsg`
      else: newStmtList()

    allCore = quote do:
      static:
        `staticInit`
        `cacheId`.set_inSystemAll true

      var `sysLen` = `sys`.count()
      if `sysLen` > 0:
        `defineIdx`

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

          template deleteEntity: untyped {.used.} =
            ## Convenience shortcut for deleting just this row's entity.
            # Although we know this delete only affects this row, we don't
            # know that it definitely will (for example if the deleteEntity depends on a 
            # condition). As such we still need to check the length each iteration.
            static:
              `cacheId`.set_inSystemDeleteRow true
              `cacheId`.set_systemCalledDeleteEntity true
            `rowEnt`.delete
            static:
              `cacheId`.set_inSystemDeleteRow false

          # Inject the statements from `all:`
          `allBody`
          when `cacheId`.systemCalledDeleteEntity or
            `cacheId`.systemCalledDelete or
            `cacheId`.sysRemoveAffectedThisSystem:
              `sysLen` = `sys`.count()
              if `sysLen` > 0 and (`idx` < `sysLen` and `sys`.groups[`idx`].entity == `rowEnt`):
                # This row wasn't deleted so move forward.
                `idx` = `idx` + 1
          else:
            `idx` = `idx` + 1
      
      static:
        `cacheId`.set_inSystemAll false
        `staticTearDown`

  id.set_sysCheckLengthPerIter false

  # Build the all items node.
  let timeWrapperAll = timeWrapper(allCore)
  if allBody.len > 0:
    allWrapper.add(quote do:
      `timeWrapperAll`
    )

  ##
  ## Streaming body
  ##

  # Use default stream rate if not provided.
  if streamAmount.kind == nnkEmpty: streamAmount = quote do: `sys`.streamRate
  let
    processed = genSym(nskVar, "processed")
    finished = genSym(nskVar, "finished")
    randomValue = bindSym "rand"

    processNext =
      case command
      of cmNone:
        quote do:
          `sys`.lastIndex = `sys`.lastIndex + 1
          `finished` = (`processed` >= `streamAmount`) or (`sys`.lastIndex >= `sysLen`)
      of cmMultiPass:
        quote do:
          `sys`.lastIndex = (`sys`.lastIndex + 1) mod `sysLen`
          `finished` = `processed` >= `streamAmount`
      of cmStochastic:
        quote do:
          `sys`.lastIndex = `randomValue`(`sys`.high)
          `finished` = `processed` >= `streamAmount`
    initFirstRun =
      case command
      of cmNone, cmMultiPass:
        quote do:
          if `sys`.lastIndex >= `sysLen` - 1: `sys`.lastIndex = 0
      of cmStochastic:
        quote do:
          if not `finished`:
            `sys`.lastIndex = `randomValue`(`sys`.high)

  let
    streamAssertCheck = getAssertItem(assertItem, sys, groupIndex)
    # Streaming body.
    streamCore = quote do:
      # loop per entity in system

      # Note that streaming bodies always check the length each iteration so
      # there's no need to modify generation when a delete is called.
      static:
        `staticInit`
        `cacheId`.set_inSystemStream  true

      var
        `sysLen` = `sys`.count()
        `processed`: int
        `finished` = `sysLen` == 0
      `defineIdx` # TODO: Ensure owner streaming starts at index 1.
      `initFirstRun`
      while `finished` == false:
        let
          ## Current index into `groups`.
          `groupIndex` {.used, inject.} = `sys`.lastIndex
          ## The entity this row started execution with.
          `rowEnt` {.used, inject.} = `sys`.groups[`groupIndex`].entity
        ## Current system item being processed.
        template item: `typeIdent` {.used.} =
          `streamAssertCheck`
          `sys`.groups[`groupIndex`]
        template deleteEntity: untyped {.used.} =
          ## Convenience shortcut for deleting just this row's entity.
          `rowEnt`.delete
        
        # Inject stream statements.
        `streamBody`
        
        `processed` = `processed` + 1
        `sysLen` = `sys`.count()

        # processing based on commands.
        `processNext`
      
      static:
        `cacheId`.set_inSystemStream false
        `staticTearDown`

  let timeWrapperStream = timeWrapper(streamCore)
  if streamBody.len > 0:
    streamWrapper = quote do:
      `timeWrapperStream`

  id.set_inSystemStream  false
  
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
      if initBody.len > 0:
        quote do:
          if unlikely(not `sys`.initialised):
            `echoInit`
            `initBody`
            `sys`.initialised = true
      else:
        initBody

  let
    # Assemble the final proc.
    systemProc = quote do:
      proc `doSystem`*(`sys`: var `sysType`) =
        `systemComment`
        `echoRun`
        if `runCheck`:
          `initWrapper`
          `startBody`
          if not `sys`.paused:
            `echoAll`
            `allWrapper`
            `streamWrapper`
          `echoFinish`
          `finishBody`
          for ent in `sys`.deleteList:
            ent.delete
          `sys`.deleteList.setLen 0
        `echoCompleted`
      template `doSystem`*: untyped =
        `doSystem`(`sysId`)

  id.set_inSystem  false

  # Store the body of the do proc.
  # The procs themselves are only accessible after commitSystem is called.
  id.set_definition(sysIndex, systemProc)

## Options specified

macro makeSystemOptFields*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[untyped], options: static[ECSSysOptions], extraFields, systemBody: untyped): untyped =
  ## Make a system, defining types, options and adding extra fields to the generated system type.
  generateSystem(id, name, componentTypes, options, extraFields, systemBody, newEmptyNode())

macro makeSystemOptFields*(name: static[string], componentTypes: openarray[untyped], options: static[ECSSysOptions], extraFields, systemBody: untyped): untyped =
  ## Make a system, defining types, options and adding extra fields to the generated system type.
  generateSystem(defaultIdentity, name, componentTypes, options, extraFields, systemBody, newEmptyNode())

macro makeSystemOpts*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[untyped], options: static[ECSSysOptions], systemBody: untyped): untyped =
  ## Make a system.
  generateSystem(id, name, componentTypes, options, newEmptyNode(), systemBody, newEmptyNode())

macro makeSystemOpts*(name: static[string], componentTypes: openarray[untyped], options: static[ECSSysOptions], systemBody: untyped): untyped =
  ## Make a system.
  generateSystem(defaultIdentity, name, componentTypes, options, newEmptyNode(), systemBody, newEmptyNode())

## No options specified

macro makeSystem*(id: static[EcsIdentity], name: static[string], componentTypes: openarray[untyped], systemBody: untyped): untyped =
  ## Make and define a system using `defaultSystemOptions`.
  generateSystem(id, name, componentTypes, defaultSystemOptions, newEmptyNode(), systemBody, newEmptyNode())

template makeSystem*(name: static[string], componentTypes: openarray[untyped], systemBody: untyped): untyped =
  defaultIdentity.makeSystem(name, componentTypes, systemBody)

macro makeSystemBody*(id: static[EcsIdentity], name: static[string], systemBody: untyped): untyped =
  ## Make a system based on types previously defined with `defineSystem`.
  ## Used to build the body for a forward declared system.
  var found: bool
  for sysName in id.allSystemNames:
    if sysName.toLowerAscii == name.toLowerAscii:
      found = true
      break
  if not found: error "`makeSystemBody` requires a `defineSystem` for \"" & name & "\""
  generateSystem(id, name, newEmptyNode(), defaultSystemOptions, newEmptyNode(), systemBody, newEmptyNode())

template makeSystemBody*(name: static[string], systemBody: untyped): untyped =
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

