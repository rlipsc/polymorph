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

## This module builds the backend macrocache functions used to handle
## state across module boundaries.
## 
## Macros generate type safe parametrised procs that statically
## concatenate an identity string with the parameters to map to
## `CacheSeq` keys.
## 
## There are two types of operations:
## 
## - List states: append only lists that map to `CacheSeq` directly.
## - "Variables": `CacheSeq` that returns the last value added.
## 
## State database access is abstracted as follows:
## 
## 1) Top level "global variables".
## 2) Top level "global lists".
## 3) Indexed lists: eg; a list for each `SystemIndex` value.
## 4) Indexed lookup lists: a list indexable by two parameters.
## 5) Conversion of objects to and from top level "variables".
## 
## There is specific and limited support for value types as this module
## is intended for internal use.
## 
## Example:
## 
## .. code-block:: nim
##   genGlobalStates(string, ["lastFoo"])
##   genListStates(SystemIndex, string, ["foo"])
##
##   const myId = newEcsIdentity("MyId")
##
##   static:
##     let system1 = 1.SystemIndex
##     for i in 0 ..< 5:
##       let value = $i
##       myId.add_foo(system1, value)
##       myId.set_lastFoo(value)
##
##     echo myId.foo(system1), " last: ", myId.lastFoo
##     # @["0", "1", "2", "3", "4"] last: 4

import macrocache, macros, ../sharedtypes, strutils

type EcsIdentity* = CacheSeq

#----------------------
# Registered components
#----------------------

proc components*(id: EcsIdentity): CacheSeq {.compileTime.} =
  CacheSeq(id.string & "Components")

proc checkId*(id: EcsIdentity, compId: ComponentTypeId) {.compileTime.} =
  let
    key = id.components
    keyLen = key.len
  if compId.int notin 1 ..< keyLen:
    let compRangeStr =
      if keyLen > 0: "[1 ..< " & $keyLen & "]"
      else: "<no components>"
    error "Invalid ComponentTypeId passed to checkId: " & $compId.int &
      ", components ids registered " & compRangeStr

#-------------------
# Registered systems
#-------------------

proc systemsKey*(id: EcsIdentity): CacheSeq {.compileTime.} =
  ## Get key for the list of system.
  CacheSeq(id.string & "System")

proc checkId*(id: EcsIdentity, sysIdx: SystemIndex) {.compileTime.} =
  let
    key = id.systemsKey
    keyLen = key.len
  if sysIdx.int notin 1 ..< keyLen:
    error "Invalid SystemIndex passed to checkId: \"" & $sysIdx.int &
      "\". System count = " & $keyLen


#--------------------------------
# CacheSeq access proc generation
#--------------------------------

proc typeNodeAccess(typeNode: NimNode): tuple[accessNode, accessType: NimNode] =
  ## Determine the "base type" of returnType where `typeNode` may
  ## be a distinct type.
  ## Since this is an internal routine, we only cover a few types.
  let
    tyImpl = typeNode.getTypeImpl
    tyImplKind = tyImpl.typeKind

  assert tyImplKind == ntyTypeDesc
  tyImpl.expectKind nnkBracketExpr
  tyImpl.expectMinLen 2

  var typeSym = tyImpl[1]
  
  typeSym.expectKind nnkSym
  
  var sourceType = typeSym.getType
  
  if sourceType.kind == nnkEnumTy:
    sourceType = typeSym
    return (ident "intVal", ident "int")

  if sourceType.typeKind == ntyDistinct:
    typeSym = sourceType[1]

  case typeSym.strVal
  of "Natural", "int", "uint", "uint16", "uint32":
    return (ident "intVal", typeSym.copy)
  of "bool":
    return (ident "boolVal", typeSym.copy)
  of "string":
    return (ident "strVal", typeSym.copy)
  of "NimNode":
    return (nil, nil)
  else:
    error "Unhandled type: " & typeNode.repr

macro genListStates(indexType, itemType: typedesc, listNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `listNames`.
  ## Each list is a sequence of `itemType` taking `indexType` as the index.
  
  result = newStmtList()
  result.add(quote do:
    {.push hint[ConvFromXtoItselfNotNeeded]:off.}
  )

  let
    (nodeGet, accessType) = typeNodeAccess itemType
    (_, idx_accessType) = typeNodeAccess indexType
    opKey = $itemType
    indexTypeStr = $indexType
    id = ident "id"
    res = ident "result"
    param = ident( toLowerAscii(indexTypeStr[0]) & indexTypeStr[1..^1] & "Value")

  for listStr in listNames:
    let
      cList = listStr.capitalizeAscii
      listKey = newLit cList & opKey
      listGet = ident listStr
      listGetNode = ident listStr & "Node"
      listAppend = ident "add" & cList
      listLen = ident "len" & cList
      value = ident "value"
      item = ident "item"

      readNode =
        if nodeGet != nil:
          quote do: `itemType`(`item`.`nodeGet`)
        else:
          quote do: `item`

      writeNode =
        if accessType != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
      
      paramKey =
        if idx_accessType != nil and idx_accessType != indexType:
          quote do: $`idx_accessType`(`param`)
        else:
          quote do: $`param`

      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `listKey` & `paramKey`)
    
    if $itemType == "NimNode":
      # Extra convenience proc for NimNodes for wrapping with a
      # statement list.
      let res = ident "result"

      result.add(quote do:
        proc `listGetNode`*(`id`: EcsIdentity, `param`: `indexType`): `itemType` {.compileTime.} =
          `res` = newStmtList()
          let key = `key`
          for event in key.items:
            `res`.add event
      )
    
    result.add(quote do:
      proc `listLen`*(`id`: EcsIdentity, `param`: `indexType`): Natural {.compileTime.} =
        `key`.len

      proc `listAppend`*(`id`: EcsIdentity, `param`: `indexType`, `value`: `itemType`) {.compileTime.} =
        `key`.add `writeNode`
      
      proc `listGet`*(`id`: EcsIdentity, `param`: `indexType`): seq[`itemType`] {.compileTime.} =
        let
          key = `key`
          listLen = key.len

        if listLen > 0:
          `res`.setLen listLen
          var i: int
          for `item` in key:
            `res`[i] = `readNode`
            i.inc
    )
  result.add(quote do:
    {.push hint[ConvFromXtoItselfNotNeeded]:on.}
  )

macro genLookupListStates(indexType1, indexType2, itemType: typedesc, listNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `listNames`.
  ## Each list is a sequence of `itemType` taking `indexType` as the index.
  
  result = newStmtList()

  let
    (_, idx_accessType1) = typeNodeAccess indexType1
    (_, idx_accessType2) = typeNodeAccess indexType2
    (nodeGet, accessType) = typeNodeAccess itemType

    opKey = $itemType
    indexTypeStr1 = $indexType1 & "1"
    indexTypeStr2 = $indexType2 & "2"
    id = ident "id"
    res = ident "result"

    param1 = ident( toLowerAscii(indexTypeStr1[0]) & indexTypeStr1[1..^1] & "Value")
    param2 = ident( toLowerAscii(indexTypeStr2[0]) & indexTypeStr2[1..^1] & "Value")

  for listStr in listNames:
    let
      cList = listStr.capitalizeAscii
      listKey = newLit cList & opKey
      listGet = ident listStr
      listGetNode = ident listStr & "Node"
      listAppend = ident "add" & cList
      listLen = ident "len" & cList
      value = ident "value"
      item = ident "item"

      readNode =
        if nodeGet != nil:
          quote do: `itemType`(`item`.`nodeGet`)
        else:
          if accessType != nil:
            quote do: `itemType`(`item`)
          else:
            quote do: `item`

      writeNode =
        if accessType != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
      p1Key =
        if idx_accessType1 != nil:
          quote do: $`idx_accessType1`(`param1`)
        else:
          quote do: $`param1`
      p2Key =
        if idx_accessType2 != nil:
          quote do: $`idx_accessType2`(`param2`)
        else:
          quote do: $`param2`

    let
      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `listKey` & `p1Key` & "_" & `p2key`)

    if $itemType == "NimNode":
      # Extra convenience proc for NimNodes for wrapping with a
      # statement list.

      result.add(quote do:
        proc `listGetNode`*(`id`: EcsIdentity, `param1`: `indexType1`, `param2`: `indexType2`): `itemType` {.compileTime.} =
          `res` = newStmtList()
          let key = `key`
          for event in key.items:
            `res`.add event
      )
    
    result.add(quote do:
      proc `listLen`*(`id`: EcsIdentity, `param1`: `indexType1`, `param2`: `indexType2`): Natural {.compileTime.} =
        `key`.len

      proc `listAppend`*(`id`: EcsIdentity, `param1`: `indexType1`, `param2`: `indexType2`, `value`: `itemType`) {.compileTime.} =
        `key`.add `writeNode`
      
      proc `listGet`*(`id`: EcsIdentity, `param1`: `indexType1`, `param2`: `indexType2`): seq[`itemType`] {.compileTime.} =
        let
          key = `key`
          listLen = key.len

        if listLen > 0:
          `res`.setLen listLen
          var i: int
          for `item` in key:
            `res`[i] = `readNode`
            i.inc
    )

macro genItemStates(indexType, itemType: typedesc, itemNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `itemNames`.
  ## Each item represents an individual value indexed by type id.
  ## The index type is integrated into the key.
  ## 
  ## Though all additions to a value are recorded, only the most
  ## recent value is returned.
  
  result = newStmtList()
  
  let
    (accessNode, accessType) = typeNodeAccess itemType
    indexTypeStr = $indexType
    id = ident "id"
    param = ident( toLowerAscii(indexTypeStr[0]) & indexTypeStr[1..^1] & "Value")
    res = ident "result"

  for itemStr in itemNames:
    let
      itemKey = newLit itemStr & indexTypeStr
      getItem = ident itemStr
      setItem = ident "set" & itemStr
      value = ident "value"

      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `itemKey` & $`param`.int)

      readNode =
        if accessType != nil:
          quote do:
            let key = `key`
            if key.len > 0:
              key[key.len - 1].`accessNode`.`itemType`
            else:
              default(`itemType`)
        else:
          quote do:
            let key = `key`
            if key.len > 0:
              key[key.len - 1]
            else:
              default(`itemType`)

      writeNode =
        if accessNode != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
    
    result.add(quote do:
      
      proc `getItem`*(`id`: EcsIdentity, `param`: `indexType`): `itemType` {.compileTime.} =
        `id`.checkId(`param`)
        {.push hint[ConvFromXtoItselfNotNeeded]: off.}
        `res` = `readNode`
        {.pop.}

      proc `setItem`*(`id`: EcsIdentity, `param`: `indexType`, `value`: `itemType`) {.compileTime.} =
        {.push hint[ConvFromXtoItselfNotNeeded]: off.}
        `id`.checkId(`param`)
        `key`.add `writeNode`
        {.pop.}
    )

macro genGlobalStates(itemType: typedesc, itemNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `itemNames`.
  ## Each item represents an individual value indexed by type id.
  ## Though all additions to a value are recorded, only the most
  ## recent value is returned.
  result = newStmtList()
  
  let
    (accessNode, accessType) = typeNodeAccess itemType
    id = ident "id"
    res = ident "result"

  for itemStr in itemNames:
    let
      itemKey = newLit itemStr
      getItem = ident itemStr
      setItem = ident "set" & itemStr
      value = ident "value"

      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `itemKey`)

      readNode =
        if accessType != nil:
          quote do:
            let key = `key`
            if key.len > 0:
              key[key.len - 1].`accessNode`.`itemType`
            else:
              default(`itemType`)
        else:
          quote do:
            let key = `key`
            if key.len > 0:
              key[key.len - 1].`itemType`
            else:
              default(`itemType`)

      writeNode =
        if accessNode != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
    
    result.add(quote do:
      
      proc `getItem`*(`id`: EcsIdentity): `itemType` {.compileTime.} =
        {.push hint[ConvFromXtoItselfNotNeeded]: off.}
        `res` = `readNode`
        {.pop.}

      proc `setItem`*(`id`: EcsIdentity, `value`: `itemType`) {.compileTime.} =
        {.push hint[ConvFromXtoItselfNotNeeded]: off.}
        `key`.add `writeNode`
        {.pop.}
    )

macro genGlobalListStates(itemType: typedesc, listNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `listNames`.
  ## Each list is a sequence of `itemType` taking `indexType` as the index.
  
  result = newStmtList()

  let
    (nodeGet, accessType) = typeNodeAccess itemType
    opKey = $itemType
    id = ident "id"
    res = ident "result"
    # Bind items for CacheString.
    csItems = bindSym "items"

  for listStr in listNames:
    let
      cList = listStr.capitalizeAscii
      listKey = newLit cList & opKey
      listGet = ident listStr
      listGetNode = ident listStr & "Node"
      listAppend = ident "add" & cList
      listLen = ident "len" & cList
      value = ident "value"
      item = ident "item"

      readNode =
        if nodeGet != nil:
          quote do: `itemType`(`item`.`nodeGet`)
        else:
          quote do: `item`

      writeNode =
        if accessType != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
      
      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `listKey`)

    if $itemType == "NimNode":
      # Extra convenience proc for NimNodes for wrapping with a
      # statement list.

      result.add(quote do:
        proc `listGetNode`*(`id`: EcsIdentity): `itemType` {.compileTime.} =
          `res` = newStmtList()
          let key = `key`
          for item in key.items:
            `res`.add item
      )

    result.add(quote do:
      {.push hint[ConvFromXtoItselfNotNeeded]: off.}

      proc `listLen`*(`id`: EcsIdentity): Natural {.compileTime.} =
        `key`.len

      proc `listAppend`*(`id`: EcsIdentity, `value`: `itemType`) {.compileTime.} =
        `key`.add `writeNode`

      proc `listGet`*(`id`: EcsIdentity): seq[`itemType`] {.compileTime.} =
        let
          key = `key`
          listLen = key.len
        var res: seq[`itemType`]
        if listLen > 0:
          res.setLen listLen
          var i: int
          for `item` in key.`csItems`:
            res[i] = `readNode`
            i.inc
        res
      
      {.pop.}
    )

#-------------------------------
# ECS database access generation
#-------------------------------

# Component properties
genItemStates(ComponentTypeId, string, ["instanceType", "refType", "initPrefix", "refInitPrefix"])
genItemStates(ComponentTypeId, bool, ["isOwned"])
genItemStates(ComponentTypeId, SystemIndex, ["systemOwner"])
genListStates(ComponentTypeId, SystemIndex, ["systems", "dependentOwners", "linked"])
genListStates(ComponentTypeId, ComponentTypeId, ["dependentComps"])

# Raw component type definition AST.
genListStates(ComponentTypeId, NimNode, ["componentDefinitions"])

# Component options
genItemStates(ComponentTypeId, Natural, ["maxComponents"])
genItemStates(ComponentTypeId, EcsCompItemStorage, ["componentStorageFormat"])
genItemStates(ComponentTypeId, EcsAccessMethod, ["accessMethod"])
genItemStates(ComponentTypeId, EcsCompRecyclerFormat, ["recyclerFormat"])
genItemStates(ComponentTypeId, bool, ["clearAfterDelete", "useThreadVar"])
genItemStates(ComponentTypeId, EcsCompInvalidAccess, ["invalidAccess"])

# System properties
genItemStates(SystemIndex, bool, ["sealed", "useThreadVar", "bodyDefined"])
genItemStates(SystemIndex, NimNode, ["instantiation", "definition", "extraFields"])
genListStates(SystemIndex, ComponentTypeId, ["ecsSysRequirements", "ecsOwnedComponents", "ecsSysNegations"])

# System groups
genListStates(string, SystemIndex, ["groupSystems"])  # Group to list of systems.
genListStates(SystemIndex, string, ["systemGroups"])  # System to list of groups.
# System options
genItemStates(SystemIndex, int, ["maxEntities"])
genItemStates(SystemIndex, ECSSysStorage, ["storageFormat"])
genItemStates(SystemIndex, ECSSysIndexFormat, ["indexFormat"])
genItemStates(SystemIndex, ECSSysTimings, ["timings"])
genItemStates(SystemIndex, ECSSysEcho, ["echoRunning"])
genItemStates(SystemIndex, bool, ["assertItem"])
genItemStates(SystemIndex, bool, ["orderedRemove"])
genItemStates(SystemIndex, ECSSysThreading, ["threading"])

# Source locations
genItemStates(SystemIndex, string, ["ecsSystemSourceLoc", "ecsSystemBodySourceLoc"])
genGlobalStates(string, ["ecsMakeEcsSourceLoc"])

# Current build info
genGlobalListStates(ComponentTypeId, [
  # Current range of component id's that have yet to be sealed.
  "ecsComponentsToBeSealed",
  # Currently sealed components: it's an error to try to create systems with these
  # as their component state operations have already been generated.
  "ecsSealedComponents"])

genGlobalListStates(string, ["codeLog"])
genGlobalStates(int, ["codeLogStart", "ecsSysIterating"])

genGlobalListStates(SystemIndex, [
  "ecsSysDefined",  # Stores a list of defined systems.
  "systemOrder"]    # Stores the order of systems for `commitSystems`.
  )

# State tracking for system execution.
genGlobalStates(bool, [
  "inSystem", "inSystemAll", "inSystemStream", "inSystemDeleteRow",
  "sysRemoveAffectedThisSystem", "systemCalledDelete",
  "logInitialised"  # Perform one log clear per unique path.
  ])

# Use the NimNode as a seq[int] as a mutable seq[SystemIndex].
genGlobalStates(NimNode, ["uncommittedSystems"])

# Which system is responsible for the current code.
genGlobalStates(SystemIndex, ["inSystemIndex"])
# Record component accesses within systems.
genListStates(SystemIndex, ComponentTypeId, ["readsFrom", "writesTo"])

#----------------------
# Events for components
#----------------------

genListStates(ComponentTypeId, NimNode, [
  "onInitCode", "onFinalisationCode", "onAddToEntCode",
  "onRemoveFromEntCode", "onAddCallback", "onRemoveCallback",
  "onAddAnySystemCode", "onRemoveAnySystemCode",
  "onInterceptUpdate", "onAddCallbackForwardDecl",
  "onRemoveCallbackForwardDecl"])

#-------------------
# Events for systems
#-------------------

genListStates(SystemIndex, NimNode, [
  "onAdded", "onRemoved",
  "onAddedCallback", "onRemovedCallback"
  ])

# Events for systems with particular components.
genLookupListStates(SystemIndex, ComponentTypeId, NimNode, ["onAddToCode", "onRemoveFromCode"])

# Record which components want to trigger an event on being added or
# removed from a system.
# This is used to check you aren't adding/removing a component
# 'onAddTo'/'onRemoveFrom' event for systems that don't use the
# component.
genListStates(SystemIndex, ComponentTypeId, ["onAddToSystemComp", "onRemoveFromSystemComp"])

#--------------
# Global events
#--------------

genGlobalStates(NimNode, ["onEntityStateChange"])
genGlobalStates(bool, ["private"])
genGlobalStates(NimNode, ["ecsCurrentOperation", "onEcsBuiltCode"])

#--------------------
# Root objects <-> DB
#--------------------

type FlatIdentDef* = tuple[path: seq[string], identDef: NimNode]

iterator flatten*(objTy: NimNode): FlatIdentDef =
  ## Iterate over identDefs for fields in the object type `objTy`
  ## including nested object fields (breadth first).
  ## `objTy` expects a symbol for an object type.

  objTy.expectKind nnkSym
  
  let
    objName = objTy.strVal
    impl = objTy.getType
  impl.expectKind nnkBracketExpr
  impl.expectLen 2

  let symImpl = impl[1].getTypeImpl
  symImpl.expectKind nnkObjectTy

  # Note: the top level of the path uses the type name rather than
  # the field name.
  var queue = @[(@[objName], symImpl)]
  
  while queue.len > 0:
    let
      item = queue.pop
      path = item[0]
      ty = item[1]

    # Process fieldDefs for this object type.
    for fieldDef in ty[2]:
      fieldDef.expectKind nnkIdentDefs

      let
        fieldNameStr = fieldDef[0].strVal
        fieldTyImpl = fieldDef[1].getTypeImpl
      
      if fieldTyImpl.kind == nnkObjectTy:
        # Add a nested object type for processing.
        queue.add (path & fieldNameStr, fieldTyImpl)
      else:
        yield (path, fieldDef)

macro genObjAccessor(accessName: untyped, storageType: typedesc): untyped =
  ## Generates CacheSeq access procs for each field within
  ## `storageType` and a helper to get or set the `storageType` in one
  ## operation.
  ## 
  ## Note that nested object types within `storageType` are flattened
  ## in macrocache storage. As such, deeply nested types with duplicate
  ## names may clash.
  ## 
  ## In this example MyType.name will clash with MyType.otherName.name.
  ## 
  ##    type
  ##      MyName = object
  ##        name: string
  ##    
  ##      MyType = object
  ##        name: string
  ##        otherName: MyName
  ##    
  ##    genObjAccessor("myType", MyType)
  ## 
  ## Therefore, keep names unique and avoid nesting.
  ## 
  if accessName.kind notin [nnkIdent, nnkStrLit]:
    error "Expected an ident or string literal for accessName"
  
  let
    setName = ident "set" & accessName.strVal
    getName = ident accessName.strVal
    id = ident "id"
    value = ident "value"

  result = newStmtList()

  var
    setInner = newStmtList()
    getInner = newStmtList()
    accessors = newStmtList()
    objAccess = newStmtList()

  # Step through IdentDefs of the storageType.
  # The lineage of the type is returned in field.path.
  for field in storageType.flatten:
    let
      fieldName = field.identDef[0].strVal
      fieldType = ident field.identDef[1].strVal

    accessors.add(quote do:
      genGlobalStates(`fieldType`, [`fieldName`])
    )

    let
      setField = ident "set" & fieldName
      getField = ident fieldName
    var
      setPath = value
      getPath = ident "result"

    for i in 1 ..< field.path.len:
      setPath = newDotExpr(setPath, ident field.path[i])
      getPath = newDotExpr(getPath, ident field.path[i])

    setInner.add(quote do:
      `id`.`setField`(`setPath`.`getField`)
    )
    getInner.add(quote do:
      `getPath`.`getField` = `id`.`getField`
    )

  objAccess.add(quote do:
    proc `setName`*(`id`: EcsIdentity, `value`: `storageType`) =
      `setInner`
    proc `getName`*(`id`: EcsIdentity): `storageType` =
      `getInner`
  )
  result.add(quote do:
    `accessors`
    `objAccess`
  )

genObjAccessor(entityOptions, EcsEntityOptions)

#------------------------------------------------
# Component type identifier storage and retrieval
#------------------------------------------------

proc typeName*(id: EcsIdentity, compId: ComponentTypeId): string =
  id.checkId compId
  id.components[compId.int].strVal

proc findCompId*(id: EcsIdentity, typeName: string): ComponentTypeId {.compileTime.} =
  ## Find the ComponentTypeId from a type name.
  ## Note: assumes ascii type identifiers.
  let fTypeName = toUpperAscii(typeName)
  let key = id.components

  var i: int
  for ty in key:
    ty.expectKind nnkStrLit

    if ty.strVal.toUpperAscii == fTypeName:
      return ComponentTypeId(i)
    i += 1
  
  InvalidComponent

proc addTypeId*(id: EcsIdentity, typeName: string): ComponentTypeId {.compileTime, discardable.} =
  ## Add a new component type and return its ComponentTypeId.
  ## Type must not already be registered.
  # This is defined below the db setup so we can use access procs here.
  
  if id.findCompId(typeName) != InvalidComponent:
    error "Component type \"" & typeName & "\" is already registered"

  let
    compsSeq = id.components
    curLen = compsSeq.len

  # The index of the CacheSeq is the ComponentTypeId.
  result = curLen.ComponentTypeId

  # Add name entry.
  compsSeq.add newLit(typeName)

proc findOrAddTypeId*(id: EcsIdentity, typeName: string): ComponentTypeId {.compileTime.} =
  ## Returns the ComponentTypeId for `typeName`. If the type is not
  ## registered, it is added and the new ComponentTypeId returned.
  result = id.findCompId(typeName)
  if result == InvalidComponent:
    result = id.addTypeId(typeName)

iterator allComponents*(id: EcsIdentity): tuple[id: ComponentTypeId, name: string] =
  let compLen = id.components.len
  if compLen > 1:
    for i in 1 ..< compLen:
      yield (i.ComponentTypeId, id.components[i].strVal)

proc allComponentsSeq*(id: EcsIdentity): seq[ComponentTypeId] =
  let compLen = id.components.len
  if compLen > 1:
    for i in 1 ..< compLen:
      result.add i.ComponentTypeId

proc allComponentNames*(id: EcsIdentity): seq[string] =
  for item in id.allComponents:
    result.add item.name

proc len_components*(id: EcsIdentity): int =
  result = id.components.len
  assert result >= 0, "EcsIdentity is not initialised"

proc typeIdRange*(id: EcsIdentity): Slice[ComponentTypeId] =
  1.ComponentTypeId ..< id.len_components().ComponentTypeId

#------------------
# System management
#------------------

proc getSystemName*(id: EcsIdentity, sysIdx: SystemIndex): string {.compileTime.} =
  ## Get the name of the system indexed by `sysIdx`.
  # This isn't called "systemName" because it clashes with the
  # `systemName` field inside a system instance.
  id.checkId sysIdx
  id.systemsKey[sysIdx.int].strVal

proc findSysIdx*(id: EcsIdentity, name: string): SystemIndex {.compileTime.} =
  ## Find a SystemIndex matching `name`.
  let
    key = id.systemsKey
    sysName = name.toUpperAscii

  var i: int
  for item in key:
    if item.strVal.toUpperAscii == sysName:
      return i.SystemIndex
    i += 1

  InvalidSystemIndex

proc addSystem*(id: EcsIdentity, name: string): SystemIndex {.discardable, compileTime.} =
  ## Create a new system.
  result = InvalidSystemIndex

  if id.findSysIdx(name) != InvalidSystemIndex:
    error "System \"" & name & "\" is already registered"

  let key = id.systemsKey
  result = key.len.SystemIndex

  # Add name entry. The index is the SystemIndex.
  key.add newLit(name)

iterator allSystems*(id: EcsIdentity): tuple[id: SystemIndex, name: string] =
  let sysLen = id.systemsKey.len
  if sysLen > 1:
    for i in 1 ..< sysLen:
      yield (i.SystemIndex, id.systemsKey[i].strVal)

proc allSystemsSeq*(id: EcsIdentity): seq[SystemIndex] =
  for sys in id.allSystems:
    result.add sys.id

proc allSystemNames*(id: EcsIdentity): seq[string] =
  for item in id.allSystems:
    result.add item.name

proc len_systems*(id: EcsIdentity): int =
  result = id.systemsKey.len
  assert result >= 0, "EcsIdentity is not initialised"

# -------------------------
# State tracking for events
# -------------------------

genGlobalStates(NimNode, ["ecsEventEnv", "ecsEventMutations"])       # Stores a list of MutationState.

#-----------------------------
# Set up an ECS build identity
#-----------------------------

proc newEcsIdentity*(name: static[string]): EcsIdentity {.compileTime.} =
  result = EcsIdentity(name)

  let key = CacheSeq(name)
  if key.len > 0:
    error "The identity \"" & name & "\" is already defined"

  # Store ident of the name for now.
  key.add ident(name)

  # Both components and systems are 1-indexed.
  discard result.addTypeId "<InvalidComponent>"
  discard result.addSystem "<InvalidSystem>"

const defaultIdentity* = newEcsIdentity("default")

#--------------------------
# Object get/set operations
#--------------------------

proc setOptions*(id: EcsIdentity, compId: ComponentTypeId, opts: ECSCompOptions) =
  id.set_maxComponents(compId, opts.maxComponents)
  id.set_componentStorageFormat(compId, opts.componentStorageFormat)
  id.set_accessMethod(compId, opts.accessMethod)
  id.set_recyclerFormat(compId, opts.recyclerFormat)
  id.set_clearAfterDelete(compId, opts.clearAfterDelete)
  id.set_useThreadVar(compId, opts.useThreadVar)
  id.set_invalidAccess(compId, opts.invalidAccess)

proc getOptions*(id: EcsIdentity, compId: ComponentTypeId): ECSCompOptions =
  #id.set_maxEntities(compId, opts.maxEntities)
  result.maxComponents = id.maxComponents(compId)
  result.componentStorageFormat = id.componentStorageFormat(compId)
  result.accessMethod = id.accessMethod(compId)
  result.recyclerFormat = id.recyclerFormat(compId)
  result.clearAfterDelete = id.clearAfterDelete(compId, )
  result.useThreadVar = id.useThreadVar(compId)
  result.invalidAccess = id.invalidAccess(compId)

proc setOptions*(id: EcsIdentity, sysId: SystemIndex, opts: ECSSysOptions) =
  id.set_maxEntities(sysId, opts.maxEntities)
  id.set_storageFormat(sysId, opts.storageFormat)
  id.set_indexFormat(sysId, opts.indexFormat)
  id.set_timings(sysId, opts.timings)
  id.set_useThreadVar(sysId, opts.useThreadVar)
  id.set_echoRunning(sysId, opts.echoRunning)
  id.set_assertItem(sysId, opts.assertItem)
  id.set_orderedRemove(sysId, opts.orderedRemove)
  id.set_threading(sysId, opts.threading)

proc getOptions*(id: EcsIdentity, sysId: SystemIndex): ECSSysOptions =
  result.maxEntities = id.maxEntities(sysId)
  result.storageFormat = id.storageFormat(sysId)
  result.indexFormat = id.indexFormat(sysId)
  result.timings = id.timings(sysId)
  result.useThreadVar = id.useThreadVar(sysId)
  result.echoRunning = id.echoRunning(sysId)
  result.assertItem = id.assertItem(sysId)
  result.orderedRemove = id.orderedRemove(sysId)
  result.threading = id.threading(sysId)
