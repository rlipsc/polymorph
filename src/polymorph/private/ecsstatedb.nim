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

import macrocache, macros, ../sharedtypes, strutils, identities
export identities

when (not isMainModule):
  import ecsstatedbaccess
  export ecsstatedbaccess
else:
  import ecsstatedbgen

  # Component properties
  genItemStates(ComponentTypeId, string, ["ecsInstanceType", "refType", "initPrefix", "refInitPrefix"])
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
  genItemStates(ComponentTypeId, ECSCompEventTemplates, ["eventTemplates"])

  # System properties
  genItemStates(SystemIndex, bool, ["sealed", "useThreadVar", "bodyDefined"])
  genItemStates(SystemIndex, NimNode, ["instantiation", "ecsSysBodyDefinition", "extraFields", "ecsSysComponentAliases", "ecsSystemBody"])
  genListStates(SystemIndex, ComponentTypeId, ["ecsSysRequirements", "ecsOwnedComponents", "ecsSysNegations"])
  genItemStates(SystemIndex, NimNode, ["ecsDeferredSysDef", "onEcsCommitSystemCode"])

  # System groups
  genListStates(string, SystemIndex, ["groupSystems"])      # Group to list of systems.
  genListStates(SystemIndex, string, ["systemGroups"])      # System to list of groups.
  genListStates(string, NimNode, ["onEcsCommitGroupCode"])

  # System options
  genItemStates(SystemIndex, int, ["maxEntities"])
  genItemStates(SystemIndex, ECSSysStorage, ["storageFormat"])
  genItemStates(SystemIndex, ECSSysIndexFormat, ["indexFormat"])
  genItemStates(SystemIndex, ECSSysTimings, ["timings"])
  genItemStates(SystemIndex, ECSSysEcho, ["echoRunning"])
  genItemStates(SystemIndex, bool, ["assertItem", "orderedRemove"])
  genItemStates(SystemIndex, ECSSysThreading, ["threading"])
  genItemStates(SystemIndex, ECSSysDefCommit, ["ecsSysCommitInstance"])
  genItemStates(SystemIndex, ECSSysItemTemplates, ["ecsSysItemTemplates"])

  # Source locations
  genItemStates(SystemIndex, string, ["ecsSystemSourceLoc", "ecsSystemBodySourceLoc"])
  genGlobalStates(string, ["ecsMakeEcsSourceLoc", "ecsSystemDeleteLoc", "ecsSystemRemoveLoc"])

  # Current build info
  genGlobalListStates(ComponentTypeId, [
    # Current range of component id's that have yet to be sealed.
    "ecsComponentsToBeSealed",
    # Currently sealed components: it's an error to try to create systems with these
    # as their component state operations have already been generated.
    "ecsSealedComponents"])

  genGlobalListStates(string, ["ecsCodeLog"])
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

  #------------------------
  # Global events and state
  #------------------------

  genGlobalStates(bool, ["private"])
  genGlobalStates(string, ["ecsDefineSystemsAsGroup", "ecsCodeLogFilename"])
  genGlobalStates(NimNode,
    [
      "ecsCurrentOperation",

      "onEntityStateChange",
      "onEcsBuildingCode",
      "onEcsBuiltCode",
      "onEcsCommitAllCode",
      "onEcsNextCommitCode",
      "onEcsNextGroupCommitCode",
      "ecsMakeEcsImports",
      "ecsCommitImports",
      "ecsMakeEcsImportFrom",
      "ecsCommitImportFrom",
    ]
  )
  
  genObjAccessor(entityOptions, EcsEntityOptions)

  # State tracking for events.
  genGlobalStates(NimNode, ["ecsEventEnv", "ecsEventMutations"])

  outputGeneratedState("ecsstatedbaccess")


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

iterator allComponents*(id: EcsIdentity): tuple[id: ComponentTypeId, name: string] =
  let compLen = id.components.len
  if compLen > 1:
    for i in 1 ..< compLen:
      yield (i.ComponentTypeId, id.components[i].strVal)

proc allComponentsSeq*(id: EcsIdentity): seq[ComponentTypeId] =
  let compLen = id.components.len
  if compLen > 1:
    for item in id.allComponents:
      result.add item.id

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
  id.set_ecsSysCommitInstance(sysId, opts.commit)
  id.set_ecsSysItemTemplates(sysId, opts.itemTemplates)

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
  result.commit = id.ecsSysCommitInstance(sysId)
  result.itemTemplates = id.ecsSysItemTemplates(sysId)
