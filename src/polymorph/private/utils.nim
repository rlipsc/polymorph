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

## This module covers shared internal tools and utilities used to build
## the ECS code.
## 
## *This module is not exported to the user*.
## 
## Functionality:
## 
## - Procs used to generate consistent field and type names.
## - Structural generation through various levels of abstraction.
## - Building user event calls.
## - Compile time utilities for inspecting and building types.
## - Walking system ownership graphs to find dependencies.

import macros, strutils, typetraits, ../sharedtypes, ecsstatedb
import debugging, tables, deques, sequtils, sets, macrocache
export debugging


proc findSystemIndex*(id: EcsIdentity, name: string): tuple[found: bool, index: SystemIndex] {.compileTime.} =
  result[1] = id.findSysIdx(name)
  result[0] = result[1] != InvalidSystemIndex

const
  # This is the postfix for both the instantiated storage variable
  # and typename created for component storage.
  storageName* = "componentStorage"
  invalidComponentStr* = "<Invalid Component>"

type
  TypeFields* = tuple[typeName: string, fields: seq[tuple[fieldNode, typeNode: NimNode]]]
  
  SystemBlockKind* = enum
    sbkFields, sbkInit, sbkStart, sbkAll, sbkStream, sbkFinish,
    sbkAdded, sbkRemoved, sbkAddedCallback, sbkRemovedCallback
  
  ComponentSet* = HashSet[ComponentTypeId]
  SystemSet* = HashSet[SystemIndex]

  ComponentIterable* = seq[ComponentTypeId] | ComponentSet | openarray[ComponentTypeId]
  SystemIterable* = seq[SystemIndex] | SystemSet

proc newComponentSet*: auto = initHashSet[ComponentTypeId]()
proc newSystemSet*: auto = initHashSet[SystemIndex]()

# ----------------
# String utilities
# ----------------


proc unCap*(text: var string) =
  if text.len > 0:
    text[0] = text[0].toLowerAscii

proc unCap*(text: string): string =
  result = text
  result.unCap


# -------------
# Set utilities
# -------------


proc incl*[T](setItems: var HashSet[T], items: seq[T]) =
  for item in items:
    setItems.incl item


# ----------------------------------
# Type names derived from user types
# ----------------------------------


proc instanceTypeName*(tyName: string): string =
  ## Instance distinct type name
  tyName & "Instance"

proc generationTypeName*(tyName: string): string =
  ## Instantiation instance distinct type
  tyName & "Generation"

proc refTypeName*(tyName: string): string =
  ## Reference container type name
  tyName & "Ref"


# ----------------------------------
# Initialisation/deleting components
# ----------------------------------


proc instanceInitName*(prefix, s: string): string =
  ## Initialiser proc name for instance of type, takes arguments to set fields
  prefix & s

proc createInstanceName*(s: string): string =
  ## Allocate function for a component slot for this type
  "gen" & s

proc deleteInstanceName*: string =
  ## Clear function for component slot for this type
  "delete"


# -------------------
# Storage field names
# -------------------


proc entityStorageTypeName*: string =
  ## Type name for entity storage
  "EntityStorage"
proc entityStorageItemTypeName*: string = "EntityComponentItem"

proc entityStorageVarName*: string =
  ## This is the name of the storage variable generated from a particular prefix.
  "entityStorage"
proc initEntityStorageTypeName*: string = "initEntityStorage"
proc entityStorageContainerTypeName*: string = "EntityStorageItems"

proc recyclerArrayLen*: string = "recycleLen"


proc storageFieldName*(typeName: string): string =
  ## Name of the storage field for this component type
  "storage" & typeName

proc instanceIdsName*(typeName: string): string =
  ## Name of the instance ids by component index for this type
  typeName.toLowerAscii & "InstanceIds"

proc aliveStateInstanceName*(typeName: string): string =
  ## Name of the array of alive state by component index for this type
  typeName.toLowerAscii() & "Alive"

proc freeInstancesName*(typeName: string): string =
  ## Name of the seq for free instances indexes for this type
  typeName.toLowerAscii() & "FreeIndexes"

proc nextInstanceName*(typeName: string): string =
  ## Next instance slot for this type
  typeName.toLowerAscii() & "NextIndex"


# ------------
# System names
# ------------


proc systemTypeName*(name: string): string = name.capitalizeAscii() & "System"
proc systemInitName*(name: string): string = "init" & name.capitalizeAscii() & "System"

proc systemAddedCBName*(name: string): string = name & "AddedCallback"
proc systemRemovedCBName*(name: string): string = name & "RemovedCallback"

const
  sysVarPrefix = "sys"
  sysItemNamePrefix = "SysItem"  # capitalisation for type
  doProcPrefix = "do"
  instPostfix* = ""

func itemTypeName*(name: string): string = sysItemNamePrefix & name.capitalizeAscii
func doProcName*(name: string): string = doProcPrefix & name.capitalizeAscii
func systemVarName*(name: string): string = sysVarPrefix & name.capitalizeAscii
func addCallbackName*(name: string): string = "addCallback" & name
func removeCallbackName*(name: string): string = "removeCallback" & name
func systemStr*(name: string): string = name & " (" & systemVarName(name) & ")"

# ------------------------------------------
# Type classes that cover component types
# and system types.
# These are useful for parameter constraints
# ------------------------------------------

proc typeClassName*: string =
  ## The name of the type class that covers all component types.
  "ComponentTypeClass"

proc refTypeClassName*: string =
  ## The name of the type class that covers all the ref types.
  "ComponentRefTypeClass"

proc instanceTypeClassName*: string =
  ## The name of the type class that covers all the distinct int types.
  "ComponentIndexTypeClass"

proc systemsTypeClassName*: string =
  "SystemsTypeClass"

# ------------------
# State DB utilities
# ------------------


type
  ComponentBuildInfo* = object
    typeId*: ComponentTypeId
    name*: string
    lcName*: string
    instanceTy*: NimNode
    isOwned*: bool
    owner*: SystemIndex

  SystemBuildInfo* = object
    index*: SystemIndex
    name*: string
    lcName*: string
    variable*: NimNode
    isOwner*: bool


proc getInfo*(id: EcsIdentity, typeId: ComponentTypeId): ComponentBuildInfo =
  let
    tyName = id.typeName(typeId)
    tyOwner = id.systemOwner(typeId)
  
  ComponentBuildInfo(
    typeId: typeId,
    name: tyName,
    lcName: tyName.unCap,
    instanceTy: ident instanceTypeName(tyName),
    owner: tyOwner,
    isOwned: tyOwner != InvalidSystemIndex,
  )

proc getInfo*(id: EcsIdentity, index: SystemIndex): SystemBuildInfo =
  let
    name = id.getSystemName(index)
  
  SystemBuildInfo(
    index: index,
    name: name,
    lcName: name.unCap,
    variable: ident systemVarName(name),
    isOwner: id.len_ecsOwnedComponents(index) > 0
  )

iterator building*(id: EcsIdentity, components: ComponentIterable): auto =
  ## Iterates through `components`, outputting standardised idents for use
  ## in building operations.
  for typeId in components:
    yield id.getInfo typeId


iterator building*(id: EcsIdentity, systems: SystemIterable): auto =
  ## Iterates through `systems`, outputting standardised idents for use
  ## in building operations.
  for sys in systems:
    yield id.getInfo sys


# Functions for synchronising ident names.

func fetchedName*(sysInfo: SystemBuildInfo, suffix: string): string =
  ## Coordinate fetched ident use in generated code.
  sysInfo.lcName & suffix

func fetchedName*(typeInfo: ComponentBuildInfo, suffix: string): string =
  ## Coordinate fetched ident use in generated code.
  typeInfo.lcName & suffix

proc fetchedIdent*(sysInfo: SystemBuildInfo, suffix: string): NimNode =
  ## Coordinate fetched ident use in generated code.
  ident "sysFetch" & sysInfo.fetchedName(suffix)

proc fetchedIdent*(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
  ## Coordinate fetched ident use in generated code.
  ident typeInfo.fetchedName(suffix)

proc curComponent*(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
  ## Coordinate fetched ident use in generated code.
  ident "curComp" & suffix

type
  ComponentAccessProc* = proc(typeInfo: ComponentBuildInfo, suffix: string): NimNode
  ComponentValidProc* =  proc(typeInfo: ComponentBuildInfo, suffix: string): NimNode

proc byFetchedIdent*(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
  typeInfo.fetchedIdent suffix

proc checkInstanceValid*(typeInfo: ComponentBuildInfo, suffix: string): NimNode =
  typeInfo.byFetchedIdent(suffix).newDotExpr ident"valid"

# Utilities for processing work from the macrocache.

iterator unsealedComponents*(id: EcsIdentity): ComponentTypeId =
  let sealedComponents = id.ecsSealedComponents
  for typeId in id.ecsComponentsToBeSealed:
    if typeId notin sealedComponents:
      yield typeId

proc unsealedComponentCount*(id: EcsIdentity): int =
  for typeId in id.unsealedComponents:
    result += 1

proc allUnsealedComponents*(id: EcsIdentity): seq[ComponentTypeId] =
  for typeId in id.unsealedComponents:
    result.add typeId

iterator unsealedSystems*(id: EcsIdentity): SystemIndex =
  ## Returns system indexes in the order of commit.
  let definedSystems = id.ecsSysDefined
  for sysId in definedSystems:
    if not id.sealed(sysId):
      yield sysId

proc allUnsealedSystems*(id: EcsIdentity): seq[SystemIndex] =
  for sysId in id.unsealedSystems:
    result.add sysId

proc unsealedSystemCount*(id: EcsIdentity): int =
  for sysId in id.unsealedSystems:
    result += 1

proc getUncommitted*(id: EcsIdentity): seq[SystemIndex] =
  ## Return a list of systems with bodies waiting to be committed.
  var
    curSystems = id.uncommittedSystems()

  result = newSeqOfCap[SystemIndex](curSystems.len)
  
  for sysInt in curSystems:
    result.add sysInt.intVal.SystemIndex

proc setUncommitted*(id: EcsIdentity, value: seq[SystemIndex]) =
  ## Update uncommittedSystems with a list of systems.
  var n = newStmtList()

  for sys in value:
    n.add newLit(sys.int)
  
  id.set_uncommittedSystems n

proc addUncommitted*(id: EcsIdentity, sys: SystemIndex) =
  ## Append to existing systems in uncommittedSystems.
  var
    curSystems = id.getUncommitted()

  curSystems.add sys
  id.setUncommitted curSystems


# --------------------
# Generating set types
# --------------------


proc componentsEnumName*: string =
  ## Name of the enum type used for sets of components.
  "ComponentsEnum"


proc systemsEnumName*: string =
  ## Name of the enum type used for sets of SystemIndex.
  "SystemsEnum"


func compEnum*(name: string): string =
  ## Return the system's enum item from its name.
  "ce" & name.capitalizeAscii


func sysEnum*(name: string): string =
  ## Return the system's enum item from its name.
  "se" & name.capitalizeAscii


proc genComponentSet*(id: EcsIdentity): NimNode =
  ## Generate an enum that covers all of the components seen so far.
  var items = nnkEnumTy.newTree()
  items.add newEmptyNode()
  items.add(ident "ceInvalid")
  
  for c in id.building(id.allUnsealedComponents):
    items.add(
      nnkEnumFieldDef.newTree(
        ident c.name.compEnum,
        newIntLitNode(c.typeId.int)
      )
    )
  let eName = ident componentsEnumName()

  result = quote do:
    type `eName`* {.used.} = `items`


proc genSystemSet*(id: EcsIdentity): NimNode =
  ## Generate an enum that covers all of the systems in this identity.
  var items = nnkEnumTy.newTree()
  items.add newEmptyNode()

  # Adding 'invalid' lets us create the set when no systems are defined.
  items.add(nnkEnumFieldDef.newTree(
    ident sysEnum("InvalidSystem"),
    newIntLitNode(0)
    )
  )
  
  for index in id.unsealedSystems:
    items.add(
      nnkEnumFieldDef.newTree(
        ident sysEnum(id.getSystemName(index).capitalizeAscii),
        newIntLitNode(index.int)
      )
    )
  let eName = ident systemsEnumName()

  result = quote do:
    type `eName`* {.used.} = `items`


# -----------------------
# Event mutation tracking
# -----------------------


type
  ParamKind* = enum pkNone, pkCompCT, pkComp, pkSys, pkSysComp


func getParamKind*(eventKind: EventKind): ParamKind =
  case eventKind
    of ekNoEvent .. ekDeleteEnt:
      pkNone
    of ekNewEntityWith .. ekRemoveComponents:
      pkCompCT
    of ekInit .. ekDeleteComp:
      pkComp
    of ekSystemAddAny .. ekSystemRemoveAny, ekRowAdded .. ekRowRemovedCb:
      pkSys
    of ekCompAddTo, ekCompRemoveFrom:
      pkSysComp


func getEventDir*(eventKind: EventKind): EventKind =
  ## Return the direction of an event, if any.

  case eventKind
    
    of ekAddComponents, ekAdd, ekAddCB, ekInit, ekSystemAddAny, ekCompAddTo, ekRowAdded, ekRowAddedCB:
      ekAddComponents

    of ekRemoveComponents, ekRemove, ekRemoveCB, ekDeleteComp, ekSystemRemoveAny, ekCompRemoveFrom, ekRowRemoved, ekRowRemovedCB:
      ekRemoveComponents
    
    else:
      ekNoEvent


type
  # EventState represents the current static event expansion or
  # a mutation to an entity.
  EventState* = object
    event*: EventKind
    indexes*: seq[int]

  ComponentMutation* = object
    mutation*: EventState
    trace*: seq[EventState]


proc initEventState(event: EventKind, indexes: openarray[int]): EventState =
  result.event = event
  result.indexes.setLen indexes.len
  for i, idx in indexes:
    result.indexes[i] = idx


proc toNode(event: EventKind, indexes: openarray[int]): NimNode =
  result = nnkBracket.newTree
  result.add newLit event.int

  for idx in indexes:
    result.add newLit idx


proc toNode(eventState: EventState): NimNode =
  result = nnkBracket.newTree
  result.add newLit eventState.event.int

  for idx in eventState.indexes:
    result.add newLit idx


proc initMutation(id: EcsIdentity, mutation: EventState): NimNode =
  result = nnkBracket.newTree
  result.add mutation.toNode
  result.add nnkBracket.newTree()
  for node in id.ecsEventEnv:
    result[1].add node.copy


proc toEventState*(node: NimNode): EventState =
  node.expectKind nnkBracket
  node.expectMinLen 2
  result.event = EventKind(node[0].intVal)

  for n in node[1 .. ^1]:
    result.indexes.add n.intVal.int


proc toMutation*(node: NimNode): ComponentMutation =
  node.expectKind nnkBracket
  node.expectMinLen 2

  result.mutation = node[0].toEventState

  result.trace.setLen node[1].len
  for i in 0 ..< node[1].len:
    result.trace[i] = node[1][i].toEventState


proc toStr(es: EventState, id: EcsIdentity): string =

  result = $es.event & " "

  let
    pk = es.event.getParamKind

  case pk
    of pkNone:
      discard

    of pkCompCT, pkComp:
      if es.indexes.len > 0:
        result &= id.typeName(es.indexes[0].ComponentTypeId)
        
        for v in es.indexes[1 .. ^1]:
          result &= ", " & id.typeName(v.ComponentTypeId)
    
    of pkSys:
      if es.indexes.len > 0:
        result &= id.getSystemName(es.indexes[0].SystemIndex).systemVarName
        
        for v in es.indexes[1 ..^ 1]:
          result &= ", " & id.getSystemName(v.SystemIndex).systemVarName

    of pkSysComp:
      if es.indexes.len > 0:
        result &= id.getSystemName(es.indexes[0].SystemIndex).systemVarName
        
        if es.indexes.len > 1:
          result &= " and "
          result &= id.typeName(es.indexes[1].ComponentTypeId)
          for v in es.indexes[2 ..^ 1]:
            result &= ", " & id.typeName(v.ComponentTypeId)


proc toStr(m: ComponentMutation, id: EcsIdentity): string =
  result = m.mutation.toStr(id) & " [ trace: "
  
  result &= m.trace[0].toStr(id)
  for i in 1 ..< m.trace.len:
    result &= " -> " & m.trace[i].toStr(id)
  
  result &= " ]"


proc toStr(v: NimNode, id: EcsIdentity, op: proc, title: string, indent = 0): string =
  if v.len > 0:
    let
      ind = "  "
      indentStr = ind.repeat(indent)

    if title.len > 0:
      result = indentStr & title & "\n"
    
    result &= indentStr & op(v[0]).toStr(id)

    for i in 1 ..< v.len:
      result &= "\n" & indentStr & op(v[i]).toStr(id)


proc eventsStr*(id: EcsIdentity, indent = 0): string =
  result = id.ecsEventEnv.toStr(id, toEventState, "Event:", indent)


proc mutationsStr*(id: EcsIdentity, indent = 0): string =
  id.ecsEventMutations.toStr(id, toMutation, "Mutations:", indent)


proc eventMutationsStr*(id: EcsIdentity, indent = 0): string =
  result = id.eventsStr(indent)
  if result.len > 0:
    result &= "\n\n"
  result &= id.mutationsStr(indent)


proc enterEvent(id: EcsIdentity, event: EventKind, indexes: openarray[int]) =
  ## Record entering this event in the static environment.
  var
    curEvents = id.ecsEventEnv
    evNode = toNode(event, indexes)

  if curEvents.len == 0 and curEvents.kind != nnkBracket:
    curEvents = nnkBracket.newTree()
  curEvents.add evNode

  id.set_ecsEventEnv curEvents


proc exitEvent(id: EcsIdentity, event: EventKind, indexes: varargs[int]) =
  ## Record leaving this event in the static environment.
  var
    curEnv = id.ecsEventEnv

  let
    idxSeq = indexes.toSeq

  if curEnv.len > 0:
    block findEntry:
      # Find and remove the last matching event.

      for i in countDown(curEnv.len - 1, 0):
        let
          es = curEnv[i].toEventState
        
        if es.event == event and idxSeq == es.indexes:
          curEnv.del i

          break findEntry
      
      error "Internal error: couldn't find record of " &
        toStr(initEventState(event, indexes), id) & " when exiting event"

    id.set_ecsEventEnv curEnv

  if id.ecsEventEnv.len == 0:

    when defined(ecsLogDetails):
      if id.ecsEventMutations.len > 0:
        id.debugMessage $event & "\n" & id.mutationsStr(id.ecsCurrentOperation.len + 2)

    id.set_ecsEventMutations nnkBracket.newTree()


proc contains*(find, test: openarray[int]): bool =
  ## `in` operator for int lists.
  ## Returns `true` when all items in `find` are in `test`.
  for v in test:
    if v notin find:
      return false
  return true


proc hasAny*(find, test: openarray[int]): bool =
  ## Returns `true` when any items in `find` are in `test`.
  for v in find:
    if v in test:
      return true
  return false


proc mutationOccurred*(id: EcsIdentity, mutations: set[EventKind], indexes: openarray[int]): bool =
  ## Returns true when any of the mutations or indexes occur together.
  for n in id.ecsEventMutations:
    n.expectLen 2
    let es = n[0].toEventState
    if es.event in mutations and indexes.hasAny(es.indexes):
      return true


proc addOccurred*(id: EcsIdentity, indexes: openarray[int]): bool =
  id.mutationOccurred({ekAddComponents}, indexes)


proc removeOccurred*(id: EcsIdentity, indexes: openarray[int]): bool =
  id.mutationOccurred({ekRemoveComponents}, indexes)


proc eventOccurred*(id: EcsIdentity, events: set[EventKind], indexes: openarray[int]): bool =
  for n in id.ecsEventEnv:
    let
      es = n.toEventState

    if es.event in events and indexes in es.indexes:
      return true

  false


proc recordMutation*(id: EcsIdentity, mutation: EventKind, indexes: openarray[int]) =
  ## Store consecutive embedded mutations.
  if id.ecsEventEnv.len > 1:
    let
      mutState = initEventState(mutation, indexes)
      compMutation = id.initMutation(mutState)

    var
      mutSoFar = id.ecsEventMutations
    
    if mutSoFar.len == 0 and mutSoFar.kind != nnkBracket:
      mutSoFar = nnkBracket.newTree()
    
    mutSoFar.add compMutation
    id.set_ecsEventMutations mutSoFar


proc trackMutation*(node: var NimNode, id: EcsIdentity, mutationType: EventKind, indexes: openarray[int], announce = true) =
  ## Wrap user callback code with static tracking to handle embedded state changes.
  
  var br = nnkBracket.newTree()
  for idx in indexes:
    br.add newLit(idx)
  
  let
    ecsId = quote do: EcsIdentity(`id`)
    mutParams = prefix(br, "@")

    startOp =
      if announce:
        let
          strOp = initEventState(mutationType, indexes).toStr(id)
        
        quote do:
          startOperation(`ecsId`, "Event " & `strOp`)
      else:
        newStmtList()

    endOp =
      if announce:
        quote do:
          endOperation(`ecsId`)
      else:
        newStmtList()
    
  node.insert(0, quote do:
    static:
      `startOp`
      enterEvent(`ecsId`, EventKind(`mutationType`), `mutParams`)
  )

  node.add(quote do:
    static:
      exitEvent(`ecsId`, EventKind(`mutationType`), `mutParams`)
      `endOp`
  )


proc trackMutation*(node: var NimNode, id: EcsIdentity, mutationType: EventKind, indexes: ComponentIterable or SystemIterable, announce = true) =
  trackMutation(node, id, mutationType, indexes.toIntList, announce)


#-------------
# Entity utils
#-------------


proc maxEntLen*(entityStorageFormat: ECSEntityItemStorage): NimNode =
  ## Returns a statement that gets the maximum number of entities based on options.
  let ecStateVarIdent = ident(entityStorageVarName())
  case entityStorageFormat
  of esSeq: newStmtList()
  of esArray:
    quote do: `ecStateVarIdent`.entityComponents.len
  of esPtrArray:
    quote do: `ecStateVarIdent`.entityComponents[].len


proc entityIdUpperBound*(storageFormat: ECSEntityItemStorage): NimNode =
  ## Returns a statement that gets the highest entityId currently available.
  let ecStateVarIdent = ident(entityStorageVarName())
  case storageFormat
  of esSeq:
    quote do: `ecStateVarIdent`.entityComponents.len
  of esArray, esPtrArray:
    quote do: `ecStateVarIdent`.nextEntityId.IdBaseType - 1


proc entAccess*(entityStorageFormat: ECSEntityItemStorage, entIdent: NimNode): NimNode =
  ## Access the entity component item via an entity ref, based on options
  ## This is wrapped by `entityData()` and used any time an entity's state is accessed.
  let ecStateVarIdent = ident(entityStorageVarName())
  case entityStorageFormat
  of esSeq, esArray:
    quote do:
      `ecStateVarIdent`.entityComponents[`entIdent`.int]
  of esPtrArray:
    quote do:
      `ecStateVarIdent`.entityComponents[][`entIdent`.int]


proc addComponentRef*(entity: NimNode, componentRef: NimNode, componentStorageFormat: ECSCompStorage): NimNode =
  # Insert a component to entity storage. Doesn't touch systems, doesn't update the set for hasComponent.
  # Components are inserted unordered.
  result =
    case componentStorageFormat
      of csTable:
        quote do:
          entityData(`entity`.entityId).componentRefs[`componentRef`.typeId] = `componentRef`
      of csSeq:
        quote do:
          entityData(`entity`.entityId).componentRefs.add(`componentRef`)
      of csArray:
        quote do:
          let newIdx = entityData(`entity`.entityId).nextCompIdx
          assert newIdx < entityData(`entity`.entityId).componentRefs.len, "Exceeded entity component storage capacity of " &
            $entityData(`entity`.entityId).componentRefs.len & " with index " & $newIdx
          entityData(`entity`.entityId).nextCompIdx = newIdx + 1
          entityData(`entity`.entityId).componentRefs[newIdx] = `componentRef`


proc addToEntityList*(id: EcsIdentity, entity: NimNode, passed: ComponentIterable, suffix: string): NimNode =
  # Add to the entity's component list.
  result = newStmtList()
  
  var
    mutationLog = nnkBracket.newTree

  for c in id.building passed:
    let
      fieldIdent = c.fetchedIdent suffix
      tyInt = c.typeId.int
    
    result.add addComponentRef(entity, newDotExpr(fieldIdent, ident "toRef"), id.componentStorageFormat)
    
    mutationLog.add tyInt.newLit
  
  if id.ecsEventEnv.len > 1:
    result.add(quote do:
      static:
        EcsIdentity(`id`).recordMutation(ekAddComponents, `mutationLog`)
    )


proc removeComponentRef(entityId, index: NimNode, typeId: ComponentTypeId, componentStorageFormat: ECSCompStorage): NimNode = 
  ## Removes a component from entity storage.
  ## 
  ## - Doesn't free the component slot,
  ## - Doesn't touch systems,
  ## - Doesn't update the intset for hasComponent.
  ## 
  ## Note: 'index' isn't used for the csTable format.

  result =
    case componentStorageFormat
      of csTable:
        # No error if key doesn't exist.
        quote do:
          entityData(`entityId`).componentRefs.del(`typeId`.ComponentTypeId)

      of csArray:
        quote do:
          let
            curHigh = entityData(`entityId`).nextCompIdx - 1
        
          if `index` < curHigh:
            # Swap last item with this one.
            entityData(`entityId`).componentRefs[`index`] = entityData(`entityId`).componentRefs[curHigh]
          # Cull length
          entityData(`entityId`).nextCompIdx -= 1

      of csSeq:
        quote do:
          # Swap with last value and cull length.
          entityData(`entityId`).componentRefs.del(`index`)


proc maybeFree(node: var NimNode, compInfo: ComponentBuildInfo, compRef: NimNode) =
  ## Owned components are a system's responsibility.
  
  if not compInfo.isOwned:
    # Free component slot.
    let
      instTy = compInfo.instanceTy
      delProc = ident deleteInstanceName()
    
    node.add(quote do:
      `delProc`(`instTy`(`compRef`.index))
    )


proc removeFromEntityList*(id: EcsIdentity, entity: NimNode, passed: ComponentIterable, suffix: string): NimNode =
  ## Remove a set of components from an entity's list.
  result = newStmtList()

  if passed.len == 0:
    warning "Internal: removeFromEntityList has been passed no components"
    return

  let
    entId = newDotExpr(entity, ident "entityId")
    index = genSym(nskForVar, "index")
    storageFormat = id.componentStorageFormat
    ed = ident "entityData" # Bind later when entityData is defined.

  case storageFormat
    of csTable:
      # Build a series of remove operations from the hash table.

      for c in id.building(passed):
        let
          typeId = c.typeId
          foundComp = genSym(nskLet, c.lcName)
        var
          coreDelete = newStmtList(
            removeComponentRef(entId, index, typeId, storageFormat)
          )
        
        coreDelete.maybeFree c, foundComp

        result.add(quote do:
          let
            `foundComp` = `ed`(`entId`).componentRefs.getOrDefault(`typeId`.ComponentTypeId)
          
          if `foundComp`.typeId == `typeId`.ComponentTypeId:
            `coreDelete`
          )
        
    of csSeq, csArray:
      # Build a case statement to process components in the list.

      let
        curCompRef = genSym(nskLet, "curComp")
        compsDeleted = genSym(nskVar, "compsDeleted")

        delCompCount = passed.len

        trackDeletions = quote do:
          `compsDeleted` = `compsDeleted` + 1
          if `compsDeleted` == `delCompCount`:
            break
      
      var
        caseStmt = nnkCaseStmt.newTree(quote do:
          `curCompRef`.typeId.int
        )
      
      for c in id.building(passed):
        var
          ofBranch = nnkOfBranch.newTree()
          coreDelete = newStmtList(
            removeComponentRef(entId, index, c.typeId, storageFormat)
          )
          
        coreDelete.maybeFree c, curCompRef
        if passed.len > 0:
          coreDelete.add trackDeletions

        ofBranch.add newLit(c.typeId.int)
        ofBranch.add(quote do:
          `coreDelete`
        )
        caseStmt.add ofBranch

      caseStmt.add nnkElse.newTree(quote do: discard)

      if passed.len > 0:
        result.add(quote do:
          var
            `compsDeleted`: int
        )
      
      let
        entCompRefLen = entId.componentRefsLen storageFormat

      result.add(quote do:
        for `index` in countDown(`entCompRefLen` - 1, 0):
          let
            `curCompRef` = `ed`(`entId`).componentRefs[`index`]

          `caseStmt`
      )

  var
    mutUpdates = newStmtList()
    mutationLog = nnkBracket.newTree
  
  for c in passed:
    let
      tyInt = c.int
    
    mutationLog.add tyInt.newLit

  mutUpdates.add(
    quote do:
      EcsIdentity(`id`).recordMutation(ekRemoveComponents, `mutationLog`)
  )

  result.add(quote do:
    static:
      `mutUpdates`
  )


proc entSetIncl*(entityId: NimNode, setVal: NimNode): NimNode =
  ## If `useSet` is true the set is updated with `setVal`,
  ## otherwise it does nothing.
  quote do:
    entityData(`entityId`).exists.incl `setVal`


proc entSetExcl*(entityId: NimNode, setVal: NimNode): NimNode =
  ## If `useSet` is true `setVal` is removed from the set,
  ## otherwise it does nothing.
  quote do:
    entityData(`entityId`).exists.excl `setVal`


proc componentRefsLen*(entityIdIdent: NimNode, componentStorageFormat: ECSCompStorage): NimNode =
  # This returns the number of items in the entity's componentRefs list, however that may be stored.
  result = case componentStorageFormat:
  of [csSeq, csTable]:
    quote:
      entityData(`entityIdIdent`).componentRefs.len
  of csArray:
    # The array book-keeps its highest value.
    quote:
      entityData(`entityIdIdent`).nextCompIdx


proc iterateComponents*(entitySym, curComp, actions: NimNode): NimNode =
  quote do:
    block:
      for `curComp` in `entitySym`:
        `actions`


#-------------
# System utils
#-------------


iterator systemTypesStr*(id: EcsIdentity, systemIndex: SystemIndex): string =
  # Utility function to yield the name of the types used in the system specified by systemIndex.
  for compId in id.ecsSysRequirements(systemIndex):
    yield id.typeName(compId)


proc strictCatchCheck(node: NimNode, id: EcsIdentity, sysIndex: SystemIndex) =
  ## Compile time check for using `item` "after" a delete or remove
  ## that affects the system.
  ## 
  ## Note that this doesn't handle conditional delete/removes.

  let
    cacheId = quote do: EcsIdentity(`id`)
    undefinedItemMsg = "Potentially unsafe access of 'item' here: the current system row may be " &
      "undefined due to an earlier "
    undefinedItemRemoveMsg = undefinedItemMsg & "removal of components that affect this system. "
    undefinedItemDeleteMsg = undefinedItemMsg & "deletion of an entity. "
    msgPostfix = "Use the 'entity' variable or the system's 'deleteList' " &
      "to avoid this error."

  if not defined(ecsPermissive):
    node.add(quote do:
      {.line.}:
        static:
          if not(`cacheId`.ecsSysIterating) or (`cacheId`.ecsSysIterating and `cacheId`.ecsEventEnv.len == 0):
            # Note: events within system iteration can skip this error
            # for two reasons:
            #
            # a) Events statically enforce system presence by monitoring
            #    mutations. This means we can rely on 'item' being valid
            #    at event entry, even when in an iterating system.
            #
            # b) Deleting 'entity' is a compile time error within events,
            #    and cannot invalidate 'item'.
            #
            # However, this guarantee does not apply within callbacks,
            # which could have many different paths to execution.
            #
            # Callbacks instead set 'sysRemoveAffectedThisSystem' when
            # they detect they're in a callback and will remove the
            # underlying system.

            if `cacheId`.sysRemoveAffectedThisSystem:
              error `undefinedItemRemoveMsg` & `msgPostfix`
            elif `cacheId`.systemCalledDelete:
              error `undefinedItemDeleteMsg` & `msgPostfix`
    )


proc systemItem*(id: EcsIdentity, sysIndex: SystemIndex, rowEntity, sys, itemIdx: NimNode): NimNode =
  ## Creates an `item` template to access the system row represented by `itemIdx`.
  
  var
    itemCore = newStmtList()
    assertItem = id.assertItem(sysIndex)
    sysItemType = ident itemTypeName(id.getSystemName sysIndex)
  
  # Compile time check for potentially invalid 'item' use.
  itemCore.strictCatchCheck(id, sysIndex)

  if assertItem:
    # The assertion conditions are checked separately from the `assert`
    # statement as `assert` itself takes a big performance hit.
    itemCore.add(
      quote do:
        if `itemIdx` notin 0 .. `sys`.high:
          assert false,
            "'item' in " & `sys`.name & " is out of bounds. " &
            "Use of 'item' after remove/delete affected this system?"
        elif `sys`.groups[`itemIdx`].entity != `rowEntity`:
          assert false,
            "'item' in " & `sys`.name & " is being used after a " &
            "remove or delete affected this system"
    )

  itemCore.add(quote do:
    `sys`.groups[`itemIdx`]
  )

  result = quote do:
    template item: `sysItemType` {.used.} =
      ## Current system item being processed.
      `itemCore`


#------------
# Index utils
#------------

proc indexRead*(sysNode, entIdNode: NimNode, idxFmt: ECSSysIndexFormat): NimNode =
  case idxFmt
  of sifTable:
    quote do:
      `sysNode`.index[`entIdNode`]
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int].row

proc indexWrite*(sysNode, entIdNode, rowNode: NimNode, idxFmt: ECSSysIndexFormat): NimNode =
  case idxFmt
  of sifTable:
    quote do:
      `sysNode`.index[`entIdNode`] = `rowNode`
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int] = (true, `rowNode`.Natural)

proc indexHasKey*(sysNode, entIdNode: NimNode, idxFmt: ECSSysIndexFormat): NimNode =
  case idxFmt
  of sifTable:
    quote do:
      `sysNode`.index.hasKey(`entIdNode`)
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int].exists

proc indexDel*(sysNode, entIdNode: NimNode, idxFmt: ECSSysIndexFormat): NimNode =
  case idxFmt
  of sifTable:
    quote do:
      `sysNode`.index.del(`entIdNode`)
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int].exists = false

proc indexTryGet*(sysNode, entIdNode, rowNode: NimNode, idxFmt: ECSSysIndexFormat): NimNode =
  ## Returns a search for an `EntityId` in a system index, placing the
  ## result in `rowNode`.
  ## 
  ## The resultant expression returns `true` when the index has the
  ## entity, and `false` if not.
  case idxFmt
  of sifTable:
    quote do:
      `rowNode` = `sysNode`.index.getOrDefault(`entIdNode`, -1)
      `rowNode` >= 0:
  of sifArray, sifAllocatedSeq:
    quote do:
      let rowData = `sysNode`.index[`entIdNode`.int]
      `rowNode` = rowData.row
      rowData.exists

proc updateIndex*(id: EcsIdentity, entity: NimNode, sys: SystemIndex, row: NimNode): NimNode =
  ## Update the system index with the entity.
  let
    systemNode = id.instantiation(sys)
    entId = quote do: `entity`.entityId
  result = systemNode.indexWrite(entId, row, id.indexFormat(sys))


#----------------------
# Owned component state
#----------------------


proc appendOwnedComponentState*(id: EcsIdentity, typeId: ComponentTypeId, row: NimNode): NimNode =
  ## Update state variables outside of entity and system storage.
  let
    typeStr = id.typeName typeId
    aliveIdent = ident aliveStateInstanceName(typeStr)
    instanceIdent = ident instanceIdsName(typeStr)
    compOpts = id.getOptions(typeId)

  case compOpts.componentStorageFormat
  of cisSeq:
    quote do:
      let
        row = `row`
        newLen = row + 1
      
      `aliveIdent`.setLen newLen
      `instanceIdent`.setLen newLen

      `aliveIdent`[row] = true
      `instanceIdent`[row] = 1.IdBaseType
  else:
    quote do:
      let row = `row`
      `aliveIdent`[row] = true
      `instanceIdent`[row] += 1.IdBaseType


proc removeOwnedComponentState*(id: EcsIdentity, typeId: ComponentTypeId, row: NimNode): NimNode =
  ## Update state variables outside of entity and system storage.
  let
    typeStr = id.typeName typeId
    aliveIdent = ident aliveStateInstanceName(typeStr)
    instanceIdent = ident instanceIdsName(typeStr)
    compOpts = id.getOptions(typeId)

  case compOpts.componentStorageFormat
  of cisSeq:
    quote do:
      if `row` == `aliveIdent`.high:
        let newLen = `aliveIdent`.len - 1
        `aliveIdent`.setLen newLen
        `instanceIdent`.setLen newLen
      else:
        `aliveIdent`[`row`] = false
  else:
    quote do:
      `aliveIdent`[`row`] = false


# ------------------------
# Assigning to system rows
# ------------------------


proc assignSysItemGetRow*(id: EcsIdentity, sys: SystemBuildInfo, value, row: NimNode, storageFormat: ECSSysStorage): NimNode =
  ## Extend system groups depending on options.
  ## 
  ## Sets `row` to the index of insertion.
  ## Note: `row` must be previously defined.

  let
    sysVar = sys.variable

  # TODO: custom user inserting instead of just appending.

  case storageFormat
    of ssSeq:
      quote do:
        `sysVar`.groups.add(`value`)
        `row` = `sysVar`.groups.high
    of ssArray:
      quote do:
        `row` = `sysVar`.nextFreeIdx
        `sysVar`.nextFreeIdx += 1
        `sysVar`.groups[`row`] = `value`


#--------------------------
# Removing from system rows
#--------------------------


proc unregister*(id: EcsIdentity, systemIndex: SystemIndex, sys, rowIdent, entIdIdent: NimNode): NimNode =
  ## Remove an entity's row in a system.
  # - Does not update the entity's storage.
  # - Does not invoke events.
  # - Assumes the row is valid.

  let
    systemStorageFormat = id.storageFormat systemIndex
    maintainOrder = id.orderedRemove systemIndex

    # When systems can be reordered removal is an O(1) operation, and
    # only needs one row to update, so we can use the supplied `rowIdent`.
    # When order is maintained, all rows after `rowIdent` must be shuffled,
    # so we need a unique variable ident to pass to `setIndex`.
    newRow =
      if maintainOrder: ident "newRow"
      else: rowIdent
    
    idxFmt = id.indexFormat(systemIndex)                            # Get index codegen options.
    updatedRowEntIdent = genSym(nskLet, "updatedRowEnt")
    updatedEntId = newDotExpr(updatedRowEntIdent, ident "entityId") # Equivalent to `updatedRowEntIdent.entityId`.
    setIndex = sys.indexWrite(updatedEntId, newRow, idxFmt)         # Updates the row index for `updatedEntId` with `updatedEntId`.
    
    entOpts = id.entityOptions
    maxEntId = entityIdUpperBound(entOpts.entityStorageFormat)      # The code to return the highest EntityId.

  let
    trimGroup = 
      case systemStorageFormat
      of ssSeq:
        if maintainOrder:
          quote do:
            `sys`.groups.delete `rowIdent`

            for `newRow` in `rowIdent` ..< `sys`.groups.len:
              let
                `updatedRowEntIdent` = `sys`.groups[`newRow`].entity
              
              # Update the index to `newRow`.
              `setIndex`

        else:
          quote do:
            let
              topIdx = `sys`.groups.high
              ri = `rowIdent`
            
            if ri < topIdx:
              `sys`.groups[ri] = move `sys`.groups[topIdx]
              
              let
                `updatedRowEntIdent` = `sys`.groups[ri].entity
              {.line.}:
                assert `updatedRowEntIdent`.alive, "Internal error: dead entity in system groups: id = " &
                  $`updatedRowEntIdent`.entityId.int &
                  ", current entity id upper bound = " & $`maxEntId`

              # Update the index to `newRow` (which equals `rowIdent`).
              `setIndex`

            {.line.}:
              assert `sys`.groups.len > 0, "Internal error: system \"" & `sys`.name &
                "\" groups are empty but is scheduling to delete from row " &
                $ri & ". Top row is " & $topIdx

            `sys`.groups.setLen(`sys`.groups.len - 1)

      of ssArray:
        if maintainOrder:
          quote do:
            let
              topIdx = `sys`.groups.high
            
            for `newRow` in `rowIdent` ..< topIdx:
              `sys`.groups[`newRow`] = move `sys`.groups[`newRow` + 1]

              let
                `updatedRowEntIdent` = `sys`.groups[`newRow`].entity
              
              # Update the index to `newRow`.
              `setIndex`
  
            `sys`.nextFreeIdx -= 1
  
        else:
          quote do:
            # Manual `del` for groups.
            let
              topIdx = `sys`.nextFreeIdx - 1
            
            if `rowIdent` < topIdx:
              `sys`.groups[`rowIdent`] = move `sys`.groups[topIdx]

              let
                `updatedRowEntIdent` = `sys`.groups[`rowIdent`].entity
              
              # Update the index to `newRow` (which equals `rowIdent`).
              `setIndex`

            `sys`.nextFreeIdx -= 1

  let
    delIndex = sys.indexDel(entIdIdent, idxFmt)
  
  var
    updateOwnedComps = newStmtList()

  for typeId in id.ecsOwnedComponents(systemIndex):
    updateOwnedComps.add id.removeOwnedComponentState(typeId, rowIdent)
  
  quote do:
    `updateOwnedComps`
    `delIndex`
    `trimGroup`


# ----------
# Type utils
# ----------


proc maybeIf*(terms, node: NimNode): NimNode =
  ## If terms is populated `if terms: node` is returned, otherwise
  ## `node` is returned.
  if terms != nil and terms.kind != nnkEmpty and (terms.kind != nnkStmtList or terms.len > 0):
    quote do:
      if `terms`: `node`
  else:
    node

iterator commaSeparate*(id: EcsIdentity, list: ComponentIterable or SystemIterable): string =
  ## Common function to produce a string of comma separated type names from ComponentTypeIds.
  var comma: bool
  for v in list:
    let str =
      when list is ComponentIterable: id.typeName v
      else: id.getSystemName v
    if comma: yield ", " & str
    else:
      comma = true
      yield str

proc commaSeparate*(id: EcsIdentity, list: ComponentIterable or SystemIterable): string {.compileTime.} =
  ## Common function to produce a string of comma separated ComponentTypeIds.
  for s in id.commaSeparate(list):
    result &= s

iterator typeDefs*(body: NimNode): NimNode =
  ## Return the nnkTypeDef nodes in a body.
  for item in body:
    if item.kind == nnkTypeSection:
      for def in item:
        if def.kind == nnkTypeDef:
          yield def

proc getTypeStr(compNode: NimNode): string =
  ## Extract the type of a symbol.

  let
    tyInst = compNode.getTypeInst()
  
  if tyInst.kind == nnkBracketExpr and tyInst[0].kind == nnkSym and
      tyInst[0].strVal.toLowerAscii == "typedesc":
    # Extract T from typedesc[T].
    tyInst[1].expectKind nnkSym
    tyInst[1].strVal
  else:
    tyInst.strVal

proc findType*(compNode: NimNode): string =
  ## Expects a typed node and tries to extract the type name.
  ## Note: converts typedesc[Type] to Type.
  case compNode.kind
    of nnkObjConstr:
      # Defined inline
      compNode.expectMinLen 1

      if compNode[0].kind == nnkDotExpr:
        compNode[0][1].strVal
      else:
        compNode[0].strVal

    of nnkSym:
      compNode.getTypeStr

    of nnkCall:
      let caller = compNode[0].getImpl()
      caller.expectKind nnkProcDef
      let callerTypeStr = $caller[3][0]
      callerTypeStr
    
    else:
      let
        tyStr = compNode.getTypeStr

      if tyStr != "":
        tyStr
      else:
        $(compNode.getTypeInst())

proc componentListToSet*(id: EcsIdentity, componentIds: seq[ComponentTypeId], setType: NimNode): NimNode =
  ## Create a set of component types.
  result = nnkCurly.newTree()
  # Add the exists flags, cast to the components enum
  for ty in componentIds:
    result.add ident id.typeName(ty).compEnum

proc typeStringToId*(id: EcsIdentity, n: string): ComponentTypeId {.compileTime.} =
  ## Returns an index for a type string, if found.
  # TODO: Find component type from AST signature instead of type string.
  # Might want to store type trees themselves and match on that.
  assert(n != "Component", "Not enough type information to create id: Receiving type `Component`, expected sub-class of Component or a registered component type")

  var r = id.findCompId(n)
  if r.int < 1:
    let
      compStr =
        if id.components.len > 1:
          id.commaSeparate(id.allComponentsSeq)
        else:
          "<No components defined>"

    error "Identity \"" & id.string & "\" cannot find type '" & n & "' in known component types: " & compStr
  r

iterator varArgsNodes(params: NimNode): NimNode =
  ## Unpacks parameter nodes given by varargs.
  case params.kind

    of nnkIdent, nnkSym:
      yield params
    
    of nnkBracket:
      if params.len > 0:
        if params[0].kind == nnkHiddenStdConv:
          params[0].expectMinLen 2

          for node in params[0][1]:
            yield node

        else:

          for node in params:
            yield node

    else:

      for node in params:
        yield node

iterator varArgsNodePairs(params: NimNode): (int, NimNode) =
  var i: int
  for node in params.varArgsNodes:
    yield (i, node)
    i.inc

proc varArgsLen(params: NimNode): int =
  case params.kind
    of nnkIdent, nnkSym:
      1
    of nnkBracket:
      if params.len > 0:
        if params[0].kind == nnkHiddenStdConv:
          params[0].expectMinLen 2
          params[0][1].len
        else:
          params.len
      else:
        0
    else:
      params.len

proc toTypeList*(id: EcsIdentity, componentList: NimNode): seq[ComponentTypeId] =
  ## Convert a list of nodes, such as from varargs[untyped], to a list of type ids.
  template process(compNode: NimNode): ComponentTypeId =
    let
      tyName = compNode.findType
    
    if tyName == "":
      error "Cannot find the type '" & compNode.repr & "' in identity \"" & id.string & "\""

    let typeId = id.typeStringToId(tyName)
    
    if typeId == InvalidComponent:
      error "Cannot resolve type id for '" & tyName & "'"
    if typeId in result:
      error "Passed more than one component of type " & tyName
    
    typeId

  result.setLen componentList.varArgsLen

  for i, node in componentList.varArgsNodePairs:
    result[i] = node.process

  assert result.len > 0, "Expected a list of component types"


proc toIntList*(list: ComponentIterable or SystemIterable): seq[int] =
  result.setLen list.len
  var i: int
  for v in list:
    result[i] = v.int
    i += 1

proc toIdent(nn: NimNode): NimNode =
  # Replace symbols with idents
  if nn.kind == nnkSym: ident nn.strVal
  else: nn

proc toSeq*[T](v: HashSet[T]): seq[T] =
  result.setLen v.len
  var i: int
  for item in v:
    result[i] = item
    i += 1

proc hasIntersection*[T](s1, s2: HashSet[T]): bool =
  ## Returns true when any member of `s1` is in `s2`.
  for v in s1:
    if v in s2:
      return true
  false


# ----------------------------------
# Node inspection / generation utils
# ----------------------------------


type
  FieldList* = seq[tuple[fieldNode, typeNode: NimNode]]

proc getFieldsFromRecCase(recCaseNode: NimNode): FieldList =
  # Return all the possible nodes in the case statement and allow
  # the language to assert when a case access is violated.
  recCaseNode.expectKind nnkRecCase

  # Add kind var.
  let kindVar = recCaseNode[0]
  result.add (kindVar[0].baseName, kindVar[1].toIdent)

  # All fields from of branches.
  for fieldIdx in 1 ..< recCaseNode.len:
    let field = recCaseNode[fieldIdx]
    field.expectKind nnkOfBranch
    let recList = field[1]
    recList.expectKind nnkRecList
    for ofField in recList:
      result.add (ofField[0].baseName, ofField[1].toIdent)

proc getFieldsFromTypeDef(typeDefNode: NimNode): tuple[typeName: string, fields: FieldList] =
  # Extract the fields from a type def.
  typeDefNode.expectKind nnkTypeDef
  typeDefNode.expectMinLen 3
  result.typeName = typeDefNode[0].basename.strVal
  let tyNode = typeDefNode[2]
  case tyNode.kind
  of nnkObjectTy:
    tyNode.expectMinLen 3

    let recList = tyNode[2]
    if recList.kind == nnkRecList:
      # recList can also be empty when there are no fields.
      for field in recList:
        if field.kind == nnkRecCase:
          result.fields.add field.getFieldsFromRecCase()
        else:
          let fType = field[^2].toIdent
          for i in 0 ..< field.len - 2:
            result.fields.add (field[i].baseName, fType)

  of nnkRefTy:
    tyNode.expectMinLen 1
    tyNode[0].expectMinLen 3

    let recList = tyNode[0][2]
    if recList.kind == nnkRecList:
      # recList can also be empty when there are no fields.
      for field in recList:
        let fType = field[^2].toIdent
        for i in 0 ..< field.len - 2:
          result.fields.add (field[i].baseName, fType)

  of nnkTupleTy:
    typeDefNode.expectMinLen 3
    for field in typeDefNode[2]:
      let fType = field[^2].toIdent
      for i in 0 ..< field.len - 2:
        result.fields.add (field[i].baseName, fType)

  else:
    echo "getFieldsFromTypeDef: Unknown kind: " & $tyNode.kind

proc getFields*(node: NimNode, typeName: string = ""): TypeFields =
  ## Process a node to extract typeDefs or tuple fields.
  ## This is used to create accessor functions for Index types,
  ## and handles most basic types including variant types.
  case node.kind
  of nnkStmtList:
    for n in node[0]:
      if n.kind == nnkTypeDef:
        return n.getFieldsFromTypeDef()
  of nnkTypeSection:
    for n in node:
      if n.kind == nnkTypeDef:
        return n.getFieldsFromTypeDef()
  of nnkTypeDef:
    return node.getFieldsFromTypeDef()
  else:
    echo "getFields: Cannot process node: " & node.treerepr

proc extractNode(chkNode: NimNode): NimNode =
  let tyNode = chkNode[2]
  case tyNode.kind
  of nnkObjectTy:
    tyNode[2]
  of nnkRefTy:
    tyNode[0][2]
  of nnkTupleTy:
    tyNode
  else:
    newEmptyNode()

proc checkDefForTy(chkNode: NimNode, typeName: string = ""): NimNode =
  ## Matches a typeDef for a particular name. If typeName is empty, matches the first typeDef.
  ## Returns an empty node if no match.
  result = newEmptyNode()
  if chkNode.kind == nnkTypeDef:
    case chkNode[0].kind
      of nnkIdent, nnkSym:
        if (typeName == "" or chkNode[0].basename.strVal == typeName):
          return chkNode.extractNode()
      of nnkPostFix:
        if (typeName == "" or chkNode[0][1].basename.strVal == typeName):
          return chkNode.extractNode()
      of nnkPragmaExpr:
        if (typeName == "" or chkNode[0][0][1].basename.strVal == typeName):
          return chkNode.extractNode()
      else:
        echo "checkDefForTy: Cannot process node \n", chkNode.treerepr
        return newEmptyNode()

proc recList*(node: NimNode, typeName: string = ""): NimNode =
  ## Extract the recList from a particular typeDef.
  ## If no `typeName` is specified, returns the first typeDef.
  ## This is for internal use only, and doesn't cover things like variant types,
  ## just for simple nnkRecList types.
  case node.kind
  of nnkStmtList:
    for statement in node:
      if statement.kind == nnkTypeSection:
        for def in statement:
          let fieldNode = checkDefForTy(def, typeName)
          if fieldNode.kind != nnkEmpty: return fieldNode
  of nnkTypeSection:
    for def in node:
      let fieldNode = checkDefForTy(def, typeName)
      if fieldNode.kind != nnkEmpty: return fieldNode
  of nnkTypeDef:
    let fieldNode = checkDefForTy(node, typeName)
    if fieldNode.kind != nnkEmpty: return fieldNode
  else: discard
  let tName = if typeName == "": "<Any type>" else: typeName
  echo "Unable to extract record list from type " & tName & ", given this node:\n" & node.treerepr
  newEmptyNode()

proc genArray*(size: int, typeName: NimNode): NimNode =
  nnkBracketExpr.newTree(
    newIdentNode "array" ,
    # Size of array
    newLit size,
    # Current type's identifier
    typeName
  )
proc genArray*(size: int, typeName: string): NimNode = genArray(size, newIdentNode(typeName))

proc genStaticArray*[T](arr: seq[seq[T]]): NimNode =
  ## This utility proc generates a static array that matches the parameter array.
  result = newStmtList()
  let typeName = T.name
  var bracket = newNimNode(nnkBracket)
  for i in 0 ..< arr.len:
    var compList = nnkBracket.newTree()
    # Add sub-elements, cast to correct type.
    for item in arr[i]:
      compList.add nnkDotExpr.newTree(newIntLitNode(item.int), ident typeName)
    if compList.len > 0:
      bracket.add(quote do: @`compList`)
    else:
      bracket.add(quote do: newSeq[`T`]())
  result.add bracket

proc genSeq*(typeIdent: NimNode): NimNode =
  nnkBracketExpr.newTree(
    newIdentNode "seq",
    typeIdent
  )
proc genSeq*(typeName: string): NimNode = genSeq(newIdentNode(typeName))

proc genTable*(typeName1, typeName2: NimNode): NimNode =
  let boundTable = bindSym "Table"
  nnkBracketExpr.newTree(
    boundTable ,
    typeName1,
    typeName2
  )

proc genField*(fieldName: string, public: bool, fieldTypeDef: NimNode, isThreadVar = false): NimNode =
  let
    nameIdent = if public:
      postFix(newIdentNode fieldName, "*")
    else:
      newIdentNode fieldName
    nameNode = if isThreadVar:
      nnkPragmaExpr.newTree(nameIdent, nnkPragma.newTree(ident "threadVar"))
    else:
      nameIdent

  nnkIdentDefs.newTree(
    # Name of field within type
    nameNode,
    # Field type definition
    fieldTypeDef,
    newEmptyNode()
  )

proc assignmentType*(valueNode: NimNode): NimNode =
  ## Determine the type of an assignment value.
  case valueNode.kind
  of nnkEmpty, nnkNilLit:
    error "Cannot determine type from nil/empty"
    newEmptyNode()
  of nnkCharLit: ident "int"
  of nnkIntLit..nnkInt64Lit: ident "int64"
  of nnkFloatLit..nnkFloat64Lit: ident "float"
  of nnkStrLit..nnkTripleStrLit: ident "string"
  else:
    nnkTypeOfExpr.newTree(valueNode)

proc injectIdent*(id: NimNode): NimNode = 
  ## Adds {.inject.} to an ident.
  nnkPragmaExpr.newTree(id, nnkPragma.newTree(ident"inject"))

proc populated*(node: NimNode): bool =
  ## Returns true when a populated statement list exist.
  ## This is useful if you end up with 'newStmtList(newStmtList())' and
  ## checking 'len > 0' is insufficient.
  node.findChild(
    it.kind == nnkStmtList and
    it.len > 0 and
    it[0].kind != nnkStmtList
  ).kind == nnkStmtList

proc unpack*(node: var NimNode, source: NimNode) =
  ## Adds `source` to `node`, unpacking from nnkStmtList if required.
  node.expectKind nnkStmtList
  
  if source.len > 0:
    if source.kind == nnkStmtList:
      for child in source:
        node.add child
    else:
      node.add source

macro isSymbol(n: untyped): untyped =
  if n.kind == nnkSym: newLit true
  else: newLit false

proc toInts*(list: ComponentIterable | SystemIterable): seq[int] =
  result.setLen list.len
  for i, c in list:
    result[i] = c.int

proc respondToPragma*(node: NimNode, pragmaName: string, actions: NimNode): NimNode =
  let
    pragmaIdent = ident pragmaName

  quote do:
    static:
      when isSymbol(`node`):
        when hasCustomPragma(`node`, `pragmaIdent`):
          `actions`


#-------------------------------------------------------------------------
# Compile time logging to macrocache to be written as run time file output
#-------------------------------------------------------------------------


import os

const
  consistencyChecking* = false
  defaultGenLogFilename* = getProjectPath() / "ecs_code_log.nim"

proc genLog*(id: static[EcsIdentity], params: varargs[string]) {.compileTime.} =
  ## Allows macros to generate a log that is then written to file.
  when defined(ecsLogCode):
    var s = ""
    for item in params:
      s &= $item
    s &= "\n"
    id.add_codeLog s

macro startGenLog*(id: static[EcsIdentity], fileName: static[string]): untyped =
  ## Empties log file.
  result = newStmtList()
  when defined(ecsLogCode):
    let
      fn = newLit fileName
      preludeFn = currentSourcePath.parentDir.parentDir.joinPath "sharedtypes.nim"
      prelude = staticRead(`preludeFn`)
    result = quote do:
      let f = `fn`.open(fmWrite)
      f.write `prelude`
      f.close
      echo "Started file \"", `fn`, "\""  

proc flushGenLog*(id: EcsIdentity, fileName: static[string]): NimNode =
  ## Write log to file.
  ## Because we cannot `import c` at compile time and write the log then,
  ## we build the code to statically write the log at run time...
  ## This means a lot of text stored in your program exe before its
  ## written to the actual log file at run time start up.
  ## To activate the code log pass `-d:ecsLogCode` flag when compiling.
  result = newStmtList()
  when defined(ecsLogCode):
    var logText = ""
    let
      fn = newLit fileName
      log = id.codeLog()
      start = id.codeLogStart()
      newLogItems = log[start .. ^1]
    
    if newLogItems.len > 0:
      for line in newLogItems:
        logText &= line
      let text = newLit logText

      # Update new log start index.
      id.set_codeLogStart id.len_codeLog()

      echo "Recording log from line " & $start & " to " &
        $newLogItems.len & ", " & $logText.len & " bytes"

      # Generate *runtime* file output of static log string.
      result.add(quote do:
        let
          f = `fn`.open(fmAppend)
          total = `text`.len
        try:
          f.write(`text`)
        finally:
          f.close
        
        echo "Appended to ECS generation log '", `fn`, "': ", total, " characters written"
      )

template genLog*(params: varargs[string]) =
  genLog(defaultIdentity, params)

template startGenLog*(fileName: static[string]): untyped =
  startGenLog(defaultIdentity, fileName)

template startGenLog*() =
  startGenLog(defaultGenLogFilename)


#---------------
# Type utilities
#---------------


proc genInfixes*(clauses: seq[NimNode] | HashSet[NimNode], connector: string): NimNode =
  ## Creates a tree of infix clauses to match the clauses list.

  var
    parent = newEmptyNode()
  
  for c in clauses:
    if parent.kind == nnkEmpty:
      parent = c
    else:
      parent = infix(parent, connector, c)
  parent

proc genTypeClass*(name: string, public: bool, nodeList: openarray[NimNode], mandatory = true): NimNode =
  ## Generates a typeclass definition for registered types.
  ## eg;
  ## type
  ##   ComponentTypeClass = Test | Test2 | Test3
  if nodeList.len == 0:
    if mandatory:
      error "No types defined to generate type class"
    else:
      return newStmtList()
  else:
    var parent = nodeList[0]
    for i in 1 ..< nodeList.len:
      parent = infix(parent, "|", nodeList[i])
    var tcn: NimNode  # Type class node.
    if public:
      tcn = postFix(newIdentNode name, "*")
    else:
      tcn = newIdentNode(name)
    result = quote do:
      type `tcn` = `parent`

proc fill*[T](val: var T, node: NimNode) =
  ## Will try to fill the fields of `val` with equivalent fields in `node`.
  ## Eg;
  ##   var
  ##     realType: MyType
  ##     myAst = quote: MyType(someField: someValue)
  ##   realType.fill(myTypeAST)
  let n = node.getImpl()
  n.expectKind nnkObjConstr
  for item in n:
    if item.kind == nnkExprColonExpr:
      let nodeField = item[0].strVal
      for field, val in val.fieldPairs:
        if field == nodeField:
          let curVal = item[1]
          case curVal.kind
          of nnkStrLit:
            when val is string: val = curVal.strVal
          of nnkIntLit:
            when val is int or val is enum: val = type(val)(curVal.intVal)
            elif val is bool: val = curVal.intVal != 0
            else:
              error "Unhandled int type for fill: " & $curVal.getType.treerepr
          else:
            error "Unhandled type for fill: " & $curVal.kind


#-------------------------
# System ownership mapping
#-------------------------


proc calcDependentOwners*(id: EcsIdentity, typeId: ComponentTypeId): seq[SystemIndex] =
  ## Returns owner systems that have dependent owned components
  ## in a chain from this component.
  ## 
  ## The returned systems require this component to instantiate
  ## a row, either directly or indirectly, through one or more
  ## other components.
  ## 
  ## For example, consider the following dependent systems:
  ##   sys1 A, B        Owns: [A, B]
  ##   sys2 A, B, C, D  Owns: [C, D]
  ##   sys3 C, D, E, F  Owns: [E, F]
  ##   sys4 E, F, G, H  Owns: [G, H]
  ## 
  ## In order for any of sys1..sys4 to exist, all the components
  ## A, B, C, D, E, F, G, and H must be added in one operation.
  ## Likewise, removal of any of these components individually will
  ## invalidate the chain of ownership for sys1..sys4, and therefore
  ## causes all of them to be removed. These components only exist
  ## in relation to each other.
  ## 
  ## This is not true for non-owned components as they have independent
  ## existence outside of a system, and can be attached to an entity
  ## even if no system uses them.

  var
    systemsToVisit = initDeque[SystemIndex]()
    queuedSystems: HashSet[SystemIndex]
    resultSystems: HashSet[SystemIndex]
    visitedComps: HashSet[ComponentTypeId]

  proc queueDependents(sys: SystemIndex) =
    # Add any systems directly referencing owned components of `sys`.
    for component in id.ecsOwnedComponents(sys):
      if component notin visitedComps:
        visitedComps.incl component

        for linkedSys in id.systems(component):
          if linkedSys notin queuedSystems:
            queuedSystems.incl linkedSys
            systemsToVisit.addLast linkedSys

  let owner = id.systemOwner(typeId)
  # Only owned components require graph traversal.
  if owner != InvalidSystemIndex and id.len_systems(typeId) > 0:
    owner.queueDependents

    while systemsToVisit.len > 0:
      let sys = systemsToVisit.popFirst
      resultSystems.incl sys
      sys.queueDependents

  let systemsUsingSourceType = id.systems(typeId).toHashSet
  toSeq(resultSystems - systemsUsingSourceType)


proc inclDependents*[T: ComponentIterable](id: EcsIdentity, comps: T): T =
  ## Return owned components that are dependent on `comps` for storage.

  template doAdd(v: ComponentTypeId): untyped =
    when comps is ComponentSet: result.incl v
    else: result.add v

  for typeId in comps:
    doAdd typeId
    for depComp in id.dependentComps(typeId):
      if depComp notin result:
        doAdd depComp


# -------------------
# System commit utils
# -------------------

proc commitSystemList*(id: EcsIdentity, systems: openarray[SystemIndex], runProc: string): NimNode =
  ## Output any uncommitted system body procs in `systems`.
  ## 
  ## If `runProc` is a non-empty string, a wrapper proc is generated to
  ## call `systems` in the order given.

  result = newStmtList()

  const
    logOrder = defined(ecsLog) or defined(ecsLogDetails)
  var
    sysCalls = newStmtList()
    noBodies: seq[string]
    uncommitted = id.getUncommitted()
  
  when logOrder:
    var runOrder: string

  for sys in systems:
    let
      definition = id.definition sys
      sysName = id.getSystemName sys
  
    if definition.len > 0:

      let ucIndex = uncommitted.find sys

      # Add system body proc.
      if ucIndex > -1:
        id.startOperation "Adding run proc for system \"" & sysName & "\""

        result.add definition
        # Mark as committed.
        id.set_bodyDefined(sys, true)
        
        # Now the definition has been added it can be removed from the
        # uncommitted list.
        uncommitted.delete ucIndex

        id.endOperation

      # Call all the parameter systems within the runProc.
      if runProc.len > 0:
        sysCalls.add nnkCall.newTree(ident doProcName(sysName))

        when logOrder:
          runOrder.add "  " & sysName & "\n"
    else:
      noBodies.add "\"" & sysName & "\""

  # Write back list with committed systems removed.
  id.setUncommitted uncommitted

  if runProc.len > 0:
    # Include a wrapper proc to execute the above systems.

    when logOrder:
      echo "Wrapper proc `" & runProc & "()` execution order:"
      
      if runOrder.len > 0:
        echo runOrder
      else:
        echo "  <No system bodies found>"

    if noBodies.len > 0:
      # It's a compile time error to generate a run proc with systems
      # that don't have bodies defined.

      var outputStr = noBodies[0]
      for i in 1 ..< noBodies.len:
        outputStr &= ", " & noBodies[i]
      
      error "Systems to be committed were missing bodies: [" &
        `outputStr` & "]"
    else:
      # Generate the run proc.
      let
        procIdent = ident runProc

      if sysCalls.len > 0:
        result.add(quote do:
          proc `procIdent`* =
            `sysCalls`
        )
      else:
        # No systems with bodies have been provided.
        # This isn't an error to allow easier prototyping.

        let emptyProcStr = newLit(
          "System run procedure `" & runProc & "` does not call any systems")
        result.add(quote do:
          proc `procIdent`* =
            {.warning: `emptyProcStr`.}
            discard
        )

# --------------
# Source parsing
# --------------


proc deExport*(code: var NimNode, suppressUnusedHints = true) =
  ## Removes top level export markers from templates, procs, funcs,
  ## macros, and var, let, and const sections.

  let hasCode = code.populated

  if suppressUnusedHints and hasCode:
    code.insert(0, quote do:
      {.push used.}
    )

  for i in 0 ..< code.len:

    template isExport(n: NimNode): bool =
      if n.kind == nnkPostFix and n[0].kind == nnkIdent and n[0].strVal == "*":
        true
      else:
        false

    # Unpack postfix export markers in place.

    case code[i].kind

      of nnkProcDef, nnkTemplateDef, nnkIteratorDef, nnkMethodDef, nnkFuncDef, nnkMacroDef:
        if code[i][0].isExport:
          code[i][0] = code[i][0][1]

      of nnkConstSection, nnkLetSection, nnkVarSection, nnkTypeSection:
        for def in 0 ..< code[i].len:

          if code[i][def].kind in [nnkConstDef, nnkIdentDefs, nnkTypeDef]:

            for id in 0 ..< code[i][def].len:

              if code[i][def][id].kind == nnkPragmaExpr:
                if code[i][def][id][0].isExport:
                  code[i][def][id][0] = code[i][def][id][0][1]

              else:
                if code[i][def][id].isExport:
                  code[i][def][id] = code[i][def][id][1]

      of nnkStmtList:
        var temp = code[i]
        temp.deExport
        code[i] = temp

      else:
        discard

  if suppressUnusedHints and hasCode:
    code.add(quote do:
      {.pop.}
    )
