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

import macros, strutils, typetraits, ../sharedtypes, ecsstatedb
import debugging, tables, deques, sequtils, sets, macrocache
export debugging

## This module covers shared internal tools and utilities used to build
## the ECS code.
## 
## *This module is not exported to the user*.
## 
## Topics:
## 
## - Procs used to generate consistent field and type names.
## - Structural generation through various levels of abstraction.
## - Building user event calls.
## - Compile time utilities for inspecting and building types.
## - Walking system ownership graphs to find dependencies.

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

# Type names derived from user types

proc instanceTypeName*(tyName: string): string =
  ## Instance distinct type name
  tyName & "Instance"

proc generationTypeName*(tyName: string): string =
  ## Instantiation instance distinct type
  tyName & "Generation"

proc refTypeName*(tyName: string): string =
  ## Reference container type name
  tyName & "Ref"

# Initialisation/deleting components

proc instanceInitName*(prefix, s: string): string =
  ## Initialiser proc name for instance of type, takes arguments to set fields
  prefix & s

proc createInstanceName*(s: string): string =
  ## Allocate function for a component slot for this type
  "gen" & s

proc deleteInstanceName*: string =
  ## Clear function for component slot for this type
  "delete"

# Storage field names

proc entityStorageTypeName*: string =
  ## Type name for entity storage
  "EntityStorage"
proc entityStorageItemTypeName*: string = "EntityComponentItem"

proc entityStorageVarName*: string =
  ## This is the name of the storage variable generated from a particular prefix.
  "entityStorage"
proc initEntityStorageTypeName*: string = "initEntityStorage"
proc finaliseEntityStorageTypeName*: string = "finaliseEntityStorage" # TODO
proc entityStorageContainerTypeName*: string = "EntityStorageItems"

proc enumName*: string =
  ## Name of enum type used for sets of components.
  "ComponentsEnum"
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

# System names

proc systemTypeName*(name: string): string = name.capitalizeAscii() & "System"
proc systemInitName*(name: string): string = "init" & name.capitalizeAscii() & "System"

proc systemAddedCBName*(name: string): string = name & "AddedCallback"
proc systemRemovedCBName*(name: string): string = name & "RemovedCallback"

const
  sysVarPrefix = "sys"
  tupleNamePrefix = "SysTuple"  # capitalisation for type
  doProcPrefix = "do"
  instPostfix* = "Inst"

func tupleName*(name: string): string = tupleNamePrefix & name.capitalizeAscii
func doProcName*(name: string): string = doProcPrefix & name.capitalizeAscii
func systemVarName*(name: string): string = sysVarPrefix & name.capitalizeAscii
func addCallbackName*(name: string): string = "addCallback" & name
func removeCallbackName*(name: string): string = "removeCallback" & name
func systemStr*(name: string): string = name & " (" & systemVarName(name) & ")"

# Type classes that cover component types. These are useful for parameter constraint.

proc typeClassName*: string =
  ## The name of the type class that covers all component types.
  "ComponentTypeClass"

proc refTypeClassName*: string =
  ## The name of the type class that covers all the ref types.
  "ComponentRefTypeClass"

proc instanceTypeClassName*: string =
  ## The name of the type class that covers all the distinct int types.
  "ComponentIndexTypeClass"

#-------------
# Entity utils
#-------------

proc maxEntLen*(options: ECSEntityOptions): NimNode =
  ## Returns a statement that gets the maximum number of entities based on options.
  let ecStateVarIdent = ident(entityStorageVarName())
  case options.entityStorageFormat
  of esSeq: newEmptyNode()
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

proc entAccess*(options: ECSEntityOptions, entIdent: NimNode): NimNode =
  ## Access the entity component item via an entity ref, based on options
  ## This is wrapped by `entityData()` and used any time an entity's state is accessed.
  let ecStateVarIdent = ident(entityStorageVarName())
  case options.entityStorageFormat
  of esSeq, esArray:
    quote do:
      `ecStateVarIdent`.entityComponents[`entIdent`.int]
  of esPtrArray:
    quote do:
      `ecStateVarIdent`.entityComponents[][`entIdent`.int]

proc addComponentRef*(entity: NimNode, componentRef: NimNode, options: ECSEntityOptions): NimNode =
  # Insert a component to entity storage. Doesn't touch systems, doesn't update the set for hasComponent.
  # Components are inserted unordered.
  case options.componentStorageFormat
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

proc addToEntityList*(id: EcsIdentity, entity: NimNode, passed: seq[ComponentTypeId]): NimNode =
  # Add to the entity's component list.
  result = newStmtList()

  for typeId in passed:
    let
      typeStr = id.typeName typeId
      fieldName = typeStr.toLower
      fieldIdent = ident fieldName & instPostfix
    result.add addComponentRef(entity, newDotExpr(fieldIdent, ident "toRef"), id.entityOptions)

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

proc componentRefsLen*(entityIdIdent: NimNode, options: ECSEntityOptions): NimNode =
  # This returns the number of items in the entity's componentRefs list, however that may be stored.
  case options.componentStorageFormat:
  of [csSeq, csTable]:
    quote:
      entityData(`entityIdIdent`).componentRefs.len
  of csArray:
    # The array book-keeps its highest value.
    quote:
      entityData(`entityIdIdent`).nextCompIdx

#------------------
# System type utils
#------------------

iterator systemTypesStr*(id: EcsIdentity, systemIndex: SystemIndex): string =
  # Utility function to yield the name of the types used in the system specified by systemIndex.
  for compId in id.ecsSysRequirements(systemIndex):
    yield id.typeName(compId)

iterator systemTypesStrPair*(id: EcsIdentity, systemIndex: SystemIndex): tuple[typeName: string, id: ComponentTypeId] =
  ## Utility function to yield the name and type id of the types used in the system specified by systemIndex.
  for compId in id.ecsSysRequirements(systemIndex):
    yield (typeName: id.typeName(compId), id: compId)

#-------------------
# User event storage
#-------------------

proc getCompEvent*(typeName, event: string): NimNode =
  let key = CacheSeq("comp" & typeName & event)
  result = newStmtList()
  for event in key.items:
    result.add event

proc addCompEvent*(typeName, event: string, value: NimNode) =
  let key = CacheSeq("comp" & typeName & event)
  key.add value

proc numCompEvents*(typeName, event: string): int =
  ("comp" & typeName & event).len

proc getSysEvent*(sysName, event: string): NimNode =
  let key = CacheSeq("sys" & sysName & event)
  result = newStmtList()
  for event in key.items:
    result.add event

proc addSysEvent*(sysName, event: string, value: NimNode) =
  let key = CacheSeq("sys" & sysName & event)
  key.add value

proc numSysEvents*(sysName, event: string): int =
  ("sys" & sysName & event).len

#------------
# Index utils
#------------

proc indexRead*(sysNode, entIdNode: NimNode, options: ECSSysOptions): NimNode =
  case options.indexFormat
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

#------------------
# Adding components
#------------------

proc addUserCompAddToSys*(id: EcsIdentity, currentNode: var NimNode, ent: NimNode, sys: SystemIndex, typeId: ComponentTypeId, fieldIdent: NimNode) =
  ## Adds user events for components that use `onSystemAddTo` and `onSystemAdd`.
  let sysNode = id.instantiation(sys)
  var addedCode = newStmtList()

  if id.len_onAddToCode(sys, typeId) > 0:

    # Event triggered if system and component type match.
    let userAddToSys = id.onAddToCodeNode(sys, typeId)
    
    if userAddToSys != nil:
      addedCode.add(quote do:
        block:
          template curEntity: EntityRef {.used.} = `ent`
          template curComponent: untyped {.used.} = `fieldIdent`
          template curSystem: untyped {.used.} = `sysNode`
          `userAddToSys`
      )

  let userAddSys = id.onAddAnySystemCodeNode(typeId)
  
  if userAddSys.len > 0:
    # Check for this type's initialiser.
    let sysNode = id.instantiation(sys)
    addedCode.add(quote do:
      block:
        template curEntity: EntityRef {.used.} = `ent`
        template curComponent: untyped {.used.} = `fieldIdent`
        ## Access to current updating system variable.
        template curSystem: untyped {.used.} = `sysNode`
        `userAddSys`
    )

  if addedCode.len > 0:
    currentNode.add addedCode

proc ownedUserInitEvents*(id: EcsIdentity, sys: SystemIndex, sysTupleVar, curComp: NimNode, typeId: ComponentTypeId, entity, value: NimNode): tuple[onAdd, onInit, onUpdate: NimNode] =
  ## Whereas un-owned components handle their user initialisation
  ## events within the component creation procedure, owned components
  ## don't have init procedures and so need to invoke this inline.
  let
    typeStr = id.typeName(typeId)
    lcTypeStr = typeStr.toLower
    typeField = ident lcTypeStr
    ownedByThisSystem = id.systemOwner(typeId) == sys

    onAddToEnt = id.onAddToEntCodeNode(typeId)
    onInit = id.onInitCodeNode(typeId)
    onInterceptInit = id.onInterceptValueInitCodeNode(typeId)

  let
    onAddCode =
      if ownedByThisSystem and onAddToEnt.len > 0:
        let userCode = onAddToEnt
        quote do:
          block:
            template curComponent: untyped {.used.} = `curComp`
            template curEntity: untyped {.used.} = `entity`
            `userCode`
      else: newStmtList()
    
    userInitCode =
      if ownedByThisSystem and onInit.len > 0:
        let code = onInit
        quote do:
          block:
            template curEntity: untyped {.used.} = `entity`
            template curComponent: untyped {.used.} = `curComp`
            `code`
      else:
        newStmtList()

    updateCode =
      if ownedByThisSystem and onInterceptInit.len > 0:
        # It's now the user's responsibility to call commit.
        let code = onInterceptInit
        quote do:
          block:
            template curEntity: untyped {.used.} = `entity`
            template curValue: untyped = `value`
            template commit(value: untyped) =
              `sysTupleVar`.`typeField` = value
            `code`
      else: newStmtList()
  
  (onAddCode, userInitCode, updateCode)

proc userSysAdded*(id: EcsIdentity, sys: SystemIndex, row: NimNode): NimNode =
  ## Invoke code defined in the system's `added:` section.
  let
    echoRunning = id.echoRunning(sys)
    sysName = id.getSystemName(sys)
    sysVar = id.instantiation(sys)
  result = newStmtList()
  if id.len_onAdded(sys) > 0:

    let
      doEcho =
        if echoRunning != seNone:
          quote do:
            echo `sysName` & " adding entry"
        else: newEmptyNode()

      userAddedEvent = id.onAddedNode(sys)

    result.add(quote do:
      block:
        let item {.inject, used.} = `sysVar`.groups[`row`].addr
        template sys: untyped {.used.} = `sysVar`
        template groupIndex: untyped {.used.} = `row`
        `doEcho`
        `userAddedEvent`
    )

  if id.len_onAddedCallback(sys) > 0:
    let cbEvent = ident sysName.systemAddedCBName
    result.add(quote do:
      `cbEvent`(`sysVar`, `row`)
    )

proc userStateChange*(id: EcsIdentity, entity: NimNode,
    state: EntityChangeEvent, cList: seq[ComponentTypeId] = @[]): NimNode =
  let
    stateChangeEvent = id.onEntityStateChangeNode()
    ent = ident "entity"
    st = ident "state"
    types = ident "types"
    tIds =
      if cList.len > 0:
        # Distinct type isn't preserved through `quote` so we have to
        # add type casts to get back to ComponentTypeId.
        var compIds = nnkBracket.newTree
        for c in cList:
          compIds.add newDotExpr(newLit c.int, ident "ComponentTypeId")
        
        quote do: `compIds`

      else:
        quote do:
          var compIds = newSeq[ComponentTypeId](`entity`.componentCount)
          var i: int
          for c in `entity`:
            compIds[i] = c.typeId
            i.inc
          compIds
  if stateChangeEvent.len > 0:
    quote do:
      block:
        const
          `st` {.inject, used.}: EntityChangeEvent = `state`.EntityChangeEvent
        let
          `ent` {.inject, used.}: EntityRef = `entity`
          `types` {.inject, used.} = `tIds`
        `stateChangeEvent`
  else:
    newStmtList()

proc updateOwnedComponentState*(id: EcsIdentity, typeId: ComponentTypeId, system: SystemIndex, row: NimNode): NimNode =
  ## Update state variables outside of entity and system storage.
  let
    typeStr = id.typeName typeId
    aliveIdent = ident aliveStateInstanceName(typeStr)
    instanceIdent = ident instanceIdsName(typeStr)
    compOpts = id.getOptions(typeId)

  case compOpts.componentStorageFormat
  of cisSeq:
    quote do:
      `aliveIdent`.setLen `row` + 1
      `aliveIdent`[`row`] = true
      `instanceIdent`.setLen `row` + 1
      `instanceIdent`[`row`] += 1.IdBaseType
  else:
    quote do:
      `aliveIdent`[`row`] = true
      `instanceIdent`[`row`] += 1.IdBaseType

proc addToSystemTuple*(systemNode: NimNode, value: NimNode, sysOpts: ECSSysOptions): NimNode =
  # Extend system groups depending on options.
  case sysOpts.storageFormat
    of ssSeq:
      quote do:
        `systemNode`.groups.add(`value`)
    of ssArray:
      quote do:
        `systemNode`.groups[`systemNode`.nextFreeIdx] = `value`
        `systemNode`.nextFreeIdx += 1

proc addSysTuple*(id: EcsIdentity, entity: NimNode, sys: SystemIndex, componentsPassed: seq[ComponentTypeId], componentValues: NimNode | seq[NimNode], postFix = instPostfix): tuple[sysUpdates, userEvents: NimNode, eventsExist: bool] =
  ## Assumes you have a generated variable that matches each field in the system tuple already defined.
  ## Eg; a system using components `A` and `B` will have variables `a: AInstance` and `b: BInstance`.
  let sysVar = id.instantiation(sys)

  # Generate system tuple assignment.
  var
    sysTuple = nnkPar.newTree()
    updateOwnedState = newStmtList()
    userSysAddCode = newStmtList()

  sysTuple.add nnkExprColonExpr.newTree(ident "entity", entity)
  for fields in id.systemTypesStrPair(sys):
    let
      tupleFieldStr = fields.typeName.toLowerAscii()
      tupleFieldIdent = ident tupleFieldStr
      compSource = ident tupleFieldStr & postfix
    
    if id.systemOwner(fields.id) == sys:
      let compIdx = componentsPassed.find(fields.id)
      assert compIdx >= 0, "Error: Cannot find owned field for " & id.typeName(fields.id) & " within " & componentValues.repr

      # Owned components are directly assigned to the tuple from the parameter list.
      sysTuple.add nnkExprColonExpr.newTree(tupleFieldIdent, componentValues[compIdx])

      let sysHigh = quote: `sysVar`.count
      updateOwnedState.add id.updateOwnedComponentState(fields.id, sys, sysHigh)

    else:
      sysTuple.add nnkExprColonExpr.newTree(tupleFieldIdent, compSource)

    # Add user events for this component being added to the system.
    id.addUserCompAddToSys(userSysAddCode, entity, sys, fields.id, compSource)

  let
    # Get code to add the tuple of components to the system groups list.
    updateGroup = addToSystemTuple(sysVar, sysTuple, id.getOptions(sys))

    # Code for updating the system index.
    entIdIdent = quote do: `entity`.entityId
    row = quote do: `sysVar`.high
    updateIndex = sysVar.indexWrite(entIdIdent, row, id.indexFormat(sys))

    userAddedEvents = id.userSysAdded(sys, row)
    # userAddedCBEvents =
    #   if id.len_onAddedCallback(sys) > 0:
    #     let
    #       sysName = id.getSystemName sys
    #       cbName = ident systemAddedCBName(sysName)
    #     quote do:
    #       `cbName`(`sysVar`, `row`)
    #   else:
    #     newStmtList()

  result.sysUpdates = quote do:
    `updateOwnedState`
    `updateGroup`
    `updateIndex`

  result.userEvents = quote do:
    `userSysAddCode`
    `userAddedEvents`
    #`userAddedCBEvents`

  result.eventsExist = userAddedEvents.len > 0 or
    userSysAddCode.len > 0

#------------------
# Removing components
#------------------

proc removeComponentRef*(entityId, index: NimNode, componentTypeId: int, componentStorageFormat: ECSCompStorage): NimNode = 
  # Removes a component from entity storage.
  # * Doesn't touch systems
  # * Doesn't update the intset for hasComponent.

  let remIdx =
    if componentStorageFormat == csTable:
      quote do: `componentTypeId`.ComponentTypeId
    else:
      newIntLitNode(componentTypeId)

  case componentStorageFormat
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

proc removeSysReference*(id: EcsIdentity, systemIndex: SystemIndex, sys, sysRowExists, rowIdent, entIdIdent, entity: NimNode): NimNode =
  ## Remove an entity's tuple from a system.
  # - Does not update the entity's storage.
  # - Does not add events.

  let
    systemStorageFormat = id.storageFormat systemIndex
    maintainOrder = id.orderedRemove systemIndex

    updatedRowEntIdent = ident "updatedRowEnt"
    updatedEntId = newDotExpr(updatedRowEntIdent, ident "entityId") # Equivalent to `updatedRowEntIdent.entityId`.

    # When systems can be reordered removal is an O(1) operation, and
    # only needs one row to update, so we can use the supplied `rowIdent`.
    # When order is maintained, all rows after `rowIdent` must be shuffled,
    # so we need a unique variable ident to pass to `setIndex`.
    newRow =
      if maintainOrder: ident "newRow"
      else: rowIdent
    
    idxFmt = id.indexFormat(systemIndex)                            # Get index codegen options.
    setIndex = sys.indexWrite(updatedEntId, newRow, idxFmt)       # Updates the row index for `updatedEntId` with `updatedEntId`.
    
    entOpts = id.entityOptions
    maxEntId = entityIdUpperBound(entOpts.entityStorageFormat)      # The code to return the highest EntityId.

  let
    topIdxIdent = ident "topIdx"

    trimGroup = 
      case systemStorageFormat
      of ssSeq:
        if maintainOrder:
          quote do:
            `sys`.groups.delete `rowIdent`

            for `newRow` in `rowIdent` ..< `sys`.groups.len:
            
              let `updatedRowEntIdent` = `sys`.groups[`newRow`].entity
              # Update the index to `newRow`.
              `setIndex`

        else:
          quote do:
            let `topIdxIdent` = `sys`.groups.high

            if `rowIdent` < `topIdxIdent`:
              `sys`.groups[`rowIdent`] = move `sys`.groups[`topIdxIdent`]
              
              let `updatedRowEntIdent` = `sys`.groups[`rowIdent`].entity
              
              assert `updatedRowEntIdent`.alive, "Internal error: Dead entity in system groups: id = " &
                $`updatedRowEntIdent`.entityId.int &
                ", current entity id upper bound = " & $`maxEntId`

              # Update the index to `newRow` (which equals `rowIdent`).
              `setIndex`

            assert `sys`.groups.len > 0, " System \"" & `sys`.name &
              "\" has empty group but is scheduled to delete from row " &
              $`rowIdent` & ". Top row is " & $`topIdxIdent`
            `sys`.groups.setLen(`sys`.groups.len - 1)

      of ssArray:
        if maintainOrder:
          quote do:
            let `topIdxIdent` = `sys`.groups.high
            for `newRow` in `rowIdent` ..< `topIdxIdent`:

              `sys`.groups[`newRow`] = move `sys`.groups[`newRow` + 1]

              let `updatedRowEntIdent` = `sys`.groups[`newRow`].entity
              # Update the index to `newRow`.
              `setIndex`
  
            `sys`.nextFreeIdx -= 1
  
        else:
          quote do:
            # Manual `del` for groups.
            let `topIdxIdent` = `sys`.nextFreeIdx - 1
            if `rowIdent` < `topIdxIdent`:

              `sys`.groups[`rowIdent`] = move `sys`.groups[`topIdxIdent`]

              let `updatedRowEntIdent` = `sys`.groups[`rowIdent`].entity
              # Update the index to `newRow` (which equals `rowIdent`).
              `setIndex`

            `sys`.nextFreeIdx -= 1

  let delIndex = sys.indexDel(entIdIdent, idxFmt)
  let r = quote do:
    if `sysRowExists`:
      `delIndex`
      `trimGroup`
  r


proc userRemoveFromEntity*(id: EcsIdentity, ent, compRef: NimNode, typeId: ComponentTypeId): NimNode =
  # Remove events code by component.
  let
    isOwned = id.systemOwner(typeId) != InvalidSystemIndex
    tyName = id.typeName(typeId)

    onRemoveFromEnt = id.onRemoveFromEntCodeNode(typeId)
    onRemoveCallback = id.onRemoveCallbackNode(typeId)
  
  var userEvents = newStmtList()

  if not isOwned and onRemoveFromEnt.len > 0:
    if onRemoveFromEnt.len > 0:
      userEvents.add newBlockStmt(onRemoveFromEnt)

  if onRemoveCallback.len > 0:
    let
      cbProcName = ident removeCallbackName(tyName)
      tyInstance = ident instanceTypeName(tyName)
    
    userEvents.add(quote do:
      `cbProcName`(`ent`, `tyInstance`(`compRef`.index)))

  if userEvents.len > 0:
    let tyInstance = ident instanceTypeName tyName
    quote do:
      block:
        ## Current component being removed from entity.
        template curComponent: untyped {.used.} = `tyInstance`(`compRef`.index)
        `userEvents`
  else:
    newStmtList()


proc userSysRemoved*(id: EcsIdentity, systemIndex: SystemIndex, sys, rowIdent, entIdIdent, entity: NimNode, sysOpts: ECSSysOptions): NimNode =
  ## Add user events for being removed from system.
  let
    sysName = id.getSystemName(systemIndex)
    assertItem =
      if id.assertItem(systemIndex): quote do:
        if `rowIdent` > `sys`.high:
          assert false,
            "'item' in " & `sys`.name & " is out of bounds. " &
            "Use of 'item' after remove/delete affected this system?"
      else:
        newStmtList()

    userAccessTmpls = quote do:
      ## Access to the current row's entity.
      template curEntity: untyped {.used.} = `sys`.groups[`rowIdent`].entity
      ## Current system item.
      template item: untyped {.used.} =
        `assertItem`
        `sys`.groups[`rowIdent`]
      ## Currently updating system variable.
      template sys: untyped {.used.} = `sys`
      template curSystem: untyped {.used.} = `sys`
      template groupIndex: untyped {.used.} = `rowIdent`

  var userRemovedEvents = newStmtList()

  if id.len_onRemoved(systemIndex) > 0:
    # Add code defined in the system's `removed:` section.
    let
      doEcho =
        if sysOpts.echoRunning != seNone:
          quote do:
            echo `sysName` & " removing entry event"
        else:
          newEmptyNode()

      sysRemovedEvent = id.onRemovedNode(systemIndex)

    userRemovedEvents.add(quote do:
      block:
        `doEcho`
        `sysRemovedEvent`
    )

  if id.len_onRemovedCallback(systemIndex) > 0:
    # Add code defined in the system's `removedCallback:` section.
    let 
      doEcho =
        if sysOpts.echoRunning != seNone:
          quote do:
            echo `sysName` & " removing entry callback event"
        else:
          newEmptyNode()

      sysRemovedEventCB =
        if id.len_onRemovedCallback(systemIndex) > 0:
          let eventProc = ident systemRemovedCBName(sysName)
          quote do:
            `eventProc`(`sys`, `rowIdent`)
        else:
          newStmtList()

    userRemovedEvents.add(quote do:
      block:
        `doEcho`
        `sysRemovedEventCB`
    )
  
  # Call relevant events for components leaving the system.
  for typeId in id.ecsSysRequirements(systemIndex):
    let
      typeName = id.typeName(typeId)
      fieldIdent = ident typeName.toLowerAscii()
      
      onRemoveAny = id.onRemoveAnySystemCodeNode(typeId)
      onRemoveFromEntCode = id.onRemoveFromEntCodeNode(typeId)
      onFinalisationCode = id.onFinalisationCodeNode(typeId)
      userSysRemFrom = id.onRemoveFromCodeNode(systemIndex, typeId)

      curCompTemplate =
        quote do:
          ## Component being removed from this system.
          template curComponent: untyped {.used.} = `sys`.groups[`rowIdent`].`fieldIdent`

    if userSysRemFrom.len > 0:
      userRemovedEvents.add(quote do:
        block:
          `curCompTemplate`
          `userSysRemFrom`
      )

    if onRemoveAny.len > 0:
      let sysRem = onRemoveAny
      if sysRem.len > 0:
        userRemovedEvents.add(quote do:
          block:
            `curCompTemplate`
            `sysRem`
        )

    if id.systemOwner(typeId) == systemIndex:
      # This system owns the component.
      # For owned components the entity removal event happens in-place
      # as it will soon be destroyed.
      if onRemoveFromEntCode.len > 0:
        let compEvent = onRemoveFromEntCode
        if compEvent != nil:
          userRemovedEvents.add(quote do:
            block:
              `curCompTemplate`
              `compEvent`
          )
      if onFinalisationCode.len > 0:
        # Only owner systems are finalised here, non-owned components
        # may be used outside of an entity and their finalisation is
        # called within the component's delete proc.
        let compEvent = onFinalisationCode
        if compEvent != nil:
          userRemovedEvents.add(quote do:
            block:
              `curCompTemplate`
              `compEvent`
          )

  if userRemovedEvents.len > 0:
    quote do:
      `userAccessTmpls`
      `userRemovedEvents`
  else:
    newStmtList()


# Type utils

iterator commaSeparate*[T: ComponentTypeId or SystemIndex](id: EcsIdentity, list: seq[T] or set[T]): string =
  ## Common function to produce a string of comma separated ComponentTypeIds.
  var comma: bool
  for v in list:
    let str =
      when T is ComponentTypeId: id.typeName v
      else: id.getSystemName v
    if comma: yield ", " & str
    else:
      comma = true
      yield str

proc commaSeparate*[T: ComponentTypeId or SystemIndex](id: EcsIdentity, list: seq[T] or set[T]): string {.compileTime.} =
  ## Common function to produce a string of comma separated ComponentTypeIds.
  for s in id.commaSeparate(list): result &= s

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

iterator typeDefs*(body: NimNode): NimNode =
  ## Return the nnkTypeDef nodes in a body.
  for item in body:
    if item.kind == nnkTypeSection:
      for def in item:
        if def.kind == nnkTypeDef:
          yield def

proc findType*(compNode: NimNode): string =
  ## Expects a typed node and tries to extract the type name.
  ## Note: converts typedesc[Type] to Type.
  result = case compNode.kind
    of nnkObjConstr:
      # Defined inline
      compNode.expectMinLen 1

      if compNode[0].kind == nnkDotExpr:
        compNode[0][1].strVal
      else:
        compNode[0].strVal
    of nnkSym:
      let tyInst = compNode.getTypeInst()
      if tyInst.kind == nnkBracketExpr and tyInst[0].kind == nnkSym and
          tyInst[0].strVal.toLowerAscii == "typedesc":
        # Extract T from typedesc[T].
        tyInst[1].expectKind nnkSym
        tyInst[1].strVal
      else:
        tyInst.strVal

    of nnkCall:
      let caller = compNode[0].getImpl()
      caller.expectKind nnkProcDef
      let callerTypeStr = $caller[3][0]
      callerTypeStr
    else:
      $(compNode.getTypeInst())

proc componentListToSet*(id: EcsIdentity, componentIds: seq[ComponentTypeId], setType: NimNode): NimNode =
  # Add to entity set in one go.
  result = nnkCurly.newTree()
  # Add the exists flags, cast to the components enum
  for ty in componentIds:
    result.add ident "ce" & id.typeName(ty)

proc typeStringToId*(id: EcsIdentity, n: string): ComponentTypeId {.compileTime.} =
  ## Returns an index for a type string, if found.
  # TODO: String based checks of types with the same name will return the same ComponentTypeId.
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

proc toTypeList*(id: EcsIdentity, componentList: NimNode): seq[ComponentTypeId] =
  ## Convert a list of nodes, such as from varargs[untyped], to a list of type ids.

  template process(compNode: NimNode, i: int) =
    let tyName = compNode.findType
    if tyName == "":
      error "Cannot find the type '" & compNode.repr & "' in identity \"" & id.string & "\""
    let typeId = id.typeStringToId(tyName)

    if typeId == InvalidComponent:
      error "Cannot resolve type id for '" & tyName & "'"
    
    if typeId in result:
      error "Passed more than one component of type " & tyName
    
    result[i] = typeId

  if componentList.kind == nnkIdent or componentList.kind == nnkSym:
    result.setLen 1
    componentList.process(0)
  else:
    result.setLen componentList.len
    for i, node in componentList:

      node.process(i)

  assert result.len > 0
  
type FieldList* = seq[tuple[fieldNode, typeNode: NimNode]]

proc toIdent(nn: NimNode): NimNode =
  # Replace symbols with idents
  if nn.kind == nnkSym: ident nn.strVal
  else: nn

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

proc genFieldAssignment*(fieldName: string, public: bool, fieldValue: NimNode): NimNode =
  ## To build the assignment part of `var x = y` statements.
  let nameIdent = if public:
      postFix(newIdentNode fieldName, "*")
    else:
      newIdentNode fieldName

  nnkIdentDefs.newTree(
    # Name of field within type
    nameIdent,
    # Field type definition
    newEmptyNode(),
    fieldValue
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
      

import os

const
  # Runtime logging.
  loggingOn* = false
  consistencyChecking* = false
  defaultGenLogFilename* = getProjectPath() / "ecs_code_log.nim"

when loggingOn:
  const runtimeLogFilename = getProjectPath() / "component_runtime_log.txt"
  var runtimeLogFile = open(runtimeLogFilename, fmWrite)
  proc closeLog*() {.noconv.} =
    runtimeLogFile.close
  addQuitProc(closeLog)  

macro log*(params: varargs[untyped]): untyped =
  result = newStmtList()
  when loggingOn:
    let s = genSym(nskVar, "s")
    result.add(quote do:
      var `s` = "")
    for item in params:
      result.add(quote do: `s` &= $`item`)
    result.add(quote do:
      runtimeLogFile.writeLine(`s` & "\n")
    )

#-------------------------------------------------------------------------
# Compile time logging to macrocache to be written as run time file output
#-------------------------------------------------------------------------

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
  assert clauses.len > 0

  var parent = newEmptyNode()
  for c in clauses:
    if parent.kind == nnkEmpty:
      parent = c
    else:
      parent = infix(parent, connector, c)
  parent

proc genTypeClass*(name: string, public: bool, nodeList: openarray[NimNode]): NimNode =
  ## Generates a typeclass definition for registered types.
  ## This is used for example in addComponent to restrict the generic parameter.
  ## eg;
  ## type
  ##   ComponentTypeClass = Test | Test2 | Test3
  assert nodeList.len > 0, "No type nodes defined to generate type class"

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

proc toSeq*[T](v: HashSet[T]): seq[T] =
  result.setLen v.len
  var i: int
  for item in v:
    result[i] = item
    i += 1

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
  ## var, let, and const sections, and macros.
  if code.len > 0 and suppressUnusedHints:
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

  if code.len > 0 and suppressUnusedHints:
    code.add(quote do:
      {.pop.}
    )
