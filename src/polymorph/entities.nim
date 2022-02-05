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


import macros, tables, sharedtypes, private/[utils, ecsstatedb]


proc recyclerType(options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: seq[EntityId]
  of rfArray:
    let maxEnts = options.maxEntities
    quote do: array[0..`maxEnts`, EntityId]

proc makeEntityItems*(id: EcsIdentity, options: EcsEntityOptions): NimNode =
  ## Create the type that holds data per entity and supporting entity utilities.
  if options.maxEntities == 0:
    if options.entityStorageFormat in [esArray, esPtrArray]:
      error "Entity generation: maxEntities cannot be zero when using a fixed size storage format type such as " & $options.entityStorageFormat
    if options.componentStorageFormat == csArray and options.maxComponentsPerEnt == 0:
      error "Entity generation: maxComponentsPerEnt cannot be zero when using a fixed size storage format type for components such as " & $options.componentStorageFormat
    if options.recyclerFormat == rfArray:
      error "Entity generation: maxEntities cannot be zero when using a fixed size recycler format type such as " & $options.recyclerFormat

  let
    entityStorageType = entityStorageTypeName()
    entityStorageItemType = entityStorageItemTypeName()
    entityStorageContainerStr = entityStorageContainerTypeName()
    entityStorageContainerName = ident entityStorageContainerStr
    entityStorageIdent = ident entityStorageType
    entityStorageItem = ident entityStorageItemType
    componentRefsStr = "componentRefs"
    initParamName = ident "value"
    maxEntities = options.maxEntities

    initEntityStorageType = ident initEntityStorageTypeName()
    entContainerType = 
      case options.entityStorageFormat
      of esSeq: genSeq(entityStorageItem)
      of esArray: genArray(maxEntities + 1, entityStorageItem)
      of esPtrArray: nnkPtrTy.newTree(genArray(maxEntities + 1, entityStorageItem))

    # Code to initialise entity state.
    entCompInit =
      case options.entityStorageFormat
      of esSeq: newStmtList()
      of esArray: newStmtList()
      of esPtrArray:
        quote do:
          `initParamName`.entityComponents = cast[`entityStorageContainerName`](alloc0(sizeOf(`entityStorageContainerName`)))
    setType = ident componentsEnumName()
    recycler = recyclerType(options)

  # TODO: Check if already imported.
  result = quote do:
    type
      `entityStorageItem`* = object
        # The component indexes stored here are only used for fetchComponent and
        # reference.
        # You can use them to update component data via entity if needed.
        # Do not modify the list though, as this will put systems out of sync.
        # whether the entity is 'alive'.
        setup*: bool
        # instance is incremented each time a new entity is generated,
        # this allow testing to ensure the entityId you're referring to 
        # hasn't been deleted and recreated at this index.
        instance*: EntityInstance

      `entityStorageContainerName` = `entContainerType`

      `entityStorageIdent`* = object
        ## Stores the entity-component state data.
        # Entity Id index into entity state.
        entityComponents: `entityStorageContainerName`
        # internal counter of how many active entities.
        entityCounter: int
        # List of spare ids to use, updated when an entity is deleted or added.
        entityRecycler: `recycler`
        # newEntityId starting at FIRST_ENTITY_ID of 1 avoids 'empty' entityID bugs.
        nextEntityId: EntityId

    proc `initEntityStorageType`(`initParamName`: var `entityStorageIdent`) =
      ## Initialiser for entity state.
      `entCompInit`
      `initParamName`.nextEntityId = FIRST_ENTITY_ID

  # Extend storage type fields.
  var
    storageItemsFields = result.recList(entityStorageItemType)
    storageFields = result.recList(entityStorageType)

  doAssert storageItemsFields.kind == nnkRecList, "Internal error: Unexpected EntityStorageItem format"
  doAssert storageFields.kind == nnkRecList, "Internal error: Unexpected EntityStorageItem format"

  let setNode = quote do: set[`setType`]
  if options.useSet:
    storageItemsFields.add genField("exists", true, setNode)

  case options.recyclerFormat
  of rfSeq: discard
  of rfArray:
    let rLen = recyclerArrayLen()
    storageFields.add genField(rLen, false, ident "Natural")
  
  # TODO Split 'componentRefs' into the smallest value for typeIds (byte/word) and an indexed matched list component index.
  # Searching/fetching through typeId is common, we should be able to fit most component lists into a single cache line.

  case options.componentStorageFormat
  of csSeq:
    storageItemsFields.add genField(componentRefsStr, true, genSeq(ident "ComponentRef"))
  of csArray:
    assert options.maxComponentsPerEnt > 0, "options.maxComponentsPerEnt needs to be above zero to store components"
    storageItemsFields.add genField(componentRefsStr, true, genArray(options.maxComponentsPerEnt, ident "ComponentRef"))
    # The current largest index in the array + 1, equivalent to `len` for seq/table.
    storageItemsFields.add genField("nextCompIdx", false, ident "int")
  of csTable:
    storageItemsFields.add genField(componentRefsStr, true, quote do: Table[ComponentTypeId, ComponentRef])

