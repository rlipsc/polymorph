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

## This module contains the user accessible shared types used in the library.

# ECS generation options and types.
type
  ECSCompStorage* = enum csSeq, csArray, csTable
  ECSErrorResponse* = enum erAssert, erRaise
  ECSEntityItemStorage* = enum esSeq, esArray, esPtrArray
  ECSRecyclerFormat* = enum rfSeq, rfArray
  ECSErrorResponses* = object
    ## Note that with cdRaise, the component list is searched for duplicates each
    ## time a component is added, even with release/danger.
    errDuplicates*: ECSErrorResponse  # TODO
    errEntityOverflow*: ECSErrorResponse
    errCaseComponent*: ECSErrorResponse
    errCaseSystem*: ECSErrorResponse  # TODO
    errIncompleteOwned*: ECSErrorResponse

  ECSEntityOptions* = object
    ## Controls code generation for entities.
    maxEntities*: Natural ## Controls the maximum amount of entities for fixed size formats (ignored for esSeq).
    componentStorageFormat*: ECSCompStorage     ## Choose format of component list in entity items.
    entityStorageFormat*: ECSEntityItemStorage  ## Choose between stack allocated or heap allocated array.
    maxComponentsPerEnt*: Natural       ## Only applies to csArray storage format.
    recyclerFormat*: ECSRecyclerFormat  ## Array access should be much faster, but takes up more space.
    # Note: sets can dramatically increase entity header size with a lot of components,
    # as each `8` components defined will double the size of the set in bytes.
    # Operations like creating, adding or removing are also slightly slower with
    # a set due to the additional update work (~5% depending on component count).
    # The advantage of using a set is that it allows an O(1) response for `hasComponent`
    # when also using seq/array component lists.
    # If you have seq/array component lists and don't want the extra memory burden
    # of sets, you can ameliorate the O(N) cost of iteration for hasComponent by
    # adding often checked/fetched components first, so finding them can return
    # earlier.
    # Component lists defined as a table will probably find the set unnecessary.
    # Note that using hasComponent often can imply the need for a new system
    # incorporating these components.
    # TODO: Add ability to mark components as priority for inserting them at the
    # start of an entity's list rather than the end.
    useSet*: bool ## Use a set for hasComponent.
    errors*: ECSErrorResponses  # Control how errors are generated.

  # Component storage options
  ECSAccessMethod* = enum amDotOp
  ECSCompItemStorage* = enum cisSeq, cisArray
  ECSCompRecyclerFormat* = enum crfArray, crfSeq
  ECSCompInvalidAccess* = enum iaIgnore, iaAssert
  ECSCompOptions* = object
    ## The prefix added to init constructors, eg for "init"; `initMyComp`.
    initPrefix*: string
    ## The prefix added to reference init constructors destined for run-time construction, eg for "tmpl"; `tmplMyComp`.
    refInitPrefix*: string
    ## Maximum amount of components for all component types in this prefix.
    maxComponents*: Natural
    ## Underlying storage format for components.
    componentStorageFormat*: ECSCompItemStorage
    ## Controls generating `.` access (which on undeclared variables will show the matching `.` for all component types),
    ## or generating templates for get and set for all top-level fields in each component.
    accessMethod*: ECSAccessMethod
    ## Underlying storage format the recycler uses to keep track of deleted component indexes.
    ## TODO. Not currently functional.
    recyclerFormat*: ECSCompRecyclerFormat
    ## Zeros memory of component after deletion.
    clearAfterDelete*: bool
    ## Declare the component arrays as {.threadVar.}
    useThreadVar*: bool
    ## Allow inserting assert checks for each instance field access.
    invalidAccess*: ECSCompInvalidAccess

  # System storage options
  ECSSysStorage* = enum ssSeq, ssArray
  ECSSysIndexFormat* = enum sifTable, sifArray, sifAllocatedSeq
  ECSSysTimings* = enum stNone, stRunEvery, stProfiling
  ECSSysEcho* = enum seNone, seEchoUsed, seEchoUsedAndRunning, seEchoUsedAndRunningAndFinished, seEchoAll
  ECSSysOptions* = object
    ## Maximum entities this system can hold.
    maxEntities*: int
    ## Underlying storage format for the system groups.
    storageFormat*: ECSSysStorage
    ## sifArray = constant time deletes, uses maxEnts * ~8 bytes per system, uses stack space. Recommended.
    ## sifTable = performance degrades with entity count, adaptive memory use.
    indexFormat*: ECSSysIndexFormat
    ## Generate timing code.
    timings*: ECSSysTimings
    ## Declare systems as {.threadVar.}
    useThreadVar*: bool
    ## Reporting system execution state can be useful for debugging blocking systems or to monitor the sequence of system actions.
    echoRunning*: ECSSysEcho
    ## Add asserts to check `item` is within bounds.
    assertItem*: bool
    ## Maintains the execution order when items are removed from groups.
    ## This changes deletion from an O(1) to an O(N) operation.
    orderedRemove*: bool

  ComponentUpdatePerfTuple* = tuple[componentType: string, systemsUpdated: int]
  EntityOverflow* = object of OverflowDefect
  DuplicateComponent* = object of ValueError

const
  defaultMaxEntities* = 10_000
  defaultInitPrefix* = "init"
  defaultRefInitPrefix* = "tmpl"

func fixedSizeSystem*(ents: int): ECSSysOptions =
  ## Shortcut for fixed size, high performance, high memory systems.
  ECSSysOptions(
    maxEntities: ents,
    storageFormat: ssArray,
    indexFormat: sifArray)

func dynamicSizeSystem*: ECSSysOptions =
  ## Shortcut for systems that adjust dynamically.
  ECSSysOptions(
    storageFormat: ssSeq,
    indexFormat: sifTable)

func fixedSizeComponents*(maxInstances: int): ECSCompOptions =
  ## Shortcut for fixed size, high performance, high memory systems.
  ECSCompOptions(
    maxComponents: maxInstances,
    initPrefix: defaultInitPrefix,
    refInitPrefix: defaultRefInitPrefix,
    componentStorageFormat: cisArray,
    recyclerFormat: crfArray,
    )

func dynamicSizeComponents*: ECSCompOptions =
  ## Shortcut for systems that adjust dynamically.
  ECSCompOptions(
    initPrefix: defaultInitPrefix,
    refInitPrefix: defaultRefInitPrefix,
    componentStorageFormat: cisSeq,
    recyclerFormat: crfSeq,
  )

func fixedSizeEntities*(ents: int, componentCapacity = 0): ECSEntityOptions =
  ## Shortcut for fixed size, high performance, high memory systems.
  ECSEntityOptions(
    maxEntities: ents,
    componentStorageFormat: if componentCapacity == 0: csSeq else: csArray,
    maxComponentsPerEnt: componentCapacity,
    entityStorageFormat: esArray,
    recyclerFormat: rfArray
    )

func dynamicSizeEntities*: ECSEntityOptions =
  ECSEntityOptions(
    componentStorageFormat: csSeq,
    entityStorageFormat: esSeq,
    recyclerFormat: rfSeq
    )

const
  defaultComponentOptions* = dynamicSizeComponents()
  defaultEntityOptions* = dynamicSizeEntities()
  defaultSystemOptions* = dynamicSizeSystem()
  
  # Save some keystrokes.
  defaultCompOpts* = defaultComponentOptions
  defaultEntOpts* = defaultEntityOptions
  defaultSysOpts* = defaultSystemOptions

# Base Polymorph types.

type
  # Base type for all ids.
  IdBaseType* = int32
  ## Index representing a system.
  SystemIndex* = distinct int

  EntityId* = distinct IdBaseType
  EntityInstance* = distinct IdBaseType

  EntityRef* = tuple[entityId: EntityId, instance: EntityInstance]
  Entities* = seq[EntityRef]

  EntityChangeEvent* = enum
    eceNoChange,
    eceNewEntityWith, eceAddComponents,
    eceConstruct, eceClone,
    eceRemoveComponents, eceDelete

  # This ideally needs to be redefined as the set size is.
  # stored value as the minimum set size and type cast to it everywhere from
  # user provided ComponentTypeIds.
  ComponentTypeIDBase* = uint16
  ComponentTypeId* = distinct ComponentTypeIDBase

  Component* {.inheritable.} = ref object of RootObj
    ## This root object allows runtime templates of components to be constructed.
    ## `registerComponents` automatically generates a type descending from here for each component
    ## type.
    ## `typeId` has to match the valid componentTypeId for the type, and is automatically
    ## initialised by called the generated ref init proc for the type (by default tmplTypeName()).
    # Exposed fType id :( Required for currying parameters to ref inits and other set ups.
    fTypeId*: ComponentTypeId

  ## 'Generic' index into a component storage array.
  ## This is 'sub-classed' into distinct types per component type by registerComponents.
  ## These distinct versions of ComponentIndex allow direct access to component storage by
  ## transforming the type at compile time to an index into the storage array that contains the
  ## component.
  ## For run-time operations on component ids, use `caseComponent` and pass the ComponentTypeId
  ComponentIndex* = distinct IdBaseType
  ## Instance count, incremented when the slot is used.
  ## This is used to protect against referencing a deleted component with the same slot index.
  ComponentGeneration* = distinct IdBaseType
  ## Allows reference to particular instances of a component.
  ## Component references are how indexes/keys to different components are stored, passed about, and fetched.
  ## Not to be confused with the reference type `Component`.
  ComponentRef* = tuple[typeId: ComponentTypeId, index: ComponentIndex, generation: ComponentGeneration]

  ## Store a list of components, can be used as a template for constructing an entity.
  ## `add` is overridden for this type to allow you to add user types or instance types
  ## and their value is assigned to a ref container ready for `construct`.
  ComponentList* = seq[Component]
  ## A template for multiple entities
  ConstructionTemplate* = seq[ComponentList]

const
  InvalidComponent* = 0.ComponentTypeId
  InvalidComponentIndex* = 0.ComponentIndex
  InvalidComponentGeneration* = 0.ComponentGeneration
  InvalidComponentRef*: ComponentRef = (InvalidComponent, InvalidComponentIndex, InvalidComponentGeneration)
  InvalidSystemIndex* = SystemIndex(0)
  
  ## An EntityId of zero indicates uninitialised data
  NO_ENTITY* = 0.EntityId
  ## Reference of an invalid entity
  NO_ENTITY_REF*: EntityRef = (entityId: NO_ENTITY, instance: 0.EntityInstance)
  # Max number of entities at once
  # Note this is the maximum concurrent entity count, and
  # defines the amount of memory allocated at start up.
  FIRST_ENTITY_ID* = (NO_ENTITY.int + 1).EntityId
  FIRST_COMPONENT_ID* = (InvalidComponentIndex.int + 1).ComponentIndex

func `==`*(s1, s2: SystemIndex): bool {.inline.} = s1.int == s2.int

func `==`*(c1, c2: ComponentTypeId): bool = c1.int == c2.int
func `==`*(i1, i2: ComponentIndex): bool = i1.int == i2.int
func `==`*(g1, g2: ComponentGeneration): bool = g1.int == g2.int

## Entities start at 1 so a zero EntityId is invalid or not found
func `==`*(e1, e2: EntityId): bool {.inline.} = e1.IdBaseType == e2.IdBaseType
func `==`*(e1, e2: EntityRef): bool {.inline.} =
  e1.entityId.IdBaseType == e2.entityId.IdBaseType and e1.instance.IdBaseType == e2.instance.IdBaseType
template valid*(entityId: EntityId): bool = entityId != NO_ENTITY
template valid*(entity: EntityRef): bool = entity != NO_ENTITY_REF

# Construction.

type
  ## Constructor called on first create.
  ConstructorProc* = proc (entity: EntityRef, component: Component, master: EntityRef): seq[Component]
  ## Constructor called after all entities in a template have been constructed.
  PostConstructorProc* = proc (entity: EntityRef, component: ComponentRef, entities: var Entities)
  ## Constructor called when `clone` is invoked.
  CloneConstructorProc* = proc (entity: EntityRef, component: ComponentRef): seq[Component]

# Fragmentation analysis.

import stats

type
  ## This object stores information about the access pattern of a
  ## component from a system.
  ComponentAccessAnalysis* = object
    name*: string
    ## The minimum forward address delta to be included in `forwardJumps`.
    ## When zero anything larger than the size of the type is included.
    jumpThreshold*: int
    ## SizeOf(T) for the component data.
    valueSize*: int
    ## How many lookups go backwards per component that might cause fetching.
    backwardsJumps*: int
    ## How many jumps in forward access that might cause fetching.
    forwardJumps*: int
    ## Average information on the system access pattern for this component.
    allData*: RunningStat
    taggedData*: RunningStat
    ## The ratio of non-sequential vs sequential address accesses.
    fragmentation*: float

  ## Information about access patterns within a system obtained by `analyseSystem`.
  SystemAnalysis* = object
    name*: string
    entities*: int
    components*: seq[ComponentAccessAnalysis]
