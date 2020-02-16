#[
  Everything in this module is exported to the user.
]#

type
  # Base type for all ids.
  IdBaseType* = int32
  ## Used to look up systems in allSystemsNode
  SystemIndex* = distinct int

  # This ideally needs to be redefined as the set size is.
  # stored value as the minimum set size and type cast to it everywhere from
  # user provided ComponentTypeIds.
  ComponentTypeIDBase* = uint16
  ComponentTypeId* = distinct ComponentTypeIDBase

  # Entity storage options
  ECSCompStorage* = enum csSeq, csArray, csTable
  ECSCompDuplicates* = enum cdAssertRaise, cdRaise
  ECSEntityItemStorage* = enum esSeq, esArray, esPtrArray
  ECSRecyclerFormat* = enum rfSeq, rfArray
  ECSEntityOptions* = object
    ## Controls the maximum amount of entities to be instantiated at any one time.
    maxEntities*: Natural
    ## Choose format of component list in entity items.
    componentStorageFormat*: ECSCompStorage
    ## Choose between stack allocated or heap allocated array.
    entityStorageFormat*: ECSEntityItemStorage
    ## Only applies to csArray storage format.
    maxComponentsPerEnt*: Natural
    ## Array access should be much faster, but takes up more space.
    recyclerFormat*: ECSRecyclerFormat
    ## Use a set for hasComponent.
    ## Note: sets can dramatically increase entity header size with a lot of components,
    ## as each `8` components defined will double the size of the set in bytes.
    ## Operations like creating, adding or removing are also slightly slower with
    ## a set due to the additional update work (~5% depending on component count).
    ## The advantage of using a set is that it allows an O(1) response for `hasComponent`
    ## when also using seq/array component lists.
    ## If you have seq/array component lists and don't want the extra memory burden
    ## of sets, you can ameliorate the O(N) cost of iteration for hasComponent by
    ## adding often checked/fetched components first, so finding them can return
    ## earlier.
    ## Component lists defined as a table will probably find the set unnecessary.
    ## Note that using hasComponent often can imply the need for a new system with
    ## that component which will remove this cost.
    # TODO: Add ability to mark components as priority for inserting them at the
    # start of an entity's list rather than the end.
    useSet*: bool
    ## Note that with cdRaise, the component list is searched for duplicates each
    ## time a component is added, even with release/danger.
    duplicates*: ECSCompDuplicates
  
  # Component storage options
  ECSAccessMethod* = enum amDotOp, amFieldTemplates
  ECSCompItemStorage* = enum cisSeq, cisArray
  ECSCompRecyclerFormat* = enum crfArray, crfSeq
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

  # System storage options
  ECSSysStorage* = enum ssSeq, ssArray
  ECSSysIndexFormat* = enum sifTable, sifArray, sifAllocatedSeq
  ECSSysTimings* = enum stNone, stRunEvery, stProfiling
  ECSSysOptions* = object
    ## Gets set automatically. Had to expose as setName can't see this field (compile time proc scope difference?)
    fName*: string 
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
    ## Debugging use: When true, echos a message for each stage (init/all/finish/completed) when a system runs.
    ## Useful for debugging blocking systems.
    echoRunning*: bool

  ComponentUpdatePerfTuple* = tuple[componentType: string, systemsUpdated: int]
  EntityOverflow* = object of Exception
  DuplicateComponent* = object of Exception

proc setName*(sysOpts: var ECSSysOptions, name: string) =
  sysOpts.fName = name

proc name*(sysOpts: ECSSysOptions): string = sysOpts.fName

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

