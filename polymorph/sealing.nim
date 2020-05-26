import
  macros, sharedtypes, private/[utils, ecsstateinfo], components, entities, #systems,
  statechanges, typetraits, runtimeconstruction
import tables, strutils
export
  onAddCallback, onRemoveCallback,
  onAdd, onRemove, onInit, onInterceptUpdate,
  onSystemAdd, onSystemAddTo, onSystemRemove, onSystemRemoveFrom

proc maxEntLen(options: ECSEntityOptions): NimNode =
  ## Returns a statement that gets the maximum number of entities based on options.
  let ecStateVarIdent = ident(entityStorageVarName())
  case options.entityStorageFormat
  of esSeq: newEmptyNode()
  of esArray:
    quote do: `ecStateVarIdent`.entityComponents.len
  of esPtrArray:
    quote do: `ecStateVarIdent`.entityComponents[].len

proc entAccess(options: ECSEntityOptions, entIdent: NimNode): NimNode =
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

proc makeEntityState(options: ECSEntityOptions): NimNode =
  ## Instantiate the variable that holds the entity component state information,
  ## and generate entity utility procs.
  let
    # TODO: Allow multiple entity states by parametrising `entityStorageTypeName`.
    ecStateVarIdent = ident(entityStorageVarName())
    eqIdent = nnkAccQuoted.newTree(ident "==")
    initEntityStorageType = ident initEntityStorageTypeName()
    storageType = ident entityStorageTypeName()
    entIdNode = ident "entityId"
    entAccess = entAccess(options, entIdNode)
    typeClass = ident typeClassName()

  result = newStmtList()

  result.add(quote do:
    # State definition
    var `ecStateVarIdent`: `storageType`
    `initEntityStorageType`(`ecStateVarIdent`)

    # Quick access to the entity's object without implying a copy
    template entityData*(`entIdNode`: EntityId): untyped = `entAccess`
    proc lastEntityId*: EntityId = (`ecStateVarIdent`.nextEntityId.IdBaseType - 1.IdBaseType).EntityId

    proc `eqIdent`*(eRef: EntityRef, e: EntityId): bool {.inline.} =
      eRef.entityId.IdBaseType == e.IdBaseType and
        eRef.instance.IdBaseType == entityData(e).instance.IdBaseType
    proc isCurrent*(eRef: EntityRef): bool =
      # Only checks instances
      eRef.instance.IdBaseType == entityData(eRef.entityId).instance.IdBaseType
    template `eqIdent`*(live: EntityId, eRef: EntityRef): bool =
      # Useful if you need to compare a reference to the live entity
      eRef.entityId.IdBaseType == live.IdBaseType and
        eRef.instance.IdBaseType == entityData(live).instance.IdBaseType
    proc instance*(e: EntityId): EntityInstance {.inline.} = entityData(e).instance
    proc instance*(e: EntityRef): EntityInstance {.inline.} = entityData(e.entityId).instance
    ## Generate a reference to a particular instance of an entity.
    proc makeRef*(entityId: EntityId): EntityRef {.inline.} = (entityId, entityData(entityId).instance)
    proc entityCount*: int = `ecStateVarIdent`.entityCounter
    ## Alive checks the entity id (the slot, not instance) is valid (not NO_ENTITY) and that it's index has been initialised.
    template alive*(entity: EntityId): bool = entity.valid and entityData(entity).setup
    ## For an EntityRef, alive checks that the instance matches the referenced entity, ie; if
    ## the entity has been deleted/recreated since the reference was made, as well as checking
    ## if the entity itself is valid and initialised.
    template alive*(entRef: EntityRef): bool =
      entRef.entityId.alive and entityData(entRef.entityId).instance.int == entRef.instance.int
    template components*(entity: EntityRef, index: int): untyped =
      ## Access to entity's components.
      assert entity.alive
      entityData(entityId).componentRefs[index]
    template withComponent*(entity: EntityRef, t: typedesc[`typeClass`], actions: untyped): untyped =
      block:
        let component {.inject.} = entity.fetchComponent(t)
        actions
  )
  genLog "# Entity tools", result.repr

proc makeEntitySupport(entOpts: ECSEntityOptions): NimNode =
  let
    entity = ident "entity"
    entIdNode = ident "entityId"
    componentTypeId = ident "componentTypeId"
    typeClass = ident typeClassName()
    hasCore = 
      if entOpts.useSet:
        let setType = ident enumName()
        quote do:
          return `componentTypeId`.`setType` in entityData(`entIdNode`).exists
      else:
        case entOpts.componentStorageFormat
        of csTable:
          quote do:
            return entityData(`entIdNode`).componentRefs.hasKey(`componentTypeId`)
        of csSeq, csArray:
          quote do:
            for c in entityData(`entIdNode`).componentRefs:
              if c.typeId == `componentTypeId`:
                return true
  result = newStmtList()
  result.add(quote do:
    proc hasComponent*(`entity`: EntityRef, `componentTypeId`: ComponentTypeId): bool =
      let `entIdNode` = `entity`.entityId
      if not `entity`.alive:
        var str = "hasComponent on dead entity: " & $`entIdNode`.int & " instance " & $(`entIdNode`.instance.int)
        if `entIdNode` != `entity`:
          str &= " expected " & $`entity`.instance.int &
            " type " & $`componentTypeId`.int
        writeStackTrace()
        quit(str)
      if entityData(`entIdNode`).setup:
        `hasCore`
    template hasComponent*(entity: EntityRef, t: typedesc[`typeClass`]): untyped =
      # TODO: doesn't support components that aren't objects, eg `registerComponent: MyComp = seq[T]`.
      entity.hasComponent t.typeId
    template contains*(entity: EntityRef, `componentTypeId`: ComponentTypeId): bool = entity.hasComponent(`componentTypeId`)
    template contains*(entity: EntityRef, t: typedesc[`typeClass`]): untyped =
      entity.hasComponent(t.typeId)
  )

  case entOpts.componentStorageFormat
  of csSeq:
    result.add(quote do:
      iterator components*(entityId: EntityId): ComponentRef =
        # Iterate through components. Different methods are required for different storage strategies.
        for item in entityData(entityId).componentRefs:
          yield item
      iterator componentPairs*(entityId: EntityId): (int, ComponentRef) =
        # Iterate through components. Different methods are required for different storage strategies.
        for i, item in entityData(entityId).componentRefs.pairs:
          yield (i, item)
      proc componentCount*(entityId: EntityId): int = entityData(entityId).componentRefs.len
      proc componentCount*(entityRef: EntityRef): int = entityData(entityRef.entityId).componentRefs.len
    )
  of csArray:
    result.add(quote do:
      iterator components*(entityId: EntityId): ComponentRef =
        # Iterate through components. Different methods are required for different storage strategies.
        let length = entityData(entityId).nextCompIdx
        for i in 0 ..< length:
          yield entityData(entityId).componentRefs[i]
      iterator componentPairs*(entityId: EntityId): (int, ComponentRef) =
        # Iterate through components. Different methods are required for different storage strategies.
        let length = entityData(entityId).nextCompIdx
        for i in 0 ..< length:
          yield (i, entityData(entityId).componentRefs[i])
      proc componentCount*(entityId: EntityId): int = entityData(entityId).nextCompIdx
      proc componentCount*(entityRef: EntityRef): int = entityData(entityRef.entityId).nextCompIdx
    )
  of csTable:
    result.add(quote do:
      iterator components*(entityId: EntityId): ComponentRef =
        # Iterate through components. Different methods are required for different storage strategies.
        for item in entityData(entityId).componentRefs.values:
          yield item
      iterator componentPairs*(entityId: EntityId): (int, ComponentRef) =
        # Iterate through components. Different methods are required for different storage strategies.
        var i: int
        for item in entityData(entityId).componentRefs.values:
          yield (i, item)
          i += 1
      proc componentCount*(entityId: EntityId): int = entityData(entityId).componentRefs.len
      proc componentCount*(entityRef: EntityRef): int = entityData(entityRef.entityId).componentRefs.len
    )

  result.add(quote do:
    ## Component iterator by EntityRef
    template components*(entity: EntityRef): untyped = entity.entityId.components
  )

  genLog "# Entity access", result.repr

proc recyclerHasData(ecStateNode: NimNode, options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.len > 0
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      `ecStateNode`.`rLen` > 0

proc recyclerGet(ecStateNode: NimNode, options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.pop
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      let curIdx = `ecStateNode`.recycleLen - 1
      let r = `ecStateNode`.entityRecycler[curIdx]
      `ecStateNode`.`rLen` = curIdx
      r

proc makeFindComponent(entityId: NimNode, componentTypeId: ComponentTypeId, options: ECSEntityOptions): NimNode =
  ## Generate code fragment for fetching a ComponentTypeId depending on entity options.
  let
    returnType = ident instanceTypeName(typeInfo[componentTypeId.int].typeName)
    res = genSym(nskVar, "res")
  case options.componentStorageFormat
  of csTable:
    quote do: `returnType`(entityData(`entityId`).componentRefs.getOrDefault(`componentTypeId`.ComponentTypeId).index)
  of csSeq, csArray:
    # We have to search the component list one by one. For small lists, this may be faster than a table
    # due to data locality. Seq is usually in heap, is more indirected than array but is more memory efficient.
    # Ideally, we would have the most commonly looked up components closer to the start of the list (added first).
    # TODO: Further investigate the performance of ordering. initial investigations with a binary insert
    # seems to be slower in the overall case, but potentially could be improved by ordering type ids by
    # use or offering some priority tagging to keep those components closer to the start of lists.
    let
      length =
        if options.componentStorageFormat == csSeq:
          quote: entityData(`entityId`).componentRefs.len
        else:
          quote: entityData(`entityId`).nextCompIdx
    quote do:
      var `res` = InvalidComponentRef
      for i in 0 ..< `length`:
        if entityData(`entityId`).componentRefs[i].typeId == `componentTypeId`.ComponentTypeId:
          `res` = entityData(`entityId`).componentRefs[i]
          break
      `returnType`(`res`.index)

proc makeFetchComponent(entOpts: ECSEntityOptions): NimNode =
  ## Generate `fetchComponent` for all types not processed so far.
  result = newStmtList()
  
  for typeId in ecsComponentsToBeSealed:
    var
      tyName = typeInfo[typeId.int].typeName
      typeNode = ident tyName
      instanceNode = ident instanceTypeName(tyName)
      tid = typeStringToId(tyName)
      entIdent = ident "entity"
      entIdIdent = ident "entityId"
      findComp = makeFindComponent(entIdIdent, tid, entOpts)
      # Needs to be an ident (rather than a sym if typed in the `quote`) to bind later.
      alive = ident "alive"

    assert tid != InvalidComponent
    result.add(quote do:
      proc fetchComponent*(`entIdent`: EntityRef, t: typedesc[`typeNode`]): `instanceNode` =
        ## Looks up and returns the instance of the component, which allows direct field access.
        ## Returns default no component index if the component cannot be found.
        ## Eg;
        ##   let comp = entity.fetchComponent CompType  # Will be of type CompTypeInstance
        ##   comp.x = 3 # Edit some supposed fields for this component.
        assert `entIdent`.`alive`, "Fetch component on dead entity. Entity ID: " & $`entIdent`.entityId.int &
          ", Instance: " & $`entIdent`.instance.int
        let `entIdIdent` = `entIdent`.entityId
        `findComp`
    )
  genLog "# Fetch component\n", result.repr

proc makeNewEntity(options: ECSEntityOptions): NimNode =
  result = newStmtList()

  let  entStorage = ident entityStorageVarName()
  # Perform relevant setup for the entity's component storage.
  let
    entityId = ident "entityId"
    compInit =
      case options.componentStorageFormat
      of csSeq:
        if options.entityStorageFormat == esPtrArray:
          quote do:
            GC_Ref(entityData(`entityId`).componentRefs)
        else:
          newEmptyNode()
      of csArray:
        quote do:
          entityData(`entityId`).nextCompIdx = 0
      of csTable:
        if options.entityStorageFormat == esPtrArray:
          quote do:
            GC_Ref(entityData(`entityId`).componentRefs)
            entityData(`entityId`).componentRefs.clear
        else:
          quote do:
            entityData(`entityId`).componentRefs.clear

  let
    # Get code for reading length
    (maxLenCheck, updateEntStorage) =
      case options.entityStorageFormat
      of esSeq:
        let extendSeq = quote do:
          `entStorage`.entityComponents.setLen `entStorage`.nextEntityId.int + 1
          `entStorage`.nextEntityId = `entStorage`.entityComponents.len.EntityId
        (newEmptyNode(), extendSeq)

      of esArray, esPtrArray:
        let
          maxEntLen = maxEntLen(options)
          lenCheck = quote do:
            # nextEntityId is recycled to zero when delete detects entityCounter == 0.
            if `entStorage`.nextEntityId.IdBaseType >= `maxEntLen`:
              writeStackTrace()
              raise newException(EntityOverflow, "Exceeded entity limit: newEntity's maximum entity count is " & $(`maxEntLen` - 1))
          setNextId = quote do:
            `entStorage`.nextEntityId = (`entStorage`.nextEntityId.IdBaseType + 1).EntityId

        (lenCheck, setNextId)

    recyclerData = entStorage.recyclerHasData(options)
    recyclerGet = entStorage.recyclerGet(options)
    setOp =
      if options.useSet: quote do:
        entityData(`entityId`).exists = {}        
      else: newEmptyNode()

  result.add(quote do:
    proc newEntity*: EntityRef =
      var `entityId`: EntityId
      if `recyclerData`:
        `entityId` = `recyclerGet`
      else:
        `maxLenCheck`
        `entityId` = `entStorage`.nextEntityId
        `updateEntStorage`
      
      # Check we're not overwriting a live entity
      assert entityData(`entityId`).setup == false, "Overwriting EntityId = " & $`entityId`.int & " counter = " & $`entStorage`.entityCounter

      `entStorage`.entityCounter += 1
      # set up the new entry with this component
      `setOp`
      `compInit`

      entityData(`entityId`).setup = true
      # TODO: Handle overflow.
      let i = (entityData(`entityId`).instance.IdBaseType + 1).EntityInstance
      entityData(`entityId`).instance = i
      (`entityId`, i)
  )

  genLog "# New Entity", result.repr

import algorithm

template getComponentUpdatePerformance: seq[ComponentUpdatePerfTuple] =
  ## Internal procedure to return the number of systems each component accesses during an update.
  ## The result is sorted.
  ## Needs to be late bound as a template; as a proc it will bind when ecsComponentsToBeSealed is
  ## empty and not update.
  var r: seq[ComponentUpdatePerfTuple]
  let systemsByCompId = compSystems()
  for tId in ecsComponentsToBeSealed:
    # Each component gets a direct access addComponent
    let
      tyNameStr = typeInfo[tId.int].typeName
      relevantSystems = systemsByCompId[tId.int]
    r.add((tyNameStr, relevantSystems.len))

  r.sort do (x, y: ComponentUpdatePerfTuple) -> int:
    cmp(y.systemsUpdated, x.systemsUpdated)
  r

macro componentUpdatePerformance*: untyped =
  ## Generates a static sequence of components and the number of systems they update,
  ## ordered by increasing system accesses. This can effectively display the cost
  ## of performing `addComponent` or `removeComponent` and thus also `delete`.
  var items = getComponentUpdatePerformance()

  var bracket = nnkBracket.newTree
  for item in items:
    bracket.add(quote do: `item`)
  result = newStmtList(nnkPrefix.newTree(ident "@", bracket))

proc makeEntities(entOpts: ECSEntityOptions): NimNode =
  ## Create entity state.
  var r = newStmtList()
  if entOpts.entityStorageFormat != esSeq and entOpts.maxEntities <= 0:
    error "Cannot generate entities with a max count of zero as the entity storage format is non-resizable"
  
  let entTypes = makeEntityItems(entOpts)
  r.add entTypes
  r.add makeEntityState(entOpts)
  r.add makeEntitySupport(entOpts)
  r.add makeNewEntity(entOpts)
  r

# Runtime systems

proc makeRuntimeTools(entOpts: ECSEntityOptions): NimNode =
  ## These tools need to use `addComponent` generated in `commitSystems`.
  let
    res = ident "result"
    compCount = newIntLitNode ecsComponentsToBeSealed.len
    strOp = nnkAccQuoted.newTree(ident "$")
    # compInst matches the template provided by caseComponent.
    compName = ident "componentName"
  result = quote do:
    ###############
    #
    # Tools
    #
    ###############

    proc `strOp`*(componentId: ComponentTypeId): string =
      ## Display the name and id for a component type.
      componentId.caseComponent:
        `res` = `compName`() & " (" & $int(componentId) & ")"

    proc toString*(componentRef: ComponentRef, showData: bool = true): string =
      ## Display the name, type and data for a component reference.
      let tId = componentRef.typeId
      tId.caseComponent:
        `res` = `compName`() & " (id: " & $int(tId) & ", index: " & $componentRef.index.int & ", generation: " & $componentRef.generation.int & ")"
        if showData:
          `res` &= ": "
          try:
            `res` &= componentInstanceType()(componentRef.index.int).access.repr
          except:
            `res` &= "<Error accessing>\n"

    proc `strOp`*(componentRef: ComponentRef, showData: bool = true): string = componentRef.toString(showData)

    proc toString*(comp: Component, showData = true): string =
      ## `$` function for dynamic component superclass.
      ## Displays the sub-class data according to the component's `typeId`.
      caseComponent comp.typeId:
        result &= `compName`()
        if showData:
          result &= ":\n" & $componentRefType()(comp).value.repr & "\n"
    
    proc `strOp`*(comp: Component): string = comp.toString

    proc toString*(componentList: ComponentList, showData: bool = true): string =
      ## `$` for listing construction templates.
      let maxIdx = componentList.high
      for i, item in componentList:
        caseComponent item.typeId:
          let s = componentRefType()(item).toString(showData)
          if i < maxIdx and not showData:
            result &= s & ", "
          else:
            result &= s
    
    proc `strOp`*(componentList: ComponentList): string = componentList.toString

    proc toString*(construction: ConstructionTemplate, showData: bool = true): string =
      for i, item in construction:
        `res` &= $i & ": " & item.toString(showData) & "\n"

    proc `strOp`*(construction: ConstructionTemplate): string = construction.toString

    ## Count of components defined for this ECS.
    proc componentCount*: int = `compCount`

    template matchToSystems*(componentTypeId: ComponentTypeId, actions: untyped): untyped =
      # Match a runtime componentTypeId with it's systems. Has to check all systems at runtime, so is slow.
      # This is intended for aiding debugging.
      forAllSystems:
        if componentTypeId in system.requirements:
          actions

proc makeRuntimeDebugOutput: NimNode =
  let
    res = ident("result")
    entity = ident("entity")
    strProcName = nnkAccQuoted.newTree(ident("$"))
    totalCount = systemInfo.len
    tsc = ident("totalSystemCount")
    strOp = nnkAccQuoted.newTree(ident "$")

  # Build static array of system components.
  var ownedComponents = nnkBracket.newTree()
  for info in typeInfo:
    if info.systemOwner != InvalidSystemIndex:
      ownedComponents.add newDotExpr(newLit info.id.int, ident "ComponentTypeId")
  
  result = quote do:
    proc listComponents*(entity: EntityRef, showData = true): string =
      ## List all components attached to an entity.
      ## The parameter `showData` controls whether the component's data is included in the output.
      `res` = ""
      if entity.alive:
        let entityId = entity.entityId
        for compRef in entityId.components:
          let compDesc = `strOp`(compRef, showData)
          `res` &= compDesc
          if compRef.typeId in `ownedComponents`:
            if not compRef.alive:
              # $typeId returns the string of the storage type for this component.
              `res` &= " <DEAD OWNED COMPONENT Type: " & $compRef.typeId & " idx " & $int(compRef.index) & ">\n"
          else:
            if not compRef.valid:
              # $typeId returns the string of the storage type for this component.
              `res` &= " <INVALID COMPONENT Type: " & $compRef.typeId & " idx " & $int(compRef.index) & ">\n"
          if showData:
            # Helps separate large components
            `res` &= "----\n"
          else: `res` &= "\n"
      else: `res` &= "[Entity not alive, no component item entry]\n"

    proc `strProcName`*(`entity`: EntityRef, showData = true): string =
      ## `$` function for `EntityRef`.
      ## List all components and what systems the entity uses.
      ## By default adds data inside components with `repr`.
      ## Set `showData` to false to just display the component types.
      let
        comps = `entity`.listComponents(showData)
        systems = `entity`.listSystems()
        sys = if systems == "": "<No systems used>" else: systems
        invalidStr = if not `entity`.entityId.valid: " INVALID/NULL ENTITY ID" else: ""
      "[EntityId: " & $(`entity`.entityId.int) &
        " (generation: " & $(`entity`.instance.int) & ")" &
        invalidStr & "\nAlive: " & $`entity`.alive &
        "\nComponents:\n" & comps &
        "Systems:\n" & $sys & "]"

    proc `strProcName`*(`entity`: EntityId): string =
      ## Display the entity currently instantiated for this `EntityId`.
      `strProcName`(`entity`.makeRef)

    proc `strProcName`*(sysIdx: SystemIndex): string =
      ## Outputs the system name passed to `sysIdx`.
      caseSystem sysIdx:
        "System " & sys.name & " (" & $int(sysIdx) & ")"

    ## Total number of systems defined
    const `tsc`* = `totalCount`
  
  genLog "# Runtime debug output:\n", result.repr

proc makeListSystem: NimNode =
  var
    res = ident "result"
    innards = newStmtList()
    entIdent = ident "entity"
  innards.add(quote do: `res` = "")
  for sysInfo in systemInfo:
    
    if sysInfo.id notin ecsSystemsToBeSealed: continue
    
    let
      sysIdx = sysInfo.id.int
      options = systemInfo[sysIdx].options
      sys = sysInfo.instantiation
      name = sysInfo.systemName
      sysVar = name.systemVarName()
      entId = entIdent.newDotExpr(ident "entityId")
      hasKey = sys.indexHasKey(entId, options)
      sysNameBracketed = newLit " (" & sysVar & ")"
      reqs = sysInfo.requirements
    innards.add(quote do:
      var inSys = true
      for req in `reqs`:
        if not (req.ComponentTypeId in `entIdent`):
          inSys = false
          break
      if inSys:
        let
          sysName: string = `name`
        if `hasKey`:
          `res` &= sysName & `sysNameBracketed` & " \n"
        else:
          `res` &= sysName & `sysNameBracketed` & " Sync issue: entity contains components but entity is missing from this system's index)\n"
      )

  result = quote do:
    proc listSystems*(`entIdent`: EntityRef): string =
      if `entIdent`.alive:
        `innards`
      else:
        if `entIdent` == NO_ENTITY_REF:
          `res` = "<Entity is NO_ENTITY_REF>"
        else:
          `res` = "<Entity is not alive>"
  genLog "# Make list system:\n", result.repr

proc doStartLog: NimNode =
  if not logInitialised.hasKey(defaultGenLogFilename):
    logInitialised.add defaultGenLogFilename, true
    quote do: startGenLog(`defaultGenLogFilename`)
  else:
    newStmtList()

proc doWriteLog: NimNode =
  quote do: flushGenLog(`defaultGenLogFilename`)

proc makeCaseComponent(componentsToInclude: seq[ComponentTypeId]): NimNode =
  ## Generate caseComponent for the current component set.
  let
    actions = ident "actions"
    id = ident "id"
  var caseStmt = nnkCaseStmt.newTree()
  caseStmt.add quote do: `id`.int

  for component in componentsToInclude:
    let
      info = typeInfo.info(component)
      compIdx = info.id.int
    var
      ofNode = nnkOfBranch.newTree()
      compVal = newIntLitNode(compIdx)
    ofNode.add compVal
    let
      tyStr = typeInfo[component.int].typeName
      ty = newIdentNode tyStr
      tyRef = newIdentNode refTypeName(tyStr)
      tyInstance = newIdentNode instanceTypeName(tyStr)
      tyInit = newIdentNode createInstanceName(tyStr)
      tyRefInit = newIdentNode refInitName(info.refInitPrefix, tyStr)
      tyDel = newIdentNode deleteInstanceName()
      # reference the alive variable for this type.
      aliveStateIdent = newIdentNode aliveStateInstanceName(tyStr)
      # reference the storage object
      storageFieldIdent = newIdentNode storageFieldName(tyStr)
      tyInstanceIds = newIdentNode instanceIdsName(tyStr)
      sysIdx = newDotExpr(newLit info.systemOwner.int, ident "SystemIndex")
    var
      accessOwningSystem = newStmtList()
      isOwnedComponent: NimNode
    
    if info.systemOwner != InvalidSystemIndex:
      isOwnedComponent = newLit true
      let sysOwner = systemInfo[info.systemOwner.int].instantiation
      accessOwningSystem.add(quote do:
        template owningSystem: untyped {.used.} = `sysOwner`
      )
    else:
      isOwnedComponent = newLit false

    # Following templates are available for use within the case statement.
    # These aren't compiled in unless the invoker uses them.
    ofNode.add(quote do:
      template componentId: untyped {.used.} = `compIdx`.ComponentTypeId
      template componentName: untyped {.used.} = `tyStr`
      template componentType: untyped {.used.} = `ty`
      template componentRefType: untyped {.used.} = `tyRef`
      template componentInit: untyped {.used.} = `tyInit`
      template componentRefInit: untyped {.used.} = `tyRefInit`
      template componentDel(index: `tyInstance`): untyped {.used.} = `tyDel`(index)
      template componentAlive: untyped {.used.} = `aliveStateIdent`
      template componentGenerations: untyped {.used.} = `tyInstanceIds`
      template componentInstanceType: untyped {.used.} = `tyInstance`
      # Component data is similar to `access` but provides the whole array for you to use.
      # Eg; `echo componentData[myComponentRef.index.int].repr`
      template componentData: untyped {.used.} = `storageFieldIdent`
      template isOwned: bool {.used.} = `isOwnedComponent`
      template owningSystemIndex: SystemIndex {.used.} = `sysIdx`
      `accessOwningSystem`
      `actions`
    )
    caseStmt.add ofNode
  let elseCode = quote do:
    raise newException(ValueError, "Invalid component type id: " & $(`id`.toInt))
  caseStmt.add(nnkElse.newTree(elseCode))
  result = newStmtList()
  result.add(quote do:
    template caseComponent*(`id`: ComponentTypeId, `actions`: untyped): untyped =
      ## Creates a case statement that matches `id` with it's component.
      ##
      ## Note:
      ## * Has no concept of entity, this is a static case statement with injected
      ##   actions
      ## * the same action block is compiled for every choice, but you can use the
      ##   local `component` template to fetch any outer scope entities you wish
      ##   on the branch qualified type type.
      ##
      ## The following will display the statically compiled name and ComponentTypeId
      ## for every component.
      ##
      ## ```
      ## myCompId.caseComponent:
      ##   echo "Component Name: ", componentName
      ##   echo "Component: ", component.repr
      ## ```
      ##
      ## Within `actions`, the following templates provide typed access to the runtime index.
      ##
      ##   * componentId: the ComponentTypeId of the component
      ##   * componentName: string name
      ##   * componentType: static type represented by `id`
      ##   * componentInstanceType: index type, eg; MyComponentInstance
      ##   * componentRefType: ref type for this component, eg: MyComponentRef
      ##   * componentInit: initialiser procedure for this type
      ##   * componentRefInit: Ref initialiser procedure for this type
      ##   * componentDel: delete procedure for this type
      ##   * componentAlive: direct access to proc to test if this component is alive
      ##   * componentGenerations: direct access to the generation values for this type
      `caseStmt`
  )
  #genLog "# Component case:\n", result.repr

proc makeMatchSystem*(systemsToInclude: seq[SystemIndex]): NimNode =
  ## Creates a case statement that matches `id` with it's component
  ## This generates a runtime case statement that will perform `actions`
  ## for all systems like so:
  ##  case index
  ##    of 0: actions
  ##    of 1: actions
  ##    ... and so on for each system index
  ## `actions` is therefore executed using the correct `system` context
  ## for the runtime system. IE, if `index` = 7 then `system` will be 
  ## the instantiated variable for the seventh system.
  ## This allows you to write generic code that dynamically applies to any system
  ## chosen at runtime.
  let
    actions = ident "actions"
    index = ident "index"
  var body = newStmtList()
  var caseStmt = nnkCaseStmt.newTree()
  
  caseStmt.add quote do: `index`.int

  for sysId in systemsToInclude:
    let sysIdx = sysId.int
    var ofNode = nnkOfBranch.newTree()
    ofNode.add newIntLitNode(sysIdx)
    var
      curSys = systemInfo[sysIdx].instantiation
      curSysTuple = ident(systemInfo[sysIdx].systemName.tupleName)
    ofNode.add(quote do:
      template sys: untyped {.used.} = `curSys`
      template SystemTupleType: typedesc {.used.} = `curSysTuple`
      `actions`
    )
    caseStmt.add ofNode
  let elseCode = quote do: raise newException(ValueError, "Invalid system index: " & $`index`.int)
  caseStmt.add(nnkElse.newTree(elseCode))
  body.add caseStmt

  var allSysBody = newStmtList()
  for i in 0 ..< systemsToInclude.len:
    let
      curSysIdx = systemsToInclude[i]
      curSys = systemInfo[curSysIdx.int].instantiation
      curSysTuple = ident(systemInfo[curSysIdx.int].systemName.tupleName)
    allSysBody.add(quote do:
      block:
        template sys: untyped {.used.} = `curSys`
        template SystemTupleType: typedesc {.used.} = `curSysTuple`
        `actions`
    )

  result = quote do:
    template caseSystem*(`index`: SystemIndex, `actions`: untyped): untyped =
      `body`

    template forAllSystems*(`actions`: untyped): untyped =
      ## This will perform `actions` for every system.
      ## Injects the `sys` template for easier operation.
      `allSysBody`

  genLog "# System matchers:\n", result.repr

proc makeCompRefAlive: NimNode =
  quote do:
    template alive*(compRef: ComponentRef): bool =
      ## Check if this component ref's index is still valid and active.
      ## Requires use of run-time case statement to match against type id.
      let index = compRef.index.int
      caseComponent compRef.typeId:
        componentAlive()[index] and compRef.generation.int == componentGenerations()[index]

proc sealComps(entOpts: ECSEntityOptions): NimNode =
  assert ecsComponentsToBeSealed.len > 0, "No components defined"

  result = newStmtList()
  result.add makeRemoveComponentDirect(entOpts)

template addPerformanceLog =
  ## Append system operations per component to the log.
  let commentStart = "# "
  genLog commentStart & "Performance:\n"
  let perf: seq[ComponentUpdatePerfTuple] = getComponentUpdatePerformance()
  for item in perf:
    if item.systemsUpdated == 0:
      genLog commentStart & item.componentType & ": <No systems using this component>"
    else:
      genLog commentStart & item.componentType & ": " & $item.systemsUpdated & " systems"
  genLog ""

proc sealEntities(entOpts: ECSEntityOptions): NimNode =
  result = newStmtList()
  result.add generateTypeStorage()
  result.add genTypeAccess()
  result.add makeEntities(entOpts)
  result.add makeCompRefAlive()
  result.add makeCaseComponent(ecsComponentsToBeSealed)
  result.add makeFetchComponent(entOpts)
  result.add makeDelete(entOpts)
  result.add makeNewEntityWith(entOpts)
  result.add makeAddComponents(entOpts)
  result.add makeMatchSystem(ecsSystemsToBeSealed)

proc sealRuntimeTools(entOpts: ECSEntityOptions): NimNode =
  result = newStmtList()
  result.add makeRuntimeTools(entOpts)
  result.add makeListSystem()
  result.add makeRuntimeDebugOutput()

macro makeEcs*(entOpts: static[ECSEntityOptions]): untyped =
  ## Seal all components, create access functions that allow adding/removing/deleting components,
  ## and instantiate entity storage.
  echo "Building ECS..."
  result = newStmtList()
  result.add doStartLog()
  addPerformanceLog()
  for info in typeInfo:
    if info.onAddCallbackForwardDecl.len > 0:
      result.add info.onAddCallbackForwardDecl
    if info.onRemoveCallbackForwardDecl.len > 0:
      result.add info.onRemoveCallbackForwardDecl

  result.add sealEntities(entOpts)
  result.add sealRuntimeTools(entOpts)
  result.add sealComps(entOpts)
  when defined(debugSystemPerformance):
    echo "Sealing complete."
  result.add makeRuntimeConstruction(entOpts)
  when defined(debugSystemPerformance):
    echo "Construction tools complete."

  for info in typeInfo:
    if info.onAddCallback.len > 0:
      result.add info.onAddCallback
    if info.onRemoveCallback.len > 0:
      result.add info.onRemoveCallback

  # Reset state for next ECS.
  ecsComponentsToBeSealed.setLen 0
  ecsSystemsToBeSealed.setLen 0
  
  # Macros such as newEntityWith and addComponents need to know about
  # the callbacks to insert them so we can't just clear them here.
  # Since the events are tied to type id, and component type ids are not
  # cleared, we can assume there will be no clashing upon further
  # events when registerComponents is invoked again.
  echo "ECS Built."
  result.add doWriteLog()

template makeEcs*(maxEnts: static[int] = defaultMaxEntities): untyped =
  const entOpts = ECSEntityOptions(maxEntities: maxEnts)
  makeEcs(entOpts)

macro flushGenLog*: untyped =
  newStmtList(doWriteLog())

macro clearGenLog*(fn: static[string]): untyped =
  newStmtList(doStartLog())
  
macro clearGenLog*: untyped =
  newStmtList(doStartLog())

import strutils

proc genRunProc(name: string): NimNode =
  let procName = ident name.toLowerAscii
  result = quote do:
    proc `procName`* =
      `runAllDoProcsNode`
  genLog "# Run proc \"" & $name & "\", " & $runAllDoProcsNode.len & " systems defined:\n", result.repr

macro commitSystems*(procName: static[string]): untyped =
  ## Output system do proc definitions at the call site.
  result = newStmtList()
  for info in systemInfo:
    if info.definition != nil:
      result.add info.definition

  if procName != "":
    # Generate wrapper proc.
    result.add genRunProc(procName)

  # Note that `allSystemsNode`, `sysNames` etc are NOT reset.
  # Reset call tree for this batch.
  runAllDoProcsNode = newStmtList()

  # Check for defined but not committed systems.
  var uncommitted: seq[string]
  for system in ecsSysDefined.keys:
    if system notin ecsSysBodiesAdded:
      uncommitted.add systemInfo[system.int].systemName
  
  if uncommitted.len > 0:
    var outputStr = uncommitted[0]
    for i in 1 ..< uncommitted.len:
      outputStr &= ", " & uncommitted[i]
    echo "Warning: Systems are defined that do not have bodies: ", outputStr

  genLog "# Commit systems:\n" & procName, result.repr

  result.add doWriteLog()
