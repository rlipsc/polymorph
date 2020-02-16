import macros, sharedtypes, private/utils, components, strutils, tables, typetraits, algorithm

macro componentToSysRequirements*(varName: untyped): untyped =
  ## Create a static sequence that matches componentTypeId to an array of indexes into systemNodes
  ## The result is an array that returns a seq of indexes into allSystemsNode by componentTypeId
  ##   `array[ComponentTypeId, seq[SystemIndex]]`
  ## This means we can check at run time what systems are required per component.
  ## Note this must only be used after seal has been called, otherwise the compile-time lists will be
  ## incomplete.
  let res = compSystems().genStaticArray()
  result = quote do:
    const `varName` = `res`
  genLog "# componentToSysRequirements:\n", result.repr

proc findType(compNode: NimNode): string =
  ## Expects a typed node and tries to extract the type name.
  const prefix = "FindType: "

  case compNode.kind
  of nnkObjConstr:
    # Defined inline
    compNode.expectMinLen 1

    if compNode[0].kind == nnkDotExpr:
      compNode[0][1].strVal
    else:
      compNode[0].strVal
  of nnkSym:
    let tyImpl = compNode.getTypeInst()
    $tyImpl
  of nnkCall:
    let caller = compNode[0].getImpl()
    caller.expectKind nnkProcDef
    let callerTypeStr = $caller[3][0]
    callerTypeStr
  else:
    error prefix & "adding components needs an object constructor like MyType(field: value) or an ident such as a variable, cannot process a " & $compNode.kind & "\n" & compNode.treerepr
    ""

proc indexRead(sysNode, entIdNode: NimNode, options: ECSSysOptions): NimNode =
  case options.indexFormat
  of sifTable:
    quote do:
      `sysNode`.index[`entIdNode`]
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int].row

proc indexWrite(sysNode, entIdNode, rowNode: NimNode, options: ECSSysOptions): NimNode =
  case options.indexFormat
  of sifTable:
    quote do:
      `sysNode`.index[`entIdNode`] = `rowNode`
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int] = (true, `rowNode`.Natural)

proc indexHasKey*(sysNode, entIdNode: NimNode, options: ECSSysOptions): NimNode =
  case options.indexFormat
  of sifTable:
    quote do:
      `sysNode`.index.hasKey(`entIdNode`)
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int].exists

proc indexDel(sysNode, entIdNode: NimNode, options: ECSSysOptions): NimNode =
  case options.indexFormat
  of sifTable:
    quote do:
      `sysNode`.index.del(`entIdNode`)
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int].exists = false

proc indexTryGet(sysNode, entIdNode, rowNode: NimNode, options: ECSSysOptions): NimNode =
  case options.indexFormat
  of sifTable:
    quote do:
      `rowNode` = `sysNode`.index.getOrDefault(`entIdNode`, -1)
      `rowNode` >= 0:
  of sifArray, sifAllocatedSeq:
    quote do:
      let rowData = `sysNode`.index[`entIdNode`.int]
      `rowNode` = rowData.row
      rowData.exists

proc componentListToSet(componentIds: seq[ComponentTypeId], setType: NimNode): NimNode =
  # Add to entity set in one go.
  result = nnkCurly.newTree()
  # Add the exists flags, cast to the components enum
  for id in componentIds:
    result.add ident "ce" & tNames[id.int]

proc genSystemUpdate(entity: NimNode, sys: SystemIndex): NimNode =
  ## Assumes you have a generated variable that matches each field in the system tuple already defined.
  let
    sysOpts = ecsSysOptions[sys.int]
    system = allSystemsNode[sys.int]

  # Generate system tuple assignment.
  var sysTuple = nnkPar.newTree()
  sysTuple.add nnkExprColonExpr.newTree(ident "entity", entity)
  for fields in sys.systemTypesStrPair:
    let
      tupleFieldStr = fields.typeName.toLowerAscii()
      tupleFieldIdent = ident tupleFieldStr
      compSource = ident tupleFieldStr & instPostfix
    sysTuple.add nnkExprColonExpr.newTree(tupleFieldIdent, compSource)

  # add the tuple of component indexes for this entity in this system.
  let updateGroup = case sysOpts.storageFormat
    of ssSeq:
      quote do: `system`.groups.add(`sysTuple`)
    of ssArray:
      quote do:
        `system`.groups[`system`.nextFreeIdx] = `sysTuple`
        `system`.nextFreeIdx += 1
    
  let
    entIdIdent = quote do: `entity`.entityId
    row = quote do: `system`.high
    updateIndex = system.indexWrite(entIdIdent, row, sysOpts)

  quote do:
    `updateGroup`
    `updateIndex`

proc entSetIncl(entOpts: ECSEntityOptions, entityId: NimNode, setVal: NimNode): NimNode =
  ## If `useSet` is true the set is updated with `setVal`,
  ## otherwise it does nothing.
  if entOpts.useSet:
    quote do:
      entityData(`entityId`).exists.incl `setVal`
  else: newEmptyNode()

proc entSetExcl(entOpts: ECSEntityOptions, entityId: NimNode, setVal: NimNode): NimNode =
  ## If `useSet` is true `setVal` is removed from the set,
  ## otherwise it does nothing.
  if entOpts.useSet:
    quote do:
      entityData(`entityId`).exists.excl `setVal`
  else: newEmptyNode()

proc addComponentRef(entity: NimNode, componentRef: NimNode, options: ECSEntityOptions): NimNode =
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
      entityData(`entity`.entityId).nextCompIdx = newIdx + 1
      assert newIdx < entityData(`entity`.entityId).componentRefs.len, "Exceeded entity component storage capacity of " &
        $entityData(`entity`.entityId).componentRefs.len & " with index " & $newIdx
      entityData(`entity`.entityId).componentRefs[newIdx] = `componentRef`

proc componentRefsLen*(entityIdIdent: NimNode, options: ECSEntityOptions): NimNode =
  # This returns the number of items in the entity's componentRefs list, however that may be stored.
  case options.componentStorageFormat:
  of [csSeq, csTable]:
    # Equivalent to entityData(entIdent.entityId).componentRefs.len
    # The above leads to `quote` inserting `entIdent` as a NimNode so we must build manually.
    newStmtList(
      newDotExpr(
        newDotExpr(
          newCall(newIdentNode("entityData"), entityIdIdent),
          newIdentNode("componentRefs")
        ),
        newIdentNode("len")
      )
    )
  of csArray:
    # The array book-keeps it's highest value.
    # Equivalent to entityData(entIdent.entityId).nextCompIdx
    newStmtList(
      newDotExpr(
        newCall(newIdentNode("entityData"), entityIdIdent),
        newIdentNode("nextCompIdx")
      )
    )

proc addCode(list: var seq[NimNode], typeIndex: int, actions: NimNode) =
  if list.len <= typeIndex:
    list.setLen typeIndex + 1
  if list[typeIndex] == nil:
    list[typeIndex] = newStmtList()
  list[typeIndex] = newStmtList(actions)

proc addCodeBlock(list: var seq[NimNode], typeIndex: int, actions: NimNode) =
  addCode(list, typeIndex, newBlockStmt(actions))

macro onAddCallback*(typeToUse: typedesc, actions: untyped): untyped =
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  let
    tyName = tNames[typeIndex]
    instTypeName = ident instanceTypeName(tyName)
    cc = ident "curComponent"
    cbProcName = ident sysAddCallbackName(tyName)
  addCallbackProcs.addCode(typeIndex, quote do:
      proc `cbProcName`(`cc`: `instTypeName`) =
        `actions`
  )
  addForwardDecls.addCode(typeIndex, quote do:
      proc `cbProcName`(`cc`: `instTypeName`)
  )
  result = newEmptyNode()

macro onRemoveCallback*(typeToUse: typedesc, actions: untyped): untyped =
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  let
    tyName = tNames[typeIndex]
    instType = ident instanceTypeName(tyName)
    cc = ident "curComponent"
    cbProcName = ident sysRemoveCallbackName(tyName)
  removeCallbackProcs.addCode(typeIndex, quote do:
      proc `cbProcName`(`cc`: `instType`) =
        `actions`
  )
  removeForwardDecls.addCode(typeIndex, quote do:
      proc `cbProcName`(`cc`: `instType`)
      )

  result = newEmptyNode()

# These inline hooks insert code at generation, and are therefore more limited in what they can do.

macro onInit*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  componentInitialisationCode.addCode(typeIndex, actions)
  result = newEmptyNode()

macro onInterceptUpdate*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a new component is instantiated,
  ## but before data has been added.
  ## The data being added can be accessed in `curComponent`, and is of
  ## the native type, not the instance type (the instance type is
  ## accessible with ).
  ## Each invocation will append to the code that will be inserted.
  ## Note: When this is hooked, the user must call update if they don't
  ## want the update parameters to be ignored.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  componentInterceptValueInitCode.addCodeBlock(typeIndex, actions)
  result = newEmptyNode()

macro onDelete*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component is deleted.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  componentFinalisationCode.addCodeBlock(typeIndex, actions)
  result = newEmptyNode()

macro onAdd*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is added to an entity.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  componentAddCode.addCodeBlock(typeIndex, actions)
  result = newEmptyNode()

macro onRemove*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from an entity.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  componentRemoveCode.addCodeBlock(typeIndex, actions)
  result = newEmptyNode()

macro onSystemAdd*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is added to any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  systemAddCode.addCodeBlock(typeIndex, actions)
  result = newEmptyNode()

macro onSystemAddTo*(typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let
    sysIndex = systemNames.find(systemName)
    typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  doAssert sysIndex != -1, "Cannot find system \"" & systemName & "\" in defined systems: " & $systemNames
  if systemAddToCode.len <= sysIndex:
    systemAddToCode.setLen sysIndex + 1
  systemAddToCode[sysIndex].add (typeIndex.ComponentTypeId, newBlockStmt(actions))
  result = newEmptyNode()

macro onSystemRemove*(typeToUse: typedesc, actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from any system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  systemRemoveCode.addCodeBlock(typeIndex, actions)
  result = newEmptyNode()

macro onSystemRemoveFrom*(typeToUse: typedesc, systemName: static[string], actions: untyped): untyped =
  ## Add some code to be executed when a component of this type is removed from this system.
  ## The system variable is provided by the `curSystem` template.
  ## Each invocation will append to the code that will be inserted.
  let
    sysIndex = systemNames.find(systemName)
    typeIndex = typeStringToId($typeToUse).int
  doAssert typeIndex != 0, "Cannot find type " & $typeToUse & " in registered components "
  doAssert sysIndex != -1, "Cannot find system \"" & systemName & "\" in defined systems: " & $systemNames
  if systemRemoveFromCode.len <= sysIndex:
    systemRemoveFromCode.setLen sysIndex + 1
  systemRemoveFromCode[sysIndex].add (typeIndex.ComponentTypeId, newBlockStmt(actions))
  result = newEmptyNode()

proc addUserSysCode(currentNode: var NimNode, sys: SystemIndex, typeId: ComponentTypeId) =
  let
    sysNode = allSystemsNode[sys.int]
    fieldName = tNames[typeId.int].toLowerAscii & instPostfix
    fieldIdent = ident fieldName
  var
    addTemplates: bool
    addedCode = newStmtList()

  if sys.int < systemAddToCode.len:
    # Check if matches a specific system and component type.
    for userData in systemAddToCode[sys.int]:
      if userData.typeId == typeId:
        let userAddToSys = userData.code

        addedCode.add(quote do:
          block:
            template curComponent: untyped {.used.} = `fieldIdent`
            template curSystem: untyped {.used.} = `sysNode`
            `userAddToSys`
        )

  if typeId.int < systemAddCode.len:
    # Check for this type's initialiser.
    let
      sysAdd = systemAddCode[typeId.int]
      sysNode = allSystemsNode[sys.int]
    if sysAdd != nil:
      if currentNode.len == 0: addTemplates = true
      addedCode.add(quote do:
        block:
          ## Access to current updating system variable.
          template curComponent: untyped {.used.} = `fieldIdent`
          template curSystem: untyped {.used.} = `sysNode`
          `sysAdd`
      )

  if addedCode.len > 0:
    currentNode.add addedCode

proc doNewEntityWith(entOpts: ECSEntityOptions, componentList: NimNode): NimNode {.compileTime.} =
  # Note: Currently does not generate a container and writes out the innards
  # within a block statement in the caller scope.
  # Pros: No call overhead when creating entities in a tight loop, and direct
  # mechanics manipulation.
  # Consequences: All ECS mechanics must be exposed to the user.
  #   * The user can potentially mess about with the internal entity state and corrupt the ECS.
  #   * The user can work with internal states and add features that need to process this info.
  result = newStmtList()
  let
    entity = genSym(nskVar, "entity")
    entIdNode = quote: `entity`.entityId
    systemsByCompId = compSystems()
  var
    componentDecl = nnkVarSection.newTree()
    paramUpdates = newStmtList()
    addToEntity = newStmtList()
    compIds: seq[ComponentTypeId]
    statements = newStmtList()
    userCompAddCode = newStmtList()

  for component in componentList:
    let
      # Search for the type name for this node
      tyName = component.findType
      # Convert to `ComponentTypeId`
      typeId = tyName.typeStringToId
      fieldName = tyName.toLowerAscii & instPostfix
      fieldIdent = ident fieldName
      # proc to instantiate this component type.
      newProc = ident createInstanceName(tyName)

    if typeId in compIds: error "newEntityWith has been passed more than one component of type " & tyName

    # For later use in updateSystems
    compIds.add typeId

    # Create storage for this component
    componentDecl.add genFieldAssignment(fieldName, false, newCall(newProc))
    # Update storage with parameter value
    paramUpdates.add(quote do:
      `fieldIdent`.update(`component`)
    )
    let compRef = quote do: `fieldIdent`.toRef
    addToEntity.add addComponentRef(entity, compRef, entOpts)

    # Component add user code.
    if typeId.int < componentAddCode.len:
      let compAdd = componentAddCode[typeId.int]
      if compAdd != nil:
        userCompAddCode.add(quote do:
          ## Current component being added to entity. 
          template curComponent: untyped {.used.} = `fieldIdent`
          `compAdd`
        )

    # User callback.
    if typeId.int < addCallbackProcs.len:
      # Check for this type's initialiser.
      let sysAddCallback = addCallbackProcs[typeId.int]
      if sysAddCallback != nil:
        let cbProcName = ident sysAddCallbackName(tyName)
        userCompAddCode.add(quote do:
          `cbProcName`(`fieldIdent`)
        )

  let setOp =
    if entOpts.useSet:
      let
        setType = ident enumName()
        setVal = componentListToSet(compIds, setType)
      entSetIncl(entOpts, entIdNode, setVal)
    else:
      newEmptyNode()

  var
    processed = initTable[int, bool]()
    userSysAddCode = newStmtList()

  statements.add(quote do:
    var `entity` = newEntity()
    `componentDecl`
    `paramUpdates`
    `addToEntity`
    `setOp`
  )
  
  for compId in compIds:
    # Get systems that use this component.
    let systems = systemsByCompId[compId.int]

    for sys in systems:
      if sys.int notin processed:
        processed.add(sys.int, true)

        # We only need to update systems that are fully qualified by componentList.
        let reqs = sysRequirements[sys.int]
        var discarded: bool
        for req in reqs:
          if req notin compIds:
            discarded = true

        if not discarded:
          # Add component to system.
          statements.add genSystemUpdate(entity, sys)
          # Add user code.
          userSysAddCode.addUserSysCode(sys, compId)

  statements.add(quote do:
    let res = `entity`
    template curEntity: EntityRef {.used.} = `entity`
    `userSysAddCode`
    `userCompAddCode`
    res
  )

  result = quote do:
    block:
      `statements`

  genLog "# newEntityWith:\n", result.repr

proc makeNewEntityWith*(entOpts: ECSEntityOptions): NimNode =
  let
    componentList = ident "componentList"
    #newEntCode = doNewEntityWith(entOpts, componentList)
  quote do:
    macro newEntityWith*(`componentList`: varargs[typed]): untyped =
      ## Create an entity with the parameter components.
      ## This macro statically generates updates for only systems
      ## entirely contained within the parameters and ensures no
      ## run time component list iterations and associated checks.
      #`newEntCode`
      doNewEntityWith(`entOpts`, `componentList`)

proc doAddComponents(entOpts: ECSEntityOptions, entity: NimNode, componentList: NimNode): NimNode =
  result = newStmtList()
  # Note, see notes on `newEntityWith`, chunks innards inside a block.
  let
    # Fetch array of component id by list of system indexes.
    systemsByCompId = compSystems()
    setType = ident enumName()
  var
    componentsPassed: seq[ComponentTypeId]
    componentValues: seq[NimNode]
    systemsToUpdate: seq[SystemIndex]
    componentsRequired: seq[ComponentTypeId]
    addToEntity = newStmtList()
    userCompAddCode = newStmtList()
    compListStr: string

  # Gather info for each component added.
  for compNode in componentList:
    let tyName = compNode.findType
    assert tyName != "", "Cannot determine type name of argument:\n" & compNode.treeRepr & "\ngetType:\n" & compNode.getType.repr
    let
      typeId = tyName.typeStringToId
      systems = systemsByCompId[typeId.int]

    # String of the parameter types for logging.
    if compListStr.len == 0: compListStr = tyName
    else: compListStr &= ", " & tyName

    for sys in systems:
      if sys notin systemsToUpdate:
        systemsToUpdate.add sys

    if typeId in componentsPassed: error "addComponents has been passed more than one component of type " & tyName
    componentsPassed.add typeId
    componentValues.add compNode

  # Create a list of components we're missing that would satisfy all systems that use our parameters.
  for sys in systemsToUpdate:
    let reqs = sysRequirements[sys.int]
    for req in reqs:
      if req notin componentsPassed and req notin componentsRequired:
        componentsRequired.add req

  var
    componentDecl = nnkVarSection.newTree()
    paramUpdates = newStmtList()
    fetchCase = nnkCaseStmt.newTree()
    fetchCompIdent = ident "comp"
    entIdIdent = quote do: `entity`.entityId
    distinguisher = quote do: `fetchCompIdent`.typeId.int
    checks = newStmtList()
  fetchCase.add distinguisher

  # Create instances for the components passed as parameters and update them.
  var returnType = nnkPar.newTree()

  for idx, comp in componentsPassed:
    let
      typeStr = tNames[comp.int]
      fieldName = typeStr.toLowerAscii & instPostfix
      newProc = ident createInstanceName(typeStr)
      value = componentValues[idx]
      # Return field
      tupleFieldStr = typeStr.toLowerAscii()
      dupeStr = newLit "Component type " & typeStr & " (" & $comp.int & ") already exists in this entity"
      dupeCheck: NimNode =
        case entOpts.duplicates
        of cdAssertRaise:
          quote do:
            when compileOption("assertions"):
              if `comp`.ComponentTypeId in `entity`:
                raise newException(DuplicateComponent, `dupeStr`)
        of cdRaise:
          quote do:
            if `comp`.ComponentTypeId in `entity`:
              raise newException(DuplicateComponent, `dupeStr`)
    
    # Check for duplicates.
    # If a duplicate component type is added it will overwrite the previous one in systems,
    # but the old one will still be first in the entity's component list and thus returned
    # from fetchComponent and deleted by removeComponent. HasComponent may also be incorrect
    # when deleting if it uses a set. The old component may also be 'leaked', in that it's index
    # will be occupied until that entity is deleted and all the entities components are purged.
    # This can lead to some hard to track down behaviour, so it's important to ensure that
    # duplicates are not allowed either in debug mode or release if you are happy to pay the cost,
    # and for some reason can't use addIfMissing/addOrUpdate.
    checks.add(quote do:
      `dupeCheck`
    )
    
    returnType.add nnkExprColonExpr.newTree(ident(tupleFieldStr), ident fieldName)

    # Create component storage
    componentDecl.add genFieldAssignment(fieldName, false, quote do:
      `newProc`()
    )
    # Update storage with parameter value
    let fieldIdent = ident fieldName
    paramUpdates.add(quote do:
      `fieldIdent`.update(`value`)
    )

    addToEntity.add(
      addComponentRef(entity, newDotExpr(fieldIdent, ident "toRef"), entOpts))

    # Component add user code.
    let typeId = typeStr.typeStringToId
    if typeId.int < componentAddCode.len:
      let compAdd = componentAddCode[typeId.int]
      if compAdd != nil:
        # TODO: Defining curEntity as a template like curComponent causes a compiler crash here.
        userCompAddCode.add(quote do:
          block:
            ## Current component being added to entity. 
            template curComponent: untyped {.used.} = `fieldIdent`
            `compAdd`
        )

    # User callback.
    if typeId.int < addCallbackProcs.len:
      # Check for this type's initialiser.
      let sysAddCallback = addCallbackProcs[typeId.int]
      if sysAddCallback != nil:
        let cbProcName = ident sysAddCallbackName(typeStr)
        userCompAddCode.add(quote do:
          `cbProcName`(`fieldIdent`)
        )

  let
    targetHigh = componentsRequired.len
    # Set up counter in case there's more than one component to fetch
    fcStr = "foundCounter"
    fc = ident fcStr

  if targetHigh > 1:
    # We have several components to fetch so we declare a counter for early exit.
    componentDecl.add genField(fcStr, false, ident "int")

  var fetches = newStmtList()
  if componentsRequired.len > 0:
    
    componentsRequired.sort do (x, y: ComponentTypeId) -> int:
      cmp(x.int, y.int)

    case entOpts.componentStorageFormat
    of csSeq, csArray:
      # Build case statement to fetch components in a single pass
      for comp in componentsRequired:
        let
          typeStr = tNames[comp.int]
          fieldName = typeStr.toLowerAscii & "Inst"
          instTypeIdent = ident(typeStr.instanceTypeName)
          fieldIdent = ident fieldName

        # Create variable for this component to update during loop
        componentDecl.add genField(fieldName, false, instTypeIdent)
        # Fetch
        let
          ofBranch = nnkOfBranch.newTree(newIntLitNode(comp.int))
          ofStmts = 
            if componentsRequired.len == 1:
              # With only one component to fetch we don't need to track count.
              newStmtList(quote do:
                `fieldIdent` = `instTypeIdent`(`fetchCompIdent`.index)
                break
              )
            else:
              newStmtList(quote do:
                `fieldIdent` = `instTypeIdent`(`fetchCompIdent`.index)
                `fc` += 1
                if `fc` > `targetHigh`: break
              )
        ofBranch.add ofStmts
        fetchCase.add ofBranch
      fetchCase.add nnkElse.newTree(newStmtList(quote do: discard))

      fetches.add(quote do:
        for `fetchCompIdent` in entityData(`entity`.entityId).componentRefs:
          `fetchCase`
      )
    of csTable:
      ## Tables can directly fetch components.
      for comp in componentsRequired:
        let
          typeStr = tNames[comp.int]
          instTypeIdent = ident(typeStr.instanceTypeName)
          fieldName = typeStr.toLowerAscii & "Inst"
          fieldIdent = ident fieldName
        fetches.add(quote do:
          let `fieldIdent` = `instTypeIdent`(entityData(`entity`.entityId).componentRefs.getOrDefault(`comp`.ComponentTypeId).index)
        )

  # Finally, update the actual systems.
  var updateSystems = newStmtList()
  for sys in systemsToUpdate:
    # TODO: Update to using genSystemUpdate. This actually does a bit more work because it adds
    # the alive checks per component so needs some of that collected info.
    var
      fieldUpdates = newStmtList()
      sysMatch: seq[NimNode]
    let
      sysOpts = ecsSysOptions[sys.int]
      system = allSystemsNode[sys.int]

      sysTupleName = systemNames[sys.int].tupleName
      sysTupleIdent = ident sysTupleName
      sysTuple = ident(sysTupleName.toLowerAscii)

    # add the tuple of component indexes for this entity in this system.
    let updateGroup = case sysOpts.storageFormat
      of ssSeq:
        quote do:
          `system`.groups.add(`sysTuple`)
      of ssArray:
        quote do:
          `system`.groups[`system`.nextFreeIdx] = `sysTuple`
          `system`.nextFreeIdx += 1

    # Build the tuple to be added to this system
    fieldUpdates.add(quote do:
      var `sysTuple`: `sysTupleIdent`
      `sysTuple`.entity = `entity`
      )

    var userSysAddCode = newStmtList()
      
    for field in sys.systemTypesStrPair:
      let
        tupleFieldStr = field.typeName.toLowerAscii()
        tupleFieldIdent = ident tupleFieldStr
        compSource = ident tupleFieldStr & instPostfix

      fieldUpdates.add(quote do: `sysTuple`.`tupleFieldIdent` = `compSource`)
      
      if field.id in componentsRequired:
        # Add this to our list of 'alive' checks.
        sysMatch.add(quote do: `compSource`.alive)

      # Include user code for adding to this system.
      userSysAddCode.addUserSysCode(sys, field.id)
    let
      hasKey = system.indexHasKey(entIdIdent, sysOpts)
      row = quote do: `system`.high
      updateIndex = system.indexWrite(entIdIdent, row, sysOpts)

      matchCode = quote do:
        `fieldUpdates`
        `updateGroup`
        `updateIndex`
        `userSysAddCode`

    # TODO: After this part (in this scope), we are doing stuff not handled by genSystemUpdate.
    # Fold this into genSystemUpdate or allow it to return the data we need here.
    # Currently componentsRequired is built from params, and not available to genSystemUpdate.
    let getIndex = system.indexRead(entIdIdent, sysOpts)
    checks.add(quote do:
      assert `hasKey` == false, "Adding: Entity already in system " & `system`.name & " Entity:\n" & $`system`.groups[`getIndex`].entity.repr & "\nEntry:" & $`system`.groups[`getIndex`]
      )

    if sysMatch.len > 0:
      # We have more than just the parameter component, check if the others are alive to match this system.
      let sysMatchClause = genInfixes(sysMatch, "and")
      updateSystems.add(newIfStmt( (sysMatchClause, matchCode) ))
    else:
      # Only the parameter components are used in this system so we proceed directly with the add code.
      updateSystems.add matchCode

  # Add to entity set in one go.
  var setVal = componentListToSet(componentsPassed, setType)
  let setOp = entSetIncl(entOpts, entIdIdent, setVal)
  
  result.add(quote do:
    block:
      `checks`
      ## Access to currently updating entity.
      template curEntity: EntityRef {.used.} = `entity`
      `componentDecl`
      `paramUpdates`
      `addToEntity`
      `setOp`
      `fetches`
      `updateSystems`
      `userCompAddCode`
      `returnType`
  )
  genLog "\n# macro addComponents(" & compListStr & "):", result.repr

proc makeAddComponents*(entOpts: ECSEntityOptions): NimNode =
  let
    componentList = ident "componentList"
    entity = ident "entity"
  quote do:
    macro addComponents*(`entity`: EntityRef, `componentList`: varargs[typed]): untyped =
      ## Generates efficient system updates for a set of components.
      ## Fetches are only performed if required components are not in the parameters.
      doAddComponents(`entOpts`, `entity`, `componentList`)

proc makeAddComponentDirect*(entOpts: ECSEntityOptions): NimNode =
  ## This macro adds `addComponent(entity, type)` for every component type.
  ## Generated code updates single components with the systems we know use that type
  ## at compile time.
  ## Unlike `addComponents` and `newEntityWith`, these procs only deal with one component at a time,
  ## but return the `ComponentRef` that's been added.
  result = newStmtList()
  let systemsByCompId = compSystems()
  var systemsEncountered: bool

  assert ecsComponentsToBeSealed.len > 0, "Cannot generate addComponent when no components have been defined"

  for tId in ecsComponentsToBeSealed:
    # Each component gets a direct access addComponent
    let
      typeId = tId.int
      tyNameStr = tNames[typeId]
      tyInstanceStr = instanceTypeName(tyNameStr)
      relevantSystems = systemsByCompId[typeId]
      tyIdent = ident tyNameStr
      tyInstanceIdent = ident tyInstanceStr
      entIdent = ident "entity"
      valIdent = ident "value"
      componentSysCount = relevantSystems.len
      # For generating useful comments for addComponent.
      sysNamePrefix = 
        if componentSysCount > 1:
          " This updates systems: "
        else:
          " This updates system: "
    var sysList: string

    if componentSysCount > 0:
      systemsEncountered = true

    for relevantSysIdx, systemIndex in relevantSystems:
      # Add update elements just for systems that use this component.
      let
        sysIdx = systemIndex.int
        sysName = systemNames[sysIdx]

      # Record which systems are affected to update addComponent's doc comment.
      if sysList.len == 0:
        sysList &= sysNamePrefix & sysName
      else:
        if relevantSysIdx == relevantSystems.high:
          sysList &= ", and " & sysName
        else:
          sysList &= ", " & sysName

    # Add a comment for each addComponent proc that shows info about the type it returns and systems it affects.
    let
      addComment = newCommentStmtNode(" Create storage for component and update, optionally returning " & tyInstanceStr & "." & sysList & ".")
      tupleField = ident tyNameStr.toLowerAscii()

    result.add(quote do:
      proc addComponent*(`entIdent`: EntityRef, `valIdent`: `tyIdent`): `tyInstanceIdent` {.discardable.} =
        `addComment`
        `entIdent`.addComponents(`valIdent`).`tupleField`

      proc addFetch*(`entIdent`: EntityRef, `valIdent`: `tyIdent`): ComponentRef =
        # create storage for component and update, returning the component reference.
        `entIdent`.addComponent `valIdent`
        (`entIdent`.fetchComponent `tyIdent`).toRef
      
      # TODO: addOrUpdate that allows granular updating fields rather than whole component item.
      proc addOrUpdate*(`entIdent`: EntityRef, `valIdent`: `tyIdent`): `tyInstanceIdent` {.discardable.} =
        ## This procedure allows you to forget about assertion failures due to duplicate adds.
        let fetched = `entIdent`.fetchComponent `tyIdent`
        if fetched.valid:
          # Replace original. No further work is required as the types or indexes have not been updated.
          fetched.update(`valIdent`)
          fetched
        else:
          # Add as normal.
          `entIdent`.addComponent `valIdent`
      
      proc addIfMissing*(`entIdent`: EntityRef, `valIdent`: `tyIdent`) =
        ## This procedure allows you to add a component only if it isn't already present.
        ## If the component is already present, no changes are made.
        if not `entIdent`.hasComponent(`tyIdent`):
          `entIdent`.addComponent `valIdent`
      )

  if not systemsEncountered:
    echo "Warning: No systems defined"

  genLog "# Adding with single components:\n", result.repr

proc removeComponentRef(entityId, index: NimNode, componentTypeId: int, options: ECSEntityOptions): NimNode = 
  # Removes a component from entity storage.
  # * Doesn't touch systems
  # * Doesn't update the intset for hasComponent.

  let remIdx =
    if options.componentStorageFormat == csTable:
      quote do: `componentTypeId`.ComponentTypeId
    else:
      newIntLitNode(componentTypeId)

  case options.componentStorageFormat
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

# RemoveComponent can also utilise the type as a static parameter to directly remove from only relevant systems.
proc removeSysReference(systemIndex: int, system, rowIdent, entIdIdent, entity: NimNode, sysOpts: ECSSysOptions): NimNode =
  # Remove an entity's tuple from a system.
  # * Does not update the entity's storage

  # Add user defined system remove code.
  var userSysRemoveCode = newStmtList()
  for typeId in sysRequirements[systemIndex]:
    let
      typeName = tNames[typeId.int]
      fieldIdent = ident typeName.toLowerAscii()

    if systemIndex < systemRemoveFromCode.len:
      for userData in systemRemoveFromCode[systemIndex.int]:
        if userData.typeId == typeId:
          let sysRem = userData.code
          if sysRem != nil:
            userSysRemoveCode.add(quote do:
              block:
                ## Access to the current system's row.
                template curRow: untyped {.used.} = `system`.groups[`rowIdent`]
                ## Access to current updating system variable.
                template curSystem: untyped {.used.} = `system`
                ## Component being removed from this system.
                template curComponent: untyped {.used.} = `system`.groups[`rowIdent`].`fieldIdent`
                `sysRem`
            )

    if typeId.int < systemRemoveCode.len:
      let sysRem = systemRemoveCode[typeId.int]
      if sysRem != nil:
        userSysRemoveCode.add(quote do:
          block:
            ## Access to current updating system variable.
            template curSystem: untyped {.used.} = `system`
            ## Component being removed from this system.
            template curComponent: untyped {.used.} = `system`.groups[`rowIdent`].`fieldIdent`
            `sysRem`
        )

  # Select seq/array trim.
  let
    topIdxIdent = ident "topIdx"
    updatedRowEntIdent = ident "updatedRowEnt"
    entId = quote do: `updatedRowEntIdent`.entityId
    # updates index with entity id to group row.
    setIndex = system.indexWrite(entId, rowIdent, sysOpts)

  let
    trimGroup = 
      case sysOpts.storageFormat
      of ssSeq:
        quote do:
          let `topIdxIdent` = `system`.high
          if `rowIdent` < `topIdxIdent`:
            `system`.groups[`rowIdent`] = `system`.groups[`topIdxIdent`]
            # Get entity that's been moved.
            let `updatedRowEntIdent` = `system`.groups[`rowIdent`][0]
            # Update the index for the moved row
            `setIndex`

          `system`.groups.setLen(`system`.groups.len - 1)
      of ssArray:
        quote do:
          let `topIdxIdent` = `system`.high
          if `rowIdent` < `topIdxIdent`:
            `system`.groups[`rowIdent`] = `system`.groups[`topIdxIdent`]
            # Get entity that's been moved.
            let `updatedRowEntIdent` = `system`.groups[`rowIdent`][0]
            # Update the index for the moved row
            `setIndex`

          `system`.nextFreeIdx -= 1

  let
    tryGetIndex = system.indexTryGet(entIdIdent, rowIdent, sysOpts)
    delIndex = system.indexDel(entIdIdent, sysOpts)

  quote do:
    if `tryGetIndex`:
      # A system might not contain a reference to this entity -
      # for example if they don't contain other required components.
      `userSysRemoveCode`
      `delIndex`
      # Trim seq/array.
      `trimGroup`

proc makeRemoveComponentDirect*(entOpts: ECSEntityOptions): NimNode =
  #[
    Important note!
    If you call removeComponent whilst in a system using that component, the current `item` will change!
    In this case, item.entity.removeComponent will cause item.entity and it's components to be different!
    This happens because `entity.removeComponent` and `entity.delete` remove items from systems by swapping
    the item with the last one in the list and reducing the list length.
  ]#
  let
    index = ident "index"
    entityIdent = ident "entity"
    entityIdIdent = ident "entityId"
    # Array of all types by systems
    systemsByCompId = compSystems()
    componentLen = componentRefsLen(entityIdIdent, entOpts)
    alive = ident "alive" # Late bind for this template
    rowIdent = ident "row"

  result = newStmtList()

  var includeTemplates: bool

  for typeId in ecsComponentsToBeSealed:
    let
      typeName = tNames[typeId.int]
      typeInstanceIdent = ident instanceTypeName(typeName)
      tyDelete = ident deleteInstanceName()

      compTypedesc = nnkBracketExpr.newTree(ident "typedesc", ident typeName)
      # System indexes that use the component being removed
      relevantSystems = systemsByCompId[typeId.int]
      removeIdxIdent = ident "compIdx"
      removeCompFromEntity = removeComponentRef(entityIdIdent, removeIdxIdent, typeId.int, entOpts)

    var
      updateSystems = newStmtList()
      visited = newSeq[bool](sysRequirements.len)

    for systemIndex in relevantSystems:
      if not visited[systemIndex.int]:
        visited[systemIndex.int] = true
        # We must remove all references to every relevant system here,
        # as we're removing a required component for the system to run.
        let
          sysOpts = ecsSysOptions[systemIndex.int]
          sysIdent = ident systemVarName(sysOpts.name)
        updateSystems.add(removeSysReference(systemIndex.int, sysIdent, rowIdent, entityIdIdent, entityIdent, sysOpts))

    var
      foundComp = ident "found"
      userCompRemoveCode = newStmtList()

    # Component remove user code.
    if typeId.int < componentRemoveCode.len:
      let compRem = componentRemoveCode[typeId.int]
      if compRem != nil:
        includeTemplates = true
        userCompRemoveCode.add(quote do:
          `compRem`
        )

    let
      removeRef = 
        if entOpts.componentStorageFormat == csTable:
          quote do:
            let `foundComp` = entityData(`entityIdIdent`).componentRefs.getOrDefault(`typeId`.ComponentTypeId)
            if `foundComp`.typeId == `typeId`.ComponentTypeId:
              `userCompRemoveCode`
              `tyDelete`(`typeInstanceIdent`(`foundComp`.index))
              `removeCompFromEntity`
        else:
          quote do:
            var `removeIdxIdent` = 0
            while `removeIdxIdent` < `componentLen`:
              # Could be faster if this list was sorted by compTypeId
              let `foundComp` = entityData(`entityIdIdent`).componentRefs[`removeIdxIdent`]
              if `foundComp`.typeId == `typeId`.ComponentTypeId:
                `userCompRemoveCode`
                `tyDelete`(`typeInstanceIdent`(`foundComp`.index))
                `removeCompFromEntity`
                break
              # For list storage types we need an index to delete from, for tables we do not.
              # This should be optimised out for table builds.
              `removeIdxIdent`.inc

    let
      setVal = ident "ce" & typeName
      setOp = entSetExcl(entOpts, entityIdIdent, setVal)
      doRemoveName = ident "doRemove" & typeName
      curEntTmpl =
        if includeTemplates:
          quote do:
            ## Access to currently updating entity.
            template curEntity: untyped {.used.} = `entityIdent`
            ## Current component being removed from entity. 
            template curComponent: untyped {.used.} = `typeInstanceIdent`(`foundComp`.index)
        else: newEmptyNode()

    result.add(quote do:
      proc `doRemoveName`(`entityIdent`: EntityRef) =
        assert `entityIdent`.`alive`
        let `entityIdIdent` = `entityIdent`.entityId

        if entityData(`entityIdIdent`).setup:
          # RowIdent is used by updateSystems.
          var `rowIdent` {.used.}: int
          `curEntTmpl`
          # Remove this entity from all relevant systems.
          `updateSystems`
          # Update set if required.
          `setOp`
          `removeRef`
            
      template removeComponent*(`entityIdent`: EntityRef, compType: `compTypedesc`) =
        ## Remove the component.
        static:
          if inSystemAll and inSystemIndex in `relevantSystems`:
            # Calling removeComponent from within a system that uses the component.
            # We don't know if it's the current row's entity or some other entity.
            sysRemoveAffectedThisSystem = true
        `doRemoveName`(`entityIdent`)
    )
  genLog "# Remove component:\n", result.repr

proc clearAllEntComponentRefs(entityId: NimNode, options: ECSEntityOptions): NimNode =
  case options.componentStorageFormat
  of csSeq:
    quote do:
      for compRef in entityData(`entityId`).componentRefs:
        caseComponent compRef.typeId:
          componentDel(componentInstanceType()(compRef.index))
      entityData(`entityId`).componentRefs.setLen(0)
  of csArray:
    quote do:
      for compIdx in 0 ..< entityData(`entityId`).nextCompIdx:
        let curCompRef = entityData(`entityId`).componentRefs[compIdx]
        caseComponent curCompRef.typeId:
          componentDel(componentInstanceType()(curCompRef.index))
      entityData(`entityId`).nextCompIdx = 0
  of csTable:
    quote do:
      for compPair in entityData(`entityId`).componentRefs.pairs:
        caseComponent compPair[1].typeId:
          componentDel(componentInstanceType()(compPair[1].index))
      entityData(`entityId`).componentRefs.clear

proc recyclerAdd(ecStateNode, entIdNode: NimNode, options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.add `entIdNode`
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      let nextIdx = `ecStateNode`.`rLen`
      assert nextIdx < `ecStateNode`.entityRecycler.len
      
      `ecStateNode`.entityRecycler[nextIdx] = `entIdNode`
      `ecStateNode`.`rLen` += 1

proc recyclerClear*(ecStateNode: NimNode, options: ECSEntityOptions): NimNode =
  case options.recyclerFormat
  of rfSeq:
    quote do: `ecStateNode`.entityRecycler.setLen 0
  of rfArray:
    let rLen = ident recyclerArrayLen()
    quote do:
      `ecStateNode`.`rLen` = 0

proc makeDelete*(options: ECSEntityOptions): NimNode =
  ## Generates delete procedures for the current entity.
  ## Delete will be created with all the systems that have been seen since the last
  ## `makeEcs` invocation.
  let
    ent = ident("entity")
    delProcName = ident("delete")
    compRefIdent = ident("compRef")
    allCompSystems = compSystems()
    storageVar = ident entityStorageVarName()
    totalSystemCount = allSystemsNode.len
    rowIdent = ident "row"
    entIdIdent = ident "entityId"
    visitedIdent = ident "visited"
    clearComponents = clearAllEntComponentRefs(entIdIdent, options)

  var
    updateSystems = newStmtList()
    caseStmt = nnkCaseStmt.newTree()
  updateSystems.add(quote do:
    # This will be filled with the groups row, retrieved from index
    var
      `rowIdent`: int
      `visitedIdent`: array[0 .. `totalSystemCount`, bool]
    )

  # Here we build a case statement executed for each of the entity's component indexes
  # so that each branch contains the appropriate free code for all systems that use
  # that component.
  # A system may have a requirement for multiple components that aren't satisfied though,
  # so we must also check if there's a row to be removed.
  caseStmt.add newDotExpr(compRefIdent, ident "typeId")
  
  var
    handledComponents: seq[int]
    # index matches component type id.
    removeBodies = newSeq[seq[NimNode]](allCompSystems.len)

  for compIdx in 1 ..< allCompSystems.len:
    let compSystems = allCompSystems[compIdx]
    # For each component, update any systems referenced.
  
    for sysIdx in compSystems:
      # Process only new systems we haven't seen before for this set of entities.
  
      if sysIdx in ecsSystemsToBeSealed:
        # Register this component for the `of` statement.
        if compIdx notin handledComponents:
          handledComponents.add compIdx

        let
          sysOpts = ecsSysOptions[sysIdx.int]
          sysName = systemNames[sysIdx.int].capitalizeAscii()
          sysIdent = ident systemVarName(sysName)
          removeSystemEntry = removeSysReference(sysIdx.int, sysIdent, rowIdent, entIdIdent, ent, sysOpts)

        # Checking visited avoids some unnecessary work.
        removeBodies[compIdx].add(
          quote do:
            if not `visitedIdent`[`sysIdx`]:
              `visitedIdent`[`sysIdx`] = true
              `removeSystemEntry`
          )

  var includeEntityTmpl: bool
  
  for compIdx in handledComponents:
    var userCompRemoveCode = newStmtList()
    let
      tyName = tNames[compIdx.int]
      tyInstance = newIdentNode instanceTypeName(tyName)

    # Component remove user code.
    if compIdx.int < componentRemoveCode.len:
      let compRem = componentRemoveCode[compIdx.int]
      if compRem != nil:
        includeEntityTmpl = true
        userCompRemoveCode.add(quote do:
          ## Current component being removed from entity. 
          template curComponent: untyped {.used.} = `tyInstance`(`compRefIdent`.index)
          `compRem`
        )

    var ofNode = nnkOfBranch.newTree()
    ofNode.add newDotExpr(newIntLitNode(compIdx), ident"ComponentTypeId")
    var ofStmts = newStmtList()
    # Add the system removal reference
    ofStmts.add removeBodies[compIdx]

    # User callback.
    var userCallback = newEmptyNode()
    if compIdx.int < removeCallbackProcs.len:
      # Check for this type's initialiser.
      let sysRemoveCallback = removeCallbackProcs[compIdx.int]
      if sysRemoveCallback != nil:
        includeEntityTmpl = true
        let cbProcName = ident sysRemoveCallbackName(tyName)
        userCallback = quote do:
          `cbProcName`(`tyInstance`(`compRefIdent`.index))

    ofStmts.add(quote do:
      `userCallback`
      `userCompRemoveCode`
      )
    ofNode.add ofStmts
    caseStmt.add(ofNode)
  var elseNode = nnkElse.newTree(newStmtList(quote do: discard))
  caseStmt.add elseNode
  
  updateSystems.add caseStmt

  let
    # For pointer arrays, the GC needs to be informed about the componentRef sequence.
    gcCleanup =
      case options.entityStorageFormat
      of esSeq, esArray: newEmptyNode()
      of esPtrArray:
        if options.componentStorageFormat in [csSeq, csTable]:
          quote do:
            GC_Unref(entityData(`entIdIdent`).componentRefs)
        else:
          newEmptyNode()
    recyclerAdd = storageVar.recyclerAdd(entIdIdent, options)
    recyclerClear = storageVar.recyclerClear(options)
    initSet =
      if options.useSet:
        quote do:
          entityData(`entIdIdent`).exists = {}        
      else: newEmptyNode()

  let
    clearRecycler = storageVar.recyclerClear(options)
    curEntTmpl =
      if includeEntityTmpl:
        quote do:
          ## Access to currently updating entity.
          template curEntity: untyped {.used.} = `ent`
      else: newEmptyNode()

  result = quote do:
    proc doDelete(`ent`: EntityRef) =

      if not `ent`.alive: return
      let `entIdIdent` = `ent`.entityId
      if entityData(`entIdIdent`).setup:
        `curEntTmpl`
        for `compRefIdent` in `entIdIdent`.components:
          `updateSystems`
        `initSet`
        # finally, clear components.
        # We don't bother calling removeComponentRef here because we're deleting all of the components and don't need them reshuffled.
        `clearComponents`
        `gcCleanup`

        entityData(`entIdIdent`).setup = false
        `storageVar`.entityCounter -= 1
        `recyclerAdd`

        if `storageVar`.entityCounter == 0:
          # Helps against nextEntityId going out of range after repeated add/delete.
          `recyclerClear`
          `storageVar`.nextEntityId = FIRST_ENTITY_ID

    template `delProcName`*(`ent`: EntityRef) =
      static:
        if inSystem and (not inSystemDeleteRow): systemCalledDelete = true
      doDelete(`ent`)

    proc deleteAll*(entities: Entities) =
      for i in 0 ..< entities.len:
        entities[i].delete

    proc resetEntityStorage* =
      ## This deletes all entities, removes them from associated systems and resets next entity.
      # TODO: More efficient to dispense with the book keeping and just clear systems & storage manually.
      # Good for testing for issues with delete though.
      # NOTE: ENTITY INSTANCES SHOULD REMAIN INTACT!
      for i in 0 ..< `storageVar`.nextEntityId.int:
        let ent = (i.EntityId).makeRef
        ent.delete
      `clearRecycler`
      `storageVar`.nextEntityId = FIRST_ENTITY_ID
      `storageVar`.entityCounter = 0

  genLog "# Delete entity:\n", result.repr

