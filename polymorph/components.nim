import macros, typetraits, strformat, sharedtypes, private/utils, strutils
export macros

macro componentNames*: untyped =
  ## Creates a runtime accessible array of typenames of all known components.
  ## This should map directly to componentTypeId.
  let comps = nnkStmtList.newTree(componentStrLits)
  result = quote do:
    @`comps`

proc addComponentTypeId(typeNameStr: string): ComponentTypeId {.compileTime.} = 
  assert typeNameStr notin tNames, "Type name " & typeNameStr & " is already registered as a component"
  tNames.add(typeNameStr)
  componentSystemOwner.add InvalidSystemIndex
  # add to runtime list to get component name from ComponentTypeId
  componentStrLits.add(newStrLitNode(typeNameStr))
  compTypeNodes.add(newIdentNode(typeNameStr))
  let res = tNames.high.ComponentTypeId
  genLog "# Added component type: \"", typeNameStr, "\" = ", $res.int
  res

proc typeStringToId*(n: string): ComponentTypeId {.compiletime.} =
  ## Returns an index for a type string, if found.
  # TODO: String based checks of types with the same name will return the same ComponentTypeId.
  # Might want to store type trees themselves and match on that.
  assert(n != "Component", "Not enough type info to create id: Receiving type `Component`, expected sub-class of Component or a registered component type")
  var r = -1
  for tId in 0 ..< tNames.len:
    if tNames[tId.int].toLowerAscii == n.toLowerAscii:
      r = tId.int
      break
  assert r.int != -1, "Cannot find type \"" & n & "\" in known component types: " & $tNames
  r.ComponentTypeId

## Uses the typeId as an index into runtime component names
proc toString*(id: ComponentTypeId): string = componentNames()[id.int]  

## Checks against invalid component (for example uninitialised data).
## This does not check to see if the component is alive in the system, only that it is semantically valid.
## Used for example to check that fetchComponent has found it's value.
proc valid*(compRef: ComponentRef): bool = compRef.typeId != InvalidComponent and compRef.generation.int != InvalidComponentGeneration.int

## Read only access to the type id inside a Component descendant
proc typeId*[T: Component](c: T): ComponentTypeId = c.fTypeId

proc init*[T: Component](c: var T) {.inline.} =
  ## Useful when storing in containers, eg; seq[Component]
  c.fTypeId = c.type.typeId()

proc checkInit*[T: Component](c: var T) {.inline.} =
  if c.fTypeId == 0: c.fTypeId = c.type.typeId()

proc makeRefCompInit(prefix: string, tyName: string, typeId: int): NimNode =
  # Generate init macro for reference type.
  # This macro curries the parameters to an object initialisation of the form `MyObjectRef(param1: value1, ...)`.
  let
    tyNameRefStr = refTypeName(tyName)
    initNameStr = refInitName(prefix, tyName)
    initName = newIdentNode(initNameStr)
    initComment = newCommentStmtNode("Initialiser for " & tyNameRefStr & ", automatically sets `typeId` to " & $typeId)
    res = newIdentNode "result"
    typeIdFieldName = "fTypeId"

  if refInitPrefixes.len <= typeId:
    refInitPrefixes.setLen(typeId + 1)
    refInitPrefixes[typeId] = prefix
  result = quote do:
    macro `initName`*(args: varargs[untyped]): untyped =
      `initComment`

      # Setup value object that stores the component data.
      var
        stmts = newStmtList()
        objData = nnkObjConstr.newTree()

      objData.add newIdentNode(`tyName`)

      # update the value object's fields.
      for arg in args:
        if arg.kind notin {nnkExprEqExpr, nnkExprColonExpr}:
          error("Expected an equals or colon assignment but got " & $arg.kind)
        # Assign this data item to the value object's field.
        objData.add nnkExprColonExpr.newTree(arg[0], arg[1])
    
      # build the ref object type constructor 
      stmts.add(nnkObjConstr.newTree(
        newIdentNode `tyNameRefStr`,
        nnkExprColonExpr.newTree(
          newIdentNode(`typeIdFieldName`),
          newDotExpr(newIntLitNode(`typeId`), newIdentNode("ComponentTypeId"))
        ),
        nnkExprColonExpr.newTree(
          newIdentNode("value"), objData
        )
        ))

      `res` = stmts

      genLog "# Creating ", `initNameStr`, " with params:\n", `res`.repr

proc makeInstanceCompInit(prefix, tyName: string, typeId: int): NimNode =
  ## This macro generates an init proc that allows setting of fields.
  ## These init procs return the instance of the type, which allows direct field access
  ## and can be converted to a ref with toRef().
  ## Eg;
  ##   let comp = newMyComponent(x = 1, y = 2)
  ## Translates to something like this:
  ##   var comp = MyComponent(typeId: MyComponent.typeId, x: 1, y: 2)
  if instanceInitPrefixes.len < typeId:
    instanceInitPrefixes.setLen(typeId + 1)
    instanceInitPrefixes[typeId] = prefix
  let
    initInstanceStr = instanceInitName(prefix, tyName)
    initInstance = newIdentNode(initInstanceStr)
    initComment = newCommentStmtNode("Initialiser for " & tyName & "(`typeId` " & $typeId & ")")
    res = newIdentNode "result"

  result = quote do:
    macro `initInstance`*(args: varargs[untyped]): untyped =
      `initComment`
      # Create value object that stores the component data and pass assignments from parameters.
      # Code produced is essentially 
      
      var
        stmts = newStmtList()
        newComp = genSym(nskLet, "newComp")

      # generate new component instance for this type:
      # (This is defined below after storage has been instantiated)
      # eg;
      #   let newComp = `initProc`()
      stmts.add(
        nnkLetSection.newTree(
          nnkIdentDefs.newTree(
            newComp,
            newEmptyNode(),
            nnkCall.newTree(
              newIdentNode createInstanceName(`tyName`)
            )
          )
        )
      )
      
      var objData = nnkObjConstr.newTree()
      objData.add newIdentNode(`tyName`)

      # update the value object's fields.
      for arg in args:
        if arg.kind notin {nnkExprEqExpr, nnkExprColonExpr}:
          error("Expected an equals or colon assignment but got " & $arg.kind)
        # Assign this data item to the value object's field.
        objData.add nnkExprColonExpr.newTree(arg[0], arg[1])

      stmts.add(nnkCall.newTree(nnkDotExpr.newTree(newComp, newIdentNode("update")), objData))

      # The return expression is the instance type of the new component.
      stmts.add newComp

      # build the type constructor
      `res` = nnkBlockStmt.newTree(
        newEmptyNode(), stmts)

  genLog "# Creating initialiser ", `initInstanceStr`, "\n", result.repr

proc nameNode(typeNode: NimNode): NimNode =
  typeNode.expectKind nnkTypeDef
  typeNode[0].baseName

proc createRefComponent(typeNode: NimNode): NimNode =
  # Creates a type inherited from `Component` that contains the fields for this type.
  # This is used for runtime templates, allowing heterogeneous list of components.
  # eg:
  #  ComponentTypeRef* = ref object of Component
  #    value*: ComponentType
  result = typeNode.copy()
  let container = nnkRecList.newTree(
    nnkIdentDefs.newTree(postfix(ident "value", "*"), ident $typeNode.nameNode, newEmptyNode()),
    )
  result[0] = postFix(newIdentNode(refTypeName($typeNode.nameNode)), "*")
  result[2] = nnkRefTy.newTree(
    nnkObjectTy.newTree(
      newEmptyNode(), nnkOfInherit.newTree(ident "Component"), container 
    )
  )

proc createRef(prefix: string, typeDef: NimNode, typeId: ComponentTypeId): NimNode =
  # Creates the ref type for this type and it's initialiser
  result = newStmtList()
  # Create init proc using prefix and type name
  let typeName = $typeDef.nameNode()
  result.add makeRefCompInit(prefix, typeName, typeId.int)
  
proc doRegisterComponents(options: ECSCompOptions, body: NimNode): NimNode =
  ## Registers types in a block to tNames.
  ## For each type provided in `body`, this macro generates:
  ##   * A ref container for the type with a typeId field for use in runtime templates.
  ##   * A static template to associate the type with a unique `ComponentTypeId`.
  ##   * An initialiser for use with runtime templates. This sets the type Id for you.
  ## The types sections you provide in `body` are passed through unaltered.

  result = newStmtList()

  echo "=== Component generation options ===\n", options.repr
  if options.componentStorageFormat != cisSeq and options.maxComponents <= 0:
    error "Component option `maxComponents` must be greater than zero when using a non-resizable storage format"

  var
    typeDeclarations = newStmtList()
    typeUtils = newStmtList()
  let previousComponentsDeclared = tNames.len

  for tyDef in body.typeDefs:
    let
      typeNameIdent = tyDef.nameNode()
      typeNameStr = $typeNameIdent
      # store component type and generate id
      typeId = addComponentTypeId(typeNameStr)
      instTypeNode = newIdentNode instanceTypeName(typeNameStr)
      generationTypeNode = newIdentNode generationTypeName(typeNameStr)
      typeIdAccessName = newIdentNode("typeId")
      tyParam = newIdentNode("ty")
      refTypeNameIdent = newIdentNode(refTypeName(typeNameStr))

    ecsComponentsToBeSealed.add typeId
    
    # record the options for later transforms such as commitSystems
    ecsCompOptions.add(options)

    # Typefields is used in field templates.
    componentFieldDefinitions.add tyDef.getFields(typeNameStr)
    
    if componentDefinitions.high < typeId.int:
      componentDefinitions.setLen typeId.int + 1
    componentDefinitions[typeId.int] = tyDef

    typeUtils.add nnkTypeSection.newTree tyDef.createRefComponent

    typeDeclarations.add(quote do:
      type `instTypeNode`* = distinct `IdBaseType`
      type `generationTypeNode`* = distinct `IdBaseType`
    )
    # Add static transform to convert from type to `ComponentTypeId`
    # eg; template typeId*(ty: A | ARef | AInstance): ComponentTypeId = 1.ComponentTypeId
    # The same value is returned for a concrete instance, ref instance, and typedesc.
    typeUtils.add(quote do:
      template `typeIdAccessName`*(`tyParam`: `typeNameIdent` | `refTypeNameIdent` | `instTypeNode` |
        typedesc[`typeNameIdent`] | typedesc[`refTypeNameIdent`] | typedesc[`instTypeNode`]): ComponentTypeId = `typeId`.ComponentTypeId
      )

  if tNames.len == previousComponentsDeclared:
    error "Cannot process registerComponents: No typedefs can be found to create components from"

  # Generate the storage array type for these components
  # storageTypeName: object =
  #   <fields of type>
  result.add typeDeclarations
  # The body always gets added unchanged, but after the instance types have been generated.
  # This allows instance types to be used within the user's type definitions.
  result.add body
  result.add typeUtils

proc generateTypeStorage*(): NimNode =
  result = newStmtList()

  var
    typeUtils = newStmtList()
    # Each component gets a unique array generated in the storage type.
    storageFields = nnkVarSection.newTree()

  for typeId in ecsComponentsToBeSealed:
    let
      options = ecsCompOptions[typeId.int]
      typeNameStr = tNames[typeId.int]
      typeNameIdent = ident typeNameStr
      refTypeNameIdent = newIdentNode(refTypeName(typeNameStr))
      tyDef = componentDefinitions[typeId.int]
      tyParam = newIdentNode("ty")

      maxComponentCount = options.maxComponents + 2
      refInitPrefix = if options.refInitPrefix != "": options.refInitPrefix
        else: defaultRefInitPrefix

    template typeFields: untyped = componentFieldDefinitions[typeId.int]
    
    doAssert typeFields.typeName == typeNameStr, "Internal error: Extracting type yielded " & typeFields.typeName & " but expected "  & typeNameStr

    let instTypeNode = newIdentNode instanceTypeName(typeNameStr)
    
    # Add this type's instance type for use in generating a type class for all instances
    instanceTypeNode.add instTypeNode

    # Add initialiser proc that sets `typeId` and passes on params to an object constructor.
    # eg; initA*(param = x): A = A(fTypeId: A.typeId, param: x)
    typeUtils.add refInitPrefix.createRef(tyDef, typeId)

    typeUtils.add(quote do:
      ## Compile-time translation between a user's type/container type to it's instance type.
      ## Useful for converting a ComponentIndex into direct storage access.
      template instanceType*(`tyParam`: typedesc[`typeNameIdent`] | typedesc[`refTypeNameIdent`]): untyped = `instTypeNode`
      ## Compile-time translation between a user's type to it's container `ref` type.
      template containerType*(`tyParam`: typedesc[`typeNameIdent`] | typedesc[`instTypeNode`]): untyped = `refTypeNameIdent`
      ## Create a `ref` container from a user object. The parameter is deepCopied.
      template makeContainer*(`tyParam`: `typeNameIdent`): `refTypeNameIdent` =
        var container = `refTypeNameIdent`(fTypeId: `typeId`.ComponentTypeId, value: `tyParam`)
        container
      ## Create a `ref` container from an instance. The parameter is deepCopied.
      template makeContainer*(`tyParam`: `instTypeNode`): `refTypeNameIdent` =
        `tyParam`.access.makeContainer()
      )

    let storageFieldName = typeNameStr.storageFieldName

    proc storageField(typeIdent: NimNode, maxComps: int): NimNode =
      case options.componentStorageFormat
        of cisArray: genArray(maxComps, typeIdent)
        of cisSeq: genSeq(typeIdent)

    let
      useThreadVar = options.useThreadVar
      ownerSystem = componentSystemOwner[typeId.int]
      supportStorageSize =
        if ownerSystem == InvalidSystemIndex:
          maxComponentCount + 1
        else:
          let sysOpts = ecsSysOptions[ownerSystem.int]
          if sysOpts.storageFormat == ssArray:
            sysOpts.maxEntities + 1
          else:
            0

    # Generate the field that will hold this component within storage.
    # eg; a: array[100000, A]
    storageFields.add genField(storageFieldName, true, typeNameIdent.storageField(supportStorageSize), useThreadVar)
    
    # Generate the last index list for this component of the specific instance type
    # eg; aLastIndex: seq[AInstance]
    let freeIdxIdent = typeNameStr.freeInstancesName
    storageFields.add genField(freeIdxIdent, true, genSeq(instTypeNode), useThreadVar)
    
    # Generate the next index variable for this component
    # eg; aNextIndex: AInstance
    let nextIdxName = typeNameStr.nextInstanceName
    storageFields.add genField(nextIdxName, true, instTypeNode, useThreadVar)

    # Generate the instance alive state array for this component type
    # eg; aAlive: array[100000, bool]
    let aliveStateIdent = typeNameStr.aliveStateInstanceName
    storageFields.add genField(aliveStateIdent, true, newIdentNode("bool").storageField(supportStorageSize), useThreadVar)

    # Generate an array of instance numbers against each component id
    # This allows us to compare an existing instance with a ComponentRef.
    let instanceIdsName = typeNameStr.instanceIdsName
    storageFields.add genField(instanceIdsName, true, newIdentNode("int32").storageField(supportStorageSize), useThreadVar)

  result.add storageFields
  result.add typeUtils
  genLog "# Component storage:\n", result.repr

proc genTypeAccess*(): NimNode =
  result = newStmtList()  
  
  var
    typeAccess = newStmtList()
    firstCompIdInits = newStmtList()

  for typeId in ecsComponentsToBeSealed:
    let
      options = ecsCompOptions[typeId.int]
      initPrefix = if options.initPrefix != "": options.initPrefix else: defaultInitPrefix
      maxComponentCount = options.maxComponents + 2
      typeNameStr = tNames[typeId.int]
      typeNameIdent = ident typeNameStr
      storageFieldName = storageFieldName(typeNameStr)
      lcTypeIdent = newIdentNode storageFieldName
      createIdent = newIdentNode createInstanceName(typeNameStr)
      deleteIdent = newIdentNode deleteInstanceName()
      freeIdxIdent = newIdentNode freeInstancesName(typeNameStr)
      instParam = ident "instance"
      instanceIds = newIdentNode instanceIdsName(typeNameStr)   # Type's instance id array
      instanceTypeIdent = ident instanceTypeName(typeNameStr)
      refTypeNameIdent = newIdentNode refTypeName(typeNameStr)  # Ref container type
      aliveIdent = newIdentNode aliveStateInstanceName(typeNameStr)
      nextIdxIdent = newIdentNode nextInstanceName(typeNameStr)
      tyParam = ident "ty"
      instTypeName = instanceTypeName(typeNameStr)
      instTypeNode = newIdentNode instTypeName                  # Type's instance type
      generationTypeNode = newIdentNode generationTypeName(typeNameStr)
      res = ident "result"
    template typeFields: untyped = componentFieldDefinitions[typeId.int]

    # Add a proc to return a new component index.
    # This is the 'new component' procedure, and uses the `initPrefix` parameters.
    typeAccess.add(makeInstanceCompInit(initPrefix, typeNameStr, typeId.int))
    
    # Potentially extra init for heap items
    case options.componentStorageFormat
      of cisArray: discard
      of cisSeq:
        firstCompIdInits.add(quote do:
          `lcTypeIdent` = newSeq[`typeNameIdent`](1)
          `aliveIdent` = newSeq[bool](1)
          `instanceIds` = newSeq[int32](1)
        )

    # Ensure first component item is valid.
    firstCompIdInits.add(quote do:
      `nextIdxIdent` = FIRST_COMPONENT_ID.`instanceTypeIdent`
    )

    let
      eqOp = nnkAccQuoted.newTree(newIdentNode "==")
      strOp = nnkAccQuoted.newTree(ident "$")
      deletedComp = newLit "<Deleted " & instTypeName # Completed below.
      invalidStr = newLit "<Invalid " & instTypeName & ">"
      userUpdateCode =
        if typeId.int < componentInterceptValueInitCode.len and
          componentInterceptValueInitCode[typeId.int] != nil:
            # It's now the user's responsibility to call update.
            componentInterceptValueInitCode[typeId.int]
        else:
          newEmptyNode()
      
      commitParam = ident "newValue"
      valueParam = ident "value"
      inst = ident "instance"
      fieldParam = ident "field"
    
    if componentSystemOwner[typeId.int] != InvalidSystemIndex:
      let
        ownerSystemIdx = componentSystemOwner[typeId.int]
        ownerSystem = allSystemsNode[ownerSystemIdx.int]
        sysTupleFieldStr = typeNameStr.toLowerAscii
        sysTupleField = ident sysTupleFieldStr
        standAloneUpdate =
          if userUpdateCode.kind != nnkEmpty:
            quote do:
              template commit(`commitParam`: `typeNameIdent`): untyped {.used.} =         
                `ownerSystem`.groups[`inst`.int].`sysTupleField` = `commitParam`
              template curValue: `typeNameIdent` = `valueParam`
              `userUpdateCode`
          else:
            quote do:
              `ownerSystem`.groups[`inst`.int].`sysTupleField` = `valueParam`

      # Access the fields of the component in storage via the index.
      # Two approaches, via dot operators or building access templates for every field in this component (not recursive).
      case options.accessMethod
      of amDotOp:
        # We only need two dot operators, one for reading and one for writing to cover all the fields in this instance.
        {.push experimental: "dotOperators".}
        let
          dotOp = nnkAccQuoted.newTree(newIdentNode ".")
          dotEqOp = nnkAccQuoted.newTree(newIdentNode ".=")
        typeAccess.add(quote do:
          template `dotOp`*(`inst`: `instanceTypeIdent`, `fieldParam`: untyped): untyped =
            when compiles(`ownerSystem`.groups[`inst`.int].`sysTupleField`.`fieldParam`):
              `ownerSystem`.groups[`inst`.int].`sysTupleField`.`fieldParam`
            else:
              {.fatal: "Cannot find field `" & `fieldParam`.repr & "` in instance type " & `typeNameStr`.}
              discard
          template `dotEqOp`*(`inst`: `instanceTypeIdent`, `fieldParam`: untyped, `valueParam`: untyped): untyped =
            when compiles(`ownerSystem`.groups[`inst`.int].`sysTupleField`.`fieldParam`):
              `ownerSystem`.groups[`inst`.int].`sysTupleField`.`fieldParam` = `valueParam`
            else:
              {.fatal: "Cannot find field `" & `fieldParam`.repr & "` in instance type " & `typeNameStr`.}
              discard
        )
        {.pop.}
      of amFieldTemplates:
        # Generate read/write access template from the type's typeDef.

        for field in typeFields.fields:
          let
            fieldName = field.fieldNode
            fieldType = field.typeNode
            setField = nnkAccQuoted.newTree(ident $fieldName & "=")
          typeAccess.add(quote do:
            template `fieldName`*(`inst`: `instanceTypeIdent`): `fieldType` = `ownerSystem`.groups[`inst`.int].`fieldName`
            template `setField`*(`inst`: `instanceTypeIdent`, `valueParam`: `fieldType`): untyped = `ownerSystem`.groups[`inst`.int].`fieldName` = `valueParam`
          )

      typeAccess.add(quote do:
        template ownedComponent*(value: typedesc[`instanceTypeIdent`] | `instanceTypeIdent` | `typeNameIdent`): bool = true
        template valid*(`inst`: `instanceTypeIdent`): bool =
          # TODO: Check generation.
          `inst`.int >= 0 and `inst`.int < `ownerSystem`.groups.len
        ## Converts the instance to a component storage directly.
        template access*(`inst`: `instanceTypeIdent`): `typeNameIdent` = `ownerSystem`.groups[`inst`.int].`sysTupleField`
        template alive*(inst: `instTypeNode`): bool =
          ## Check if this component ref's index is still valid.
          `aliveIdent`[inst.int] == true
        template generation*(inst: `instTypeNode`): untyped =
          ## Access the generation of this component.
          `generationTypeNode`(`instanceIds`[inst.int]).ComponentGeneration
        template `createIdent`: untyped = discard
        proc `deleteIdent`*(`instParam`: `instanceTypeIdent`) = discard
        ## Updating storage.
        ## `update` operates as a simple assignment into the storage array and therefore operates on the type's `==` proc.
        template update*(`instParam`: `instanceTypeIdent`, `valueParam`: `typeNameIdent`): untyped =
          `standAloneUpdate`
      )

    else:

      let
        standAloneUpdate =
          if userUpdateCode.kind != nnkEmpty:
            quote do:
              template commit(`commitParam`: `typeNameIdent`): untyped {.used.} =         
                `lcTypeIdent`[`instParam`.int] = `commitParam`
              template curValue: `typeNameIdent` = `valueParam`
              `userUpdateCode`
          else:
            quote do:
              `lcTypeIdent`[`instParam`.int] = `valueParam`

      # Add a translation from the type of the component to the base storage array for it's data.
      # This allows ComponentIndex.storage[some_index] to refer to other components of the same type.
      typeAccess.add(quote do:
        template storage*(`tyParam`: typedesc[`typeNameIdent`] | typedesc[`refTypeNameIdent`]): untyped = `lcTypeIdent`
        )

      # The user can choose whether they want to use dot operators or generate access templates for each field.
      # Dot operators:
      #   * Pros: Tend to work seamlessly with other templates and overloading resolution.
      #   * Cons: Can get confused when chaining long sequences together (in which case, use `access`).
      # Templates:
      #   * Pros: In theory better direct replacement of statements so more able to chain without issue.
      #   * Cons: In reality, variables with the same name as a template field can fail as ambiguous.
      #   * We can replace templates with procs, but now we the overhead of new stack frames for each access (though
      #     expect compiler to inline), and may also change semantics to copy instead of access.

      # Access the fields of the component in storage via the index.
      # Two approaches, via dot operators or building access templates for every field in this component (not recursive).
      case options.accessMethod
      of amDotOp:
        # We only need two dot operators, one for reading and one for writing to cover all the fields in this instance.
        {.push experimental: "dotOperators".}
        let
          dotOp = nnkAccQuoted.newTree(newIdentNode ".")
          dotEqOp = nnkAccQuoted.newTree(newIdentNode ".=")
        typeAccess.add(quote do:
          template `dotOp`*(`inst`: `instanceTypeIdent`, `fieldParam`: untyped): untyped =
            when compiles(`lcTypeIdent`[`inst`.int].`fieldParam`):
              `lcTypeIdent`[`inst`.int].`fieldParam`
            else:
              {.fatal: "Cannot find field `" & `fieldParam`.repr & "` in instance type " & `typeNameStr`.}
              discard
          template `dotEqOp`*(`inst`: `instanceTypeIdent`, `fieldParam`: untyped, `valueParam`: untyped): untyped =
            when compiles(`lcTypeIdent`[`inst`.int].`fieldParam`):
              `lcTypeIdent`[`inst`.int].`fieldParam` = `valueParam`
            else:
              {.fatal: "Cannot find field `" & `fieldParam`.repr & "` in instance type " & `typeNameStr`.}
              discard
        )
        {.pop.}
      of amFieldTemplates:
        # Generate read/write access template from the type's typeDef.
        doAssert typeFields.typeName == typeNameStr, "Internal error: Extracting type yielded " & typeFields.typeName & " but expected "  & typeNameStr

        for field in typeFields.fields:
          let
            fieldName = field.fieldNode
            fieldType = field.typeNode
            setField = nnkAccQuoted.newTree(ident $fieldName & "=")
          typeAccess.add(quote do:
            template `fieldName`*(`inst`: `instanceTypeIdent`): `fieldType` = `lcTypeIdent`[instance.int].`fieldName`
            template `setField`*(`inst`: `instanceTypeIdent`, `valueParam`: `fieldType`): untyped = `lcTypeIdent`[instance.int].`fieldName` = `valueParam`
          )

      typeAccess.add(quote do:
        ## Conve2rts the instance to a component storage directly.
        template ownedComponent*(value: typedesc[`instanceTypeIdent`] | `instanceTypeIdent` | `typeNameIdent`): bool = false
        template access*(instance: `instanceTypeIdent`): `typeNameIdent` = `lcTypeIdent`[instance.int]
        template alive*(inst: `instTypeNode`): bool =
          ## Check if this component ref's index is still valid.
          `aliveIdent`[inst.int] == true
        template valid*(inst: `instanceTypeIdent`): bool = inst.int != InvalidComponentIndex.int and inst.alive
        template generation*(inst: `instTypeNode`): untyped =
          ## Access the generation of this component.
          `generationTypeNode`(`instanceIds`[inst.int]).ComponentGeneration
      )

      let
        # delIdx is the index of the component in it's storage.
        delIdx = ident "idx"
        storageHigh = newDotExpr(lcTypeIdent, ident "high")
        freeIdxLen = quote do: `freeIdxIdent`.len
        freeIdxPop = quote do: `freeIdxIdent`.pop

      # This is expected to return the typed index for the new component.
      let handleNextStorageItem =
        case options.componentStorageFormat
        of cisArray:
          quote do:
            let cur = `nextIdxIdent`
            assert `nextIdxIdent`.int < `maxComponentCount`, "Ran out of space to store this component (max slot index: " &
              $(`maxComponentCount` - 1) & ", asked for " & $(`nextIdxIdent`.int) & ")"
            `nextIdxIdent` = (`nextIdxIdent`.int + 1).`instanceTypeIdent`
            cur
        of cisSeq:
          quote do:
            let newLen = `lcTypeIdent`.len + 1
            `lcTypeIdent`.setLen(newLen)
            `instanceIds`.setLen(newLen)
            `aliveIdent`.setLen(newLen)
            `lcTypeIdent`.high.`instanceTypeIdent`

      # Remove `delIdx` from the component array.
      let handleDelStorageItem =
        case options.componentStorageFormat
        of cisArray:
          quote do:
            # We need to subtract the length by one because the first item starts at one.
            if `freeIdxIdent`.len == `storageHigh` - 1:
              # If the free list is full, everything's free so we can reset the list.
              `freeIdxIdent`.setLen 0
            else:
              `freeIdxIdent`.add `delIdx`.`instanceTypeIdent`
        of cisSeq:
          quote do:
            # We don't need nextIdxIdent as this is naturally managed by seq.len
            if `freeIdxIdent`.high == `lcTypeIdent`.high:
              # Shrink seqs and update next index var.
              # We need to reserve index zero for invalid component.
              # The free list is full, everything is free so we can reset the list.
              `freeIdxIdent`.setLen(0)
            if `delIdx` == `lcTypeIdent`.high:
              let newLen = max(1, `lcTypeIdent`.len - 1)
              `lcTypeIdent`.setLen(newLen)
              `instanceIds`.setLen(newLen)
              `aliveIdent`.setLen(newLen)
            else:
              # Add to free indexes.
              `freeIdxIdent`.add `delIdx`.`instanceTypeIdent`

      let
        componentLen =
          case options.componentStorageFormat
          of cisArray:
            quote do:
              `nextIdxIdent`.int - 1
          of cisSeq:
            quote do:
              `lcTypeIdent`.len

      let
        res = ident "result"
        clearOnDelete =
          if options.clearAfterDelete:
            quote do:
              zeroMem(`instParam`.access.addr, `typeNameIdent`.sizeOf)
          else:
            newEmptyNode()
        userInitCode =
          if typeId.int < componentInitialisationCode.len and
            componentInitialisationCode[typeId.int] != nil:
              let code = componentInitialisationCode[typeId.int]
              quote do:
                block:
                  template curComponent: `instanceTypeIdent` = `res`
                  `code`
          else:
            newEmptyNode()
        userFinalCode =
          if typeId.int < componentFinalisationCode.len and
            componentFinalisationCode[typeId.int] != nil:
              componentFinalisationCode[typeId.int]
          else:
            newEmptyNode()
        
        rawCreate = quote do:
          var r: `instanceTypeIdent`
          if `freeIdxLen` > 0:
            # Use last spare index.
            r = `freeIdxPop`
          else:
            # Generate a new index.
            r = `handleNextStorageItem`
          assert r.int != 0
          # Update alive state.
          `aliveIdent`[r.int] = true
          # Update instance state.
          # This means the first valid index will start at one,
          # and an instance of zero can be used as an invalid state (see InvalidComponentRef)
          `instanceIds`[r.int] += 1
          r

        newCompUpdate =
          if userUpdateCode.kind != nnkEmpty:
            quote do:
              template commit(`commitParam`: `typeNameIdent`): untyped {.used.} =         
                `lcTypeIdent`[`res`.int] = `commitParam`
              template curValue: `typeNameIdent` = `valueParam`
              `userUpdateCode`
          else:
            quote do:
              `lcTypeIdent`[`res`.int] = `valueParam`

      typeAccess.add(quote do:
        proc `createIdent`*: `instanceTypeIdent` =
          ## Create a component instance for this type.
          # This proc is generated for each type to use the separate storage locations.
          # This involves either popping an index from the component's free list or incrementing it's next value counter,
          # then setting it's alive index to true.
          `res` = `rawCreate`
          `userInitCode`
        proc `deleteIdent`*(`instParam`: `instanceTypeIdent`) =
          ## Free a component instance
          let `delIdx` = `instParam`.int
          assert `delIdx` < `aliveIdent`.len, "Cannot delete, instance is out of range"
          if `aliveIdent`[`delIdx`]:
            `userFinalCode`
            # Only add something to the delete list if it's still alive, duplicate items would means all kinds of chaos.
            `aliveIdent`[`delIdx`] = false
            # Let delete know this index is now free.

            # TODO: With many items being deleted rapidly, the seq behind this will suffer much memory allocation and data moving.
            `handleDelStorageItem`
            # Clear the memory for this type.
            `clearOnDelete`
        ## Create a new component instance. Does not update systems.
        template newInstance*(`tyParam`: typedesc[`typeNameIdent`] | typedesc[`instanceTypeIdent`]): `instanceTypeIdent` = `createIdent`()
        ## Create a new component instance from the supplied value. Does not update systems.
        proc newInstance*(`valueParam`: `typeNameIdent`): `instanceTypeIdent` {.inline.} =
          `res` = `rawCreate`
          `userInitCode`
          `newCompUpdate`
        ## Creates a new component from a generated `ref` component descendant. Does not update systems.
        template newInstance*(`tyParam`: typedesc[`typeNameIdent`] | typedesc[`instanceTypeIdent`], val: Component): untyped =
          newInstance(`refTypeNameIdent`(val).value)
        ## Marks a component as deleted. Does not update systems.
        template delInstance*(`tyParam`: `typeNameIdent` | `instanceTypeIdent`): untyped = `tyParam`.`deleteIdent`()

        # TODO: Add possibility to merge updates, so that only fields you mention change, the rest remain untouched.
        # This is as opposed to the current update which replaces the whole contents of the variable with the new one.
        # Potential name: `edit`.
        # myComp.edit:
        #   field1 = value1
        #   field2 = value2

        ## Updating storage.
        ## `update` operates as a simple assignment into the storage array and therefore operates on the type's `==` proc.
        template update*(`instParam`: `instanceTypeIdent`, `valueParam`: `typeNameIdent`): untyped =
          #template curComponent: `instanceTypeIdent` = `instParam`
          `standAloneUpdate`

        template componentCount*(`tyParam`: typedesc[`typeNameIdent`] | typedesc[`instanceTypeIdent`]): untyped =
          ## Returns an estimate of the number of allocated components.
          ## This value is based on last index used and the number of indexes waiting to be used (from previous deletes).
          # Value zero is invalid, so we subtract the index by one.
          let freeCount = `freeIdxIdent`.len
          `componentLen` - freeCount
      )

    typeAccess.add(quote do:
      template `eqOp`*(i1, i2: `instanceTypeIdent`): bool = i1.int == i2.int

      template toRef*(inst: `instTypeNode`): ComponentRef =
        ## Utility function that takes this type's distinct `ComponentIndex`,
        ## returned for example from fetchComponent, and creates a reference
        ## tuple for the live component currently at this index.
        (inst.typeId, inst.ComponentIndex, inst.generation)

      proc `strOp`*(val: `instanceTypeIdent`): string =
        if val.valid:
          try:
            `res` = val.access.repr
          except:
            `res` = "<Error accessing>\n"
        else:
          if val.int == InvalidComponentIndex.int:
            `res` = `invalidStr`
          else:
            `res` = `deletedComp` & " (index: " & $val.int & ")>"
      )

  result.add typeAccess
  result.add firstCompIdInits

  # Add a generated type class that covers all the types we've registered here.
  result.add genTypeClass(typeClassName(), true, compTypeNodes)

  # Type class for all the instances.
  result.add genTypeClass(instanceTypeClassName(), true, instanceTypeNode)

  # Add an `add` for `ComponentList` that handles `typeId`
  let
    allCompsTC = ident typeClassName()
    allInstTC = ident instanceTypeClassName()
  result.add(quote do:
    proc add*(items: var ComponentList, component: `allCompsTC`|`allInstTC`) =
      ## Add a component to a template list, automatically handling `typeId`.
      system.add items, component.makeContainer()
    )

  genLog "# Component access:\n", result.repr, "\n(End)"

macro registerComponents*(options: static[ECSCompOptions], body: untyped): untyped =
  doRegisterComponents(options, body)

macro registerComponents*(maxComponents: static[int], body: untyped): untyped =
  ## Shortcut for registerComponents using defaults.
  var defaultCompOpts = defaultComponentOptions
  defaultCompOpts.maxComponents = maxComponents
  doRegisterComponents(defaultCompOpts, body)

proc genForAllComponents(typeId: ComponentTypeId, actions: NimNode): NimNode =
  let
    n = tNames[typeId.int]
    instType = newIdentNode instanceTypeName(n)
    accessArray = newIdentNode storageFieldName(n)
  result = newStmtList(quote do:
    template componentTypeId: untyped = `typeId`
    template componentName: untyped = `n`
    template componentInstType: untyped = `instType`
    for i in 0 ..< `accessArray`.len:
      template index: untyped = i
      template component: untyped = `accessArray`[i]
      `actions`
  )

macro forAllComponents*(typeId: static[ComponentTypeId], actions: untyped): untyped =
  ## Perform `actions` for every component of run time type `typeId`.
  genForAllComponents(typeId, actions)

template forAllComponents*(typeVal: typedesc, actions: untyped): untyped =
  forAllComponents(typeVal.typeId, actions)

macro forAllComponentTypes*(actions: untyped): untyped =
  ## Perform `actions` for every component type currently defined.
  ## Type iteration does not include InvalidComponent.
  result = newStmtList()
  for typeId in 1..tNames.high:
    result.add genForAllComponents(typeId.ComponentTypeId, actions)

macro clearAll*(): untyped =
  result = quote do:
    forAllComponentTypes:
      forAllComponents:
        typeId.clear

proc toInt*(c: ComponentTypeId): int = c.int

macro componentsDefined*: untyped =
  ## Returns the count of components defined so far in the compile process.
  newLit tNames.len
