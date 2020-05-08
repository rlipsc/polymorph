import macros, strutils, typetraits, ../sharedtypes, tables

#[
  This module covers shared internal utilities.
  
  *This module is not exported to the user*.

  * Procs used to generate consistent field and type names
  * Macro generation utils
]#

var
  # Current range of component id's that have yet to be sealed.
  ecsComponentsToBeSealed* {.compileTime.}: seq[ComponentTypeId]
  # Current range of systems that have yet to be processed by `makeEcs`.
  ecsSystemsToBeSealed* {.compileTime.}: seq[SystemIndex]
  # Indexed by component
  ecsCompOptions* {.compileTime.} = newSeq[ECSCompOptions](1)
  # Indexed by system
  ecsSysOptions* {.compileTime.} = newSeq[ECSSysOptions]()
  ecsSysDefined* {.compileTime.}: Table[SystemIndex, bool]
  ecsSysBodiesAdded* {.compileTime.}: Table[SystemIndex, bool]
  componentDefinitions* {.compileTime.}: seq[NimNode]

  # These variables allow adapting system code generation when systems request to change entity state.
  # * entity.delete called within a system body invokes a check to ensure index is within length each iteration.
  #   This is required because delete cannot know at compile time what components an entity might have, and if
  #   the entity is part of this system. Thus to prevent the user deleting entities in front of it's iteration path,
  #   and causing it's own index to be invalid, the length must be checked each iteration.
  #   This check can be avoided if you only delete the current row using deleteEntity

  # Current state of generation.
  inSystem* {.compileTime.}: bool
  inSystemAll* {.compileTime.}: bool
  inSystemStream* {.compileTime.}: bool
  inSystemDeleteRow* {.compileTime.}: bool
  # Which system we're building.
  inSystemIndex* {.compileTime.}: SystemIndex
  # Control iteration generation.
  sysCheckLengthPerIter* {.compileTime.}: bool
  sysRemoveAffectedThisSystem* {.compileTime.}: bool
  systemCalledDelete* {.compileTime.}: bool
  systemCalledDeleteEntity* {.compileTime.}: bool

  # Perform one log clear per unique path.
  logInitialised* {.compileTime.}: Table[string, bool]

proc findSystemIndex*(name: string): tuple[found: bool, index: int] {.compileTime.} =
  for idx, item in ecsSysOptions:
    if name.toLowerAscii == item.name.toLowerAscii:
      return (true, idx)

const
  # This is the postfix for both the instantiated storage variable and typename created for component storage.
  storageName* = "componentStorage"
  invalidComponentStr* = "<Invalid Component>"

type
  TypeFields* = tuple[typeName: string, fields: seq[tuple[fieldNode, typeNode: NimNode]]]

var
  # Names of all components by ComponentTypeId.
  tNames* {.compileTime.} = @[invalidComponentStr]
  # componentStrLits always starts with InvalidComponent at index zero.
  componentStrLits* {.compileTime.} = nnkBracket.newTree(newStrLitNode(invalidComponentStr))
  # List of prefixes for ref initialisers.
  refInitPrefixes* {.compileTime.} = newSeq[string]()
  # One prefix per invocation of `registerComponents`.
  # Prefixes for instance initialisers
  instanceInitPrefixes* {.compileTime.} = newSeq[string]()
  # List of idents for the component types, this is used to generate a type class.
  # Reset after each call to `registerComponents`
  compTypeNodes* {.compileTime.} = newSeq[NimNode]()
  # List of Instance type nodes
  # Reset after each call to `registerComponents`
  instanceTypeNode* {.compileTime.} = newSeq[NimNode]()

  # User defined initialisation code. This is called immediately after the instance is created.
  componentInitialisationCode* {.compileTime.} = newSeq[NimNode]()
  # User defined initialisation code for when a user calls called immediately after the instance is created,
  # but before it has been updated with any values.
  componentInterceptValueInitCode* {.compileTime.} = newSeq[NimNode]()
  # User defined finalisation code. This is called immediately before a component is deleted.
  componentFinalisationCode* {.compileTime.} = newSeq[NimNode]()
  # Stores user defined add code.
  # Code added here will be executed once after the add operation has completed,
  # and before any system update code for this component.
  componentAddCode* {.compileTime.} = newSeq[NimNode]()

  # Stores user defined code to be inserted when a component is removed from an entity.

  componentRemoveCode* {.compileTime.} = newSeq[NimNode]()
  addCallbackProcs* {.compileTime.} = newSeq[NimNode]()
  addForwardDecls* {.compileTime.} = newSeq[NimNode]()
  removeCallbackProcs* {.compileTime.} = newSeq[NimNode]()
  removeForwardDecls* {.compileTime.} = newSeq[NimNode]()

  # Stores which systems own components.
  # Indexed by ComponentTypeId.
  componentSystemOwner* {.compiletime.} = newSeq[SystemIndex](1)
  # Stores which components are owned for a system.
  # Indexed by SystemIndex.
  systemOwnedComponents* {.compiletime.} = newSeq[seq[ComponentTypeId]]()

  # Indexed by component id.
  # Stores the component's type name and field nodes.
  componentFieldDefinitions* {.compileTime.} = newSeq[TypeFields](1)

proc isOwned*(typeId: ComponentTypeId): bool = componentSystemOwner[typeId.int] != InvalidSystemIndex

proc hasUserIntercept*(typeId: ComponentTypeId): bool =
  typeId.int < componentInterceptValueInitCode.len and
  componentInterceptValueInitCode[typeId.int] != nil

# Systems
var
  # List of NimNodes of the System variables built by `defineSystem`.
  allSystemsNode* {.compileTime.} = newStmtList()
  # Definitions for system procs. Added in commitSystems.
  systemProcs* {.compileTime.} = newSeq[NimNode]()
  # Component requirements for each system.
  sysRequirements* {.compileTime.} = newSeq[seq[ComponentTypeId]]()
  # Idents for the system requirements.
  sysTypes* {.compileTime.} = newSeq[seq[NimNode]]()
  # Idents of run procedures.
  runAllDoProcsNode* {.compileTime.} = newStmtList()
  # Record of system names for use constructing setup at runtime.
  systemNames* {.compileTime.} = newSeq[string]()
  # Stores user defined add code.
  # Note this is indexed by ComponentTypeId, not SystemIndex.
  systemAddCode* {.compileTime.} = newSeq[NimNode]()
  # Stores user defined code to be inserted when a component is removed from a system.
  systemRemoveCode* {.compileTime.} = newSeq[NimNode]()
  # Code specifically for a particular component and system.
  # Indexed by system index.
  systemAddToCode* {.compileTime.} = newSeq[seq[tuple[typeId: ComponentTypeId, code: NimNode]]]()
  # Code specifically for a particular component and system.
  # Indexed by system index.
  systemRemoveFromCode* {.compileTime.} = newSeq[seq[tuple[typeId: ComponentTypeId, code: NimNode]]]()

# Type names derived from user types

# Instance distinct type name
proc instanceTypeName*(tyName: string): string = tyName & "Instance"
# Instantiation instance distinct type
proc generationTypeName*(tyName: string): string = tyName & "Generation"
# Reference container type name
proc refTypeName*(tyName: string): string = tyName & "Ref"
# Initialiser proc name for instance reference types, takes arguments to set fields
proc refInitName*(prefix, s: string): string = ($prefix & s).toLowerAscii()

# Initialisation/deleting components

# Initialiser proc name for instance of type, takes arguments to set fields
proc instanceInitName*(prefix, s: string): string = prefix & s
# Allocate function for a component slot for this type
proc createInstanceName*(s: string): string = "gen" & s
# Clear function for component slot for this type
proc deleteInstanceName*: string = "delete"

# Storage field names

## Type name for entity storage
proc entityStorageTypeName*: string = "EntityStorage"
proc entityStorageItemTypeName*: string = "EntityComponentItem"
# This is the name of the storage variable generated from a particular prefix.
proc entityStorageVarName*: string = "entityStorage"
proc initEntityStorageTypeName*: string = "initEntityStorage"
proc finaliseEntityStorageTypeName*: string = "finaliseEntityStorage" # TODO
proc entityStorageContainerTypeName*: string = "EntityStorageItems"
# Name of enum type used for sets of components.
proc enumName*: string = "ComponentsEnum"
proc recyclerArrayLen*: string = "recycleLen"

## Name of the storage field for this component type
proc storageFieldName*(typeName: string): string = "storage" & typeName
## Name of the instance ids by component index for this type
proc instanceIdsName*(typeName: string): string = typeName.toLowerAscii & "InstanceIds"
## Name of the array of alive state by component index for this type
proc aliveStateInstanceName*(typeName: string): string = typeName.toLowerAscii() & "Alive"
## Name of the seq for free instances indexes for this type
proc freeInstancesName*(typeName: string): string = typeName.toLowerAscii() & "FreeIndexes"
## Next instance slot for this type
proc nextInstanceName*(typeName: string): string = typeName.toLowerAscii() & "NextIndex"

## System names

proc systemTypeName*(name: string): string = name.capitalizeAscii() & "System"
proc systemInitName*(name: string): string = "init" & name.capitalizeAscii() & "System"

const
  sysVarPrefix = "sys"
  tupleNamePrefix = "SysTuple"  # capitalisation for type
  doProcPrefix = "do"
  instPostfix* = "Inst"

proc tupleName*(name: string): string = tupleNamePrefix & name.capitalizeAscii
proc doProcName*(name: string): string = doProcPrefix & name.capitalizeAscii
proc systemVarName*(name: string): string = sysVarPrefix & name.capitalizeAscii
proc addCallbackName*(name: string): string = "addCallback" & name
proc removeCallbackName*(name: string): string = "removeCallback" & name

# Type classes that cover component types. These are useful for parameter constraint.

## The name of the type class that covers all component types.
proc typeClassName*: string = "ComponentTypeClass"
## The name of the type class that covers all the distinct int types.
proc instanceTypeClassName*: string = "ComponentIndexTypeClass"


# Entity utils


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
      entityData(`entity`.entityId).nextCompIdx = newIdx + 1
      assert newIdx < entityData(`entity`.entityId).componentRefs.len, "Exceeded entity component storage capacity of " &
        $entityData(`entity`.entityId).componentRefs.len & " with index " & $newIdx
      entityData(`entity`.entityId).componentRefs[newIdx] = `componentRef`

proc addToEntityList*(entity: NimNode, passed: seq[ComponentTypeId], entOpts: ECSEntityOptions): NimNode =
  # Add to the entity's component list.
  result = newStmtList()

  for typeId in passed:
    let
      typeStr = tNames[typeId.int]
      fieldName = typeStr.toLower
      fieldIdent = ident fieldName
    result.add addComponentRef(entity, newDotExpr(fieldIdent, ident "toRef"), entOpts)

proc entSetIncl*(entOpts: ECSEntityOptions, entityId: NimNode, setVal: NimNode): NimNode =
  ## If `useSet` is true the set is updated with `setVal`,
  ## otherwise it does nothing.
  if entOpts.useSet:
    quote do:
      entityData(`entityId`).exists.incl `setVal`
  else: newEmptyNode()

proc entSetExcl*(entOpts: ECSEntityOptions, entityId: NimNode, setVal: NimNode): NimNode =
  ## If `useSet` is true `setVal` is removed from the set,
  ## otherwise it does nothing.
  if entOpts.useSet:
    quote do:
      entityData(`entityId`).exists.excl `setVal`
  else: newEmptyNode()

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


# System utils


iterator systemTypesStr*(systemIndex: SystemIndex): string =
  # Utility function to yield the name of the types used in the system specified by systemIndex.
  for id in sysRequirements[systemIndex.int]:
    yield tNames[id.int]

iterator systemTypesStrPair*(systemIndex: SystemIndex): tuple[typeName: string, id: ComponentTypeId] =
  ## Utility function to yield the name and type id of the types used in the system specified by systemIndex.
  for id in sysRequirements[systemIndex.int]:
    yield (typeName: tNames[id.int], id: id)

template compSystems*: untyped =
  ## Creates a lookup of all known component type ids to a list of systems that use them.
  # sysRequirements is indexed by system to give component, but we want to index component to get systems.
  var r = newSeq[seq[SystemIndex]](tNames.len)
  for i in 0 ..< tNames.len: # 0 is invalid component but we need to include in the result
    # Create a list of system indexes for each component type
    for sysIdx in 0 ..< sysRequirements.len:
      if i.ComponentTypeId in sysRequirements[sysIdx]:
        r[i].add sysIdx.SystemIndex
  r

proc indexRead*(sysNode, entIdNode: NimNode, options: ECSSysOptions): NimNode =
  case options.indexFormat
  of sifTable:
    quote do:
      `sysNode`.index[`entIdNode`]
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int].row

proc indexWrite*(sysNode, entIdNode, rowNode: NimNode, options: ECSSysOptions): NimNode =
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

proc indexDel*(sysNode, entIdNode: NimNode, options: ECSSysOptions): NimNode =
  case options.indexFormat
  of sifTable:
    quote do:
      `sysNode`.index.del(`entIdNode`)
  of sifArray, sifAllocatedSeq:
    quote do:
      `sysNode`.index[`entIdNode`.int].exists = false

proc indexTryGet*(sysNode, entIdNode, rowNode: NimNode, options: ECSSysOptions): NimNode =
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

proc updateIndex*(entity: NimNode, sys: SystemIndex, row: NimNode, sysOpts: ECSSysOptions): NimNode =
  ## Update the system index with the entity.
  let
    systemNode = allSystemsNode[sys.int]
    entId = quote do: `entity`.entityId
  result = systemNode.indexWrite(entId, row, sysOpts)

proc addSystemTuple*(systemNode: NimNode, value: NimNode, sysOpts: ECSSysOptions): NimNode =
  # Extend system groups depending on options.
  case sysOpts.storageFormat
    of ssSeq:
      quote do:
        `systemNode`.groups.add(`value`)
    of ssArray:
      quote do:
        `systemNode`.groups[`systemNode`.nextFreeIdx] = `value`
        `systemNode`.nextFreeIdx += 1

proc genSystemUpdate*(entity: NimNode, sys: SystemIndex, componentsPassed: seq[ComponentTypeId], componentValues: NimNode): NimNode =
  ## Assumes you have a generated variable that matches each field in the system tuple already defined.
  let
    sysOpts = ecsSysOptions[sys.int]
    system = allSystemsNode[sys.int]

  # Generate system tuple assignment.
  var
    sysTuple = nnkPar.newTree()
    updateOwnedAlive = newStmtList()
  sysTuple.add nnkExprColonExpr.newTree(ident "entity", entity)
  for fields in sys.systemTypesStrPair:
    let
      tupleFieldStr = fields.typeName.toLowerAscii()
      tupleFieldIdent = ident tupleFieldStr
      compSource = ident tupleFieldStr & instPostfix
    
    if componentSystemOwner[fields.id.int] == sys:
      let compIdx = componentsPassed.find(fields.id)
      assert compIdx >= 0, "Cannot find field for " & tNames[fields.id.int] & " within " & componentValues.repr
      sysTuple.add nnkExprColonExpr.newTree(tupleFieldIdent, componentValues[compIdx])

      # Update alive and instance lists for the owned component.
      # This is normally done in the component's creation.
      let
        aliveIdent = ident aliveStateInstanceName(fields.typeName)
        instanceIdent = ident instanceIdsName(fields.typeName)
        compOpts = ecsCompOptions[fields.id.int]

      if compOpts.componentStorageFormat == cisSeq:
        updateOwnedAlive.add(quote do:
          `aliveIdent`.setLen(`system`.count + 1)
          `aliveIdent`[`system`.count] = true
          `instanceIdent`.setLen(`system`.count + 1)
          `instanceIdent`[`instanceIdent`.high] += 1
        )
    else:
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
    `updateOwnedAlive`
    `updateGroup`
    `updateIndex`


# State list updates


proc updateOwnedComponentState*(typeId: ComponentTypeId, system: SystemIndex): NimNode =
  let
    typeStr = tNames[typeId.int]
    aliveIdent = ident aliveStateInstanceName(typeStr)
    instanceIdent = ident instanceIdsName(typeStr)
    compOpts = ecsCompOptions[typeId.int]
    systemNode = allSystemsNode[system.int]
  
  # TODO: Generation.

  case compOpts.componentStorageFormat
  of cisSeq:
    quote do:
      `aliveIdent`.setLen `systemNode`.count + 1
      `aliveIdent`[`systemNode`.count] = true
      `instanceIdent`.setLen `systemNode`.count + 1
      `instanceIdent`[`systemNode`.count] += 1.IdBaseType
  else:
    quote do:
      `aliveIdent`[`systemNode`.count] = true
      `instanceIdent`[`systemNode`.count] += 1.IdBaseType

# Type utils

iterator commaSeparate*[T: ComponentTypeId or SystemIndex](list: seq[T] or set[T]): string =
  ## Common function to produce a string of comma separated ComponentTypeIds.
  var comma: bool
  for v in list:
    let str =
      when T is ComponentTypeId: tNames[v.int]
      else: systemNames[v.int]
    if comma: yield ", " & str
    else:
      comma = true
      yield str

proc commaSeparate*[T: ComponentTypeId or SystemIndex](list: seq[T] or set[T]): string {.compileTime.} =
  ## Common function to produce a string of comma separated ComponentTypeIds.
  for s in list.commaSeparate: result &= s

iterator typeDefs*(body: NimNode): NimNode =
  ## Return the nnkTypeDef nodes in a body.
  for item in body:
    if item.kind == nnkTypeSection:
      for def in item:
        if def.kind == nnkTypeDef:
          yield def

type FieldList = seq[tuple[fieldNode, typeNode: NimNode]]

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
    echo "Unknown kind: " & $tyNode.kind

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
    echo "Cannot process node: " & node.treerepr

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
  nnkBracketExpr.newTree(
    newIdentNode "Table" ,
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
    valueNode.getType()

import os

const
  # Runtime logging.
  loggingOn* = false
  consistencyChecking* = false
  defaultGenLogFilename* = getProjectPath() / "ecs_code_log.nim"

when defined(debugComponentGeneration):
  ## Compile-time code generation logging data.
  var logData* {.compileTime.}: seq[string]

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

template genLog*(params: varargs[string]) =
  ## Allows macros to generate a log that is then written to file.
  when defined(debugComponentGeneration):
    var s = ""
    for item in params:
      s &= $item
    logData.add s & "\n"

macro startGenLog*(fileName: static[string]): untyped =
  ## Empties log file.
  result = newStmtList()
  when defined(debugComponentGeneration):
    let fn = newLit fileName    
    result = quote do:
      let f = `fn`.open(fmWrite)
      f.close
      echo "Cleared log file \"", `fn`, "\""

macro flushGenLog*(fileName: static[string]): untyped =
  ## Write log to file.
  ## Because we cannot `import c` at compile time and write the log then,
  ## we build the code to statically write the log at run time...
  ## This means a lot of text stored in your program exe before it's
  ## written to the actual log file, so is hidden behind the onerously
  ## long `-d:debugComponentGeneration` flag.
  result = newStmtList()
  when defined(debugComponentGeneration):
    let fn = newLit fileName
    
    # `$logData` doesn't convert `\n` to EOL so we add by line at compile time.
    var logText = ""
    for line in logData:
      logText &= line
    let text = newLit logText

    # Generate *runtime* file output of static log string.
    result = quote do:
      let
        f = `fn`.open(fmAppend)
        total = `text`.len
      try:
        f.write(`text`)
      finally:
        f.close
      echo "Added to log at ", `fn`, ": ", total, " characters written "

proc flushGenLog*() =
  flushGenLog(defaultGenLogFilename)

proc startGenLog*() =
  flushGenLog(defaultGenLogFilename)

proc genInfixes*(clauses: seq[NimNode], connector: string): NimNode =
  ## Creates a tree of infix clauses to match the clauses list.
  var parent = clauses[0]
  for i in 1 ..< clauses.len:
    parent = infix(parent, connector, clauses[i])
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
  genLog "# Typeclass:\n", result.repr

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

