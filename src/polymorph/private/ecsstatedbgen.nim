import macros, macrocache, strutils, nodeutils

const ecsAccessProcCache* = CacheSeq("ecsGeneratedAccessProcs")

#--------------------------------
# CacheSeq access proc generation
#--------------------------------

proc typeNodeAccess(typeNode: NimNode): tuple[accessNode, accessType: NimNode] =
  ## Determine the "base type" of returnType where `typeNode` may
  ## be a distinct type.
  ## Since this is an internal routine, we only cover a few types.
  let
    tyImpl = typeNode.getTypeImpl
    tyImplKind = tyImpl.typeKind

  assert tyImplKind == ntyTypeDesc
  tyImpl.expectKind nnkBracketExpr
  tyImpl.expectMinLen 2

  var typeSym = tyImpl[1]
  
  typeSym.expectKind nnkSym
  
  var sourceType = typeSym.getType
  
  if sourceType.kind == nnkEnumTy:
    sourceType = typeSym
    return (ident "intVal", ident "int")

  if sourceType.typeKind == ntyDistinct:
    typeSym = sourceType[1]

  case typeSym.strVal
  of "Natural", "int", "uint", "uint16", "uint32":
    return (ident "intVal", typeSym.copy)
  of "bool":
    return (ident "boolVal", typeSym.copy)
  of "string":
    return (ident "strVal", typeSym.copy)
  of "NimNode":
    return (nil, nil)
  else:
    error "Unhandled type: " & typeNode.repr


macro genListStates*(indexType, itemType: typedesc, listNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `listNames`.
  ## Each list is a sequence of `itemType` taking `indexType` as the index.
  
  result = newStmtList()
  result.add(quote do:
    {.push hint[ConvFromXtoItselfNotNeeded]:off.}
  )

  let
    (nodeGet, accessType) = typeNodeAccess itemType
    (_, idx_accessType) = typeNodeAccess indexType
    opKey = $itemType
    indexTypeStr = $indexType
    id = ident "id"
    res = ident "result"
    param = ident( toLowerAscii(indexTypeStr[0]) & indexTypeStr[1..^1] & "Value")
    listLen = ident "listLen"
    value = ident "value"
    item = ident "item"
    idx = ident "i"
    fetchedKey = ident "keyVal"
    event = ident "event"

  for listStr in listNames:
    let
      cList = listStr.capitalizeAscii
      listKey = newLit cList & opKey
      listGet = ident listStr
      listGetNode = ident listStr & "Node"
      listAppend = ident "add" & cList
      listLenProc = ident "len" & cList

      readNode =
        if nodeGet != nil:
          quote do: `itemType`(`item`.`nodeGet`)
        else:
          quote do: `item`

      writeNode =
        if accessType != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
      
      paramKey =
        if idx_accessType != nil and idx_accessType != indexType:
          quote do: $`idx_accessType`(`param`)
        else:
          quote do: $`param`

      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `listKey` & `paramKey`)
    
    if $itemType == "NimNode":
      # Extra convenience proc for NimNodes for wrapping with a
      # statement list.
      let res = ident "result"

      result.add(quote do:
        proc `listGetNode`*(`id`: EcsIdentity, `param`: `indexType`): `itemType` {.compileTime.} =
          `res` = newStmtList()
          let `fetchedKey` = `key`
          for `event` in `fetchedKey`.items:
            `res`.add(`event`)
      )
    
    result.add(quote do:
      proc `listLenProc`*(`id`: EcsIdentity, `param`: `indexType`): Natural {.compileTime.} =
        `key`.len

      proc `listAppend`*(`id`: EcsIdentity, `param`: `indexType`, `value`: `itemType`) {.compileTime.} =
        `key`.add(`writeNode`)
      
      proc `listGet`*(`id`: EcsIdentity, `param`: `indexType`): seq[`itemType`] {.compileTime.} =
        let
          `fetchedKey` = `key`
          `listLen` = `fetchedKey`.len

        if `listLen` > 0:
          `res`.setLen `listLen`
          var `idx`: int
          for `item` in `fetchedKey`:
            `res`[`idx`] = `readNode`
            `idx`.inc
    )
  result.add(quote do:
    {.push hint[ConvFromXtoItselfNotNeeded]:on.}
  )
  ecsAccessProcCache.add newLit(result.repr)


macro genLookupListStates*(indexType1, indexType2, itemType: typedesc, listNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `listNames`.
  ## Each list is a sequence of `itemType` taking `indexType` as the index.
  
  result = newStmtList()

  let
    (_, idx_accessType1) = typeNodeAccess indexType1
    (_, idx_accessType2) = typeNodeAccess indexType2
    (nodeGet, accessType) = typeNodeAccess itemType

    opKey = $itemType
    indexTypeStr1 = $indexType1 & "1"
    indexTypeStr2 = $indexType2 & "2"
    id = ident "id"
    res = ident "result"
    value = ident "value"
    item = ident "item"
    fetchedKey = ident "keyVal"
    event = ident "event"
    i = ident "i"
    listLen = ident "listLen"

    param1 = ident( toLowerAscii(indexTypeStr1[0]) & indexTypeStr1[1..^1] & "Value")
    param2 = ident( toLowerAscii(indexTypeStr2[0]) & indexTypeStr2[1..^1] & "Value")

  for listStr in listNames:
    let
      cList = listStr.capitalizeAscii
      listKey = newLit cList & opKey
      listGet = ident listStr
      listGetNode = ident listStr & "Node"
      listAppend = ident "add" & cList
      listLenProc = ident "len" & cList

      readNode =
        if nodeGet != nil:
          quote do: `itemType`(`item`.`nodeGet`)
        else:
          if accessType != nil:
            quote do: `itemType`(`item`)
          else:
            quote do: `item`

      writeNode =
        if accessType != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
      p1Key =
        if idx_accessType1 != nil:
          quote do: $`idx_accessType1`(`param1`)
        else:
          quote do: $`param1`
      p2Key =
        if idx_accessType2 != nil:
          quote do: $`idx_accessType2`(`param2`)
        else:
          quote do: $`param2`

    let
      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `listKey` & `p1Key` & "_" & `p2key`)

    if $itemType == "NimNode":
      # Extra convenience proc for NimNodes for wrapping with a
      # statement list.

      result.add(quote do:
        proc `listGetNode`*(`id`: EcsIdentity, `param1`: `indexType1`, `param2`: `indexType2`): `itemType` {.compileTime.} =
          `res` = newStmtList()
          let `fetchedKey` = `key`
          for `event` in `fetchedKey`.items:
            `res`.add(`event`)
      )
    
    result.add(quote do:
      proc `listLenProc`*(`id`: EcsIdentity, `param1`: `indexType1`, `param2`: `indexType2`): Natural {.compileTime.} =
        `key`.len

      proc `listAppend`*(`id`: EcsIdentity, `param1`: `indexType1`, `param2`: `indexType2`, `value`: `itemType`) {.compileTime.} =
        `key`.add(`writeNode`)
      
      proc `listGet`*(`id`: EcsIdentity, `param1`: `indexType1`, `param2`: `indexType2`): seq[`itemType`] {.compileTime.} =
        let
          `fetchedKey` = `key`
          `listLen` = `fetchedKey`.len

        if `listLen` > 0:
          `res`.setLen `listLen`
          var `i`: int
          for `item` in `fetchedKey`:
            `res`[`i`] = `readNode`
            `i`.inc
    )
  ecsAccessProcCache.add newLit(result.repr)


macro genItemStates*(indexType, itemType: typedesc, itemNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `itemNames`.
  ## Each item represents an individual value indexed by type id.
  ## The index type is integrated into the key.
  ## 
  ## Though all additions to a value are recorded, only the most
  ## recent value is returned.
  
  result = newStmtList()
  
  let
    (accessNode, accessType) = typeNodeAccess itemType
    indexTypeStr = $indexType
    id = ident "id"
    param = ident( toLowerAscii(indexTypeStr[0]) & indexTypeStr[1..^1] & "Value")
    res = ident "result"

    value = ident "value"
    isNimNode = accessNode.isNil

    key = ident "key"
    keyLen = ident "keyLen"
    idx = ident "idx"
    n = ident "n"
    curList = ident "curList"
    entry = ident "entry"

  for itemStr in itemNames:
    let
      itemKey = newLit itemStr & indexTypeStr
      getItem = ident itemStr
      setItem = ident "set" & itemStr
      
      # NOTE: Calling another generated access proc within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      getKey = quote do:
        CacheSeq(`id`.string & `itemKey`)
      
      lastEntry = quote do: `key`[`key`.len - 1]

      getEntry =
        if accessType != nil: quote do:
          `accessNode`(`lastEntry`[`idx`])
        else: quote do:
          `lastEntry`[`idx`]
      
      readNode =
        quote do:
          block:
            let
              `key` = `getKey`
              `keyLen` = `key`.len
              `idx` = `param`.int
            if `keyLen` > 0:
              let `entry` = `key`[`keyLen` - 1].copy
              if `entry`.len > `idx` and `lastEntry`[`idx`].kind != nnkEmpty:
                `itemType`(`getEntry`)
              else:
                default(`itemType`)
            else:
              default(`itemType`)


      writeNode =
        if accessNode != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
    
    result.add(quote do:
      
      proc `getItem`*(`id`: EcsIdentity, `param`: `indexType`): `itemType` {.compileTime.} =
        `id`.checkId(`param`)
        {.push hint[ConvFromXtoItselfNotNeeded]: off.}
        `res` = `readNode`
        {.pop.}

      proc `setItem`*(`id`: EcsIdentity, `param`: `indexType`, `value`: `itemType`) {.compileTime.} =
        {.push hint[ConvFromXtoItselfNotNeeded]: off.}
        `id`.checkId(`param`)
        let
          `idx` = `param`.int
          `keyLen` = `getKey`.len
        var `curList` = newStmtList()
        if `keyLen` > 0:
          for `n` in `getKey`[`keyLen` - 1]:
            `curList`.add(`n`)
        while `curList`.len <= `idx`:
          `curList`.add newEmptyNode()
        `curList`[`idx`] = `writeNode`
        `getKey`.add(`curList`)
        {.pop.}
    )

    if isNimNode:
      # Helper for appending code to NimNode states.
      let appendItem = ident "append" & itemStr

      result.add(quote do:
        proc `appendItem`*(`id`: EcsIdentity, `param`: `indexType`, `value`: `itemType`) {.compileTime.} =
          {.push hint[ConvFromXtoItselfNotNeeded]: off.}

          # TODO: refactor
          let
            `idx` = `param`.int
            `keyLen` = `getKey`.len
          var `curList` = newStmtList()
          if `keyLen` > 0:
            for `n` in `getKey`[`keyLen` - 1]:
              `curList`.add(`n`)
          while `curList`.len <= `idx`:
            `curList`.add newEmptyNode()
          if `curList`[`idx`].kind == nnkEmpty:
            `curList`[`idx`] = newStmtList(`value`)
          else:
            `curList`[`idx`].add(`value`)
          `getKey`.add(`curList`)
          {.pop.}
      )
  ecsAccessProcCache.add newLit(result.repr)


macro genGlobalStates*(itemType: typedesc, itemNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `itemNames`.
  ## Each item represents an individual value indexed by type id.
  ## Though all additions to a value are recorded, only the most
  ## recent value is returned.
  result = newStmtList()
  
  let
    (accessNode, accessType) = typeNodeAccess itemType
    id = ident "id"
    res = ident "result"
    value = ident "value"
    fetchedKey = ident "keyVal"
    curNode = ident "curNode"

  for itemStr in itemNames:
    let
      itemKey = newLit itemStr
      getItem = ident itemStr
      setItem = ident "set" & itemStr
      isNimNode = accessNode.isNil

      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `itemKey`)

      readNode =
        if accessType != nil:
          quote do:
            block:
              let `fetchedKey` = `key`
              if `fetchedKey`.len > 0:
                `fetchedKey`[`fetchedKey`.len - 1].`accessNode`.`itemType`
              else:
                default(`itemType`)
        else:
          quote do:
            block:
              let `fetchedKey` = `key`
              if `fetchedKey`.len > 0:
                `fetchedKey`[`fetchedKey`.len - 1].`itemType`
              else:
                default(`itemType`)

      writeNode =
        if accessNode != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
    
    result.add(quote do:
      
      proc `getItem`*(`id`: EcsIdentity): `itemType` {.compileTime.} =
        {.push hint[ConvFromXtoItselfNotNeeded]: off.}
        `res` = `readNode`
        {.pop.}

      proc `setItem`*(`id`: EcsIdentity, `value`: `itemType`) {.compileTime.} =
        {.push hint[ConvFromXtoItselfNotNeeded]: off.}
        `key`.add(`writeNode`)
        {.pop.}
    )

    if isNimNode:
      # Helper for appending code to NimNode states.
      let appendItem = ident "append" & itemStr

      result.add(quote do:
        proc `appendItem`*(`id`: EcsIdentity, `value`: `itemType`) {.compileTime.} =
          {.push hint[ConvFromXtoItselfNotNeeded]: off.}
          var `curNode` = `readNode`
          if `curNode`.isNil:
            `curNode` = newStmtList()
          `curNode`.add(`value`)
          `key`.add(`curNode`)
          {.pop.}
      )
  ecsAccessProcCache.add newLit(result.repr)


macro genGlobalListStates*(itemType: typedesc, listNames: static[openarray[string]]): untyped =
  ## Creates typed CacheSeq access procs for each item in `listNames`.
  ## Each list is a sequence of `itemType` taking `indexType` as the index.
  
  result = newStmtList()

  let
    (nodeGet, accessType) = typeNodeAccess itemType
    opKey = $itemType
    id = ident "id"
    res = ident "result"
    # Bind items for CacheString.
    csItems = bindSym "items"
    value = ident "value"
    item = ident "item"
    fetchedKey = ident "keyVal"
    i = ident "i"

  for listStr in listNames:
    let
      cList = listStr.capitalizeAscii
      listKey = newLit cList & opKey
      listGet = ident listStr
      listGetNode = ident listStr & "Node"
      listAppend = ident "add" & cList
      listLen = ident "len" & cList
      listAccess = ident "cacheSeq" & cList

      readNode =
        if nodeGet != nil:
          quote do: `itemType`(`item`.`nodeGet`)
        else:
          quote do: `item`

      writeNode =
        if accessType != nil:
          quote do: newLit(`accessType`(`value`))
        else:
          quote do: `value`
      
      # NOTE: Calling another generated access procs within this quote
      # statement may cause the key to be duplicated and therefore incorrect.
      key = quote do:
        CacheSeq(`id`.string & `listKey`)

    if $itemType == "NimNode":
      # Extra convenience proc for NimNodes for wrapping with a
      # statement list.

      result.add(quote do:
        proc `listGetNode`*(`id`: EcsIdentity): `itemType` {.compileTime.} =
          `res` = newStmtList()
          let `fetchedKey` = `key`
          for `item` in `fetchedKey`.items:
            `res`.add `item`
      )

    result.add(quote do:
      {.push hint[ConvFromXtoItselfNotNeeded]: off.}

      proc `listLen`*(`id`: EcsIdentity): Natural {.compileTime.} =
        `key`.len

      proc `listAccess`*(`id`: EcsIdentity): CacheSeq {.compileTime.} = `key`

      proc `listAppend`*(`id`: EcsIdentity, `value`: `itemType`) {.compileTime.} =
        `key`.add(`writeNode`)

      proc `listGet`*(`id`: EcsIdentity): seq[`itemType`] {.compileTime.} =
        let
          `fetchedKey` = `key`
          `listLen` = `fetchedKey`.len
        if `listLen` > 0:
          `res`.setLen `listLen`
          var `i`: int
          for `item` in `fetchedKey`.`csItems`:
            `res`[`i`] = `readNode`
            `i`.inc
      
      {.pop.}
    )
  ecsAccessProcCache.add newLit(result.repr)

#-------------------------------
# ECS database access generation
#-------------------------------


macro outputGeneratedState*(filename: string): untyped =
  ## Output code to write the generated ecs state procs to a file.
  ## This only needs to be done when state db entries change.
  var codeText = """
## This module contains the `ecsstatedb` generated procedures for accessessing
## `macrocache` compile time state.
## 
## Changes to `ecsstatedb` may require regeneration of this file which can
## be done by running `ecsstatedb` as a stand alone module.
## 


import macrocache, macros, ../sharedtypes, identities

"""

  for n in ecsAccessProcCache:
    codeText &= n.strVal

  let
    path = newLit filename.getCallSitePath
    expLen = codeText.len
    codeLit = newLit codeText

  result = quote do:
    let fn = `path` & `filename` & ".nim"
    echo "Creating ECS state access '", fn, "'"
    let
      f = fn.open(fmWrite)
      total = `codeLit`.len
    try:
      f.write(`codeLit`)
    finally:
      f.close
    
    if total != `expLen`:
      echo "  Warning: ECS state access file expected to be length " &
        $`expLen` & " but OS returned " & $total


macro genObjAccessor*(accessName: untyped, storageType: typedesc): untyped =
  ## Generates CacheSeq access procs for each field within
  ## `storageType` and a helper to get or set the `storageType` in one
  ## operation.
  ## 
  ## Note that nested object types within `storageType` are flattened
  ## in macrocache storage. As such, deeply nested types with duplicate
  ## names may clash.
  ## 
  ## In this example MyType.name will clash with MyType.otherName.name.
  ## 
  ##    type
  ##      MyName = object
  ##        name: string
  ##    
  ##      MyType = object
  ##        name: string
  ##        otherName: MyName
  ##    
  ##    genObjAccessor("myType", MyType)
  ## 
  ## Therefore, keep names unique and avoid nesting.
  ## 
  if accessName.kind notin [nnkIdent, nnkStrLit]:
    error "Expected an ident or string literal for accessName"
  
  let
    setName = ident "set" & accessName.strVal
    getName = ident accessName.strVal
    id = ident "id"
    value = ident "value"

  result = newStmtList()

  var
    setInner = newStmtList()
    getInner = newStmtList()
    accessors = newStmtList()
    objAccess = newStmtList()

  # Step through IdentDefs of the storageType.
  # The lineage of the type is returned in field.path.
  for field in storageType.flatten:
    let
      fieldName = field.identDef[0].strVal
      fieldType = ident field.identDef[1].strVal

    accessors.add(quote do:
      genGlobalStates(`fieldType`, [`fieldName`]) # Note: updates ecsAccessProcCache.
    )

    let
      setField = ident "set" & fieldName
      getField = ident fieldName
    var
      setPath = value
      getPath = ident "result"

    for i in 1 ..< field.path.len:
      setPath = newDotExpr(setPath, ident field.path[i])
      getPath = newDotExpr(getPath, ident field.path[i])

    setInner.add(quote do:
      `id`.`setField`(`setPath`.`getField`)
    )
    getInner.add(quote do:
      `getPath`.`getField` = `id`.`getField`
    )

  objAccess.add(quote do:
    proc `setName`*(`id`: EcsIdentity, `value`: `storageType`) =
      `setInner`
    proc `getName`*(`id`: EcsIdentity): `storageType` =
      `getInner`
  )
  
  let codeText = objAccess.repr

  result.add(quote do:
    `accessors`
    `objAccess`
    static:
      ecsAccessProcCache.add(newLit(`codeText`))
  )
