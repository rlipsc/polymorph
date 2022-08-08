# SPDX-License-Identifier: Apache-2.0

# Copyright (c) 2022 Ryan Lipscombe
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


## This module contains utilities for working with `NimNode`.


import macros, strutils, tables


type
  TypeFields* = tuple[typeName: string, fields: seq[tuple[fieldNode, typeNode: NimNode]]]
  FieldList* = seq[tuple[fieldNode, typeNode: NimNode]]
  FlatIdentDef* = tuple[path: seq[string], identDef: NimNode]

# ----------------------------------
# Node inspection / generation utils
# ----------------------------------


proc toIdent*(nn: NimNode): NimNode =
  # Replace symbols with idents
  if nn.kind == nnkSym: ident nn.strVal
  else: nn


proc getFieldsFromRecCase(recCaseNode: NimNode): FieldList =
  # Return all the possible nodes in the case statement and allow
  # the language to assert when a case access is violated.
  recCaseNode.expectKind nnkRecCase

  # Add kind var.
  let kindVar = recCaseNode[0]
  result.add (kindVar[0].basename, kindVar[1].toIdent)

  # All fields from of branches.
  for fieldIdx in 1 ..< recCaseNode.len:
    let field = recCaseNode[fieldIdx]
    field.expectKind nnkOfBranch
    let recList = field[1]
    recList.expectKind nnkRecList
    for ofField in recList:
      result.add (ofField[0].basename, ofField[1].toIdent)


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
            result.fields.add (field[i].basename, fType)

  of nnkRefTy:
    tyNode.expectMinLen 1
    tyNode[0].expectMinLen 3

    let recList = tyNode[0][2]
    if recList.kind == nnkRecList:
      # recList can also be empty when there are no fields.
      for field in recList:
        let fType = field[^2].toIdent
        for i in 0 ..< field.len - 2:
          result.fields.add (field[i].basename, fType)

  of nnkTupleTy:
    typeDefNode.expectMinLen 3
    for field in typeDefNode[2]:
      let fType = field[^2].toIdent
      for i in 0 ..< field.len - 2:
        result.fields.add (field[i].basename, fType)

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
        echo "checkDefForTy: Cannot process node \n", chkNode.treeRepr
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
  echo "Unable to extract record list from type " & tName & ", given this node:\n" & node.treeRepr
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
  let typeName = $T
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
  for n in node:
    if n.kind == nnkStmtList:
      if n.populated:
        return true
    else:
      return true


proc unpack*(node: NimNode, source: NimNode) =
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


proc respondToPragma*(node: NimNode, pragmaName: string, actions: NimNode): NimNode =
  let
    pragmaIdent = ident pragmaName

  quote do:
    static:
      when isSymbol(`node`):
        when hasCustomPragma(`node`, `pragmaIdent`):
          `actions`


iterator flatten*(objTy: NimNode): FlatIdentDef =
  ## Iterate over identDefs for fields in the object type `objTy`
  ## including nested object fields (breadth first).
  ## `objTy` expects a symbol for an object type.

  objTy.expectKind nnkSym
  
  let
    objName = objTy.strVal
    impl = objTy.getType
  impl.expectKind nnkBracketExpr
  impl.expectLen 2

  let symImpl = impl[1].getTypeImpl
  symImpl.expectKind nnkObjectTy

  # Note: the top level of the path uses the type name rather than
  # the field name.
  var queue = @[(@[objName], symImpl)]
  
  while queue.len > 0:
    let
      item = queue.pop
      path = item[0]
      ty = item[1]

    # Process fieldDefs for this object type.
    for fieldDef in ty[2]:
      fieldDef.expectKind nnkIdentDefs

      let
        fieldNameStr = fieldDef[0].strVal
        fieldTyImpl = fieldDef[1].getTypeImpl
      
      if fieldTyImpl.kind == nnkObjectTy:
        # Add a nested object type for processing.
        queue.add (path & fieldNameStr, fieldTyImpl)
      else:
        yield (path, fieldDef)


# ----------------------
# Source path processing
# ----------------------


proc getCallSitePath*(node: NimNode): string =
  let li = node.lineInfoObj
  result = li.filename

  let
    importSep = '/'
    backSep = '\\'
  
  template found(v: int): bool = v > -1

  # Find OS directory separator from 'lineInfo'.
  var parentSep = result.rFind backSep
  let dirSep =
    if found(parentSep): backSep
    else:
      parentSep = result.rFind importSep
      importSep
    
  if not parentSep.found:
    error "ECS import cannot find a '\\' or '/' separator from the call site: '" & result & "'"
  
  result = result[0 .. parentSep]
  result = result.replace " "
  if dirSep != importSep:
    result = result.replace(dirSep, importSep)


proc getFilename*(str: string): string =
  ## Extract filename from a path.
  let dirSep1 = str.rFind('/')
  if dirSep1 > -1:
    if dirSep1 == str.high:
      # String ends with separator, check for previous separator.
      let dirSep2 = str.rFind('/', last = str.high - 1)
      if dirSep2 > -1:
        if dirSep1 - dirSep2 < 1: error "ecsImport: cannot read '//' in " & str
        str[dirSep2 + 1 ..< dirSep1]
      else:
        str[0 ..< dirSep1]
    else:
      str[dirSep1 + 1 .. ^1]
  else:
    str


# --------------
# Source parsing
# --------------


proc unType(node: NimNode): NimNode =
  case node.kind:
  of {nnkIdent, nnkSym}:
    return ident(node.strVal)
  else:
    if result.len > 0:
      result = node.kind.newTree()
      for child in node:
        result.add unType(child)
    else:
      return node.copy


template isExport(n: NimNode): bool =
  if n.kind == nnkPostFix and n[0].kind == nnkIdent and n[0].strVal == "*":
    true
  else:
    false


proc remExport(node: NimNode, id: int) =
  if node[id].kind == nnkPragmaExpr:
    if node[id][0].isExport:
      node[id][0] = node[id][0][1]
  else:
    if node[id].isExport:
      node[id] = node[id][1]


proc deExport*(code: NimNode, suppressUnusedHints = true): NimNode =
  ## Removes top level export markers from templates, procs, funcs,
  ## macros, and var, let, and const sections.

  result = unType(code)

  let hasCode = code.populated

  if suppressUnusedHints and hasCode:
    result.insert(0, quote do:
      {.push used.}
    )

  for i in 0 ..< result.len:

    # Unpack postfix export markers in place.

    case result[i].kind

      of nnkProcDef, nnkTemplateDef, nnkIteratorDef, nnkMethodDef, nnkFuncDef, nnkMacroDef:
        if result[i][0].isExport:
          result[i][0] = result[i][0][1]

      of nnkWhenStmt:
        for def in 0 ..< result[i].len:
          if result[i][def].kind in [nnkElifBranch]:
            result[i][def][1].expectKind nnkStmtList
            result[i][def][1] = result[i][def][1].deExport

      of nnkConstSection, nnkLetSection, nnkVarSection, nnkTypeSection:
        for def in 0 ..< result[i].len:
          if result[i][def].kind in [nnkConstDef, nnkIdentDefs, nnkTypeDef, nnkElifBranch]:
            for id in 0 ..< result[i][def].len:
              result[i][def].remExport id

      of nnkStmtList:
        # Only the top level should push and pop {.used.}.
        result[i] = result[i].deExport(suppressUnusedHints = false)

      else:
        discard

  if suppressUnusedHints and hasCode:
    result.add(quote do:
      {.pop.}
    )


# ---------------
# Import handling
# ---------------


proc leafFromImport(node: NimNode): NimNode =
  case node.kind
    of nnkIdent:
      node          # Single module.
    of nnkInfix:
      leafFromImport(node[^1])
    of nnkPrefix:
      leafFromImport(node[^1])
    of nnkBracket:  # Multiple modules.
      node
    else:
      nil


proc splitModules*(curImports: NimNode, modules: NimNode): NimNode =
  ## Returns a node with deduplicated modules and their call site.
  if curImports.isNil:
    result = newStmtList()
  else:
    result = curImports.copy

  template unique(m): bool =
    result.findChild(
      it.kind == nnkBracket and
      it[1] == m
    ).isNil

  # Import statements bind their location to the macro source.
  let callSitePath = modules.getCallSitePath

  for i, node in modules:
    let leaf = leafFromImport(node)
    if leaf.kind == nnkBracket and leaf.len > 0:
      for m in leaf:
        if unique(m):
          # Isolate branch as a separate import.
          var branch = node
          branch.del branch.len - 1
          branch.add m
          result.add nnkBracket.newTree(newLit callSitePath, branch.copy)
    else:
      if unique(node):
        result.add nnkBracket.newTree(newLit callSitePath, node.copy)
