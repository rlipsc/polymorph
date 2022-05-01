## This module contains the `ecsstatedb` generated procedures for accessessing
## `macrocache` compile time state.
## 
## Changes to `ecsstatedb` may require regeneration of this file which can
## be done by running `ecsstatedb` as a stand alone module.
## 


import macrocache, macros, ../sharedtypes, identities


proc ecsInstanceType*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): string {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "ecsInstanceTypeComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        string(strVal(key[key.len - 1][idx]))
      else:
        default(string)
    else:
      default(string)
  {.pop.}

proc setecsInstanceType*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                         value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "ecsInstanceTypeComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "ecsInstanceTypeComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(string(value))
  CacheSeq(id.string & "ecsInstanceTypeComponentTypeId").add(curList)
  {.pop.}

proc refType*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): string {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "refTypeComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        string(strVal(key[key.len - 1][idx]))
      else:
        default(string)
    else:
      default(string)
  {.pop.}

proc setrefType*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                 value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "refTypeComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "refTypeComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(string(value))
  CacheSeq(id.string & "refTypeComponentTypeId").add(curList)
  {.pop.}

proc initPrefix*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): string {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "initPrefixComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        string(strVal(key[key.len - 1][idx]))
      else:
        default(string)
    else:
      default(string)
  {.pop.}

proc setinitPrefix*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                    value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "initPrefixComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "initPrefixComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(string(value))
  CacheSeq(id.string & "initPrefixComponentTypeId").add(curList)
  {.pop.}

proc refInitPrefix*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): string {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "refInitPrefixComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        string(strVal(key[key.len - 1][idx]))
      else:
        default(string)
    else:
      default(string)
  {.pop.}

proc setrefInitPrefix*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                       value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "refInitPrefixComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "refInitPrefixComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(string(value))
  CacheSeq(id.string & "refInitPrefixComponentTypeId").add(curList)
  {.pop.}

proc isOwned*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): bool {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "isOwnedComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        bool(boolVal(key[key.len - 1][idx]))
      else:
        default(bool)
    else:
      default(bool)
  {.pop.}

proc setisOwned*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                 value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "isOwnedComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "isOwnedComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(bool(value))
  CacheSeq(id.string & "isOwnedComponentTypeId").add(curList)
  {.pop.}

proc systemOwner*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): SystemIndex {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "systemOwnerComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        SystemIndex(intVal(key[key.len - 1][idx]))
      else:
        default(SystemIndex)
    else:
      default(SystemIndex)
  {.pop.}

proc setsystemOwner*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                     value: SystemIndex) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "systemOwnerComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "systemOwnerComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "systemOwnerComponentTypeId").add(curList)
  {.pop.}

{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenSystems*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "SystemsSystemIndex" & $uint16(componentTypeIdValue)).len

proc addSystems*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                 value: SystemIndex) {.compileTime.} =
  CacheSeq(id.string & "SystemsSystemIndex" & $uint16(componentTypeIdValue)).add(
      newLit(int(value)))

proc systems*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    SystemIndex] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "SystemsSystemIndex" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = SystemIndex(item.intVal)
      i.inc

proc lenDependentOwners*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "DependentOwnersSystemIndex" &
      $uint16(componentTypeIdValue)).len

proc addDependentOwners*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                         value: SystemIndex) {.compileTime.} =
  CacheSeq(id.string & "DependentOwnersSystemIndex" &
      $uint16(componentTypeIdValue)).add(newLit(int(value)))

proc dependentOwners*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    SystemIndex] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "DependentOwnersSystemIndex" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = SystemIndex(item.intVal)
      i.inc

proc lenLinked*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "LinkedSystemIndex" & $uint16(componentTypeIdValue)).len

proc addLinked*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                value: SystemIndex) {.compileTime.} =
  CacheSeq(id.string & "LinkedSystemIndex" & $uint16(componentTypeIdValue)).add(
      newLit(int(value)))

proc linked*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    SystemIndex] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "LinkedSystemIndex" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = SystemIndex(item.intVal)
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenDependentComps*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "DependentCompsComponentTypeId" &
      $uint16(componentTypeIdValue)).len

proc addDependentComps*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                        value: ComponentTypeId) {.compileTime.} =
  CacheSeq(id.string & "DependentCompsComponentTypeId" &
      $uint16(componentTypeIdValue)).add(newLit(uint16(value)))

proc dependentComps*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "DependentCompsComponentTypeId" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc componentDefinitionsNode*(id: EcsIdentity;
                               componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "ComponentDefinitionsNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenComponentDefinitions*(id: EcsIdentity;
                              componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "ComponentDefinitionsNimNode" &
      $uint16(componentTypeIdValue)).len

proc addComponentDefinitions*(id: EcsIdentity;
                              componentTypeIdValue: ComponentTypeId;
                              value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "ComponentDefinitionsNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc componentDefinitions*(id: EcsIdentity;
                           componentTypeIdValue: ComponentTypeId): seq[NimNode] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "ComponentDefinitionsNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
proc maxComponents*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "maxComponentsComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        Natural(intVal(key[key.len - 1][idx]))
      else:
        default(Natural)
    else:
      default(Natural)
  {.pop.}

proc setmaxComponents*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                       value: Natural) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "maxComponentsComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "maxComponentsComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(Natural(value))
  CacheSeq(id.string & "maxComponentsComponentTypeId").add(curList)
  {.pop.}

proc componentStorageFormat*(id: EcsIdentity;
                             componentTypeIdValue: ComponentTypeId): ECSCompItemStorage {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "componentStorageFormatComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSCompItemStorage(intVal(key[key.len - 1][idx]))
      else:
        default(ECSCompItemStorage)
    else:
      default(ECSCompItemStorage)
  {.pop.}

proc setcomponentStorageFormat*(id: EcsIdentity;
                                componentTypeIdValue: ComponentTypeId;
                                value: ECSCompItemStorage) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "componentStorageFormatComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "componentStorageFormatComponentTypeId")[
        keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "componentStorageFormatComponentTypeId").add(curList)
  {.pop.}

proc accessMethod*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): ECSAccessMethod {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "accessMethodComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSAccessMethod(intVal(key[key.len - 1][idx]))
      else:
        default(ECSAccessMethod)
    else:
      default(ECSAccessMethod)
  {.pop.}

proc setaccessMethod*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                      value: ECSAccessMethod) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "accessMethodComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "accessMethodComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "accessMethodComponentTypeId").add(curList)
  {.pop.}

proc recyclerFormat*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): ECSCompRecyclerFormat {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "recyclerFormatComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSCompRecyclerFormat(intVal(key[key.len - 1][idx]))
      else:
        default(ECSCompRecyclerFormat)
    else:
      default(ECSCompRecyclerFormat)
  {.pop.}

proc setrecyclerFormat*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                        value: ECSCompRecyclerFormat) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "recyclerFormatComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "recyclerFormatComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "recyclerFormatComponentTypeId").add(curList)
  {.pop.}

proc clearAfterDelete*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): bool {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "clearAfterDeleteComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        bool(boolVal(key[key.len - 1][idx]))
      else:
        default(bool)
    else:
      default(bool)
  {.pop.}

proc setclearAfterDelete*(id: EcsIdentity;
                          componentTypeIdValue: ComponentTypeId; value: bool) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "clearAfterDeleteComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "clearAfterDeleteComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(bool(value))
  CacheSeq(id.string & "clearAfterDeleteComponentTypeId").add(curList)
  {.pop.}

proc useThreadVar*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): bool {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "useThreadVarComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        bool(boolVal(key[key.len - 1][idx]))
      else:
        default(bool)
    else:
      default(bool)
  {.pop.}

proc setuseThreadVar*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                      value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "useThreadVarComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "useThreadVarComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(bool(value))
  CacheSeq(id.string & "useThreadVarComponentTypeId").add(curList)
  {.pop.}

proc invalidAccess*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): ECSCompInvalidAccess {.
    compileTime.} =
  id.checkId(componentTypeIdValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "invalidAccessComponentTypeId")
      keyLen = key.len
      idx = componentTypeIdValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSCompInvalidAccess(intVal(key[key.len - 1][idx]))
      else:
        default(ECSCompInvalidAccess)
    else:
      default(ECSCompInvalidAccess)
  {.pop.}

proc setinvalidAccess*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                       value: ECSCompInvalidAccess) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(componentTypeIdValue)
  let
    idx = componentTypeIdValue.int
    keyLen = CacheSeq(id.string & "invalidAccessComponentTypeId").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "invalidAccessComponentTypeId")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "invalidAccessComponentTypeId").add(curList)
  {.pop.}

proc sealed*(id: EcsIdentity; systemIndexValue: SystemIndex): bool {.compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "sealedSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        bool(boolVal(key[key.len - 1][idx]))
      else:
        default(bool)
    else:
      default(bool)
  {.pop.}

proc setsealed*(id: EcsIdentity; systemIndexValue: SystemIndex; value: bool) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "sealedSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "sealedSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(bool(value))
  CacheSeq(id.string & "sealedSystemIndex").add(curList)
  {.pop.}

proc useThreadVar*(id: EcsIdentity; systemIndexValue: SystemIndex): bool {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "useThreadVarSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        bool(boolVal(key[key.len - 1][idx]))
      else:
        default(bool)
    else:
      default(bool)
  {.pop.}

proc setuseThreadVar*(id: EcsIdentity; systemIndexValue: SystemIndex;
                      value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "useThreadVarSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "useThreadVarSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(bool(value))
  CacheSeq(id.string & "useThreadVarSystemIndex").add(curList)
  {.pop.}

proc bodyDefined*(id: EcsIdentity; systemIndexValue: SystemIndex): bool {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "bodyDefinedSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        bool(boolVal(key[key.len - 1][idx]))
      else:
        default(bool)
    else:
      default(bool)
  {.pop.}

proc setbodyDefined*(id: EcsIdentity; systemIndexValue: SystemIndex; value: bool) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "bodyDefinedSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "bodyDefinedSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(bool(value))
  CacheSeq(id.string & "bodyDefinedSystemIndex").add(curList)
  {.pop.}

proc instantiation*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "instantiationSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        NimNode(key[key.len - 1][idx])
      else:
        default(NimNode)
    else:
      default(NimNode)
  {.pop.}

proc setinstantiation*(id: EcsIdentity; systemIndexValue: SystemIndex;
                       value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "instantiationSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "instantiationSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = value
  CacheSeq(id.string & "instantiationSystemIndex").add(curList)
  {.pop.}

proc appendinstantiation*(id: EcsIdentity; systemIndexValue: SystemIndex;
                          value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "instantiationSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "instantiationSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  if curList[idx].kind == nnkEmpty:
    curList[idx] = newStmtList(value)
  else:
    curList[idx].add(value)
  CacheSeq(id.string & "instantiationSystemIndex").add(curList)
  {.pop.}

proc ecsSysBodyDefinition*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "ecsSysBodyDefinitionSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        NimNode(key[key.len - 1][idx])
      else:
        default(NimNode)
    else:
      default(NimNode)
  {.pop.}

proc setecsSysBodyDefinition*(id: EcsIdentity; systemIndexValue: SystemIndex;
                              value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "ecsSysBodyDefinitionSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "ecsSysBodyDefinitionSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = value
  CacheSeq(id.string & "ecsSysBodyDefinitionSystemIndex").add(curList)
  {.pop.}

proc appendecsSysBodyDefinition*(id: EcsIdentity; systemIndexValue: SystemIndex;
                                 value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "ecsSysBodyDefinitionSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "ecsSysBodyDefinitionSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  if curList[idx].kind == nnkEmpty:
    curList[idx] = newStmtList(value)
  else:
    curList[idx].add(value)
  CacheSeq(id.string & "ecsSysBodyDefinitionSystemIndex").add(curList)
  {.pop.}

proc extraFields*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "extraFieldsSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        NimNode(key[key.len - 1][idx])
      else:
        default(NimNode)
    else:
      default(NimNode)
  {.pop.}

proc setextraFields*(id: EcsIdentity; systemIndexValue: SystemIndex;
                     value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "extraFieldsSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "extraFieldsSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = value
  CacheSeq(id.string & "extraFieldsSystemIndex").add(curList)
  {.pop.}

proc appendextraFields*(id: EcsIdentity; systemIndexValue: SystemIndex;
                        value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "extraFieldsSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "extraFieldsSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  if curList[idx].kind == nnkEmpty:
    curList[idx] = newStmtList(value)
  else:
    curList[idx].add(value)
  CacheSeq(id.string & "extraFieldsSystemIndex").add(curList)
  {.pop.}

{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenEcsSysRequirements*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "EcsSysRequirementsComponentTypeId" &
      $int(systemIndexValue)).len

proc addEcsSysRequirements*(id: EcsIdentity; systemIndexValue: SystemIndex;
                            value: ComponentTypeId) {.compileTime.} =
  CacheSeq(id.string & "EcsSysRequirementsComponentTypeId" &
      $int(systemIndexValue)).add(newLit(uint16(value)))

proc ecsSysRequirements*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "EcsSysRequirementsComponentTypeId" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

proc lenEcsOwnedComponents*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "EcsOwnedComponentsComponentTypeId" &
      $int(systemIndexValue)).len

proc addEcsOwnedComponents*(id: EcsIdentity; systemIndexValue: SystemIndex;
                            value: ComponentTypeId) {.compileTime.} =
  CacheSeq(id.string & "EcsOwnedComponentsComponentTypeId" &
      $int(systemIndexValue)).add(newLit(uint16(value)))

proc ecsOwnedComponents*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "EcsOwnedComponentsComponentTypeId" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

proc lenEcsSysNegations*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "EcsSysNegationsComponentTypeId" &
      $int(systemIndexValue)).len

proc addEcsSysNegations*(id: EcsIdentity; systemIndexValue: SystemIndex;
                         value: ComponentTypeId) {.compileTime.} =
  CacheSeq(id.string & "EcsSysNegationsComponentTypeId" &
      $int(systemIndexValue)).add(newLit(uint16(value)))

proc ecsSysNegations*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "EcsSysNegationsComponentTypeId" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
proc ecsDeferredSysDef*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "ecsDeferredSysDefSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        NimNode(key[key.len - 1][idx])
      else:
        default(NimNode)
    else:
      default(NimNode)
  {.pop.}

proc setecsDeferredSysDef*(id: EcsIdentity; systemIndexValue: SystemIndex;
                           value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "ecsDeferredSysDefSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "ecsDeferredSysDefSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = value
  CacheSeq(id.string & "ecsDeferredSysDefSystemIndex").add(curList)
  {.pop.}

proc appendecsDeferredSysDef*(id: EcsIdentity; systemIndexValue: SystemIndex;
                              value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "ecsDeferredSysDefSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "ecsDeferredSysDefSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  if curList[idx].kind == nnkEmpty:
    curList[idx] = newStmtList(value)
  else:
    curList[idx].add(value)
  CacheSeq(id.string & "ecsDeferredSysDefSystemIndex").add(curList)
  {.pop.}

proc onEcsCommitSystemCode*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "onEcsCommitSystemCodeSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        NimNode(key[key.len - 1][idx])
      else:
        default(NimNode)
    else:
      default(NimNode)
  {.pop.}

proc setonEcsCommitSystemCode*(id: EcsIdentity; systemIndexValue: SystemIndex;
                               value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "onEcsCommitSystemCodeSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "onEcsCommitSystemCodeSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = value
  CacheSeq(id.string & "onEcsCommitSystemCodeSystemIndex").add(curList)
  {.pop.}

proc appendonEcsCommitSystemCode*(id: EcsIdentity;
                                  systemIndexValue: SystemIndex; value: NimNode) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "onEcsCommitSystemCodeSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "onEcsCommitSystemCodeSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  if curList[idx].kind == nnkEmpty:
    curList[idx] = newStmtList(value)
  else:
    curList[idx].add(value)
  CacheSeq(id.string & "onEcsCommitSystemCodeSystemIndex").add(curList)
  {.pop.}

{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenGroupSystems*(id: EcsIdentity; stringValue: string): Natural {.
    compileTime.} =
  CacheSeq(id.string & "GroupSystemsSystemIndex" & $stringValue).len

proc addGroupSystems*(id: EcsIdentity; stringValue: string; value: SystemIndex) {.
    compileTime.} =
  CacheSeq(id.string & "GroupSystemsSystemIndex" & $stringValue).add(
      newLit(int(value)))

proc groupSystems*(id: EcsIdentity; stringValue: string): seq[SystemIndex] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "GroupSystemsSystemIndex" & $stringValue)
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = SystemIndex(item.intVal)
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenSystemGroups*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "SystemGroupsstring" & $int(systemIndexValue)).len

proc addSystemGroups*(id: EcsIdentity; systemIndexValue: SystemIndex;
                      value: string) {.compileTime.} =
  CacheSeq(id.string & "SystemGroupsstring" & $int(systemIndexValue)).add(
      newLit(string(value)))

proc systemGroups*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[string] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "SystemGroupsstring" & $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = string(item.strVal)
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc onEcsCommitGroupCodeNode*(id: EcsIdentity; stringValue: string): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnEcsCommitGroupCodeNimNode" &
      $stringValue)
  for event in keyVal.items:
    result.add(event)

proc lenOnEcsCommitGroupCode*(id: EcsIdentity; stringValue: string): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnEcsCommitGroupCodeNimNode" & $stringValue).len

proc addOnEcsCommitGroupCode*(id: EcsIdentity; stringValue: string;
                              value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnEcsCommitGroupCodeNimNode" & $stringValue).add(value)

proc onEcsCommitGroupCode*(id: EcsIdentity; stringValue: string): seq[NimNode] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnEcsCommitGroupCodeNimNode" & $stringValue)
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
proc maxEntities*(id: EcsIdentity; systemIndexValue: SystemIndex): int {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "maxEntitiesSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        int(intVal(key[key.len - 1][idx]))
      else:
        default(int)
    else:
      default(int)
  {.pop.}

proc setmaxEntities*(id: EcsIdentity; systemIndexValue: SystemIndex; value: int) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "maxEntitiesSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "maxEntitiesSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "maxEntitiesSystemIndex").add(curList)
  {.pop.}

proc storageFormat*(id: EcsIdentity; systemIndexValue: SystemIndex): ECSSysStorage {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "storageFormatSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSSysStorage(intVal(key[key.len - 1][idx]))
      else:
        default(ECSSysStorage)
    else:
      default(ECSSysStorage)
  {.pop.}

proc setstorageFormat*(id: EcsIdentity; systemIndexValue: SystemIndex;
                       value: ECSSysStorage) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "storageFormatSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "storageFormatSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "storageFormatSystemIndex").add(curList)
  {.pop.}

proc indexFormat*(id: EcsIdentity; systemIndexValue: SystemIndex): ECSSysIndexFormat {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "indexFormatSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSSysIndexFormat(intVal(key[key.len - 1][idx]))
      else:
        default(ECSSysIndexFormat)
    else:
      default(ECSSysIndexFormat)
  {.pop.}

proc setindexFormat*(id: EcsIdentity; systemIndexValue: SystemIndex;
                     value: ECSSysIndexFormat) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "indexFormatSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "indexFormatSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "indexFormatSystemIndex").add(curList)
  {.pop.}

proc timings*(id: EcsIdentity; systemIndexValue: SystemIndex): ECSSysTimings {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "timingsSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSSysTimings(intVal(key[key.len - 1][idx]))
      else:
        default(ECSSysTimings)
    else:
      default(ECSSysTimings)
  {.pop.}

proc settimings*(id: EcsIdentity; systemIndexValue: SystemIndex;
                 value: ECSSysTimings) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "timingsSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "timingsSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "timingsSystemIndex").add(curList)
  {.pop.}

proc echoRunning*(id: EcsIdentity; systemIndexValue: SystemIndex): ECSSysEcho {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "echoRunningSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSSysEcho(intVal(key[key.len - 1][idx]))
      else:
        default(ECSSysEcho)
    else:
      default(ECSSysEcho)
  {.pop.}

proc setechoRunning*(id: EcsIdentity; systemIndexValue: SystemIndex;
                     value: ECSSysEcho) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "echoRunningSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "echoRunningSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "echoRunningSystemIndex").add(curList)
  {.pop.}

proc assertItem*(id: EcsIdentity; systemIndexValue: SystemIndex): bool {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "assertItemSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        bool(boolVal(key[key.len - 1][idx]))
      else:
        default(bool)
    else:
      default(bool)
  {.pop.}

proc setassertItem*(id: EcsIdentity; systemIndexValue: SystemIndex; value: bool) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "assertItemSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "assertItemSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(bool(value))
  CacheSeq(id.string & "assertItemSystemIndex").add(curList)
  {.pop.}

proc orderedRemove*(id: EcsIdentity; systemIndexValue: SystemIndex): bool {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "orderedRemoveSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        bool(boolVal(key[key.len - 1][idx]))
      else:
        default(bool)
    else:
      default(bool)
  {.pop.}

proc setorderedRemove*(id: EcsIdentity; systemIndexValue: SystemIndex;
                       value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "orderedRemoveSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "orderedRemoveSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(bool(value))
  CacheSeq(id.string & "orderedRemoveSystemIndex").add(curList)
  {.pop.}

proc threading*(id: EcsIdentity; systemIndexValue: SystemIndex): ECSSysThreading {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "threadingSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSSysThreading(intVal(key[key.len - 1][idx]))
      else:
        default(ECSSysThreading)
    else:
      default(ECSSysThreading)
  {.pop.}

proc setthreading*(id: EcsIdentity; systemIndexValue: SystemIndex;
                   value: ECSSysThreading) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "threadingSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "threadingSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "threadingSystemIndex").add(curList)
  {.pop.}

proc ecsSysCommitInstance*(id: EcsIdentity; systemIndexValue: SystemIndex): ECSSysDefCommit {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "ecsSysCommitInstanceSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        ECSSysDefCommit(intVal(key[key.len - 1][idx]))
      else:
        default(ECSSysDefCommit)
    else:
      default(ECSSysDefCommit)
  {.pop.}

proc setecsSysCommitInstance*(id: EcsIdentity; systemIndexValue: SystemIndex;
                              value: ECSSysDefCommit) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "ecsSysCommitInstanceSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "ecsSysCommitInstanceSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(int(value))
  CacheSeq(id.string & "ecsSysCommitInstanceSystemIndex").add(curList)
  {.pop.}

proc ecsSystemSourceLoc*(id: EcsIdentity; systemIndexValue: SystemIndex): string {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "ecsSystemSourceLocSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        string(strVal(key[key.len - 1][idx]))
      else:
        default(string)
    else:
      default(string)
  {.pop.}

proc setecsSystemSourceLoc*(id: EcsIdentity; systemIndexValue: SystemIndex;
                            value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "ecsSystemSourceLocSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "ecsSystemSourceLocSystemIndex")[keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(string(value))
  CacheSeq(id.string & "ecsSystemSourceLocSystemIndex").add(curList)
  {.pop.}

proc ecsSystemBodySourceLoc*(id: EcsIdentity; systemIndexValue: SystemIndex): string {.
    compileTime.} =
  id.checkId(systemIndexValue)
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let
      key = CacheSeq(id.string & "ecsSystemBodySourceLocSystemIndex")
      keyLen = key.len
      idx = systemIndexValue.int
    if keyLen > 0:
      let entry = key[keyLen - 1].copy
      if entry.len > idx and key[key.len - 1][idx].kind != nnkEmpty:
        string(strVal(key[key.len - 1][idx]))
      else:
        default(string)
    else:
      default(string)
  {.pop.}

proc setecsSystemBodySourceLoc*(id: EcsIdentity; systemIndexValue: SystemIndex;
                                value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  id.checkId(systemIndexValue)
  let
    idx = systemIndexValue.int
    keyLen = CacheSeq(id.string & "ecsSystemBodySourceLocSystemIndex").len
  var curList = newStmtList()
  if keyLen > 0:
    for n in CacheSeq(id.string & "ecsSystemBodySourceLocSystemIndex")[
        keyLen - 1]:
      curList.add(n)
  while curList.len <= idx:
    curList.add newEmptyNode()
  curList[idx] = newLit(string(value))
  CacheSeq(id.string & "ecsSystemBodySourceLocSystemIndex").add(curList)
  {.pop.}

proc ecsMakeEcsSourceLoc*(id: EcsIdentity): string {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsMakeEcsSourceLoc")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].strVal.string
    else:
      default(string)
  {.pop.}

proc setecsMakeEcsSourceLoc*(id: EcsIdentity; value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsMakeEcsSourceLoc").add(newLit(string(value)))
  {.pop.}

proc ecsSystemDeleteLoc*(id: EcsIdentity): string {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsSystemDeleteLoc")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].strVal.string
    else:
      default(string)
  {.pop.}

proc setecsSystemDeleteLoc*(id: EcsIdentity; value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsSystemDeleteLoc").add(newLit(string(value)))
  {.pop.}

proc ecsSystemRemoveLoc*(id: EcsIdentity): string {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsSystemRemoveLoc")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].strVal.string
    else:
      default(string)
  {.pop.}

proc setecsSystemRemoveLoc*(id: EcsIdentity; value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsSystemRemoveLoc").add(newLit(string(value)))
  {.pop.}

{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenEcsComponentsToBeSealed*(id: EcsIdentity): Natural {.compileTime.} =
  CacheSeq(id.string & "EcsComponentsToBeSealedComponentTypeId").len

proc cacheSeqEcsComponentsToBeSealed*(id: EcsIdentity): CacheSeq {.compileTime.} =
  CacheSeq(id.string & "EcsComponentsToBeSealedComponentTypeId")

proc addEcsComponentsToBeSealed*(id: EcsIdentity; value: ComponentTypeId) {.
    compileTime.} =
  CacheSeq(id.string & "EcsComponentsToBeSealedComponentTypeId").add(
      newLit(uint16(value)))

proc ecsComponentsToBeSealed*(id: EcsIdentity): seq[ComponentTypeId] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "EcsComponentsToBeSealedComponentTypeId")
    lenEcsComponentsToBeSealed = keyVal.len
  if lenEcsComponentsToBeSealed > 0:
    result.setLen lenEcsComponentsToBeSealed
    var i: int
    for item in keyVal.items:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

{.pop.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenEcsSealedComponents*(id: EcsIdentity): Natural {.compileTime.} =
  CacheSeq(id.string & "EcsSealedComponentsComponentTypeId").len

proc cacheSeqEcsSealedComponents*(id: EcsIdentity): CacheSeq {.compileTime.} =
  CacheSeq(id.string & "EcsSealedComponentsComponentTypeId")

proc addEcsSealedComponents*(id: EcsIdentity; value: ComponentTypeId) {.
    compileTime.} =
  CacheSeq(id.string & "EcsSealedComponentsComponentTypeId").add(
      newLit(uint16(value)))

proc ecsSealedComponents*(id: EcsIdentity): seq[ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "EcsSealedComponentsComponentTypeId")
    lenEcsSealedComponents = keyVal.len
  if lenEcsSealedComponents > 0:
    result.setLen lenEcsSealedComponents
    var i: int
    for item in keyVal.items:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

{.pop.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenCodeLog*(id: EcsIdentity): Natural {.compileTime.} =
  CacheSeq(id.string & "CodeLogstring").len

proc cacheSeqCodeLog*(id: EcsIdentity): CacheSeq {.compileTime.} =
  CacheSeq(id.string & "CodeLogstring")

proc addCodeLog*(id: EcsIdentity; value: string) {.compileTime.} =
  CacheSeq(id.string & "CodeLogstring").add(newLit(string(value)))

proc codeLog*(id: EcsIdentity): seq[string] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "CodeLogstring")
    lenCodeLog = keyVal.len
  if lenCodeLog > 0:
    result.setLen lenCodeLog
    var i: int
    for item in keyVal.items:
      result[i] = string(item.strVal)
      i.inc

{.pop.}
proc codeLogStart*(id: EcsIdentity): int {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "codeLogStart")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.int
    else:
      default(int)
  {.pop.}

proc setcodeLogStart*(id: EcsIdentity; value: int) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "codeLogStart").add(newLit(int(value)))
  {.pop.}

proc ecsSysIterating*(id: EcsIdentity): int {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsSysIterating")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.int
    else:
      default(int)
  {.pop.}

proc setecsSysIterating*(id: EcsIdentity; value: int) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsSysIterating").add(newLit(int(value)))
  {.pop.}

{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenEcsSysDefined*(id: EcsIdentity): Natural {.compileTime.} =
  CacheSeq(id.string & "EcsSysDefinedSystemIndex").len

proc cacheSeqEcsSysDefined*(id: EcsIdentity): CacheSeq {.compileTime.} =
  CacheSeq(id.string & "EcsSysDefinedSystemIndex")

proc addEcsSysDefined*(id: EcsIdentity; value: SystemIndex) {.compileTime.} =
  CacheSeq(id.string & "EcsSysDefinedSystemIndex").add(newLit(int(value)))

proc ecsSysDefined*(id: EcsIdentity): seq[SystemIndex] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "EcsSysDefinedSystemIndex")
    lenEcsSysDefined = keyVal.len
  if lenEcsSysDefined > 0:
    result.setLen lenEcsSysDefined
    var i: int
    for item in keyVal.items:
      result[i] = SystemIndex(item.intVal)
      i.inc

{.pop.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenSystemOrder*(id: EcsIdentity): Natural {.compileTime.} =
  CacheSeq(id.string & "SystemOrderSystemIndex").len

proc cacheSeqSystemOrder*(id: EcsIdentity): CacheSeq {.compileTime.} =
  CacheSeq(id.string & "SystemOrderSystemIndex")

proc addSystemOrder*(id: EcsIdentity; value: SystemIndex) {.compileTime.} =
  CacheSeq(id.string & "SystemOrderSystemIndex").add(newLit(int(value)))

proc systemOrder*(id: EcsIdentity): seq[SystemIndex] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "SystemOrderSystemIndex")
    lenSystemOrder = keyVal.len
  if lenSystemOrder > 0:
    result.setLen lenSystemOrder
    var i: int
    for item in keyVal.items:
      result[i] = SystemIndex(item.intVal)
      i.inc

{.pop.}
proc inSystem*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "inSystem")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setinSystem*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "inSystem").add(newLit(bool(value)))
  {.pop.}

proc inSystemAll*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "inSystemAll")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setinSystemAll*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "inSystemAll").add(newLit(bool(value)))
  {.pop.}

proc inSystemStream*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "inSystemStream")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setinSystemStream*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "inSystemStream").add(newLit(bool(value)))
  {.pop.}

proc inSystemDeleteRow*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "inSystemDeleteRow")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setinSystemDeleteRow*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "inSystemDeleteRow").add(newLit(bool(value)))
  {.pop.}

proc sysRemoveAffectedThisSystem*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "sysRemoveAffectedThisSystem")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setsysRemoveAffectedThisSystem*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "sysRemoveAffectedThisSystem").add(newLit(bool(value)))
  {.pop.}

proc systemCalledDelete*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "systemCalledDelete")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setsystemCalledDelete*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "systemCalledDelete").add(newLit(bool(value)))
  {.pop.}

proc logInitialised*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "logInitialised")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setlogInitialised*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "logInitialised").add(newLit(bool(value)))
  {.pop.}

proc uncommittedSystems*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "uncommittedSystems")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setuncommittedSystems*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "uncommittedSystems").add(value)
  {.pop.}

proc appenduncommittedSystems*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "uncommittedSystems")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "uncommittedSystems").add(curNode)
  {.pop.}

proc inSystemIndex*(id: EcsIdentity): SystemIndex {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "inSystemIndex")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.SystemIndex
    else:
      default(SystemIndex)
  {.pop.}

proc setinSystemIndex*(id: EcsIdentity; value: SystemIndex) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "inSystemIndex").add(newLit(int(value)))
  {.pop.}

{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenReadsFrom*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "ReadsFromComponentTypeId" & $int(systemIndexValue)).len

proc addReadsFrom*(id: EcsIdentity; systemIndexValue: SystemIndex;
                   value: ComponentTypeId) {.compileTime.} =
  CacheSeq(id.string & "ReadsFromComponentTypeId" & $int(systemIndexValue)).add(
      newLit(uint16(value)))

proc readsFrom*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "ReadsFromComponentTypeId" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

proc lenWritesTo*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "WritesToComponentTypeId" & $int(systemIndexValue)).len

proc addWritesTo*(id: EcsIdentity; systemIndexValue: SystemIndex;
                  value: ComponentTypeId) {.compileTime.} =
  CacheSeq(id.string & "WritesToComponentTypeId" & $int(systemIndexValue)).add(
      newLit(uint16(value)))

proc writesTo*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "WritesToComponentTypeId" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc onInitCodeNode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnInitCodeNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnInitCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnInitCodeNimNode" & $uint16(componentTypeIdValue)).len

proc addOnInitCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                    value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnInitCodeNimNode" & $uint16(componentTypeIdValue)).add(
      value)

proc onInitCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnInitCodeNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onFinalisationCodeNode*(id: EcsIdentity;
                             componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnFinalisationCodeNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnFinalisationCode*(id: EcsIdentity;
                            componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnFinalisationCodeNimNode" &
      $uint16(componentTypeIdValue)).len

proc addOnFinalisationCode*(id: EcsIdentity;
                            componentTypeIdValue: ComponentTypeId;
                            value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnFinalisationCodeNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc onFinalisationCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnFinalisationCodeNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onAddToEntCodeNode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnAddToEntCodeNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnAddToEntCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnAddToEntCodeNimNode" & $uint16(componentTypeIdValue)).len

proc addOnAddToEntCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                        value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnAddToEntCodeNimNode" & $uint16(componentTypeIdValue)).add(
      value)

proc onAddToEntCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnAddToEntCodeNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onRemoveFromEntCodeNode*(id: EcsIdentity;
                              componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnRemoveFromEntCodeNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnRemoveFromEntCode*(id: EcsIdentity;
                             componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnRemoveFromEntCodeNimNode" &
      $uint16(componentTypeIdValue)).len

proc addOnRemoveFromEntCode*(id: EcsIdentity;
                             componentTypeIdValue: ComponentTypeId;
                             value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnRemoveFromEntCodeNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc onRemoveFromEntCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnRemoveFromEntCodeNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onAddCallbackNode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnAddCallbackNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnAddCallback*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnAddCallbackNimNode" & $uint16(componentTypeIdValue)).len

proc addOnAddCallback*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId;
                       value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnAddCallbackNimNode" & $uint16(componentTypeIdValue)).add(
      value)

proc onAddCallback*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnAddCallbackNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onRemoveCallbackNode*(id: EcsIdentity;
                           componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnRemoveCallbackNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnRemoveCallback*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnRemoveCallbackNimNode" &
      $uint16(componentTypeIdValue)).len

proc addOnRemoveCallback*(id: EcsIdentity;
                          componentTypeIdValue: ComponentTypeId; value: NimNode) {.
    compileTime.} =
  CacheSeq(id.string & "OnRemoveCallbackNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc onRemoveCallback*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnRemoveCallbackNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onAddAnySystemCodeNode*(id: EcsIdentity;
                             componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnAddAnySystemCodeNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnAddAnySystemCode*(id: EcsIdentity;
                            componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnAddAnySystemCodeNimNode" &
      $uint16(componentTypeIdValue)).len

proc addOnAddAnySystemCode*(id: EcsIdentity;
                            componentTypeIdValue: ComponentTypeId;
                            value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnAddAnySystemCodeNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc onAddAnySystemCode*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnAddAnySystemCodeNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onRemoveAnySystemCodeNode*(id: EcsIdentity;
                                componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnRemoveAnySystemCodeNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnRemoveAnySystemCode*(id: EcsIdentity;
                               componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnRemoveAnySystemCodeNimNode" &
      $uint16(componentTypeIdValue)).len

proc addOnRemoveAnySystemCode*(id: EcsIdentity;
                               componentTypeIdValue: ComponentTypeId;
                               value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnRemoveAnySystemCodeNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc onRemoveAnySystemCode*(id: EcsIdentity;
                            componentTypeIdValue: ComponentTypeId): seq[NimNode] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnRemoveAnySystemCodeNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onInterceptUpdateNode*(id: EcsIdentity;
                            componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnInterceptUpdateNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnInterceptUpdate*(id: EcsIdentity;
                           componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnInterceptUpdateNimNode" &
      $uint16(componentTypeIdValue)).len

proc addOnInterceptUpdate*(id: EcsIdentity;
                           componentTypeIdValue: ComponentTypeId; value: NimNode) {.
    compileTime.} =
  CacheSeq(id.string & "OnInterceptUpdateNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc onInterceptUpdate*(id: EcsIdentity; componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnInterceptUpdateNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onAddCallbackForwardDeclNode*(id: EcsIdentity;
                                   componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnAddCallbackForwardDeclNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnAddCallbackForwardDecl*(id: EcsIdentity;
                                  componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnAddCallbackForwardDeclNimNode" &
      $uint16(componentTypeIdValue)).len

proc addOnAddCallbackForwardDecl*(id: EcsIdentity;
                                  componentTypeIdValue: ComponentTypeId;
                                  value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnAddCallbackForwardDeclNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc onAddCallbackForwardDecl*(id: EcsIdentity;
                               componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnAddCallbackForwardDeclNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onRemoveCallbackForwardDeclNode*(id: EcsIdentity;
                                      componentTypeIdValue: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnRemoveCallbackForwardDeclNimNode" &
      $uint16(componentTypeIdValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnRemoveCallbackForwardDecl*(id: EcsIdentity;
                                     componentTypeIdValue: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnRemoveCallbackForwardDeclNimNode" &
      $uint16(componentTypeIdValue)).len

proc addOnRemoveCallbackForwardDecl*(id: EcsIdentity;
                                     componentTypeIdValue: ComponentTypeId;
                                     value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnRemoveCallbackForwardDeclNimNode" &
      $uint16(componentTypeIdValue)).add(value)

proc onRemoveCallbackForwardDecl*(id: EcsIdentity;
                                  componentTypeIdValue: ComponentTypeId): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnRemoveCallbackForwardDeclNimNode" &
        $uint16(componentTypeIdValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc onAddedNode*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnAddedNimNode" & $int(systemIndexValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnAdded*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnAddedNimNode" & $int(systemIndexValue)).len

proc addOnAdded*(id: EcsIdentity; systemIndexValue: SystemIndex; value: NimNode) {.
    compileTime.} =
  CacheSeq(id.string & "OnAddedNimNode" & $int(systemIndexValue)).add(value)

proc onAdded*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[NimNode] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnAddedNimNode" & $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onRemovedNode*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnRemovedNimNode" & $int(systemIndexValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnRemoved*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnRemovedNimNode" & $int(systemIndexValue)).len

proc addOnRemoved*(id: EcsIdentity; systemIndexValue: SystemIndex;
                   value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnRemovedNimNode" & $int(systemIndexValue)).add(value)

proc onRemoved*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[NimNode] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnRemovedNimNode" & $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onAddedCallbackNode*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnAddedCallbackNimNode" &
      $int(systemIndexValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnAddedCallback*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnAddedCallbackNimNode" & $int(systemIndexValue)).len

proc addOnAddedCallback*(id: EcsIdentity; systemIndexValue: SystemIndex;
                         value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnAddedCallbackNimNode" & $int(systemIndexValue)).add(
      value)

proc onAddedCallback*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnAddedCallbackNimNode" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onRemovedCallbackNode*(id: EcsIdentity; systemIndexValue: SystemIndex): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnRemovedCallbackNimNode" &
      $int(systemIndexValue))
  for event in keyVal.items:
    result.add(event)

proc lenOnRemovedCallback*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnRemovedCallbackNimNode" & $int(systemIndexValue)).len

proc addOnRemovedCallback*(id: EcsIdentity; systemIndexValue: SystemIndex;
                           value: NimNode) {.compileTime.} =
  CacheSeq(id.string & "OnRemovedCallbackNimNode" & $int(systemIndexValue)).add(
      value)

proc onRemovedCallback*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    NimNode] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnRemovedCallbackNimNode" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
proc onAddToCodeNode*(id: EcsIdentity; systemIndex1Value: SystemIndex;
                      componentTypeId2Value: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnAddToCodeNimNode" &
      $int(systemIndex1Value) &
      "_" &
      $uint16(componentTypeId2Value))
  for event in keyVal.items:
    result.add(event)

proc lenOnAddToCode*(id: EcsIdentity; systemIndex1Value: SystemIndex;
                     componentTypeId2Value: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnAddToCodeNimNode" & $int(systemIndex1Value) & "_" &
      $uint16(componentTypeId2Value)).len

proc addOnAddToCode*(id: EcsIdentity; systemIndex1Value: SystemIndex;
                     componentTypeId2Value: ComponentTypeId; value: NimNode) {.
    compileTime.} =
  CacheSeq(id.string & "OnAddToCodeNimNode" & $int(systemIndex1Value) & "_" &
      $uint16(componentTypeId2Value)).add(value)

proc onAddToCode*(id: EcsIdentity; systemIndex1Value: SystemIndex;
                  componentTypeId2Value: ComponentTypeId): seq[NimNode] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnAddToCodeNimNode" &
        $int(systemIndex1Value) &
        "_" &
        $uint16(componentTypeId2Value))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

proc onRemoveFromCodeNode*(id: EcsIdentity; systemIndex1Value: SystemIndex;
                           componentTypeId2Value: ComponentTypeId): NimNode {.
    compileTime.} =
  result = newStmtList()
  let keyVal = CacheSeq(id.string & "OnRemoveFromCodeNimNode" &
      $int(systemIndex1Value) &
      "_" &
      $uint16(componentTypeId2Value))
  for event in keyVal.items:
    result.add(event)

proc lenOnRemoveFromCode*(id: EcsIdentity; systemIndex1Value: SystemIndex;
                          componentTypeId2Value: ComponentTypeId): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnRemoveFromCodeNimNode" & $int(systemIndex1Value) &
      "_" &
      $uint16(componentTypeId2Value)).len

proc addOnRemoveFromCode*(id: EcsIdentity; systemIndex1Value: SystemIndex;
                          componentTypeId2Value: ComponentTypeId; value: NimNode) {.
    compileTime.} =
  CacheSeq(id.string & "OnRemoveFromCodeNimNode" & $int(systemIndex1Value) &
      "_" &
      $uint16(componentTypeId2Value)).add(value)

proc onRemoveFromCode*(id: EcsIdentity; systemIndex1Value: SystemIndex;
                       componentTypeId2Value: ComponentTypeId): seq[NimNode] {.
    compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnRemoveFromCodeNimNode" &
        $int(systemIndex1Value) &
        "_" &
        $uint16(componentTypeId2Value))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = item
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: off.}
proc lenOnAddToSystemComp*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnAddToSystemCompComponentTypeId" &
      $int(systemIndexValue)).len

proc addOnAddToSystemComp*(id: EcsIdentity; systemIndexValue: SystemIndex;
                           value: ComponentTypeId) {.compileTime.} =
  CacheSeq(id.string & "OnAddToSystemCompComponentTypeId" &
      $int(systemIndexValue)).add(newLit(uint16(value)))

proc onAddToSystemComp*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnAddToSystemCompComponentTypeId" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

proc lenOnRemoveFromSystemComp*(id: EcsIdentity; systemIndexValue: SystemIndex): Natural {.
    compileTime.} =
  CacheSeq(id.string & "OnRemoveFromSystemCompComponentTypeId" &
      $int(systemIndexValue)).len

proc addOnRemoveFromSystemComp*(id: EcsIdentity; systemIndexValue: SystemIndex;
                                value: ComponentTypeId) {.compileTime.} =
  CacheSeq(id.string & "OnRemoveFromSystemCompComponentTypeId" &
      $int(systemIndexValue)).add(newLit(uint16(value)))

proc onRemoveFromSystemComp*(id: EcsIdentity; systemIndexValue: SystemIndex): seq[
    ComponentTypeId] {.compileTime.} =
  let
    keyVal = CacheSeq(id.string & "OnRemoveFromSystemCompComponentTypeId" &
        $int(systemIndexValue))
    listLen = keyVal.len
  if listLen > 0:
    result.setLen listLen
    var i: int
    for item in keyVal:
      result[i] = ComponentTypeId(item.intVal)
      i.inc

{.push, hint[ConvFromXtoItselfNotNeeded]: on.}
proc private*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "private")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setprivate*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "private").add(newLit(bool(value)))
  {.pop.}

proc ecsDefineSystemsAsGroup*(id: EcsIdentity): string {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsDefineSystemsAsGroup")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].strVal.string
    else:
      default(string)
  {.pop.}

proc setecsDefineSystemsAsGroup*(id: EcsIdentity; value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsDefineSystemsAsGroup").add(newLit(string(value)))
  {.pop.}

proc ecsCodeLogFilename*(id: EcsIdentity): string {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsCodeLogFilename")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].strVal.string
    else:
      default(string)
  {.pop.}

proc setecsCodeLogFilename*(id: EcsIdentity; value: string) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsCodeLogFilename").add(newLit(string(value)))
  {.pop.}

proc ecsCurrentOperation*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsCurrentOperation")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setecsCurrentOperation*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsCurrentOperation").add(value)
  {.pop.}

proc appendecsCurrentOperation*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "ecsCurrentOperation")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "ecsCurrentOperation").add(curNode)
  {.pop.}

proc onEntityStateChange*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "onEntityStateChange")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setonEntityStateChange*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "onEntityStateChange").add(value)
  {.pop.}

proc appendonEntityStateChange*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "onEntityStateChange")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "onEntityStateChange").add(curNode)
  {.pop.}

proc onEcsBuildingCode*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "onEcsBuildingCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setonEcsBuildingCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "onEcsBuildingCode").add(value)
  {.pop.}

proc appendonEcsBuildingCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "onEcsBuildingCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "onEcsBuildingCode").add(curNode)
  {.pop.}

proc onEcsBuiltCode*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "onEcsBuiltCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setonEcsBuiltCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "onEcsBuiltCode").add(value)
  {.pop.}

proc appendonEcsBuiltCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "onEcsBuiltCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "onEcsBuiltCode").add(curNode)
  {.pop.}

proc onEcsCommitAllCode*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "onEcsCommitAllCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setonEcsCommitAllCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "onEcsCommitAllCode").add(value)
  {.pop.}

proc appendonEcsCommitAllCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "onEcsCommitAllCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "onEcsCommitAllCode").add(curNode)
  {.pop.}

proc onEcsNextCommitCode*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "onEcsNextCommitCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setonEcsNextCommitCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "onEcsNextCommitCode").add(value)
  {.pop.}

proc appendonEcsNextCommitCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "onEcsNextCommitCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "onEcsNextCommitCode").add(curNode)
  {.pop.}

proc onEcsNextGroupCommitCode*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "onEcsNextGroupCommitCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setonEcsNextGroupCommitCode*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "onEcsNextGroupCommitCode").add(value)
  {.pop.}

proc appendonEcsNextGroupCommitCode*(id: EcsIdentity; value: NimNode) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "onEcsNextGroupCommitCode")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "onEcsNextGroupCommitCode").add(curNode)
  {.pop.}

proc ecsMakeEcsImports*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsMakeEcsImports")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setecsMakeEcsImports*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsMakeEcsImports").add(value)
  {.pop.}

proc appendecsMakeEcsImports*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "ecsMakeEcsImports")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "ecsMakeEcsImports").add(curNode)
  {.pop.}

proc ecsCommitImports*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsCommitImports")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setecsCommitImports*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsCommitImports").add(value)
  {.pop.}

proc appendecsCommitImports*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "ecsCommitImports")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "ecsCommitImports").add(curNode)
  {.pop.}

proc ecsMakeEcsImportFrom*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsMakeEcsImportFrom")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setecsMakeEcsImportFrom*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsMakeEcsImportFrom").add(value)
  {.pop.}

proc appendecsMakeEcsImportFrom*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "ecsMakeEcsImportFrom")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "ecsMakeEcsImportFrom").add(curNode)
  {.pop.}

proc ecsCommitImportFrom*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsCommitImportFrom")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setecsCommitImportFrom*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsCommitImportFrom").add(value)
  {.pop.}

proc appendecsCommitImportFrom*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "ecsCommitImportFrom")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "ecsCommitImportFrom").add(curNode)
  {.pop.}

proc maxEntities*(id: EcsIdentity): Natural {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "maxEntities")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.Natural
    else:
      default(Natural)
  {.pop.}

proc setmaxEntities*(id: EcsIdentity; value: Natural) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "maxEntities").add(newLit(Natural(value)))
  {.pop.}

proc componentStorageFormat*(id: EcsIdentity): ECSCompStorage {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "componentStorageFormat")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSCompStorage
    else:
      default(ECSCompStorage)
  {.pop.}

proc setcomponentStorageFormat*(id: EcsIdentity; value: ECSCompStorage) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "componentStorageFormat").add(newLit(int(value)))
  {.pop.}

proc entityStorageFormat*(id: EcsIdentity): ECSEntityItemStorage {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "entityStorageFormat")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSEntityItemStorage
    else:
      default(ECSEntityItemStorage)
  {.pop.}

proc setentityStorageFormat*(id: EcsIdentity; value: ECSEntityItemStorage) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "entityStorageFormat").add(newLit(int(value)))
  {.pop.}

proc maxComponentsPerEnt*(id: EcsIdentity): Natural {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "maxComponentsPerEnt")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.Natural
    else:
      default(Natural)
  {.pop.}

proc setmaxComponentsPerEnt*(id: EcsIdentity; value: Natural) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "maxComponentsPerEnt").add(newLit(Natural(value)))
  {.pop.}

proc recyclerFormat*(id: EcsIdentity): ECSRecyclerFormat {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "recyclerFormat")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSRecyclerFormat
    else:
      default(ECSRecyclerFormat)
  {.pop.}

proc setrecyclerFormat*(id: EcsIdentity; value: ECSRecyclerFormat) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "recyclerFormat").add(newLit(int(value)))
  {.pop.}

proc useSet*(id: EcsIdentity): bool {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "useSet")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].boolVal.bool
    else:
      default(bool)
  {.pop.}

proc setuseSet*(id: EcsIdentity; value: bool) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "useSet").add(newLit(bool(value)))
  {.pop.}

proc strDefault*(id: EcsIdentity): ECSStrDefault {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "strDefault")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSStrDefault
    else:
      default(ECSStrDefault)
  {.pop.}

proc setstrDefault*(id: EcsIdentity; value: ECSStrDefault) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "strDefault").add(newLit(int(value)))
  {.pop.}

proc errDuplicates*(id: EcsIdentity): ECSErrorResponse {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "errDuplicates")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSErrorResponse
    else:
      default(ECSErrorResponse)
  {.pop.}

proc seterrDuplicates*(id: EcsIdentity; value: ECSErrorResponse) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "errDuplicates").add(newLit(int(value)))
  {.pop.}

proc errEntityOverflow*(id: EcsIdentity): ECSErrorResponse {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "errEntityOverflow")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSErrorResponse
    else:
      default(ECSErrorResponse)
  {.pop.}

proc seterrEntityOverflow*(id: EcsIdentity; value: ECSErrorResponse) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "errEntityOverflow").add(newLit(int(value)))
  {.pop.}

proc errCaseComponent*(id: EcsIdentity): ECSErrorResponse {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "errCaseComponent")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSErrorResponse
    else:
      default(ECSErrorResponse)
  {.pop.}

proc seterrCaseComponent*(id: EcsIdentity; value: ECSErrorResponse) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "errCaseComponent").add(newLit(int(value)))
  {.pop.}

proc errCaseSystem*(id: EcsIdentity): ECSErrorResponse {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "errCaseSystem")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSErrorResponse
    else:
      default(ECSErrorResponse)
  {.pop.}

proc seterrCaseSystem*(id: EcsIdentity; value: ECSErrorResponse) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "errCaseSystem").add(newLit(int(value)))
  {.pop.}

proc errIncompleteOwned*(id: EcsIdentity): ECSErrorResponse {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "errIncompleteOwned")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].intVal.ECSErrorResponse
    else:
      default(ECSErrorResponse)
  {.pop.}

proc seterrIncompleteOwned*(id: EcsIdentity; value: ECSErrorResponse) {.
    compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "errIncompleteOwned").add(newLit(int(value)))
  {.pop.}

proc setentityOptions*(id: EcsIdentity; value: ECSEntityOptions) =
  id.setmaxEntities(value.maxEntities)
  id.setcomponentStorageFormat(value.componentStorageFormat)
  id.setentityStorageFormat(value.entityStorageFormat)
  id.setmaxComponentsPerEnt(value.maxComponentsPerEnt)
  id.setrecyclerFormat(value.recyclerFormat)
  id.setuseSet(value.useSet)
  id.setstrDefault(value.strDefault)
  id.seterrDuplicates(value.errors.errDuplicates)
  id.seterrEntityOverflow(value.errors.errEntityOverflow)
  id.seterrCaseComponent(value.errors.errCaseComponent)
  id.seterrCaseSystem(value.errors.errCaseSystem)
  id.seterrIncompleteOwned(value.errors.errIncompleteOwned)

proc entityOptions*(id: EcsIdentity): ECSEntityOptions =
  result.maxEntities = id.maxEntities
  result.componentStorageFormat = id.componentStorageFormat
  result.entityStorageFormat = id.entityStorageFormat
  result.maxComponentsPerEnt = id.maxComponentsPerEnt
  result.recyclerFormat = id.recyclerFormat
  result.useSet = id.useSet
  result.strDefault = id.strDefault
  result.errors.errDuplicates = id.errDuplicates
  result.errors.errEntityOverflow = id.errEntityOverflow
  result.errors.errCaseComponent = id.errCaseComponent
  result.errors.errCaseSystem = id.errCaseSystem
  result.errors.errIncompleteOwned = id.errIncompleteOwned

proc ecsEventEnv*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsEventEnv")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setecsEventEnv*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsEventEnv").add(value)
  {.pop.}

proc appendecsEventEnv*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "ecsEventEnv")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "ecsEventEnv").add(curNode)
  {.pop.}

proc ecsEventMutations*(id: EcsIdentity): NimNode {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  result = block:
    let keyVal = CacheSeq(id.string & "ecsEventMutations")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  {.pop.}

proc setecsEventMutations*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  CacheSeq(id.string & "ecsEventMutations").add(value)
  {.pop.}

proc appendecsEventMutations*(id: EcsIdentity; value: NimNode) {.compileTime.} =
  {.push, hint[ConvFromXtoItselfNotNeeded]: off.}
  var curNode = block:
    let keyVal = CacheSeq(id.string & "ecsEventMutations")
    if keyVal.len > 0:
      keyVal[keyVal.len - 1].NimNode
    else:
      default(NimNode)
  if curNode.isNil:
    curNode = newStmtList()
  curNode.add(value)
  CacheSeq(id.string & "ecsEventMutations").add(curNode)
  {.pop.}
