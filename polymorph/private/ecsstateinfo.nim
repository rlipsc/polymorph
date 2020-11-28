import ../sharedtypes, macros, tables

#[
  This module defines types that store the compile-time collected type info of components and systems.
]#

type
  
  ## Details for generating components.
  ComponentInfo* = object
    id*: ComponentTypeId
    typeName*: string
    instanceType*: string
    refType*: string
    refInitPrefix*: string

    isOwned*: bool
    systemOwner*: SystemIndex
    fields*: seq[tuple[fieldNode, typeNode: NimNode]]
    systems*: seq[SystemIndex]

    onInitCode*, onFinalisationCode*,
      onAddToEntCode*, onRemoveFromEntCode*,
      onAddCallback*, onRemoveCallback*,
      onAddAnySystemCode*, onRemoveAnySystemCode*,
      onInterceptValueInitCode*,

      onAddCallbackForwardDecl*,
      onRemoveCallbackForwardDecl*: NimNode
    
    options*: ECSCompOptions
    
  ComponentData = seq[ComponentInfo]

  ## Details for generating systems.
  SystemInfo* = object
    id*: SystemIndex
    systemName*: string
    # Adding to system events
    onAddToCode*, onRemoveFromCode*: Table[ComponentTypeId, NimNode]
    onAdded*, onRemoved*: NimNode

    requirements*: seq[ComponentTypeId]
    ownedComponents*: seq[ComponentTypeId]
    #
    instantiation*: NimNode
    definition*: NimNode
    
    options*: ECSSysOptions

  SystemData* = seq[SystemInfo]

proc initComponentInfoEvents*(info: var ComponentInfo) =
  info.systemOwner = InvalidSystemIndex
  info.onInitCode = newStmtList()
  info.onFinalisationCode = newStmtList()
  info.onAddToEntCode = newStmtList()
  info.onRemoveFromEntCode = newStmtList()
  info.onAddCallback = newStmtList()
  info.onRemoveCallback = newStmtList()
  info.onAddCallbackForwardDecl = newStmtList()
  info.onRemoveCallbackForwardDecl = newStmtList()
  info.onInterceptValueInitCode = newStmtList()
  info.onAddAnySystemCode = newStmtList()
  info.onRemoveAnySystemCode = newStmtList()

# TODO: Aim to transition component and system info to be handed down
# rather than globals defined here.
# The goal is to be able to pass around ECS definitions as meta-types
# for code generation, and ultimately, read in and generate ECS
# architecture from files.
var
  # All type data.
  typeInfo* {.compileTime.} = newSeq[ComponentInfo](1)
  # All system deta.
  systemInfo* {.compileTime.} = newSeq[SystemInfo]()

# ComponentInfo utils.

proc contains*(cd: ComponentData, typeName: string): bool =
  for item in cd:
    if item.typeName == typeName: return true

proc contains*(cd: ComponentData, typeId: ComponentTypeId): bool =
  for item in cd:
    if item.id == typeId: return true

proc info*(cd: ComponentData, id: ComponentTypeId): ComponentInfo {.compileTime.} =
  assert id.int in 0 ..< cd.len, "Invalid type id: " & $id.int
  cd[id.int]

proc typeName*(cd: ComponentData, id: ComponentTypeId): string {.compileTime.} =
  cd[id.int].typeName

proc systemOwner*(cd: ComponentData, id: ComponentTypeId): SystemIndex {.compileTime.} =
  cd[id.int].systemOwner

proc allTypeNames*(cd: ComponentData): string =
  if cd.len > 1:
    result &= cd[1].typeName
    for i in 2 ..< cd.len:
      result &= ", " & cd[i].typeName

proc isOwned*(cd: ComponentData, typeId: ComponentTypeId): bool =
  cd[typeId.int].systemOwner != InvalidSystemIndex

# SystemInfo utils.

proc contains*(sd: SystemData, systemName: string): bool =
  for item in sd:
    if item.systemName == systemName: return true

proc allSystemNames*(sd: SystemData): string =
  if sd.len > 0:
    result &= sd[0].systemName    
    for i in 1 ..< sd.len:
      result &= ", " & sd[i].systemName
