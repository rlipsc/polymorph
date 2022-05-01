import macros, macrocache, ../sharedtypes

type EcsIdentity* = CacheSeq

#----------------------
# Registered components
#----------------------

proc components*(id: EcsIdentity): CacheSeq {.compileTime.} =
  CacheSeq(id.string & "Components")

proc checkId*(id: EcsIdentity, compId: ComponentTypeId) {.compileTime.} =
  let
    key = id.components
    keyLen = key.len
  if compId.int notin 1 ..< keyLen:
    # let compRangeStr =
    #   if keyLen > 0: "[1 ..< " & $keyLen & "]"
    #   else: "<no components>"
    # error "Invalid ComponentTypeId passed to checkId: " & $compId.int &
    #   ", components ids registered " & compRangeStr
    error "Invalid ComponentTypeId passed to checkId"

#-------------------
# Registered systems
#-------------------

proc systemsKey*(id: EcsIdentity): CacheSeq {.compileTime.} =
  ## Get key for the list of system.
  CacheSeq(id.string & "System")

proc checkId*(id: EcsIdentity, sysIdx: SystemIndex) {.compileTime.} =
  let
    key = id.systemsKey
    keyLen = key.len
  if sysIdx.int notin 1 ..< keyLen:
    # error "Invalid SystemIndex passed to checkId: \"" & $sysIdx.int &
    #   "\". System count = " & $keyLen
    error "Invalid SystemIndex passed to checkId"

