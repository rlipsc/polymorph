# -----------------------
# Event mutation tracking
# -----------------------


import macros, ../sharedtypes, ecsstatedb, debugging
from strutils import repeat


type
  ParamKind* = enum pkNone, pkCompCT, pkComp, pkSys, pkSysComp

func getParamKind*(eventKind: EventKind): ParamKind =
  case eventKind
    of ekNoEvent .. ekDeleteEnt:
      pkNone
    of ekNewEntityWith .. ekRemoveComponents:
      pkCompCT
    of ekInit .. ekDeleteComp:
      pkComp
    of ekSystemAddAny .. ekSystemRemoveAny, ekRowAdded .. ekRowRemovedCb:
      pkSys
    of ekCompAddTo, ekCompRemoveFrom:
      pkSysComp


func getEventDir*(eventKind: EventKind): EventKind =
  ## Return the direction of an event, if any.

  case eventKind
    
    of ekAddComponents, ekAdd, ekAddCB, ekInit, ekSystemAddAny, ekCompAddTo, ekRowAdded, ekRowAddedCB:
      ekAddComponents

    of ekRemoveComponents, ekRemove, ekRemoveCB, ekDeleteComp, ekSystemRemoveAny, ekCompRemoveFrom, ekRowRemoved, ekRowRemovedCB:
      ekRemoveComponents
    
    else:
      ekNoEvent


type
  # EventState represents the current static event expansion or
  # a mutation to an entity.
  EventState* = object
    event*: EventKind
    indexes*: seq[int]

  ComponentMutation* = object
    mutation*: EventState
    trace*: seq[EventState]


proc initEventState(event: EventKind, indexes: openarray[int]): EventState =
  result.event = event
  result.indexes.setLen indexes.len
  for i, idx in indexes:
    result.indexes[i] = idx


proc toNode(eventState: EventState): NimNode =
  result = nnkBracket.newTree
  result.add newLit eventState.event.int

  for idx in eventState.indexes:
    result.add newLit idx


proc captureMutation(id: EcsIdentity, mutation: EventState): NimNode =
  ## Create a new ComponentMutation with the current event history.
  result = nnkBracket.newTree
  result.add mutation.toNode
  result.add nnkBracket.newTree()
  for node in id.ecsEventEnv:
    result[1].add node.copy


proc toEventState*(node: NimNode): EventState =
  ## Translate a NimNode to an EventState.
  const headerLen = 1
  node.expectKind nnkBracket
  node.expectMinLen headerLen
  result.event = EventKind(node[0].intVal)

  result.indexes = newSeqOfCap[int](node.len - headerLen)
  for n in node[headerLen .. ^1]:
    result.indexes.add n.intVal.int


proc toMutation*(node: NimNode): ComponentMutation =
  ## Translate a NimNode to a ComponentMutation.
  node.expectKind nnkBracket
  node.expectMinLen 2

  result.mutation = node[0].toEventState

  result.trace.setLen node[1].len
  for i in 0 ..< node[1].len:
    result.trace[i] = node[1][i].toEventState


proc contains*(find, test: openarray[int]): bool =
  ## `in` operator for int lists.
  ## Returns `true` when all items in `find` are in `test`.
  for v in test:
    if v notin find:
      return false
  return true


proc hasAny*(find, test: openarray[int]): bool =
  ## Returns `true` when any items in `find` are in `test`.
  for v in find:
    if v in test:
      return true
  return false


proc mutationOccurred*(id: EcsIdentity, mutations: set[EventKind], indexes: openarray[int]): bool =
  ## Returns true when any of the mutations or indexes occur together.
  for n in id.ecsEventMutations:
    n.expectLen 2
    let es = n[0].toEventState  # Read mutation type (don't need trace).
    if es.event in mutations and indexes.hasAny(es.indexes):
      return true


proc addOccurred*(id: EcsIdentity, indexes: openarray[int]): bool =
  id.mutationOccurred({ekAddComponents}, indexes)


proc removeOccurred*(id: EcsIdentity, indexes: openarray[int]): bool =
  id.mutationOccurred({ekRemoveComponents}, indexes)


proc eventOccurred*(id: EcsIdentity, events: set[EventKind], indexes: openarray[int]): bool =
  for n in id.ecsEventEnv:
    let es = n.toEventState
    if es.event in events and indexes in es.indexes:
      return true
  false


proc toStr(es: EventState, id: EcsIdentity, indent = 0): string =
  result = $es.event & " "

  let
    pk = es.event.getParamKind
  
  func quote(str: string): string = "\"" & str & "\""

  case pk
    of pkNone:
      discard

    of pkCompCT, pkComp:
      if es.indexes.len > 0:
        result &= id.typeName(es.indexes[0].ComponentTypeId)
        
        for v in es.indexes[1 .. ^1]:
          result &= ", " & id.typeName(v.ComponentTypeId)
    
    of pkSys:
      if es.indexes.len > 0:
        result &= id.getSystemName(es.indexes[0].SystemIndex).quote
        
        for v in es.indexes[1 ..^ 1]:
          result &= ", " & id.getSystemName(v.SystemIndex).quote

    of pkSysComp:
      if es.indexes.len > 0:
        result &= id.getSystemName(es.indexes[0].SystemIndex).quote
        
        if es.indexes.len > 1:
          result &= " and "
          result &= id.typeName(es.indexes[1].ComponentTypeId)
          for v in es.indexes[2 ..^ 1]:
            result &= ", " & id.typeName(v.ComponentTypeId)


proc toStr(m: ComponentMutation, id: EcsIdentity): string =
  result = m.mutation.toStr(id) & " [ "
  
  result &= m.trace[0].toStr(id)
  for i in 1 ..< m.trace.len:
    result &= " -> " & m.trace[i].toStr(id)
  
  result &= " ]"


proc toStr(v: NimNode, id: EcsIdentity, op: proc, title: string, indent = 0): string =
  ## Process nodes in `v` with `op` and pass the resulting type `toStr` to output a string.
  if v.len > 0:
    let
      ind = "  "
      indentStr = ind.repeat(indent)

    if title.len > 0:
      result = indentStr & title & "\n"
    
    result &= indentStr & ind & op(v[0]).toStr(id)

    for i in 1 ..< v.len:
      result &= "\n" & indentStr & ind & op(v[i]).toStr(id)


proc eventsStr*(id: EcsIdentity, indent = 0): string =
  result = id.ecsEventEnv.toStr(id, toEventState, "Event:", indent)


proc mutationsStr*(id: EcsIdentity, indent = 0): string =
  id.ecsEventMutations.toStr(id, toMutation, "Mutations:", indent)


proc eventMutationsStr*(id: EcsIdentity, indent = 0): string =
  result = id.eventsStr(indent)
  if result.len > 0:
    result &= "\n\n"
  result &= id.mutationsStr(indent)
  if result.len > 0:
    result &= "\n"


proc enterEvent(id: EcsIdentity, event: EventKind, indexes: openarray[int]) =
  ## Record entering this event in the static environment.
  var
    curEvents = id.ecsEventEnv
    evNode = initEventState(event, indexes).toNode

  if curEvents.len == 0 and curEvents.kind != nnkBracket:
    curEvents = nnkBracket.newTree()
  curEvents.add evNode

  id.set_ecsEventEnv curEvents


proc exitEvent(id: EcsIdentity, event: EventKind, indexes: seq[int]) =
  ## Record leaving this event in the static environment.
  var curEvents = id.ecsEventEnv
  let idxSeq = indexes

  if curEvents.len > 0:
    block findEntry:
      # Find and remove the last matching event.
      for i in countDown(curEvents.len - 1, 0):
        let es = curEvents[i].toEventState
        if es.event == event and idxSeq == es.indexes:
          curEvents.del i
          break findEntry

      error "Internal error: couldn't find record of " &
        toStr(initEventState(event, indexes), id) & "when exiting event:\nHistory:\n" &
        id.eventsStr

    id.set_ecsEventEnv curEvents

  if id.ecsEventEnv.len == 0:
    # Reset event environment.
    when defined(ecsLogDetails):
      if id.ecsEventMutations.len > 0:
        id.debugMessage $event & "\n" & id.mutationsStr(id.ecsCurrentOperation.len + 2)
    
    id.set_ecsEventMutations nnkBracket.newTree()


proc recordMutation*(id: EcsIdentity, mutation: EventKind, indexes: openarray[int]) =
  ## Store consecutive embedded mutations.
  if id.ecsEventEnv.len > 0:
    let
      mutState = initEventState(mutation, indexes)
      compMutation = id.captureMutation(mutState)

    var mutSoFar = id.ecsEventMutations
    
    if mutSoFar.len == 0 and mutSoFar.kind != nnkBracket:
      mutSoFar = nnkBracket.newTree()
    
    mutSoFar.add compMutation
    id.set_ecsEventMutations mutSoFar


proc trackMutation*(node: var NimNode, id: EcsIdentity, mutationType: EventKind, indexes: openarray[int], announce = true) =
  ## Wrap user callback code with static tracking to handle embedded state changes.
  
  var br = nnkBracket.newTree()
  for idx in indexes:
    br.add newLit(idx)
  
  let
    ecsId = quote do: EcsIdentity(`id`)
    mutParams = prefix(br, "@")

    startOp =
      if announce:
        let strOp = initEventState(mutationType, indexes).toStr(id)
        quote do:
          startOperation(`ecsId`, "Checking event " & `strOp`)
      else:
        newStmtList()

    endOp =
      if announce:
        quote do:
          endOperation(`ecsId`)
      else:
        newStmtList()
    
  node.insert(0, quote do:
    static:
      `startOp`
      enterEvent(`ecsId`, EventKind(`mutationType`), `mutParams`)
  )

  node.add(quote do:
    static:
      exitEvent(`ecsId`, EventKind(`mutationType`), `mutParams`)
      `endOp`
  )


proc trackMutation*(node: var NimNode, id: EcsIdentity, mutationType: EventKind, indexes: seq[ComponentTypeId] or seq[SystemIndex], announce = true) =
  trackMutation(node, id, mutationType, indexes.toIntList, announce)

