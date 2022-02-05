import polymorph

template defineEventSpectrum*(cOpts: ECSCompOptions, sOpts: ECSSysOptions, eOpts: ECSEntityOptions) {.dirty.} =

  registerComponents cOpts:
    type
      EventTest1 = object
      EventTest2 = object

  defineSystem "simpleEvents1", [EventTest1], sOpts
  defineSystem "simpleEvents2", [EventTest1], sOpts
  defineSystem "simpleEvents3", [EventTest1], sOpts
  defineSystem "simpleEvents4", [EventTest1, own EventTest2], sOpts

  # ------------

  type
    Events = object
      evName: string
      c: set[EventKind]
      cs: Table[string, set[EventKind]]

  proc addEvents(c: typedesc, addedSystems: seq[string], extra: set[EventKind]): Events =
    ## Returns the expected `Event` object for the component `c` being
    ## added to an entity and `systems`.

    result = Events(
      evName: $c,
      c: {ekInit, ekAdd, ekAddCb} + extra
    )
    
    for s in addedSystems:
      result.cs[s] = {ekSystemAddAny, ekCompAddTo, ekRowAdded, ekRowAddedCb}

  proc removeEvents(c: typedesc, removedSystems: seq[string], extra: set[EventKind]): Events =
    ## Returns the expected `Event` object for the component `c` being
    ## removed from an entity and `systems`.

    result = Events(
      evName: $c,
        c: {ekRemove, ekDeleteComp, ekRemoveCb} + extra
      )
    
    for s in removedSystems:
      result.cs[s] = {ekSystemRemoveAny, ekCompRemoveFrom, ekRowRemoved, ekRowRemovedCb}

  proc diff[T](t1, t2: Table[string, T]): Table[string, T] =
    ## Returns the items `t2` is missing from `t1`.
    for k, v in t1:
      if k notin t2:
        result[k] = v
      else:
        let d = v - t2[k]
        if d.len > 0:
          result[k] = d

  proc diff(e1, e2: Events): Events =
    result.evName = e1.evName & " diff"
    result.c = e1.c -  e2.c
    result.cs = e1.cs.diff e2.cs

  proc len(events: Events): int =
    result = events.c.len
    for v in events.cs.values:
      result += v.len

  var
    componentEvents: Table[string, Events]

  proc regEvent(ty: typedesc, event: EventKind) =
    let
      key = $ty

    echo "[ Type ", key, " event ", event, " ]"
    
    componentEvents.withValue(key, curEv):
      check event notin curEv.c
      curEv.c.incl event
    do:
      componentEvents[key] = Events(
        evName: key,
        c: {event}
      )

  proc regEvent(ty: typedesc, event: EventKind, sys: string) =
    let
      key = $ty
    
    echo "[ Type ", key, " event ", event, " for ", sys, " ]"

    componentEvents.withValue(key, curEv):
      curEv.cs.withValue(sys, curVal):
        check event notin curVal[]
        curVal[].incl event
      do:
        curEv.cs[sys] = {event}
    do:
      componentEvents[key] = Events(
        evName: key,
        cs: {sys: {event}}.toTable
      )

  var
    entityChanges: seq[(int, EventKind, seq[ComponentTypeId])]

  onEntityChange:
    entityChanges.add (entity.entityId.int, state, types)
    for c in types:
      caseComponent c:
        componentType.regEvent state


  EventTest1.onInit: curComponent.accessType.regEvent ekInit
  EventTest1.onAdd: curComponent.accessType.regEvent ekAdd
  EventTest1.onAddCallback: curComponent.accessType.regEvent ekAddCb
  EventTest1.onUpdate: curComponent.accessType.regEvent ekUpdate
  EventTest1.onRemove: curComponent.accessType.regEvent ekRemove
  EventTest1.onDelete: curComponent.accessType.regEvent ekDeleteComp
  EventTest1.onRemoveCallback: curComponent.accessType.regEvent ekRemoveCb
  EventTest1.onSystemAdd: curComponent.accessType.regEvent ekSystemAddAny, sys.name
  EventTest1.onSystemRemove: curComponent.accessType.regEvent ekSystemRemoveAny, sys.name
  EventTest1.onSystemAddTo "simpleEvents1": curComponent.accessType.regEvent ekCompAddTo, sys.name
  EventTest1.onSystemAddTo "simpleEvents2": curComponent.accessType.regEvent ekCompAddTo, sys.name
  EventTest1.onSystemAddTo "simpleEvents3": curComponent.accessType.regEvent ekCompAddTo, sys.name
  EventTest1.onSystemAddTo "simpleEvents4": curComponent.accessType.regEvent ekCompAddTo, sys.name
  EventTest1.onSystemRemoveFrom "simpleEvents1": curComponent.accessType.regEvent ekCompRemoveFrom, sys.name
  EventTest1.onSystemRemoveFrom "simpleEvents2": curComponent.accessType.regEvent ekCompRemoveFrom, sys.name
  EventTest1.onSystemRemoveFrom "simpleEvents3": curComponent.accessType.regEvent ekCompRemoveFrom, sys.name
  EventTest1.onSystemRemoveFrom "simpleEvents4": curComponent.accessType.regEvent ekCompRemoveFrom, sys.name

  EventTest2.onInit: curComponent.accessType.regEvent ekInit
  EventTest2.onAdd: curComponent.accessType.regEvent ekAdd
  EventTest2.onAddCallback: curComponent.accessType.regEvent ekAddCb
  EventTest2.onUpdate: curComponent.type.regEvent ekUpdate
  EventTest2.onRemove: curComponent.accessType.regEvent ekRemove
  EventTest2.onDelete: curComponent.accessType.regEvent ekDeleteComp
  EventTest2.onRemoveCallback: curComponent.accessType.regEvent ekRemoveCb
  EventTest2.onSystemAdd: curComponent.type.regEvent ekSystemAddAny, sys.name
  EventTest2.onSystemRemove: curComponent.type.regEvent ekSystemRemoveAny, sys.name
  EventTest2.onSystemAddTo "simpleEvents4": curComponent.type.regEvent ekCompAddTo, sys.name
  EventTest2.onSystemRemoveFrom "simpleEvents4": curComponent.type.regEvent ekCompRemoveFrom, sys.name

  makeSystemOpts "simpleEvents1", [EventTest1], sOpts:
    added:
      EventTest1.regEvent ekRowAdded, sys.name
    addedCallback:
      EventTest1.regEvent ekRowAddedCb, sys.name
    removed:
      EventTest1.regEvent ekRowRemoved, sys.name
    removedCallback:
      EventTest1.regEvent ekRowRemovedCb, sys.name

  makeSystemOpts "simpleEvents2", [EventTest1], sOpts:
    added:
      EventTest1.regEvent ekRowAdded, sys.name
    addedCallback:
      EventTest1.regEvent ekRowAddedCb, sys.name
    removed:
      EventTest1.regEvent ekRowRemoved, sys.name
    removedCallback:
      EventTest1.regEvent ekRowRemovedCb, sys.name

  makeSystemOpts "simpleEvents3", [EventTest1], sOpts:
    added:
      EventTest1.regEvent ekRowAdded, sys.name
    addedCallback:
      EventTest1.regEvent ekRowAddedCb, sys.name
    removed:
      EventTest1.regEvent ekRowRemoved, sys.name
    removedCallback:
      EventTest1.regEvent ekRowRemovedCb, sys.name

  makeSystemOpts "simpleEvents4", [EventTest1, EventTest2], sOpts:
    added:
      EventTest1.regEvent ekRowAdded, sys.name
      EventTest2.regEvent ekRowAdded, sys.name
    addedCallback:
      EventTest1.regEvent ekRowAddedCb, sys.name
      EventTest2.regEvent ekRowAddedCb, sys.name
    removed:
      EventTest1.regEvent ekRowRemoved, sys.name
      EventTest2.regEvent ekRowRemoved, sys.name
    removedCallback:
      EventTest1.regEvent ekRowRemovedCb, sys.name
      EventTest2.regEvent ekRowRemovedCb, sys.name


  # ------------

  makeEcsCommit("run", eOpts)

  # ------------

  proc `$`(e: Events, indent = 0): string =
    result = "events \"" & e.evName & "\n"

    let
      rLen = result.len
      subIndent = indent + 2
      indentStr = ' '.repeat(subIndent)

    if e.c.len > 0:
      result &= indentStr & $e.c & "\n"
    
    if e.cs.len > 0:
      result &= indentStr & "e.sev: " & $e.cs & "\n"
    
    if result.len == rLen:
      result &= indentStr & "[No items]\n"
    result &= ' '.repeat(indent) & '-'.repeat(10) & "\n"

  template checkEvents(task: string, expected: Events) =

    let
      cTypeStr = expected.evName
      events = componentEvents.getOrDefault cTypeStr
    
    if cTypeStr notin componentEvents:
      echo "  [No events found]"
      check false
    else:
      let
        missing = expected.diff events
        unexpected = events.diff expected
        passed = missing.len == 0 and unexpected.len == 0

      echo "\nTask \"" & task & "\" for " & cTypeStr & ": "
      if not passed:
        echo "  Used ", `$`(events, 2)
        if missing.len > 0:
          echo "  Missing ", `$`(missing, 2)
        if unexpected.len > 0:
          echo "  Unexpected ", `$`(unexpected, 2)
      else:
        echo "  <Passed>"
      # forAllSystems:
      #   echo sys.name, ": ", sys.count
      
      componentEvents.del cTypeStr
      check passed

    echo "  ---"


  suite "Event invocation": 

    proc clearEvents =
      componentEvents.clear
      entityChanges.setLen 0

    const
      allSys = @["simpleEvents1", "simpleEvents2", "simpleEvents3", "simpleEvents4"]
      et1 = @["simpleEvents1", "simpleEvents2", "simpleEvents3"]
      et2 = @["simpleEvents4"]

    var
      e1: EntityRef

    test "newEntityWith 1":
      
      e1 = newEntityWith(EventTest1())
      checkEvents("new entity EventTest1", EventTest1.addEvents(et1, {ekNewEntityWith}))

    clearEvents()

    test "delete 1":
      e1.delete
      checkEvents("delete 1", EventTest1.removeEvents(et1, {ekDeleteEnt}))

    clearEvents()

    test "newEntityWith 2":

      e1 = newEntityWith(EventTest1(), EventTest2())

      checkEvents("new entity EventTest1", EventTest1.addEvents(allSys, {ekNewEntityWith}))
      checkEvents("new entity EventTest2", EventTest2.addEvents(et2, {ekNewEntityWith}))

    clearEvents()

    test "remove 1":
      e1.remove EventTest1

      checkEvents("remove", EventTest1.removeEvents(allSys, {ekRemoveComponents}))
      checkEvents("remove", EventTest2.removeEvents(et2, {ekRemoveComponents}))

      # We expect EventTest2 to be removed too as it is dependent on EventTest1.
      check sysSimpleEvents4.count == 0
      check e1.componentCount == 0

    clearEvents()

    test "add 1":
      e1.add EventTest1()

      checkEvents("add", EventTest1.addEvents(et1, {ekAddComponents}))

    clearEvents()

    test "add 2":
      e1.add EventTest2()
      var
        et1expected = EventTest1.addEvents(et2, {})
      
      # EventTest1 has already triggered init and add events in "add 1".
      et1expected.c.excl {ekAdd, ekAddCb, ekInit}

      checkEvents("add 2", et1expected)
      checkEvents("add 2", EventTest2.addEvents(et2, {ekAddComponents}))

    clearEvents()

    test "remove 2":
      e1.remove EventTest2()
      var
        et1expected = EventTest1.removeEvents(et2, {})
      
      # EventTest1 isn't being destroyed.
      et1expected.c.excl {ekRemove, ekRemoveCb, ekDeleteComp}

      checkEvents("remove 2", et1expected)
      checkEvents("remove 2", EventTest2.removeEvents(et2, {ekRemoveComponents}))

    clearEvents()

    test "delete 2":
      e1.add EventTest2()

      clearEvents()
    
      e1.delete

      checkEvents("delete 2", EventTest1.removeEvents(allSys, {ekDeleteEnt}))
      checkEvents("delete 2", EventTest2.removeEvents(et2, {ekDeleteEnt}))
    
    clearEvents()

    test "construct":

      let
        e {.used.} = cl(EventTest1(), EventTest2()).construct

      checkEvents("construct", EventTest1.addEvents(allSys, {ekConstruct}))
      checkEvents("construct", EventTest2.addEvents(et2, {ekConstruct}))

  flushGenLog()

when isMainModule:
  import tables, unittest, strutils

  static:
    defaultIdentity.set_private true

  const
    cOpts = fixedSizeComponents(100)
    sOpts = fixedSizeSystem(100)
    eOpts = fixedSizeEntities(100)

  block:
    defineEventSpectrum(cOpts, sOpts, eOpts)
