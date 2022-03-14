import polymorph

template testEvents*(componentOptions: ECSCompOptions, systemOptions: ECSSysOptions) {.dirty.} =

  registerComponents(componentOptions):
    type
      Test1* = object
        data*: int
      Test2* = object
        val*: float

  defineSystem("a", [Test1], systemOptions)
  defineSystem("b", [Test1], systemOptions)
  defineSystem("c", [Test1], systemOptions)
  defineSystem("d", [Test1, own Test2], systemOptions)
  
  type
    TestTypeKind = enum ttTest, ttTest2
    TestType = object
      case kind: TestTypeKind
      of ttTest: test: Test1
      of ttTest2: test2: Test2

  var
    compAdds: int
    comp2Adds: int
    systemAdds: Table[string, int]
    systemAddTos: Table[string, int]
    comp1Intercepts: int
    comp2Intercepts: int
    expected: seq[TestType]
    eventLog: seq[string]
  
  const
    displayLog = defined(ecsLog)
  
  template log(str: string, indent: int) =
    when displayLog:
      let
        info = instantiationInfo()
        source = currentSourcePath() & "(" & $info.line & ") "
        indStr = spaces(indent * 2) & str
        padding = max(2, 80 - indStr.len)
      eventLog.add indStr & spaces(padding) & source
  
  template log(str: string): untyped =
    log(str, 0)
  
  template doCheck(curComp: untyped, prefix: string, expectedKind: TestTypeKind) =
    var found: bool
    test "Event " & prefix & " got " & $curComp.type & ", expects " & $expectedKind:
      for i in countDown(expected.high, 0):
        if expected[i].kind == expectedKind:
          found = true
          when curComp is Test1Instance:
            check curComp.valid
            check curComp.access == expected[i].test
          elif curComp is Test2Instance:
            check curComp.valid
            check curComp.access == expected[i].test2
          elif curComp is Test1:
            check curComp == expected[i].test
          elif curComp is Test2:
            check curComp == expected[i].test2
          else:
            checkpoint "Unknown type for curComponent: " & $curComp.type
            fail()
          break
      if not found:
        checkpoint "Unexpected kind " & $expectedKind & " found. Expected list: " & $expected & ", curComponent TypeId " & $(curComp.repr)
      check found

  Test1.onInterceptUpdate:
    let
      lastVal {.inject.} = comp1Intercepts
    comp1Intercepts += 1
    log &"Intercepting update for Test1, count: {lastVal} -> {comp1Intercepts}", 1

  Test1.onAdd:
    let
      lastVal = compAdds
    curComponent.doCheck("onAdd", ttTest)
    compAdds += 1
    log &"Entity: {curEntity.entityId.int} adding Test1, count: {lastVal} -> {compAdds}", 1

  Test1.onRemove:
    let
      lastVal = compAdds
    curComponent.doCheck("onRemove", ttTest)
    compAdds -= 1
    log &"Entity: {curEntity.entityId.int} removing Test1, count: {lastVal} -> {compAdds}", 1
  
  Test1.onSystemAdd:
    curComponent.doCheck("onSystemAdd", ttTest)
    let lastVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = lastVal + 1
    log &"Entity: {curEntity.entityId.int} Test1 adding to system {curSystem.name} count: {lastVal} -> {systemAdds[curSystem.name]}", 1

  Test1.onSystemAddTo "a":
    curComponent.doCheck("onSystemAddTo \"a\"", ttTest)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal + 1
    log &"Entity: {curEntity.entityId.int} Test1 adding to specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1

  Test1.onSystemAddTo "b":
    curComponent.doCheck("onSystemAddTo \"b\"", ttTest)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal + 1
    log &"Entity: {curEntity.entityId.int} Test1 adding to specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1

  Test1.onSystemAddTo "c":
    curComponent.doCheck("onSystemAddTo \"c\"", ttTest)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal + 1
    log &"Entity: {curEntity.entityId.int} Test1 adding to specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1

  Test1.onSystemAddTo "d":
    curComponent.doCheck("onSystemAddTo \"d\"", ttTest)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal + 1
    log &"Entity: {curEntity.entityId.int} Test1 adding to specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1
  
  Test1.onSystemRemove:
    curComponent.doCheck("onSystemRemove", ttTest)
    let lastVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = lastVal - 1
    log &"Entity: {curEntity.entityId.int} Test1 removing from system {curSystem.name} count: {lastVal} -> {systemAdds[curSystem.name]}", 1
    check systemAddTos[curSystem.name] >= 0

  Test1.onSystemRemoveFrom "a":
    curComponent.doCheck("onSystemRemoveFrom \"a\"", ttTest)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal - 1
    log &"Entity: {curEntity.entityId.int} Test1 removing from specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1
    check systemAddTos[curSystem.name] >= 0
    assert systemAddTos[curSystem.name] >= 0

  Test1.onSystemRemoveFrom "b":
    curComponent.doCheck("onSystemRemoveFrom \"b\"", ttTest)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal - 1
    log &"Entity: {curEntity.entityId.int} Test1 removing from specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1
    check systemAddTos[curSystem.name] >= 0

  Test1.onSystemRemoveFrom "c":
    curComponent.doCheck("onSystemRemoveFrom \"c\"", ttTest)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal - 1
    log &"Entity: {curEntity.entityId.int} Test1 removing from specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1
    check systemAddTos[curSystem.name] >= 0

  Test1.onSystemRemoveFrom "d":
    curComponent.doCheck("onSystemRemoveFrom \"d\"", ttTest)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal - 1
    log &"Entity: {curEntity.entityId.int} Test1 removing from specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1
    check systemAddTos[curSystem.name] >= 0

  Test2.onInterceptUpdate:
    let
      lastVal {.inject.} = comp2Intercepts
    comp2Intercepts += 1
    log &"ntercepting update for Test2, count: {lastVal} -> {comp1Intercepts}", 1

  Test2.onAdd:
    let
      lastVal = comp2Adds
    curComponent.doCheck("onAdd", ttTest2)
    comp2Adds += 1
    log &"Entity: {curEntity.entityId.int} adding Test2, count: {lastVal} -> {comp2Adds}", 1

  Test2.onRemove:
    let
      lastVal = comp2Adds
    curComponent.doCheck("onRemove", ttTest2)
    comp2Adds -= 1
    log &"Entity: {curEntity.entityId.int} removing Test2, count: {lastVal} -> {comp2Adds}", 1

  Test2.onSystemAdd:
    curComponent.doCheck("onSystemAdd", ttTest2)
    let lastVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = lastVal + 1
    log &"Entity: {curEntity.entityId.int} Test2 adding to system {curSystem.name} count: {lastVal} -> {systemAdds[curSystem.name]}", 1

  Test2.onSystemRemove:
    curComponent.doCheck("onSystemRemove", ttTest2)
    let lastVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = lastVal - 1
    log &"Entity: {curEntity.entityId.int} Test2 removing from system {curSystem.name} count: {lastVal} -> {systemAdds[curSystem.name]}", 1

  Test2.onSystemAddTo "d":
    curComponent.doCheck("onSystemAddTo \"d\"", ttTest2)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal + 1
    log &"Entity: {curEntity.entityId.int} Test2 adding to specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1

  Test2.onSystemRemoveFrom "d":
    curComponent.doCheck("onSystemRemoveFrom \"d\"", ttTest2)
    let lastVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = lastVal - 1
    log &"Entity: {curEntity.entityId.int} Test2 removing from specific system {curSystem.name} count: {lastVal} -> {systemAddTos[curSystem.name]}", 1

  const
    maxEnts = 100

  makeECS(ECSEntityOptions(maxEntities: maxEnts))
  commitSystems("run")

  var otherEnts: seq[EntityRef]

  comp2Intercepts = 0

  proc checkGroups(prefix = "") =
    forAllSystems:
      test prefix & ": Check group trimming for system \"" & sys.name & "\"" :
        var
          ok = true
          deadEnts: int
        for i, ite in sys.groups:
          if not ite.entity.alive:
            checkpoint "Group idx " & $i & " is dead in system \"" & sys.name & "\" EntityId " & $ite.entity.entityId.int
            deadEnts += 1
            ok = false
          check ite.entity.alive
        if deadEnts > 0: checkpoint "Dead/Total for system \"" & sys.name & "\": " & $deadEnts & "/" & $sys.groups.len
        check ok

  let entCount = 1
  var
    testVal: Test1
    otherEntVal = Test1(data: 180)
    test2Val: Test2
    e1, e2, e3: EntityRef
    expectedE1, expectedE2, expectedE3: seq[TestType]
  for i in 0 ..< entCount:
    suite "e1 newEntityWith...":
      test "e1 newEntityWith":
        testVal = Test1(data: 1)
        expectedE1 = @[TestType(kind: ttTest, test: testVal)]
        expected = expectedE1

        log "NewEntityWith Test1:"
        e1 = newEntityWith(
          testVal 
        )

    suite "Other entity...":
      test "Other entity":
        expected = @[TestType(kind: ttTest, test: otherEntVal)]
        log "NewEntityWith Test1:"
        otherEnts.add newEntityWith(Test1(data: 180))

    suite "e2 addComponents...":
      test "e2 addComponents Test1, Test2":
        e2 = newEntity()
        testVal = Test1(data: rand 10000)
        test2Val = Test2(val: rand 1.0)
        
        expectedE2 = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
        expected = expectedE2
        log "AddComponents: Test1, Test2"
        discard e2.addComponents(testVal, test2Val)

    checkGroups("e2 addComponents")
      
    suite "e2 deleting...":
      test "e2 deleting":
        # Expected should match addComponents.
        log "Deleting:"
        e2.delete

    checkGroups("e2 deleting")

    suite "e3 addComponent 1...":
      test "e3 addComponent 1 Test1":
        e3 = newEntity()
        testVal = Test1(data: 3)
        expectedE3 = @[TestType(kind: ttTest, test: testVal)]
        expected = expectedE3

        log "AddComponent Test1:"
        e3.addComponent testVal
      
    suite "e3 addComponent 2...":
      test "e3 addComponent 2 Test2":
        test2Val = Test2(val: rand 1.0)
        # The entity already had Test1 and so now matches "d", as such
        # we should expect two system add invocations for Test1 and Test1 2 being placed within "d".  
        expected = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
        log "AddComponent Test2:"
        discard e3.addComponents test2Val
    
    suite "e3 update":
      test "e3 update Test1":
        let
          t1 = e3.fetch Test1
          t2 = e3.fetch Test2
        t1.update t1.access
        t2.update t2.access
        t1.update t1.access
        t2.update t2.access
    
    suite "e3 Removing component...":
      test "e3 Removing component Test1, (dependent) Test2":
        # This will trigger for both components in the "d" system as well as for each of the other
        # systems that take Test1.
        expected = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
        log "RemoveComponent:"

        e3.removeComponent Test1
        
        # We expect that because Test2 is owned, removing Test1
        # invalidates Test2's storage so it is also removed.
        check e3.componentCount == 0
        checkGroups("e3 remove Test1")

    suite "e1 delete...":
      test "e1 delete":
        expected = expectedE1
        log "Delete:"
        e1.delete
        checkGroups("e1 deleting")
    suite "e3 delete...":
      test "e3 delete":
        expected = @[TestType(kind: ttTest2, test2: test2Val)]
        e3.delete
        checkGroups("e3 deleting")
    suite "Construct":
      test "Single component construction: Test1":
        let
          testVal = Test1(data: 7)

        check compAdds == otherEnts.len
        check comp2Adds == 0

        expected = @[TestType(kind: ttTest, test: testVal)]
        let entity1 = @[Component(testVal.makeContainer)].construct()

        check compAdds == otherEnts.len + 1
        check comp2Adds == 0

        entity1.delete

        check compAdds == otherEnts.len
        check comp2Adds == 0

      test "Construction with an owned component: Test1, Test2":

        # Construction of owned type with multiple\n components.
        expected = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
        let entity2 = @[testVal.makeContainer, test2Val.makeContainer].construct()
        
        check compAdds == otherEnts.len + 1
        check comp2Adds == 1
        let comp = entity2.fetch(Test1, Test2)
        check:
          comp.test1.valid
          comp.test2.valid
          entity2 in sysA
          entity2 in sysB
          entity2 in sysC
          entity2 in sysD
        

        entity2.delete

        check compAdds == otherEnts.len
        check comp2Adds == 0
        check:
          entity2 notin sysA
          entity2 notin sysB
          entity2 notin sysC
          entity2 notin sysD

    log "---"

  test "Deleting other ents...":
    log "Delete other ents:"
    expected = @[TestType(kind: ttTest, test: otherEntVal)]
    for ent in otherEnts:
      ent.delete
  
  test "Add/remove component events balance":
    check compAdds == 0
    check comp2Adds == 0

  proc balanced(table: Table[string, int]): bool =
    for pair in table.pairs:
      if pair[1] != 0:
        echo "System " & pair[0] & " has non-zero add count: " & $pair[1]
        return false
    true
  
  test "Add to any system":
    check systemAdds.balanced
  test "Add to specific system":
    check systemAddTos.balanced
  test "Intercept update unowned":
    check comp1Intercepts == entCount * 2
  test "Intercept update owned":
    check comp2Intercepts == entCount * 2
  
  when displayLog:
    echo "Log:"
    for item in eventLog:
      echo item
  
  when displayLog:
    echo "Finish."
  flushGenLog()

when isMainModule:
  import tables, strformat, strutils, random, unittest

  static:
    defaultIdentity.set_private true
  block:
    testEvents(defaultCompOpts, defaultSysOpts)
