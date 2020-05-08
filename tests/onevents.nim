import polymorph, unittest

template defineTest*(componentOptions: ECSCompOptions, systemOptions: ECSSysOptions) {.dirty.} =
  registerComponents(componentOptions):
    type
      Test1* = object
        data*: int
      Test2* = object
        val*: float

  defineSystem("a", [Test1], systemOptions)
  defineSystem("b", [Test1], systemOptions)
  defineSystem("c", [Test1], systemOptions)
  defineSystemOwner("d", [Test1, Test2], [Test2], systemOptions)
  
  type
    TestTypeKind = enum ttTest, ttTest2
    TestType = object
      case kind: TestTypeKind
      of ttTest: test: Test1
      of ttTest2: test2: Test2

  import tables, strformat
  var
    compAdds: int
    comp2Adds: int
    systemAdds: Table[string, int]
    systemAddTos: Table[string, int]
    comp1Intercepts: int
    comp2Intercepts: int
    expected: seq[TestType]
    eventLog: seq[string]
  const displayLog = defined(onEventsLog)
  template log(str: string): untyped =
    when displayLog: eventLog.add str

  proc doCheck[T](curComp: T, prefix: string, expectedKind: TestTypeKind) =
    var found: bool
    test "Event " & prefix & " " & $curComp.type & " expects " & $expectedKind:
      for i in countDown(expected.high, 0):
        if expected[i].kind == expectedKind:
          found = true
          when curComp is Test1Instance:
            check curComp.access == expected[i].test
          elif curComp is Test2Instance:
            check curComp.access == expected[i].test2
          elif curComp is Test1:
            check curComp == expected[i].test
          elif curComp is Test2:
            check curComp == expected[i].test2
          else:
            checkpoint "Unknown type for curComponent"
            fail()
          return
      if not found:
        checkpoint "Cannot find expected kind " & $expectedKind & " in expected list: " & $expected & ", curComponent TypeId " & $curComp.typeId.int
      check found

  Test1.onInterceptUpdate:
    comp1Intercepts += 1
    log &" Intercepting adding Test1\n Intercept count: {comp1Intercepts}"
    commit(curValue)
  Test1.onAdd:
    curComponent.doCheck("onAdd", ttTest)
    compAdds += 1
    log &"Entity: {curEntity.entityId.int} adding Test1, count: {compAdds}"
  Test1.onRemove:
    curComponent.doCheck("onRemove", ttTest)
    compAdds -= 1
    log &"Entity: {curEntity.entityId.int} removing Test1, count: {compAdds}"
  
  Test1.onSystemAdd:
    curComponent.doCheck("onSystemAdd", ttTest)
    let curVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = curVal + 1
    log &"Entity: {curEntity.entityId.int} Test1 adding to system {curSystem.name} count: {systemAdds[curSystem.name]}"
  Test1.onSystemAddTo "a":
    curComponent.doCheck("onSystemAddTo \"a\"", ttTest)
    let curVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = curVal + 1
    log &"Entity: {curEntity.entityId.int} Test1 adding to specific system {curSystem.name} count: {systemAddTos[curSystem.name]}"
  
  Test1.onSystemRemove:
    curComponent.doCheck("onSystemRemove", ttTest)
    let curVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = curVal - 1
    log &"Entity: {curEntity.entityId.int} Test1 removing from system {curSystem.name} count: {systemAdds[curSystem.name]}"
  Test1.onSystemRemoveFrom "a":
    curComponent.doCheck("onSystemRemoveFrom \"a\"", ttTest)
    let curVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = curVal - 1
    log &"Entity: {curEntity.entityId.int} Test1 removing from specific system {curSystem.name} count: {systemAddTos[curSystem.name]}"

  Test2.onInterceptUpdate:
    comp2Intercepts += 1
    log &" Intercepting adding Test2\n Intercept count: {comp2Intercepts}"
    commit(curValue)
  Test2.onAdd:
    curComponent.doCheck("onAdd", ttTest2)
    comp2Adds += 1
    log &"Entity: {curEntity.entityId.int} adding Test2, count: {comp2Adds}"
  Test2.onRemove:
    curComponent.doCheck("onRemove", ttTest2)
    comp2Adds -= 1
    log &"Entity: {curEntity.entityId.int} removing Test2, count: {comp2Adds}"

  Test2.onSystemAdd:
    curComponent.doCheck("onSystemAdd", ttTest2)
    let curVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = curVal + 1
    log &"Entity: {curEntity.entityId.int} Test2 adding to system {curSystem.name} count: {systemAdds[curSystem.name]}"
  Test2.onSystemRemove:
    curComponent.doCheck("onSystemRemove", ttTest2)
    let curVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = curVal - 1
    log &"Entity: {curEntity.entityId.int} Test2 removing from system {curSystem.name} count: {systemAdds[curSystem.name]}"

import random

const
  maxEnts = 100
  compOpts = ECSCompOptions(maxComponents: maxEnts)
  sysOpts = ECSSysOptions(maxEntities: maxEnts)
  entOpts = ECSEntityOptions(maxEntities: maxEnts)

defineTest(compOpts, sysOpts)
makeECS(entOpts)
commitSystems("run")

proc runOnEvents* = 
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
    # Expecting x2 Test2 instances going through `update`.
    suite "e1 newEntityWith...":
      
      testVal = Test1(data: 1)
      expectedE1 = @[TestType(kind: ttTest, test: testVal)]
      expected = expectedE1

      log "NewEntityWith:"
      e1 = newEntityWith(
        testVal 
      )

    suite "Other entity...":
      expected = @[TestType(kind: ttTest, test: otherEntVal)]
      log "NewEntityWith:"
      otherEnts.add newEntityWith(Test1(data: 180))

    suite "e2 addComponents...":
      e2 = newEntity()
      testVal = Test1(data: rand 10000)
      test2Val = Test2(val: rand 1.0)
      
      expectedE2 = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
      expected = expectedE2
      log "AddComponents:"
      discard e2.addComponents(testVal, test2Val)

    checkGroups("e2 addComponents")
      
    suite "e2 deleting...":
      # Expected should match addComponents.
      log "Deleting:"
      e2.delete

    checkGroups("e2 deleting")

    suite "e3 addComponent 1...":
      e3 = newEntity()
      testVal = Test1(data: 3)
      expectedE3 = @[TestType(kind: ttTest, test: testVal)]
      expected = expectedE3

      log "AddComponent Test1:"
      discard e3.addComponent testVal
      
    suite "e3 addComponent 2...":
      test2Val = Test2(val: rand 1.0)
      # The entity already had Test1 and so now matches "d", as such
      # we should expect two system add invocations for Test1 and Test1 2 being placed within "d".  
      expected = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
      log "AddComponent Test2:"
      discard e3.addComponents test2Val
    
    suite "e3 Removing component...":
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
      expected = expectedE1
      log "Delete:"
      e1.delete
      checkGroups("e1 deleting")
    suite "e3 delete...":
      expected = @[TestType(kind: ttTest2, test2: test2Val)]
      e3.delete
      checkGroups("e3 deleting")
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
  test "Intercept create unowned":
    check comp1Intercepts == entCount * 4
  test "Intercept create owned":
    check comp2Intercepts == entCount * 2
  
  when displayLog:
    echo "Log:"
    for item in eventLog: echo item
  echo "Finish."
  flushGenLog()

when isMainModule:
  runOnEvents()
