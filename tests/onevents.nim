import polymorph, unittest

template defineTest*(componentOptions: ECSCompOptions, systemOptions: ECSSysOptions) {.dirty.} =
  registerComponents(componentOptions):
    type
      Test* = object
        data*: int
      Test2 = object
        val*: float

  defineSystem("a", [Test], systemOptions)
  defineSystem("b", [Test], systemOptions)
  defineSystem("c", [Test], systemOptions)
  defineSystem("d", [Test, Test2], systemOptions)
  
  # TODO: Move storage and access procs to sealing,
  # allowing us to add hooks for component creation/deletion as well.
  # This means we have hooks for adding components and creating/freeing components.
    
  type
    TestTypeKind = enum ttTest, ttTest2
    TestType = object
      case kind: TestTypeKind
      of ttTest: test: Test
      of ttTest2: test2: Test2

  import tables, strformat
  var
    compAdds: int
    comp2Adds: int
    systemAdds: Table[string, int]
    systemAddTos: Table[string, int]
    comp2Intercepts: int
    expected: seq[TestType]
    eventLog: seq[string]
  const displayLog = defined(onEventsLog)
  template log(str: string): untyped =
    when displayLog: eventLog.add str

  proc doCheck[T](curComp: T, expectedKind: TestTypeKind) =
    var found: bool
    for i in countDown(expected.high, 0):
      if expected[i].kind == expectedKind:
        found = true
        when curComp is TestInstance:
          check curComp.access == expected[i].test
        elif curComp is Test2Instance:
          check curComp.access == expected[i].test2
        else:
          check 1 == 0, "Unknown type for curComponent"
        return
    assert found, "Cannot find expected kind " & $expectedKind & " Expected: " & $expected & " TypeId " & $curComp.typeId.int
  
  Test.onAdd:
    curComponent.doCheck(ttTest)
    compAdds += 1
    log &"Entity: {curEntity.entityId.int} adding Test, count: {compAdds}"
  Test.onRemove:
    curComponent.doCheck(ttTest)
    compAdds -= 1
    log &"Entity: {curEntity.entityId.int} removing Test, count: {compAdds}"

  Test.onSystemAdd:
    curComponent.doCheck(ttTest)
    let curVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = curVal + 1
    log &"Entity: {curEntity.entityId.int} Test adding to system {curSystem.name} count: {systemAdds[curSystem.name]}"
  Test.onSystemAddTo "a":
    curComponent.doCheck(ttTest)
    let curVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = curVal + 1
    log &"Entity: {curEntity.entityId.int} Test adding to specific system {curSystem.name} count: {systemAddTos[curSystem.name]}"
  
  Test.onSystemRemove:
    curComponent.doCheck(ttTest)
    let curVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = curVal - 1
    log &"Entity: {curEntity.entityId.int} Test removing from system {curSystem.name} count: {systemAdds[curSystem.name]}"
  Test.onSystemRemoveFrom "a":
    curComponent.doCheck(ttTest)
    let curVal = systemAddTos.getOrDefault(curSystem.name)
    systemAddTos[curSystem.name] = curVal - 1
    log &"Entity: {curEntity.entityId.int} Test removing from specific system {curSystem.name} count: {systemAddTos[curSystem.name]}"


  Test2.onInterceptUpdate:
    comp2Intercepts += 1
    log &" Intercepting adding Test2\n Intercept count: {comp2Intercepts}"
    commit(curValue)
  Test2.onAdd:
    curComponent.doCheck(ttTest2)
    comp2Adds += 1
    log &"Entity: {curEntity.entityId.int} adding Test2, count: {comp2Adds}"
  Test2.onRemove:
    curComponent.doCheck(ttTest2)
    comp2Adds -= 1
    log &"Entity: {curEntity.entityId.int} removing Test2, count: {comp2Adds}"

  Test2.onSystemAdd:
    curComponent.doCheck(ttTest2)
    let curVal = systemAdds.getOrDefault(curSystem.name)
    systemAdds[curSystem.name] = curVal + 1
    log &"Entity: {curEntity.entityId.int} Test2 adding to system {curSystem.name} count: {systemAdds[curSystem.name]}"
  Test2.onSystemRemove:
    curComponent.doCheck(ttTest2)
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

  suite "On<Event> checks":
    let entCount = 1
    var
      testVal: Test
      otherEntVal = Test(data: 180)
      test2Val: Test2
      e1, e2, e3: EntityRef
      expectedE1, expectedE2, expectedE3: seq[TestType]
    for i in 0 ..< entCount:
      # Expecting x2 Test2 instances going through `update`.
      test "e1 newEntityWith...":
        
        testVal = Test(data: 1)
        expectedE1 = @[TestType(kind: ttTest, test: testVal)]
        expected = expectedE1

        log "NewEntityWith:"
        e1 = newEntityWith(
          testVal
        )

      test "Other entity...":
        expected = @[TestType(kind: ttTest, test: otherEntVal)]
        log "NewEntityWith:"
        otherEnts.add newEntityWith(Test(data: 180))

      test "e2 addComponents...":
        e2 = newEntity()
        testVal = Test(data: rand 10000)
        test2Val = Test2(val: rand 1.0)
        
        expectedE2 = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
        expected = expectedE2
        log "AddComponents:"
        discard e2.addComponents(testVal, test2Val)
      
      test "e2 deleting...":
        # Expected should match addComponents.
        log "Deleting:"
        e2.delete

      test "e3 addComponent 1...":
        e3 = newEntity()
        testVal = Test(data: 3)
        expectedE3 = @[TestType(kind: ttTest, test: testVal)]
        expected = expectedE3

        log "AddComponent Test:"
        e3.addComponent testVal
        
      test "e3 addComponent 2...":
        test2Val = Test2(val: rand 1.0)
        # The entity already had Test and so now matches "d", as such
        # we should expect two system add invocations for Test and Test 2 being placed within "d".  
        expected = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
        log "AddComponent Test2:"
        e3.addComponent test2Val
      
      test "e3 Removing component...":
        # This will trigger for both components in the "d" system as well as for each of the other
        # systems that take Test.
        expected = @[TestType(kind: ttTest, test: testVal), TestType(kind: ttTest2, test2: test2Val)]
        log "RemoveComponent:"
        e3.removeComponent Test
      
      test "e1 & e3 delete...":
        expected = expectedE1
        log "Delete:"
        e1.delete
        expected = @[TestType(kind: ttTest2, test2: test2Val)]
        e3.delete
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
    check comp2Intercepts == entCount * 2
  
  when displayLog:
    echo "Log:"
    for item in eventLog: echo item
  echo "Finish."


when isMainModule:
  runOnEvents()
