template runGroupsAndOrder* {.dirty.} =
  registerComponents(defaultCompOpts):
    type
      TestOrder = object

  # defineSystem sets order.
  const
    sOptsIP = ECSSysOptions(commit: sdcInPlace)
    sOptsDM = ECSSysOptions(commit: sdcDeferMakeEcs)

  # OrderA's type and instance definition has been deferred to 'makeEcs()'
  # with 'sdcDeferMakeEcs'. This is the default.
  defineSystem("orderA", [TestOrder], sOptsDM)
  assert not compiles(sysOrderA.count)

  # OrderB is defined with 'sdcInPlace' to immediately output the
  # system's type and initialise an instance.
  defineSystem("orderB", [TestOrder], sOptsIP)
  assert compiles(sysOrderB.count)

  defineGroupStart "adHocGroup"       # Put following systems into a group.
  defineSystem("orderC", [TestOrder], sOptsIP)
  assert compiles(sysOrderC.count)    # Check the system variable is instantiated.
  defineGroupEnd()                    # Stop registering to a group.

  defineSystem("orderD", [TestOrder])
  assert not compiles(sysOrderD.count)  # Check system instantiation is deferred.

  # Group and order a selection of systems.

  # Adding a system to a group removes it from the output of commitSystems.
  # Each group proc is output with commitGroup.

  defineGroup("sysGroup1", ["orderB", "orderA"])
  defineGroup("sysGroup2", ["orderA", "orderB"])


  var systemUpdates: seq[string]

  # A selection of ways to define the system body in a different order.
  # These makeSystems already have their order defined above, so the
  # order the bodies are encountered should not influence execution order.

  makeSystem("orderD", [TestOrder]):
    all: systemUpdates.add sys.name

  makeSystemBody("orderC"):
    all: systemUpdates.add sys.name

  makeSystemOpts("orderB", [TestOrder], sOptsIP):
    # tOptions must match
    all: systemUpdates.add sys.name

  makeSystemBody("orderA"):
    all: systemUpdates.add sys.name

  # When a system isn't already defined, makeSystem invokes defineSystem
  # and order is therefore appended as found in the source.

  makeSystem("orderE", [TestOrder]):
    all: systemUpdates.add sys.name

  # Pull currently ungrouped systems into a group.
  defineGroupCurrent "groupCurrent"

  makeSystem("orderF", [TestOrder]):
    all: systemUpdates.add sys.name

  onEcsBuilding:
    # The code is injected *before* the output of 'makeEcs'.
    when defined(ecsLog):
      echo "Started makeEcs build"

    type InitEvent = enum eeAll, eeGroup
    var initEvents {.inject.}: seq[(InitEvent, string)]
    initEvents.add (eeAll, "Building")

  onEcsBuilt:
    # This code is added after 'makeEcs' has finished building the ECS
    # and entities can be created and modified.
    initEvents.add (eeAll, "Built")
    # OrderB definition is deferred to the beginning of 'makeEcs()'.
    assert compiles(sysOrderB.count)

  template eventStr: untyped =
    $context & (if group.len > 0: " " & group else: "")    

  onEcsCommitAll:
    # This code is injected before every 'commitSystems' and 'commitGroups'
    # outputs the system run procedures.
    block:
      let str = "Commit all - " & eventStr
      when defined(ecsLog):
        echo str
      initEvents.add (eeAll, str)

  onEcsCommitNextGroup:
    # This code is emitted once before the next 'commitGroup'.
    when defined(ecsLog):
      echo "First commitGroup - " & eventStr
    initEvents.add (eeGroup, "NextGroup")

  onEcsCommitNext:
    # This code is emitted once before the next 'commitSystems'.
    when defined(ecsLog):
      echo "First commitSystems - " & eventStr
    initEvents.add (eeAll, "Next")

  onEcsCommitGroups ["sysGroup1"]:
    # This is emitted before the "sysGroup1" group is committed.
    assert group == "sysGroup1"
    when defined(ecsLog):
      echo "Specific group - ", eventStr
    initEvents.add (eeGroup, "Commit specific - " & group)

  onEcsCommitSystem "orderD":
    # Emitted after the run procedure is defined for a specific system.
    when defined(ecsLog):
      echo "Order D's run procedure now exists"
    assert compiles(doOrderD())
    assert not compiles(doOrderE())
  
  onEcsCommitSystem "orderE":
    # Emitted after the run procedure is defined for a specific system.
    when defined(ecsLog):
      echo "Order E's run procedure now exists"
    assert compiles(doOrderD())
    assert compiles(doOrderE())

  makeEcs()

  commitGroup("sysGroup1", "runGroup1") # Expected: onEcsCommitAll, onEcsCommitNextGroup, onEcsCommitGroups.
  commitGroup("sysGroup2", "runGroup2") # Expected: onEcsCommitAll, onEcsCommitGroups.
  commitGroup("adHocGroup", "runAdHocGroup")
  commitGroup("groupCurrent", "runCurGroup")
  commitSystems("runRest")              # Expected: onEcsCommitAll, onEcsCommitNext
  
  # Clear identity events for any following ECS.
  clearOnEcsBuilding()
  clearOnEcsBuilt()
  clearOnEcsCommitAll()
  
  let e {.used.} = newEntityWith(TestOrder())

  runGroup1()
  runGroup2()
  runAdHocGroup()
  runCurGroup()
  runRest()

  suite "Groups and ordering systems":
    test "Commit events":
      # Commit events are in widest to smallest scope.
      check initEvents == @[
        (eeAll, "Building"),
        (eeAll, "Built"),
        (eeAll, "Commit all - ccCommitGroup sysGroup1"),
        (eeGroup, "Commit specific - sysGroup1"),
        (eeGroup, "NextGroup"),
        (eeAll, "Commit all - ccCommitGroup sysGroup2"),
        (eeAll, "Commit all - ccCommitGroup adHocGroup"),
        (eeAll, "Commit all - ccCommitGroup groupCurrent"),
        (eeAll, "Commit all - ccCommitSystems"),
        (eeAll, "Next"),
      ]
    test "Check order":
      check systemUpdates ==
        @[
          "orderB",
          "orderA",
          "orderA",
          "orderB",
          "orderC",
          "orderD",
          "orderE",
          "orderF",
        ]
  flushGenLog()

when isMainModule:
  
  import polymorph, unittest

  runGroupsAndOrder()
