template runGroupsAndOrder* {.dirty.} =
  registerComponents(defaultCompOpts):
    type
      TestOrder = object

  # defineSystem sets order.

  defineSystem("orderA", [TestOrder])
  defineSystem("orderB", [TestOrder])
  defineSystem("orderC", [TestOrder])
  defineSystem("orderD", [TestOrder])

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

  makeSystemOpts("orderB", [TestOrder], defaultSysOpts):
    all: systemUpdates.add sys.name

  makeSystemBody("orderA"):
    all: systemUpdates.add sys.name

  # When a system isn't already defined, makeSystem invokes defineSystem
  # and order is therefore appended as found in the source.

  makeSystem("orderE", [TestOrder]):
    all: systemUpdates.add sys.name

  makeSystem("orderF", [TestOrder]):
    all: systemUpdates.add sys.name

  onEcsBuilding:
    # The code is injected before the output of makeEcs.
    when defined(ecsLog):
      echo "[ Started makeEcs build ]"

    type InitEvent = enum eeAll, eeGroup
    var initEvents {.inject.}: seq[(InitEvent, string)]
    initEvents.add (eeAll, "Building")

  onEcsBuilt:
    # This code is added after makeEcs has created a usable ECS.
    # System variables are updated but their run procedures have yet to be created.
    initEvents.add (eeAll, "Built")

  template eventStr: untyped =
    $context & (if group.len > 0: " " & group else: "")    

  onEcsCommitAll:
    # This code is emitted before every commitSystems and commitGroups.
    block:
      let str = "Commit all - " & eventStr
      when defined(ecsLog):
        echo "[ ", str, " ]"
      initEvents.add (eeAll, str)

  onEcsNextGroupCommit:
    when defined(ecsLog):
      echo "[ First commitGroup - " & eventStr & " ]"
    initEvents.add (eeGroup, "NextGroup")

  onEcsNextCommit:
    # This code is emitted once before the next commitSystems.
    when defined(ecsLog):
      echo "[ First commitSystems - " & eventStr & " ]"
    initEvents.add (eeAll, "Next")

  onEcsCommitGroups ["sysGroup1"]:
    # This is emitted only when sysGroup1 is emitted.
    assert group == "sysGroup1"
    when defined(ecsLog):
      echo "[ Specific group - ", eventStr, " ]"
    initEvents.add (eeGroup, "Commit specific - " & group)

  makeEcs()

  commitGroup("sysGroup1", "runGroup1") # Expected: onEcsCommitAll, onEcsNextGroupCommit, onEcsCommitGroups.
  commitGroup("sysGroup2", "runGroup2") # Expected: onEcsCommitAll, onEcsCommitGroups.
  commitSystems("runRest")              # Expected: onEcsCommitAll, onEcsNextCommit
  
  # Clear onEcsCommitAll event for any following ECS.
  clearOnEcsCommitAll()
  
  let e {.used.} = newEntityWith(TestOrder())

  runGroup1()
  runGroup2()
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

when isMainModule:
  
  import polymorph, unittest

  runGroupsAndOrder()
