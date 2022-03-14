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

  onEcsBuilt:
    type InitEvent = enum eeAll, eeGroup
    var initEvents {.inject.}: seq[(InitEvent, string)]

  onEcsCommitAll:
    # This code is emitted before every commitSystems and commitGroups.
    if curGroup.len > 0:
      when defined(ecsLog):
        echo "[ Commit all for group \"" & curGroup & "\" ]"
      initEvents.add (eeGroup, curGroup)
    else:
      when defined(ecsLog):
        echo "[ Commit all ]"
      initEvents.add (eeAll, "")

  onEcsCommitGroups ["sysGroup1"]:
    # This is emitted when sysGroup1 is emitted.
    assert curGroup == "sysGroup1"
    when defined(ecsLog):
      echo "[ Committing specific group ", curGroup, " ]"
    initEvents.add (eeGroup, "specific " & curGroup)

  onEcsNextCommit:
    # This code is emitted once before the next commitSystems, then reset.
    when defined(ecsLog):
      echo "[ First commitSystems ]"
    initEvents.add (eeAll, "Next")

  makeEcs()

  commitGroup("sysGroup1", "runGroup1")
  commitGroup("sysGroup2", "runGroup2")
  commitSystems("runRest")
  
  # Clear onEcsCommitAll event for any following ECS.
  clearOnEcsCommitAll()
  
  let e {.used.} = newEntityWith(TestOrder())

  runGroup1()
  runGroup2()
  runRest()

  suite "Groups and ordering systems":
    test "Commit events":
      check initEvents == @[
        (eeGroup, "sysGroup1"),
        (eeGroup, "specific sysGroup1"),
        (eeGroup, "sysGroup2"),
        (eeAll, "Next"),
        (eeAll, ""),
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
