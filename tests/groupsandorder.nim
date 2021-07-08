import polymorph

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

makeEcs()

commitGroup("sysGroup1", "runGroup1")
commitGroup("sysGroup2", "runGroup2")
commitSystems("runRest")

let e {.used.} = newEntityWith(TestOrder())

runGroup1()
runGroup2()
runRest()

import unittest

suite "Groups and ordering systems":
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
