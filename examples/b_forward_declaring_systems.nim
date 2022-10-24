## Systems have a lot of flexibility in how they're used.
## 
## This example shows how we can forward declare systems with
## `defineSystem` then add the system body after the ECS has been sealed
## by `makeEcs`.
## 
## Forward declared systems participate in the ECS state, so adding and
## removing components they use cause the system's state to be updated.
## 
## The run procedure itself is generated separately by `commitSystems`.
## 
## To combine both `makeEcs` and `commitSystems`, use `makeEcsCommit`.

import polymorph

registerComponents defaultCompOpts:
  type
    Comp1 = object
    Comp2 = object

# Define a system.
defineSystem "mySystem1", [Comp1]

# Define a system and pass compile options to it.
defineSystem "mySystem2", [Comp1, Comp2], defaultSysOpts

# Define a system and code body at the same time.
makeSystem "mySystem3", [Comp1, Comp2]:
  all:
    echo "Comp1: ", item.comp1

# Once the ECS is sealed, new systems can't be defined using these components.
# 'makeEcs' outputs the ECS but not the system execution procedures.
# This lets us add the system bodies below.
makeEcs()

# Create a code body for the previously defined system.
makeSystemBody "mySystem1":
  all:
    echo "Comp1: ", item.comp1

# This time we're using 'makeSystem' to define the body for the other
# system.
#
# Options and component parameters we specify here must match the system's
# defineSystem. If not, a compile time error is raised.
#
# This can be useful when we want to see the components available when
# writing the body for a forward declared system (which could be in
# another module), and have these assumptions checked against the
# original definition.
makeSystem "mySystem2", [Comp1, Comp2]:
  all:
    echo "Comp1: ", item.comp1

commitSystems("runAll")

runAll()
