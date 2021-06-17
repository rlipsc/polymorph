import polymorph

registerComponents(defaultCompOpts):
  type
    Comp1 = object
    Comp2 = object

# Define a system.
defineSystem("mySystem1", [Comp1])

# Define a system and pass compile options to it.
defineSystem("mySystem2", [Comp1, Comp2], defaultSysOpts)

# Define a system and code body at the same time.
makeSystem("mySystem3", [Comp1, Comp2]):
  all:
    echo "Comp1: ", item.comp1

# Once the ECS is sealed, new systems can't be defined using these components.
makeEcs()

# Define a code body for a previously defined system.
makeSystemBody("mySystem1"):
  all:
    echo "Comp1: ", item.comp1

# Define a code body for a previously defined system with makeSystem.
# Options are retrieved from the system's defineSystem.
makeSystem("mySystem2", [Comp1, Comp2]):
  all:
    echo "Comp1: ", item.comp1
