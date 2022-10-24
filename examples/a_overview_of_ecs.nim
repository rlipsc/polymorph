## Building an ECS has two stages.
## 
## First, we design the the components and systems we want to use with
## `registerComponents`/`register` and `makeSystem`/`makeSystemOpts`.
## 
## After the design is fully specified, the ECS is 'sealed' and
## generated for use with `makeEcsCommit`.
## 
## The ECS is then ready for use.

import polymorph

# The design stage.

# Create a component that can be added to entities.
registerComponents defaultCompOpts:
  type
    MyComponent = object

# Create a system that processes a component.
makeSystem "mySystem", [MyComponent]:
  all:
    echo "MyComponent: ", item.myComponent

# Seal the design and generate the ECS.
# The string parameter sets the name of the run procedure for systems.
makeEcsCommit "runSystems"

# Entities can now be used.

# Run the committed systems in the order they exist in the code.
runSystems()
