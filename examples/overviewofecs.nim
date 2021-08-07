import polymorph

# Design stage.

registerComponents defaultCompOpts:
  type
    MyComponent = object

makeSystem "mySystem", [MyComponent]:
  all:
    echo "MyComponent: ", item.myComponent

# Seal the design and generate the ECS.

makeEcs()

# Output the system code.
commitSystems "runSystems"

# Run the committed systems in the order they exist in the code.
runSystems()
