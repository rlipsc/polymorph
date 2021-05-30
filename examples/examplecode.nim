import polymorph

# Define components types.
registerComponents(defaultCompOpts):
  type
    Position = object
      x, y: int
    Velocity = object
      x, y: int

# Define logic to operate on a set of components.
makeSystem("move", [Position, Velocity]):
  all:
    item.position.x += item.velocity.x
    item.position.y += item.velocity.y

# Generate the ECS API.
makeEcs()

# Output defined systems, executed with a proc named `run`.
commitSystems("run")

# Create an entity to use the "move" system.
let
  movingEntity = newEntityWith(
    Position(x: 1, y: 1),
    Velocity(x: 2, y: -1),
  )

# Run "move" once.
run()

# Check the new component values.
let pos = movingEntity.fetch Position
assert pos.x == 3 and pos.y == 0