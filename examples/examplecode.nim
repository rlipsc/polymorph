import polymorph

# Define component data
register defaultCompOpts:
  type
    Pos = object
      x, y: int
    Vel = object
      x, y: int

# Act on components
makeSystem "move", [Pos, Vel]:
  all:
    item.pos.x += item.vel.x
    item.pos.y += item.vel.y

# Generate the framework and execution procedure
makeEcsCommit "run"

# Create entities to use the "move" system
let
  moving = newEntityWith(
    Pos(x: 0, y: 0),
    Vel(x: 1, y: 1)
  )

# Execute "move" a number of times
for i in 0 ..< 4:
  run()

# Check the new component values
let pos = moving.fetch Pos
assert pos.x == 4 and pos.y == 4
