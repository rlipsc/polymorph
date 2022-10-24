## This example shows an ECS that acts as a simple physics engine.

import polymorph

# Parse some types as components.
register defaultCompOpts:
  type
    Pos = object
      x, y: float
    Vel = object
      x, y: float
    Gravity = object
      strength: float
    Bounce = object   # A dataless 'tag' component.

makeSystem "move", [Pos, Vel]:
  # Calculate basic movement.
  all:
    pos.x += vel.x
    pos.y += vel.y

makeSystem "gravity", [Vel, Gravity]:
  # Apply a gravity force to 'Vel'.
  all:
    vel.y -= gravity.strength

makeSystem "bounce", [Pos, Vel, Bounce]:
  all:
    # Correct 'Pos.y' to never goes below zero, enacting a simple bounce
    # to 'Vel.y' if this occurs.
    if pos.y <= 0.0:
      pos.y = 0.0
      vel.y = abs(vel.y) * 0.5

# Generate the framework and system procedures.
makeEcsCommit "run"

# Create entities to use the "move" system.
let
  moving = newEntityWith(
    Pos(x: 0.0, y: 0.0),
    Vel(x: 1.0, y: 1.0)
  )

# Execute participating systems a few times.
# Only "move" is active so far.
for i in 0 ..< 4:
  run()

# Check the new component values
let (pos, vel) = moving.fetch(Pos, Vel)
assert pos.x == 4.0 and pos.y == 4.0

# Now let's add gravity and bouncing to the entity.
# Note: when adding multiple components, the result is a tuple of the
# new instances.
# We're not interested in those right now, so we're 'discard' this tuple.
discard moving.add(Gravity(strength: 1.0), Bounce())

# Now when we run the systems, "gravity" and "bounce" participates.
for i in 0 ..< 4:
  run()

assert pos.x == 8.0 and pos.y == 2.0
assert vel.x == 1.0 and vel.y == -3.0


