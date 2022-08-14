## Entity-component-systems with Polymorph

### About

Polymorph lets you write software in the [entity-component-system](https://en.wikipedia.org/wiki/Entity_component_system) pattern (or ECS for short).

The library implements a queryless, system oriented ECS focused on compile time code generation driven by your specific design and usage.
This generative approach produces extremely lean and performant code that scales from microcontroller event loops to complex interactive simulations.

### ECS?

The ECS pattern consists of three elements: an `entity` that represents a set of `component` data types at run time, and `system` logic which operates on sets of components.

#### ECS compared to OOP

An `entity` is analogous to an abstract `object` in [object-oriented programming](https://en.wikipedia.org/wiki/Object-oriented_programming).
The design approach, however, is inverted between ECS and OOP.

OOP encourages a top down design, where tasks are subdivided into simpler tasks to reduce complexity and share code.
Each object has a single concrete type that ultimately describes a specifically designed behaviour.

By contrast, entities are purely abstract, and can hold any set of component data at run time.
ECS design is bottom up and data driven, where behaviour emerges from composing the parameters of system logic.
This flat structure lets us focus on building naturally isolated, inherently reusable processes controlled by data.

### A closer look

The essence of ECS is declarative dispatch. System logic declares its component parameters, and is automatically run for every entity that matches.
This leaves you free to write composable processing that can be combined however you like at run time.

ECS emerged from the design flexibility needs of the game development industry, and naturally lends itself to dynamic simulations.
A simple physics simulation therefore makes a good starting place to demonstrate the pattern:

```nim
import polymorph

# Define the parameters of our simulation with components.
register defaultCompOpts:
  type
    Pos = object
      x, y: int
    Vel = object
      x, y: int
    Gravity = object
      amount: int

# Define system logic for components.

makeSystem "gravity", [Gravity, Vel]: all:
  vel.y += gravity.amount

makeSystem "move", [Pos, Vel]: all:
  pos.x += vel.x
  pos.y += vel.y

# Generate the ECS framework for use.
makeEcsCommit "run"

# Create an entity that satisfies the "move" system.
let moves = newEntityWith(Pos(x: 1, y: 1), Vel(x: 1, y: 2))

# Run participating systems (just "move") once.
run()

let pos = moves.fetch Pos # Get a reference to the Pos component.
echo pos                  # (x: 2, y: 3)

# Adding Gravity will satisfy the "gravity" system, as the entity already has Vel.
moves.add Gravity(amount: -1)

# Run the systems a few more times.
for i in 0..4:
  run()
echo pos                  # (x: 7, y: -2)
echo moves.fetch(Vel)     # (x: 1, y: -3)

# Without Vel, neither system will take part, and Pos is unchanged.
moves.remove Vel
for i in 0..4:
  run()
# The position is unchanged.
echo pos                  # (x: 7, y: -2)
```
