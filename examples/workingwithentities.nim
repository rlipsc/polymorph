import polymorph

registerComponents(defaultCompOpts):
  type
    Comp1* = object
      value*: int
    Comp2* = object
      value*: string
    Comp3* = object
      value*: float

makeSystem "mySystem", [Comp1, Comp2]:
  all: discard

makeEcs()

let
  # An entity without any components.
  entity = newEntity()
  
  # Create an entity and include it in "mySystem".
  entWithComps = newEntityWith(
    Comp1(value: 123),
    Comp2(value: "Foo"))

discard entity.add(Comp1(value: 456), Comp2(value: "Bar"))
when false:
  # Adding the same component again fails - this component already exists!
  entity.add(Comp1(value: 789))

entity.remove Comp1, Comp2
entity.delete
