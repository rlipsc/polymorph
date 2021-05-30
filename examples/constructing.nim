import polymorph

registerComponents(defaultCompOpts):
  type
    Comp1 = object
      value: int
    Comp2 = object
      value: string

makeEcs()

let
  entityBlueprint = cl(Comp1(value: 1234), Comp2(value: "Foo"))
  entity = entityBlueprint.construct

let
  c1 = entity.fetch Comp1
  c2 = entity.fetch Comp2

assert c1.value == 1234
assert c2.value == "Foo"
