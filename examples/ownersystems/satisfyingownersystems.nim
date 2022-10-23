import polymorph

registerComponents defaultCompOpts:
  type
    A = object
    B = object
    C = object

defineSystemOwner "fullyOwned", [A, B, C], [A, B, C], defaultSysOpts

makeEcs()

when false:
  # Error: we're missing B and C to create a row in "fullyOwned".
  let e1 = newEntityWith(A())

when false:
  let e2 = newEntity()
  # Error: we're missing C.
  discard e2.add(A(), B())
