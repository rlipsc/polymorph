import polymorph

const maxEnts* = 10_000

registerComponents(maxEnts):
  type
    IncComp* = object
    B* = object
      value*: int
    C* = object
      value*: int


