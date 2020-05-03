import polymorph

const maxEnts* = 10_000

registerComponents(maxEnts):
  type
    Value* = object
      amount*: int
    IncValue* = object
    AddValue* = object
      amount*: int


