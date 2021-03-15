import polymorph

const
  maxEnts* = 10_000

const id* = newEcsIdentity("fwdDecl")

id.registerComponents(ECSCompOptions()):
  type
    Value* = object
      amount*: int
    IncValue* = object
    AddValue* = object
      amount*: int


