import polymorph, sysdefines, types

# We've already defined this system in `forwardDeclSysDefines` so its type info is fixed.
id.makeSystem("incValue", [Value, IncValue]):
  all:
    item.value.amount += 1

# Define a system inline without defineSystem.
id.makeSystem("incAmount", [AddValue, IncValue]):
  all:
    item.addValue.amount += 1

id.makeSystem("addValue", [Value, AddValue]):
  all:
    item.value.amount += item.addValue.amount

id.makeEcs(ECSEntityOptions())

id.commitSystems("runAllSystems")
