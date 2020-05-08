import polymorph, sysdefines, types

# We've already defined this system in `forwardDeclSysDefines` so it's type info is fixed.
makeSystem("incValue", [Value, IncValue]):
  all:
    item.value.amount += 1

# Define a system inline without defineSystem.
makeSystem("incAmount", [AddValue, IncValue]):
  all:
    item.addValue.amount += 1

makeSystem("addValue", [Value, AddValue]):
  all:
    item.value.amount += item.addValue.amount

makeEcs()

commitSystems("runAllSystems")
