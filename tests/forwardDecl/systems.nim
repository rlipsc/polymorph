import polymorph, sysdefines, types

# We've already defined this system in `forwardDeclSysDefines` so it's type info is fixed.
makeSystemBody("incB"):
  all:
    item.b.value += 1

# Mixing with inline declared.
# `makeSystem` adds to systems and so must be invoked before `makeEcs`.
# If we were declaring all our systems with `defineSystem`,
# we could put the `makeEcs(maxEnts)` in that module.
makeSystem("incC", [IncComp, C]):
  all:
    item.c.value += 1

makeSystemBody("addBC"):
  all:
    item.b.value += item.c.value

makeEcs()

commitSystems("runAllSystems")
