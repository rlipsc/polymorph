import polymorph

const
  maxEnts = 25
  entOpts = ECSEntityOptions(maxEntities: maxEnts,
    entityStorageFormat: esSeq,
    componentStorageFormat: csArray,
    maxComponentsPerEnt: 10)
  sysOpts = ECSSysOptions(maxEntities: maxEnts,
    storageFormat: ssSeq,
    indexFormat: sifArray)
  compOpts = ECSCompOptions(maxComponents: maxEnts,
    componentStorageFormat: cisSeq)

registerComponents(compOpts):
  type
    A = object
      val: int
    B = object
      val: int
    C = object
      val: int
    D = object
      val: int

    E = object
      val: int
    F = object
      val: int

    G = object
      val: int
    H = object
      val: int

defineSystemOwner("fullyOwned", [A, B, C, D], [A, B, C, D], sysOpts)
# Both these systems rely on `fullyOwned` to exist as the holder
# of components [A, B, C, D].
defineSystem(     "refToOwned1", [A, B, C, D], sysOpts )
defineSystem(     "refToOwned2", [A, B], sysOpts )
# Combining ownership.
# This system overlaps owned [C, D] from `fullyOwned` with its own
# owned components.
defineSystemOwner("partiallyOwned", [C, D, E, F], [E, F], sysOpts)
# Standard system for balance.
defineSystem(     "unowned", [G, H], sysOpts)
# Inline define of another system that relies on `fullyOwned`.
makeSystem("refToOwned3", [A, B, C, D]):
  all:
    item.a.val += item.b.val
    item.c.val += item.d.val

makeEcs(entOpts)
commitSystems("run")

import unittest

proc doTests =
  var ents: seq[EntityRef]
  suite "newEntityWith owned components":
    test "NewEntityWith correctness":
      for i in 1..11:
        let e = newEntityWith(
          A(val: i), B(val: i), C(val: i), D(val: i), E(val: i), F(val: i), G(val: i), H(val: i))
        ents.add e
        check e.hasComponent A
        check e.hasComponent B
        check e.hasComponent C
        check e.hasComponent D
        check e.fetchComponent(A).val == i
        check e.fetchComponent(B).val == i
        check e.fetchComponent(C).val == i
        check e.fetchComponent(D).val == i
        check e.fetchComponent(E).val == i
        check e.fetchComponent(F).val == i
        check e.fetchComponent(G).val == i
        check e.fetchComponent(H).val == i
        check e in sysFullyOwned
        check e in sysRefToOwned1
        check e in sysRefToOwned2
        check e in sysPartiallyOwned
        check e in sysUnowned
        check e in sysRefToOwned3

        e.removeComponent A

        check e notin sysFullyOwned
        check e notin sysRefToOwned1
        check e notin sysRefToOwned2
        check e notin sysPartiallyOwned
        # The unowned system is independent to component A.
        check e in sysUnowned
        check e notin sysRefToOwned3
    test "Incomplete newEntityWith missing A":
      check not(compiles( newEntityWith(B(), C(), D()) ))
    test "Incomplete newEntityWith missing B":
      check not(compiles( newEntityWith(A(), C(), D()) ))
    test "Incomplete newEntityWith missing C":
      check not(compiles( newEntityWith(A(), B(), D()) ))
    test "Incomplete newEntityWith missing D":
      check not(compiles( newEntityWith(A(), B(), C()) ))
  suite "Incomplete owned components":
    test "Add components fully owned A":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(A()) ))
    test "Add components fully owned B":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(B()) ))
    test "Add components fully owned C":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(C()) ))
    test "Add components fully owned D":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(D()) ))
    test "Add components fully owned A B":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(A()) B() ))
    test "Add components fully owned B C":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(B(), C()) ))
    test "Add components fully owned C D":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(C(), D()) ))
    test "Add components fully owned A B C":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(A(), B(), C()) ))
    test "Add components fully owned B C D":
      let e = newEntity()
      ents.add e
      check not(compiles( e.addComponents(B(), C(), D()) ))
  suite "Adding to owned systems":
    let e = newEntity()
    ents.add e
    test "Add components correctness":
      let
        r = e.addComponents(A(val: 1), B(val: 2), C(val: 3), D(val: 4))
      check:
        r.a.valid
        r.a.access.val == 1
      check:
        r.b.valid
        r.b.access.val == 2
      check:
        r.c.valid
        r.c.access.val == 3
      check:
        r.d.valid
        r.d.access.val == 4
    test "Add/Remove with interdependent owner systems":
      ents.deleteAll
      let m = maxEnts
      for i in 0 ..< m:
        ents.add newEntityWith(A(val: 1), B(val: 2), C(val: 3), D(val: 4), E(val: 5), F(val: 6))
      
      echo analyseSystem(sysFullyOwned)
      echo analyseSystem(sysPartiallyOwned)

      # Removing the owned component `D` invalidates the storage
      # row in the owner system `sysFullyOwned`, and so components
      # A, B, and C are also invalid and removed.
      # The system `sysPartiallyOwned` owns E and F but also uses
      # indirections to C and D which are now invalid, invalidating
      # this system too, and removing E and F.
      # This leaves the entity with no components.
      for i in 0 ..< ents.len:
        check ents[i].alive
        ents[i].removeComponent D
        check ents[i].componentCount == 0
      check sysfullyOwned.count == 0
      check sysrefToOwned1.count == 0
      
      check sysrefToOwned2.count == 0
      check syspartiallyOwned.count == 0
      check sysunowned.count == 0
      check sysRefToOwned3.count == 0
      for ent in ents:
        check ent.alive
        check not ent.hasComponent(A)
        check not ent.hasComponent(B)
        check not ent.hasComponent(C)
        check not ent.hasComponent(D)
        check not ent.hasComponent(E)
        check not ent.hasComponent(F)
      ents.deleteAll
      check entityCount() == 0

flushGenLog()

doTests()
run()

