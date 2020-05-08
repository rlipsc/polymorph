import polymorph

const
  maxEnts = 1_000_000
  entOpts = ECSEntityOptions(maxEntities: maxEnts, entityStorageFormat: esArray, componentStorageFormat: csArray, maxComponentsPerEnt: 4)
  sysOpts = ECSSysOptions(maxEntities: maxEnts, storageFormat: ssArray, indexFormat: sifArray)
  compOpts = ECSCompOptions(maxComponents: 4, componentStorageFormat: cisArray)

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
defineSystem(     "refToOwned", [A, B, C, D], sysOpts )
defineSystemOwner("partiallyOwned", [C, D, E, F], [E, F], sysOpts)
defineSystem(     "unowned", [G, H], sysOpts)

makeSystem("test", [A, B, C, D]):
  all:
    item.a.val += item.b.val
    item.c.val += item.d.val

makeEcs(entOpts)
commitSystems("run")

import unittest

var ents: seq[EntityRef]
suite "newEntityWith owned components":
  test "NewEntityWith correctness":
    for i in 0..10:
      let
        v = i + 1
        e = newEntityWith(A(val: v), B(val: v), C(val: v), D(val: v))
      ents.add e
      check e.hasComponent A
      check e.hasComponent B
      check e.hasComponent C
      check e.hasComponent D
      check e.fetchComponent(A).val == v
      check e.fetchComponent(B).val == v
      check e.fetchComponent(C).val == v
      check e.fetchComponent(D).val == v
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

flushGenLog()
run()
echo "Current entity count after tests: ", entityCount()
ents.deleteAll
for i in 0 ..< maxEnts:
  discard newEntityWith(A(val: 1), B(val: 2), C(val: 3), D(val: 4))

echo "Added"
import times, stats
var rs: RunningStat

let
  entTests = 100

for i in 0 ..< entTests:
  let s = cpuTime()
  run()
  let f = cpuTime()
  rs.push(f - s)

echo "Mean run time for ", maxEnts, " entities over ", entTests, " runs: ", rs.mean
echo "Run time variance: ", rs.variance
echo "Finish."
flushGenLog()
