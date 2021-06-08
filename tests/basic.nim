import polymorph

if not defined(debug): quit("Basic tests must be run in debug mode.")
template runBasic*(entOpts: ECSEntityOptions, compOpts: ECSCompOptions, sysOpts: ECSSysOptions) {.dirty.} =
  import unittest

  registerComponents(compOpts):
    type
      AddOne = object
      IntCont = object
        value: int
      FloatCont = object
        value: float
      DeleteSelf = object
      DeleteArb = object
      RemoveSelf = object
      RemDelSelf = object
      NoSystemsComp = object

  defineSystem("incInt", [AddOne, IntCont], sysOpts)

  makeSystem("incInt", [AddOne, IntCont]):
    all:
      item.intCont.value += 1

  makeSystemOpts("incFloat", [AddOne, FloatCont], sysOpts):
    all:
      item.floatCont.value += 1.0

  makeSystemOpts("delOwnEnt", [DeleteSelf], sysOpts):
    all:
      # Built in to efficiently delete currently executing row.
      deleteEntity()

  makeSystemOpts("arbDel", [DeleteArb], sysOpts):
    all:
      item.entity.delete

  makeSystemOpts("removeSelf", [RemoveSelf], sysOpts):
    all:
      item.entity.removeComponent RemoveSelf

  const testDeleteMods = [3, 7, 8]

  makeSystemOptFields("removeAndDeleteSelf", [RemDelSelf], sysOpts) do:
    counter: int
    deletedCount: int
    removedCount: int
  do:
    start:
      sys.counter = 0
      sys.deletedCount = 0
      sys.removedCount = 0
    all:
      if sys.counter mod testDeleteMods[0] == 0:
        sys.deletedCount.inc
        deleteEntity()
      elif sys.counter  mod testDeleteMods[1] == 0:
        sys.deletedCount.inc
        item.entity.delete
      elif sys.counter  mod testDeleteMods[2] == 0:
        sys.removedCount.inc
        item.entity.removeComponent RemDelSelf
      sys.counter.inc

  makeSystemOpts("multipleBlocks", [AddOne], sysOpts):
    init:
      let initActivated = true
    init: check initActivated == true
    start:
      type State = enum None, Started, Finished
      var
        number: int
        state: State
    start:
      state = Started
      let
        a = 1
        b = 2
        c = 3
        d = 4
    all: number += a
    all: number += b
    stream 2: number += c
    stream 2: number += d
    finish:
      check state == Started
      state = Finished
    finish: check number == (sys.count * (a + b)) + (2 * (c + d))
    finish: check state == Finished

  makeEcs(entOpts)
  commitSystems("run")

  template fetchCheck(ent: EntityRef, compType, actions: untyped): untyped =
    ## Performs basic checks that should pass when expecting a component,
    ## and passes value to `actions`.
    block:
      check ent.hasComponent compType
      let comp {.inject.} = ent.fetchComponent compType
      check comp.valid
      actions

  proc runBasicTests() =
    suite "Basic operations":
      var ents = newSeq[EntityRef]()
      
      test "New entity":
        let e = newEntity()
        check e.alive
        ents.add e    

      test "Add component":
        let
          e = newEntity()
          lastCount = sysIncInt.count
        ents.add e
        e.addComponent IntCont(value: 5)
        check sysIncInt.count == lastCount

        e.fetchCheck IntCont:
          check comp.value == 5

        e.addComponent AddOne()
        check sysIncInt.count == lastCount + 1

      test "New entity with component":
        var e = newEntityWith(FloatCont(value: 12.34), AddOne(), IntCont(value: 12))

        e.fetchCheck IntCont:
          check comp.value == 12

        e.fetchCheck FloatCont:
          check comp.value == 12.34

        e.fetchCheck AddOne:
          discard

      test "Add components":
        var e = newEntity()
        let
          comps = e.addComponents( IntCont(value: 12), FloatCont(value: 12.34) )
          intCont = comps.intCont
        
        e.fetchCheck IntCont:
          check comp.value == 12
          check comp == intCont

        e.fetchCheck FloatCont:
          check comp.value == 12.34

      test "Remove component":
        block:
          # One component on a one component system.
          let
            e = newEntity()
            lastCount = sysDelOwnEnt.count
          ents.add e
          e.addComponent DeleteSelf()
          check sysDelOwnEnt.count == lastCount + 1

          e.removeComponent DeleteSelf
          check not e.hasComponent DeleteSelf

          check e.componentCount == 0
          check sysDelOwnEnt.count == lastCount
          e.delete
        block:
          # Two components.
          let
            e = newEntity()
            lastCount = sysIncInt.count
          ents.add e
          
          e.addComponent IntCont()
          check sysIncInt.count == lastCount
          e.addComponent AddOne()
          check sysIncInt.count == lastCount + 1
          
          e.removeComponent IntCont
          check not e.hasComponent IntCont
          check e.componentCount == 1
          check sysIncInt.count == lastCount
          e.delete

      test "Delete":
        # No need to update ents for transient entity.
        let e = newEntity()
        e.addComponent IntCont()
        e.delete
        check not e.alive

      test "Delete row":
        # Delete the current row within a system body.
        const tests = 20
        var delEnts = newSeq[EntityRef](tests)
        for i in 0 ..< tests:
          delEnts[i] = newEntityWith(AddOne(), IntCont(), FloatCont(), DeleteSelf())
          check delEnts[i].alive
        
        doDelOwnEnt()

        for i in 0 ..< tests:
          check not delEnts[i].alive

      test "Delete arbitrary row within system":
        # Delete an arbitrary row within a system body.
        # Despite doing the same work as the delete row test,
        # because we use `delete`, which isn't constrained at compile time,
        # the system must invoke a length check in iteration.
        const tests = 20
        var delEnts = newSeq[EntityRef](tests)
        for i in 0 ..< tests:
          delEnts[i] = newEntityWith(AddOne(), IntCont(), FloatCont(), DeleteArb())
          check delEnts[i].alive
        
        doArbDel()

        for i in 0 ..< tests:
          check not delEnts[i].alive

      test "Remove own component within system":
        const tests = 20
        var remEnts = newSeq[EntityRef](tests)
        for i in 0 ..< tests:
          remEnts[i] = newEntityWith(AddOne(), IntCont(), FloatCont(), RemoveSelf())
          check remEnts[i].alive
        
        doRemoveSelf()
        check sysRemoveSelf.count == 0
        remEnts.deleteAll

      test "Remove component with no systems":
        var ent = newEntityWith(NoSystemsComp())
        check ent.hasComponent NoSystemsComp
        ent.removeComponent NoSystemsComp
        check not(ent.hasComponent NoSystemsComp)

      test "Check some system values":
        let e = newEntity()
        ents.add e
        let comps = e.addComponents( IntCont(value: 1), AddOne(), FloatCont(value: 1.0) )
        run()
        e.fetchCheck IntCont:
          check comp.value == 2
          check comps.intCont == comp
        e.fetchCheck FloatCont:
          check comp.value == 2.0
          check comps.floatCont == comp

      test "Delete list":
        ents.deleteAll

      test "Add component that already exists":
        let e = newEntityWith(IntCont())
        expect Exception:
          e.addComponent IntCont()
        expect Exception:
          discard e.addComponents IntCont()

      proc expectedIterations(c: int): int =
        ## Models the number of iterations a system takes
        ## to delete all its entities, given a set pattern.
        var
          x = c
          i: int
        while x > 0:
          for modValue in testDeleteMods[0..1]:
            if i mod modValue == 0:
              x -= 1
              break
          if x == 0:
            return
          result.inc
          i.inc
      
      proc expectedDeletes(c: int): int =
        ## Models how many deletes may occur in a set of iterations.
        for i in 0 ..< c:
          for modValue in testDeleteMods[0..1]:
            if i mod modValue == 0:
              result.inc
              break

      proc expectedRemoves(c: int): int =
        ## Models how many removes may occur in a set of iterations.
        for i in 0 ..< c:
          if i mod testDeleteMods[0] != 0 and i mod testDeleteMods[1] != 0 and
            i mod testDeleteMods[2] == 0:
              result.inc

      proc countAlive(ents: seq[EntityRef]): int =
        for ent in ents:
          if ent.alive: result.inc

      test "Deleting the current row or removing the component":
        var ents: seq[EntityRef]
        let entCount = 10
        var
          sysEnts = entCount
          expEnts = entCount
        for i in 0 ..< entCount:
          ents.add newEntityWith(RemDelSelf(), IntCont())

        let
          iters = expectedIterations(entCount)
          wholeIters = iters div entCount
        var cumIter: int

        for i in 0 ..< wholeIters:
          
          doRemoveAndDeleteSelf()

          check sysRemoveAndDeleteSelf.counter == sysEnts
          
          let
            expRemoves = expectedRemoves(sysEnts)
            expDeletes = expectedDeletes(sysEnts)
          expEnts -= expDeletes
          sysEnts -= expDeletes + expRemoves
          cumIter += sysRemoveAndDeleteSelf.counter

          check ents.countAlive == expEnts
          check sysRemoveAndDeleteSelf.count == sysEnts
          check sysRemoveAndDeleteSelf.deletedCount == expDeletes          
          check sysRemoveAndDeleteSelf.removedCount == expRemoves

        # Remaining iteration
        cumIter += sysRemoveAndDeleteSelf.counter
        doRemoveAndDeleteSelf()
        check sysRemoveAndDeleteSelf.counter == sysEnts
        let
          expRemoves = expectedRemoves(sysEnts)
          expDeletes = expectedDeletes(sysEnts)
        expEnts -= expDeletes
        sysEnts -= expDeletes + expRemoves

        check ents.countAlive == expEnts
        check sysRemoveAndDeleteSelf.count == sysEnts
        check sysRemoveAndDeleteSelf.deletedCount == expDeletes
        check sysRemoveAndDeleteSelf.removedCount == expRemoves
        ents.deleteAll
  
  flushGenLog()
  runBasicTests()

when isMainModule:
  const
    maxEnts = 10_000
    compOpts = ECSCompOptions(maxComponents: maxEnts)
    entOpts = ECSEntityOptions(maxEntities: maxEnts, componentStorageFormat: csSeq)
    sysOpts = ECSSysOptions(maxEntities: maxEnts)

  runBasic(entOpts, compOpts, sysOpts)
