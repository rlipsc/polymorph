import polymorph, types, systems, sysdefines, unittest

export systems, types

proc runForwardDecl* =
  suite "Forward declarations":
    test "Test":
      const maxEnts = 4
      var ents = newSeq[EntityRef]()

      for i in 0 ..< maxEnts div 2:
        ents.add newEntityWith(IncComp(), B())
        ents.add newEntityWith(IncComp(), B(), C())

      runAllSystems()
      
      for i, ent in ents:
        let
          incComp = ent.fetchComponent IncComp
          b = ent.fetchComponent B
          c = ent.fetchComponent C
        
        check incComp.valid

        if i mod 2 == 0:
          check:
            b.valid
            not c.valid
        else:
          check:
            b.valid
            c.valid
            b.value == 2
            c.value == 1
