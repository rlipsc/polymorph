import polymorph, types, systems, sysdefines, unittest

export systems, types

proc runForwardDecl* =
  suite "Forward declarations":
    test "Test":
      const maxEnts = 4
      var ents = newSeq[EntityRef]()

      for i in 0 ..< maxEnts:
        let v = i + 1
        if i mod 2 == 0:
          ents.add newEntityWith(Value(amount: v), AddValue(amount: v))
        else:
          ents.add newEntityWith(AddValue(amount: v), IncValue())

      runAllSystems()
      
      for i, ent in ents:
        let
          value = ent.fetchComponent Value
          incValue = ent.fetchComponent IncValue
          addValue = ent.fetchComponent AddValue
        
        check addValue.valid

        if i mod 2 == 0:
          check:
            value.valid
            not incValue.valid
            value.amount == (i + 1) * 2 
        else:
          check:
            incValue.valid
            addValue.amount == (i + 1) + 1

when isMainModule:
  runForwardDecl()