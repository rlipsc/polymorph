import polymorph


template testNegation* {.dirty.} =

  const
    cOpts = defaultCompOpts
    sOpts = defaultSysOpts
    eOpts = EcsEntityOptions(strDefault: sdHideData)

  registerComponents(cOpts):
    type
      Req1 = object
      Req2 = object

      Neg1 = object
      Neg2 = object
      Neg3 = object

  # Ensure prototyping works with negation.
  defineSystem "neg1", [Req1, Req2, not Neg1], sOpts

  type
    Event = object
      kind: EventKind
      v1: int
      v2: ComponentTypeId

  onEcsBuilt:
    proc `$`(e: Event): string =
      result = $e.kind
      case e.kind
        of ekInit .. ekDeleteComp:
          caseComponent e.v1.ComponentTypeId:
            result &= " " & $componentType()
        of ekSystemAddAny .. ekSystemRemoveAny, ekRowAdded .. ekRowRemovedCb:
          caseSystem e.v1.SystemIndex:
            result &= " System: " & sys.name
        of ekCompAddTo, ekCompRemoveFrom:
          caseSystem e.v1.SystemIndex:
            result &= " System: " & sys.name
          caseComponent e.v2:
            result &= " " & $componentType()
        else:
          discard
      result &= "\n"

  var
    # TODO: check events.
    events: seq[Event]


  makeSystem "neg1", [Req1, Req2, not Neg1]:
    added: events.add Event(kind: ekRowAdded, v1: sys.id.int)
    removed: events.add Event(kind: ekRowRemoved, v1: sys.id.int)
    addedCallback: events.add Event(kind: ekRowAddedCb, v1: sys.id.int)
    removedCallback: events.add Event(kind: ekRowRemovedCb, v1: sys.id.int)
    all:
      entity.delete

  Req1.onAdd: events.add Event(kind: ekAdd, v1: curComponent.typeId.int)
  Req1.onAddCallback: events.add Event(kind: ekAddCb, v1: curComponent.typeId.int)
  Req1.onSystemAdd: events.add Event(kind: ekSystemAddAny, v1: sys.id.int, v2: curComponent.typeId)
  Req1.onSystemAddTo "neg1": events.add Event(kind: ekCompAddTo, v1: sys.id.int, v2: curComponent.typeId)

  makeSystemOpts "neg2", [Req1, Req2, not Neg1, not Neg2], sOpts:
    discard

  makeSystemOpts "neg3", [Req1, Req2, not Neg1, not Neg2, not Neg3], sOpts:
    discard

  makeSystemOpts "neg4", [Req1, Req2, not Neg1, not Neg2, not Neg3], sOpts:
    discard

  makeEcsCommit("runNegSystems", eOpts)

  suite "Component negation":
    test "newEntityWith: no negations":
      let
        noNeg = newEntityWith(Req1(), Req2())
      
      check noNeg in sysNeg1
      check noNeg in sysNeg2
      check noNeg in sysNeg3

    test "newEntityWith: one negation":
      let
        neg1 = newEntityWith(Req1(), Req2(), Neg2())

      check neg1 in sysNeg1
      check neg1 notin sysNeg2
      check neg1 notin sysNeg3

      neg1.delete

      check neg1 notin sysNeg1
      check neg1 notin sysNeg2
      check neg1 notin sysNeg3

    test "newEntityWith: two negations":
      let
        neg2 = newEntityWith(Req1(), Req2(), Neg3())
      
      check neg2 in sysNeg1
      check neg2 in sysNeg2
      check neg2 notin sysNeg3

      neg2.remove Neg3

      check neg2 in sysNeg1
      check neg2 in sysNeg2
      check neg2 in sysNeg3

      neg2.delete

      check neg2 notin sysNeg1
      check neg2 notin sysNeg2
      check neg2 notin sysNeg3

    test "newEntityWith: three negations":
      let
        neg3 = newEntityWith(Req1(), Req2(), Neg1(), Neg2(), Neg3())
      
      check neg3 notin sysNeg1
      check neg3 notin sysNeg2
      check neg3 notin sysNeg3

      neg3.delete

      check neg3 notin sysNeg1
      check neg3 notin sysNeg2
      check neg3 notin sysNeg3

    test "Remove":
      test "Removing negated components":
        let
          neg1 = newEntityWith(Req1(), Req2(), Neg2())

        neg1.remove Neg2

        check neg1 in sysNeg1
        check neg1 in sysNeg2
        check neg1 in sysNeg3

    test "Add":
      test "Adding negated components":

        let
          negAdding = newEntityWith(Req1(), Req2())
        
        negAdding.add Neg3()

        check negAdding in sysNeg1
        check negAdding in sysNeg2
        check negAdding notin sysNeg3

        negAdding.add Neg2()

        check negAdding in sysNeg1
        check negAdding notin sysNeg2
        check negAdding notin sysNeg3

        negAdding.add Neg1()

        check negAdding notin sysNeg1
        check negAdding notin sysNeg2
        check negAdding notin sysNeg3

when isMainModule:
  import unittest
  testNegation()
