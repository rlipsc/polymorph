import polymorph

template defineConstructAndClone*(entOpts: ECSEntityOptions, compOpts: ECSCompOptions, sysOpts: ECSSysOptions) {.dirty.} =
  registerComponents(compOpts):
    type
      AC = object
        val: int
      BC = object
        val: float
      CC = object
        val: string
      ReplacedFrom = object
        val: int
      ReplacedTo = object
        val: int

  defineSystem "test", [own AC, BC], sysOpts
  defineSystem "test2", [AC, own BC], sysOpts
  defineSystem "test3", [ReplacedTo], sysOpts

  makeSystem "test", [AC, BC]:
    discard

  const ccOpts = block:
    var curOpts = entOpts
    curOpts.runtimeConstructionHooks = true
    curOpts
    
  makeEcs(ccOpts)
  commitSystems("run")

  proc runConstructAndClone() =
    suite "Construction":
      test "Building from template":
        let templ: ConstructionTemplate = @[
          cl(AC(val: 1), BC(val: 123.456), CC(val: "Hello"))
        ]

        let
          ent = templ.construct()
          (ac, bc, cc) = ent[0].fetch(AC, BC, CC)
        check:
          ac.valid
          bc.valid
          cc.valid
        check:
          ac.alive
          bc.alive
          cc.alive
        check:
          ac.val == 1
          bc.val == 123.456
          cc.val == "Hello"
        check:
          ent[0] in sysTest
          ent[0] in sysTest2

      test "Incomplete owned system build":
        let templ: ConstructionTemplate = @[
          cl(AC(val: 1), CC(val: "Hello"))
        ]
        case entOpts.errors.errIncompleteOwned
          of erAssert:
            expect AssertionDefect:
              let e = templ.construct
              for x in e:
                echo x
          of erRaise:
            expect Exception:
              let e = templ.construct
              for x in e:
                echo x
      
    suite "Cloning":
      test "Check values":
        let
          ent = newEntityWith(AC(val: 123), BC(val: 123.456), CC(val: "Hello"))
          cloned = ent.clone()

        check cloned.fetch(AC).val == 123
        check cloned.fetch(BC).val == 123.456
        check cloned.fetch(CC).val == "Hello"
        check:
          cloned in sysTest
          cloned in sysTest2

    suite "Events":

      registerConstructor ReplacedFrom, proc(entity: EntityRef, component: Component, context: EntityRef): seq[Component] =
        let rf = ReplacedFromRef(component).value
        result.add ReplacedTo(val: rf.val)

      registerPostConstructor ReplacedTo, proc(entity: EntityRef, component: ComponentRef, entities: var Entities) =
        let rf = ReplacedToInstance(component.index)
        rf.val *= 2

      test "Replace component during construction":

        let
          templ: ConstructionTemplate = @[
            cl(ReplacedFrom(val: 123))
          ]
          ents = templ.construct()

        check not ents[0].hasComponent(ReplacedFrom)
        check ents[0].hasComponent ReplacedTo
        check ents[0].fetchComponent(ReplacedTo).val == 123 * 2
        check ents[0] in sysTest3

      # TODO: use CT events.
      registerCloneConstructor ReplacedFrom, proc(entity: EntityRef, component: ComponentRef): seq[Component] =
        let rf = ReplacedFromInstance(component.index)
        result.add ReplacedTo(val: rf.val)

      test "Replace component during cloning":
        let
          ent = newEntityWith(AC(val: 123), BC(val: 123.456), CC(val: "Hello"), ReplacedFrom(val: 123))
          cloned = ent.clone
        check cloned.hasComponent AC
        check cloned.hasComponent BC
        check cloned.hasComponent CC
        check cloned.hasComponent ReplacedTo
        check cloned.fetchComponent(AC).val == 123
        check cloned.fetchComponent(BC).val == 123.456
        check cloned.fetchComponent(CC).val == "Hello"
        check cloned.fetchComponent(ReplacedTo).val == 123
        check:
          cloned in sysTest
          cloned in sysTest2
          cloned in sysTest3
    flushGenLog()
  
when isMainModule:
  import unittest
  
  const
    entOpts = defaultEntityOptions
    sysOpts = defaultSystemOptions
    compOpts = defaultComponentOptions

  echo sysOpts
  from tables import hasKey

  # Test private generation.
  static:
    defaultIdentity.set_private true
  
  block:
    defineConstructAndClone(entOpts, compOpts, sysOpts)
    runConstructAndClone()
