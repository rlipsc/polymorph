import polymorph, unittest

template defineConstructAndClone*(entOpts: ECSEntityOptions, compOpts: ECSCompOptions, sysOpts: ECSSysOptions) {.dirty.} =
  registerComponents(compOpts):
    type
      A = object
        val: int
      B = object
        val: float
      C = object
        val: string
      ReplacedFrom = object
        val: int
      ReplacedTo = object
        val: int

  defineSystemOwner("test", [A, B], [A, B], sysOpts)
  defineSystem("test2", [A, B], sysOpts)
  defineSystem("test3", [ReplacedTo], sysOpts)

const
  entOpts = defaultEntityOptions
  sysOpts = defaultSystemOptions
  compOpts = defaultComponentOptions

defineConstructAndClone(entOpts, compOpts, sysOpts)
makeEcs(entOpts)
commitSystems("run")

proc runConstructAndClone*() =

  suite "Construction":
    test "Building from template":
      let templ: ConstructionTemplate = @[
        @[tmplA(val = 1), tmplB(val = 123.456), tmplC(val = "Hello")]
      ]

      let ent = templ.construct()
      check ent[0].hasComponent A
      check ent[0].hasComponent B
      check ent[0].hasComponent C
      let
        a = ent[0].fetchComponent A
        b = ent[0].fetchComponent B
        c = ent[0].fetchComponent C
      check:
        a.valid
        b.valid
        c.valid
      check:
        a.val == 1
        b.val == 123.456
        c.val == "Hello"
      check:
        sysTest.index.hasKey(ent[0].entityId)

    test "Incomplete owned system build":
      let templ: ConstructionTemplate = @[
        @[tmplA(val = 1), tmplC(val = "Hello")]
      ]
      expect Exception:
        let e = templ.construct
        echo e
    
  suite "Cloning":
    test "Check values":
      let
        ent = newEntityWith(A(val: 123), B(val: 123.456), C(val: "Hello"))
        cloned = ent.clone()
      check cloned.hasComponent A
      check cloned.hasComponent B
      check cloned.hasComponent C
      check cloned.fetchComponent(A).val == 123
      check cloned.fetchComponent(B).val == 123.456
      check cloned.fetchComponent(C).val == "Hello"

  suite "Events":

    registerConstructor ReplacedFrom, proc(entity: EntityRef, component: Component, master: EntityRef): seq[Component] =
      let rf = ReplacedFromRef(component).value
      result.add tmplReplacedTo(val = rf.val)

    registerPostConstructor ReplacedTo, proc(entity: EntityRef, component: ComponentRef, entities: var Entities) =
      let rf = ReplacedToInstance(component.index)
      rf.val *= 2

    test "Replace component during construction":

      let
        templ: ConstructionTemplate = @[
          @[tmplReplacedFrom(val = 123).Component]
        ]
        ents = templ.construct()

      check not ents[0].hasComponent(ReplacedFrom)
      check ents[0].hasComponent ReplacedTo
      check ents[0].fetchComponent(ReplacedTo).val == 123 * 2
      check ents[0] in sysTest3

    registerCloneConstructor ReplacedFrom, proc(entity: EntityRef, component: ComponentRef): seq[Component] =
      let rf = ReplacedFromInstance(component.index)
      result.add tmplReplacedTo(val = rf.val)

    test "Replace component during cloning":
      let
        ent = newEntityWith(A(val: 123), B(val: 123.456), C(val: "Hello"), ReplacedFrom(val: 123))
        cloned = ent.clone
      check cloned.hasComponent A
      check cloned.hasComponent B
      check cloned.hasComponent C
      check cloned.hasComponent ReplacedTo
      check cloned.fetchComponent(A).val == 123
      check cloned.fetchComponent(B).val == 123.456
      check cloned.fetchComponent(C).val == "Hello"
      check cloned.fetchComponent(ReplacedTo).val == 123
  
  flushGenLog()

when isMainModule:
  runConstructAndClone()