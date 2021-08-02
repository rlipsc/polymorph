template testTransitionComponents*: untyped {.dirty.} =
  registerComponents(defaultCompOpts):
    type
      W = object
        val: int
      X = object
        val: int
      Y = object
        val: int
      Z = object
        val: string

  makeEcs()

  suite "Transitioning components":
    let
      compsXY = cl(X(val: 456), Y(val: 987))
      compsXZ = cl(X(val: 999), Z(val: "DEF"))
      compsYZ = cl(Y(val: 789), Z(val: "GHI"))
      
      entity = newEntityWith(W(val: 136), X(val: 123), Z(val: "ABC"))

    test "Setup":
      let
        w = entity.fetch W
        x = entity.fetch X
        z = entity.fetch Z
      
      check w.valid
      check x.valid
      check z.valid

      check w.val == 136
      check x.val == 123
      check z.val == "ABC"

    test "Transition XZ":
      entity.transition(compsXY, compsXZ)

      let
        w = entity.fetch W
        x = entity.fetch X
        z = entity.fetch Z
      
      check w.valid
      check x.valid
      check z.valid

      check not(entity.has Y)
      check w.val == 136
      check x.val == 999
      check z.val == "DEF"

    test "Transition XY (1)":
      entity.transition(compsXZ, compsXY)

      let
        w = entity.fetch W
        x = entity.fetch X
        y = entity.fetch Y
      
      check w.valid
      check x.valid
      check y.valid

      check not(entity.has Z)
      check w.val == 136
      check x.val == 456
      check y.val == 987

    test "Transition YZ":
      entity.transition(compsXY, compsYZ)
      
      let
        w = entity.fetch W
        y = entity.fetch Y
        z = entity.fetch Z
      
      check w.valid
      check y.valid
      check z.valid

      check not(entity.has X)
      check w.val == 136
      check y.val == 789
      check z.val == "GHI"

    test "Transition XY (2)":
      entity.transition(compsXZ, compsXY)

      let
        w = entity.fetch W
        x = entity.fetch X
        y = entity.fetch Y
      
      check w.valid
      check x.valid
      check y.valid

      check not(entity.has Z)
      check w.val == 136
      check x.val == 456
      check y.val == 987

when isMainModule:
  import polymorph, unittest
  
  static: defaultIdentity.set_private true

  block:
    testTransitionComponents()
