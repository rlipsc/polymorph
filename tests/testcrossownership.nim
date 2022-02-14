template defineCrossOwnership*() {.dirty.} =
  when not declared(ECSEntityOptions):
    {.fatal: "This test requires importing polymorph".}

  const
    maxEnts = 25
    entOpts = ECSEntityOptions(maxEntities: maxEnts,
      entityStorageFormat: esSeq,
      componentStorageFormat: csArray,
      maxComponentsPerEnt: 20)
    sysOpts = ECSSysOptions(maxEntities: maxEnts,
      storageFormat: ssSeq,
      indexFormat: sifArray)
    compOpts = ECSCompOptions(maxComponents: maxEnts,
      componentStorageFormat: cisSeq)

  registerComponents(compOpts):
    type
      AX = object
        val: int
      BX = object
        val: int
      CX = object
        val: int
      DX = object
        val: int

      EX = object
        val: int
      FX = object
        val: int

      GX = object
        val: int
      HX = object
        val: int

      IX = object
        val: int
      JX = object
        val: int

      KX = object  # Unowned.
        val: int
      LX = object  # Unowned.
        val: int

  defineSystem("sys1", [own AX, own BX, own CX, own DX], sysOpts)
  defineSystem("sys2", [CX, DX, own EX, own FX], sysOpts)         # Dep sys1
  defineSystem("sys3", [EX, FX, own GX, own HX], sysOpts)         # Dep sys2 -> sys1
  defineSystem("sys4", [AX, EX, GX, HX, own IX, own JX], sysOpts) # Dep sys1, sys2, sys3
  defineSystem("sys5", [AX, EX, GX, HX, IX, JX, KX, LX], sysOpts) # Dep sys1, sys2, sys3, sys4, sys6
  defineSystem("sys6", [AX, KX, LX], sysOpts)                     # Dep sys1

  makeEcs(entOpts)
  commitSystems("run")

  suite "Chaining ownership":
      
    test "Add/remove state dependencies followed":
      let e1 = newEntityWith(AX(), BX(), CX(), DX(), EX(), FX(), GX(), HX(), IX(), JX(), KX(), LX())
      check:
        e1 in sysSys1
        e1 in sysSys2
        e1 in sysSys3
        e1 in sysSys4
      check:
        e1.hasComponent AX
        e1.hasComponent BX
        e1.hasComponent CX
        e1.hasComponent DX
        e1.hasComponent EX
        e1.hasComponent FX
        e1.hasComponent GX
        e1.hasComponent HX
        e1.hasComponent IX
        e1.hasComponent JX
        e1.hasComponent KX
        e1.hasComponent LX

      e1.removeComponent DX

      check:
        e1 notin sysSys1
        e1 notin sysSys2
        e1 notin sysSys3
        e1 notin sysSys4
        e1 notin sysSys5
        e1 notin sysSys6
        e1.componentCount == 2
        KX in e1
        LX in e1
