import polymorph

template defineSay*(compOpts: EcsCompOptions, sysOpts: EcsSysOptions) {.dirty.} =
  # The {.dirty.} pragma passes the code through for outside access.

  registerComponents compOpts:
    type Say* = object
      text*: string

  makeSystemOpts "sayer", [Say], sysOpts:
    all: echo item.say.text
    finish: sys.remove Say
