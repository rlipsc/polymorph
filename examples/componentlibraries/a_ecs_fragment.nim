## Here we're creating a component and system to be combined with other
## ECS designs.
## 
## The component and system is placed into a template so we can let the
## user decide compile options.

import polymorph

template defineSay*(compOpts: EcsCompOptions, sysOpts: EcsSysOptions) {.dirty.} =
  # The {.dirty.} pragma passes the code through for outside access.
  # Without this, the template would generate unique for everything.

  registerComponents compOpts:
    type Say* = object
      text*: string

  makeSystemOpts "sayer", [Say], sysOpts:
    all: echo item.say.text
    # Once finished, the component is removed from all entities in the system.
    finish: sys.remove Say
