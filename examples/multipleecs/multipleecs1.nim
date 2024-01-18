# Module multipleecs1

import polymorph

var someData*: int

registerComponents defaultCompOpts:
  type Comp1* = object
    value*: int

makeSystem "test1", [Comp1]:
  all: item.comp1.value += 1

makeEcs()
commitSystems("run")

let e = newEntity(Comp1(value: 2))

run()
someData = e.fetch(Comp1).value
