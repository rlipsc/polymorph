# Module multipleecs2

import polymorph

var someData*: int

registerComponents defaultCompOpts:
  type Comp2 = object
    value: int

makeSystem "test2", [Comp2]:
  all: item.comp2.value += 2

makeEcs()
commitSystems("run")

let e = newEntityWith(Comp2(value: 2))

run()
someData = e.fetch(Comp2).value
