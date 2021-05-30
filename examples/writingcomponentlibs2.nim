import polymorph, writingcomponentlibs1

const
  compOpts = defaultCompOpts
  sysOpts = defaultSysOpts

defineSay(compOpts, sysOpts)

registerComponents(compOpts):
  type
    SayEvery = object
      current, ticks: Natural

makeSystemOpts("echoTicks", [SayEvery], sysOpts):
  all:
    let say = item.sayEvery
    if say.current mod say.ticks == 0:
      entity.add Say(text: $say.current)
    say.current += 1

makeEcs()
commitSystems("run")

let entity = newEntityWith(SayEvery(ticks: 25))
for i in 0 ..< 100: run()
