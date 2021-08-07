import polymorph, writingcomponentlibs1

const
  compOpts = defaultCompOpts
  sysOpts = defaultSysOpts

# Include the `Say` component and system.
defineSay(compOpts, sysOpts)

# Add our own component and system.

registerComponents compOpts:
  type
    SayOnTick = object
      current, ticks: Natural

makeSystemOpts "sayTicks", [SayOnTick], sysOpts:
  all:
    let sot = item.sayOnTick
    if sot.current mod sot.ticks == 0:
      entity.add Say(text: $sot.current)
    sot.current += 1

# Seal and generate.

makeEcs()
commitSystems "run"

# Try out the ECS.

let entity = newEntityWith(SayOnTick(ticks: 25))
for i in 0 ..< 100: run()
