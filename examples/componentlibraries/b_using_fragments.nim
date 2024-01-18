## Here we're importing the 'Say' component and adding a component and
## system to extend it.

import polymorph, a_ecs_fragment

const
  compOpts = defaultCompOpts
  sysOpts = defaultSysOpts

# Include the `Say` component and system using our options.
defineSay(compOpts, sysOpts)

# Add our own component and system.

registerComponents compOpts:
  type
    SayOnTick = object
      current, ticks: Natural

makeSystemOpts "sayTicks", [SayOnTick], sysOpts:
  all:
    # Add Say to entities at tick intervals.
    # The 'Say' component acts like a job and is removed after its system
    # has run.
    let sot = item.sayOnTick
    if sot.current mod sot.ticks == 0:
      entity.add Say(text: $sot.current)
    sot.current += 1

# Seal and generate.

makeEcsCommit "run"

# Try out the ECS.

let entity = newEntity(SayOnTick(ticks: 25))
for i in 0 ..< 100: run()
