## Here's a motivating example for a simple library system that works
## on other components.
## 
## The included template creates a system to clamp x and y fields for a
## parameter component, letting us specify different values as required.

import polymorph

template makeClampXYSystem*(component: typedesc, clampRange: Slice[float]) {.dirty.} =
  makeSystem "clamp" & $component, [comp: component]:
    all:
      if comp.x < clampRange.a:
        comp.x = clampRange.a
      elif comp.x > clampRange.b:
        comp.x = clampRange.b
      if comp.y < clampRange.a:
        comp.y = clampRange.a
      elif comp.y > clampRange.b:
        comp.y = clampRange.b


register defaultCompOpts:
  type Pos = object
    x, y: float
  type Scale = object
    x, y: float

# Ensure Scale is within a normalised range.
makeClampXYSystem(Scale, 0.0 .. 1.0)

var posRange = -10.0 .. 10.0

# This time we're passing a variable to the system so we can change the
# clamp range.
#
# This needs to be visible from where the system is output (i.e., where
# 'makeEcsCommit' is run).
#
# An alternative might be to add a field to the system to store the range.
makeClampXYSystem(Pos, posRange)

makeEcsCommit "run"

let e = newEntityWith(
  Pos(x: 10.5, y: -20.0),
  Scale(x: 100.0, y: 0.5)
)

# Run the systems to clamp the values.
run()

assert e.fetch(Pos).access == Pos(x: 10.0, y: -10.0)
assert e.fetch(Scale).access == Scale(x: 1.0, y: 0.5)

# Reduce the Pos clamp area and run again.
posRange = -5.0 .. 5.0
run()
assert e.fetch(Pos).access == Pos(x: 5.0, y: -5.0)
assert e.fetch(Scale).access == Scale(x: 1.0, y: 0.5)

