## Here we use `ecsImport` to create an import to module_1 prefixed with
## the current path.

import polymorph

register defaultCompOpts:
  type
    Bar* = object
      value*: int

# Record import relative to our current path
# to be output later by 'makeEcs'.
ecsImport module_1

# This system uses our 'bar' variable from subdir/module_1.
makeSystem "bar", [Bar]:
  all: echo foo + bar.value
