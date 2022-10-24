import polymorph

registerComponents defaultCompOpts:
  type
    Original = object
      data: int
    Replaced = object
      data: int

makeEcs()

# Event to replace the Original type with Replaced.
# With this event, constructed entities will never contain the
# 'Original' component, but it can still be added to the entity after
# construction.

# This event will replace the `Original` component type with `Replaced`
# during construction.
proc replaceOriginal(entity: EntityRef, component: Component, context: EntityRef): seq[Component] =
  let original = OriginalRef(component).value
  result.add Replaced(data: original.data)

registerConstructor Original, replaceOriginal

# Build an entity from a component list using the `Original` type.
let entity = Original(data: 1234).cl.construct

let
  o = Original(data: 1234)

  # Build an entity using a ComponentList.
  e = cl(o).construct
  r = e.fetch Replaced

assert not e.has(Original)
assert r.valid
assert r.data == o.data

# Constructing multiple entity events.

proc countEntities(entity: EntityRef, component: ComponentRef, entities: var Entities) =
  echo "Construction has a 'Replaced', built: ", entities.len, " entities"

registerPostConstructor Replaced, countEntities

let entities = @[cl(o)].construct

# Cloning event.

proc displayCloning(entity: EntityRef, component: ComponentRef): seq[Component] =
  let instance = ReplacedInstance(component.index)
  echo "We're cloning 'Replaced': ", instance
  # Create a copy of the original component.
  result.add instance.makeContainer

registerCloneConstructor Replaced, displayCloning

let
  cloned = e.clone
  r2 = cloned.fetch Replaced

assert not cloned.has(Original)
assert r2.valid
assert r2.data == o.data

