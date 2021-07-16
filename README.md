- [Project overview](#project-overview)
  - [ECS](#ecs)
  - [Polymorph](#polymorph)
  - [Project goals](#project-goals)
  - [Example code](#example-code)
  - [Compile time focus](#compile-time-focus)
    - [Characteristics](#characteristics)
  - [Why Nim?](#why-nim)
  - [Polymers companion library](#polymers-companion-library)
- [Overview of building an ECS](#overview-of-building-an-ecs)
  - [After `makeEcs`](#after-makeecs)
- [Defining components](#defining-components)
  - [`registerComponents` generated types](#registercomponents-generated-types)
  - [Components and instance types](#components-and-instance-types)
  - [Instances in components](#instances-in-components)
- [Defining systems](#defining-systems)
  - [Providing custom system fields](#providing-custom-system-fields)
  - [Committing systems](#committing-systems)
  - [System execution order](#system-execution-order)
  - [Grouping systems](#grouping-systems)
  - [System anatomy](#system-anatomy)
    - [System scope blocks](#system-scope-blocks)
    - [Work item blocks](#work-item-blocks)
    - [Removing components during `all` or `stream` blocks](#removing-components-during-all-or-stream-blocks)
  - [System utilities](#system-utilities)
    - [`clear`](#clear)
    - [`remove`/`removeComponents`](#removeremovecomponents)
- [Working with entities](#working-with-entities)
  - [Creating entities](#creating-entities)
  - [Adding components to existing entities](#adding-components-to-existing-entities)
  - [Fetching and checking for components](#fetching-and-checking-for-components)
  - [Removing components](#removing-components)
  - [Deleting entities](#deleting-entities)
- [Constructing entities at run time](#constructing-entities-at-run-time)
  - [Constructing multiple entities](#constructing-multiple-entities)
  - [Cloning entities](#cloning-entities)
- [Switching components with component lists](#switching-components-with-component-lists)
- [Events](#events)
  - [Inline events](#inline-events)
  - [System events](#system-events)
    - [Mutating entities within inline system events](#mutating-entities-within-inline-system-events)
  - [Change events](#change-events)
  - [`onEcsBuilt`](#onecsbuilt)
  - [Construction and clone events](#construction-and-clone-events)
    - [Construction events](#construction-events)
- [Writing component libraries](#writing-component-libraries)
- [Code generation options](#code-generation-options)
  - [EcsCompOptions](#ecscompoptions)
    - [EcsCompOptions fields](#ecscompoptions-fields)
  - [EcsSysOptions](#ecssysoptions)
    - [EcsSysOptions fields](#ecssysoptions-fields)
  - [EcsEntityOptions](#ecsentityoptions)
    - [EcsEntityOptions fields](#ecsentityoptions-fields)
- [Compile switches](#compile-switches)
- [Performance considerations](#performance-considerations)
  - [Memory access patterns](#memory-access-patterns)
  - [Fragmentation analysis](#fragmentation-analysis)
- [Owned components and removing indirection](#owned-components-and-removing-indirection)
  - [Satisfying owner systems](#satisfying-owner-systems)
  - [Deleting from owner systems](#deleting-from-owner-systems)
- [ECS identities](#ecs-identities)
  - [Multiple ECS outputs with a single identity](#multiple-ecs-outputs-with-a-single-identity)
  - [Multiple identities](#multiple-identities)
- [Future work](#future-work)
  - [Performance](#performance)
  - [Utility](#utility)


# Project overview

This library provides a lean abstraction for writing programs with the [entity-component-system](https://en.wikipedia.org/wiki/Entity_component_system) pattern (or ECS for short).

## ECS

This pattern lets you create sets of data types at run time and dispatch code by type combinations.

  - A set of types is an **entity**.
  - A data type is a **component**.
  - A **system** is program logic running over components.

Advantages of using ECS include: design agility, run time composition, [the principles](https://www.sebaslab.com/the-quest-for-maintainable-code-and-the-path-to-ecs/) of [SOLID](https://en.wikipedia.org/wiki/SOLID), naturally decoupled and reusable systems, and high performance through cache friendly batch processing.

## Polymorph

This library takes a generative, system oriented approach to ECS.

Systems store execution state and require no work to iterate. Adding or removing components outputs code to directly update the compile time inferred systems. ECS functionality is generated from the relationship between systems and components.

The output is statically dispatched with a sequential flow, optimised to each system/component design.

## Project goals

- Manage complexity with declarative dispatch and run time composition.
- Scalable, low boilerplate platform for composing data oriented designs.
- No runtime, zero system iteration overhead.
- Leverage static typing and metaprogramming to elide run time work.
- No external dependencies.

## Example code

```nim
import polymorph

# Create some components.
registerComponents(defaultCompOpts):
  type
    Position = object
      x, y: int
    Velocity = object
      x, y: int

# Define logic to operate on a set of components.
makeSystem("move", [Position, Velocity]):
  all:
    item.position.x += item.velocity.x
    item.position.y += item.velocity.y

# Generate the ECS interface.
makeEcs()

# Output defined systems and choose name of execution proc.
commitSystems("run")

# Create an entity to use the "move" system.
let
  movingEntity = newEntityWith(
    Position(x: 1, y: 1),
    Velocity(x: 2, y: -1),
  )

# Runs the "move" system once.
run()

# Check the new component values.
let pos = movingEntity.fetch Position
assert pos.x == 3 and pos.y == 0
```

## Compile time focus

Adding and removing components generate updates for system lists based on the static types involved. These system state changes do the minimum run time work the system/component design allows for.

Functionality such as building entities from blueprints, cloning entities, and debugging utilities are also fully generated from your types and system design at compile time.

The more that state can be determined at compile time, the less run time work is needed. Creating a new entity with a set of components fully specifies the entity state, and systems are unconditionally matched at compile time. Any system updates required are then output directly without any run time speculative work.

Adding and removing multiple components at once is also minimised at compile time, outputting single pass inline operations.

### Characteristics

- ***Design driven***: run time work is generated from the component/system design. Tiny designs yield tiny outputs, complex designs only pay for what they use.
- ***No run time manager***: no query engine, no system iteration overhead. Always up to date systems.
- ***Low cost to change components***: freely evolve entities with wildly disparate components without moving memory.
- ***Sequential code flow***: flatten run time composition, declarative code execution, and event hooks into a linear flow without needing callbacks or virtual calls.
- ***Architecturally simple output***: outputs simple loops over lists in a set order. Aims to scale from stack only, low resource environments to cache efficient, high performance data processing.
- ***Granular code generation options***: select different data structures for each component and system, choose error handling mechanisms, system interval execution, indexing and removal strategies, and more - without changing any code.

## Why Nim?

[Nim](https://nim-lang.org/) is an extremely adaptable language with low development friction, very high performance, and fast compile times. It's built to be readable with a flexible syntax.

The language is extremely portable, compiling to C, C++, ObjC, and JavaScript, along with good Python interop. Nim's static typing and high level abstractions can be shared across domain boundaries and interface with a huge variety of ecosystems.

Extensibility is a core philosophy, with hygienic macros using the language in a VM to process abstract syntax trees directly. Nim's compile time evaluation and well supported macros make this library possible.

## Polymers companion library

The [**Polymers**](https://github.com/rlipsc/polymers/) library provides ready-made components and systems for various tasks:

  - `Console`: reading keyboard and mouse events, writing text with normalised (-1, 1) coordinates.
  - `Database`: performing queries with ODBC.
  - `Networking`: components for:
    - socket TCP/IP (Windows IOCP) and UDP,
    - HTTP processing,
    - Serving webpages,
    - JSON RPC over HTTP.
  - `OpenGl`: render instanced models using the [glBits](https://github.com/rlipsc/glbits) shader wrapper.
  - `Physics`: components for interacting with the [Chipmunk2D](https://chipmunk-physics.net/) physics engine.

Included in the [demos](https://github.com/rlipsc/polymers/tree/master/demos) folder are examples combining different components from Polymers.
  - [A console database browser](https://github.com/rlipsc/polymers/blob/master/demos/dbbrowser.nim).
  - [Swish around 250,000 colour changing particles with your mouse](https://github.com/rlipsc/polymers/blob/master/demos/particledemo.nim).
  - [A particle life simulation](https://github.com/rlipsc/polymers/blob/master/demos/particlelife.nim).
  - [A simple 2D game using OpenGL](https://github.com/rlipsc/polymers/blob/master/demos/simplegame.nim).
  - [A simple web server](https://github.com/rlipsc/polymers/blob/master/demos/simplewebsite.nim).

# Overview of building an ECS

Polymorph uses a gather and seal process to generate the output.

This is a three stage process:

1. ***Design the components and systems*** using `registerComponents`, `defineSystems` and/or `makeSystem`.
2. ***Seal the design*** and generate the ECS interface with `makeEcs`.
3. ***Output the program logic*** for systems with `commitSystems`.

For example:

```nim
import polymorph

# Design stage.

registerComponents(defaultCompOpts):
  type
    MyComponent = object

makeSystem("mySystem", [MyComponent]):
  all:
    echo "MyComponent: ", item.myComponent

# Seal the design and generate the ECS.

makeEcs()

# Output the system code.
commitSystems("runSystems")

# Run the committed systems in the order they're defined.
runSystems()
```

## After `makeEcs`

Once all the components and systems have been defined, the `makeEcs` macro generates the core entity operations, allowing you to use the design:

  - Macros:
    - `newEntityWith`: create an entity with a set of components.
    - `add`/`addComponents`: add components to an entity.
    - `remove`/`removeComponents`: remove components from an entity.
  - Procs:
    - `newEntity`: create an entity.
    - `fetch`/`fetchComponent`: returns a component instance from an entity.
    - `delete`: delete the entity.
    - `construct`: build an entity from a list of components.
    - `clone`: copy an entity.
  - Templates:
    - `caseComponent`: a `case` statement for run time component type ids.
    - `caseSystem`: a `case` statement for run time system ids.
  - Debugging:
    - `$` operators for entities, component instances, and other supporting types.

# Defining components

Creating components is as simple as wrapping their type definition with `registerComponents`. This will process any `typedef` and pass the block through unaltered.

`registerComponents` takes two parameters:

* An `ECSCompOptions` object. This controls code generation for these components.
* A block of code with typedefs in it.

For example, to create three components, `A`, `B`, and `C`.

```nim
import polymorph

# 'defaultCompOpts' and other ECS option types are defined in
# 'sharedTypes.nim'.
registerComponents(defaultCompOpts):
  type
    A = object
    B = object
      text: string
    C = object
      value: int
```

You can use `registerComponents` multiple times before actually constructing the ECS, for example to split component definitions over separate modules.

Each `registerComponents` may have separate options.

As a special case, you can include type definitions in `registerComponents` that should not be made into components with the `{.notComponent.}` pragma. This can be useful when you want to refer to types within component definitions, but can't define them externally to the type block.

```nim
registerComponents(defaultCompOpts):
  type
    Data {.notComponent.} = object
      value: int
    E = object
      data: Data
```

## `registerComponents` generated types

Each type passed to `registerComponents` creates two other types to support its use as a component:

1) An instance type, generated with the `Instance` postfix. This is used to point to a specific component's data in storage.

2) A container type, generated with the `Ref` postfix. This is inherited from the `Container` type, and allows `seq[Component]` with different component types for [`construct`](#constructing-entities-at-run-time) to build arbitrary entities at run time.

## Components and instance types

Instance types are `distinct` integers that reference values for that type, and are how components are generally represented within the ECS.

For example systems use instances by default, and `fetch` returns them. Any component attached to an entity can be represented by an instance.

It can be useful to understand how this mechanism works for usability and performance reasons.

Polymorph implements the dot accessors `.` and `.=` for instance types so that fields can be accessed like the original data type. This works by using the integer value of the instance to index into the type's associated component storage.

```nim
import polymorph

registerComponents(defaultCompOpts):
  type
    Foo = object
      text: string
      value: int
    Bar = object
      value: float

makeEcs()

# Create an entity with both components.
let
  entity = newEntityWith(
    Foo(text: "Hello!", value: 123),
    Bar(value: 12.34))

# Get instances for the components.
let
  foo = entity.fetch Foo
  bar = entity.fetch Bar

# Act on component fields through instances.
foo.text = "Hello"
foo.value = 17
assert foo.value == 17
bar.value = 5.5

# This is transformed into something like:
storageFoo[foo.int].text = "Hello"
storageFoo[foo.int].value = 17
assert storageFoo[foo.int].value == 17
storageBar[bar.int].value = 5.5
```

Instance types have reference semantics, and as they are simply integers they're cheap to pass about and validate without invoking the GC. Looking up the index to storage is one indirection with list storages, and that indirection should have a good chance of being in the cache - assuming a linear component allocation pattern.

Sometimes, however, you want access to the underlying type directly. For this, there is the `access` template, which transposes the storage lookup for the whole component type.

```nim
  let copyFoo = foo.access
  # Translates to something like this:
  let copyFoo = storageFoo[foo.int]
```

You can also update components in one go using instances:

```nim
foo.update Foo(text: "Hey there", value: 18)
```

To check if instances returned from `fetch` were found, use the `valid` template.

```nim
let foo = entity.fetch Foo
if not foo.valid:
  echo "Cannot find Foo on this entity"
```

## Instances in components

`registerComponents` generates and inserts instance types before the component definitions themselves. This lets you use instance types within component definitions and have type safe links to other components.

Instance types are always defined as the component type name postfixed with "Instance".

```nim
# Parse and generate instance types.
registerComponents(10):
  type
    A = object
      b: BInstance
    B = object
      a: AInstance

# Outputs something like:
type
  AInstance = distinct int
  BInstance = distinct int

  A = object
    b: BInstance
  B = object
    a: AInstance
```

> ***Note***: **instances don't store generation information**. Use `ComponentRef` to include generation checking.

# Defining systems

Systems perform all the program logic for an ECS. Each system is essentially a proc which loops over a set of components to perform some work.

For systems to participate in an ECS, they must first be defined with `defineSystem`, passing the name of the system along with the component types it uses. This allows `makeEcs` to create ECS operations based on how systems and components interact.

After a system is defined, it needs program logic assigned to it in order to perform work. Such logic is referred to as a system *body*.

System bodies can only be defined once per system.

When `defineSystem` is used, you can declare the body of a system later with `makeSystemBody`. This allows you to keep the code for a system separate from its definition.

Alternatively, you can define a system and its body at the same time with `makeSystem`, or `makeSystemOpts` if you wish to specify system compile options.

`makeSystem` is passed the name and component types like `defineSystem`. If the system is already defined, `makeSystem` will just add the system body whilst ensuring that the components and options match the previous definition. It's a compile time error for the types or options to be mismatched between `defineSystem` and `makeSystem` for the same system.

Once a system is defined (whether using `defineSystem` or `makeSystem`), the system type is created and its variable is instantiated. These variables are created with the system's name, prefixed with `sys` and are immediately accessible.

```nim
import polymorph

registerComponents(defaultCompOpts):
  type
    Comp1 = object
    Comp2 = object

# Define a system.
defineSystem("mySystem1", [Comp1])

# Access the instantiated system.
assert sysMySystem1.count == 0

# Define a system and pass compile options to it.
defineSystem("mySystem2", [Comp1, Comp2], defaultSysOpts)

# Define a system and code body at the same time.
makeSystem("mySystem3", [Comp1, Comp2]):
  all:
    echo "Comp1: ", item.comp1

# Once the ECS is sealed, new systems can't be defined using these components.
makeEcs()

# Define a code body for a previously defined system.
makeSystemBody("mySystem1"):
  all:
    echo "Comp1: ", item.comp1

# Define a code body for a previously defined system with makeSystem.
# Options are retrieved from the system's defineSystem.
makeSystem("mySystem2", [Comp1, Comp2]):
  all:
    echo "Comp1: ", item.comp1
```

## Providing custom system fields

Passing field definitions to `defineSystem` will add them to the system variable declaration.

```nim
const sysOpts = EcsSysOptions()

defineSystem("mySystem", [Comp1], sysOpts):
  myFieldStr: string

sysMySystem.myFieldStr = "Foo"

makeSystemBody("mySystem"):
  echo "myFieldStr = ", sys.myFieldStr
```

These fields can also be initialised at system creation.

```nim
defineSystem("mySystem", [Comp1], sysOpts):
  myFieldStr = "Foo"
```

When the type cannot be inferred but needs to be initialised, you can explicitly define it.

Since the `name: type = value` isn't valid syntax in this context, the type is defined with `->`.

```nim
defineSystem("mySystem", [Comp1], sysOpts):
  myFieldStr -> string = "Foo"
```

Fields can also be added when defining a system and its body with `makeSystem` by using the `fields` block. If the system has already been defined these fields are checked to match the definition.

```nim
makeSystem("mySystem", [Comp1]):
  fields:
    myFieldInt: int
  echo "This system has a custom field: ", sys.myFieldInt
```

## Committing systems

Once committed, each system is output as a procedure named after the system, prefixed with "`do`".

Separating `makeEcs` and `commitSystems` allows for more design flexibility, for example:
  - to write code that uses the sealed ECS (after `makeEcs`) and is also used within system code,
  - to allow the sealed ECS and system code as separate imports,
  - to split systems into separate wrapper procs.

> Note: systems bind at the site they're output, *not* where the body is defined with `makeSystem`. As such, external routines or variables used by systems need to be accessible at the commit site.

```nim
# CommitSystems uses the string parameter to create a wrapper proc that
# runs the systems it outputs in order.
commitSystems("runMySystems")

# Run systems output by `commitSystems` above.
runMySystems()

# You can run systems individually using their `do` proc.
doMySystem()
```

## System execution order

The order systems are run is key in determining how your ECS operates.

Systems are added to the output of `commitSystems` in the order `defineSystem` is encountered.

When no matching `defineSystem` is present, `makeSystem` will invoke `defineSystem` for you, defining the order as it's encountered in the source code.

```nim
import polymorph

var executionOrder: seq[string]

registerComponents(defaultCompOpts):
  type Foo = object

# The order systems are defined sets the order they're run when output
# by `commitSystems`.
defineSystem("a", [Foo], EcsSysOptions(maxEntities: 1))
defineSystem("b", [Foo])

# Define the system "c" and also add a code body.
makeSystem("c", [Foo]):
  executionOrder.add sys.name

# Seal and generate the ECS.
makeEcs()

# Define the code body for "a".
makeSystemBody("a"):
  executionOrder.add sys.name

# Both "c" and "a" have bodies waiting to be committed, but "b" doesn't
# have a code body yet and so is not included in the output of `commitSystems`.
#
# As systems are run in the order they're defined, the output proc will
# run "a" then "c".
commitSystems("runAC")

# Define the body for "b". This will be included in the next `commitSystems`.
makeSystemBody("b"):
  executionOrder.add sys.name

# Only "b" has an uncommitted code body so the output will just run "b".
commitSystems("runB")

# Execute the two run procs.
runAC()
runB()

# Check the order of execution is as expected.
assert executionOrder == @["a", "c", "b"]
```

## Grouping systems

Systems can be extracted into separate procedures using `defineGroup`. Grouping can be useful to separate the concerns of multiple systems to particular procedures.

Systems can be part of *multiple* groups at the same time.

When a system is grouped, it will not be output by `commitSystem` and must be manually output using `commitGroup`.

Grouping can be performed explicitly by passing a list of systems to `defineGroup`, or ad hoc by using `defineGroup` without specifying systems. In the latter case, any systems that have been defined but *not already grouped or committed* are added to the group.

It's a compile time error to try to perform `commitGroup` for systems that don't have bodies.

Grouping can occur before or after `makeEcs`.

Group names are case insensitive.

```nim
import polymorph

registerComponents(defaultCompOpts):
  type Comp1 = object

makeSystem("g1a", [Comp1]): discard
makeSystem("g1b", [Comp1]): discard

# Gather previously defined systems into a group.
defineGroup "group1"

makeSystem("g2a", [Comp1]): discard
makeSystem("g2b", [Comp1]): discard

# Add specific systems to a group.
defineGroup "group2", ["g2a", "g2b"]

makeSystem("ungrouped", [Comp1]): discard

makeEcs()

commitGroup "group1", "runGroup1"
commitGroup "group2", "runGroup2"
# Commit the remaining "ungrouped" system.
commitSystems "runUngrouped"

runGroup1()
runGroup2()
runUngrouped()
```

## System anatomy

Systems offer several labelled blocks that allow executing code in particular contexts. These are classified into two categories: system scope blocks and work item blocks.

System scope blocks (`init`, `start`, `finish`) are run at specific points in a system's execution. They may be written at any point in the root of the system's body, but are *extracted* from the body and pieced together in the order they're written, to be executed within the appropriate scope.

Work item blocks (`all`, `stream`) are run for items in the system's work list. These blocks are expanded in place within the system body. Multiple occurrences of these blocks are run as they are encountered within the system body and allow multiple passes of the system's work list, potentially with other code executed between them.

Systems can be disabled by setting `disabled = true` in the system variable. When disabled, a system will not perform any blocks, even `init`, until `disabled = false`.

Systems can be paused by setting `paused = true` in the system variable. When paused, the system scope blocks `init`, `start`, and `finish` are still executed. This allows control of paused state within these blocks.

Code in the root of a system body is executed when the system is run and `paused` and `disabled` are `false`.

Within a system the `sys` template refers to the variable for the current system.
### System scope blocks

- `init:` this is run when the system's `initialised` flag is unset. After this block is run the system's `initialised` boolean is set to true. You can set `initialised` to false to re-trigger this block.
- `start:` executed every time a non-disabled system begins running, before the check for `sys.paused`. The `start` block is in the same scope as the system body and the `all`, `stream`, and `finish` blocks, so variables and data defined in `start` can be used within these blocks. The `start` block can also be useful to change `sys.paused` according to some condition to inhibit or allow the system body to run.
- `finish:` run after a system body has finished executing, regardless of `sys.paused` state. This can be useful for cleaning up or changing `sys.paused` state.

### Work item blocks

These block include an `item` template to access the current row entity's components. This template provides access to the entity being processed as `item.entity`, and components the system uses defined as the type name in lower case (eg; `item.myComponent`).

- `all:` this is usually where most processing is performed and is executed for each entity/component combination in the system's work list.

- `stream:` process a portion of the entities in the system. This has several variations:
  - `stream:` process `sys.streamRate` entities. If `sys.streamRate` is zero, all entities are processed as if this were an `all` block.
  - `stream N:` the same as `stream`, but overrides `sys.streamRate` to fix the number of entities processed per run to `N`.
  - `stream multipass:` the same as `stream`, but will force `sys.streamRate` entities to be processed per run. If the number of entities is less than `sys.streamRate`, they will be reprocessed until `sys.streamRate` is met.
  - `stream multipass N:` the same as `stream multipass` but overrides `sys.streamRate` to `N` entities.
  - `stream stochastic:` processes `sys.streamRate` entities selected at random from the system.
  - `stream stochastic N:` the same as `stream stochastic` but overrides `sys.streamRate` to `N` entities.

```nim
import polymorph

registerComponents(defaultCompOpts):
  type Comp1 = object

makeSystem("allTheBlocks", [Comp1]):
  init:
    echo "Init: first run for ", sys.name
  start:
    echo "Start: beginning execution for ", sys.name
  
  echo "Body: this system has started running!"
  
  all:
    echo "All: running for entity ", item.entity.entityId.int
  
  echo "Body: the all block has finished!"
  
  stream:
    echo "Stream: running for entity ", item.entity.entityId.int
  
  echo "Body: the stream block has finished!"

  finish: echo "Finish: finished execution for ", sys.name
  start: echo "Start: still beginning execution for ", sys.name

makeEcs()
commitSystems("run")
let entity = newEntityWith(Comp1())
run()
```

The above outputs:
```
Init: first run for allTheBlocks
Start: beginning execution for allTheBlocks
Start: still beginning execution for allTheBlocks
Body: this system has started running!
All: running for entity 1
Body: the all block has finished!
Stream: running for entity 1
Body: the stream block has finished!
Finish: finished execution for allTheBlocks
```

### Removing components during `all` or `stream` blocks

Operations that remove rows from the *currently iterating* system in a work item block may invalidate the `item` template until the next row.

This means that `item.entity.delete` makes `item` refer to a different entity and set of components, or even invalid memory!

To force checking this condition each time `item` is accessed at run time, set `assertItem = true` in the `ECSSysOptions` passed when defining the system.

To help with these cases without needing run time checks, the `entity` variable accessible in these blocks references the entity that the row *originally* started with, regardless of the system state. This can be useful when you want to perform destructive operations to the row (such as `delete` or `removeComponent` for a component the system uses, which causes the row to be removed) without worrying about the `item` being invalidated:
  ```nim
  makeSystem("processEntities", [SomeComponent]):
    all:
      # Removing a component used by the system will remove
      # the row and invalidates `item`.
      entity.remove SomeComponent
      # We can still change the original entity even though
      # `item.entity` may now be different.
      entity.add SomeOtherComponent
  ```

Polymorph detects when you add or remove components that affect the currently iterating system within `all` and `stream` blocks at compile time, and will add extra checks to ensure iterations stay within bounds.

*Deleting* entities, however, is opaque to compile time analysis as it cannot be known what components exist on a run time entity. This means these checks will always be added when deleting entities inside `all` or `stream` blocks.

To see information about which system iteration loops are affected by removes/deletes, compile with `-d:ecsPerformanceHints`.

Another option for deleting entities is to use the system's `deleteList`. Any entity added to this `seq` will be removed after the `finish` block is executed, and the list is then cleared. These deletes don't affect iteration, but simple appending may cause a heap allocation and potentially memory moving as part of the standard `seq` operation. For extensive use of `deleteList`, it may be worth setting the capacity at the start of system execution.

```nim
makeSystem("deleteEntities", [SomeComponent]):
  all:
    sys.deleteList.add entity
```

## System utilities

You can perform delete/remove operations on systems as a whole with the following two operations:

### `clear`

Deletes **all entities** in the given system. Use with caution!

```nim
mySystem.clear
```

### `remove`/`removeComponents`

Removes one or more components from all entities in the given system.

```nim
mySystem.remove Comp1, Comp2, Comp3
```

Within system blocks you can use the `sys` template with these utilities to act on the current system:

```nim
makeSystem("removeComp1", Comp1):
  finish: sys.remove Comp1
```

# Working with entities

## Creating entities

Once `makeEcs` has finished, entities can be created with either `newEntity` or `newEntityWith`, where the latter allows setting up entities with components.

In particular, `newEntityWith` can be a very performant way to create entities, as by definition the entity can only contain the components passed to it, and the systems that need to be updated are fully constrained at compile time.

This means that the output code consists of simply updating the entity's internal list and producing static system updates only where parameter components fully satisfy systems. No conditional work is required.

```nim
registerComponents(defaultCompOpts):
  type
    Comp1 = object
      value: int
    Comp2 = object
      value: string
    Comp3 = object
      value: float

makeSystem("mySystem", [Comp1, Comp2]):
  all: discard

makeEcs()

let
  # An entity without any components.
  entity = newEntity()
  
  # Create an entity and include it in "mySystem".
  entWithComps = newEntityWith(
    Comp1(value: 123),
    Comp2(value: "Foo"))
```

## Adding components to existing entities

Entities can be updated 'piecemeal' with `add` or `addComponents`.

This operation also supports multiple components at a time. Providing multiple components in one operation (as opposed to several singular `add` operations) can help inform code generation to confirm systems that are definitely being updated, producing non-conditional code.

> **Note**: it's a run time error to add a component type that already exists on the entity.

Adding components returns the component instances that have been added. For multiple components, this is a named tuple of the components being added. For single components, just the instance is returned.

```nim
# `comps` is a named tuple with the instances of the components we've added.
let comps = entity.add(Comp1(value: 456), Comp2(value: "Bar")

echo "Comp1: ", comps.comp1, " Comp2: ", comps.comp2

# For single components, just the component being added is returned.
let comp3 = entity.add(Comp3(value: 0.123))

echo "Comp3: ", comp3
```

## Fetching and checking for components

To retrieve a component from an entity, use `fetch`/`fetchComponent`. This takes the *type* of the component and returns an instance.

Fetching a component that doesn't exist on the entity returns `InvalidComponent`, and can be checked for with `valid`.

```nim
let comp1 = myEntity.fetch Comp1
if comp1.valid:
  echo "Comp1 was found: ", comp1
else:
  echo "Comp1 was not found."
```

To see if a component exists on an entity without fetching it, use `has`/`hasComponent`.

```nim
if myEntity.has(Comp1):
  echo "Comp1 was found!"
else:
  echo "Comp1 was not found."
```

## Removing components

Removing components is performed with `remove` or `removeComponents`. Much like adding, this operation also allows multiple components to be removed in a single operation.

This takes the *type* of components.

```nim
# Remove components from the entity and systems using Comp1 and/or Comp2.
entity.remove Comp1, Comp2
```

## Deleting entities

Entities are deleting using the `delete` operation. This operation is fully run time bound, as components cannot be determined at compile time. However, like other operations, it is generated as a static proc from your design at compile time, so fewer components in a design will generate less code.

In general, the performance of `delete` depends on how many components the entity has. Deleting also doesn't need to bookkeep the components being removed like `remove` does.

```nim
# Remove the entity and delete associated component and/or system storage.
entity.delete
```

# Constructing entities at run time

`makeEcs` generates a `construct` procedure that lets you build entities from lists of components.

This is fairly efficient, as the list is parsed for types then the entity is created in a single integrated operation in a similar way to `newEntityWith`. As such it can elide the speculation and repeated work of multiple separate `addComponent` operations.

To allow storing different component types in a single list, `registerComponents` generates a `ref` container type for each component, descended from the `Component` object.

These container types are defined as the component type name postfixed with `Ref`.

The `construct` procedure then takes a `seq[Component]`, aliased as `ComponentList`, to build an entity.

All container types have a `typeId` that must be initialised with the component's `ComponentTypeId` in order for `construct` to extract the value from the subtype. Containers with an uninitialised `typeId` will cause `construct` to fail at run time.

The `makeContainer` template will correctly set up containers for individual components.

```nim
let mcContainer = MyComponent(data: 1234).makeContainer
```

The `cl` macro makes setting up a `ComponentList` much more convenient.

This macro lets you mix the original component types and `ref` container types, and ensures containers are correctly set up. Another advantage of `cl` is that it always outputs `ComponentList`, and avoids over constraining the list type when single components are used - for example `@[MyComponentRef()]` is of type `seq[MyComponentRef]`, not the `seq[Component]` type that `construct` expects.

```nim
import polymorph

registerComponents(defaultCompOpts):
  type
    Comp1 = object
      value: int
    Comp2 = object
      value: string

makeEcs()

let
  entityBlueprint = cl(Comp1(value: 1234), Comp2(value: "Foo"))
  entity = entityBlueprint.construct

let
  c1 = entity.fetch Comp1
  c2 = entity.fetch Comp2

assert c1.value == 1234
assert c2.value == "Foo"
```

## Constructing multiple entities

To create multiple entities in one operation, a `seq[ComponentList]` is used. This is aliased as `ConstructionTemplate`.

```nim
let
  myEntityBlueprints = @[
    cl(Comp1(value: 1234), Comp2(value: "Foo")),
    cl(Comp2(value: "Bar")),
  ]
  myEntities = myEntityBlueprints.construct

echo myEntities[0], myEntities[1]
```

## Cloning entities

Entities may be duplicated at run time with the generated `clone` procedure.

```nim
let myClose = myEntity.clone
```

This performs less validation work than `construct`, as the entity state must already be valid for the entity to exist.

# Switching components with component lists

The `transition` template lets you remove one set of components and add or update another. This can be useful for using components for state machine behaviour. Only components within the parameter lists `prevState` and `newState` are considered. Other components are unaffected.

This comes in two flavours:
- `transition(entity: EntityRef, prevState, newState: ComponentList, transitionType: static[EntityTransitionType])`

  `transitionType` controls whether to just update components that
  are in both states, or to always remove components in
  `prevState` and add `newState`.

  - A transition type of `ettUpdate` will remove components that are in
  `prevState` but don't exist in `newState`, and update components that
  exist in both `prevState` and `newState`.
  Events such as `onAdd`/`onRemove` for updated components are not
  triggered, the data for the component is just updated.

  - A transition type of `ettRemoveAdd` will always trigger events
  such as `onAdd`/`onRemove`, but does more work when many components
  are shared between `prevState` and `newState` and may reorder more
  system rows. This can be useful for components containing managed
  resources and other situations where events must be triggered.

- `transition(entity: EntityRef, prevState, newState: ComponentList)`
  
  This version calls `transition` with `ettUpdate`.

> Note: be aware when using `transition` whilst iterating in a system that removing components the system uses can invalidate the current `item` template.

```nim
import polymorph

registerComponents(defaultCompOpts):
  type
    A = object
      value: int
    B = object
      value: int
    C = object
      value: string
    D = object
      value: string

makeEcs(defaultEntOpts)

let
  compsAB = cl(A(value: 456), B(value: 789))
  compsAD = cl(A(value: 999), D(value: "Bar"))
  
  entity = newEntityWith(A(value: 123), C(value: "Foo"))

entity.transition(compsAB, compsAD)
# A is overwritten with the A in `compsAC` and D is added.
# There's no B to remove yet and C is unaffected.
# Entity is now: A(value: 999), C(value: "Foo"), D(value: "Bar")

entity.transition(compsAD, compsAB)
# A is overwritten with the A in `compsAB`, D is removed, and B is added.
# C is unaffected.
# Entity is now (A(value: 456), C(value: "Foo"), B(value: 789))
```

# Events

Polymorph includes a variety of events for different situations. Most of these are 'inline' and are included ad hoc when required in the output code. All events are called immediately at the point of invocation.

In general, events are invoked *after* the state has been fully resolved for events that trigger when 'adding' and *before* the state has been resolved for 'removing' events.

## Inline events

These events are directly injected during a state change without any call overhead. This makes them great for component initialisations/deinitialisations, monitoring, and other light work.

Each event *appends* code, which is run in the order the event code is added. This allows extending events on components even if they already have existing event code.

Note: as these events are defined during `makeEcs` they *do not have access to the sealed ECS*.
This is important because it means you ***cannot add or remove components*** within these events.

| Event | Parameters | Triggered |
|---|---|---|
| `onAdd` | A component type | When this type is added to an entity |
| `onRemove` | A component type | When this type is removed from an entity |
| `onSystemAdd` | A component type | When a type is added to any system |
| `onSystemRemove` | A component type | When a type is removed from any system |
| `onSystemAddTo` | A component type and a system name | When a type is added to a particular system |
| `onSystemRemoveFrom` | A component type and a system name | When a type is removed from a particular system |

## System events

Additionally, there are two important system events which offer access to all the components involved in a system. These come in two flavours, inline and callbacks.

These events allow using the sealed ECS, such that you can add or remove components from entities within the events themselves.

    Note: whilst being able to change entities during active events can be extremely useful, it can dramatically reduce transparency and increase complexity - particularly if your system is used externally, such as in a library.

| Event | Triggered |
|---|---|
| `added` | When a new row is added to the system |
| `removed` | When a row is removed from the system |
| `addedCallback` | Call a procedure when a new row is added to the system |
| `removedCallback` | Call a procedure when a row is removed from the system |

### Mutating entities within inline system events

Inline events are expanded by the compiler and can therefore create cycles at compile time. For example, if your `added` event for a system adds a component that directly or indirectly ends up triggering the original `added` event again, code generation would get stuck in a cycle.

Polymorph guards against event cycles at compile time and will fail with an error message when events of the same type invoke the same system more than once. However this is limited to code expansion, not semantic analysis. It cannot determine conditional event triggers, for example.

This caveat only applies to inline system events `added` and `removed`. Callback events such as `addedCallback` and `removedCallback` do not require expansion in this way, and so offer the benefits of manipulating entities without the potential for compile time cycles.

It's worth being aware of how events affect a design, particularly if your systems are being used by other developers who may use their own events which could be triggered along with, or inside, your event code.

## Change events

| Event | Triggered |
|---|---|
| `onEntityChange` | When components are added or removed from any entity |

This event is triggered whenever an entity state changes. This includes components being added or removed from entities, new entities are constructed from a template of components, or entities are deleted. However it is not triggered for 'empty' entities such as created with `newEntity` (since no components have been added yet).

This example shows logging all component changes for entities:

```nim
onEntityChange:
  echo "Change: ", entity.entityId.int, ": ", state, ": ", types
```
## `onEcsBuilt`

Code passed to this macro will be emitted after `makeEcs` has completed.

This can be useful when you want to define logic that uses the ECS and must be available as soon as possible. For example, for use within system bodies, for general set up situations, or for providing utility functions.

In particular, this is invaluable for library components/systems, where some initialisation or utility procedures may use the ECS, but the library doesn't have control over when `makeEcs` is run.

```nim
registerComponents(defaultCompOpts):
  type Counter = object
    value: int

onEcsBuilt:
  # This code is inserted after makeEcs has completed.
  echo "makeEcs has finished!"

  # This procedure uses the ECS.
  proc inc(entity: EntityRef) =
    let counter = entity.fetch Counter
    if counter.valid:
      counter.value.inc

makeEcs()
# Outputs "makeEcs has finished!" to the console, and the `inc` proc is
# included.

let ent = newEntityWith(Counter(value: 1))

# Use the previously defined proc.
ent.inc

assert ent.fetch(Counter).value == 2
```

## Construction and clone events

These events allow you to intercept the construction and cloning of entities and edit, replace, ignore, or provide multiple components in response.

Changing the component types being added is not possible with other events.

To register these events `makeEcs` must have been run, and as such they have full access to ECS operations.

One use for these kind of events is for blueprinting run time only data such as external resources.
One component type can be used to provide information for instantiation, and is replaced with a different component that represents initialised data. This can be useful for separating the concerns of systems.

Another example is for meta-components. One component can contain information that adds one or more other components when passed to `construct`. Components can also create and manage sets of *entities* on construction as well.

### Construction events

- `registerConstructor` takes a component type and a proc of the form:
  ```nim
  proc (entity: EntityRef, component: Component, context: EntityRef): seq[Component]
  ```
  - `entity`: the current entity being constructed.

  - `component`: the `Component` container supertype passed from the `ComponentList` being built.

    To get the value stored within, type cast to the container subtype for the component you're hooking and access its `value` field.
    
    The component's container type is created by `registerComponents` as the component name with the `Ref` postfix, for example a `MyComponent` type would use `MyComponentRef(component).value`.

  - `context`: the `construct` proc allows optionally passing an entity to provide input to construction events.

    If no entity is provided, `context` will match the `entity` parameter.

    When `construct` creates multiple entities from a `ConstructionTemplate`, the first entity built is always passed as `context` to these events. This allows you to define the context for multiple entities in the `ConstructionTemplate` itself.
  
  Components are added by appending to the `seq[Component]` result. It's possible to add any number of components to the result, as long as there aren't any repeated types. It's also valid to not add to the result and elide the component from the entity.

  ```nim
  registerComponents(defaultCompOpts):
    type
      Original = object
        data: int
      Replaced = object
        data: int
  
  makeEcs()

  # This event will replace the `Original` component type with `Replaced`
  # during construction.
  proc replaceOriginal(entity: EntityRef, component: Component, context: EntityRef): seq[Component] =
    let original = OriginalRef(component).value
    result.add Replaced(data: original.data)

  registerConstructor Original, replaceOriginal

  # Build an entity from a component list using the `Original` type.
  let entity = Original(data: 1234).cl.construct

  # Confirm the replaced component.
  assert not entity.has(Original)
  assert entity.has(Replaced)
  assert entity.fetch(Replaced).data == 1234
  ```
  
- `registerPostConstructor`: takes a component type and proc of the form:
  ```nim
  proc (entity: EntityRef, component: ComponentRef, entities: var Entities)
  ```
  - `entity`: the entity being constructed.
  - `component`: the component ref that's been assigned to the entity. To convert this to an instance to access it, type cast the index field with the component's instance type, which is the type postfixed with `Instance`. For example for `MyComponent` the instance would be accessed with `MyComponentInstance(component.index)`.
  - `entities`: the list of entities that will be returned to the user.

  These events are called after multiple entities are built by using `construct` with a `ConstructionTemplate`. The event allows work to be performed on the fully constructed sets of entities.
  
  It does not allow changing the type of components *as they're added* like `registerConstructor`, but allows full manipulation of the fully constructed entities afterwards.

  This can be useful to update components that keep track of other entities, or perform other multi-entity work.

  ```nim
  proc countEntities(entity: EntityRef, component: ComponentRef, entities: var Entities) =
    echo "I counted: ", entities.len

  registerPostConstructor MyOtherComponent, countEntities
  ```

- `registerCloneConstructor`: takes a component type and proc of the form:
  ```nim
  proc (entity: EntityRef, component: ComponentRef): seq[Component]
  ```
  - `entity`: the new cloned entity.
  - `component`: the ***source*** component being cloned. This is a `ComponentRef`, to convert this to an instance to access it, type cast the index field with the component's instance type. For example: `MyComponentInstance(component.index)`.

  Clone events allow you to transform components from entities built by `clone`. This can be useful when resources need to perform some extra work to be duplicated, or to deny duplication entirely.

  ```nim
  proc displayCloning(entity: EntityRef, component: ComponentRef): seq[Component] =
    let instance = MyOtherComponentInstance(component.index)
    echo "We're cloning MyOtherComponent: ", instance
    # Create a copy of the original component.
    result.add instance.makeContainer

  registerCloneConstructor MyOtherComponent, displayCloning
  ```

# Writing component libraries

Components and systems can be shared and reused by defining them within templates.

Code generation options can be passed through these templates to let the consumer define the implementation details.

```nim
# Module writingcomponentlibs1
import polymorph

template defineSay*(compOpts: EcsCompOptions, sysOpts: EcsSysOptions) {.dirty.} =
  # The {.dirty.} pragma passes the code through for outside access.

  registerComponents(compOpts):
    type Say* = object
      text*: string

  makeSystemOpts("sayer", [Say], sysOpts):
    all: echo item.say.text
    finish: sys.remove Say
```

This can then be instantiated along with other components and systems.

```nim
import polymorph, writingcomponentlibs1

const
  compOpts = defaultCompOpts
  sysOpts = defaultSysOpts

# Include the `Say` component and system.
defineSay(compOpts, sysOpts)

# Add our own component and system.

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

# Seal and generate.

makeEcs()
commitSystems("run")

# Try out the ECS.

let entity = newEntityWith(SayEvery(ticks: 25))
for i in 0 ..< 100: run()
```
The following output is emitted:
```
0
25
50
75
```

# Code generation options

Polymorph offers three configuration types to control how code is generated.

## EcsCompOptions

When passed to `registerComponents`, this object controls how these components are generated at compile time.

### EcsCompOptions fields

- `maxComponents`: maximum amount of components when `cisArray` is selected for storage.
- `componentStorageFormat`: Underlying storage format for components.
  - `cisSeq`: store components in a heap allocated `seq`. This allows dynamically sizing storage by component. Resizing often, however, requires moving memory which can degrade performance.
  - `cisArray`: store components in a stack allocated, fixed size array of size `maxComponents`.
- `accessMethod`: controls accessing fields through instances. This currently only offers dot operator access with `amDotOp`.
- `clearAfterDelete`: when set, zeros memory of components after deletion.
- `useThreadVar`: declare the component arrays as {.threadVar.}.
- `invalidAccess`: allow inserting assert checks for each instance field access.
  - `iaIgnore`: no check is performed.
  - `iaAssert`: a run time assert is invoked.

## EcsSysOptions

When passed to `defineSystem`, `defineSystemOwner`, or `makeSystem`, this object controls how systems are generated at compile time.

### EcsSysOptions fields

- `maxEntities`: maximum entities this system can hold for `ssArray` storage, and for indexes set to `sifArray` or `sifAllocatedSeq`.
- `storageFormat`: underlying storage format for the system groups.
  - `ssSeq`: heap allocated and resized dynamically. Useful for adaptive memory use. Resizing often, however, involves moving memory which can degrade performance.
  - `ssArray`: stack allocated and fixed size according to `maxEntities`.
- `indexFormat`:
  - `sifArray`: constant time deletes, uses `maxEntities` * ~8 bytes per system, uses stack space. Recommended for performance applications.
  `sifAllocatedSeq`: heap allocated storage, initialised to `maxEntities`. Useful for large amounts of entities where stack space is at a premium.
  `sifTable`: adaptive memory use, but requires reallocated when extended.
- `timings`: generate timing code.
- `useThreadVar`: declare the system variable as `{.threadVar.}`.
- `echoRunning`: reporting system execution state can be useful for debugging blocking systems or to monitor the sequence of system actions.
  - `seNone`: no reporting.
  - `seEchoUsed`: echo when a defined system block has been entered.
  - `seEchoUsedAndRunning`: echo when a system proc has been entered, and also any defined system block has been entered
  - `seEchoUsedAndRunningAndFinished`: echo when a system proc has been entered, and also any defined system block has been entered, and also when the system proc has completed and is about to return.
  - `seEchoAll`: unconditionally echo when a system proc has been entered, any system block has been entered (even if not defined by the user), and when the system has completed.
- `assertItem`: add asserts to check the system `item` is within bounds.
- `orderedRemove`: maintains the execution order when items are removed from groups. This changes deletion from an O(1) to an O(N) operation.

## EcsEntityOptions

When passed to `makeEcs`, this object controls how entities are generated at compile time.

### EcsEntityOptions fields

- `maxEntities`: controls the maximum amount of entities for fixed size formats (ignored for `esSeq`).
- `componentStorageFormat`: choose the format of the component list in entity items.
  - `csSeq`: store entity components in a dynamically allocated heap `seq`.
  - `csArray`: store entity components in a fixed sized, stack allocated array of size `maxEntities`.
  - `csTable`: store entity components in a dynamically allocated `Table`. This offers O(1) fetch access, but may use more memory and be slower to iterate an entity's components than a `seq`.
- `entityStorageFormat`: choose between stack allocated or heap allocated array.
- `maxComponentsPerEnt`: only applies to `csArray` storage format.
- `recyclerFormat`: array access should be much faster, but takes up more space.
- `useSet`: use a set for hasComponent.
  - Note: sets can dramatically increase entity header size with a lot of components,
    as each `8` components defined will double the size of the set in bytes.

    Operations like creating, adding or removing are also slightly slower with
    a set due to the additional update work (~5% depending on component count).
    
    The advantage of using a set is that it allows an O(1) response for `hasComponent`
    when also using seq/array component lists.
    
    If you have seq/array component lists and don't want the extra memory burden
    of sets, you can ameliorate the O(N) cost of iteration for hasComponent by
    adding often checked/fetched components first, so finding them can return
    earlier.
    
    Component lists defined as a table will probably find the set unnecessary.
    
    Note that using hasComponent often can imply the need for a new system
    incorporating these components.
- `errors`: control how errors are generated.
  Each option here may use `erAssert` (the default) or `erRaise` to generate a run time exception.
  - `errEntityOverflow`: control how exceeding the maximum number of entities is reported.
  - `errCaseComponent`: control how invalid component types in `caseComponent` is reported.
  - `errIncompleteOwned`: control what happens when owned components passed to `addComponent` do not fully satisfy their owner system.

# Compile switches

- `-d:ecsLog`: use this switch to `echo` the ECS generation process.
  
  This includes:
    - component generation information,
    - system generation information,
    - the system execution order in the wrapper proc created by `commitSystems`.

- `-d:ecsLogDetails`: this expands on `ecsLog` and includes:
  - the ECS compile options used for systems and components,
  - details about each stage of the compile process by identity,
  - the creation for `delete`, `construct`, and `clone`,
  - operations such as `newEntityWith`, `addComponents` and `removeComponents`,
  - events triggered within operations,
  - event chains within system `onAdded`/`onRemoved` events.

- `-d:ecsPerformanceHints`: displays compile time information for tuning memory access patterns.
  This includes:
    - read/write component instance field access within a system; when there are many field accesses, it may be worth considering a read/update/write pattern and use `access` and/or `update`,
    - use of remove/delete in a system that has incurred an iteration checks,

- `-d:ecsLogCode`: outputs the generated code from `registerComponents`, `makeEcs`, and `commitSystems`, along with any ECS operations performed up to this point to a log file for inspection. This can be useful for low level debugging or performance reasons, or just to understand the raw logic generated from the ECS design.

  Useful things in the code log:
  - storage variables and access machinery for components and systems,
  - the output of `macro` operations such as `newEntityWith`, `addComponents`, and `removeComponents`,
  - `proc` operations such `construct`, `clone`, and `delete`,
  - how events are expanded within operations in the design.
  
  Note that any generated code *after* the above operations has to be manually 'flushed' with `flushGenLog()`. The log will then include subsequent operations.

  > **Important note**: as Nim doesn't currently allow you to output to files at compile time, the log is generated at compile time but written at ***run time***. In other words, using this switch will bulk your output executable and write a static string to file at program startup.
  
  The log file is named `ecs_code_log.nim` and will be placed in the same path as the executable when run. This file isn't actually runnable as it doesn't include the full program context, and the `.nim` extension is just a hint for editors to use syntax highlighting.

# Performance considerations

## Memory access patterns

Polymorph is a very thin layer over simple list processing, and the order of memory accesses is (currently) up to the user to manage. Future version may ameliorate this by optimising the insertion points of new rows, for now, high performance applications may find it useful to understand how systems execute with respect to memory access.

Systems have no overhead to iterate, but each (non-[owned](#owned-components-and-removing-indirection)) component is accessed via an index indirection to a list of structures. This format can be cache efficient for systems that access all the fields in the component, but can be less so when components contain large and seldom accessed fields, as this unused data may be pulled from RAM and fill the cache only to be skipped over by systems.

The key to high performance is to fully utilise the instruction and data cache within the CPU, fetching memory from RAM only when necessary. Components that are processed often should therefore be designed to hold the minimum reasonable data required for associated system tasks.

When a system's indirections are skipping backwards or far forwards in memory, performance will degrade as the CPU cache cannot be effectively used.

This can happen if systems often have rows removed then readded again from adding/removing components. The current implementation reuses component slots in the reverse order they're deleted, but new system rows are appended to the end of the system. Depending on the pattern of deletes and creates, this can mean these systems are accessing memory locations out of sequence.

```nim
# Consider this abstract representation of the memory locations that a
# system is accessing as it's iterating:
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
# This is an optimal access pattern, lending itself to good cache utilisation.

# Suppose components are now removed and readded many times, we could
# end up with something like this:
[9, 1, 7, 2, 8, 3, 6, 0, 5, 4]
# As this system iterates, it's skipping about in memory and the cache
# can't be as effectively used.
```

This kind of 'system churn' can dramatically affect the performance of systems by reducing the chance of data being in the CPU cache, causing many long trips to main memory. Each system has a unique "indirection window" to the underlying component storage based on the order of updates.

One solution is to simply not change components often and maintain bulk entity creation/deletion, or to design systems that undergo churn to work with smaller data sets that fit within cache, if possible. However, it can often be useful to take advantage of the dynamic qualities of run time composition in designs with large amounts of data.

Two tools that can help when performance sensitive systems often reorder their execution:

- Statistically analyse a system's memory access with `analyseSystem`.

- Remove indirection with owned components: inline component data and lifetime to a system.

## Fragmentation analysis

Ideally, a system processes each component sequentially, meaning component memory access moves forward by the size of the component. Systems which randomly access component storage will perform worse because the cache is not being effectively used.

To investigate how a system is accessing component storage, there is the `analyseSystem` proc.

This runs through each memory access the system is making and records the difference in subsequent accesses of the same component. This provides a "fragmentation" metric, which is a normalised value representing what portion of a system's memory accesses are *not* sequential.

```nim
import polymorph, random

registerComponents(defaultCompOpts):
  type Comp1 = object

makeSystem("frag", [Comp1]):
  all: discard

makeEcs()

let num = 100
var ents = newSeqOfCap[EntityRef](num)

for i in 0 ..< num:
  # Appending to the "frag" system and creating the component leads to
  # "frag" accessing Comp1 in sequentially.
  ents.add newEntityWith(Comp1())

for i in 0 ..< num:
  ents[i].remove Comp1
  ents[i].add Comp1()

# Display fragmentation analysis.
echo sysFrag.analyseSystem()
```

Passing the output of `analyseSystem` to the `$` operator includes some further processing of the results. Each component is listed along with the statistical characteristics of the locations that are referenced from the system.

This data also lets you understand the "shape" of indirections within a system and overall pattern of accesses, the trend of non-sequential accesses (forward or backwards in memory), the coefficient of variation, how this relates to a normal distribution, and how many outliers are present.

Tabulated to the right is a simplified 'quick look' version of the data.

> Note that 'outliers' in the "Kurtosis/spread" right tabulated data refer to the excess kurtosis. Many/few outliers in this case refer to outliers from the normal distribution for this data set.

The analysis is split into two parts for each component: statistics for out of order accesses as "Fragmentation", and statistics for all memory accesses within the system.

Here we can see that the fragmentation is not too bad in this system. Fragmentation for Comp1 is at just 2%, with only 2 accesses being out of sequence.

```
Analysis for frag (100 rows of 1 components):
  comp1:
    Value size: 1 B, jump threshold: 1 B
    Jumps over threshold : 1                                          Jump ahead: 1.0101 %
    Backwards jumps      : 1                                          Jump back: 1.0101 %
    Fragmentation: 2 % (n = 2):
      Mean scale: -48 times threshold
      Min: -98, max: 2, sum: -96                                      Range: 100 B
      Mean: -48
      Std dev: 50                                                     CoV: -1.0417
      Variance: 2500
      Kurtosis/spread (normal = 3.0): -2 (excess: -5)                 Few outliers
      Skewness: 0
    All address deltas (n = 99):
      Min: -98, max: 2, sum: 1                                        Range: 100 B
      Mean: 0.0101
      Std dev: 9.901                                                  CoV: 980.201
      Variance: 98.0302
      Kurtosis/spread (normal = 3.0): 93.9904 (excess: 90.9904)       Many outliers
      Skewness: -9.7969                                               Outliers trend backwards
```

However, if we perform the same operation to entities in a random order we see a different pattern.

In the example below, by removing and adding the component to the same entity, Comp1's storage may be reused whilst the system row in "frag" is appended to.

This can cause the order of accesses to Comp1, from the perspective of the "frag" system, to be out of sequence.

```nim
for i in 0 ..< num:
  let index = rand(num - 1)
  ents[index].remove Comp1
  ents[index].add Comp1()

echo sysFrag.analyseSystem()
```

Here the fragmentation is at 88%, with nearly half being forward accesses and nearly half skipping backwards in memory. This will likely perform much worse, unless all the data was already in cache.

```
Analysis for frag (100 rows of 1 components):
  comp1:
    Value size: 1 B, jump threshold: 1 B
    Jumps over threshold : 43                                         Jump ahead: 43.4343 %
    Backwards jumps      : 45                                         Jump back: 45.4545 %
    Fragmentation: 88 % (n = 88):
      Mean scale: -0.3523 times threshold
      Min: -85, max: 84, sum: -31                                     Range: 169 B
      Mean: -0.3523
      Std dev: 39.0137                                                CoV: -110.7486
      Variance: 1522.0691
      Kurtosis/spread (normal = 3.0): -0.6593 (excess: -3.6593)       Few outliers
      Skewness: 0.0711                                                Outliers trend forwards
    All address deltas (n = 99):
      Min: -85, max: 84, sum: -20                                     Range: 169 B
      Mean: -0.202
      Std dev: 36.7849                                                CoV: -182.0854
      Variance: 1353.1309
      Kurtosis/spread (normal = 3.0): -0.3686 (excess: -3.3686)       Few outliers
      Skewness: 0.0632                                                Outliers trend forwards
```

# Owned components and removing indirection

System can 'inline' component data into their work list, ensuring that each system item contains component data side by side in memory. Systems that own all of their components have no indirection and process memory in a forward access pattern. This offers a theoretically higher iteration speed, fewer cache misses, and potentially SIMD optimisations.

The practical performance advantages from this, however, depend on the nature of the system's work, the size of the components, and whether all or only some of the component data is used. This guarantee only applies to the owning system.

For components that are to be added and removed often from entities, owned components offer the advantage that their owning system will always access memory forwards when executing, even if the order of data itself may change.

> Note that systems are compacted when rows are removed, and the order of data may change.

To declare component ownership the system must be defined using `defineSystemOwner`. This takes a secondary set of components that the system will manage. Owned components must be components the system uses.

```nim
registerComponents(defaultCompOpts):
  type
    A = object
    B = object
    C = object

# This system uses components A, B, and C, and owns the storage for
# components A and C.
defineSystemOwner("owns", [A, B, C], [A, C], defaultSysOpts)
```

> **Note:** A component type can only be owned by a single system.

When a system iterates, components that it owns are inlined into the row type itself, and require no indirections to access.

As owned components are stored within the actual system and are adjacent in memory, they can be a useful tool to help maintain cache performance even with high system churn.

References to owned components outside of the owning system itself, such as in other systems or retrieved from an entity with `fetch`, are indirections into the owning system's work list and therefore retain reference semantics.

> **Note:** Owned components have the same semantics as the source type when accessed within the owning system:
> ```nim
> makeSystemBody("owns"):
>   all:
>     # Component instances used for non-owned components have reference
>     # semantics; copies of the instance will refer to the same memory
>     # location.
>     #
>     # Within an owned system the source type is used directly.
>     # As the `A` component is defined above as an object, it uses
>     # value semantics and by default Nim will copy values on
>     # assignment.
>     let aCopy = item.a
> ```

## Satisfying owner systems

When a system owns components, a new row can only be created if all of the system's owned components are present in a single state change.

Incomplete owned state results in a compile time error for `newEntityWith` or for `add`/`addComponents` that affect fully owned systems. For systems that include instanced components, a run time error occurs for `add`/`addComponents` if the owning system cannot access the required components from the entity.

These constraints also apply to `construct`, which will fail at run time if ownership cannot be satisfied.

For example:

```nim
import polymorph

registerComponents(defaultCompOpts):
  type
    A = object
    B = object
    C = object

defineSystemOwner("fullyOwned", [A, B, C], [A, B, C], defaultSysOpts)

makeEcs()

# Error: Owned component(s) [A] need their owner systems completed with component(s): [B, C]
let e1 = newEntityWith(A())

let e2 = newEntity()
# Error: Cannot add A, missing required owned component C
discard e2.add(A(), B())
```

## Deleting from owner systems

When a component is owned by a system, its storage space depends on the owning system matching the entity and cannot exist independently. If a component is removed such that an owning system is no longer satisfied, the system row is removed and along with it any other components that are owned by the system.

Therefore operations that result in removing a row from an owning system ensure that:

  1. Other owned components in the system row will also be removed (non-owned components are unaffected).
  2. If other owning systems depend on these components, they too will remove their owned components, and so on.

This process is determined at compile time based on the component types involved, and output as static code to perform the final updates.

This occurs even for indirect system dependencies, where multiple owning system share components.

For example:

```nim
# The "fullyOwned" system holds the storage for A, B, C, and D.
defineSystemOwner("fullyOwned", [A, B, C, D], [A, B, C, D], sysOpts)

# Both these systems rely on `fullyOwned` to exist as they use its components.
defineSystem("refToOwned1", [A, B, C, D], sysOpts)
defineSystem("refToOwned2", [A, B], sysOpts)

# This system requires owned components C, and D from `fullyOwned` and
# also has its own owned components, E and F.
defineSystemOwner("partiallyOwned", [C, D, E, F], [E, F], sysOpts)

makeEcs()

let e = newEntityWith(A(), B(), C(), D(), E(), F())

e.remove D

assert e.componentCount == 0

# Removing the owned component `D` invalidates the storage
# row in the owner system `sysFullyOwned`, and so components
# A, B, and C are also invalid and removed.
#
# The system `sysPartiallyOwned` owns E and F but also relies on
# indirections to C and D which are now invalid, invalidating
# this system too, and removing E and F.
#
# This leaves the entity with no components.
```

# ECS identities

Polymorph supports generating multiple ECS designs during a single compilation.

Designs are handled by *identities*. An identity is like a write-only compile time database that stores all the information required to build the ECS.

If not otherwise specified, an ECS is built with the default identity.

## Multiple ECS outputs with a single identity

After `makeEcs` has completed generating the ECS from the identity, the state is marked as processed and subsequent component/system definitions will apply to the next `makeEcs` statement.

This allows the same identity (such as the default identity) to generate multiple different ECS outputs in serial.

The resultant ECS outputs are incompatible and must be generated in separate modules, otherwise the type names will clash. This means you can't use entities from multiple ECS together, though you can communicate by sharing memory outside of entities.

The `ComponentTypeId` for components created in `registerComponents` is consecutively allocated by identity, so whilst the multiple ECS outputs are incompatible directly, the `typeId` will be unique for each component defined through the same identity.

You therefore must provide unique names for components and systems that use the same identity.

For example:

```nim
# Module1

import polymorph

registerComponents(defaultCompOpts):
  type Comp1 = object

makeSystem("test1", [Comp1]):
  all: discard

makeEcs()
```

```nim
# Module2

import polymorph

registerComponents(defaultCompOpts):
  type Comp2 = object

makeSystem("test2", [Comp2]):
  all: discard

makeEcs()
```

```nim
# Module3

import module1, module2

# Both ECS are generated.
```

## Multiple identities

You may create new identities with `newEcsIdentity`.

In most cases, you only need one identity, and the default identity is assumed.
However, it can be useful to create separate ECS identities to fully isolate an ECS design, for example for creating an internal ECS for use in library code whilst being unaffected by a user's ECS design, or simply to split out designs.

```nim
import polymorph

const myId = newEcsIdentity("myId")

myId.registerComponents(defaultCompOpts):
  type Comp1 = object

myId.makeSystem("test1", [Comp1]):
  all: discard

myId.makeEcs(defaultEntOpts)
```

# Future work

## Performance

- Optimise component metadata management.
- Allow component storage to select between 'array of struct' and 'struct of array'.
- Heuristics for inserting components and systems to minimise fragmentation with high system churn.
- Automatically group systems that interact into threads (see the [automated threading](https://github.com/rlipsc/polymorph/issues/4) issue).

## Utility

- Allow run time component and system definitions to support plugin architectures.
