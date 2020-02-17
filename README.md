# **Polymorph**

- [**Polymorph**](#polymorph)
  - [What is Polymorph?](#what-is-polymorph)
  - [Background and motivation](#background-and-motivation)
    - [Design expression and object orientation](#design-expression-and-object-orientation)
    - [Data orientation and entity component systems](#data-orientation-and-entity-component-systems)
  - [Advantages](#advantages)
      - [Easier expression](#easier-expression)
      - [Simpler designs](#simpler-designs)
      - [Adaptable designs](#adaptable-designs)
      - [Understandable designs](#understandable-designs)
      - [Changing behaviour at run-time](#changing-behaviour-at-run-time)
    - [Performance](#performance)
  - [tl;dr](#tldr)
  - [What does Polymorph do differently](#what-does-polymorph-do-differently)
  - [Polymorph offers:](#polymorph-offers)
  - [How it looks](#how-it-looks)
  - [Using Polymorph](#using-polymorph)
    - [Core concepts](#core-concepts)
      - [How systems, entities and components interact](#how-systems-entities-and-components-interact)
      - [Instance types](#instance-types)
      - [Instances in components](#instances-in-components)
    - [Design tips](#design-tips)
    - [Anatomy](#anatomy)
    - [Defining components](#defining-components)
    - [Defining systems](#defining-systems)
  - [Using Polymorph](#using-polymorph-1)
    - [Event patterns](#event-patterns)
  - [Polymorph anatomy](#polymorph-anatomy)
    - [Adding components](#adding-components)
  - [Limitations and improvements](#limitations-and-improvements)
    - [Additive systems only](#additive-systems-only)
    - [One indirection away.](#one-indirection-away)

## What is Polymorph?

Polymorph is a composable entity component system generator.

You can use it to write data orientated software and libraries that offer efficient run time composability, extendible and maintainable designs, and high performance.

## Background and motivation

### Design expression and object orientation

In object oriented programming, an overarching design goal is to capture the essence of different parts of a problem space and describe them as objects. Objects are self-contained with an internal data state and attached functionality for working with this internal state. These objects can then interact or subclass, and therefore allow uniform interfaces, modular parts, and code reuse.

However, describing the 'thingness' of interacting problem parts is often a hard problem, philosophically speaking. Even worse, fully elucidated object orientation can crystallise a design, introducing a high cost of change. Yet we're driven to do so, because vague objects don't help us express solutions.

Describing what a thing *is*, even in an abstract sense, can serve to constrain expression. When aiming for uniform interfaces and code reuse with inheritance, we can instead get incompatible ancestries and hard to understand, compile-time opaque virtual hierarchies that demand extra effort to extend and maintain.

Inheritance trees guide child objects toward increasing specialisation, and static composition fixes an object's capabilities at design time. Careful decisions must be made constantly through development as to which assumptions are currently being baked into the program's structure.

Perhaps the apex of this issue can be encountered as [the Deadly Diamond of Death](https://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem), making the conceptual design of large interacting systems a critical, yet extremely difficult process to get right.

It's usually possible to find some kind of work arounds. Sometimes, however, they come at significant development cost, such as having to refactor large portions of code due to what seems like a simple unexpected customer requirement or unforeseen hierarchy design clashes three years into a project stunting a new feature. In other cases, conceptualising objects at all hinders solving the problem by imposing a structure that isn't there.

Of course, objects aren't the only way we can conceptualise a problem space. Rather than a top down approach of hierarchies of behaviour specialisations, we can apply a bottom up approach using data itself to directly invoke associated behaviour.

### Data orientation and entity component systems

Instead of describing "what" something is and classifying it's abilities, we can describe the behaviours and attributes themselves and combine them together to get the desired result.

To do this we have a simple id number called an `entity` that we allow pure data types (termed `components`) to be tagged to.

Logic `systems` then 'subscribe' to which components they want to use and when matching components are added to an entity, the entity gets added to that system's work list.

Data is now entirely separated from code, and what data an entity has linked to it dictates what systems will run.

## Advantages

#### Easier expression

This approach often ends up being a more natural way to express a problem - for example it's easier and more useful to describe the appearance and behaviour of a cat than describe it by building a taxonomy of animal abstractions. Describing other things can be done by mixing, matching and adding new attributes. Code is shared by the attribute, which can be as granular as you like so there's lots of opportunity for reuse.

#### Simpler designs

With a flat architecture, we no longer need to care about what a thing **is** or how it relates to other things, and can instead focus on writing behaviours. This opens doors in how an implementation can be approached, as we now get a huge increase in both design time and run time flexibility.

#### Adaptable designs

Changing core designs is now massively simpler. Creating or extending existing functionality can be done by adding new components and/or systems in total isolation, and no need for mocking frameworks when you can drive systems directly and organically by just adding the component to be consumed.

Entity component systems also allow several programming paradigms at the same time, with the same interface, without spaghetti code. Components can be tags, descriptors, messages, events, data storage, observers, dispatchers, or all of these things at once, without any interference.

#### Understandable designs

Despite the ability to completely change entity function at run-time, it becomes much easier to reason about behaviour as you're looking directly at the fragments of behaviour rather than trees of potential behaviour.

Control flow also becomes easier to reason about as each system is statically defined and executed in a set sequence by default. Systems can be turned on or off individually, paused, and worked on independently.

#### Changing behaviour at run-time

Changing an entity's components at run time allows rapid prototyping of different features as well as some designs that are impossible without run-time composition.

We could easily remove some `ReadKeyboard` component temporarily - or attach it to a completely different entity without prior design, or swap a `RenderScreen` component for `RenderFile`, or `RenderPrinter` - or just pause the whole render system independently. We can split features by using tag components to be added/removed at run-time, or tie systems to trigger on a `IncomingPacket` by simply including the event component in a system's requirements.

Components can be used to efficiently trigger further processing, tags to signal a result, or pass messages.

### Performance

These design advantages come with a nice implementation side effect; since we're batch processing things in systems we now have a much lower overhead due to:

* Better cache use - Computers love repeating operations. Running systems pull on the same component storages repeatedly, naturally promoting cache population.

* Eliminate virtual call overhead - Virtual table lookups can require several indirections to get to data or code per object access. On top of that, the pointer chasing makes some optimisations much more difficult. Polymorph's components are always one index lookup away into packed lists of components for that type.

* Better compilation results - Systems process their data in tight loops making optimisations such as vectorisation and unrolling easier for the compiler to spot.

* Value types - Since there's no need for inheritance your components can exist on the stack, which can offer a substantial increase in performance over heap allocation.

## tl;dr

Polymorph takes some data types and code that takes these types, and generates an efficient interface so you can add these types to an id number. Code with parameters that are satisfied by the types on the id automatically has it's work list updated with these types.

Polling the generated code then causes any systems with work to be run.

## What does Polymorph do differently

Polymorph allows you to compose fragments of data orientated designs together at compile time, then 'seal' them into an efficient static ECS. This approach allows reusable libraries of components that can be shared and combined together to create larger collections. These in turn can be composed together and so on, offering metacomposition at compile-time with a uniform API.

This means you can build say, component/systems that read & write to the `terminal`, and independent components/systems for `database` access, and someone else can import both and just add that components to an entity and it just works. In fact, [ecs_consoleevents](https://github.com/rlipsc/polyshards/blob/master/console/ecs_consoleevents.nim) and [ecs_db_threads](https://github.com/rlipsc/polyshards/blob/master/db/ecs_db_threads.nim) are both usable example in the polyshards repo, along with a [simple database browser](https://github.com/rlipsc/polyshards/blob/master/demos/dbbrowserthreads.nim) program that uses both.

Focused on efficiency and minimum boiler plate, Polymorph tries to do as much work at compile-time as possible to generate code that only does what it absolutely has to. For example adding sets of components to an entity generates static updates for only the union of systems that use them - no need for run-time checks, searching for matching systems, or sorting.

This means you can pick and choose what to include in your ECS and performance adapts to your design, allowing everything from tiny event tickers where storage must be tightly controlled to massive dynamic simulations with millions of entities.

Polymorph offers granular control over code generation, whether to use arrays, seqs or hash tables for component/entity/system storage, make tradeoffs between query performance and storage space, which systems are defined as `threadVar`, and a host of other options.

All generated code can be logged to a file for viewing so you can see exactly what is happening.

Despite being almost entirely metaprogramming, compilation is fast even with large projects thanks to the Nim compiler. Even the stack traces on errors are comprehensible!

## Polymorph offers:

* Rapidly create ***run-time dynamic***, easily ***extendible***, and ***maintainable programs*** with a low boilerplate, pared down API.
* A naturally ***data orientated*** design allows effortless composition at run-time.
* Implicitly ***non-blocking*** operation supporting async operations naturally.
* ***Deterministic*** with a non-branching, forward logical control flow.
* ***High performance*** batch processing for free, cache and optimisation friendly, and has ***no virtual call overhead***.
* ***Exportable ECS fragments***, allowing shareable libraries of reusable components and systems.
* All ECS code is ***built entirely from your types and systems at compile time*** so you only pay for what you use.
* ***Type safe*** and no pointers or hard casting required.
* High degree of ***control over individual systems*** lets you switch on or off functionality at run time, adaptively stream entities, or run systems at set intervals.
* ***Transparent mocking*** - Since data state is fully in the component, we get the ability to mock anything for free by simply adding the effect we want directly, innately triggering system behaviour.
* Entities can be ***constructed from template lists***, cloned, or made into templates at run-time.
* Full complement of callback ***events*** and inline pseudo-events that are statically included in generated code.
* Built in ***debugging features*** such as outputting entities and triggered systems to string/console, and individual system checkpoints.
* Separate callback initialisers for template constructions. This gives the flexibility to for example, store templates of components that generate different components upon construction. 

## How it looks

```nim
import polymorph

# Uses typedefs passed as components
registerComponents(defaultComponentOptions):
  type
    Data = object
      value: int
    Inc = object
    Multiply = object
      amount: int

# Create systems to act on the components

makeSystem("addOne", [Data, Inc]):
  # `item` is generated from the types passed within `[]`
  all: item.data.value += 1

makeSystem("multiply", [Data, Multiply]):
  all: item.data.value *= item.multiply.amount

# Seal and generate ECS
makeEcs()
commitSystems("run")

let
  entity = newEntityWith(Data(value: 4), Inc(), Multiply(amount: 2))
  # Fetching returns a component reference.
  # These are typed indexes into storage rather than pointers/gc refs.
  data = entity.fetchComponent Data

run()

# Fields in a reference can be accessed as if they were the source type
assert data.value == 10

# Generated `$` means we can display the state of the entity easily.
echo entity

# echo outputs:

# [EntityId: 1 (instance: 1)
# Alive: true
# Components:
# Data (id: 1, index: 1, generation: 1) Data: [value = 10]
# ----
# Inc (id: 2, index: 1, generation: 1) Data: []
# ----
# Multiply (id: 3, index: 1, generation: 1) Data: [amount = 2]
# ----
# Systems:
# aaddOne (sysAddOne) 
# multiply (sysMultiply) 
# ]
```

## Using Polymorph

### Core concepts

#### How systems, entities and components interact

* Systems are simple loops iterating through a work list. They don't actively search/match entities or components.
* Systems are **only** updated when an entity adds or removes a relevant component. Where possible, this is determined at compile-time.
* A system always has a `groups` field that represents it's work list. When an entity adds a component and a system is now satisfied, the list is updated with the entity reference and the instance types of the components that it satisfies.
* The work list is a tuple defined by `defineSystem` or `makeSystem` from the types passed.
* Systems may add or delete entities/component whilst running, and even delete the system's current entity whilst iterating. Polymorph can tell when a system modifies itself whilst running (for example, removing a component the system uses) and will insert an extra length check per iteration at compile-time. A warning message is generated for this when `-d:debugSystemPerformance` is passed.

#### Instance types

These distinct types are generated from components passed to `registerComponents`. They are simply the index into the component's storage list/table.

You'll see instances everywhere in Polymorph. They are how components are referenced within systems, and `fetchComponent` returns them. Any 'live' component is represented by an instance.

Polymorph overloads the `.` and `.=` operators so that these instance types can be used exactly as you would the real data type. This is done by simply transforming the instance into the code to lookup into storage. 

```nim
import polymorph
registerComponents(10):
  type
    MyComp = object
      text: string
      value: int
    AnotherComp = object
      value: float

makeEcs()

let
  # Create components directly 
  myComp = genMyComp()
  anotherComp = genAnotherComp()
# myComp is an index into component storage
myComp.text = "Hello"
# for an array/seq becomes something like:
# storageMyComp[myComp.int].text = "Hello"
myComp.value = 17
# storageMyComp[myComp.int].value = 17
anotherComp.value = 5.5
# storageAnotherComp[anotherComp.int].value = 5.5

assert myComp.value == 17
# assert storageMyComp[myComp.int].value == 17
```

For the majority of cases, you can treat instance types as if they were a `ref` type; they are simply numbers so they're cheap to pass about. Looking up the index to storage is one indirection with list storages, and that indirection should have a good chance of being in the cache assuming linear component allocation pattern.

Sometimes, however, you want access to the underlying type and not use an indirection. For this, there is the `access` template, which also transposes the storage lookup but for the whole component type.

```nim
  let copyMyComp = myComp.access
  # Translates to something like this
  let copyMyComp = storageMyComp[myComp.int]
```

You can also update components in one go using instances:

```nim
let myComp = entity.fetchComponent MyComp
myComp.update(MyComp(text: "Hey there", value: 18))
```

To check if instances exist, use the `valid` template.

```nim
let myComp = entity.fetchComponent MyComp
if not myComp.valid:
  echo "Cannot find MyComp on this entity:\n", entity
```

#### Instances in components

Polymorph inserts the definitions for the instances generated from components before the component definitions themselves. This allows you to use them within component definitions, and have type safe many-to-one links to components.

```nim
import polymorph
registerComponents(10):
  # The types are scanned and instance definitions inserted here,
  # then this type block is added. 
  type
    A = object
      b: BInstance
    B = object
      a: AInstance
```

### Design tips

In terms of performance, Polymorph favours **system iteration** over **entity modification/generation**.

Creating/deleting entities, adding/removing components from entities do work proportional to the complexity of your design, and whilst every effort is made to ameliorate this by only generating code for systems touched by the update, if you have a hundred systems that use a component with no other requirements, that's a hundred systems that need updating each time the component gets added to, or removed from an entity.

The good news is, if you do have a hundred systems using a component, and no other requirements, that's a hundred quick updates! In practice, you'll probably use multiple components for systems.

### Anatomy

There are three main parts to defining an ECS.

* `registerComponents`: this creates storage, access procs, and instance types from the typedefs passed.
* `defineSystem`

There are three stages in compilation with Polymorph:

* Before `makeEcs`:
  * You are free to `registerComponents`,  `defineSystems` and `makeSystem`.
* After `makeEcs`:
  * Component types and system definitions are sealed.
  * Can now use entities and add/remove components.
  * Can still add the code body for systems defined before `makeEcs` using `defineSystem`. The order they are defined in matters if you're using the wrapped proc from `commitSystems`, otherwise you can call systems in any order you like.
* After `commitSystems`: The system procs are output and optionally wrapped in a 'run all' proc. The ECS is now fully defined. 

Before, you are free to `registerComponents` and `defineSystems`.

After `makeEcs`, registering components and defining systems don't contribute to the ECS, as it would need to be regenerated.

### Defining components

Creating components is as simple as wrapping their definition with `registerComponents`. This will extract any `typedef` and pass the block through untouched.

`registerComponents` takes two parameters:

* Either:
  * the maximum number of concurrent components for this block.
  * or an `ECSCompOptions` object. This contains detailed settings for code generation.
* A block of code with typedefs in it.


For example, to create three components, `A`, `B`, and `C`.

```nim
import polymorph

registerComponents(20):
  type
    A = object
    B = object
    C = object
```

You can invoke `registerComponents` multiple times before actually constructing the  ECS, this helps allow splitting definitions into separate modules.

### Defining systems

Systems are what actually do the work. They are essentially procs which loop over a work list.

There are two main ways to define systems, as a prototype with `defineSystem`, or declare inline with `makeSystem`. Both of these take a list of component types.

Systems are split into three sections:

* `init:` This is run when the system's `initialised` flag is unset, and after this block `initialised` is set to true. You can manipulate `initialised` to trigger this block.
* `start:` Run every time a non-disabled system begins. Do any setup required outside of looping here, or skip the `all:` block by setting `sys.paused = true`.
* `all:` This is usually where the crux of processing happens. This block is given an `item` template to reference the current and is executed for every row in the work list.
* `finish:` Any work that needs to happen after items have been looped.

## Using Polymorph

Using an entity component system can be quite a change of pace from object orientated programming.

In a sense, the whole ECS is like one big state machine, where systems turn wheels of components.

### Event patterns

Components work quite well to signal some occurrence.
As the system loops are cooperatively non-blocking, we can write asynchronous responses that are
activated upon a certain component, or combination of components being present.
Once the component has been finished with, it can be removed.

```nim
  import polymorph, random, times

  registerComponents(defaultComponentOptions):
    type
      RandomValue = object
        valueRange: HSlice[int, int]
        target: int
      ValueOccurred = object
        value: int

  # Generate 'event'
  makeSystem("randomValues", [RandomValue]):
    all:
      let rVal = item.randomValue
      if rand(rVal.valueRange) == rVal.target:
        item.entity.addComponent ValueOccurred(value: rVal.target)

  # Consume 'event'
  makeSystem("hasValue", [ValueOccurred]):
    all:
      echo "Value ", item.valueOccurred.value, " has occurred at ", getTime()
      item.entity.removeComponent ValueOccurred

  makeEcs()
  commitSystems("run")

  let e = newEntityWith(RandomValue(valueRange: 0..5_000_000, target: 4))
  while true:
    run()
```

## Polymorph anatomy

Entities: Distinct int into a structure containing a list of components.

### Adding components

Polymorph relies on static typing to make efficiency savings. Whenever you add a component, you're also implicitly declaring what systems you want to update based on `defineSystem`/`makeSystem`. This means, perhaps ironically, that virtual polymorphism is a hindrance in Polymorph.

There are several ways to add components to entities in increasing performance.

Currently only the 'real' component types (ie; what you pass to `registerComponents`) can be added to entities. Originally, both user types and generated instance types could be added but it introduced ambiguity that can cost in run-time performance so it's been removed - instead you can clone a live component's instance using `instance.access`.

Components are assigned to storage using `=`, so copy semantics depend on your component's type; for example `object` will copy the object, `ref`/`pointer` will copy the link to the object.

1) `newEntityWith`: This macro takes a list of components, calculates what systems will need to be updated at compile time, and inserts code to generate a new entity and only update those systems. This performs the least amount of run-time work because it knows the passed components are the only ones the entity currently has.
2) `addComponents`: This macro also takes a list of components and calculates the union of the systems it needs to update at compile-time. However like the singular `addComponent`, it must include any systems that use any of the parameter components, in case the entity currently also satisfies these systems (whereas `newEntityWith` knows there are no other components except the parameters). This is faster than individually adding several components because the parameters provide a minimum set of system updates that we can statically generate. So, if we add twenty components that are only used on one system, just the update for that system will be generated.
3) `addComponent`: Singular. This proc statically knows the type you're adding, and generates code for just systems that are defined to use that type. For each of these systems the run-time components are checked and when matched, a row is added.

## Limitations and improvements

### Additive systems only

Polymorph currently only allows additive systems. This means they can only specify "Match all these components", not "Match these components but not if we have these". Adding this feature is relatively straight forward to implement, I just haven't personally needed it yet as the same effect can be usually be achieved by adding a tag component. There's something in this additive only approach I quite like - perhaps it's that it forces you to simply system and component interaction and the extra cognitive load of considering the absence of components.

Having said that, I will add this feature sooner or later because it can simply some approaches and probably will help keep the number of components down. Besides, you could still take an additive only approach with this implemented.

### One indirection away.

Currently accessing components from systems is by default an index lookup. This is usually faster than a pointer indirection mainly because it offers more optimisation opportunities for the compiler (such as SIMD), as indexes are more constrained than a raw pointer. However, nothing is as fast as directly working on memory in a big block, especially if your components are small (so access overhead matters).

Of course, the caveat is that indirection can only be removed for one system, in order to keep mutable access synchronised without copying.

To tackle this, I plan to allow for systems to declare they "own" components, this will cause the system's tuple to act as storage and contain the full type instead of an instance, and code generated for creating/removing and accessing these components is replaced with the same operations directly on the system's tuple.

In this way, you can optimise critical systems without changing any other code.

