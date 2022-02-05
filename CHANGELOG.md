# Changelog

## v0.3.0 2022-2-5


### Goals

- Support component negation for systems.
- Add compile time checks for event consistency.
- Modularise state change internals.

### Added

- System requirements allow component negation with the `not` prefix.

  For example:
      makeSystem "mySystem", [Comp1, not Comp2]

- Events now perform static analysis to ensure that embedded mutations
  don't invalidate other events expected within a state change. This
  means you can more safely mutate entities within events whilst having
  your design checked when you compile.

  When event mutations clash with coherent state changes a reason is
  output along with traces for both the calling event stack, and the
  event that cause the clashing mutation.

  This means you can write code like this:
  
      MyComponent.onAdd:
        entity.remove MyComponent
  
  But this raises a compile time error:

      MyComponent.onAdd:
        entity.remove MyComponent
      MyComponent.onAddCallback:
        echo "T"

  To turn this feature off, compile with `-d:ecsPermissive`.

- System owned components can be declared with the `own` prefix.

  For example:
      makeSystem "mySystem", [Comp1, own Comp2]

- `all` and `stream` blocks can now be used at any level of code within
  the system body and not just at the top level.

- `all` and `stream` blocks will raise a compile time error if they
  have been embedded within themselves (this was not possible before
  as only the top level was checked).

- Systems can now be defined with no components, allowing general
  purpose code in the system workflow.
  
  These systems have the same control features, like pausing and
  extracting to groups, but cannot use blocks that rely on entities
  such as `all`, `stream`, or add/remove events, since they don't have
  a `groups` field to store component data.

- Events now allow accessing the current entity and system with
  `entity` and `sys`, as well as `curEntity` and `curSystem`.

- The compile log (e.g., `-d:ecsLog`) now includes the source file and
  line of system definitions.

- Errors for field/option mismatch with `makeSystem` now show the
  source file and line of the original `defineSystem`.

- The error for attempting to redefine a system body with `makeSystem`
  now show the source file and line of the original `makeSystem`.

- `construct` is now `{.discardable.}`.

- `construct(quantity: int)` allows easier repeat construction of a
  `ComponentList`.

- `hasAny` takes multiple component types and returns true when any of
  them exist on the entity.

- `fetch` now supports returning multiple components to a tuple like
  the `add` operation.

- `accessType` template returns the source component type from an
  instance type.

- Improved error message when existing components are added.

- Systems now have a type class for generic access to all systems.

- `register` can be used instead of `registerComponents`.

- `makeEcsCommit` performs `makeEcs`, then `commitSystems`.

- Comping with `-d:ecsLogDetails` is now much, much more detailed.
  Operations now list their compile time invocation history, including
  output of entity mutations by operation.

- `EcsSysOptions` now includes a `threading` field that, when set to
  `sthDistribute`, will set up a `threads` `seq`. This is set to the
  number of cores when the system is instantiated. Spawning threads and
  joining them is currently up to the user. Future versions will manage
  threads for you when this option is enabled.

- All ECS operations are mangled with a unique identifier to avoid
  variable collisions and allow better operation nesting.

  Pass `-d:ecsNoMangle` to output code without appending a signature -
  Useful with `-d:ecsLogCode` to make the logged code easier to read.

- Improvements and additions to test suites such as negation testing.

### Changes

- Streamlined and standardised events. More data access within events.
  All relevant events now include `entity` access, and component events
  involving systems now get access to the system row with `item`.

- Events that have access to the `item` template now include asserts
  from `assertItem` and static `ecsStrict` checks.

- Deleting the host/calling entity within an event is now a compile
  time error. Other entities within events can still be deleted.

- `has` can now take multiple arguments: `echo entity.has(A, B, C)`.

- `delete` now parses the entity's components for systems to query,
  instead of querying all the systems.
  
  This makes destruction do work proportional to the number of run time
  components, instead of the number of compile time systems. This
  should mean `delete` is faster.

- Only `delete`, `construct` and `clone` are now generated as procs, as
  these state changes are fixed to the design. All other state changes
  are lazily constructed through generics and macros.
  
  This offers two benefits: state changes can be interleaved in events,
  and there's no time wasted on unused procs during compilation.

  The original assumption with generating procs for 'commonly used'
  operations such as 'addComponent Type()' would save compilation time
  over running a macro to generate. However, the opposite was often
  true; compile times were increased by generating procedures that
  weren't used.

- System compile time errors are checked before any macrocache updates.
  This change means you can use `when not compiles(...)` with defining
  a system without create partial system entries.

- `makeSystem` for a system previously defined with `defineSystem` now
  ignores component order when checking matching requirements.
  Note that the definition order is still semantically significant when
  a system is first defined, as this informs the order of fields within
  the system's item type.

- Component events that involve systems are announced as system events
  when `echoRunning` is active.

- Component system events for missing systems create an `error` instead
  of using `doAssert` for a cleaner message.

- Strict checking outputs specific messages for removing and deleting.

- `addComponent` is now a `template` and only constructed when used.
  This reduces the need to build the code for every single combination
  of singular adds to an entity which can constrain compilation for
  some designs.

- `EcsSysOptions.assertItem` for `defaultSystemOptions` is set to the
  value of `compileOptions("assertions")`. This means by default debug
  builds will assert valid state within `item`.

### Fixed

- The `onDelete` event is now properly exported.

- Multiple callback bodies such as `onAddCallback`,`onRemoveCallback`,
  now append to statements within a single callback proc instead of
  trying to compile multiple procs with clashing names.

- `onEcsBuilt` is now reset at the end of `makeEcs` so that future
  ECS outputs using the same identity don't output the code again.
  Unfortunately this isn't possible with `onEntityChange` because
  state changes require access after `makeEcs` has completed.

- Running systems at time intervals now applies to the entire system
  body, and will process multiple `all` or `stream` blocks.
  Previously, each `all`/`stream` would reset timings, meaning only the
  first block would be run.

### Removed

- Unused reference to `inSystemDeleteRow` macrocache entry.

- `commit` has been removed from the intercept update event (see
  breaking changes).

- Removed `newEntityTemplate` and `initComponents` as these are
  better supported by `cl()`.

### Breaking changes

- `ecsStrict` is now the default. To disable strict item checking, pass
  `-d:ecsPermissive`.

  Strict checking will produce a compile time error if a system/event
  context accesses `item` after doing something that has a possibility
  to remove/invalidate the host context. However, this check doesn't
  understand control flow or conditional statements, merely top to
  bottom evaluation, and fully run time bound operations such as
  `construct`, `clone`, `transition`, and `delete` count as affecting
  all components.

- Similarly, trying to `delete` the `entity` that invokes an event will
  now halt compilation with an error. This feature helps maintains
  state integrity when events remove the calling entity.

- The `onUpdate` event has changed:
  
  1) the `commit` proc has been removed. This event no longer allows
  ignoring data updates when `commit` is not called.
  
  The intention of the `commit` mechanism was to give the user explicit
  control over whether to apply given component data. There is an
  argument that this kind of mid-state data discarding invalidates the
  transactional data integrity of state changes for seemingly little
  value.

  The unique semantics of this event is also a potential for bugs,
  since if `commit` is not called, the data is dropped.

  Finally, the cancelling special case hasn't come up (anecdotally),
  and is probably better served with a specific buffer mechanism at the
  component level.

  2) this event is now only called when `update` is used on a component
  instance, and not in other contexts like new entities (for which you
  should use `onInit` or `onAdd`/`onAddCallback`).

- Events are now performed separately from system/entity state changes
  and should be more lenient to embedding ECS state changes within
  events. Event invocation order may differ slightly from previous
  versions.

- Within `caseSystem`, the `SystemTupleType` template was renamed
  to `ItemType`.
  
- Generation history for components with `seq` storage are now lost
  when the last item is deleted. Technically this was true before,
  however it was possible for the `seq` to be extended and if no
  reallocation was performed for old instance values, to remain and be
  incremented. This was undefined behaviour.
  
  Now, when a `seq` component slot is removed and added again, the new
  value starts at `1.ComponentGeneration`.
  
  Note that this means when storing component references for volatile
  entities, `seq` may cause reused slots at the end of the `seq` can
  clash with stored component references.

- When system timing options are set, the `sys.lastTick` field has been
  renamed to `sys.lastRun`.

## v0.2.2 2021-8-13

### Added

- `EcsEntityOptions` includes a `strDefault` option to define the default behaviour of the `$` operator for entities and components between displaying component data and just listing components.

### Fixed

- Updated mention of `Container` to `Component` in the `README.md`.
- Updated link in `README.md` for the Polymers `spaceshooter` demo.

### Improved

- Expanded the ECS introduction in the `README.md` and some clean up of headings for options.
- Nimble doesn't mention an invalid project structure.

## v0.2.1 2021-8-7

### Added

- Private scope ECS generation.
- `sysProcessed` can be used within `stream` blocks to get the number of rows/entities the block has processed.

### Fixed

- `defineSystemOwner` wasn't passing custom fields.
- `stream multipass` and `stream stochastic` no longer produce unknown identifier errors.
- `stream` blocks starting at the last item no longer skip to the first item.
- Streaming owner systems no longer process an extra row.
- Using non-component types in system requirements now produces a compile error.
- `useSet` now includes components to the set.
- The correct ident is now used internally for `transition`.
- The `onSystemAddTo` and `onSystemRemoveFrom` events no longer fail to compile in rare cases.

### Improved

- System stream parameters can be unordered.
- More features documented in the `README.md`.

### Changed

- Owner systems now index from zero. Owned component instances are **`valid` when uninitialised**, and `fetch` returns an instance set to `-1` when it cannot find an owned component.

## v0.2.0 2021-7-23

This release is a complete internal restructuring to support system-owned components, new events, and a macrocache backend. Whilst effort has been made to maintain the current API, there are some significant changes.

### Added

- **Ease of use:**

  - Improved documentation to cover all library functionality.
  
  - Added code examples from documentation to the `examples` folder.

  - More descriptive error messages.

  - `onEcsBuilt` allows you to emit code after `makeEcs` has finished. This code can use entities and ECS operations, which can be useful for utility functions and setup for systems and libraries.

  - `defaultComponentOptions`, `defaultEntityOptions`, and `defaultSystemOptions` now have shorter aliases as `defaultCompOpts`, `defaultEntOpts`, and `defaultSysOpts`.

  - `EcsEntityOptions` allows selection of error response between assertions and raising exceptions.

- **Components:**

  - `removeComponent`, `addComponent`, and `fetchComponent` can now be written `remove`, `add`, and `fetch`.

  - You can now remove multiple components in one state change with `remove`/`removeComponents`.

  - `update`/`updateComponents` supports updating multiple components with a `ComponentList`. Only existing components on the entity are updated, and others in the `ComponentList` are ignored.

  - `typeName` converts a run time `ComponentTypeId` to the string of the type it represents.

  - `componentCount` returns the number of components in use for a component type or instance type.

  - `componentStorage` lets you access the component's storage list.

  - `registerComponents` will ignore types with the `{.notComponent.}` pragma.

- **Systems:**

  - You can now write code directly in the system body that is executed when the system is run. This means blocks are no longer a requirement of system bodies and you don't need to use `start` blocks to declare variables. Another benefit is you can write system code that respects the `paused` state even when not using any blocks.

  - The `all` and `stream` blocks are now expanded within the system body. This allows multiple passes and further processing such as running code in the body between these blocks.

  - Systems now allow multiple blocks of the same type.

  - Systems passed the same component more than once will now throw an error at compile time.

  - System fields can be defined with the `fields:` block within `makeSystem`. When a system has previously been defined with `defineSystem`, the `fields:` block is checked to match this definition.
  
  - Systems now support a `stream stochastic:` block to randomly select items to process.

  - Systems can be grouped to run as separate procedures with `defineGroup`. These systems are removed from the pending output of `commitSystems` and are instead emitted with `commitGroup`. It's a compile time error to emit groups with systems that don't have bodies defined.

  - The system utility `clear` allows deleting all entities in a system.
  
  - The system utility `remove` allows removing multiple components from all entities in a system.

  - `expectSystems` checks that an entity has a specific set of systems and will `doAssert` if the entity does not satisfy these systems.
  
  - `systemsUsed` returns a string containing the systems that would be used for entities with a particular set of components.

  - `analyseSystem` provides statistical analysis of the memory addresses accessed by a system.

  - Systems include a `deleteList` `seq` that will delete any entities added to it after the system has finished executing.

  - Systems include an `id` field with the system's `SystemIndex`.

  - `caseSystem` allows generic operations based on a run time `SystemIndex` in a similar way to `caseComponent`.

  - System options given to `defineSystem` are now checked to match options passed to `makeSystemOpts`.

- **ECS identities:**

  - Multiple ECS may be generated separately using `const` identity strings.

- **System owned components:**

  - Systems can now own the storage and lifetime of components they use with `defineSystemOwner`. Owned components are inlined into the groups field of the owner system and depend on the system row to exist. When iterating, systems access their owned components in order as contiguous memory and without indirection. Access to owned components from other systems is a single indirection.

    - A component type can only be owned by one system.
    - Adding owned components to entities without meeting the full component requirements of the owner systems will result in an error.
    - Removing owned components will cascade system removal for other owned components in the system.

- **System events:**

  - When defining a system, you can now use `added` and `removed` blocks to let you process state changes in a system context. These events let you work with sets of components using `item` as if you were using an `all` or `stream` block.

  - Actions in `added` and `removed` are performed immediately as part of a state change such as `add` or `remove`, and not during the system's normal cycle. 

- **`onEntityChange` event:**

  - This event is triggered for any entity that has components added or removed.

- **Run-time construction:**

  - Creating entities with `construct` and `clone` is now more efficient and flexible.

    - These operations are now generated as integrated state changes for dynamic sets of components.
    - `construct` builds a map of types then updates all relevant systems with a single generated state change.
    - Previously `add` was called for each component, with significant overhead.
    - `clone` can elide some validity checks.

  - The `cl` macro makes building a `ComponentList` easy by allowing both source component types and container types to be mixed, whilst handling the internal `typeId` field for you. This macro also reduces boilerplate by handling a single component without having to typecast it to `Component`.

  - `construct` and `clone` now call user events.

  - Post construction events allow adding/removing components.

  - `transition` allows state machine like switching of components between two `ComponentList` parameters.

- **Misc utilities:**

  - `high(entityType: typedesc[EntityId] | typedesc[EntityRef])` returns the highest `EntityId` currently in use.

  - `EntityRef` now has a `valid` template.

- **New templates within `caseComponent`:**

  - `isOwned` returns a bool constant indicating if the component type is owned.

  - `owningSystemIndex` returns the `SystemIndex` of the owner system (or `InvalidSystem` if none).

  - `owningSystem` lets you access the component's owner system variable. Note that this is only generated for components that are owned.

  - `componentData` lets you access the component's storage list.
  
- **Compile switches:**
  
  - `ecsLog`: output ECS generation progress.
  
  - `ecsLogDetails`: output detailed ECS generation information.
  
  - `ecsStrict` includes a check at compile time to catch accessing `item` after a remove or delete has affected the system (which can potentially change what `item` refers to).

### Changed

- The library now uses the `macrocache` module to store the ECS state instead of `{.compileTime.}` variables.

- The default stream count is now `1`, changed from `10`.

- User events now run outside of state changes instead of inside them, and can rely on the state being fully defined when invoked.

- `SystemIndex` now starts from `1` and `SystemIndex(0) == InvalidSystemIndex`. Previously `InvalidSystemIndex` was equal to `SystemIndex(-1)`. This change means default (zero) values are invalid, and `SystemIndex` achieves parity with `ComponentTypeId`.

### Breaking changes

- **Removed:**

  - `makeSystemOptFields` has been removed, as it is no longer needed. Fields in `makeSystem` and `makeSystemOpts` can be defined using the `fields:` block.

  - The `tmpl` and `init` macros generated for each component have been removed. Use the `cl` macro or manually initialise component containers with `makeContainer`.
  
  - The `deleteEntity` template within `all` and `stream` blocks has been removed as it offers no value over `entity.delete`.

  - `reprocessRow` has been removed. This is now handled automatically at compile time.

  - `addFetch` has been removed. This operation was ambiguous with `addComponent` and whilst `addComponent` returns a component instance, `addFetch` returned a `ComponentRef`, which is less useful. To obtain a `ComponentRef` from an instance, use the `toRef` template.

  - `amFieldTemplates` in `EcsCompOptions.accessMethod` has been removed as it did not offer any significant functionality over `amDotOp`.

  - Within `caseComponent`, the `componentRefInit` template has been removed as these initialisers are no longer present.

- **Changed:**

  - **System execution order**: `defineSystem` now sets the order that systems are run by `commitSystems`, where previously this was set when the system *body* was defined by `makeSystem` or `makeSystemBody`. When no matching `defineSystem` exists, `makeSystem` appends order as before when it invokes `defineSystem`. This means the order must be defined before `makeEcs` is run, whereas before it could be defined after `makeEcs`.

  - `commitSystems` will only output systems that have not already been output and are not grouped.

  - The construction callback API has changed: constructor/clone callbacks now return a `seq[Component]` which is processed after the callback to ensure systems are correctly linked. Construction/clone callbacks should create the appropriate `Component` containers and add them to the `seq` result.

  - Within `caseComponent`, the template `componentInstanceIds` has been renamed `componentGenerations`.

  - The compile switch `debugSystemPerformance` is now `ecsPerformanceHints`. This option now also tracks and displays component read/write access within systems.

  - The compile switch `debugComponentGeneration` is now `ecsLogCode`.

---

## v0.1.0 2020-5-1

### Added

- **Initial ECS design:**
  - Components have storage generated from their types.
  - All component accesses are an index to their storage.
  - State change operations are generated at compile time.
