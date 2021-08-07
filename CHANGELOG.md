# Changelog

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
