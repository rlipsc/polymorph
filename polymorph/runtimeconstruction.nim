
import macros, private/[ecsstateinfo, utils], sharedtypes, tables
from strutils import toLowerAscii

proc buildConstructionCaseStmt(entity: NimNode, entOpts: ECSEntityOptions, cloning: bool): tuple[core, userDecls, userCode: NimNode] =
  ## Build a case statement to handle updating systems linked to any component
  ## found in the input list.
  let
    compIndexInfo = ident "curCompInfo"
    visited = ident "visited"
    types = ident "types"

  var
    compCase = nnkCaseStmt.newTree()
    userCode = newStmtList()
    userCodeBoolDecls: Table[SystemIndex, NimNode]

    # Fetch array of component id by list of system indexes.
    systemsByCompId = compSystems()

  # Select on typeId.
  compCase.add(quote do: `compIndexInfo`[0].int)

  for typeId in ecsComponentsToBeSealed:
    template ofCompInfo: untyped = typeInfo[typeId.int]
    let
      linkedSystems = systemsByCompId[typeId.int]
      ofInstType = ident ofCompInfo.typeName.instanceTypeName
    var
      addToSystems = newStmtList()
      ofBranch = nnkOfBranch.newTree()
      ofStmts = newStmtList()
      
      userAddComponent = newStmtList()

    ofBranch.add newLit typeId.int

    let
      source = 
        if cloning:
          quote do: `compIndexInfo`[1]
        else:
          quote do: `compIndexInfo`[1][1]

    # User callback.
    if ofCompInfo.onAddCallback.len > 0:
      # Check for this type's initialiser.
      let cbProcName = ident addCallbackName(ofCompInfo.typeName)
      userAddComponent.add(quote do:
        `cbProcName`(`entity`, `ofInstType`(`source`))
      )

    # Component add user code.
    let userAddToEnt = ofCompInfo.onAddToEntCode
    if userAddToEnt.len > 0:
      userAddComponent.add(quote do:
        block:
          ## Current component being added to entity. 
          template curComponent: untyped {.used.} = `ofInstType`(`source`)
          `userAddToEnt`
      )

    for sys in linkedSystems:
      template sysInfo: untyped = systemInfo[sys.int]
      let
        requiredCompsLen = sysInfo.requirements.len
        systemStr = sysInfo.systemName
        sysTupleType = ident tupleName(systemStr)
        sysTupleVar = ident tupleName(systemStr).toLowerAscii
        systemNode = sysInfo.instantiation
        newRow = ident "newRow"
        # This is recorded in the match block and used for user events.
        sysUserCodeIdx = ident "run" & systemStr & "UserCodeIdx"

      var
        matchList = newSeqOfCap[NimNode](requiredCompsLen)
        sysFields = newStmtList()
        stateUpdates = newStmtList()
        assertions = newStmtList()
        userSysAddCode = newStmtList()

      if requiredCompsLen > 0:
        # Generate code to check the system is satisfied,
        # and code to do the assignment of the tuple elements.

        for comp in sysInfo.requirements:
          template compInfo: untyped = typeInfo[comp.int]
          let
            typeStr = compInfo.typeName
            typeField = ident typeStr.toLowerAscii
            instType = ident typeStr.instanceTypeName
            refType = ident typeStr.refTypeName
            ownedByThisSystem = compInfo.systemOwner == sys
            compId = comp.int

          if comp != typeId:
            if ownedByThisSystem:
              # It's a run time error to not specify all the owned components.
              let caseTypeIdStr = typeInfo[typeId.int].typeName
              assertions.add quote do:
                assert `types`.hasKey(`comp`),
                  "Cannot construct: Specified owned component \"" & `caseTypeIdStr` &
                  "\" also needs owned component \"" & `typeStr` & "\""
            else:
              # if statement clause fragment.
              matchList.add quote do:
                `types`.hasKey(`comp`)

          let source =
            if comp == typeId:
              quote do: `compIndexInfo`[1]
            else:
              quote do: `types`[`compId`]

          # Handle system's `added` block.
          #let
            #[curComp =
              if cloning: quote do: `instType`(`source`)
              else: quote do: `instType`(`source`[1])]#
          
          # TODO: Check cloning works with this
          userSysAddCode.addUserSysCode(entity, sys, comp, quote do:
            `systemNode`.groups[`sysUserCodeIdx`].`typeField`)

          # Assignment of the tuple field for this component.
          if not cloning:
            if ownedByThisSystem:
              # Owned fields also need their state initialised.
              stateUpdates.add updateOwnedComponentState(comp, sys, newRow)
              # Assignment source is the parameter ref type.
              sysFields.add(quote do:
                `sysTupleVar`.`typeField` = `refType`(`source`[0]).value)
            else:
              # We already have the Instance type stored for unowned components.
              sysFields.add(quote do:
                `sysTupleVar`.`typeField` = `instType`(`source`[1]))
          else:
            if ownedByThisSystem:
              # Owned fields also need their state initialised.
              stateUpdates.add updateOwnedComponentState(comp, sys, newRow)
              # Assignment source is copied from the owned component.
              sysFields.add(quote do:
                `sysTupleVar`.`typeField` = `instType`(`source`).access)
            else:
              # We already have the Instance type stored for unowned components.
              sysFields.add(quote do:
                `sysTupleVar`.`typeField` = `instType`(`source`))

      let
        sysOpts = systemInfo[sys.int].options
        updateIndex = entity.updateIndex(sys, newRow, sysOpts)
        addToSystem = addToSystemTuple(systemNode, sysTupleVar, sysOpts)
        hasUserAddToSysCode = userSysAddCode.len > 0
        hasUserAddedBlock = systemInfo[sys.int].onAdded.len > 0
        hasUserSystemCode = hasUserAddToSysCode or hasUserAddedBlock

        setUserCodeIdx =
          if hasUserSystemCode: quote do:
            `sysUserCodeIdx` = `newRow`
          else: newStmtList()
        
        matchCode = quote do:
          if not `visited`[`sys`.int]:
            `visited`[`sys`.int] = true
            `assertions`
            var `sysTupleVar`: `sysTupleType`
            `sysTupleVar`.entity = `entity`
            `sysFields`
            `addToSystem`
            let `newRow` = `systemNode`.high
            `updateIndex`
            `stateUpdates`
            `setUserCodeIdx`

      if hasUserSystemCode and sys notin userCodeBoolDecls:
        # Both events for adding to systems and the `added:` block within a system are invoked
        # in their own block, after state construction.
        userCodeBoolDecls.add sys, newIdentDefs(sysUserCodeIdx, ident "int", newLit -1)

        let addedEvent = sys.sysInvokeAdded(sysUserCodeIdx)

        userCode.add(quote do:
          if `sysUserCodeIdx` >= 0:
            `addedEvent`
            `userSysAddCode`
        )
      
      if matchList.len > 0:
        # Wrap matchCode with if statement so we only add when the system is satisfied.
        let sysMatchClause = genInfixes(matchList, "and")
        addToSystems.add(newIfStmt( (sysMatchClause, matchCode) ))
        
      else:
        # No conditions, always add to this system.
        addToSystems.add matchCode
    
    let
      hasSystems = addToSystems.len > 0
      hasUserComponentEvents = userAddComponent.len > 0

    if hasSystems or hasUserComponentEvents:
      if hasSystems:
        ofStmts.add addToSystems
      if hasUserComponentEvents:
        ofStmts.add userAddComponent
      ofBranch.add ofStmts
      compCase.add ofBranch

  compCase.add nnkElse.newTree(newStmtList(quote do: discard))

  var decls = nnkVarSection.newTree()
  for node in userCodeBoolDecls.values:
    decls.add node

  (compCase, decls, userCode)

proc makeRuntimeConstruction*(entOpts: ECSEntityOptions): NimNode =
  ## Builds procedures for creating entities from component lists
  ## at run time.
  let
    cList = ident "componentList"
    construction = ident "construction"
    master = ident "master"
    visited = ident "visited"
    types = ident "types"
    compIndexInfo = ident "curCompInfo"
  let
    maxSys = systemInfo.len
    reference = ident "reference"
    res = ident "result"
    addToEnt = addComponentRef(res, reference, entOpts)
    tcIdent = ident typeClassName()
    iterVar = ident "i"
    component = ident "component"
    compIdx = ident "compIdx"
    entity = ident "entity"
    typeId = ident "typeId"
    callback = ident "callback"

    constructCaseInfo = buildConstructionCaseStmt(res, entOpts, false)
    userConstructEvents = constructCaseInfo.userCode
    userDecls = constructCaseInfo.userDecls
    constructCase = constructCaseInfo.core

    cloneCaseInfo = buildConstructionCaseStmt(res, entOpts, true)
    cloneCase = cloneCaseInfo.core
    userCloneEvents = cloneCaseInfo.userCode
    userCloneDecls = constructCaseInfo.userDecls

    entId = quote do: `entity`.entityId
    entCompCount = componentRefsLen(entId, entOpts)

    # Assumes components are added in ascending order.
    lowCompId = ecsComponentsToBeSealed[0].int
    highCompId = ecsComponentsToBeSealed[^1].int
    curEntTempl = 
      quote do:
        template curEntity: EntityRef {.used.} = `res`

  result = quote do:
    var
      # Note: As zero is an invalid component, a component count of eg 5 indicates valid indices of 0..5, not 0..4.
      manualConstruct: array[`lowCompId`..`highCompId`, ConstructorProc]
      # Post constructors are called after an entity template has been constructed.
      postConstruct: array[`lowCompId`..`highCompId`, PostConstructorProc]
      # Clone constructors allow custom handling of components by type when `clone` is called on an entity.
      cloneConstruct: array[`lowCompId`..`highCompId`, CloneConstructorProc]

    # Do not rely on the order a callback is invoked when constructing templates

    proc registerConstructor*(`typeId`: ComponentTypeId, `callback`: ConstructorProc) = manualConstruct[`typeId`.int] = `callback`
    template registerConstructor*(t: typedesc[`tcIdent`], `callback`: ConstructorProc) = registerConstructor(t.typeId, `callback`)

    proc registerPostConstructor*(`typeId`: ComponentTypeId, `callback`: PostConstructorProc) = postConstruct[`typeId`.int] = `callback`
    template registerPostConstructor*(t: typedesc[`tcIdent`], `callback`: PostConstructorProc) = registerPostConstructor(t.typeId, `callback`)

    proc registerCloneConstructor*(`typeId`: ComponentTypeId, `callback`: CloneConstructorProc) = cloneConstruct[`typeId`.int] = `callback`
    template registerCloneConstructor*(t: typedesc[`tcIdent`], `callback`: CloneConstructorProc) = registerCloneConstructor(t.typeId, `callback`)

    proc construct*(`cList`: ComponentList, `master`: EntityRef = NO_ENTITY_REF): EntityRef =
      ## Create a runtime entity from a list of components.
      ## The user may use `registerCallback` to control construction of particular types.
      ## When called from a `ConstructionList`, `master` is set to the first entity constructed.

      `res` = newEntity()
      # master defaults to the current entity if nothing specified
      let
        masterRef = if `master`.entityId != NO_ENTITY:
          `master`
        else:
          `res`
      # Build an index to component list, which helps speed things
      # up when checking systems.
      var
        `types`: Table[int, tuple[`component`: Component, `compIdx`: ComponentIndex]]
        `visited`: array[`maxSys`, bool]
      `curEntTempl`

      for compRef in `cList`:
        assert compRef.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $compRef.typeId.int
        assert not `types`.hasKey(compRef.typeId.int), "Cannot construct: Entity has duplicate components for " & $compRef.typeId
        var `reference`: ComponentRef

        # Index and unowned component generation.
        caseComponent compRef.typeId:
          # trigger construction callback if present
          let cb = manualConstruct[compRef.typeId.int]
          if cb != nil:
            # The callback is responsible for adding reference(s) from the source entity.
            # Callbacks may add multiple sets of components or none at all.
            let compsAdded = cb(`res`, compRef, masterRef)
            # We must now include manually added components to the index so
            # that systems are updated in the second stage below.
            # It is still possible to add owned components, and their
            # dependencies will be checked in the second pass.

            # We now have to work out which types have been returned.
            for comp in compsAdded:
              assert comp.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $comp.typeId.int
              assert not `types`.hasKey(comp.typeId.int), "Cannot construct: Entity has duplicate components for " & $comp.typeId
              caseComponent comp.typeId:
                when owningSystemIndex == InvalidSystemIndex:
                  `reference` = newInstance(componentRefType()(comp).value).toRef
                else:
                  # This reference isn't valid until we add the tuple in the second pass.
                  `reference` = (
                    componentId(),
                    owningSystem.count.ComponentIndex,
                    1.ComponentGeneration
                  )
                `addToEnt`
                `types`.add(comp.typeId.int, (comp, `reference`.index))
          else:
            when owningSystemIndex == InvalidSystemIndex:
              `reference` = newInstance(componentRefType()(compRef).value).toRef            
            else:
              # This reference isn't valid until we add the tuple in the second pass.
              `reference` = (
                componentId(),
                owningSystem.count.ComponentIndex,
                1.ComponentGeneration
              )
            `addToEnt`
            `types`.add(compRef.typeId.int, (compRef, `reference`.index))

      # Update systems.
      `userDecls`
      for `compIndexInfo` in `types`.pairs:
        `constructCase`
      `userConstructEvents`

    proc construct*(`construction`: ConstructionTemplate): seq[EntityRef] =
      ## Constructs multiple entities and returns their entity ids.
      ## The first entity in the list is passed to the others as the "master".
      ## This same entity is also passed to each individual component's constructor,
      ## this allows components to have some reference to their construction environment.
      ## For example, the first entity can contain a physics body component that others may
      ## reference.
      ## No other structure is assumed, and the meaning of 'master' is defined by the user.
      ## Components are constructed in order, calling manual construction code per type,
      ## then a second pass calls post construction procs with reference to the completed component
      ## lists.
      ## Post construction procs are fed the fully constructed entity and it's existing component,
      ## along with the rest of the constructed entities in this template.
      ## This allows fetching components to read/modify initialised values.

      if `construction`.len > 0:
        result.setLen(`construction`.len)
        result[0] = `construction`[0].construct(NO_ENTITY_REF)

        for `iterVar` in 1 ..< `construction`.len:
          result[`iterVar`] = `construction`[`iterVar`].construct(result[0])

        # Re-parse components to check for and activate post construction callbacks.
        # This post-step allows the user to do any multi-entity work that may be required
        # such as physics linkage and so on.
        # You can add/remove components within callbacks here.
        var `iterVar`: int
        while `iterVar` < `res`.len:
          let `entity` = `res`[`iterVar`]
          var compIdx: int
          while compIdx < `entCompCount`:
            let
              compRef = entityData(`entity`.entityId).componentRefs[compIdx]
              tId = compRef.typeId
              pc = postConstruct[tId.int]
            if pc != nil:
              # Callback passes this entity and the fully constructed result array.
              pc(`entity`, compRef, `res`)
            compIdx += 1
              
          `iterVar` += 1

    proc toTemplate*(`entity`: EntityRef): seq[Component] =
      ## Creates a list of components ready to be used for construction.
      assert `entity`.alive
      let length = `entCompCount`
      `res` = newSeq[Component](length)
      for i, compRef in `entity`.entityId.componentPairs:
        caseComponent(compRef.typeId):
          `res`[i] = componentInstanceType()(compRef.index).makeContainer()

    proc clone*(`entity`: EntityRef): EntityRef =
      ## Copy an entity's components to a new entity.
      ## Note that copying objects with pointers/references can have undesirable results.
      ## For special setup, use `registerCloneConstructor` for the type. This gets passed
      ## the clone type it would have added. You can then add a modified component or 
      ## entirely different set of components, or ignore it by not adding anything.
      assert `entity`.alive, "Cloning a dead entity"

      `res` = newEntity()
      # Build an index of component type to its instantiated instance.
      var
        `types`: Table[int, ComponentIndex]
        `visited`: array[`maxSys`, bool]
      `curEntTempl`

      for compRef in `entity`.components:
        assert compRef.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $compRef.typeId.int
        assert not `types`.hasKey(compRef.typeId.int), "Cannot construct: Entity has duplicate components for " & $compRef.typeId
        var `reference`: ComponentRef

        # Index and unowned component generation.
        caseComponent compRef.typeId:
          # trigger construction callback if present
          let cb = cloneConstruct[compRef.typeId.int]
          if cb != nil:
            # The callback is responsible for adding reference(s) from the source entity.
            # Callbacks may add multiple sets of components or none at all.
            let compsAdded = cb(`res`, compRef)
            # We must now include manually added components to the index so
            # that systems are updated in the second stage below.
            # It is still possible to add owned components, and their
            # dependencies will be checked in the second pass.

            # We now have to work out which types have been returned.
            for comp in compsAdded:
              assert comp.typeId != InvalidComponent, "Cannot construct: invalid component type id: " & $comp.typeId.int
              assert not `types`.hasKey(comp.typeId.int), "Cannot construct: Entity has duplicate components for " & $comp.typeId
              caseComponent comp.typeId:
                when owningSystemIndex == InvalidSystemIndex:
                  `reference` = newInstance(componentRefType()(comp).value).toRef            
                else:
                  # This reference isn't valid until we add the tuple in the second pass.
                  `reference` = compRef
                `addToEnt`
                `types`.add(comp.typeId.int, `reference`.index)
          else:
            when owningSystemIndex == InvalidSystemIndex:
              `reference` = newInstance(componentInstanceType()(compRef.index).access).toRef            
            else:
              # This reference isn't valid until we add the tuple in the second pass.
              `reference` = compRef
            `addToEnt`
            `types`.add(compRef.typeId.int, `reference`.index)
            
      # Update systems.
      `userCloneDecls`
      for `compIndexInfo` in `types`.pairs:
        `cloneCase`
      `userCloneEvents`

  genLog "# Run-time construction tools:\n", result.repr
