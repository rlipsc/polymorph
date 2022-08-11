## This module simplifies the invocation of user events.

import macros, ../sharedtypes, ecsstatedb, utils, mutationtracking

# -----------------
# Event user access
# -----------------


proc userEntAccess*(entitySym: NimNode): NimNode =
  let ent = ident "entity"
  result = quote do:
    {.line.}:
      let `ent` {.used, inject, hostEntity.} = `entitySym`
      template curEntity: untyped {.used.} = `ent`


proc userCompNameAccess(id: EcsIdentity, compId: ComponentTypeId, comp, ty: NimNode): NimNode =
  ## User access to a component.
  let
    tyStr = id.typeName(compId)
    tyIdent = ident tyStr
    field = ident tyStr.unCap
  result = quote do:
    template `field`: `ty` {.used.} = `comp`
    template access(ty: typedesc[`tyIdent`] | typedesc[`ty`]): `ty` {.used.} = `comp`


proc userCompAccess(id: EcsIdentity, compId: ComponentTypeId, comp, ty: NimNode): NimNode =
  ## User access to the component hosting an event.
  result = quote do:
    template curComponent: `ty` {.used.} = `comp`
    template component: `ty` {.used.} = `comp`
  
  if id.eventTemplates(compId) == cetEnabled:
    result.add id.userCompNameAccess(compId, comp, ty)


proc userSysCompAccess*(id: EcsIdentity, sysIndex: SystemIndex, rowEntity: NimNode, sysVar, row: NimNode): NimNode =
  ## User access to a system row.
  result = newStmtList()
  var aliased: seq[ComponentTypeId]

  if id.ecsSysComponentAliases(sysIndex).len > 0:
    # Add alias templates.
    for aliasNode in id.ecsSysComponentAliases(sysIndex):
      let
        compId = aliasNode[0].intVal.ComponentTypeId
        aliasNode = ident aliasNode[1].strVal
        tyStr = id.typeName(compId)
        tyIdent = ident tyStr
        field = ident tyStr.unCap
        ownerSys = id.systemOwner compId
        accessAsValue = ownerSys == sysIndex
        ty =
          if accessAsValue: ident tyStr
          else: ident instanceTypeName(tyStr)
        # The item core is used here to save the extra template expansion of 'item'.
        itemCore = systemItemCore(id, sysIndex, rowEntity, sysVar, row)
        accessCore = newDotExpr(itemCore, field)
      
      result.add(quote do:
        template `aliasNode`: `ty` {.used.} = `accessCore`
        template access(ty: typedesc[`tyIdent`]): `ty` {.used.} = `aliasNode`()
      )
      aliased.add compId
  
  if id.ecsSysItemTemplates(sysIndex) == sitEnabled:
    # Add templates for system fields.
    for compId in id.ecsSysRequirements(sysIndex):
      if compId notin aliased:
        let
          field = ident id.typeName(compId).unCap
          # The 'field' template we're making has the same name as the
          # object field we want to access, preventing use of 'item.field'
          # without recursing the template again (then reporting it's
          # been given too many parameters).
          # To save these complications, the 'item' internals are used
          # directly.
          itemCore = systemItemCore(id, sysIndex, rowEntity, sysVar, row)
          ownerSys = id.systemOwner compId
          tyStr = id.typeName(compId)
          ty =
            if ownerSys == sysIndex: ident tyStr
            else: ident instanceTypeName(tyStr)
          accessCore = newDotExpr(itemCore, field)
        
        result.add id.userCompNameAccess(compId, accessCore, ty)


proc userSysAccess(id: EcsIdentity, sysIndex: SystemIndex, rowEntity: NimNode, sysVar, row: NimNode): NimNode =
  ## Returns user access templates for a system.

  let sysItem = id.systemItem(sysIndex, rowEntity, sysVar, row)
  
  result = newStmtList()
  result.add(quote do:
    `sysItem`
    template sys: untyped {.used.} = `sysVar`
    template curSystem: untyped {.used.} = `sysVar`
    template groupIndex: untyped {.used.} = `row`
  )
  result.add id.userSysCompAccess(sysIndex, rowEntity, sysVar, row)


proc eventCb(ent, compInst: NimNode, typeInfo: ComponentBuildInfo, cbProcName: NimNode): NimNode =
  quote do:
    `cbProcName`(`ent`, `compInst`)


proc eventSystemCb(node: NimNode, id: EcsIdentity, sysInfo: SystemBuildInfo, row: NimNode, cbProcName: NimNode) =
  let
    sysVar = sysInfo.variable
  
  node.add(quote do:
    `cbProcName`(`sysVar`, `row`)
  )


# --------------
# Running events
# --------------


type
  AccessUtility* = enum auEntity, auComponent, auSystem
  AccessUtilities* = set[AccessUtility]
  
  EventContext* = object
    entity*:      NimNode
    component*:   tuple[info: ComponentBuildInfo, instance: NimNode]
    system*:      tuple[info: SystemBuildInfo, row: NimNode, announce: ECSSysEcho]
    used*:        bool
    entityComps*: seq[ComponentTypeId]


proc newEventContext*(entity: NimNode): EventContext =
  ## Partially set up an event context.
  result = EventContext(entity: entity)


proc newEventContext*(entity: NimNode, comp: ComponentBuildInfo, inst: NimNode): EventContext =
  ## Partially set up an event context.
  result = EventContext(
    entity: entity,
    component: (comp, inst),
    entityComps: @[comp.typeId])


proc newEventContext*(entity: NimNode, comps: ComponentIterable): EventContext =
  ## Set up a context for multiple components.
  result = EventContext(entity: entity, entityComps: newSeq[ComponentTypeId](comps.len))
  for i, c in comps:
    result.entityComps[i] = c


proc newEventContext*(id: EcsIdentity, entity: NimNode, comp: ComponentBuildInfo, sys: SystemBuildInfo, inst, row: NimNode): EventContext =
  ## Set up an event context.
  result = EventContext(
    entity: entity,
    component: (comp, inst),
    system: (sys, row, id.echoRunning(sys.index))
  )


proc newEventContext*(id: EcsIdentity, entity: NimNode, sys: SystemBuildInfo, row: NimNode): EventContext =
  ## Set up an event context.
  result = EventContext(
    entity: entity,
    system: (sys, row, id.echoRunning(sys.index))
  )


proc accessType(context: EventContext): NimNode =
  ## Returns the value type for owned components being accessed from
  ## their host system, otherwise returns the instance type.
  let
    tyStr = context.component.info.name
    owner = context.component.info.owner
  if owner != InvalidSystemIndex and owner == context.system.info.index:
    ident tyStr
  else:
    ident instanceTypeName(tyStr)


proc echoRunningEvent(node: var NimNode, id: EcsIdentity, eventKind: EventKind, context: EventContext) =
  ## Announce a system event.
  
  if context.system.announce != seNone:
    let
      sysName = context.system.info.name
      opStr = $eventKind

    node.add(quote do:
      echo `sysName` & " " & `opStr`
    )


proc buildAccess(node: var NimNode, id: EcsIdentity, context: EventContext, access: AccessUtilities) =
  ## Create access templates after the context has been populated.
  for util in access:
    case util
      of auEntity:
        node.unpack userEntAccess(context.entity)
      of auComponent:
        node.unpack id.userCompAccess(context.component.info.typeId, context.component.instance, context.accessType)
      of auSystem:
        let (info, row, _) = context.system
        node.unpack id.userSysAccess(info.index, context.entity, info.variable, row)


proc read*(id: EcsIdentity, eventKind: EventKind, context: EventContext): NimNode =
  ## Get the read procedure for an event type.
  let
    typeInfo = context.component.info
    sysInfo = context.system.info

  case eventKind
    of ekNoEvent,
      ekNewEntityWith,
      ekAddComponents,
      ekConstruct,
      ekClone,
      ekRemoveComponents,
      ekDeleteEnt:            id.onEntityStateChange

    of ekAdd:                 id.onAddToEntCodeNode typeInfo.typeId
    of ekRemove:              id.onRemoveFromEntCodeNode typeInfo.typeId
    of ekAddCB:               id.onAddCallbackNode typeInfo.typeId
    of ekRemoveCB:            id.onRemoveCallbackNode typeInfo.typeId
    of ekInit:                id.onInitCodeNode typeInfo.typeId
    of ekUpdate:              id.onInterceptUpdateNode typeInfo.typeId
    of ekDeleteComp:          id.onFinalisationCodeNode typeInfo.typeId

    of ekSystemAddAny:        id.onAddAnySystemCodeNode typeInfo.typeId
    of ekSystemRemoveAny:     id.onRemoveAnySystemCodeNode typeInfo.typeId

    of ekCompAddTo:           id.onAddToCodeNode(sysInfo.index, typeInfo.typeId)
    of ekCompRemoveFrom:      id.onRemoveFromCodeNode(sysInfo.index, typeInfo.typeId)

    of ekRowAdded:            id.onAddedNode sysInfo.index
    of ekRowRemoved:          id.onRemovedNode sysInfo.index
    of ekRowAddedCb:          id.onAddedCallbackNode sysInfo.index
    of ekRowRemovedCb:        id.onRemovedCallbackNode sysInfo.index


proc invokeEvent*(node: var NimNode, id: EcsIdentity, context: var EventContext, eventKind: EventKind) =

  if eventKind == ekNoEvent:
    return
  
  var
    eventCode = id.read(eventKind, context).copy
    prelude = newStmtList()

  if eventCode.len == 0:
    return

  context.used = true

  let
    (typeInfo, compInst) = context.component
    sysInfo = context.system.info
    paramKind = getParamKind(eventKind)
    eventDir = eventKind.getEventDir

    ek = newLit $eventKind
    ecsId = quote do: EcsIdentity(`id`)

  const
    ie = "Internal error: "
    tc = "type context is not initialised"
    sc = "System context is not initialised"
    de = "Embedded event recursively invokes itself through '"
    ei = " has been invalidated by "
    br = ":\n"

  prelude.echoRunningEvent(id, eventKind, context)

  proc checkStateInvalidation(code: NimNode, sysIndex: SystemIndex) =
    ## Catch embedded system mutations that have invalidated this event.
    let
      sysInt = sysIndex.int
      mutOccur = bindSym("mutationOccurred", brClosed)
      sysName = newLit sysInfo.name
    var
      checks = newStmtList()
    
    case eventDir

      of ekAddComponents:
        checks.add(
          quote do:
            if `mutOccur`(`ecsId`, {ekRowRemoved}, [`sysInt`]):
              error "Event '" & `ek` & "' for system \"" & `sysName` & "\"" &
                `ei` & "a previous event removing from this system before it could be called" & `br` &
                eventMutationsStr(`ecsId`)
        )

      of ekRemoveComponents:
        checks.add(
          quote do:
            if `mutOccur`(`ecsId`, {ekRowAdded}, [`sysInt`]):
              error "Event '" & `ek` & "' for system \"" & `sysName` & "\"" &
                `ei` & "a previous event adding to this system before it could be called" & `br` &
                eventMutationsStr(`ecsId`)
        )
      else:
        discard
    
    if checks.len > 0:
      code.add(quote do:
        static:
          `checks`
      )      

  proc checkStateInvalidation(code: NimNode, comps: openarray[ComponentTypeId]) =
    ## Catch embedded component mutations that have invalidated this event.
    let
      compStr = id.commaSeparate(comps)
      compInts = comps.toIntList
    var
      checks = newStmtList()

    case eventDir
      of ekAddComponents:

        if not defined(ecsPermissive):
          checks.add(quote do:
            if removeOccurred(`ecsId`, `compInts`):
              error "Event '" & `ek` & "'" & `ei` & "removing [" & `compStr` & "]" & `br` & eventMutationsStr(`ecsId`)
          )

      of ekRemoveComponents:

        if not defined(ecsPermissive):
          checks.add(quote do:
            if addOccurred(`ecsId`, `compInts`):
              error "Event '" & `ek` & "'" & `ei` & "adding [" & `compStr` & "]" & `br` & eventMutationsStr(`ecsId`)
          )
      else:
        discard
    
    if checks.len > 0:
      code.add(quote do:
        static:
          `checks`
      )

  proc checkRecursion(data: openarray[int]) =
    ## Catch endlessly recursing events at compile time.
    
    if id.eventOccurred({eventKind}, data):
      error de & $eventKind & "'" & br & id.eventMutationsStr

  # Check state change against history.

  if id.ecsEventEnv.len > 0:
    case paramKind
      of pkNone:
        checkRecursion([0])
        eventCode.trackMutation(id, eventKind, [0])

      of pkCompCT:
        if context.entityComps.len == 0:
          error ie & tc
        let intComps = context.entityComps.toIntList
        checkRecursion(intComps)
        prelude.checkStateInvalidation(context.entityComps)
        eventCode.trackMutation(id, eventKind, intComps)

      of pkComp:
        if context.component.info.typeId == InvalidComponent:
          error ie & tc
        checkRecursion([typeInfo.typeId.int])
        prelude.checkStateInvalidation([typeInfo.typeId])
        eventCode.trackMutation(id, eventKind, [typeInfo.typeId.int])
      
      of pkSys:
        # Callbacks and inline system events differ in their recursion checking.
        if context.system.info.index == InvalidSystemIndex:
          error ie & sc
        prelude.checkStateInvalidation(sysInfo.index)
        eventCode.trackMutation(id, eventKind, [sysInfo.index.int])

      of pkSysComp:
        if context.system.info.index == InvalidSystemIndex:
          error ie & sc
        if context.component.info.typeId == InvalidComponent:
          error ie & tc

        checkRecursion([sysInfo.index.int, typeInfo.typeId.int])
        prelude.checkStateInvalidation(sysInfo.index)
        eventCode.trackMutation(id, eventKind, [sysInfo.index.int, typeInfo.typeId.int])

    node.add prelude

  # Build the event.

  case eventKind

    of ekNoEvent:
      discard

    of  ekNewEntityWith, ekAddComponents,
        ekConstruct, ekClone,
        ekRemoveComponents, ekDeleteEnt:
      
      # Entity events.

      let
        entity = context.entity
        typeId = context.component.info.typeId

        tIds =
          if context.entityComps.len > 0:
            # Component parameters are available at compile time.
            var
              compIds = nnkBracket.newTree
            
            for c in context.entityComps:
              compIds.add newLit(c.int).newDotExpr ident"ComponentTypeId"
            
            quote do:
              @`compIds`

          elif typeId != InvalidComponent:
            # A single component parameter is available at compile time.
            quote do:
              @[`typeId`.ComponentTypeId]

          else:
            # Evaluate all components on the entity at run time.
            quote do:
              block:
                var
                  compIds = newSeq[ComponentTypeId](`entity`.componentCount)
                  i: int
                for c in `entity`:
                  compIds[i] = c.typeId
                  i.inc
                compIds

        st = ident "state"
        types = ident "types"
      
      let userAccess = userEntAccess(context.entity)
      node.add(quote do:
        block:
          const `st` {.inject, used.} = `eventKind`.EventKind
          let `types` {.inject, used.} = `tIds`
          
          `userAccess`
          `eventCode`
      )
  
    of ekInit, ekUpdate:
      var userAccess = newStmtList()
      # These events occur directly on the component without any other context.
      userAccess.unpack id.userCompAccess(context.component.info.typeId, context.component.instance, context.accessType)

      node.add(quote do:
        block:
          `userAccess`
          `eventCode`
      )

    of ekAdd, ekRemove, ekDeleteComp:
      var userAccess = newStmtList()
      userAccess.buildAccess(id, context, {auEntity, auComponent})

      node.add(quote do:
        block:
          `userAccess`
          `eventCode`
      )

    of ekAddCB:
      node.add eventCb(context.entity, compInst, typeInfo, ident addCallbackName(typeInfo.name))
    
    of ekRemoveCB:
      node.add eventCb(context.entity, compInst, typeInfo, ident removeCallbackName(typeInfo.name))

    of ekSystemAddAny, ekSystemRemoveAny, ekCompAddTo, ekCompRemoveFrom:
      # Component-system events.
      var userAccess = newStmtList()
      userAccess.buildAccess(id, context, {auEntity, auComponent, auSystem})

      node.add(quote do:
        block:
          `userAccess`
          `eventCode`
      )

    of ekRowAdded, ekRowRemoved:
      # System events.

      checkRecursion([sysInfo.index.int])

      var userAccess = newStmtList()
      userAccess.buildAccess(id, context, {auEntity, auSystem})
      
      node.add(quote do:
        block:
          `userAccess`
          `eventCode`
      )

    of ekRowAddedCb:
      # Callbacks allow recursion at run time.
      node.eventSystemCb(id, sysInfo, context.system.row, ident systemAddedCBName(sysInfo.name))

    of ekRowRemovedCb:
      # Callbacks allow recursion at run time.
      node.eventSystemCb(id, sysInfo, context.system.row, ident systemRemovedCBName(sysInfo.name))


proc buildEventCallback*(id: EcsIdentity, typeId: ComponentTypeId, mutation: EventKind): tuple[forwardDecl, procDecl: NimNode] =
  ## Create a forward declaration and proc body for an event body.
  let
    tyName = id.typeName(typeId)
    instTypeName = ident instanceTypeName(tyName)
    cc = ident "cc"
    ce = ident "ce"
  var
    body, cbProcName: NimNode
    
  case mutation

    of ekAddCb:
      body = id.onAddCallbackNode(typeId)
      cbProcName = ident addCallbackName(tyName)
  
    of ekRemoveCb:
      body = id.onRemoveCallbackNode(typeId)
      cbProcName = ident removeCallbackName(tyName)
    else:
      error "Cannot process " & $mutation & " here"
  
  # Add access templates within the callback body.

  let
    entAccess = userEntAccess(ce)
    # The callback parameter is always an instance, even for owned components.
    compAccess = id.userCompAccess(typeId, cc, instTypeName)

  body.trackMutation(id, mutation, [typeId.int])

  result.forwardDecl = quote do:
    proc `cbProcName`*(`ce`: EntityRef, `cc`: `instTypeName`)

  result.procDecl = quote do:
    proc `cbProcName`*(`ce`: EntityRef, `cc`: `instTypeName`) =
      `entAccess`
      `compAccess`
      `body`


proc buildEventCallback*(id: EcsIdentity, sys: SystemBuildInfo, mutation: EventKind): tuple[forwardDecl, procDecl: NimNode] =
  ## Create a forward declaration and proc body for an event body.
  
  let
    sysType = ident systemTypeName(sys.name)
    procParam = ident "sys"
    gi = ident "groupIndex"
    startEntity = genSym(nskLet, "rowEntity")
    sysItem = id.systemItem(sys.index, startEntity, procParam, gi)
    entAccess = userEntAccess(quote do: `procParam`.groups[`gi`].entity)

  var
    body = newStmtList()
    cbProcName: NimNode

  # Use access templates.
  case mutation
    
    of ekRowAddedCb:
      body.add id.onAddedCallbackNode(sys.index)
      cbProcName = ident systemAddedCBName(sys.name)
    
    of ekRowRemovedCb:
      body.add id.onRemovedCallbackNode(sys.index)
      cbProcName = ident systemRemovedCBName(sys.name)
    else:
      error "Cannot process " & $mutation & " here"
  
  body.trackMutation(id, mutation, [sys.index.int])
  
  result.forwardDecl = quote do:
    proc `cbProcName`(`procParam`: var `sysType`, `gi`: int)
  
  result.procDecl = quote do:
    proc `cbProcName`(`procParam`: var `sysType`, `gi`: int) =
      let `startEntity` = `procParam`.groups[`gi`].entity
      `sysItem`
      `entAccess`
      `body`
