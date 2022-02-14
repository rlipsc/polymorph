# SPDX-License-Identifier: Apache-2.0

# Copyright (c) 2021 Ryan Lipscombe
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

## This module generates code to perform a state change.
## 
## The `StateChangeDetails` object holds the parameters and output code
## to create the state transition.
## 
## 


import macros, strutils, sets, tables
import ../sharedtypes, ecsstatedb
import utils, eventutils, statechangeutils


# ---------------------------
# System state change parsing
# ---------------------------

type
  ComponentGroup* = object
    required*: ComponentSet
    negated*: ComponentSet

  SystemUpdate* = ref object
    sysFetches*: SystemSet    # Check/get the entity in these systems.
    update*: NimNode
    events*: NimNode
    deferredEvents*: NimNode  # System events triggered by negation to be run after system state changes.
  
  # The update groups and event groups must be executed in the order
  # they're provided so that the user can rely on predictable state
  # changes.
  ComponentUpdateGroups*  = OrderedTable[ComponentGroup, SystemUpdate]
  SystemUpdateGroups*     = OrderedTable[SystemIndex, SystemUpdate]
  
  StateChangeDetailsKind* = enum
    scdkNewEntity = "New entity",
    scdkAdd = "Add component(s)",
    scdkRemove = "Remove components(s)",
    scdkDelete = "Delete entity",
    scdkConstruct = "Construct entity",
    scdkClone = "Clone entity"

  ## Stores state changes details and code output.
  StateChangeDetails* = ref object

    kind*: StateChangeDetailsKind

    # User passed data.
    values*: NimNode

    passed*: seq[ComponentTypeId]
    passedSet*: ComponentSet
    
    # Generated ident postfix for this operation.
    suffix*: string

    newComps*: ComponentSet         # Create a new slot for this component.
    compFetches*: ComponentSet      # Fetch these from the entity at the start of the op.
    compRemoves*: ComponentSet      # Remove these components from the entity.

    componentGroups*: ComponentUpdateGroups   # Guard updates by component set.
    systemGroups*: SystemUpdateGroups         # Guard updates by system.
    compAccess*: ComponentAccessProc          # A proc taking typeId and returning code to read a component's data.
    compValid*: ComponentValidProc            # A proc returning a bool statement for component existence.

    sysProcessed*: NimNode    # Set to a set[SysEnum] ident.
    iterating*: NimNode       # A for var expected to contain the typed component index.

    newDecls*: NimNode        # Declare and variable init code.
    fetches*: NimNode         # Code to perform fetches.
    postFetches*: NimNode     # Run time checks after fetched have been performed.
    asserts*: NimNode         # Code to assert expected states.
    systemUpdates*: NimNode   # Perform the state change for systems.
    allEvents*: NimNode       # All user events.
    deferredEvents*: NimNode  # Events that need to be run after system state changes.


proc newSystemUpdate*(): SystemUpdate =
  SystemUpdate(
    update: newStmtList(),
    events: newStmtList(),
    deferredEvents: newStmtList()
  )


proc initCode*(scd: var StateChangeDetails) =
  ## Clear generated code but not the state change parameters.
  scd.asserts = newStmtList()
  scd.newDecls = nnkVarSection.newTree()
  scd.fetches = newStmtList()
  scd.postFetches = newStmtList()
  scd.systemUpdates = newStmtList()
  scd.allEvents = newStmtList()
  scd.deferredEvents = newStmtList()
  scd.componentGroups.clear
  scd.systemGroups.clear


proc newStateChangeDetails*(
    kind: StateChangeDetailsKind,
    compAccess: ComponentAccessProc = byFetchedIdent,
    compValid = checkInstanceValid): StateChangeDetails =
  
  result = StateChangeDetails()
  result.kind = kind
  result.compAccess = compAccess
  result.compValid = compValid
  result.initCode


proc newStateChangeDetails*(
    id: EcsIdentity,
    kind: StateChangeDetailsKind,
    values: NimNode,
    compAccess: ComponentAccessProc = byFetchedIdent,
    compValid = checkInstanceValid): StateChangeDetails =
  
  result = newStateChangeDetails(kind, compAccess, compValid)
  result.values = values
  result.passed = id.toTypeList(values)
  result.passedSet = result.passed.toHashSet


proc createComponents(id: EcsIdentity, entity: NimNode, details: var StateChangeDetails) =
  ## Creates the passed components.
  
  if details.values.len == 0:
    return

  assert details.passed.len == details.values.len,
    "Internal error: passed/values mismatch when creating components"

  for i in 0 ..< details.passed.len:
    let
      c = id.getInfo details.passed[i]
      typeId = c.typeId

    details.newComps.incl typeId

    let
      fieldSym = details.compAccess(c, details.suffix)
      instType = c.instanceTy

    if c.isOwned:
      let
        sysVar = id.instantiation c.owner
        # The owned instance referred to here is yet to be created.
        nextSystemRow = quote do:
          `sysVar`.count.`instType`

      details.newDecls.add newIdentDefs(fieldSym, newEmptyNode(), nextSystemRow)
    
    else:
      let
        value = details.values[i]
        newInstance = quote do:
          newInstance(`value`)
      
      details.newDecls.add newIdentDefs(fieldSym, newEmptyNode(), newInstance)


proc buildRemoveCompEvents(id: EcsIdentity, context: var EventContext, c: ComponentBuildInfo): NimNode =
  result = newStmtList()

  result.invokeEvent(id, context, ekRemove)
  result.invokeEvent(id, context, ekRemoveCb)

  if c.isOwned:
    # For unowned components ekDeleteComp is invoked within the component's finalisation proc.
    result.invokeEvent(id, context, ekDeleteComp)


proc removeComponentEvents(id: EcsIdentity, entity: NimNode, details: var StateChangeDetails) =
  ## Invoke remove events for details.passed.
  ## 

  if not details.iterating.isNil:
    assert details.passed.len > 0, "Internal error: there must be at least one component passed when iterating"

    let c = id.getInfo(details.passed[0])

    var context = newEventContext(entity, c, details.iterating)
    let events = id.buildRemoveCompEvents(context, c)
    if events.len > 0:
      details.allEvents.add events

  else:
    
    for c in id.building(details.passed):

      let fetchedIdent = details.compAccess(c, details.suffix)
      var context = newEventContext(entity, c, fetchedIdent)
      let events = id.buildRemoveCompEvents(context, c)
      
      if events.len > 0:
        details.allEvents.add events
        details.compFetches.incl c.typeId


proc buildAddCompEvents(node: var NimNode, id: EcsIdentity, context: var EventContext, c: ComponentBuildInfo) =
  if c.isOwned:
    node.invokeEvent(id, context, ekInit)
  
  node.invokeEvent(id, context, ekAdd)
  node.invokeEvent(id, context, ekAddCb)


proc addComponentEvents(id: EcsIdentity, entity: NimNode, details: var StateChangeDetails) =
  # Invoke component add events for details.passed.

  if not details.iterating.isNil:
    assert details.passed.len > 0, "Internal error: there must be at least one component when iterating"
    
    let c = id.getInfo(details.passed[0])
    var context = newEventContext(entity, c, details.iterating)

    details.allEvents.buildAddCompEvents(id, context, c)

  else:
    for c in id.building(details.passed):
      var context = newEventContext(entity, c, details.compAccess(c, details.suffix))
      details.allEvents.buildAddCompEvents(id, context, c)


proc gateSystemActions(details: var StateChangeDetails, sys: SystemBuildInfo, actions: NimNode): NimNode =
  ## Perform `actions` when `sys` is available at run time.

  if not details.sysProcessed.isNil:
    # Constrain to one execution per system.

    let
      sysProcessed = details.sysProcessed
      thisSystem = ident sysEnum(sys.name)

    result = quote do:
      if `thisSystem` notin `sysProcessed`:
        `sysProcessed`.incl `thisSystem`
        `actions`
  
  else:
    result = actions


proc gateSystemUpdates(details: var StateChangeDetails, sys: SystemBuildInfo, sysUpdate: SystemUpdate): SystemUpdate =
  ## Perform `actions` when `sys` is available at run time.
  
  result = newSystemUpdate()
  result.sysFetches = sysUpdate.sysFetches

  let
    sysFetch = sys.fetchedIdent details.suffix
  
  if sysUpdate.update.len > 0:
    let
      updateActions = sysUpdate.update
      updates = quote do:
        if `sysFetch`.found:
          `updateActions`
    
    result.update = details.gateSystemActions(sys, updates)

  if sysUpdate.events.len > 0:
    let
      eventActions = sysUpdate.events
      events = quote do:
        if `sysFetch`.found:
          `eventActions`

    result.events = details.gateSystemActions(sys, events)

  if sysUpdate.deferredEvents.len > 0:
    let
      eventActions = sysUpdate.deferredEvents
      events = quote do:
        if `sysFetch`.found:
          `eventActions`

    result.deferredEvents = details.gateSystemActions(sys, events)


proc addToSystem(id: EcsIdentity, entity: NimNode, details: var StateChangeDetails, change: SystemChange) =

  assert change.kind == sckAdd

  if id.ecsEventEnv.len > 1:
    # This is an embedded event.

    if id.eventOccurred({ekRowAdded, ekRowAddedCB}, [change.sys.int]):
      error "A system add event is trying to invoke itself in system \"" &
        id.getSystemName(change.sys) & "\":\n" & id.eventMutationsStr

  if details.kind == scdkDelete:
    # Delete should not invoke add operations.
    # Note: this should never occur since delete uses remove state
    # changes explicitly.
    return

  if details.kind != scdkNewEntity:
    if change.fromNegation:
      # Adds invoked from negations need to check that the component is
      # present to be removed.
      details.compFetches.incl details.passedSet
    else:
      details.compFetches.excl details.passedSet
  
    details.compFetches.incl (change.checkIncl + change.checkExcl)
  
  let
    sys = id.getInfo change.sys

    orderedReq = id.ecsSysRequirements sys.index

    (possible, sysItemValue, ownedStateUpdates) = id.buildSysItem(
      entity,
      sys,
      orderedReq,
      details.passed,
      details.values,
      details.suffix,
      details.compAccess
    )

  if possible:
    # This system can be added if conditions are met.

    if sys.isOwner:
      # Ensure the required components to complete the owner system are
      # present at run time.
      # The run time response for failing to satisfy the owner is given
      # by ECSErrorResponses.
      details.postFetches.add id.checkRequired(
        entity,
        sys.index,
        change.checkIncl,
        change.checkExcl,
        details.suffix,
        details.compValid
      )

    # Append system updates by component set.
    let
      insertRow = genSym(nskVar, "row")
      entId = entity.newDotExpr ident"entityId"

    # Update system index.
    let
      idxFmt = id.indexFormat(sys.index)
      hasEntity = sys.variable.indexHasKey(entId, idxFmt)
      writeIndex = sys.variable.indexWrite(entId, insertRow, idxFmt)
      sysInt = sys.index.int

    var
      addCompGroup = details.componentGroups.mgetOrPut(
        ComponentGroup(
          required: change.checkIncl,
          negated: change.checkExcl
        ),
        newSystemUpdate()
      )
      stateUpdate = newStmtList()
    
    if details.kind != scdkNewEntity:
      let
        dupMsg1 = newLit "Duplicate insert of entityId "
        dupMsg2 = newLit " for system \"" & sys.name & "\""
      
      stateUpdate.add(quote do:
        {.line.}:
          assert not(`hasEntity`), `dupMsg1` & $`entId`.int & `dupMsg2`
      )
    
    stateUpdate.add(quote do:
      var `insertRow`: int
    )
    stateUpdate.add id.assignSysItemGetRow(sys, sysItemValue, insertRow, id.storageFormat(sys.index))
    stateUpdate.add writeIndex

    # Record this mutation in the output code.
    if not defined(ecsPermissive):
      stateUpdate.add(quote do:
        static:
          recordMutation(EcsIdentity(`id`), ekRowAdded, [`sysInt`])
      )

    if ownedStateUpdates.len > 0:
      stateUpdate.add ownedStateUpdates
  
    # Gate updates to only be performed once.
    addCompGroup.update.add details.gateSystemActions(sys, stateUpdate)

    # Component-system events.

    let
      sysRow = sys.fetchedIdent(details.suffix).newDotExpr(ident "row")
    var
      events = newStmtList()
      context = id.newEventContext(entity, sys, sysRow)
      addSystemGroup = details.systemGroups.mgetOrPut(
        sys.index,
        newSystemUpdate()
      )

    for c in id.building(orderedReq):

      let
        sysVar = sys.variable
        fieldIdent = ident c.lcName
        inst = quote do:
          `sysVar`.groups[`sysRow`].`fieldIdent`
      
      context.component = (c, inst)

      events.invokeEvent(id, context, ekCompAddTo)
      events.invokeEvent(id, context, ekSystemAddAny)

    # System events.

    events.invokeEvent(id, context, ekRowAdded)
    events.invokeEvent(id, context, ekRowAddedCb)
    
    if context.used:
      addSystemGroup.sysFetches.incl sys.index
      
      if change.fromNegation:
        # System events invoked from the negation of components must be
        # triggered after the system state has been resolved.
        # Normally remove operations invoke their events before system
        # state changes so that you can access the system being removed.
        # Within an add context, events are already invoked after system
        # state changes so there is no need to special case this.
        addSystemGroup.deferredEvents.add events
      else:
        addSystemGroup.events.add events

  else:
    # One or more owned components for this system were missing from
    # the passed components.

    if details.values.len > 0:
      # The user has provided components but not enough to create a row
      # in the system.
      let
        owned = id.ecsOwnedComponents(sys.index).toHashSet
        ownedPassed = owned.intersection details.passedSet
        ownedMissing = owned - details.passedSet
      
      if ownedPassed.len > 0:
        error "The owned components [" & id.commaSeparate(ownedPassed) & 
          "] must be passed with [" &
          id.commaSeparate(ownedMissing) &
          "] to satisfy the system owner \"" & sys.name & "\". Given: [" &
          id.commaSeparate(details.passed) & "]"
    
    else:
      # No new values means we cannot create owned components and owner
      # systems cannot participate here.
      discard


proc removeFromSystem(id: EcsIdentity, entity: NimNode, details: var StateChangeDetails, change: SystemChange) =
  ## Remove state change for a system.

  assert change.kind == sckRemove

  if id.ecsEventEnv.len > 1:
    # This is an embedded event.

    if id.eventOccurred({ekRowRemoved, ekRowRemovedCb}, [change.sys.int]):
      # This state change would be recursive at compile time.
      error "A system remove event is trying to invoke itself in system \"" &
        id.getSystemName(change.sys) & "\":\n" & id.eventMutationsStr

  let
    sys = id.getInfo change.sys

  var
    removeSystemRow = details.systemGroups.mgetOrPut(
      sys.index,
      newSystemUpdate()
    )
  
  removeSystemRow.sysFetches.incl sys.index
  if sys.isOwner:
    for c in id.ecsOwnedComponents(sys.index):
      details.compRemoves.incl c

  let
    entId = newDotExpr(entity, ident "entityId")
    sysFetch = sys.fetchedIdent details.suffix
    row = sysFetch.newDotExpr ident"row"
    sysInt = sys.index.int

  removeSystemRow.update.add id.unregister(sys.index, sys.variable, row, entId)
  
  # Record this mutation in the output code.
  if not defined(ecsPermissive):
    removeSystemRow.update.add(quote do:
      static:
        recordMutation(EcsIdentity(`id`), ekRowRemoved, [`sysInt`])
    )

  # Events

  let
    sysRow = sysFetch.newDotExpr ident"row"
    sysVar = sys.variable
    orderedReq = id.ecsSysRequirements sys.index
  var
    context = newEventContext(id, entity, sys, sysRow)
    sysEvents = newStmtList()

  for c in id.building(orderedReq):
    # Extend to remove dependent owned components.
    for depComp in id.dependentComps(c.typeId):
      if depComp notin details.passedSet:
        details.compFetches.incl depComp  # For events.

    let
      fieldIdent = ident c.lcName
      inst =
        quote do:
          `sysVar`.groups[`sysRow`].`fieldIdent`

    context.component = (c, inst)

    sysEvents.invokeEvent(id, context, ekCompRemoveFrom)
    sysEvents.invokeEvent(id, context, ekSystemRemoveAny)

  sysEvents.invokeEvent(id, context, ekRowRemoved)
  sysEvents.invokeEvent(id, context, ekRowRemovedCb)

  if context.used:
    removeSystemRow.events.add sysEvents


proc applyChange*(id: EcsIdentity, entity: NimNode, details: var StateChangeDetails, change: SystemChange) =
  case change.kind
    of sckAdd:
      id.addToSystem(entity, details, change)

    of sckRemove:
      id.removeFromSystem(entity, details, change)


proc doSysUpdate(id: EcsIdentity, entity: NimNode, sysUpdate: SystemUpdate, guard: NimNode, details: var StateChangeDetails) =
  var
    updateRows = sysUpdate.update
  
  if sysUpdate.sysFetches.len > 0:
    var
      systemFetches = newStmtList()

    systemFetches.buildFetchSystems(id, entity, sysUpdate.sysFetches, details.suffix)

    # Duplicate the system fetch operation for events if required.
    
    if updateRows.len > 0:
      var update = newStmtList()
      update.add systemFetches.copy
      update.add maybeIf(guard, updateRows)
      details.systemUpdates.add update

    # System events for this group.
    if sysUpdate.events.len > 0:
      var events = newStmtList()
      events.add systemFetches.copy
      events.add sysUpdate.events
      details.allEvents.add newBlockStmt(maybeIf(guard, events))

    if sysUpdate.deferredEvents.len > 0:
      var events = newStmtList()
      events.add systemFetches.copy
      events.add sysUpdate.deferredEvents
      details.deferredEvents.add newBlockStmt(maybeIf(guard, events))

  else:
    details.systemUpdates.add maybeIf(guard, updateRows)

    if sysUpdate.events.len > 0:
      # System events for this group.
      details.allEvents.add maybeIf(guard, sysUpdate.events)

    if sysUpdate.deferredEvents.len > 0:
      details.deferredEvents.add maybeIf(guard, sysUpdate.deferredEvents)


proc buildSystemUpdates(id: EcsIdentity, entity: NimNode, details: var StateChangeDetails) =
  ## Build modifications to systems.
  ## Expects `details` to be fully populated.

  # Actions depending on component presence.
  for group, sysUpdate in details.componentGroups:
    id.doSysUpdate(
      entity,
      sysUpdate,
      guard = id.buildSysCheck(
        group.required,
        group.negated,
        details.suffix,
        details.compValid),
      details
    )

  # Actions depending on system presence.
  for sys, sysUpdate in details.systemGroups:
    let
      info = id.getInfo sys
      updateSystem = details.gateSystemUpdates(info, sysUpdate)
    
    id.doSysUpdate(
      entity,
      updateSystem,
      nil,
      details
    )


proc buildStateChange*(id: EcsIdentity, entity: NimNode, details: var StateChangeDetails) =
  ## Build the current state change operation within `details`.

  case details.kind
    of scdkNewEntity, scdkConstruct, scdkClone, scdkAdd:
      id.createComponents(entity, details)
      id.addComponentEvents(entity, details)

    of scdkRemove, scdkDelete:
      id.removeComponentEvents(entity, details)

  details.fetches.buildFetchComponents(
    id,
    entity,
    details.compFetches - details.newComps,
    details.suffix,
    earlyExit = true,
    details.compAccess
  )
  
  details.fetches.add details.postFetches

  id.buildSystemUpdates(entity, details)

