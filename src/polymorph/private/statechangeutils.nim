## Tools for building ECS state change operations.
## 

import macros, strutils, sets
import ../sharedtypes, ecsstatedb, utils


# -----------------
# General utilities
# -----------------

iterator building*(id: EcsIdentity, systems: seq[SystemIndex] | HashSet[SystemIndex]): auto =
  ## Iterates through `systems`, outputting standardised idents for use
  ## in building operations.
  for sys in systems:
    let
      sysName = id.getSystemName sys
    
    yield (
      id: sys,
      name: sysName,
      inst: id.instantiation sys,
      sysVar: ident systemVarName(sysName),
      foundBool: ident sysName.toLower & "Found",
      foundRow: ident sysName.toLower & "FoundRow"
    )

iterator building*(id: EcsIdentity, components: seq[ComponentTypeId] | HashSet[ComponentTypeId]): auto =
  ## Iterates through `components`, outputting standardised idents for use
  ## in building operations.
  for typeId in components:
    let
      typeStr = id.typeName typeId
      sysOwner = id.systemOwner(typeId)
      instStr = typeStr.instanceTypeName
    yield (
      typeId: typeId,
      typeName: typeStr,
      instType: instStr,
      instField: typeStr.toLowerAscii & instPostfix,
      owner: sysOwner,
      isOwned: sysOwner != InvalidSystemIndex,
    )

proc satisfied*(id: EcsIdentity, sysIndex: SystemIndex, compList: openarray[ComponentTypeId]): bool {.compileTime.} =
  ## Returns true when a set of components matches a system.
  ## Used to check static constraints at compile time.
  for req in id.ecsSysRequirements(sysIndex):
    if req notin compList:
      return false

  for req in id.ecsSysNegations(sysIndex):
    if req in compList:
      return false

  true


# -----------------
# Entity operations
# -----------------

proc buildInstanceVars*(id: EcsIdentity, entity: NimNode, components: seq[ComponentTypeId]): NimNode =
  ## Defines and initialises instance variables for each component in `components`.
  result = newStmtList()
  
  var
    vars = nnkVarSection.newTree()
    ownedInits = newStmtList()

  for c in id.building(components):
    vars.add genField(c.instField, false, ident c.instType)
    
    if c.isOwned:
      let
        fieldIdent = ident c.instField
        instTypeIdent = ident c.instType
      ownedInits.add(quote do:
        `fieldIdent` = -1.`instTypeIdent`
      )
  
  if vars.len > 0:
    result.add vars
  if ownedInits.len > 0:
    result.add ownedInits


proc buildFetch*(id: EcsIdentity, entity: NimNode, components: seq[ComponentTypeId]): NimNode =
  # Outputs fetches from `entity` for all `components`.
  result = newStmtList()

  result.add id.buildInstanceVars(entity, components)

  case id.componentStorageFormat
    
    of csSeq, csArray:
      # Seach the entity's component list for the given components.
      let
        totalComps = components.len
        multipleFetches = totalComps > 1

        fieldCounter = ident "fieldCounter"
        curComp = ident "curComp"

      if totalComps > 0:

        let
          exitConditions =
            if multipleFetches:
              quote do:
                `fieldCounter` += 1
                if `fieldCounter` == `totalComps`:
                  break
            else:
              quote do:
                break

        if multipleFetches:
          result.add(quote do:
            var `fieldCounter`: int
          )

        # Build a case statement with an 'of' branch for the given components.
        
        var
          fetchCase = nnkCaseStmt.newTree(
            quote do:
              `curComp`.typeId.int
          )

        for c in id.building(components):
          let
            field = ident c.instField
            instType = ident c.instType
            owner = id.systemOwner(c.typeId)

          fetchCase.add nnkOfBranch.newTree(
            newIntLitNode(c.typeId.int),
            newStmtList(quote do:
                `field` = `instType`(`curComp`.index)
                `exitConditions`
            )
          )

        fetchCase.add nnkElse.newTree(
          quote do:
            discard
          )

        result.add(quote do:
          for `curComp` in entityData(`entity`.entityId).componentRefs:
            `fetchCase`
        )

    of csTable:
      # Output direct table lookups for the components.
      result = newStmtList()

      for c in id.building(components):
        let
          comp = c.typeId
          instTyIdent = ident c.instType
          fieldIdent = ident c.instField
        
        result.add(quote do:
          let `fieldIdent` = `instTyIdent`(entityData(`entity`.entityId).componentRefs.getOrDefault(`comp`.ComponentTypeId).index)
        )


proc buildFetch2*(id: EcsIdentity, entity: NimNode, lookFor: seq[ComponentTypeId]): NimNode =
  ## Outputs code to:
  ##  - fetch a list of components from the entity's component list
  ##  - place these into instance variables named after the type.

  result = newStmtList()

  var
    # Create component variable to be populated by the fetch.
    compDecl = id.buildInstanceVars(entity, lookFor)

    fetch = newStmtList()
    fieldCounter = ident "fieldCounter"
    targetHigh = lookFor.len
    ownedInits {.used.} = newStmtList()

  if targetHigh > 0:
    let multipleFetches = targetHigh > 1

    if multipleFetches:
      result.add(quote do:
        var `fieldCounter`: int
      )

    case id.componentStorageFormat
    of csSeq, csArray:
      # Loop through components and extract with a case statement.
      let
        fetchCompIdent = ident "curComp"
      var
        fetchCase = nnkCaseStmt.newTree()

      fetchCase.add(quote do:
        `fetchCompIdent`.typeId.int
      )

      for typeId in lookFor:
        let
          typeStr = id.typeName typeId
          fieldName = typeStr.toLowerAscii & instPostfix
          fieldIdent = ident fieldName
          instTypeIdent = ident typeStr.instanceTypeName
          owner = id.systemOwner(typeId)

        if owner != InvalidSystemIndex:
          # Owned system fetched require initialisation as the uninitialised
          # type will pass a `valid` check.
          ownedInits.add(quote do:
            `fieldIdent` = -1.`instTypeIdent`
          )

        let
          getInstance = quote do:
            `fetchCompIdent`.index
          ofBranch = nnkOfBranch.newTree(newIntLitNode(typeId.int))
          ofStmts = 
            if not multipleFetches:
              # With only one component to fetch we don't need to track count.
              newStmtList(quote do:
                `fieldIdent` = `instTypeIdent`(`getInstance`)
                break
              )
            else:
              newStmtList(quote do:
                `fieldIdent` = `instTypeIdent`(`getInstance`)
                `fieldCounter` += 1
                if `fieldCounter` == `targetHigh`: break
              )
        ofBranch.add ofStmts
        fetchCase.add ofBranch

      fetchCase.add nnkElse.newTree(quote do: discard)
      fetch.add(quote do:
        for `fetchCompIdent` in entityData(`entity`.entityId).componentRefs:
          `fetchCase`
      )

    of csTable:
      ## Tables can directly fetch components.
      for comp in lookFor:
        let
          typeStr = id.typeName comp
          instTypeIdent = ident(typeStr.instanceTypeName)
          fieldName = typeStr.toLowerAscii & instPostfix
          fieldIdent = ident fieldName
        fetch.add(quote do:
          let `fieldIdent` = `instTypeIdent`(entityData(`entity`.entityId).componentRefs.getOrDefault(`comp`.ComponentTypeId).index)
        )

    result.add compDecl

    if ownedInits.len > 0:
      result.add ownedInits

    result.add fetch


# -----------------
# System operations
# -----------------


proc buildFindSystemVars*(id: EcsIdentity, systems: seq[SystemIndex] | HashSet[SystemIndex]): NimNode =
  # Set up variables for populating a find boolean and row index for a set of systems.
  assert systems.len > 0

  result = nnkVarSection.newTree()
  
  for sys in id.building(systems):
  
    result.add newIdentDefs(sys.foundBool, ident "bool")
    result.add newIdentDefs(sys.foundRow, ident "int")


proc buildFindSystems*(id: EcsIdentity, entityRef: NimNode, systems: seq[SystemIndex] | HashSet[SystemIndex]): NimNode =
  ## Fetch the system rows for the entity, if present.
  ## Expects `bool` and `int` find variables for each system as set up in `buildFindSystemVars`.
  result = newStmtList()
  
  let
    entId = entityRef.newDotExpr(ident "entityId")
  
  for sys in id.building(systems):
    let
      row = sys.foundRow
      tryGetIndex = sys.inst.indexTryGet(
        entId,
        row,
        id.indexFormat(sys.id))
      found = sys.foundBool
    
    result.add(quote do:
      if `tryGetIndex`:
        `found` = true
        `row` = `row`
    )


proc buildRemoveSystemEvents*(id: EcsIdentity, entityRef: NimNode, systems: seq[SystemIndex] | HashSet[SystemIndex]): NimNode =
  # Return user event handlers triggered on removal from a system.
  result = newStmtList()

  let
    entId = entityRef.newDotExpr(ident "entityId")
  
  for sys in id.building(systems):
    let
      sysOpts = id.getOptions sys.id
      userSysRemove = id.userSysRemoved(sys.id, sys.inst, sys.foundRow, entId, entityRef, sysOpts)
    
    if userSysRemove.len > 0:
      let
        found = sys.foundBool
      
      result.add(quote do:
        if `found`:
          `userSysRemove`
      )


proc buildRemoveSystems*(id: EcsIdentity, entityRef: NimNode, systems: seq[SystemIndex] | HashSet[SystemIndex]): NimNode =
  # Return code to remove the system rows.
  result = newStmtList()
  let
    entId = entityRef.newDotExpr(ident "entityId")
  
  for sys in id.building(systems):
    result.add removeSysReference(id, sys.id, sys.sysVar, sys.foundBool, sys.foundRow, entId, entityRef)


