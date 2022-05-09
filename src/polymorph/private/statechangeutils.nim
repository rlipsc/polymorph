# SPDX-License-Identifier: Apache-2.0

# Copyright (c) 2020 Ryan Lipscombe
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

## Tools for building ECS state change operations:
## 
## - translating lists of components into system updates,
## - generating code for fetching, conditionals, and building system rows.
## 


import macros, strutils, sets, tables
import ../sharedtypes, ecsstatedb, utils

proc satisfied*(id: EcsIdentity, sysIndex: SystemIndex, compList: ComponentIterable): bool {.compileTime.} =
  ## Returns true when a set of components matches a system's requirements.

  for req in id.ecsSysRequirements(sysIndex):
    if req notin compList:
      return false
  
  for req in id.ecsSysNegations(sysIndex):
    if req in compList:
      return false
  
  true

iterator satisfiedSystems*(id: EcsIdentity, compList: ComponentIterable): SystemIndex =
  ## Yields system indexes that are satisfied by `compList`.

  var
    sysProcessed: SystemSet

  for comp in compList:

    for sys in id.systems comp:
      
      if sys notin sysProcessed:
        sysProcessed.incl sys
        
        var found = true

        for req in id.ecsSysRequirements(sys):
          if req notin compList:
            found = false
            break
    
        if found:
          for neg in id.ecsSysNegations(sys):
            if neg in compList:
              found = false
              break
          
          if found:
            yield sys


type
  SystemChangeKind* = enum sckAdd, sckRemove

  SystemChange* = object
    sys*: SystemIndex
    fromNegation*: bool
    case kind*: SystemChangeKind
      of sckAdd:
        checkIncl*, checkExcl*: ComponentSet
      of sckRemove:
        discard


iterator addStateChanges*(id: EcsIdentity, given: seq[ComponentTypeId], givenSet: ComponentSet): SystemChange =
  ## Iterates over systems that might be affected by the given components.
  ## 
  ## Yields possible systems and the conditions required.
  
  var
    processed: SystemSet
  
  for c in given:
    for sys in id.linked c:
      if sys notin processed:
        processed.incl sys

        let
          req = id.ecsSysRequirements(sys).toHashSet
          neg = id.ecsSysNegations(sys).toHashSet
          own = id.ecsOwnedComponents(sys).toHashSet
          mandatory = givenSet.intersection(own).len > 0

        if own.len > 0:
          if mandatory:
            # This is the owner system for a passed owned component.
            # Owned components cannot be stored without fully satisfying
            # their owner systems.
            let
              givenNeg = neg.intersection givenSet

            if givenNeg.len > 0:
              error "Cannot instantiate owned component " & id.typeName(c) &
                " because the system is negated with the passed components: " &
                id.commaSeparate(givenNeg)
            else:
              if own <= givenSet:
                # We have all the owned components we need to create a row,
                # possibly pending other component conditions.
                yield SystemChange(
                  kind: sckAdd,
                  sys: sys,
                  checkIncl: req - givenSet,
                  checkExcl: neg
                )
              else:
                error "Cannot instantiate component " & id.typeName(c) &
                  " as the owner system \"" & id.getSystemName(sys) &
                  "\" requires other owned components: " &
                  id.commaSeparate(own - givenSet)
          else:
            # This system doesn't match any conditions.
            discard
        else:
          if neg.disjoint(givenSet):
            # No passed negations affect this system.
            # Negations might still exist on the entity.
            let
              inCommon = req.intersection givenSet
            
            if inCommon.len > 0:
              yield SystemChange(
                kind: sckAdd,
                sys: sys,
                checkIncl: req - inCommon,
                checkExcl: neg
              )
            else:
              # This system doesn't match any conditions.
              discard
          else:
            # Adding these components negates this system (if present).
            yield SystemChange(
              sys: sys,
              fromNegation: true,
              kind: sckRemove,
            )


iterator removeStateChanges*(id: EcsIdentity, removing: seq[ComponentTypeId]): SystemChange =
  ## Returns a set of systems that interact with these components.
  var processed: SystemSet
  let removingSet = removing.toHashSet

  for c in removing:

    for sys in id.linked c:
      if sys notin processed:
        processed.incl sys

        let
          neg = id.ecsSysNegations(sys).toHashSet
          req = id.ecsSysRequirements(sys).toHashSet
          negIntersect = neg.intersection removingSet

        if req.hasIntersection(removingSet):
          # This system's requirements are invalidated by the remove. 
          yield
            SystemChange(
              sys: sys,
              kind: sckRemove
            )
        
        else:
          # The removed components haven't affected system requirements.

          if negIntersect.len > 0:
            # This remove might have satisfied a system's negations.

            # Note: it's possible that any of these components have already
            # been removed and the negation is already satisfied.
            
            if id.ecsOwnedComponents(sys).len == 0:
              let
                req = id.ecsSysRequirements(sys).toHashSet
                neg = id.ecsSysNegations(sys).toHashSet
                otherNegs = neg - negIntersect
              
              yield
                SystemChange(
                  sys: sys,
                  fromNegation: true,
                  kind: sckAdd,
                  # Note: negIntersect is included to ensure negated component
                  # are present to be removed in order for this state change
                  # to occur. This handles remove ops where the component to
                  # be removed isn't on the entity.
                  checkIncl: req + negIntersect,
                  checkExcl: otherNegs
                )
            else:
              # An owner system that has one or more matching component
              # negations.
              #
              # Since owned components can only be directly created in an add
              # operation we cannot create a new row for this system.
              discard
          
          else:
            # There are no negations in common with this system, and no
            # negations match either.
            discard


# --------------------
# Component operations
# --------------------


proc buildVars*(node: var NimNode, id: EcsIdentity, components: ComponentIterable, suffix: string, compAccess: ComponentAccessProc) =
  ## Create instance variables corresponding to a set of components.

  if components.len == 0:
    return

  var
    vars = nnkVarSection.newTree()
  
  for c in id.building components:
    let
      #field = c.fetchedIdent(suffix)
      field = c.compAccess(suffix)
      instTypeIdent = c.instanceTy
    
    if c.isOwned:
      vars.add newIdentDefs(field, newEmptyNode(), quote do: -1.`instTypeIdent`)
    else:
      vars.add newIdentDefs(field, instTypeIdent)

  if vars.len > 0:
    node.add vars


proc buildFetchComponents*(node: var NimNode, id: EcsIdentity, entity: NimNode, components: ComponentIterable,
    suffix: string, earlyExit: bool, compAccess: ComponentAccessProc) =
  ## Outputs code to populate component variables from `entity`.

  if components.len == 0:
    return

  case id.componentStorageFormat
    
    of csSeq, csArray:
      # Builds a loop over the entity's components with a case statement
      # to populate the given components.

      node.buildVars(id, components, suffix, compAccess)
      
      let
        totalComps = components.len
        multipleFetches = totalComps > 1

        fieldCounter = genSym(nskVar, "fieldCounter")
        curCompRef = genSym(nskForVar, "curCompRef")

      if totalComps > 0:

        let
          exitConditions =
            if earlyExit:
              if multipleFetches:
                quote do:
                  `fieldCounter` += 1
                  if `fieldCounter` == `totalComps`:
                    break
              else:
                quote do:
                  break
            else:
              newStmtList()

        if earlyExit and multipleFetches:
          node.add(quote do:
            var `fieldCounter`: int
          )

        var
          fetchCase = nnkCaseStmt.newTree(
            quote do:
              `curCompRef`.typeId.int
          )

        # Add 'of' branches.

        for c in id.building components:
          let
            field = c.compAccess(suffix)
            instType = c.instanceTy

          fetchCase.add nnkOfBranch.newTree(
            newLit c.typeId.int,
            newStmtList(quote do:
              `field` = `instType`(`curCompRef`.index)
              `exitConditions`
            )
          )

        fetchCase.add nnkElse.newTree(
          quote do:
            discard
          )

        node.add(quote do:
          for `curCompRef` in `entity`:
            `fetchCase`
        )

    of csTable:
      # Output direct table lookups for the components.
      
      let
        entityId = quote do: `entity`.entityId
        entityData = entAccess(id.entityStorageFormat, entityId)

      for c in id.building components:
        let
          comp = c.typeId
          instTyIdent = c.instanceTy
          field = c.compAccess(suffix)
          getComp = quote do:
            `entityData`.componentRefs.getOrDefault(`comp`.ComponentTypeId)
          
        node.add(quote do:
          let `field` = `instTyIdent`(`getComp`.index)
        )


# -----------------
# System operations
# -----------------


proc buildVars*(id: EcsIdentity, systems: SystemIterable, typeNode: NimNode, suffix: string): NimNode =
  ## Create variables corresponding to a set of systems.
  
  if systems.len == 0:
    return newStmtList()

  result =
    nnkVarSection.newTree()
  
  for sys in id.building systems:
    result.add newIdentDefs(
      sys.fetchedIdent(suffix),
      typeNode)


proc buildFetchSystemsDecl*(node: var NimNode, id: EcsIdentity, systems: SystemIterable, suffix: string) =
  node.add id.buildVars(systems, ident "SystemFetchResult", suffix)


proc buildFetchSystemsExe*(node: var NimNode, id: EcsIdentity, entity: NimNode, systems: SystemIterable, suffix: string) =
  ## Fetch the system rows for the entity, if present.
  ## Expects `bool` and `int` find variables for each system as set up in `buildFindSystemVars`.
  let
    entityId = entity.newDotExpr(ident "entityId")
  
  for sys in id.building systems:
    let
      # sysFound is of type `SystemFetchResult`.
      sysFound = sys.fetchedIdent(suffix)
      foundRow =  quote do:
        `sysFound`.row

      tryGetIndex = sys.variable.indexTryGet(
        entityId,
        foundRow,
        id.indexFormat(sys.index)
      )
    
    node.add(quote do:
      `sysFound`.found = `tryGetIndex`
      )


proc buildFetchSystems*(node: var NimNode, id: EcsIdentity, entity: NimNode, systems: SystemIterable, suffix: string) =
  node.buildFetchSystemsDecl(id, systems, suffix)
  node.buildFetchSystemsExe(id, entity, systems, suffix)


proc buildSysItem*(id: EcsIdentity, entity: NimNode, sys: SystemBuildInfo,
    orderedRequirements, passedTypes: ComponentIterable, passedValues: NimNode,
    suffix: string, compAccess: ComponentAccessProc): tuple[possible: bool, itemValue, ownedStateUpdates: NimNode] =
  ## Build a new item row and assign the system requirements.
  ## 
  ## All owned components for this system must be included in
  ## `passedValues` in order to create a row.

  result.itemValue = nnkObjConstr.newTree()
  result.itemValue.add ident itemTypeName(sys.name)
  result.itemValue.add nnkExprColonExpr.newTree(ident "entity", entity)
  result.ownedStateUpdates = newStmtList()
  result.possible = true

  let
    sysVar = sys.variable

  for c in id.building orderedRequirements:
    if c.owner == sys.index:
      # Owned components are directly assigned to the item from the parameter list.
      
      if passedValues.len == 0:
        result.possible = false
        break

      let
        ownedIndex = passedTypes.find c.typeId

      if ownedIndex < 0:
        result.possible = false
        break

      result.itemValue.add nnkExprColonExpr.newTree(
        ident c.lcName,
        passedValues[ownedIndex]
      )

      result.ownedStateUpdates.add id.appendOwnedComponentState(c.typeId, sysVar.newDotExpr(ident "high"))
    
    else:
      # Use the externally fetched instance value for the field.
      result.itemValue.add nnkExprColonExpr.newTree(
        ident c.lcName,
        c.compAccess(suffix)
      )


proc buildSysCheck*(id: EcsIdentity, requirements, negations: ComponentIterable, suffix: string, compValid: ComponentValidProc): NimNode =
  ## Returns the infix for an 'if' statement from the system requirements.
  ## For example: `comp1 and comp2 and not(comp3)`.

  var
    reqElements: seq[NimNode]
  
  for c in id.building requirements:
    reqElements.add c.compValid(suffix)

  for c in id.building negations:
    let
      isValid = c.compValid suffix

    reqElements.add(quote do:
      (not `isValid`)
    )

  genInfixes(reqElements, "and")


proc checkRequired*(id: EcsIdentity, entity: NimNode, sysIndex: SystemIndex, req, neg: ComponentSet, suffix: string,
    compValid: ComponentValidProc): NimNode =
  ## Run time check to ensure required components exist to satisfy owned systems.
  result = newStmtList()

  if req.len == 0 and neg.len == 0:
    return

  let
    matchesSystem = id.buildSysCheck(req, neg, suffix, compValid)
  
  var
    reqStr = id.commaSeparate(req)

  if neg.len > 0:
    reqStr &= " and not " & id.commaSeparate(neg)

  let
    unsatisfiedErrStr =
      newLit "Cannot complete this add operation because one or more passed components " &
      "are owned but the the owner system '" & id.getSystemName(sysIndex) &
      "' cannot be fully satisfied. System requirements: [" & reqStr & "], entity's components: "

  result.add(
    case id.errIncompleteOwned
    of erAssert:
      quote do:
        {.line.}:
          assert `matchesSystem`, `unsatisfiedErrStr` & (
            if `entity`.componentCount == 0: "<none>"
            else: "\n" & `entity`.listComponents(showData = false))
    
    of erRaise:
      quote do:
        if not(`matchesSystem`):
          {.line.}:
            raise newException(ValueError, `unsatisfiedErrStr`)
  )


