## This module parses component parameters passed to operations like
## `addComponent` into maps of tasks for code generation.

import macros, strutils, sets
import ../sharedtypes, ecsstatedb, utils, statechangeutils


type
  ## This structure is used at compile-time to convert a list of
  ## component parameters such as given to `addComponent`, into a map
  ## of tasks for code generation to use.
  ComponentParamInfo* = ref object

    ## The component types of arguments in their given order.
    passed*:          seq[ComponentTypeId]

    ## The actual code/variable/data given for the component matching
    ## `passed`.
    values*:          seq[NimNode]

    ## Non-owned components belonging to owner systems that haven't
    ## been passed.
    ## 
    ## These must be fetched at run-time, and must exist for the owner
    ## system row to be created.
    requiredFetches*: seq[ComponentTypeId]

    ## These components are the union of non-owned components that
    ## share a system with one or more of the components in `passed`.
    ## 
    ## For example:
    ##   Given system `sysA` uses components [A, B, C], if you
    ##   `addComponent A()` on its own, `B` and `C` will be in
    ##   `lookFor` to see if the entity satisfies `sysA` at run-time.
    lookFor*:         seq[ComponentTypeId]

    ## Components that require storage generation, stored as an index into `passed`.
    generateIdx*:     seq[int]

    ## Owned components, their owning systems, and index into `passed`.
    owned*:           seq[tuple[id: ComponentTypeId, sys: SystemIndex, passedIdx: int]]

    ## Non-owning system that may or may not be satisfied.
    unownedSystems*:  seq[SystemIndex]

    ## Systems that own passed components and must be fully satisfied.
    ownedSystems*:    seq[SystemIndex]

    ## Systems that no longer match due to negated components, and
    ## require their row removed.
    unsatisfied*: seq[SystemIndex]


proc parseComponentParameters*(id: EcsIdentity, componentList: NimNode): ComponentParamInfo =
  ## Collect information from parameters passed as part of a state change
  ## and process into targetted lists for each code generation job.

  # Generate a list of SystemIndexes used by each ComponentTypeId.
  result = ComponentParamInfo()

  # Initial parse of parameters to component ids and value nodes.
  for compNode in componentList:
    let
      tyName = compNode.findType
    
    doAssert tyName != "", "Cannot determine type name of argument:\n" & compNode.treeRepr & "\ngetType:\n" & compNode.getType.repr

    # Find the ComponentTypeId for this type.
    let
      typeId = id.typeStringToId(tyName)
    
    if typeId in result.passed:
      error "Passed more than one component of type " & tyName

    if typeId in result.passed:
      error "Component type " & tyName & " appears more than once in parameters"
    
    result.passed.add typeId
    result.values.add compNode

  # Process parameters to enforce dependent requirements.
  for i, typeId in result.passed:
    let
      ownerSystem = id.systemOwner(typeId)
      isOwned = ownerSystem != InvalidSystemIndex

    if isOwned:
      if ownerSystem notin result.ownedSystems:
        # Owning systems must have all their components fully satisfied.
        result.owned.add (typeId, ownerSystem, i)
        result.ownedSystems.add ownerSystem

        # Add component instances that must be valid to support the owner system for a component passed.
        for comp in id.ecsSysRequirements(ownerSystem):
          if comp != typeId and comp notin result.passed:
            if id.isOwned comp:
              let
                curName = id.typeName(comp)
                typeStr = id.typeName(typeId)
              
              error "Cannot add " & typeStr & ", missing required owned component " & curName

            elif comp notin result.requiredFetches:
              result.requiredFetches.add comp
              result.lookFor.add comp

    else:
      # Passed components that are not owned must generate a storage slot index.
      result.generateIdx.add i

    # Create a list of components we're missing that would potentially
    # satisfy all systems that use our parameters.
    # We only need to check systems directly using each component,
    # not dependent owner systems, as we can assume incomplete
    # ownership has been disallowed above.
    for sys in id.systems(typeId):

      let sysIsOwner = id.len_ecsOwnedComponents(sys) > 0
      if not sysIsOwner:
        # Owner systems that weren't in the parameters are ignored since
        # they cannot exist in isolation.

        if sys notin result.unownedSystems:
          result.unownedSystems.add sys

        for tId in id.ecsSysRequirements(sys):
          if tId != typeId and tId notin result.passed and tId notin result.lookFor:
            result.lookFor.add tId

  # Determine the number of systems that will have rows removed by this operation
  # due to the system failing a <not component> requirement.
  var
    sysProcessed: HashSet[SystemIndex]
  
  for typeId in result.passed:
    for sys in id.systems(typeId):
      if sys notin sysProcessed:
        sysProcessed.incl sys

        if typeId in id.ecsSysNegations sys:
          
          assert sys notin result.unsatisfied, "Duplicate unsatisfied systems"

          result.unsatisfied.add sys
