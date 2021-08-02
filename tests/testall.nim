import polymorph

import basic, forwardDecl/forwardDeclTest as fdcl, onevents,
  constructandclone, streaming, groupsandorder, transitioncomponents,
  tables, unittest

const
  compOpts = ECSCompOptions()
  entOpts = ECSEntityOptions()
  sysOpts = ECSSysOptions()


static:
  defaultIdentity.set_private true

block:
  runBasic(entOpts, compOpts, sysOpts)
  
  # Check we can create entities with components from `runBasic`.
  let e = newEntityWith(IntCont(), AddOne())
  e.expectSystems ["incInt", "multipleBlocks"]
  e.delete
block:
  defineConstructAndClone(entOpts, compOpts, sysOpts)
  runConstructAndClone()
block:
  runGroupsAndOrder()
block:
  testTransitionComponents()
block:
  defineStreaming()
  testStreaming()

static:
  defaultIdentity.set_private false

runForwardDecl()
runOnEvents()

flushGenLog()
