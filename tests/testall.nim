import polymorph

import testbasic, forwardDecl/forwardDeclTest as fdcl, testonevents,
  testeventspectrum, testownedcomponents, testconstructandclone,
  teststreaming, testgroupsandorder, testtransitioncomponents,
  testcrossownership, testnegation,
  tables, unittest, strutils, strformat, random

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
  defineCrossOwnership()
block:
  testNegation()
block:
  testOwnedComponents()
block:
  defineStreaming()
  testStreaming()
block:
  runForwardDecl()
block:
  testEvents(defaultCompOpts, defaultSysOpts)
block:
  const
    cOpts = fixedSizeComponents(100)
    sOpts = fixedSizeSystem(100)
    eOpts = fixedSizeEntities(100)
  defineEventSpectrum(cOpts, sOpts, eOpts)  

static:
  defaultIdentity.set_private false


flushGenLog()
