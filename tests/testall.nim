import polymorph, basic, forwardDecl/forwardDeclTest as fdcl, onevents

const
  compOpts = ECSCompOptions()
  entOpts = ECSEntityOptions()
  sysOpts = ECSSysOptions()

runBasic(entOpts, compOpts, sysOpts)
runForwardDecl()
runOnEvents()

flushGenLog()
