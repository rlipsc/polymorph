import polymorph, basic, forwardDecl/forwardDeclTest as fdcl, onevents, constructandclone

const
  compOpts = ECSCompOptions()
  entOpts = ECSEntityOptions()
  sysOpts = ECSSysOptions()

runBasic(entOpts, compOpts, sysOpts)
runForwardDecl()
runConstructAndClone()
runOnEvents()

flushGenLog()
