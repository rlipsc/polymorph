import polymorph, basic, forwardDecl/forwardDeclTest as fdcl, onevents, constructandclone, streaming

const
  compOpts = ECSCompOptions()
  entOpts = ECSEntityOptions()
  sysOpts = ECSSysOptions()

runBasic(entOpts, compOpts, sysOpts)
runForwardDecl()
runConstructAndClone()
runOnEvents()
testStreaming()

flushGenLog()
