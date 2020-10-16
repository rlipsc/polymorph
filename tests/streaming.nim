import polymorph, unittest

#[
  Some basic tests to affirm the number of passes streaming systems perform.
]#

registerComponents(defaultComponentOptions):
  type
    StreamA = object
      value: int
    StreamB = object
      value: int

makeSystem("streamByRate", [StreamA, StreamB]):
  start:
    var processed: int
  stream:
    processed += 1
  finish:
    check processed == sys.streamRate

const fixedStreamRate = 50
makeSystem("streamed", [StreamA, StreamB]):
  start:
    var processed: int
  stream fixedStreamRate:
    processed += 1
  finish:
    check processed == min(fixedStreamRate, sys.groups.len)

const multiPassRate = 20
makeSystem("streamMulti", [StreamA, StreamB]):
  start:
    var processed: int
  stream multipass multiPassRate:
    processed += 1
  finish:
    check processed == multiPassRate

const stochasticRate = 5
makeSystem("streamSto", [StreamA, StreamB]):
  start:
    var processed: int
  stream stochastic stochasticRate:
    processed += 1
  finish:
    check processed == stochasticRate

makeEcs()
commitSystems("run")

let totalEnts = 10
for i in 0 ..< totalEnts:
  discard newEntityWith(StreamA(), StreamB())

proc testStreaming* =
  suite "Streaming":
    test "All":
      for i in 0..1:
        run()

when isMainModule:
  testStreaming()
