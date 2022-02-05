template defineStreaming* {.dirty.} =
  ## Some basic tests to affirm the number of passes streaming systems perform.

  registerComponents(defaultComponentOptions):
    type
      StreamA = object
        value: int
      StreamB = object
        value: int

  makeSystem("stream", [StreamA, StreamB]):
    var processed: int
    stream:
      processed += 1
    check processed == sys.streamRate

  makeSystem("streamFixed", [StreamA, StreamB]):
    const fixedStreamRate = 50
    var processed: int
    stream fixedStreamRate:
      processed += 1
    check processed == min(fixedStreamRate, sys.groups.len)

  makeSystem("streamMulti", [StreamA, StreamB]):
    var processed: int
    sys.streamRate = 50
    stream multipass:
      processed += 1
    check processed == sys.streamRate

  makeSystem("streamMultiFixed", [StreamA, StreamB]):
    let rate = 20
    var processed: int
    stream multipass rate:
      processed += 1
    stream rate multipass:
      processed += 1
    check processed == rate * 2

  makeSystem("streamSto", [StreamA, StreamB]):
    var processed: int
    sys.streamRate = 60
    stream stochastic:
      processed += 1
    # Stochastic implies multipass.
    check processed == sys.streamRate

  makeSystem("streamStoFixed", [StreamA, StreamB]):
    const rate = 5
    var processed: int
    stream stochastic rate:
      processed += 1
    stream rate stochastic:
      processed += 1
    check processed == rate * 2


  makeEcs()
  commitSystems("run")

  let totalEnts = 10
  for i in 0 ..< totalEnts:
    discard newEntityWith(StreamA(), StreamB())

  proc testStreaming =
    suite "Streaming":
      test "All":
        for i in 0..1:
          run()

when isMainModule:
  import polymorph, unittest

  defineStreaming()
  testStreaming()
