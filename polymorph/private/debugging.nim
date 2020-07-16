import os, strformat

proc debugMsg*(prefix, msg: string) =
  ## Report the string `s` along with the line number and proc it was invoked outside of Polymorph.
  let entries = getStackTraceEntries()

  const
    sourcePath = currentSourcePath()
    sourceRoot = sourcePath.parentDir.parentDir.splitPath[0]

  # Find first stack line outside of the current source directory.
  var stackIdx: int
  for idx in countDown(entries.high, 0):
    let
      fn = $entries[idx].filename
      dir = fn.parentDir.splitPath[1]
    if dir[0..min(dir.high, sourceRoot.high)] != sourceRoot:
      stackIdx = idx
      break
  let
    stack = entries[stackIdx]
    fnStr = $(stack.filename)
    fn = fnStr.extractFilename 
    debugPrefix = prefix & ": [" & fn & " in " & $stack.procname & ", line: " & $stack.line & "]: "
  debugEcho debugPrefix & msg

template debugPerformance*(msg: string) =
  when defined(debugSystemPerformance):
    debugEcho "Performance: ", msg

template debugPerformanceAtLine*(msg: string) =
  when defined(debugSystemPerformance):
    debugMsg "Performance", msg

