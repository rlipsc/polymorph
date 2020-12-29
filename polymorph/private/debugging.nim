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

import os

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

