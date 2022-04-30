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

import macros, ecsstatedb

when defined(ecsLogDetails) or defined(ecsPerformanceHints):
  from strutils import capitalizeAscii, repeat


proc debugMessage*(id: EcsIdentity, op: string, extraIndent = 0) = 
  when defined(ecsLogDetails):
    let
      curIndent = id.ecsCurrentOperation.len
      identStr = "  ".repeat curIndent + extraIndent

    debugEcho identStr & "[ " & capitalizeAscii(op) & " ]"
  else:
    discard


proc startOperation*(id: EcsIdentity, op: string) {.compileTime.} = 
  when defined(ecsLogDetails) or defined(ecsPerformanceHints):
    var curOps = id.ecsCurrentOperation

    when defined(ecsLogDetails):
      id.debugMessage(capitalizeAscii(op))

    if curOps.len == 0:
      curOps = nnkBracket.newTree()
    curOps.add newLit(op)
    id.set_ecsCurrentOperation curOps

  else:
    discard


proc endOperation*(id: EcsIdentity) {.compileTime.} = 
  when defined(ecsLogDetails) or defined(ecsPerformanceHints):
    var
      curOps = id.ecsCurrentOperation.copy

    curOps.expectKind nnkBracket
    if curOps.len == 0:
      error "Internal error: trying to end an operation when no operations exist"

    when defined(ecsLogDetails):
      if curOps.len == 1:
        # Space out final operations.
        debugEcho ""

    # TODO: Make operation finishing announcements an option.
    #   let
    #     lastOp = curOps[^1].strVal
    #   id.debugMessage "End " & lastOp, cr = true

    curOps.del curOps.len - 1
    id.set_ecsCurrentOperation curOps
  else:
    discard


template debugPerformance*(id: EcsIdentity, msg: string) =
  when defined(ecsPerformanceHints):
    const curIndent = id.ecsCurrentOperation.len()
    debugMessage(id, "Performance: " & "  ".repeat(curIndent) & msg)


proc ecsOperation*(id: EcsIdentity, opName: string, code: NimNode): NimNode =
  when defined(ecsLogDetails) or defined(ecsPerformanceHints):
    quote do:
      static: startOperation(EcsIdentity(`id`), `opName`)
      `code`
      static: endOperation(EcsIdentity(`id`))


template ecsBuildOperation*(id: EcsIdentity, opName: string, code: untyped): untyped =
  startOperation(id, opName)
  code
  endOperation(id)

