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

when defined(ecsLogDetails):
  from strutils import capitalizeAscii

proc startOperation*(id: EcsIdentity, op: string) {.compileTime.} = 

  id.set_ecsCurrentOperation op

  when defined(ecsLogDetails):
    let
      curOp = id.ecsCurrentOperation()
      curIndent = id.ecsCurrentOperationIndent()
      newIndent = curIndent & "  "
    id.set_ecsCurrentOperationIndent newIndent
    debugEcho curIndent & "[ ", capitalizeAscii(curOp) & " ]"

proc endOperation*(id: EcsIdentity) {.compileTime.} = 

  when defined(ecsLogDetails):
    let
      curOp = id.ecsCurrentOperation()
      curIndent = id.ecsCurrentOperationIndent()
      newIndent =
        if curIndent.len > 2:
          curIndent[2..^1]
        else:
          ""
    id.set_ecsCurrentOperationIndent newIndent

  id.set_ecsCurrentOperation ""

template debugPerformance*(id: EcsIdentity, msg: string) =
  when defined(ecsPerformanceHints):
    debugEcho "Performance: " & id.ecsCurrentOperationIndent() & msg

proc ecsOperation*(id: EcsIdentity, opName: string, code: NimNode): NimNode =
  quote do:
    static: startOperation(EcsIdentity(`id`), `opName`)
    `code`
    static: endOperation(EcsIdentity(`id`))

template ecsBuildOperation*(id: EcsIdentity, opName: string, code: untyped): untyped =
  startOperation(id, opName)
  code
  endOperation(id)

