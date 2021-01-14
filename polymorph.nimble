packageName   = "Polymorph"
version       = "0.1.0"
author        = "rlipsc"
description   = "Compile-time composable entity component system generator"
license       = "Apache License 2.0"

requires "nim >= 1.0.0"

task test, "Test suite":
  exec "nim c -d:debug -r tests/testall"
