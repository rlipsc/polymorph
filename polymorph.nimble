packageName   = "polymorph"
version       = "0.2.1"
author        = "Ryan Lipscombe"
description   = "A compile time focused entity-component-system generator"
license       = "Apache License 2.0"
skipDirs      = @["examples", "tests"]

requires "nim >= 1.4.2"

task test, "Test suite":
  exec "nim c -d:debug -r tests/testall"
