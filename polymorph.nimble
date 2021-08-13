packageName   = "Polymorph"
version       = "0.2.1"
author        = "Ryan Lipscombe"
description   = "An entity-component-system generator"
license       = "Apache License 2.0"
skipDirs      = @["examples"]

requires "nim >= 1.4.2"

task test, "Test suite":
  exec "nim c -d:debug -r tests/testall"
