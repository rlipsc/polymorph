packageName   = "Polymorph"
version       = "0.2.0"
author        = "Ryan Lipscombe"
description   = "An entity-component-system generator"
license       = "Apache License 2.0"

requires "nim >= 1.4.2"

task test, "Test suite":
  exec "nim c -d:debug -r tests/testall"
