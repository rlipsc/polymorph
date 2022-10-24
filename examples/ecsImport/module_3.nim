## When `makeEcsCommit` runs it outputs the system code defined in
## 'subdir/module_2'.
## 
## The "bar" system defined there uses `ecsImport` to perform a relative
## import from 'module_1'. If we didn't do this, the `foo` variable that
## "bar" accesses couldn't be seen here.
## 
## `makeEcs` will first try these imports with a path relative to the
## call site of `ecsImport`. In this case the 'subdir' path is prefixed
## to arrive at 'import subdir/module_1'.
## 
## If this can't be compiled, the untouched 'import module_1' is run.

import polymorph, subdir/module_2

makeEcsCommit "run"
