## It can be useful to automate the creation of systems for separate
## components.
## 
## Here we're incorporating external component types into a system.
## 
## Since the system can't know the component to be used ahead of time,
## we can't use the name to access it in `item`.
## 
## Instead, we alias the given component and then can access it directly.
## 
## This lets you build library components that seamlessly combine with
## user components.

import polymorph

template systemUsingComp*(userComponent: typedesc) {.dirty.} =
  makeSystem "use" & $userComponent, [uc: userComponent]:
    # The 'uc:' prefix sets the name of the component's access template
    # within the system.
    echo "Running ", sys.name
    all:
      # Aliasing to 'uc' lets us access the data for 'userComponent'
      # without knowing the type's name.
      echo uc
      
      # Alternatively, we can use 'access' on the 'userComponent' type.
      assert uc == userComponent.access

# Create component to use with our systems
register defaultCompOpts:
  type
    MyComp1 = object
      data: int
    MyComp2 = object
      data: int

# Automate the system for our two components.
systemUsingComp(MyComp1)
systemUsingComp(MyComp2)

makeEcsCommit "run"

let e = newEntity(MyComp1(data: 2), MyComp2(data: 3))

run()
