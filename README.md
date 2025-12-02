# Yet Another HDL Attempt

## Some Commands


```bash
mill hdl.runMain hdl.demo
mill hdl.runMain hdl.elaborateTest
mill hdl.runMain hdl.compileTimeTypeTests
```

- will things like automatic port construction & lazy modules work with this approach?
- refactor???


things to test:

- io bundle directions
- connection type checking
- `reduce`, `foreach` function test
- see if inheritance btw modules work
- type parameterization of modules
- parallel elaboration
    - check that all instances of a module with different params are all elaborated


TODO

- Don't flatten io ports in `registerIO`

- operators
- vectors and heterogenous vectors
- behavioral statements
- memories
