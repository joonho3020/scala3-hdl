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

- check that all instances of a module with different params are all elaborated
