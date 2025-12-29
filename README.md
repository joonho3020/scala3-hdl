# Hardware Description Language Embedded in Scala3

The syntax of this HDL is mostly similar to Chisel.
The major API differences are listed in [HDL Features](./docs/hdl-features.md).
To generate Verilog, this HDL emits CHIRRTL and passes the output to CIRCT.

## Some Example Designs

- [RISC-V superscalar in-order core](./riscv_inorder)
- [RISC-V superscalar out-of-order core](./riscv_ooo)

## Documentation

To generate the API docs run:

```bash
./mill hdl.docJar
```

The Scaladoc HTML files are located at (open this in your browser):

```bash
out/hdl/scalaDocGenerated.dest/javadoc/index.html
```
