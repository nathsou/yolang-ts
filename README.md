# Yolang

Simple procedural language targeting LLVM

## Usage

Follow the [llvm-bindings](https://github.com/ApsarasX/llvm-bindings#install) instructions to install llvm.


```bash
$ yarn install
$ node build/yolang.js examples/euler2.yo
```

```
Usage: yolang [options] <input>

Arguments:
  input                input source file

Options:
  -V, --version        output the version number
  -o, --out <file>     output file
  -t, --target <name>  target: host | wasm | wasi (default: "host")
  -r, --run            compile and run the program
  -a, --artifacts      build artificats directory
  -T, --type-check     type check only
  -D, --debug          create a debug build
  --show:types         show types
  --show:sweet         show sweet IR
  --show:bitter        show bitter IR
  --show:mono          show the monomorphized program
  --show:llvm          show LLVM IR
  --show:time          show pipeline timing
  -h, --help           display help for command
```

## References

- Sérgio Medeiros, Fabio Mascarenhas, 2018, [Syntax Error Recovery in Parsing Expression Grammars](https://arxiv.org/abs/1806.11150)
- Daan Leijen, 2005, [Extensible records with scoped labels](https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/)