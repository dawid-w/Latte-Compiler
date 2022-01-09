## Latte Compiler
**Latte** to **llvm** compiler. 

### Usage:
```
make                            # build
./latc_llvm program.lat         # compile -> generates .ll and .bc files
```

### Project structure:
* **bad** - invalid usage examples
* **good** - valid usage examples
* **src**
    * **Latte** - Parser module, generated with BNFC
    * **Compiler.hs** - LLVM Compiler module
    * **StaticAnalysis.hs** - Static Analysis module
    * **Types.hs** - Auxiliary compiler module
    * **Env.hs** - Auxiliary compiler module
    * **Instructions.hs** - Auxiliary compiler module
    * **latte.cf** - Latte grammar
    * **main.hs** - Main Compiler file
    * **Makefile**
* **Makefile**
* **run_good.sh** - Run valid usage examples
* **run_bad.sh** - Run invalid usage examples
* **test.sh** - Run all test cases
* **lib/runtime.c** - Lib file

## Dependencies
Compiler uses **BNFC** to generate Latte parser.
