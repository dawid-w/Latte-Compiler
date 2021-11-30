## Latte Compiler
**Latte** to **llvm** compiler. 

### Usage:
```
make                            # build
./latc program.lat              # compile
```

### Project structure:
* **bad** - invalid usage examples
* **good** - valid usage examples
* **src**
    * **Latte** - Parser module, generated with BNFC
    * **LLVMCompiler.hs** - LLVM Compiler module
    * **Types.hs** - Auxiliary types module
    * **latte.cf** - Latte grammar
    * **Makefile**
* **Makefile**

## Dependencies
Compiler uses **BNFC** to generate Latte parser.