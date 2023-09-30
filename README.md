# My "C" Compiler

This is a project following the [Write a compiler](https://norasandler.com/2017/11/29/Write-a-Compiler.html) series to implement a compiler for a small subset of C. I chose to use Rust for the implementation.

I've deviated from the tutorial in a couple of places in order to support the x86-64 instruction set. Currently, only 64 bit integers are supported/used though.

## How to run

In order to run the compiler, you first need to compile the compiler by running
```
cargo build --release
```
in the project folder.

Afterwards you can compile a file by running
```
./my_copiler.sh example.c
```
This will create an `./a.out` file in the file's directory. This script is mainly intended to be run by the `write_a_c_compiler` test suite. When inside of the `write_a_c_compiler` sub-director, you can run
```
./test_compiler.sh ../my_compiler.sh `seq 1 9`
```
to run the currently supported tests.

## Project Files

The source code is located in the `src` folder. The source is split into multiple files:
- `main.rs`: The main file, which parses the command line arguments and calls the other functions.
- `lexer.rs`: The lexer, which converts the input into a stream of tokens.
- `parser.rs`: The parser, which converts the stream of tokens into an abstract syntax tree.
- `ast.rs`: The abstract syntax tree, which is used to represent the parsed input.
- `generator.rs`: The generator, which converts the abstract syntax tree into assembly code.

## To Dos

- [ ] Add proper command line argument parsing
- [ ] Add unit tests
- [ ] Add docs
- [ ] Add support for more operators
- [ ] Replace some recursions with folds
- [ ] Modularize the code more
- [ ] Make ints 32 bit and add support for long
- [ ] Make context thread-safe