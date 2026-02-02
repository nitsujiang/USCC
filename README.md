# USCC - University Simple C Compiler

A compiler for the USC (University Simple C) language, built using the LLVM framework as its code generation backend.

Developed as part of CSE 504: Compiler Design at Stony Brook University.

Original framework by Sanjay Madhav (madhav@usc.edu).

## Features

- **Lexical Analysis**: Flex-based scanner for tokenization
- **Recursive Descent Parser**: Hand-written parser with AST generation
- **Semantic Analysis**: Type checking, symbol table management, scope resolution
- **SSA Construction**: Static Single Assignment form for optimization
- **Optimizations**:
  - Constant folding and propagation
  - Constant branch elimination
  - Dead block removal
  - Loop Invariant Code Motion (LICM)
- **Code Generation**: LLVM IR bitcode output

## Building

Requires LLVM 3.5 libraries to be built and available.

```bash
cd uscc
make
```

The compiler binary will be at `bin/uscc`.

## Usage

```bash
# Print AST
./bin/uscc -a input.usc

# Generate LLVM bitcode (default)
./bin/uscc input.usc -o output.bc

# Generate with optimizations
./bin/uscc -O input.usc -o output.bc

# Print LLVM IR to stdout
./bin/uscc -p input.usc

# Print symbol table
./bin/uscc -l input.usc
```

## Testing

Run tests from the `tests/` directory using Python 2:

```bash
cd tests
python2 testSemant.py   # Semantic analysis tests (43 tests)
python2 testEmit.py     # Code generation tests (21 tests)
python2 testOpt.py      # Optimization tests (21 tests)
```

## Project Structure

```
uscc/
├── scan/       # Lexical analyzer (Flex)
├── parse/      # Parser, AST nodes, semantic analysis, code emission
├── opt/        # Optimization passes (SSA, constant folding, LICM, etc.)
├── uscc/       # Main driver
├── tests/      # Test suite
└── report/     # Assignment reports (PA1-PA5)
```

## License

BSD License - See LICENSE.TXT for details.

