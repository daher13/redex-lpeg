# Redex-LPEG

**Redex-LPEG** is a Parsing Expression Grammar (PEG) interpreter and compiler implemented in [Racket](https://racket-lang.org/). It translates PEG grammars into a custom Low-level PEG (LPEG) instruction set and executes them using a virtual machine.

## Features

- **PEG Grammar Definition**: Define grammars using PEG notation.
- **Compilation to LPEG**: Convert PEG grammars into a sequence of LPEG instructions.
- **Virtual Machine Execution**: Execute LPEG instructions to parse input strings.
- **Type System Integration**: Incorporate type checking within grammar definitions.
- **Visualization Tools**: Visualize grammars and their corresponding instruction sets.

## Project Structure

- `peg.rkt`: Defines the PEG grammar structures and parsing rules.
- `peggen.rkt`: Compiles PEG grammars into LPEG instruction sequences.
- `lpeg.rkt`: Implements the virtual machine to execute LPEG instructions.
- `type-system.rkt`: Provides type checking functionalities for grammars.
- `view.rkt`: Contains tools for visualizing grammars and instructions.
- `main.rkt`: Entry point for running the interpreter.
- `compiler/`: Directory containing additional compilation tools and resources.

## Getting Started

### Prerequisites

- [Racket](https://racket-lang.org/) installed on your system.

### Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/daher13/redex-lpeg.git
   cd redex-lpeg

2. Run the interpreter:

   ```bash
   racket main.rkt
