# hasm

![Rust Build & Test](https://github.com/AshtonSnapp/hasm/actions/workflows/rust.yml/badge.svg)

The Homebrew Assembler. Currently supporting the 16-bit Cellia architecture and the 8-bit [ROCKET88 architecture](https://github.com/SlithyMatt/rocket88/).

Each architecture supported by `hasm` will be separated into its own module, although every architecture's assembler code will have the same general structure: you have a _lexer_ which takes in files of assembly code and outputs streams of tokens which are fed into a _parser_ which structures those tokens into a _file syntax tree_. Then the syntax trees are fed into a _linker_ which tries to combine all of these trees into a single _program tree_, which is finally fed into a _binary generator_ which does exactly what you think.

Right now I'm still trying to implement the assemblers for the two architectures I mentioned earlier, and I've only just now gotten to the parser. It's going to be a pain to write anything that's actually decently capable, but it'll be worth it. While Cellia is my brain-child, I'm going to write the ROCKET88 parser first since it's a simpler architecture.

## Building `hasm`

Clone this repository to your local machine, `cd` into the directory, and run `cargo build`. Simple!
