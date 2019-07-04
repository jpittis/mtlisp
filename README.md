# MTLisp

Because who doesn't like creating GitHub repos before writing any code.

## Raison D'Etre

I learnt the basics of compiler and interpreter construction while working on
[olimpico-scheme](github.com/jpittis/olimpico-scheme). I ended up digging
myself a hole of bad design decisions in favour of hacking something together
quickly. This project is my attempt to do things right TM.

## Important Lessons from Olimpico Scheme

- Write disassembler and debug tooling from the beginning.
- Use a bunch of monads to simplify implementation:
  - State monad for passing around environment.
  - Fresh monad for generating unique labels.
  - CodeGen monad for generating instruction with a DSL.
- Parameterize the bytecode AST by address type. For example, high level
  labels, relative offset per function and then finally total offset.
- Use good software design principles (especially relating to decoupling)
  because there will be a lot of refactoring whenever dead ends are hit.
- Write the first version of the bytecode interpreter in Haskell to take
  advantage of shared datatypes.
- Actually read some books / papers and do a lot of thinking... compiler
  writing is actually kinda hard.
