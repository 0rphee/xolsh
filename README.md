# xolsh - hslox

```sh
# Build
cabal build xolsh-exe
# Run a lox file
cabal run xolsh-exe -- file.lox
# Run interactive lox repl
cabal run xolsh-exe
```

A complete Haskell tree-walking implementation of the `lox` programming language (jlox) from the [Crafting Interpreters Book](https://craftinginterpreters.com/). It fully passes the jlox test suite and has an [online playground](https://0rphee.github.io/xolsh/).

The code style of the interpreter is mostly a port/adaptation of the original java implementation, with a more idiomatic Haskell style where possible.

- Areas for improvement:
   - Try to write a one-pass scanning-parsing step with parser combinators (w/flatparse) to compare performance.
   - Optimize variable & field access (through vectors?).
   - A rewrite with an effect system would probably give many benefits, specially to the code style.
