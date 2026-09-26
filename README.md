# schemerize

A small Scheme interpreter written in Haskell, built by working through
[Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).
It includes many of the tutorial's exercises along the way.

This is a work in progress. So far it can parse expressions, evaluate a set of
primitive functions, and report errors.

## Features

**Parsing**
- Atoms, strings (with `\"`, `\\`, `\n`, `\r`, `\t` escapes) and booleans (`#t`, `#f`)
- Numbers: integers, radix prefixes (`#b`, `#o`, `#d`, `#x`), floats (`3.14`),
  rationals (`3/4`) and complex numbers (`3+4i`)
- Characters: `#\a`, `#\space`, `#\newline`
- Lists, dotted lists (`(a . b)`) and vectors (`#(1 2 3)`)
- Quote shorthand: `'x`, `` `x ``, `,x` and `,@x`

**Evaluation**
- Self-evaluating values and `quote`
- Arithmetic on integers: `+`, `-`, `*`, `/`, `mod`, `quotient`, `remainder`
- Type predicates: `symbol?`, `string?`, `char?`, `boolean?`, `number?`,
  `complex?`, `real?`, `rational?`, `integer?`, `list?`, `pair?`, `null?`, `vector?`
- Symbol conversion: `symbol->string`, `string->symbol`

**Errors**

Parse errors, wrong numbers of arguments, type mismatches and unknown functions
are printed as messages instead of crashing the interpreter.

## Building

You need GHC and cabal. It was developed with GHC 9.10 and cabal 3.16.

```sh
cabal build
```

## Usage

The interpreter evaluates a single expression passed as a command-line argument:

```sh
$ cabal run schemerize -- "(+ 2 3)"
5
$ cabal run schemerize -- "(symbol->string 'abc)"
"abc"
$ cabal run schemerize -- "(+ 2)"
Expected 2 args; found values 2
$ cabal run schemerize -- "(symbol->string 5)"
Invalid type: expected symbol, found 5
```

## License

MIT. See [LICENSE](LICENSE).
