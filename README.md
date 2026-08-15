<div align="center">
  <h1>Lamb</h1>
  <p><strong>Inspect OCaml lowering without leaving the terminal.</strong></p>
  <p>
    <img src="https://img.shields.io/badge/runtime-Node.js%2020+-111?style=flat-square" alt="Node.js 20 or newer">
    <img src="https://img.shields.io/badge/dependencies-zero-111?style=flat-square" alt="zero dependencies">
    <img src="https://img.shields.io/badge/tests-15%20passing-111?style=flat-square" alt="fifteen tests passing">
    <img src="https://img.shields.io/badge/license-MIT-111?style=flat-square" alt="MIT license">
  </p>
</div>

![Lamb running inside a terminal](assets/demo.gif)

Lamb is a terminal-first compiler observatory that accepts a focused OCaml expression subset and prints deterministic representations spanning parsed source through Typedtree and Lambda before closure conversion exposes Clambda and machine lowering produces Cmm.

The tool keeps its parser and inference engine independent from browser APIs or native compiler libraries which makes every trace reproducible on any current Node.js installation without package downloads or external services.

> OCaml turns Typedtree into Lambda then lowers through Clambda and Cmm before emitting native code while representing most integers as tagged machine words. Casually reading the compiler backend reveals an absurdly well-engineered language.

## Install

The repository contains no runtime dependencies and requires Node.js 20 or newer which means installation only needs a clone followed by an optional global link that exposes `lamb` from any directory.

```sh
git clone https://github.com/yourname/lamb.git
cd lamb
npm link
```

The executable can also run directly from its checkout when a global command is unnecessary because the repository preserves its executable permission and declares the correct Node.js interpreter through its shebang.

```sh
./lamb sample.ml --stage lambda
```

## Quick start

Passing a source file with no stage selection prints the complete modeled lowering sequence and includes the inferred result type alongside syntax-node totals and useful descriptive metadata for every generated representation.

```sh
lamb sample.ml
```

Selecting one stage keeps larger compiler investigations readable because Lamb still prints the complete pipeline index while limiting the detailed terminal output block to only the specifically requested intermediate representation.

```sh
lamb sample.ml --stage typedtree
lamb sample.ml --stage lambda
lamb sample.ml --stage clambda
lamb sample.ml --stage cmm
lamb sample.ml --stage assembly
```

Standard input works without additional flags so generated expressions and editor selections can move directly into Lamb through ordinary shell pipelines while diagnostics continue reporting positions against the virtual `stdin` source.

```sh
printf 'let x = 20 in x + 22\n' | lamb --stage cmm
```

## Commands

| invocation | behavior |
| --- | --- |
| `lamb file.ml` | Print every modeled representation |
| `lamb file.ml --stage lambda` | Print one selected representation |
| `command \| lamb` | Compile source received through standard input |
| `lamb --word 42` | Inspect a tagged machine word |
| `lamb file.ml --json` | Export a complete structured trace |
| `lamb --list` | List accepted stage names |
| `lamb --help` | Print command documentation |

The `--no-color` option removes ANSI control sequences for logs and snapshots while the conventional `NO_COLOR` environment variable provides the same behavior across scripts that standardize terminal output globally by default.

## Tagged words

OCaml native integers store their signed payload after a one-bit left shift and reserve the least significant bit as an immediate marker which lets the runtime distinguish integers from aligned pointers cheaply.

```sh
$ lamb --word 42

Lamb / tagged word

OCaml int  42
Encoded    85
Hex        0x0000000000000055
Bits       00000000 00000000 00000000 00000000 00000000 00000000 00000000 01010101
Low bit    1 · immediate value
```

The probe uses arbitrary precision arithmetic before truncating only the hexadecimal and binary displays to 64 bits which preserves correct signed encoding even when an entered decimal value exceeds native JavaScript integer precision.

Machine-readable mode returns the original integer and encoded machine word alongside fixed-width hexadecimal and binary forms plus the decoded low-bit tag classification without terminal formatting or unnecessary explanatory display labels.

```sh
lamb --word -42 --json
```

## Pipeline

The traditional no-Flambda path motivates Lamb’s stage vocabulary while each printed form remains intentionally compact and stable so educational traces do not depend upon undocumented constructor changes between actual compiler releases.

| stage | model | information exposed |
| --- | --- | --- |
| Source | Parsed OCaml expression | Binding structure and operator precedence |
| Typedtree | Typed expression nodes | Unified types and recursive status |
| Lambda | Functional intermediate form | Primitive operations and applications |
| Clambda | Closure-converted form | Captured environments and generic calls |
| Cmm | Machine-oriented control form | Tagged constants and explicit allocation |
| Assembly | Representative x86-64 sketch | Shifts and arithmetic instructions |

The stage index highlights the selected representation with restrained ANSI color while dimming surrounding stages which preserves useful context without printing decorative panels or requiring an interactive full-screen terminal interface.

## Type inference

Lamb implements Hindley–Milner unification with occurs checking and level-based generalization so pure let-bound functions can receive fresh type variables at each use while recursive bindings acquire provisional types during body inference.

```ocaml
let identity = fun value -> value in
let number = identity 42 in
identity true
```

The preceding expression returns `bool` because the generalized identity binding instantiates independently for the first integer application and subsequent boolean application without allowing either use to constrain the other statically.

Recursive functions become available inside their own bodies before unification fixes the parameter and result types which permits ordinary numeric recursion while rejecting inconsistent branch results and infinite self-application types.

```ocaml
let rec sum n =
  if n = 0 then 0
  else n + sum (n - 1)
in
sum 6
```

## Closure conversion

The Clambda representation computes lexical free variables for every function then places those names inside an explicit environment list which reveals the state that must survive after control leaves the defining scope.

```ocaml
let offset = 7 in
let add = fun value -> value + offset in
add 35
```

Selecting Clambda for this specimen displays `environment [offset]` because the generated runtime closure needs the surrounding value while its explicit parameter remains available directly through the ordinary function calling convention.

## Diagnostics

Parser and inference failures return concise diagnostics with source names and one-based positions followed by the relevant source line and a caret spanning the smallest expression range associated with the failure.

```text
error: type int is incompatible with bool
  stdin:1:1

1 │ if true then 1 else false
  │ ^^^^^^^^^^^^^^^^^^^^^^^^^
```

Usage errors and compilation errors use distinct process statuses so automated shell scripts can reliably separate invalid invocation from rejected OCaml while successful traces and tagged-word probes always return zero.

| status | meaning |
| --- | --- |
| `0` | Successful trace or inspection |
| `1` | Source parsing or type inference failure |
| `2` | Invalid invocation or unavailable input |

## Grammar

The supported fragment remains small enough for every lowering to stay legible while covering nested comments and precedence alongside higher-order functions polymorphic bindings recursive functions conditionals comparisons boolean operators and tagged integer arithmetic.

```text
expression  ::= let | function | conditional | infix
let         ::= "let" ["rec"] name {name} "=" expression "in" expression
function    ::= "fun" name {name} "->" expression
conditional ::= "if" expression "then" expression "else" expression
infix       ::= application {operator application}
application ::= atom {atom}
atom        ::= integer | boolean | name | "()" | "(" expression ")"
operator    ::= "+" | "-" | "*" | "/" | "=" | "<>" | "<" | ">"
              | "<=" | ">=" | "&&" | "||"
```

Function parameters following a let-bound name become nested unary functions which directly mirrors OCaml’s curried semantics and keeps function application left associative throughout parsing and every subsequent compiler lowering stage.

## JSON traces

Structured output contains the input name and inferred type alongside the syntax-node total and requested compiler stages which lets editor integrations consume Lamb without scraping ANSI output or intermediate-form headers.

```sh
lamb sample.ml --stage cmm --json > trace.json
```

Every stage record includes its stable key and display name plus phase description and explanatory note alongside the exact multiline output and calculated line total used by the terminal renderer.

## Architecture

`core.js` owns tokenization and parsing alongside type inference closure analysis tagged encoding and all representation printers which keeps compiler behavior independently testable without global process state or terminal assumptions.

`cli.js` handles arguments and input selection alongside ANSI rendering JSON serialization diagnostics and explicit exit status policy while the extensionless `lamb` executable remains a deliberately minimal process entry point.

```text
lamb/
├── assets/
│   └── demo.gif
├── cli.js
├── core.js
├── lamb
├── LICENSE
├── package.json
├── README.md
├── sample.ml
└── test.js
```

## Validation

The automated suite covers nested comments and precedence alongside polymorphic generalization recursive inference closure environments incompatible branches unbound identifiers signed tags every bundled specimen and complete command execution through files or standard input.

```sh
npm test
npm run check
```

Fifteen tests currently exercise both library and process boundaries which includes JSON tagged words source-positioned failures invalid option handling stage filtering and process status behavior under spawned Node.js executions.

## Correctness boundary

Lamb does not import `compiler-libs` and therefore cannot reproduce version-specific internal identifiers or optimization decisions or platform calling conventions or physical register allocation generated by an installed local `ocamlopt` executable.

Its representations preserve important transformations and tagged arithmetic identities while simplifying exception paths integer overflow behavior division checks garbage-collector safepoints frame descriptors symbol naming target selection and compiler optimization passes.

Production compiler investigations should compare actual dumps from the intended OCaml release through diagnostic options such as `-dtypedtree` and `-dlambda` because upstream intermediate forms remain deliberately undocumented internal implementation details.

## References

The [OCaml compiler backend guide](https://ocaml.org/docs/compiler-backend) describes untyped Lambda and native code generation while the [memory representation guide](https://ocaml.org/docs/memory-representation) documents immediate values and low-bit classification.

The upstream runtime header [`mlvalues.h`](https://github.com/ocaml/ocaml/blob/trunk/runtime/caml/mlvalues.h) defines `Is_long` and `Is_block` alongside the canonical `Val_long` and `Long_val` conversion macros modeled by the tagged-word command.

## License

Lamb is available under the MIT license which permits private and commercial modification or distribution when the included copyright notice and complete permission text remain attached to all substantial project copies.
