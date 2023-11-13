# Lambda Interpreter

A simple interpreter for a toy language `L` that is defined as:
```shell
<e> ::= <id>                  ;; identifer (C-style)
      | \ <id> . <e>          ;; abstraction
      | <e> <e>               ;; application
      | let <id> = <e> in <e> ;; let expression
```

An example program is presented in `test/factorial.l`. It computes `8! = 40320`.

The implementation is based on a notion of environments that do not map ids to values, but rather to "continuations"(a variant of the CEK machine).

To view the evaluation context after `n` β-reductions, do:
```shell
dune clean && dune build
./run -step <n> <input_file>
```
