tiger - Compiler for Tiger language from "Modern Compiler Implementation
in ML"
===============================================================================

The Tiger language is a small language with nested functions, record values with implicit pointers, arrays, interger, string variables and a few simple structured control constructs.

Development
---------------------

For development you will need to install `core` and `menhir`. For running tests you will need to install
the `oUnit` package and reconfigure the build process to enable tests:

``` shell
# Setup isolated sandbox/switch
opam switch 4.06.1-tiger --alias 4.06.1

# Install dependencies
opam install "dune=1.3.0"
opam pin add -n tiger .
opam install --deps-only tiger -t

# Building
jbuilder build

# Testing
jbuilder runtest
```

Tests are under `test` directory.

Tiger Examples
---------------------

There are some examples of Tiger programs under `examples`.


Copyright and license
---------------------

tiger is distributed under the terms of the Berkeley software distribution
license (3 clauses).

Resources
---------------------

http://janmidtgaard.dk/quickcheck/slides/day3.pdf
https://github.com/iitaku/tiger/blob/master/chap4/ocaml/semant.ml
https://github.com/gnuvince/ocaml-tiger/blob/master/semant.mli
https://github.com/thizanne/tiger/blob/14e4c9a6005d1455122f366a965e487434219f47/src/semant.ml
