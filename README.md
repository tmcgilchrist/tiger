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
opam switch 4.03.0-tiger --alias 4.03.0

# Install dependencies
opam install core_kernel menhir oUnit utop

./configure --enable-tests

make && make test
```

Tests are under `test` directory.

Tiger Examples
---------------------

There are some examples of Tiger programs under `examples`.


Copyright and license
---------------------

tiger is distributed under the terms of the Berkeley software distribution
license (3 clauses).
