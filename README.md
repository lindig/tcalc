

![Build](https://github.com/lindig/tcalc/workflows/CI/badge.svg)

# TCalc

This is a minimal desktop calculator (taken from the Lex/Yacc section of
the [OCaml] manual) that can parse `3:04.5` as a time and covert it into
184.5 seconds. I'm using it for simple time calculations.

```
$ tcalc
3:04.5
184.50 (00:03:04.50)
11:52.2 / 2670 * 500
133.37 (00:02:13.37)
$ 
```
* Operators: + - * / ^
* Command line editing and history
* Variables

# Installation via Opam

[Opam] is the [OCaml] package manager that installs packages by
compiling them from source code.

```
$ opam install tcalc
```

# Installation from Sources

This is an [OCaml] project, it assumes you have available on a Unix
system:

* OCaml
* Dune
* Make

```
make 
make install
```

# Contribute

If you find this useful, please contribute back by raising pull
requests for improvements you made.

[OCaml]:  https://www.ocaml.org/
[Opam]:   http://opam.ocaml.org
