#
# This Makefile is not called from Opam but only used for
# convenience during development
#

DUNE 	= dune

.PHONY: all install test clean uninstall format

all:
	$(DUNE) build

install: all
	$(DUNE) install tcalc

uninstall:
	$(DUNE) uninstall

test:
	$(DUNE) runtest

clean:
	$(DUNE) clean

utop:
	$(DUNE) utop

run:
	$(DUNE) exec tcalc

yacc:
	ocamlyacc -v lib/parser.mly

format:
	$(DUNE) build --auto-promote @fmt
	opam lint --normalise tcalc.opam > tcalc.tmp && mv tcalc.tmp tcalc.opam
	dune format-dune-file dune-project > $$$$ && mv $$$$ dune-project
	git ls-files '**/*.[ch]' | xargs -n1 indent -nut -i8

# vim:ts=8:noet:

