(lang dune 2.0)

(name tcalc)

(generate_opam_files true)

(source
 (github lindig/tcalc))

(license Unlicense)

(authors "Christian Lindig <lindig@gmail.com>")

(maintainers "Christian Lindig <lindig@gmail.com>")

(package
 (name tcalc)
 (synopsis "Minimal desktop calculator for timestamps")
 (description
  "TCalc implements a minimal desktop calculator that in addition to\n  floating point numbers recognises durations in hh:min:sec format and\n  converts them to seconds. This can simplify time-based calculations.\n  This command-line application provides command-line editing and\n  history. Command math functions line sin or exp are available.")
 (depends
  (dune
   (>= "2.0"))
  (ocaml
   (>= 4.13.0))
  (linenoise
   (>= 1.3.1))))
