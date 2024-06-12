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
  "TCalc implements a minimal desktop calculator that in addition to floating point numbers recognises durations in hh:min:sec format and converts them to seconds. This can simplify time-based calculations.")
 (depends
  (dune
   (>= "2.0"))
  (ocaml
   (>= 4.08.0))
  (linenoise
   (>= 1.3.1))))
