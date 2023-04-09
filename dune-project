(lang dune 3.7)

(name Ocaban)

(generate_opam_files false)

(source
 (github elzibus/Ocaban))

(authors "elzibus")

(maintainers "elzibus")

(license unlicense)

(package
 (name Ocaban)
 (synopsis "sokoban game")
 (description "sokoban game in OCaml")
 (depends ocaml dune)
 (tags
  (topics ocaml game sokoban "ocaml-canvas")))
