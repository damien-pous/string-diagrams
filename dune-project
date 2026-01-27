(lang dune 3.14)

(name string-diagrams)

(generate_opam_files true)

(source
 (github damien-pous/string-diagrams))

(authors "Damien Pous")

(maintainers "Damien Pous")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name string-diagrams)
 (synopsis "visualize and edit string diagrams")
 (description "visualize and edit string diagrams")
 (depends ocaml dune cairo2-gtk vg store)
 (tags
   ("string diagrams" "monoidal categories")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
