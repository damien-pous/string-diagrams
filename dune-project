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
 (depends
	ocaml dune cairo2-gtk vg store tiny_httpd
	(ocaml-lsp-server :with-dev-setup)
	(ocamlformat :with-dev-setup)
	(merlin :with-dev-setup)
	(ocp-indent :with-dev-setup)
	(ocp-index :with-dev-setup))
 (tags
   ("string diagrams" "monoidal categories")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
