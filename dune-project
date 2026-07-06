(lang dune 3.21)
(name pidgin)
(version dev)
(generate_opam_files)

(source (github cargocut/pidgin))
(license BSD-3-Clause)

(authors
  "d-plaindoux <d.plaindoux@free.fr>"
  "xhtmlboi <xhtmlboi@gmail.com>"
  "xvw <xaviervdw@gmail.com>"
  "msp <spawnmick@gmail.com>"
  "grm <grimfw@gmail.com>")
(maintainers
  "d-plaindoux <d.plaindoux@free.fr>"
  "xhtmlboi <xhtmlboi@gmail.com>"
  "xvw <xaviervdw@gmail.com>"
  "msp <spawnmick@gmail.com>"
  "grm <grimfw@gmail.com>")

(package
 (name pidgin)
 (synopsis "A common language for describing and validating data structures")
 (description "Pidgin is a common language for dealing with serialization an deserialization of complex data structures")
 (depends
 (ocaml (>= 5.0.0))
 (nel (>= 1.0.0))

 ;; test setup
 (alcotest (and :with-test (>= 1.9.1)))

 ;; dev setup
 (utop :with-dev-setup)
 (ocamlformat :with-dev-setup)
 (ocp-indent :with-dev-setup)
 (merlin :with-dev-setup)
 (ocaml-lsp-server :with-dev-setup)))

(package
  (name pidgin_pp)
  (synopsis "Pretty-printers for the Pidgin Language")
  (description "Defined a set of pretty-printers to handle the different types described by the Pidgin library")
  (depends
    (ocaml (>= 5.0.0))
    (pidgin (= :version))))