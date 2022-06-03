(lang dune 3.1)

(using coq 0.3)

(name three_coloring_problem)

(generate_opam_files true)

(source
 (github bot-mne-v-rot/3-coloring-problem))

(authors "Guys with big dicks")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://github.com/bot-mne-v-rot/3-coloring-problem/)

(implicit_transitive_deps true)

(package
  (name three_coloring_problem)
  (synopsis "3-coloring problem")
  (description "Solving 3-coloring problem with SAT-solvers and proving correctness in Coq")
  (depends 
    (dune (>= 2.0))
    (ocaml (and (>= 4.13.0) (< 4.14.0)))
    (ocamlformat :with-test)
    js_of_ocaml
    js_of_ocaml-compiler
    js_of_ocaml-lwt
    js_of_ocaml-tyxml
    js_of_ocaml-ppx
    ppx_compare
    ppx_let
    ppx_sexp_conv
    ppx_string_interpolation
    coq-of-ocaml
    coq
  ))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
