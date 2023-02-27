(lang dune 3.6)

(name embedded_ocaml_templates)

(using menhir 2.1)

(generate_opam_files true)

(maintainers "Emile Trotignon <emile.trotignon@gmail.com>")

(authors "Emile Trotignon <emile.trotignon@gmail.com>")

(source
 (github EmileTrotignon/embedded_ocaml_templates))

(package
 (name embedded_ocaml_templates)
 (license MIT)
 (synopsis
  "EML is a simple templating language that lets you generate text with plain OCaml")
 (description
  "EML is a simple templating language that lets you generate text with plain OCaml")
 (depends
  (ocaml
   (>= 4.08.0))
  (dune
   (>= 2.8.0))
  (sedlex
   (>= 2.0))
  uutf
  pprint
  (ppxlib
   (>= 0.8.0))
  containers
  ppx_inline_test))
