(lang dune 3.2)

(name tree_diff)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tree_diff)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune batteries sexplib ppx_sexp_conv)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
