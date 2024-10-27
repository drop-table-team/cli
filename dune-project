(lang dune 3.15)

(name hackathon_cli)

(generate_opam_files true)

(source
 (github drop-table-team/cli))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name hackathon_cli)
 (synopsis "CLI to directly interface with API")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (cli)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
