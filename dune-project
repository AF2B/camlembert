(lang dune 3.0)

(name camlembert)

(generate_opam_files true)

(package
 (name camlembert)
 (synopsis "Terminal user interfaces for OCaml, ported from Rust's Ratatui")
 (description
  "Camlembert is an immediate-mode terminal UI library. Widgets draw into a cell buffer, the buffer is diffed against the previous frame, and only the changed cells are sent to a backend.")
 (maintainers "André Borba <andre.borbaaf2b@gmail.com>")
 (authors "André Borba <andre.borbaaf2b@gmail.com>")
 (license MIT)
 (source
  (github AF2B/camlembert))
 (tags
  (tui terminal ui ratatui))
 (depends
  (ocaml
   (>= 5.1))
  uucp
  uuseg
  (alcotest
   (and
    :with-test
    (>= 1.6)))
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))
