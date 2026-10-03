(lang dune 3.24)
(using menhir 3.0)

(name kingcrush)

(maintainers "seb&seb")
(authors "seb&seb")
(homepage "nohome")
(bug_reports "nobugs")

(generate_opam_files true)

(package
  (name kingcrush)
  (synopsis "Some")
  (description "some")

  (depends
    dune
    (ocaml (>= 4))
    tsdl
    tsdl-image
    tsdl-mixer
    tsdl-ttf  ))
