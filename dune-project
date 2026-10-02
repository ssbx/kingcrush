(lang dune 3.24)

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
    (dune (and :build (= 3.24)))
    (ocaml (= 4.14))
    tsdl
    tsdl-image
    tsdl-mixer
    tsdl-ttf  ))
