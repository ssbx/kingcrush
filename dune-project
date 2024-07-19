(lang dune 3.5)

(name kingcrush)
(version 1.0)

(maintainers "seb&seb")
(authors "seb&seb")
(homepage "nohome")
(bug_reports "nobugs")

(generate_opam_files true)

(formatting (enabled_for ocaml))

(package
  (name kingcrush)
  (synopsis "Some")
  (description "some")

  (depends
    dune
    ocaml
    (caml-sdl2  (= 1.0))
    (caml-sdl2-mixer (= 1.0))
    (caml-sdl2-image (= 1.0))
    (caml-sdl2-ttf   (= 1.0))
  )
)
