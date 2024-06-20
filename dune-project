(lang dune 3.5)
(using dune_site 0.1)

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
  (sites
    (share pieces)
    (share sounds)
    (share data)
    (share fonts)
    (share images))

  (depends
    (dune (and :build (= 3.5.0)))
    (cppo (and :build (= 1.6.9)))
    (ocaml (= 4.14.2))
    (tsdl  (= 0.9.9))
    (tsdl-image (= 0.3.2))
    (tsdl-mixer (= 0.3.2))
    (tsdl-ttf   (= 0.3.2))
    (dune-site (= 3.5.0))))
