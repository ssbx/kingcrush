(lang dune 3.5)
(using dune_site 0.1)

(name kingcrush)

(generate_opam_files false)

(package
  (name kingcrush)
  (sites
    (share pieces)
    (share sounds)
    (share data)
    (share fonts)
    (share images))
  (depends
    (ocaml (>= 4.08))
    (tsdl  (>= 1.0.0))
    (tsdl-image (>= 0.6))
    (tsdl-mixer (>= 0.6))
    (tsdl-ttf   (>= 0.6))
    (dune-site (>= 3.15.2))))
