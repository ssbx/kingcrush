(lang dune 3.5)
(using dune_site 0.1)

(name chesspuzzles)

(generate_opam_files false)
(license GPL-3.0-or-later)
(authors "Seb https://github.com/ssbx")
(maintainers "Seb https://github.com/ssbx")
(source (uri git+https://github.com/ssbx/chesspuzzles.git))
(bug_reports https://github.com/ssbx/chesspuzzles)
(homepage https://github.com/ssbx/chesspuzzles)

(package
  (name chesspuzzles)
  (synopsis "I am a chess puzzle solving game")
  (sites
    (share pieces)
    (share sounds)
    (share data)
    (share fonts)
    (share images)
    )
  (description
    "Offline chess puzzle solving game using SDL, with particular focus on
    energy efficiency. Puzzles comes from the lichess.org puzzle database")
    (depends
      (ocaml (>= 4.08))
      (tsdl  (>= 1.0.0))
      (tsdl-image (>= 0.6))
      (tsdl-mixer (>= 0.6))
      (tsdl-ttf   (>= 0.6))
      (dune-site (>= 3.15.2))
      )
    (tags (game board chess puzzle lichess sdl)))
