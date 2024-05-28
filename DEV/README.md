# ocaml chesspuzzles

Program to practice chess puzzles offline. Particular effort is made on energy
efficiency. It is developped on Linux, should compile on Window with proper
Ocaml installation.

(TODO: port to android)
## dependencies

    > `` apt install ocaml opam && eval $(opam env) ``

## install

By default installation prefix is **$HOME/.local**.

```console
$ git clone https://github.com/ssbx/chesspuzzles && cd chesspuzzles
$ opam install --deps-only ./chesspuzzle.opam
$ make install
```
## play

    > `` ~/.local/bin/chesspuzzles ``

Chesspuzzles uses [lichess.org](https://lichess.org) puzzles.
