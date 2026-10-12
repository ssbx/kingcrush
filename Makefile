datadir = $(shell pwd)/data

.PHONY: all
all: run

.PHONY: run
run:
	dune exec -- kingcrush --with-datadir=$(datadir)

.PHONY: run2
run2:
	dune exec ucitest

.PHONY: gen_themes
gen_themes:
	dune exec -- kingcrush --with-datadir=$(datadir) --generate-themes-in=$(datadir)

.PHONY: install
install: build
	opam install ./kingcrush.opam

.PHONY: uninstall
uninstall:
	opam uninstall kingcrush

.PHONY: clean
clean:
	dune clean

.PHONY: dist-clean
dist-clean: clean
	rm -rf _build _opam

.PHONY: init init_sub
init_sub:
	git submodule update --init
init: _opam init_sub data data/puzzles.csv

_opam:
	dune build --auto-promote @opam
	opam switch create --deps-only ./ 5.5.1

data/puzzles.csv: data/puzzles.csv.gz
	cd data && gzip -dk puzzles.csv.gz

