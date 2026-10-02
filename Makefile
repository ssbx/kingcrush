prefix  = /
bindir  = $(prefix)
datadir = $(prefix)
root    = $(shell pwd)

APPNAME	  = kingcrush
VERSION   = 0.1
DUNE_ARGS = --prefix=$(prefix) --bindir=$(bindir) \
	    --datadir=$(datadir) --destdir=$(destdir) \
	    --profile=release

destdir = _build/$(APPNAME)

.PHONY: all
all: run

.PHONY: run
run:
	dune exec -- $(APPNAME) --with-datadir=$(root)/data

.PHONY: gen_themes
gen_themes:
	dune exec -- $(APPNAME) --with-datadir=$(root)/data --generate-themes-in=$(root)/data

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
	opam switch create --deps-only ./ 4.14.4

data/puzzles.csv: data/puzzles.csv.gz
	cd data && gzip -dk puzzles.csv.gz

