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

.PHONY: default run build clean install uninstall gen_themes install-deps

default: run

all: install-deps build data/puzzles.csv

install-deps:
	opam install --deps-only ./kingcrush.opam

run:
	dune exec -- $(APPNAME) --with-datadir=$(root)/data

gen_themes:
	dune exec -- $(APPNAME) --with-datadir=$(root)/data --generate-themes-in=$(root)/data

build: data/puzzles.csv
	dune build

install: build
	opam install ./kingcrush.opam

uninstall:
	opam uninstall kingcrush

clean:
	dune clean

# real targets
data/puzzles.csv: data/puzzles.csv.gz
	cd data && gzip -dk puzzles.csv.gz
