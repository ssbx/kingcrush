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

.PHONY: default run build clean install uninstall gen_themes run_uci_test dev_init fmt

default: run

run: build
	dune exec -- $(APPNAME) --with-datadir=$(root)/_build/default/data

run_uci_test:
	dune exec -- $(APPNAME) --test-uci

gen_themes:
	dune exec -- $(APPNAME) --with-datadir=$(root)/data --generate-themes-in=$(root)/data

build:
	dune build

install: clean build
	dune install $(DUNE_ARGS)

uninstall:
	dune uninstall $(DUNE_ARGS)

release: install
	tar -C _build -cz -f $(APPNAME).$(VERSION).tgz $(APPNAME)

clean:
	rm -f $(APPNAME).$(VERSION).tgz
	dune clean

fmt:
	dune build @fmt

kingcrush.opam: dune-project
	dune build kingcrush.opam

dev_install:
	opam install -v --working-dir ./kingcrush.opam

dev_init: kingcrush.opam

