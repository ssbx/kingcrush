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

.PHONY: default run build clean install uninstall gen_themes run_uci_test

default: run

run:
	dune exec -- $(APPNAME) --with-datadir=$(root)/data

run_uci_test:
	dune exec -- $(APPNAME) --test-uci

gen_themes:
	dune exec -- $(APPNAME) --with-datadir=$(root)/data --generate-themes-in=$(root)/data

build: data/puzzles.csv
	dune build

install: build
	rm -rf $(destdir)
	dune install $(DUNE_ARGS)

uninstall:
	dune uninstall $(DUNE_ARGS)

release: install
	tar -C _build -cz -f $(APPNAME).$(VERSION).tgz $(APPNAME)


clean:
	rm -f $(APPNAME).$(VERSION).tgz
	dune clean

# real targets
data/puzzles.csv: data/puzzles.csv.gz
	cd data && gzip -dk puzzles.csv.gz
