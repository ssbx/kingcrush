#prefix = $(HOME)/.local
prefix = /opt/kingcrush

.PHONY: run build clean install uninstall

run: install
	$(prefix)/bin/kingcrush

build: data/puzzles.csv
	dune build

install: build uninstall
	dune install --prefix=$(prefix)

uninstall:
	dune uninstall --prefix=$(prefix)

clean:
	dune clean


# real targets
data/puzzles.csv: data/puzzles.csv.gz
	cd data && gzip -dk puzzles.csv.gz
