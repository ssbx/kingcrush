prefix = $(HOME)/.local

.PHONY: run build clean format install uninstall rel-check desktop_entry

run: build
	dune exec -- chesspuzzles
	#dune exec -- chesspuzzles --disable-anims --disable-audio
	#dune exec -- chesspuzzles --generate-themes

build: chesspuzzles.opam assets/puzzles.csv
	dune build

clean:
	rm -f chesspuzzles.desktop
	dune clean

format:
	dune fmt

desktop_entry:
	rm -f chesspuzzles.desktop
	cat desktop.in | \
		sed 's|@APPBIN@|$(prefix)/bin/chesspuzzles|' | \
		sed 's|@APPICON@|$(prefix)/share/chesspuzzles/pieces/default/wK.png|' \
		> chesspuzzles.desktop

install: build desktop_entry
	dune install --verbose --release --prefix=$(prefix)
	mv chesspuzzles.desktop $(prefix)/share/applications/

uninstall:
	dune uninstall --verbose --release --prefix=$(prefix)

rel-check:
	dune-release check

# real targets
assets/puzzles.csv: assets/puzzles.csv.gz
	cd assets && gzip -dk puzzles.csv.gz

chesspuzzles.opam: dune-project
	rm -f chesspuzzles.opam
	dune build chesspuzzles.opam
