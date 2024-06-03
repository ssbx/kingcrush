prefix = $(HOME)/.local

.PHONY: run build clean format install uninstall rel-check desktop_entry

run: build
	dune exec -- kingcrush
	#dune exec -- kingcrush --disable-anims --disable-audio
	#dune exec -- kingcrush --generate-themes

build: data/puzzles.csv
	dune build

clean:
	dune clean

format:
	dune fmt

desktop_entry:
	rm -f kingcrush.desktop
	cat support/desktop.in | \
		sed 's|@APPBIN@|$(prefix)/bin/kingcrush|' | \
		sed 's|@APPICON@|$(prefix)/share/kingcrush/pieces/default/wK.png|' \
		> kingcrush.desktop

install: build desktop_entry
	dune install --verbose --release --prefix=$(prefix)
	mv kingcrush.desktop $(prefix)/share/applications/

uninstall:
	dune uninstall --verbose --release --prefix=$(prefix)

rel-check:
	dune-release check

# real targets
data/puzzles.csv: data/puzzles.csv.gz
	cd data && gzip -dk puzzles.csv.gz

