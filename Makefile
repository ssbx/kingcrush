prefix = $(HOME)/.local
LOCAL_ENV=./.opam_env

.PHONY: run build clean format install uninstall rel-check desktop_entry

run: build
	. $(LOCAL_ENV) && dune exec -- kingcrush
	#dune exec -- kingcrush --disable-anims --disable-audio
	#dune exec -- kingcrush --generate-themes

build: $(LOCAL_ENV) data/puzzles.csv
	. $(LOCAL_ENV) && dune build

clean: $(LOCAL_ENV)
	. $(LOCAL_ENV) && dune clean

format: $(LOCAL_ENV)
	. $(LOCAL_ENV) && dune fmt

desktop_entry:
	rm -f kingcrush.desktop
	cat support/desktop.in | \
		sed 's|@APPBIN@|$(prefix)/bin/kingcrush|' | \
		sed 's|@APPICON@|$(prefix)/share/kingcrush/pieces/default/wK.png|' \
		> kingcrush.desktop

install: build desktop_entry
	. $(LOCAL_ENV) && dune install --verbose --release --prefix=$(prefix)
	mv kingcrush.desktop $(prefix)/share/applications/

uninstall: $(LOCAL_ENV)
	. $(LOCAL_ENV) && dune uninstall --verbose --release --prefix=$(prefix)

rel-check: $(LOCAL_ENV)
	. $(LOCAL_ENV) && dune-release check

# real targets
data/puzzles.csv: data/puzzles.csv.gz
	cd data && gzip -dk puzzles.csv.gz

$(LOCAL_ENV):
	opam env > $(LOCAL_ENV)


