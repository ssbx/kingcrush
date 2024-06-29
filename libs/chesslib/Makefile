.PHONY: build install clean uninstall update

build:
	dune build

install:
	opam install ./chesslib.opam

uninstall:
	opam remove chesslib

update:
	if [ -n $(GAMEDEVDIR) ]; then cp -vr $(GAMEDEVDIR)/libs/chesslib/* . ; fi

clean:
	dune clean
