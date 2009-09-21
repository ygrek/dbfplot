
.PHONY: build clean

build:
		ocamlbuild dbfplot.byte

clean:
		ocamlbuild -clean
