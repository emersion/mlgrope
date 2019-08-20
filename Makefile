all:
	dune build @all
	cp _build/default/src/main.exe mlgrope

clean:
	dune clean

indent:
	ocamlformat -i src/*.ml

.PHONY: all clean
