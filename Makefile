all:
	dune build @all
	cp _build/default/src/main.exe mlgrope

clean:
	dune clean

.PHONY: all clean
