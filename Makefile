all:
	dune build @all
	cp _build/default/main.exe mlgrope

clean:
	dune clean

.PHONY: all clean
