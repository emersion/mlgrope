all:
	ocamlc -thread -o mlgrope graphics.cma unix.cma threads.cma mlgrope.mli backend.mli backend.ml frontend.ml mlgrope.ml
