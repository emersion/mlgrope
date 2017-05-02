all:
	ocamlc -thread -c mlgrope.mli backend.mli backend.ml frontend.ml mlgrope.ml
	ocamlc -thread -o mlgrope graphics.cma unix.cma threads.cma mlgrope.cmo backend.cmo frontend.cmo
