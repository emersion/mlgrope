all:
	ocamlc -o mlgrope graphics.cma unix.cma mlgrope.mli backend.ml frontend.ml mlgrope.ml
