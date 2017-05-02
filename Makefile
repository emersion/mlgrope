all:
	ocamlc -o mlgrope graphics.cma mlgrope.mli backend.ml frontend.ml mlgrope.ml
