OCAMLC ?= ocamlc
CFLAGS = -thread
CMAS = graphics.cma unix.cma threads.cma

mlgrope:
	$(OCAMLC) $(CFLAGS) -c mlgrope.mli backend.mli backend.ml frontend.ml mlgrope.ml
	$(OCAMLC) $(CFLAGS) -o $@ $(CMAS) mlgrope.cmo backend.cmo frontend.cmo

all: mlgrope

.PHONY: all
