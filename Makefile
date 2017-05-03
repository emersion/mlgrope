OCAMLC ?= ocamlc
CFLAGS = -thread
CMAS = graphics.cma unix.cma threads.cma
CMIS = mlgrope.cmi backend.cmi frontend.cmi
CMOS = mlgrope.cmo backend.cmo frontend.cmo main.cmo

all: mlgrope
.PHONY: all

%.cmi: %.mli
	$(OCAMLC) $(CFLAGS) -c $<
%.cmo: %.ml $(CMIS)
	$(OCAMLC) $(CFLAGS) -c $<
mlgrope: $(CMOS)
	$(OCAMLC) $(CFLAGS) -o $@ $(CMAS) $^
