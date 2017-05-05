OCAMLC ?= ocamlc
CFLAGS = -thread
CMAS = graphics.cma unix.cma threads.cma
CMIS = mlgrope.cmi backend.cmi frontend.cmi level.cmi
CMOS = mlgrope.cmo backend.cmo frontend.cmo level.cmo main.cmo

all: mlgrope
clean:
	rm -rf mlgrope *.cmo *.cmi
.PHONY: all clean

%.cmi: %.mli
	$(OCAMLC) $(CFLAGS) -c $<
%.cmo: %.ml $(CMIS)
	$(OCAMLC) $(CFLAGS) -c $<
mlgrope: $(CMOS)
	$(OCAMLC) $(CFLAGS) -o $@ $(CMAS) $^
