OCAMLC ?= ocamlc
CFLAGS = -thread
CMAS = graphics.cma unix.cma threads.cma
MODULES = util image math2d collide mlgrope backend frontend level player editor menu
CMIS = $(MODULES:=.cmi)
CMOS = $(MODULES:=.cmo) main.cmo

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
