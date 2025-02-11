OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib

SRCS=lib/bstr.ml lib/slice.ml lib/bin.ml bin/generate.ml
OBJS=$(SRCS:.ml=.cmo)
OPTOBJS=$(SRCS:.ml=.cmx)

OCAMLCFLAGS=-I lib -w "@1..3@5..28@30..39@43@46..47@49..57@61..62-40" \
	-strict-sequence -strict-formats -short-paths -keep-locs -g -bin-annot-occurrences \
	-no-alias-deps -opaque

CFLAGS=-Wcast-align

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma .cmxa

.ml.cmo:
	@echo "OCAMLC $<"
	@$(OCAMLC) $(OCAMLCFLAGS) -c $<

.mli.cmi:
	@echo "OCAMLC $<"
	@$(OCAMLC) $(OCAMLCFLAGS) -c $<

.ml.cmx:
	@echo "OCAMLOPT $<"
	@$(OCAMLOPT) $(OCAMLCFLAGS) -c $<

.cmo.cma:
	@echo "OCAMLC -a $<"
	@$(OCAMLC) -a $< $@

.c.o:
	@echo "CC $<"
	@$(OCAMLC) -ccopt "$(CFLAGS)" $< -o $@

.depend: $(SRCS)
	@echo "OCAMLDEP **/*.mli **/*.ml"
	@$(OCAMLDEP) **/*.mli **/*.ml > .depend
	@echo "lib/bstr.o: lib/bstr.c" >> .depend
	@echo "lib/bstr.cmxa: lib/bstr.cmx lib/bstr.o" >> .depend
	@echo "lib/bstr.cma: lib/bstr.cmo lib/bstr.o" >> .depend

include .depend

lib/dllbstr.so lib/libbstr.a lib/bstr.cmxa lib/bstr.cma: lib/bstr.mllib
	@echo "OCAMLMKLIB $^"
	@$(OCAMLMKLIB) -o lib/bstr -oc lib/bstr -args $<

.PHONY: clean
clean:
	rm -rf lib/*.cm{o,x,i,a,xa}
	rm -rf lib/*.{o,a}
