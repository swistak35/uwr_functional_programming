TARGET = wlang
OBJECTS = wlexer.cmo wparser.cmo wast.cmo printer.cmo lint.cmo interpreter.cmo main.cmo
CAMLYACC_OPTS=-v
OCAMLC_OPTS=-g

wlang:	$(OBJECTS)
		ocamlc $(OCAMLC_OPTS) -o $@ $(OBJECTS)

clean:
		rm -rf *.cmi *.cmo $(TARGET)
		rm -rf wparser.ml wparser.mli wlexer.ml wlexer.mli wparser.output

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

wparser.ml:		wparser.mly
				ocamlyacc $(CAMLYACC_OPTS) wparser.mly

wparser.mli:	wparser.mly
				ocamlyacc $(CAMLYACC_OPTS) wparser.mly

wlexer.ml:		wlexer.mll
				ocamllex wlexer.mll

wast.cmo:		wast.ml
				ocamlc $(OCAMLC_OPTS) -c wast.ml

wparser.cmi:	wparser.mli
				ocamlc $(OCAMLC_OPTS) -c wparser.mli

wparser.cmo:	wparser.ml wast.cmo wparser.cmi
				ocamlc $(OCAMLC_OPTS) -c wparser.ml

wlexer.cmo:		wlexer.ml wparser.cmo
				ocamlc $(OCAMLC_OPTS) -c wlexer.ml

interpreter.cmo:interpreter.ml wast.cmo
				ocamlc $(OCAMLC_OPTS) -c interpreter.ml

compiler.cmo:	compiler.ml wast.cmo
				ocamlc $(OCAMLC_OPTS) -c compiler.ml

printer.cmo:	printer.ml wast.cmo
				ocamlc $(OCAMLC_OPTS) -c printer.ml

lint.cmo:		lint.ml wast.cmo
				ocamlc $(OCAMLC_OPTS) -c lint.ml

main.cmo:		main.ml interpreter.cmo
				ocamlc $(OCAMLC_OPTS) -c main.ml