
OCBFLAGS = -I src
OCB = ocamlbuild -use-menhir -use-ocamlfind $(OCBFLAGS)

all: interp # compil

interp:
	$(OCB) src/interp/main.native
	mv main.native interpretor

compil:
	$(OCB) src/compil/main.native
	mv main.native compiler

clean:
	$(OCB) -clean

realclear:
	rm -f *.cmx *.cmi *.o *~ *.out
