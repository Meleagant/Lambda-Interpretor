
OCBFLAGS = -I src/interp -I src/compil
OCB = ocamlbuild -use-menhir -use-ocamlfind $(OCBFLAGS)

all: interp compil

interp:
	$(OCB) src/interp/interp.native
	mv interp.native interpretor

interp_doc: interp
	$(OCB) doc/interp.docdir/index.html

compil:
	$(OCB) src/compil/main.native
	mv main.native compiler

tests:
	sh test.sh

clean:
	$(OCB) -clean
	rm -rf *~ ./*/*~

realclear:
	rm -f *.cmx *.cmi *.o *~ *.out
