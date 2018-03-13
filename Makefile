
OCBFLAGS = -I src
OCB = ocamlbuild -use-menhir -use-ocamlfind $(OCBFLAGS)

all: interp compil

interp:
	$(OCB) src/interp/main.byte
	mv main.byte interpretor
	
compil:
	$(OCB) src/compil/main.byte
	mv main.byte compiler

clean:
	$(OCB) -clean

clear:
	rm *.cmx *.cmi *.o *~ *.out


