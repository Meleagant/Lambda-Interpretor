all:
	ocamlbuild -use-menhir src/main.byte
	mv main.byte interpretor
	ocamlbuild -use-menhir compiler/main.byte
	mv main.byte compiler

clean:
	rm interpretor
	rm compiler
	rm -rf *_build/

clear:
	rm *.cmx *.cmi *.o *~ *.out

