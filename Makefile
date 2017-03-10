all:
	ocamlbuild -use-menhir src/main.byte
	mv main.byte interpretor

clean:
	rm interpretor
	rm -rf *_build/

clear:
	rm *.cmx *.cmi *.o *~ *.out

