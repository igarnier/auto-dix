all:
	ocamlbuild -clean
	ocamlbuild -use-ocamlfind -cflags -annot -I permgroup tests.native

profile:
	ocamlbuild -clean
	ocamlbuild -use-ocamlfind -cflags -annot -I permgroup tests.p.native


clean:
	ocamlbuild -clean
	rm *.pdf *.mps *.log gmon.out
	./clean.sh
