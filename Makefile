main:
	ocamlbuild -clean
	ocamlbuild -use-ocamlfind -pkg unix,graphics,cairo -cflags -annot -I vlayout -I models main.native

simplest:
	ocamlbuild -clean
	ocamlbuild  -use-ocamlfind -pkg unix,graphics,cairo -cflags -annot simplest.native

test-reduction:
	ocamlbuild -clean
	ocamlbuild  -use-ocamlfind -pkg unix,graphics,cairo -cflags -annot testreduction.d.byte

test-hull:
	ocamlbuild -clean
	ocamlbuild  -use-ocamlfind -pkg unix,graphics,cairo -cflags -annot testconvexhull.d.byte

clean:
	ocamlbuild -clean
	rm *.pdf *.mps *.log
	./clean.sh
