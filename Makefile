all:
	jbuilder build

# profile:
# 	ocamlbuild -clean
# 	ocamlbuild -use-ocamlfind -cflags -annot -I permgroup tests.p.native


clean:
	jbuilder clean
	rm *.pdf *.mps *.log gmon.out
	./clean.sh
