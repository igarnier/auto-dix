#ocamlc -c -g -dtypes float3.ml mat3.ml springmodel.ml utils.ml
#ocamlc -c graph.mli
#ocamlc -g -dtypes -I /home/piotr/.opam/4.00.1/lib/ocaml graphics.cma float3.ml mat3.ml springmodel.ml utils.ml graph.ml generation.ml display.ml smiles.ml cycles.ml main.ml -o genmol

ocamlopt  prelude.mli group.mli perm.mli bsgs.mli graph.mli auto.mli # main.ml

ocamlopt -inline 50 -o main -annot unix.cmxa prelude.ml group.ml perm.ml bsgs.ml graph.ml auto.ml  main.ml
