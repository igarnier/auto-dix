(* -----------------------------
   Test automorphism detection *)

module Auto = Auto.Make
  (struct type t = unit
          let compare _ _ = 0
          let print _ = "" 
          let inhabited = ()
   end)
  (struct type t = unit
          let compare _ _ = 0
          let print _ = ""
          let inhabited = ()
   end)


let size  = int_of_string Sys.argv.(1)
let edgep = float_of_string Sys.argv.(2)
let runs  = int_of_string Sys.argv.(3)

let rec test i =
  if i = runs then ()
  else
    let graph = Graph.erdos_renyi size edgep (fun _ -> ()) (fun _ -> ()) in
    let autos = Auto.compute_automorphisms graph in
    test (i + 1)

let _ = test 0
  

(* let g = Graph.complete_simple_graph 6 *)

(* let _ = Graph.to_dot "k6" "k6" g (fun _ _ -> "") (fun _ -> "") *)

(* let autos = Auto.compute_automorphisms g *)

(* let rec closure perms = *)
(*   let cl = *)
(*     List.fold_left (fun acc p -> *)
(*       List.fold_left (fun acc p' -> *)
(*         let a = Perm.ArrayBased.prod p p' in *)
(*         let b = Perm.ArrayBased.prod p (Perm.ArrayBased.inv p') in *)
(*         let c = Perm.ArrayBased.prod (Perm.ArrayBased.inv p) p' in *)
(*         let d = Perm.ArrayBased.prod (Perm.ArrayBased.inv p) (Perm.ArrayBased.inv p') in *)
(*         Prelude.union [a; b; c; d] acc *)
(*       ) perms acc *)
(*     ) perms perms *)
(*   in *)
(*   if cl = (List.sort compare perms) then *)
(*     cl *)
(*   else *)
(*     closure cl *)

(* let _ = Printf.printf "generator count: %d\n%!" (List.length autos)     *)

(* let autos = closure autos *)

(* let _ = Printf.printf "automorphism count: %d\n" (List.length autos) *)

(* let graph =  *)
(*   let g = Graph.empty in *)
(*   let g = Graph.add_node_with_colour g 1 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_node_with_colour g 0 in *)
(*   let g = Graph.add_edge g 0 "" 1 in *)
(*   let g = Graph.add_edge g 1 "" 2 in *)
(*   let g = Graph.add_edge g 2 "" 3 in *)
(*   Graph.add_edge g 3 "" 0 *)

(* let automorphisms = Auto.compute_automorphisms graph *)

(* let _ = List.iter (fun x -> Printf.printf "%s\n" (Perm.ArrayBased.print x)) (Prelude.filter_duplicates automorphisms) *)

(* let _ =   *)
(*   Printf.printf "number of automorphism checks: %s\n" (Int64.to_string !Auto.auto_count) *)

(* let _ =   *)
(*   Printf.printf "cumultative time spent in automorphism computation: %f seconds\n" (!Auto.cmlt) *)

(*
let _ =
  Perm.ArrayBased.size := 3

module PermTest = Bsgs.Make2(Perm.ArrayBased)

open PermTest


let generators = 
  [ [| 1; 0; 2 |];
    [| 0; 2; 1 |]
      
  ]

let partial_bsgs = compute_partial_subgroup_chain generators

let _ = Prelude.log (print partial_bsgs)
  
let _ = schreier_sims_aux partial_bsgs.chain 0
let _ = Prelude.log "----------"
let _ = schreier_sims_aux partial_bsgs.chain 1

let _ =
  let g = (Perm.of_concrete [| 1; 0; 2 |]) in
  match strip partial_bsgs.chain 1 g with
  | Ok w ->
    let w = Prelude.to_sseq Perm.print "." w in 
    Prelude.log (Printf.sprintf "** for gen %s, decomposition %s" (Perm.print g) w)
  | DropOut(i, residue) ->
    Prelude.log (Printf.sprintf "** for gen %s, residue %s at %d" (Perm.print g) (Perm.print residue) i)

(*let _ = schreier_sims_aux partial_bsgs.chain 0*)

let _ = Prelude.log (print partial_bsgs)
*)
