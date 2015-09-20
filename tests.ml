module G = Graph.Make(Aux.Unit)(Aux.Unit)

module C = Canonical.Make(G)

let v = Array.init 9 (fun i -> G.V.create ())
                         
let graph =
  let edges =
    [ v.(0), v.(1);
      v.(1), v.(2);
      v.(0), v.(3);
      v.(1), v.(4);
      v.(2), v.(5);
      v.(3), v.(4);
      v.(4), v.(5);
      v.(3), v.(6);
      v.(4), v.(7);
      v.(5), v.(8);
      v.(6), v.(7);
      v.(7), v.(8) ]
  in
  List.fold_left (fun graph (v, v') ->
                  G.add_edge_e graph (G.E.create v () v')
                 ) G.empty edges

(* let partition = C.initial_partition graph *)

(* let _ = *)
(*   List.iter (fun cell -> *)
(*              List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell; *)
(*              Printf.printf "\n"; *)
(*             ) partition *)

let _ = Printf.printf "------------------\n"
            
let partition = [ { C.list = Array.to_list v; size = 9 } ]
  (* [ *)
  (*   [ v.(0) ]; *)
  (*   [ v.(2); v.(6); v.(8) ]; *)
  (*   [ v.(1); v.(3); v.(5); v.(7) ]; *)
  (*   [ v.(4) ] *)
  (* ] *)

let _ =
  List.iter (fun cell ->
             List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell.C.list;
             Printf.printf "\n";
            ) partition

let _ = Printf.printf "==================\n"            
            
    
let partition =
  match C.refine_partition graph partition
  with
  | None -> (Printf.printf "done.\n"; exit 1)
  | Some partition -> partition

let _ =
  List.iter (fun cell ->
             List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell.C.list;
             Printf.printf "\n";
            ) partition

let _ = Printf.printf "==================\n"            
            
let partition =
  match C.refine_partition graph partition
  with
  | None -> (Printf.printf "done.\n"; exit 1)
  | Some partition -> partition

let _ =
  List.iter (fun cell ->
             List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell.C.list;
             Printf.printf "\n";
            ) partition
            

let _ = Printf.printf "==================\n"            
            
let partition =
  match C.refine_partition graph partition
  with
  | None -> (Printf.printf "done.\n"; exit 1)
  | Some partition -> partition

let _ =
  List.iter (fun cell ->
             List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell.C.list;
             Printf.printf "\n";
            ) partition
            
