module G = Graph.Make(Autotools.Unit)(Autotools.Unit)

module C = Canonical.Make(G)

(* load a (uncolored, simple) graph in DREADNAUT format *)

let vertex () =
  let table = Hashtbl.create 101 in
  fun (v : int) ->
  if Hashtbl.mem table v then
    Hashtbl.find table v
  else
    let vertex = G.V.create () in
    Hashtbl.add table v vertex;
    vertex

let scan_dotted_integer s =
  try Scanf.sscanf s "%d." (fun x -> x)
  with _ ->
    Scanf.sscanf s "%d" (fun x -> x)
                         
let load filename =
  let open Printf in
  let fd     = open_in filename in
  let vertex = vertex () in
  let graph  = ref G.empty in
  let linen  = ref 0 in
  begin
    try
      while true do
        incr linen;
        let line = input_line fd in
        if String.length line = 0 then ()
        else if line.[0] = '$' then ()
        else
          (let fields = Autotools.ExtString.nsplit line " " in
           match fields with
           | [] ->
              failwith (sprintf "When parsing %s: error, empty line" filename)
           | node :: neighbours ->
              let node = Scanf.sscanf node "%d:" vertex in
              List.iter (fun str ->
                         let neighbour = vertex (scan_dotted_integer str) in
                         graph := (G.add_edge_e !graph (G.E.create node () neighbour))
                        ) neighbours)
      done
    with
    | End_of_file -> close_in fd
    | exn ->
       (printf "At line %d, when parsing %s: error\n" !linen filename;
        raise exn)
  end;
  !graph
                         
(* let v = Array.init 9 (fun i -> G.V.create ()) *)
                         
(* let graph = *)
(*   let edges = *)
(*     [ v.(0), v.(1); *)
(*       v.(1), v.(2); *)
(*       v.(0), v.(3); *)
(*       v.(1), v.(4); *)
(*       v.(2), v.(5); *)
(*       v.(3), v.(4); *)
(*       v.(4), v.(5); *)
(*       v.(3), v.(6); *)
(*       v.(4), v.(7); *)
(*       v.(5), v.(8); *)
(*       v.(6), v.(7); *)
(*       v.(7), v.(8) ] *)
(*   in *)
(*   List.fold_left (fun graph (v, v') -> *)
(*                   G.add_edge_e graph (G.E.create v () v') *)
(*                  ) G.empty edges *)

let graph = load Sys.argv.(1)

let (autos, mini) = C.compute graph

let _ = Printf.printf "%d automorphisms\n" (C.SchreierSims.order autos)
                 
let _ = exit 1
                 
(* let _ = Printf.printf "------------------\n" *)

(* let list = [ v.(0); v.(1); v.(2); v.(3); v.(4) ] *)

(* let partition = [ { C.list = list; size = 5 } ] *)
(*   (\* [ *\) *)
(*   (\*   [ v.(0) ]; *\) *)
(*   (\*   [ v.(2); v.(6); v.(8) ]; *\) *)
(*   (\*   [ v.(1); v.(3); v.(5); v.(7) ]; *\) *)
(*   (\*   [ v.(4) ] *\) *)
(*   (\* ] *\) *)

(* let _ = *)
(*   List.iter (fun cell -> *)
(*              List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell.C.list; *)
(*              Printf.printf "\n"; *)
(*             ) partition *)

(* let _ = Printf.printf "==================\n"             *)
            
    
(* let partition = *)
(*   match C.refine_partition graph partition *)
(*   with *)
(*   | None -> (Printf.printf "done.\n"; exit 1) *)
(*   | Some partition -> partition *)

(* let _ = *)
(*   List.iter (fun cell -> *)
(*              List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell.C.list; *)
(*              Printf.printf "\n"; *)
(*             ) partition *)

(* let _ = Printf.printf "==================\n"             *)
            
(* let partition = *)
(*   match C.refine_partition graph partition *)
(*   with *)
(*   | None -> (Printf.printf "done.\n"; exit 1) *)
(*   | Some partition -> partition *)

(* let _ = *)
(*   List.iter (fun cell -> *)
(*              List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell.C.list; *)
(*              Printf.printf "\n"; *)
(*             ) partition *)
            

(* let _ = Printf.printf "==================\n"             *)
            
(* let partition = *)
(*   match C.refine_partition graph partition *)
(*   with *)
(*   | None -> (Printf.printf "done.\n"; exit 1) *)
(*   | Some partition -> partition *)

(* let _ = *)
(*   List.iter (fun cell -> *)
(*              List.iter (fun v -> Printf.printf "%s," (G.V.to_string v)) cell.C.list; *)
(*              Printf.printf "\n"; *)
(*             ) partition *)
            
