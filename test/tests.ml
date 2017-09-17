open Graph
open Auto_dix
open Autotools

module G = Persistent.Graph.AbstractLabeled(Unit)(Unit)

module C =
  Canonical.Make
    (G)
    (Unit)
    (Unit)
    (struct include G.V let to_string x = string_of_int (hash x) end)
    (struct
      type t = G.edge
      let to_string (e : t) = Printf.sprintf "(%d,%d)" (G.V.hash (G.E.src e)) (G.V.hash (G.E.dst e))
    end)

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

let ints = [2;3;4;5;6]

let cmz  = "../../../graphs/pg/pg2-2.dre"

let results =
  [2;3;4;5;6]
  |> List.map (Printf.sprintf "../../../graphs/k/k-%d.dre")
  |> List.map (fun s -> (s, load s))
  |> List.map (fun (s, g) -> (s, C.compute g))

let _ =
  List.iter (fun (s, (autos, mini, wtf)) ->
      let order = C.Permgroup.order autos in
      Printf.printf "%s: %d automorphisms\n" s order
    ) results

let results =
  [ cmz ]
  |> List.map (fun s -> (s, load s))
  |> List.map (fun (s, g) -> (s, C.compute g))

let _ =
  List.iter (fun (s, (autos, mini, wtf)) ->
      let order = C.Permgroup.order autos in
      Printf.printf "%s: %d automorphisms\n" s order
    ) results

let _ = exit 0
