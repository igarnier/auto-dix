open Autotools
(* (g : G.t) are labelled graph with vertex labels in VL and edge labels in EL.
   In particular, it is a *mistake* to identify vertex labels and vertices:
   for unlabelled graphs, use unit labels (otherwise, you will get only one
   automorphism ...) *)

module type Lab = sig
  include Printable
  include Ordered with type t := t
end

module Make
    (G : Graph.Sig.P)
    (EdgeLab : Lab with type t = G.E.label)
    (VertLab : Lab with type t = G.V.label)
    (PrintVert : Printable with type t = G.V.t)
    (PrintEdge : Printable with type t = G.E.t)    =
struct

  (* set of vertex labels *)
  module VLabelSet      = Set.Make(VertLab)
  (* multiset of edge labels *)
  module ELabelMSet     = Autotools.Multiset(EdgeLab)
  (* multiset of multisets of edge labels *)
  module ELabelMSetMSet = Autotools.Multiset(ELabelMSet)

  (* [vertex_vertex_sig graph v v'] returns the multisets of labels of 
   * resp. edges from v to v' and edges from v' to v *)
  let vertex_vertex_sig : G.t -> G.vertex -> G.vertex -> ELabelMSet.t =
    fun graph v v' ->
      let edges = G.find_all_edges graph v v' in
      List.fold_left
        (fun set e ->
           ELabelMSet.add (G.E.label e) set
        ) ELabelMSet.empty edges

  exception FastExit of int

  (* N.B.: we only compare isomorphic variants on the same vertex set *)
  let graph_compare g1 g2 =
    try
      G.fold_vertex
        (fun v1 ->
           G.fold_vertex
             (fun v2 _ ->
                let sig1 = vertex_vertex_sig g1 v1 v2 and
                sig2 = vertex_vertex_sig g2 v1 v2 in
                let c = ELabelMSet.compare sig1 sig2 in
                if c <> 0 then
                  raise (FastExit c)
                else
                  c
             ) g2
        ) g1 0
    with
      FastExit c -> c

  module Perm          = Permgroup.Perm.CycleBased(struct include G.V let to_string x = string_of_int (hash x) end)
  module Permgroup     = Permgroup.Group.Make(Perm)
  module GraphMap      = Map.Make(struct include G let compare = graph_compare end)

  type cell = { list : G.vertex list;
                size : int }

  type partition = cell list

  let _ = assert (not G.is_directed)

  let fold_cell ~f ~cell ~acc = List.fold_left f acc cell.list

  let initial_partition (graph : G.t) : partition =
    (* Split vertices by their labels *)
    let tbl    = Hashtbl.create 51 in
    let vertlist, len = G.fold_vertex (fun v (acc, len) -> (v :: acc, len + 1)) graph ([], 0) in
    if len = 0 then
      failwith (Printf.sprintf "%s, %d: empty graph\n" __MODULE__ __LINE__)
    else
      let labels =
        List.fold_left
          (fun set v ->
             let label = G.V.label v in
             let ()    = Hashtbl.add tbl label v in
             VLabelSet.add label set
          ) VLabelSet.empty vertlist
      in
      let cells =
        VLabelSet.fold
          (fun label acc ->
             (* NB: label is given by fold in increasing order *)
             let vertices = Hashtbl.find_all tbl label in
             { list = vertices; size = (List.length vertices) } :: acc
          ) labels []
      in
      List.rev cells

  let vertex_cell_sig : G.t -> G.vertex -> cell -> ELabelMSetMSet.t =
    fun graph v cell ->
      fold_cell
        ~cell
        ~acc:ELabelMSetMSet.empty
        ~f:(fun setmset v' ->
            ELabelMSetMSet.add (vertex_vertex_sig graph v v') setmset
          )

  let cell_shatters_cell ~graph ~target_cell ~splitter_cell =
    (* TODO optim: exception to exit *)
    let outcome =
      fold_cell
        ~cell:target_cell
        ~acc:`Empty
        ~f:(fun acc v ->
            match acc with
            | `Shatters -> acc
            | `Empty    ->
              let signature = vertex_cell_sig graph v splitter_cell in
              `Witness signature
            | `Witness sig' ->
              let signature = vertex_cell_sig graph v splitter_cell in
              let c = ELabelMSetMSet.compare signature sig' in
              if c = 0 then
                acc
              else
                `Shatters
          )
    in
    match outcome with
    | `Empty ->
      failwith (Printf.sprintf "Error at %s %d\n%!" __MODULE__ __LINE__)
    | `Shatters  -> true
    | `Witness _ -> false

  let exists_cell_shattering_cell  ~graph ~partition ~target_cell =
    try
      let outcome =
        List.find
          (fun splitter_cell ->
             cell_shatters_cell ~graph ~target_cell ~splitter_cell
          ) partition
      in
      Some outcome
    with Not_found -> None

  let rec insert_into_bucket elt signature bucket_list =
    match bucket_list with
    | []           -> (signature, 1, [elt]) :: []
    | (si, size, bucket as hd) :: tail ->
      let c = ELabelMSetMSet.compare signature si in
      if c < 0 then
        (signature, 1, [elt]) :: bucket_list
      else if c = 0 then
        (si, size + 1, elt :: bucket) :: tail
      else
        hd :: (insert_into_bucket elt signature tail)

  let perform_cell_split ~graph ~target_cell ~splitter_cell ~context =
    (* let _ = *)
    (*   Printf.printf *)
    (*     "performing cell split with target cell = %s and splitter cell = %s\n" *)
    (*     (Autotools.to_sseq G.V.to_string "," target_cell.list) *)
    (*     (Autotools.to_sseq G.V.to_string "," splitter_cell.list) *)
    (* in *)
    let buckets =
      fold_cell
        ~cell:target_cell
        ~acc:[]
        ~f:(fun acc v ->
            let signature = vertex_cell_sig graph v splitter_cell in
            insert_into_bucket v signature acc
          )
    in
    let cells = List.map (fun (_, size, list) -> { list; size }) buckets in
    let (prefix, suffix) = context in
    (* TODO: ugly concats. *)
    prefix @ cells @ suffix

  let refine_partition : G.t -> partition -> partition option =
    (* TODO optim: exception to exit *)
    fun graph partition ->
      Autotools.zip_fold
        (fun rev_prefix cell suffix acc ->
           (* TODO optim: if cell is singleton, skip all computations. *)
           match acc with
           | Some result -> acc
           | None ->
             let prefix = List.rev rev_prefix in (* zip_fold gives prefix in reverse order. *)            
             let outcome = exists_cell_shattering_cell graph prefix cell in
             let outcome =
               match outcome with
               | Some _ -> outcome
               | None   -> exists_cell_shattering_cell graph (cell :: suffix) cell in
             match outcome with
             | None ->
               (* let _ = *)
               (*   Printf.printf *)
               (*     "cell = %s is not splittable\n" *)
               (*     (Autotools.to_sseq G.V.to_string "," cell.list) *)
               (* in *)
               None
             | Some cell' ->
               Some (perform_cell_split ~graph ~target_cell:cell ~splitter_cell:cell' ~context:(prefix, suffix))
        ) None partition

  let rec refine_partition_until_equitable graph partition =
    match refine_partition graph partition with
    | None           -> partition
    | Some partition -> refine_partition_until_equitable graph partition

  let rec is_partition_discrete = function
    | [] -> true
    | cell :: tail ->
      (cell.size = 1) && (is_partition_discrete tail)

  let permutation_of_discrete_partition graph part =
    let vertices : G.vertex list = List.rev (G.fold_vertex (fun v acc -> v :: acc) graph []) in
    let part : G.vertex list =
      List.map
        (fun cell ->
           match cell.list with
           | [x] -> x
           | _ -> failwith "permutation_of_discrete_partition: partition is not discrete"
        ) part
    in
    Perm.of_mapping (List.combine part vertices)

  let apply_perm graph perm =
    G.map_vertex (Perm.action perm) graph

  let points_in_same_orbit group point1 point2 =
    let res = Permgroup.mem group (Perm.of_cycles [[| point1; point2 |]]) in
    if res then
      Printf.printf "two points in the same orbit!\n%!";
    res

  type outcome_rec =
    {
      automorphisms : Permgroup.t;
      explored      : Perm.t GraphMap.t;
      minimizer     : G.t;
      min_perm      : Perm.t
    }

  (* outcome of [explore] are: a canonical graph labeling and the automorphism group of [graph]*)
  let find_graph graph explored =
    try Some (GraphMap.find graph explored)
    with Not_found -> None

  let minimal_graph g1 g2 =
    let c = graph_compare g1 g2 in
    if c < 0 then
      g1
    else
      g2

  let print_partition partition =
    Autotools.to_sseq (fun cell ->
        let s = Autotools.to_sseq PrintVert.to_string "," cell.list in
        Printf.sprintf "(%s)" s
      ) "; " partition


  let print_vertex (v : G.V.t) =
    let vs = PrintVert.to_string v in
    let ls = VertLab.to_string (G.V.label v) in
    Printf.sprintf "(%s : %s)" vs ls
      
  let print_edge (e : G.E.t) =
    let es = PrintEdge.to_string e in
    let ls = EdgeLab.to_string (G.E.label e) in
    Printf.sprintf "(%s : %s)" es ls       

  let print_graph g =
    let vertices = List.rev (G.fold_vertex (fun v acc -> v :: acc) g []) in
    let edges    = G.fold_edges_e (fun e acc -> e :: acc) g [] in
    let vs = Autotools.to_sseq print_vertex "," vertices in
    let es = Autotools.to_sseq print_edge "," edges in
    Printf.printf "vertices = %s;\nedges =\n%s\n" vs es

  let rec explore graph ({ automorphisms; explored; minimizer; min_perm } as outcome) partition =
    if is_partition_discrete partition then
      (* let _ = *)
      (*   Printf.printf "explore discrete: %s\nexplored card:%d\n" (print_partition partition) (GraphMap.cardinal explored) *)
      (*   (\* Printf.printf "explored:\n"; *\) *)
      (*   (\* GraphMap.iter (fun gr _ -> print_graph gr) explored; *\) *)
      (*   (\* Printf.printf "candidate:\n"; *\) *)
      (*   (\* print_graph (apply_perm graph (permutation_of_discrete_partition graph partition)) *\) *)
      (* in *)
      (* leaf of the search tree *)
      let perm   = permutation_of_discrete_partition graph partition in
      (* let _ = Printf.printf "corresponding perm:\n%s\n" (Perm.print perm) in *)
      let graph' = apply_perm graph perm in
      match find_graph graph' explored with
      | None ->
        let outcome =
          {
            outcome with
            explored  = GraphMap.add graph' perm explored;
            minimizer = minimal_graph minimizer graph';
            min_perm  = perm;
          }
        in
        `Outcome outcome
      | Some perm' ->
        let auto = Perm.prod (Perm.inv perm) perm' in
        (* let _    = Printf.printf "found automorphism\n%s\nobtained from\n%s\nand from\n%s\n" (Perm.print auto) (Perm.print perm) (Perm.print perm') in *)
        let outcome =
          if Perm.equal perm perm' then
            outcome
          else
            { outcome with
              automorphisms = Permgroup.extend_mc automorphisms auto
            }
        in
        `Outcome outcome
        (* `FastExit(graph', outcome) *)
    else
      (* let _ = *)
      (*   Printf.printf "explore indiscrete: %s\n" (print_partition partition) *)
      (* in *)
      find_first_nontrivial_cell graph outcome partition []

  and find_first_nontrivial_cell graph outcome partition rev_prefix =
    match partition with
    | [] -> (* all cells are discrete *)
      failwith (Printf.sprintf "Error at %s %d: all cells are discrete\n%!" __MODULE__ __LINE__)
    | cell :: tail ->
      if cell.size <= 1 then
        find_first_nontrivial_cell graph outcome tail (cell :: rev_prefix)
      else
        (* enumerate all splits up to automorphism *)
        enumerate_splits graph outcome (rev_prefix, cell, tail)

  and enumerate_splits graph ({ explored = pre_explored } as outcome) (rev_prefix, cell, tail) =
    Autotools.zip_fold
      (fun rev_cell_prefix v cell_tail outcome_acc ->
         match outcome_acc with
         | `FastExit(_, _) ->
           outcome_acc
         | `Outcome outcome -> 
           if List.exists (points_in_same_orbit outcome.automorphisms v) rev_cell_prefix then
             outcome_acc
           else
             let cell' =
               {
                 list = List.rev_append rev_cell_prefix cell_tail;
                 size = cell.size - 1
               }
             in
             let v_cell     = { list = [v]; size = 1 } in
             let partition' = List.rev_append rev_prefix (v_cell :: cell' :: tail) in
             let partition' = refine_partition_until_equitable graph partition' in
             let result     = explore graph outcome partition' in
             match result with
             | `Outcome _ -> result
             | `FastExit(graph, outcome) ->
               (* are we responsible for this fast exit? *)
               if GraphMap.mem graph outcome.explored && not (GraphMap.mem graph pre_explored) then
                 `Outcome outcome
               else
                 result
      ) (`Outcome outcome) cell.list

  let compute graph =
    let partition = initial_partition graph in
    let partition = refine_partition_until_equitable graph partition in
    let outcome_rec =
      {
        automorphisms = Permgroup.trivial;
        explored      = GraphMap.empty;
        minimizer     = graph;
        min_perm      = Perm.identity
      }
    in
    match explore graph outcome_rec partition with
    | `Outcome out -> (out.automorphisms, out.minimizer, Perm.to_mapping out.min_perm)
    | `FastExit _  -> failwith "error"


  (* let rec explore graph explored aut minimizer partition = *)
  (*   if is_partition_discrete partition then *)
  (*     let digest = apply_perm graph partition in *)
  (*     begin match digest_find_opt digest explored with *)
  (*           | None      -> () *)
  (*           | Some perm -> *)
  (*              add_automorphism aut (Perm.product (Perm.inverse partition) perm) *)
  (*     end; *)
  (*     digest_add digest partition explored; *)
  (*     digest_min minimizer digest *)
  (*   else *)
  (*     find_first_smallest_cell graph explored aut minimizer partition *)

  (* and find_first_smallest_cell graph explored aut minimizer partition smallest = *)
  (*   match partition with *)
  (*   | [] -> *)
  (*      None *)
  (*   | cell :: tail -> *)
  (*      match smallest with *)
  (*      |  ->  *)

  (* and enumerate_splits graph explored aut minimizer partition = *)
  (*   match partition with *)
  (*   | [] -> *)
  (*      failwith (Printf.sprintf "Error at %s %d\n%!" __MODULE__ __LINE__) *)
  (*   | cell :: tail -> *)

end
