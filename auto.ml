open Prelude

module Make
  (NLab : Ordered)
  (LLab : Ordered) =
struct

  (* Automorphism group discovery for (undirected) simple 
   * colored graphs - should work for pretty much any kind 
   * of graph. *)

  (* ---------- *)
  (* Type decls *)
  
  type cell       = IntSet.t

  type partition  = cell list

  type dpartition = {
    dom   : partition;
    codom : partition
  }

  type exploration =
  | Exhaustive   of dpartition list
  | CosetPruning of Perm.ArrayBased.t
  | OrbitPruning of int * dpartition * (int * dpartition) list

  (* Used to quickly escape loops and nested recursions *)
  exception FastExit

  (* Used in coset pruning *)
  exception SolutionFound of (Perm.ArrayBased.t list)

  (* ------------------------------------------------------ *)
  (* Implementing Saucy-like automorphism group computation *)

  let make_root size =
    let l = mk_ints 0 (size-1) in
    let s = [List.fold_right IntSet.add l IntSet.empty] in
    { dom   = s;
      codom = s  }

  let print partition =
    let p x = Printf.printf "%d " (IntSet.choose x) in
    List.iter p partition.dom;
    Printf.printf "\n";
    List.iter p partition.codom;
    Printf.printf "\n------------\n"

  let print partition =
    let p x = print_string (IntSet.print x) in
    List.iter p partition.dom;
    Printf.printf "\n";
    List.iter p partition.codom;
    Printf.printf "\n------------\n"

  let rec cells_equal (a : cell) (b : cell) = IntSet.equal a b
    
  let rec partitions_isomorphic pa pb =
    match (pa, pb) with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | a :: la, b :: lb ->
      (IntSet.cardinal a = IntSet.cardinal b) && partitions_isomorphic la lb
        
  (* Assume the cell is well-formed. Searches for first non-singleton
     cell and split it. Assumes well formed cell. Assumes that the
     partition is not discrete (case checked in check_for_matching) *)
  let rec exhaustive_children domain codomain dacc cacc =
    match domain, codomain with
    | [], [] ->
      failwith "Auto.exhaustive_children: discrete partition"
    | dcell :: dtail, ccell :: ctail ->
      if IntSet.is_empty dcell then
        failwith "Auto.exhaustive_children: empty cells"
      else if IntSet.is_singleton dcell then
        exhaustive_children dtail ctail (dcell :: dacc) (ccell :: cacc)
      else     
        let elt     = IntSet.choose dcell in
        let fixed   = IntSet.singleton elt in
        let dcell'  = IntSet.remove elt dcell in
        let dom     = List.rev_append dacc (fixed :: dcell' :: dtail) in
        let result  = IntSet.fold (fun codomelt acc ->
          let ccell'  = IntSet.remove codomelt ccell in
          let cofixed = IntSet.singleton codomelt in
          { dom   = dom;
            codom = List.rev_append cacc (cofixed :: ccell' :: ctail) } :: acc
        ) ccell [] in
        Exhaustive result
    | _ -> failwith "Auto.exhaustive_children: partitions not matching in size"

  (* Tries to identify a cell amenable to orbit pruning. If successful, return corresponding goal. *)
  let rec check_for_orbit_pruning domain codomain dacc cacc =
    match domain, codomain with
    | [], [] ->
      None
    | dcell :: dtail, ccell :: ctail ->
      if IntSet.is_singleton dcell then
        check_for_orbit_pruning dtail ctail (dcell :: dacc) (ccell :: cacc)
      else 
        let isect = IntSet.inter dcell ccell in
        if IntSet.is_empty isect then
          check_for_orbit_pruning dtail ctail (dcell :: dacc) (ccell :: cacc)
        else
          let elt = IntSet.choose isect in
          let fixed   = IntSet.singleton elt in
          let dcell'  = IntSet.remove elt dcell in
          let dom     = List.rev_append dacc (fixed :: dcell' :: dtail) in
          let ccell'  = IntSet.remove elt ccell in
          let cosets  = IntSet.fold (fun codomelt acc ->
            let ccell'  = IntSet.remove codomelt ccell in
            let cofixed = IntSet.singleton codomelt in
            (codomelt,
             { dom   = dom;
               codom = List.rev_append cacc (cofixed :: ccell' :: ctail) }) :: acc
          ) ccell' [] in
          let stabilizer =
            let cofixed = fixed in
            { dom   = dom;
              codom = List.rev_append cacc (cofixed :: ccell' :: ctail) }
          in
          Some (OrbitPruning(elt, stabilizer, cosets))
    | _ -> failwith "Auto.check_for_matching: partitions not matching in size"

  (* Assumes the cell is well-formed and non-discrete. 
     Checks for coset pruning opportunities. *)
  let rec check_for_matching domain codomain size acc acclen =
    match domain, codomain with
    | [], [] ->
      if acclen = 0 then
        None
      else
        (let perm = Array.init size (fun i -> i) in
         List.iter (fun (x,y) -> perm.(x) <- y) acc;
         Some (CosetPruning (Perm.ArrayBased.of_array perm)))
    | dcell :: dtail, ccell :: ctail ->
      if IntSet.is_empty dcell then
        failwith "Auto.produce_children: empty cells"
      else if IntSet.is_singleton dcell then
        let x = IntSet.choose dcell in
        let y = IntSet.choose ccell in
        check_for_matching dtail ctail size ((x,y) :: acc) (acclen + 1)
      else if IntSet.equal dcell ccell then
        check_for_matching dtail ctail size acc acclen
      else
        None
    | _ -> failwith "Auto.check_for_matching: partitions not matching in size"

  let check_for_matching domain codomain size =
    check_for_matching domain codomain size [] 0

  let check_for_discreteness list =
    List.for_all IntSet.is_singleton list
      
  let produce_children domain codomain size =
    match check_for_matching domain codomain size with
    | Some outcome -> outcome
    | None ->
      match check_for_orbit_pruning domain codomain [] [] with
      | Some outcome -> outcome
      | None ->
        exhaustive_children domain codomain [] []
      
  (* Convert a discrete partition to an array-based permutation.
   * Assumes discreteness of partition. *)
  let to_perm : int -> dpartition -> Perm.ArrayBased.t =
    fun size partition ->
      let arr = Array.create size 0 in
      try      
        List.iter2 (fun cell1 cell2 ->
          arr.(IntSet.choose cell1) <- IntSet.choose cell2
        ) partition.dom partition.codom;
        Perm.ArrayBased.of_array arr
      with Invalid_argument _ ->
        failwith "Auto.to_perm: partition has invalid size"


  (* Test whether a permutation presented as a discrete partition is 
   * a graph automorphism. This function assumes that the graph is
   * colored but /simple/. *)
  let rec is_automorphism_aux 
      (graph : (NLab.t, LLab.t) Graph.t) 
      (sigma : Perm.ArrayBased.t)
      (queue : int list) 
      (explored : bool array) 
      =
    match queue with
    | [] -> ()
    | node :: queue ->
      if explored.(node) then
        is_automorphism_aux graph sigma queue explored
      else
        let info  = Graph.get_info graph node in
        let info' = Graph.get_info graph (Perm.ArrayBased.action sigma node) in
        if (Graph.clr info) = (Graph.clr info') && List.length (Graph.adj info) = List.length (Graph.adj info') then
          (let queue = List.fold_left (fun acc (lab, n) ->
            if List.mem (lab, Perm.ArrayBased.action sigma n) (Graph.adj info') then
              n :: acc
            else
              raise FastExit
           ) queue (Graph.adj info) in
           explored.(node) <- true;
           is_automorphism_aux graph sigma queue explored)
        else
          raise FastExit

  let auto_count = ref Int64.zero
            
  let is_automorphism graph sigma =
    auto_count := Int64.add !auto_count Int64.one;
    (* pick any node *)
    let (node, _) = Graph.NodeIdMap.choose (Graph.info graph) in
    let visited   = Array.make (Graph.size graph) false in
    try
      (is_automorphism_aux graph sigma [node] visited;
       true)
    with
      FastExit -> false

  (* ------------------------------------- *)
  (* Implementation of partition refinment *)

  (* Instantiate multiset module for links *)
  module Mset = Prelude.Multiset(LLab)

  (* Total order on graph features *)
  let feature_compare ((nc, ld) : NLab.t * Mset.t) ((nc', ld') : NLab.t * Mset.t) =
    let c = NLab.compare nc nc' in
    if c = 0 then
      Mset.compare ld ld'        
    else c

  let rec mem_assoc' key = function
    | [] -> None
    | (clr, x) :: tail ->
      if x = key then Some clr
      else mem_assoc' key tail

  let rec compute_edgecounts vertex view acc =
    match view with
    | [] -> acc
    | (v, info, count) :: tail ->
      (match mem_assoc' vertex (Graph.adj info) with
      | None -> 
        compute_edgecounts vertex tail acc
      | Some clr ->
        count := Mset.add clr !count; 
        compute_edgecounts vertex tail (Mset.add clr acc)
      )

  let compute_edgecounts vertex view = compute_edgecounts vertex view Mset.empty

  (* The following code ignores the colours of edges. It is faster but distinguishes less
   * features of the graph. In simple experiments, it appears to be enough ... *)
(*
  let feature_compare ((nc, ld) : NLab.t * int) ((nc', ld') : NLab.t * int) =
    let c = NLab.compare nc nc' in
    if c = 0 then
      compare ld ld'
    else c

  let rec mem' key = function
    | [] -> false
    | (clr, x) :: tail ->
      x = key || mem' key tail

  let rec compute_edgecounts vertex view acc =
    match view with
    | [] -> acc
    | (v, info, count) :: tail ->
      if mem' vertex info.Graph.adj then
        compute_edgecounts vertex tail acc
      else
        (incr count;
         compute_edgecounts vertex tail (acc+1))

  let compute_edgecounts vertex view = compute_edgecounts vertex view 0
*)

  (* Compute local degrees of each vertex in a cell. *)
  (* Also, edge colours. We don't need just a count, we need a multiset of edge colours. *)
  let compute_local_view (graph : (NLab.t, LLab.t) Graph.t) cell =
    let result = IntSet.fold (fun vertex view ->
      let counter = compute_edgecounts vertex view in
      ((vertex, Graph.get_info graph vertex, ref counter) :: view) 
    ) cell [] in
    List.map (fun (v, i, c) -> (v, (Graph.clr i, !c))) result

  let rec partition_views dom_view codom_view dom codom dom_cells codom_cells =
    match (dom_view, codom_view) with
    | (v, f) :: [], (v', f') :: [] ->
      if feature_compare f f' = 0 then
        Some ((IntSet.add v dom) :: dom_cells, (IntSet.add v' codom) :: codom_cells)
      else
        None
    | (u, f) :: ((v, g) :: _ as dom_tail), (u', f') :: ((v', g') :: _ as codom_tail) ->
      if feature_compare f f' = 0 then
        if feature_compare f g = 0 then
          (* stay in the same feature class: accumulate in current domain and codomain cells *)
          partition_views dom_tail codom_tail (IntSet.add u  dom) (IntSet.add u' codom) dom_cells codom_cells
        else
          (* Change feature class: push current cells in current dpartition *)
          partition_views dom_tail codom_tail IntSet.empty IntSet.empty ((IntSet.add u dom) :: dom_cells) ((IntSet.add u' codom) :: codom_cells)
      else
        None (* There is no way the input cell can yield an automorphism *)
    | _ -> failwith "Auto.partition_views: views have not the same size."


  (* Precondition: dom and codom have the same size. This function will
   * use information from the graph to split the cells into "equivalence" classes,
   * where two vertices are equivalent if they have the same color and the same
   * local degree. *)
  let refine_cell graph domc codomc domp codomp =
    let dom_view    = compute_local_view graph domc in
    let dom_view    = List.sort (fun (_, f) (_, f') -> feature_compare f f') dom_view in
    let codom_view  = compute_local_view graph codomc in
    let codom_view  = List.sort (fun (_, f) (_, f') -> feature_compare f f') codom_view in
    match partition_views dom_view codom_view IntSet.empty IntSet.empty domp codomp with
    | None -> raise FastExit
    | Some parts -> parts

  (* Precondition: same number of cells in domain and codomain partitions *)
  let rec refine_partition_aux graph domp codomp =
    match domp, codomp with
    | [], [] -> ([], [])
    | domcell :: domtail, codomcell :: codomtail ->
      let (dom', codom') = refine_partition_aux graph domtail codomtail in
      refine_cell graph domcell codomcell dom' codom'
    | _ ->
      failwith "Auto.refine_partition: partitions are ill-formed"

  let refine_partition graph dpartition =
    let (dom, codom) = refine_partition_aux graph dpartition.dom dpartition.codom in
    { dom; codom }

  let rec compute_automorphisms_aux graph root exhaustive acc =
    try
      (* let _ = print root in *)
      if check_for_discreteness root.dom then
        (* Leaf *)
        let root' = to_perm (Graph.size graph) root in
        if is_automorphism graph root' then
          if exhaustive then
            root' :: acc
          else
            raise (SolutionFound (root' :: acc))
        else
          acc
      else
        let root = refine_partition graph root in
        match produce_children root.dom root.codom (Graph.size graph) with
        | Exhaustive children ->
        (* let _ = Printf.printf "exhaustive\n" in *)
          List.fold_left (fun acc child ->
            compute_automorphisms_aux graph child exhaustive acc
          ) acc children
        | CosetPruning perm ->
        (* let _ = Printf.printf "coset pruning %s ... recall root:\n" (Perm.ArrayBased.print perm) in *)
        (* let _ = print root in *)
          if is_automorphism graph perm then
          (* let _ = Printf.printf "sucess\n" in *)
            perm :: acc
          else
          (* let _ = Printf.printf "failure\n" in *)
            acc
        | OrbitPruning(fixedpoint, stabilizing, cosets) ->
        (* let _ = Printf.printf "orbit pruning\n" in *)
          let acc = compute_automorphisms_aux graph stabilizing exhaustive acc in
          List.fold_left (fun acc (_, child) ->
            try
              compute_automorphisms_aux graph child false acc
            with
            | SolutionFound acc -> acc
          ) acc cosets
    with
      FastExit -> acc

  let timer = Prelude.create_timer ()
  let cmlt  = ref 0.0

  let compute_automorphisms : (NLab.t, LLab.t) Graph.t -> Perm.ArrayBased.t list =
    fun graph ->
      let _ = Prelude.start_timer timer in
      let res = compute_automorphisms_aux graph (make_root (Graph.size graph)) true [] in
      cmlt := !cmlt +. (Prelude.get_timer timer);
      res
    

end
