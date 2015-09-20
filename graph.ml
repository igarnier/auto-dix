open Aux
    
module Make(VL : PrintableOrderedType)(EL : PrintableOrderedType) =
  struct

    module type VERTEX =
      sig

        type t

        val compare : t -> t -> int

        val hash : t -> int

        val equal : t -> t -> bool

        module L : Aux.PrintableOrderedType

        type label = L.t

        val create : label -> t

        val label : t -> label

        val to_string : t -> string

      end

    module type EDGE =
      sig

        type t

        val compare : t -> t -> int

        type vertex

        val src : t -> vertex

        val dst : t -> vertex

        module L : Aux.PrintableOrderedType
                         
        type label = L.t

        val create : vertex -> label -> vertex -> t
                                                    
        val label : t -> label

        val to_string : t -> string
                               
      end
        
    module V =
      struct

        module L = VL
        
        type label = L.t

        type t = { v : Fresh.t; lbl : VL.t }

        let compare x y =
          let delta = x.v - y.v in
          if delta = 0 then
            VL.compare x.lbl y.lbl
          else
            delta

        let hash (x : t) = Hashtbl.hash x.v

        let equal (x : t) (y : t) = x.v = y.v

        let create b = { v = Fresh.gen (); lbl = b }

        let label { lbl } = lbl
                              
        let to_string { v; lbl } =
          Printf.sprintf "%s/%s" (string_of_int v) (VL.to_string lbl)
                         
      end

    module UPair = Aux.UnorderedPair(V)
        
    module E =
      struct
        
        type vertex = V.t

        module L = EL
                        
        type label = L.t
                       
        type t = { src : vertex; dst : vertex; lbl : EL.t }

        let compare e e' =
          let c = EL.compare e.lbl e'.lbl in
          if c = 0 then
            UPair.compare (e.src, e.dst) (e'.src, e'.dst)
          else
            c
               
        (* let c_src = V.compare e.src e'.src in *)
        (* let c_dst = V.compare e.dst e'.dst in *)
        (* if c_src = 0 && c_dst = 0 then *)
        (* if V.equal e.src e'.src && V.equal e.dst e'.dst || *)
        (*    V.equal e.src e'.dst && V.equal e.dst e'.src then *)
        (*                      Pervasives.compare e.lbl e'.lbl *)
        (*                    else *)

        let equal e e' =
          (compare e e') = 0
                             
        let src { src } = src
                            
        let dst { dst } = dst

        let create src lbl dst = { src; lbl; dst }

        let label { lbl } = lbl

        let to_string { src; lbl; dst } =
          Printf.sprintf "%s -- %s --> %s"
                         (V.to_string src)
                         (EL.to_string lbl)
                         (V.to_string dst)

      end


    let is_directed = false

    type vertex = V.t
    type edge   = E.t

    module UPairMap  = Map.Make(UPair)
    module EdgeSet   = Set.Make(E)
    module VertexSet = Set.Make(V)

    type t =
      {
        vertices : VertexSet.t;                 (* vertices *)
        edges    : (E.t list) UPairMap.t;       (* get digraph by replacing UPairMap by PairMap (and updating code ...) *)
        iedges   : EdgeSet.t
      }

    (* accessors *)
        
    (* let size { vertices } = List.length vertices *)

    let empty =
      {
        vertices = VertexSet.empty;
        edges    = UPairMap.empty;
        iedges   = EdgeSet.empty    
      }

    let add_vertex graph vertex =
      { graph with
        vertices = VertexSet.add vertex graph.vertices
      }

    let mem_vertex graph vertex =
      VertexSet.mem vertex graph.vertices

    let mem_edge_e graph edge =
      EdgeSet.mem edge graph.iedges

    let find_all_edges graph v1 v2 =
      try UPairMap.find (v1, v2) graph.edges
      with Not_found -> []
                    
    let remove_vertex graph vertex =
      if mem_vertex graph vertex then
        let vertices = VertexSet.remove vertex graph.vertices in
        let edges    =
          UPairMap.filter
            (fun (v, v') _ -> not (V.equal vertex v || V.equal vertex v'))
            graph.edges
        in
        let iedges   =
          EdgeSet.filter
            (fun edge -> not (V.equal (E.src edge) vertex || V.equal (E.dst edge) vertex))
            graph.iedges
        in
        {
          vertices; edges; iedges
        }
      else
        graph

    let add_edge_e graph edge =
      let src, dst = (E.src edge, E.dst edge) in
      let graph = add_vertex graph src in
      let graph = add_vertex graph dst in
      let edges =
        try UPairMap.find (src, dst) graph.edges
        with Not_found -> []
      in
      let edges  = UPairMap.add (src, dst) (edge :: edges) graph.edges in
      let iedges = EdgeSet.add edge graph.iedges in
      {
        graph with edges; iedges
      }

    let remove_edge_e graph edge =
      let src, dst = (E.src edge, E.dst edge) in      
      if not (mem_vertex graph src && mem_vertex graph dst) then
        raise (Invalid_argument "Graph.remove_edge_e: vertices not found")
      else
        try
          let edges = UPairMap.find (src, dst) graph.edges in
          let edges = List.filter (fun e -> (E.compare e edge) <> 0) edges in
          let edges = UPairMap.add (src, dst) edges graph.edges in
          let iedges = EdgeSet.remove edge graph.iedges in
          { graph with edges; iedges }
        with Not_found -> graph

    let is_empty graph =
      VertexSet.is_empty graph.vertices
                         
    let nb_vertex graph =
      VertexSet.fold (fun _ -> (+) 1) graph.vertices 0

    let nb_edges graph =
      EdgeSet.fold (fun _ -> (+) 1) graph.iedges 0

    (* /!\ Assumes undirected graph /!\ *)
    let in_degree graph vertex =
      UPairMap.fold
        (fun (v1, v2) edges acc ->
         match edges with
         | [] -> acc
         | _ -> 
            if V.equal v1 vertex || V.equal v2 vertex then
              1 + acc
            else
              acc
        ) graph.edges 0
        
    (* Undirected graph *)
    let out_degree = in_degree 
                       
    let succ graph vertex =
      UPairMap.fold
        (fun (v1, v2) edges acc ->
         match edges with
         | [] -> acc
         | _ -> 
            if V.equal v1 vertex then
              v2 :: acc
            else if v2 = vertex then
              v1 :: acc
            else
              acc
        ) graph.edges []

    (* Undirected graph *)                                           
    let pred = succ
                 
    let succ_e graph vertex =
      UPairMap.fold
        (fun (v1, v2) edges acc ->
         match edges with
         | [] -> acc
         | _ -> 
            if V.equal v1 vertex || V.equal v2 vertex then
              List.rev_append edges acc
            else
              acc
        ) graph.edges []

    (* Undirected graph *)
    let pred_e = succ_e

    let iter_vertex f graph =
      VertexSet.iter f graph.vertices

    let fold_vertex f graph acc =
      VertexSet.fold f graph.vertices acc

    let iter_edges_e f graph =
      EdgeSet.iter f graph.iedges

    let fold_edges f graph acc =
      UPairMap.fold (fun (v, v') edges acc -> f v v' edges acc) graph.edges acc

    let fold_edges_e f graph acc =
      EdgeSet.fold f graph.iedges acc

    let map_vertex f graph =
      let g =
        fold_vertex (fun v g ->
                     add_vertex g (f v)
                    ) graph empty
      in
      fold_edges_e (fun e g ->
                    add_edge_e g { e with E.src = (f e.E.src); E.dst = (f e.E.dst) }
                   ) graph g


    let iter_succ f graph vertex =
      let s = succ graph vertex in
      List.iter f s

    let iter_pred = iter_succ

    let fold_succ f graph vertex acc =
      let s = succ graph vertex in
      List.fold_left (fun acc e -> f e acc) acc s

    let fold_pred = fold_succ

    (* iter/fold on all edges going from/to a vertex. *)

    let iter_succ_e f graph vertex =
      let es = succ_e graph vertex in
      List.iter f es

    let iter_pred_e = iter_succ_e
                        
    let fold_succ_e f graph vertex acc =
      let es = succ_e graph vertex in
      List.fold_left (fun acc e -> f e acc) acc es

    let fold_pred_e = fold_succ_e

    (* Classical algorithms *)

    let induced_subgraph graph vertices =
      let edges    =
        UPairMap.filter
          (fun (v, v') _ -> (VertexSet.mem v vertices) && (VertexSet.mem v' vertices)) graph.edges
      in
      let iedges   =
        EdgeSet.filter
          (fun e -> VertexSet.mem (E.src e) vertices && VertexSet.mem (E.dst e) vertices) graph.iedges
      in
      {
        vertices; edges; iedges
      }
        
    let partition graph vertices =
      let verts, coverts = VertexSet.partition (fun v -> List.exists (V.equal v) vertices) graph.vertices in
      (induced_subgraph graph verts, induced_subgraph graph coverts)
        
    (* let edges = *)
    (*   UPairMap.filter (fun (v,v') _ -> (List.mem v vertices) && (List.mem v' vertices)) g.edges *)
    (* and coedges = *)
    (*   UPairMap.filter (fun (v,v') _ -> not (List.mem v vertices) && not (List.mem v' vertices)) g.edges *)
    (* in *)
    (* let iedges = *)
    (*   IntMap.filter (fun id (v, v', _) -> UPairMap.mem (v, v') edges) g.iedges *)
    (* in *)
    (* let coiedges = *)
    (*   IntMap.filter (fun id (v, v', _) -> UPairMap.mem (v, v') coedges) g.iedges     *)
    (* in *)
    (* ({ vertices = verts; edges = edges; iedges = iedges }, *)
    (*  { vertices = coverts; edges = coedges; iedges = coiedges }) *)


    let connected_components =
      let rec transitive_closure vertex graph visited_vertices =
        let verts = succ graph vertex in
        let not_yet_visited =
          List.filter (fun v -> not (List.exists (V.equal v) visited_vertices)) verts in
        if not_yet_visited = [] then
          visited_vertices
        else
          List.fold_left
            (fun acc v ->
             transitive_closure v graph (v :: acc)
            ) visited_vertices not_yet_visited
      in
      let rec loop graph connected_components =
        try
          let v            = VertexSet.choose graph.vertices in
          let clo          = transitive_closure v graph [v] in
          let subg, cosubg = partition graph clo in
          loop cosubg (subg :: connected_components)
        with
        | Not_found -> (* empty graph*)
           connected_components
      in
      fun graph -> loop graph []

  (* Glue two graphs along partial injections on vertices /and/ edges. 
   * /!\ We __assume__ the graphs have disjoint vertex and edge sets!
   * TODO: assert correctness of vinj and einj (no edge twisting, color preservation)
   *)
                        (* let glue_graphs *)
                        (*       (type vertex_color) *)
                        (*       (type edge_color) *)
                        (*       (g1 : (vertex_color, edge_color) t) *)
                        (*       (g2 : (vertex_color, edge_color) t) *)
                        (*       (vinj : (vertex, vertex) Pinj.t) *)
                        (*       (einj : (Edge.uid, Edge.uid) Pinj.t) *)
                        (*   = *)
                        (*   (\* map each pair (v, v') \in vinj to a fresh vertex *\) *)
                        (*   let fresh_vmap = vmap_from_vinj vinj in *)
                        (*   let vmap       = extend_map_to_identity fresh_vmap in *)
                        (*   (\* same for edges *\) *)
                        (*   let fresh_emap = emap_from_einj vmap einj in *)
                        (*   let emap       = extend_map_to_identity fresh_emap in   *)
                        (*   let vertices_from_g1 = *)
                        (*     IntMap.fold *)
                        (*       (fun v clr acc -> *)
                        (*        IntMap.add (vmap v) clr acc *)
                        (*       ) g1.vertices IntMap.empty *)
                        (*   in *)
                        (*   let vertices_from_g1g2 = *)
                        (*     IntMap.fold *)
                        (*       (fun v clr acc -> *)
                        (*        IntMap.add (vmap v) clr acc *)
                        (*       ) g2.vertices vertices_from_g1 *)
                        (*   in *)
                        (*   let edges_from_g1 = *)
                        (*     UPairMap.fold *)
                        (*       (fun (v1, v2) edges acc -> *)
                        (*        let pair  = (vmap v1, vmap v2) in *)
                        (*        let edges = List.map (fun x -> { x with Edge.uid = emap x.Edge.uid }) edges in *)
                        (*        let edges = Aux.filter_duplicates edges in *)
                        (*        UPairMap.add pair edges acc *)
                        (*       ) g1.edges UPairMap.empty *)
                        (*   in *)
                        (*   let edges_from_g1g2 = *)
                        (*     UPairMap.fold *)
                        (*       (fun (v1, v2) edges acc -> *)
                        (*        let pair  = (vmap v1, vmap v2) in *)
                        (*        let edges = List.map (fun x -> { x with Edge.uid = emap x.Edge.uid }) edges in *)
                        (*        let prev_edges = *)
                        (*          try UPairMap.find pair acc *)
                        (*          with Not_found -> [] *)
                        (*        in *)
                        (*        let edges = Aux.filter_duplicates (List.rev_append prev_edges edges) in *)
                        (*        UPairMap.add pair edges acc *)
                        (*       ) g2.edges edges_from_g1 *)
                        (*   in *)
                        (*   let iedges = *)
                        (*     UPairMap.fold *)
                        (*       (fun (v1, v2) edges iedges -> *)
                        (*        List.fold_left *)
                        (*          (fun iedges { Edge.uid; color } -> *)
                        (*           IntMap.add uid (v1, v2, color) iedges *)
                        (*          ) iedges edges *)
                        (*       ) edges_from_g1g2 IntMap.empty *)
                        (*   in *)
                        (*   let gluing = *)
                        (*     { *)
                        (*       vertices = vertices_from_g1g2; *)
                        (*       edges  = edges_from_g1g2; *)
                        (*       iedges = iedges *)
                        (*     } *)
                        (*   in *)
                        (*   (gluing, fresh_vmap, vmap, fresh_emap, emap) *)

  end
