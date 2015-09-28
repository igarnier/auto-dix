(* Abstract signature for concrete, persistent graphs.
 * This signature is mostly compatible with that provided by the OCamlGraph library. 
 * The only differences are:
 * - we ask for to_string functions in VERTEX and EDGE.
 * - we ask for a max_vertex function for graphs, returning the biggest vertex.
 * - we don't ask for add_edge, remove_edge, mem_edge, find_edge, iter_edges
 * - the signature of fold_edges is richer
 * - we want to compare graphs
 *)

module type S =
  sig

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

        
    type t

    module V : VERTEX

    type vertex = V.t

    module E : EDGE with type vertex = vertex

    type edge = E.t

    val is_directed : bool

    (* Creating graphs *)

    val empty : t

    (* The empty graph. *)

    val add_vertex : t -> vertex -> t

    (* add_vertex g v adds the vertex v to the graph g. Just return g if v is already in g. *)

    val remove_vertex : t -> vertex -> t

    (* remove g v removes the vertex v from the graph g (and all the edges going from v in g). Just return g if v is not in g. *)

    (* val add_edge : t -> vertex -> E.label -> vertex -> t *)

    (* add_edge g v1 v2 adds an edge from the vertex v1 to the vertex v2 in the graph g. Add also v1 (resp. v2) in g if v1 (resp. v2) is not in g. Just return g if this edge is already in g. *)

    val add_edge_e : t -> edge -> t

    (* add_edge_e g e adds the edge e in the graph g. Add also E.src e (resp. E.dst e) in g if E.src e (resp. E.dst e) is not in g. Just return g if e is already in g. *)

    (* val remove_edge : t -> vertex -> vertex -> t *)
                                    
    (* remove_edge g v1 v2 removes the edge going from v1 to v2 from the graph g. If the graph is labelled, all the edges going from v1 to v2 are removed from g. Just return g if this edge is not in g. *)
    (* Raises Invalid_argument if v1 or v2 are not in g. *)

    val remove_edge_e : t -> edge -> t

    (* remove_edge_e g e removes the edge e from the graph g. Just return g if e is not in g. *)
    (* Raises Invalid_argument if E.src e or E.dst e are not in g. *)
                                       

    (* Size functions *)                    

    val is_empty : t -> bool

    val nb_vertex : t -> int

    val nb_edges : t -> int


    (* Degree of a vertex *)

    val out_degree : t -> vertex -> int

    (* out_degree g v returns the out-degree of v in g.
   Raises Invalid_argument if v is not in g. *)

    val in_degree : t -> vertex -> int

    (* in_degree g v returns the in-degree of v in g.
   Raises Invalid_argument if v is not in g. *)

    (* Membership functions *)

    val mem_vertex : t -> vertex -> bool

    (* val mem_edge : t -> vertex -> vertex -> bool *)

    val mem_edge_e : t -> edge -> bool

    (* val find_edge : t -> vertex -> vertex -> edge *)

    (* find_edge g v1 v2 returns the edges from v1 to v2 if they exists.
     * returns empty list if no such edge exist. *)

    val find_all_edges : t -> vertex -> vertex -> edge list

    (* find_all_edges g v1 v2 returns all the edges from v1 to v2. *)

    (* Successors and predecessors *)

    val succ : t -> vertex -> vertex list

    (* succ g v returns the successors of v in g.
   Raises Invalid_argument if v is not in g. *)

    val pred : t -> vertex -> vertex list

    (* pred g v returns the predecessors of v in g.
   Raises Invalid_argument if v is not in g. *)

    (* Labeled edges going from/to a vertex *)

    val succ_e : t -> vertex -> edge list

    (* succ_e g v returns the edges going from v in g.
   Raises Invalid_argument if v is not in g. *)

    val pred_e : t -> vertex -> edge list

    (* pred_e g v returns the edges going to v in g.
   Raises Invalid_argument if v is not in g. *)

    (* Graph iterators *)

    val iter_vertex : (vertex -> unit) -> t -> unit

    (* Iter on all vertices of a graph. *)

    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a

    (* Fold on all vertices of a graph. *)

    (* val iter_edges : (vertex -> vertex -> unit) -> t -> unit *)

    (* Iter on all edges of a graph. Edge label is ignored. *)

    val fold_edges : (vertex -> vertex -> edge list -> 'a -> 'a) -> t -> 'a -> 'a
                                                           
    (* Fold on all edges of a graph. Edge label is ignored. *)

    val iter_edges_e : (edge -> unit) -> t -> unit

    (* Iter on all edges of a graph. *)

    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a

    (* Fold on all edges of a graph. *)

    val map_vertex : (vertex -> vertex) -> t -> t

    (* Map on all vertices of a graph. *)

    (* Vertex iterators *)

    (* Each iterator iterator f v g iters f to the successors/predecessors of v in the graph g and raises Invalid_argument if v is not in g. It is the same for functions fold_* which use an additional accumulator. *)

    (* iter/fold on all successors/predecessors of a vertex. *)

    val iter_succ : (vertex -> unit) -> t -> vertex -> unit

    val iter_pred : (vertex -> unit) -> t -> vertex -> unit

    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

    (* iter/fold on all edges going from/to a vertex. *)

    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit

    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit

    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a


    (* Classical algorithms *)

    val induced_subgraph : t -> Set.Make(V).t -> t

    val partition : t -> vertex list -> t * t

    val connected_components : t -> t list                                          

    (* Total order on graphs *)
    val compare : t -> t -> int
                                      
  end                                                               
