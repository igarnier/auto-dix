module NodeIdMap :
  sig
    type key = int
    type +'a t
    (* val empty : 'a t *)
    (* val is_empty : 'a t -> bool *)
    (* val mem : key -> 'a t -> bool *)
    (* val add : key -> 'a -> 'a t -> 'a t *)
    (* val singleton : key -> 'a -> 'a t *)
    (* val remove : key -> 'a t -> 'a t *)
    (* val merge : *)
    (*   (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t *)
    (* val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int *)
    (* val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool *)
    (* val iter : (key -> 'a -> unit) -> 'a t -> unit *)
    (* val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b *)
    (* val for_all : (key -> 'a -> bool) -> 'a t -> bool *)
    (* val exists : (key -> 'a -> bool) -> 'a t -> bool *)
    (* val filter : (key -> 'a -> bool) -> 'a t -> 'a t *)
    (* val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t *)
    (* val cardinal : 'a t -> int *)
    (* val bindings : 'a t -> (key * 'a) list *)
    (* val min_binding : 'a t -> key * 'a *)
    (* val max_binding : 'a t -> key * 'a *)
    val choose : 'a t -> key * 'a
    (* val split : key -> 'a t -> 'a t * 'a option * 'a t *)
    (* val find : key -> 'a t -> 'a *)
    (* val map : ('a -> 'b) -> 'a t -> 'b t *)
    (* val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t *)
    (* val find_opt : key -> 'a t -> 'a option *)
  end

type ('n, 'l) info

type ('n, 'l) t

val empty : ('a, 'b) t
val size : ('a, 'b) t -> int
val info : ('a, 'b) t -> ('a, 'b) info NodeIdMap.t
val root : ('a, 'b) t -> int

val clr : ('a, 'b) info -> 'a
val adj : ('a, 'b) info -> ('b * int) list
val deg : ('a, 'b) info -> int

val get_info : ('a, 'b) t -> NodeIdMap.key -> ('a, 'b) info

val add_node_with_colour : ('a, 'b) t -> 'a -> ('a, 'b) t
val add_edge : ('a, 'b) t -> NodeIdMap.key -> 'b -> NodeIdMap.key -> ('a, 'b) t
val to_dot : string -> string -> ('a, 'b) t -> ('a -> NodeIdMap.key -> string) -> ('b -> string) -> unit
val print : ('a, 'b) t -> ('a -> NodeIdMap.key -> string) -> ('b -> string) -> unit
val erdos_renyi : int -> float -> (unit -> 'a) -> (unit -> 'b) -> ('b, 'a) t
val complete_simple_graph : int -> (unit, unit) t
