module type Ordered =
  sig
    type t
    val compare : t -> t -> int
    val inhabited : t
    val print : t -> string
  end

val ( ++ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val to_sseq : ('a -> string) -> string -> 'a list -> string

val strof_ilist : int list -> string

val strof_iarr : int array -> string

val dup : 'a -> int -> 'a list

val mk_ints : int -> int -> int list

val list_max : 'a -> 'a list -> 'a

val list_min : 'a -> 'a list -> 'a

val filter_duplicates' : 'a list -> 'a list

val filter_duplicates : 'a list -> 'a list

val union : 'a list -> 'a list -> 'a list

val sections : 'a list list list -> 'a list list

module IntMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val find_opt : key -> 'a t -> 'a option
  end

module IntSet :
  sig
    type elt = int
    type t = Empty | Node of t * elt * t * int
    val compare_elt : int -> int -> int
    val height : t -> int
    val create : t -> elt -> t -> t
    val bal : t -> elt -> t -> t
    val add : elt -> t -> t
    val singleton : elt -> t
    val add_min_element : elt -> t -> t
    val add_max_element : elt -> t -> t
    val join : t -> elt -> t -> t
    val min_elt : t -> elt
    val max_elt : t -> elt
    val remove_min_elt : t -> t
    val merge : t -> t -> t
    val concat : t -> t -> t
    val split : int -> t -> t * bool * t
    val empty : t
    val is_empty : t -> bool
    val is_singleton : t -> bool
    val mem : int -> t -> bool
    val remove : int -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    type enumeration = End | More of elt * t * enumeration
    val cons_enum : t -> enumeration -> enumeration
    val compare_aux : enumeration -> enumeration -> int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> 'a) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements_aux : elt list -> t -> elt list
    val elements : t -> elt list
    val choose : t -> elt
    val print : t -> string
  end

module Multiset :
  functor (O : Ordered) ->
    sig
      module M :
        sig
          type key = O.t
          type 'a t = 'a Map.Make(O).t
          val empty : 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val add : key -> 'a -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val max_binding : 'a t -> key * 'a
          val choose : 'a t -> key * 'a
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val find : key -> 'a t -> 'a
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        end
      type t = int M.t
      val empty : 'a M.t
      val count : M.key -> t -> int
      val add : M.key -> t -> int M.t
      val fold : (M.key -> 'a -> 'b -> 'b) -> 'a M.t -> 'b -> 'b
      val equal : t -> t -> bool
      val compare : t -> t -> int
    end

module DynArray :
  sig
    type 'a t = { mutable fill : int; mutable arr : 'a array; default : 'a; }
    val realloc : 'a t -> unit
    val create : int -> 'a -> 'a t
    val get_unsafe : 'a t -> int -> 'a
    val get : 'a t -> int -> 'a
    val set : 'a t -> int -> 'a -> unit
    val append : 'a t -> 'a -> unit
  end

val take_until : 'a Queue.t -> ('a -> bool) -> 'a option

module Subarray :
  sig
    type 'a t = { mutable left : int; mutable right : int; data : 'a array; }
    val create : int -> 'a -> 'a t
    val data : 'a t -> 'a array
    val left : 'a t -> int
    val right : 'a t -> int
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val first : 'a t -> 'a
    val set_first : 'a t -> 'a -> unit
    val last : 'a t -> 'a
    val set_last : 'a t -> 'a -> unit
    val push_left : 'a t -> unit
    val pull_right : 'a t -> unit
    val of_array : 'a array -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

type timer

val create_timer : unit -> timer
val start_timer : timer -> unit
val reset_timer : timer -> unit
val get_timer : timer -> float

val log : string -> unit
