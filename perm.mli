(* The abstract signature of an (integer) permutation implementation *)
module type PermSig =
sig

  type t

  (* Equality test *)
  val equal : t -> t -> bool

  (* TODO identity should takes as an argument the size of the set on which PermSig acts. *)
  val identity : t

  (* product *)
  val prod     : t -> t -> t

  (* inverse *)
  val inv      : t -> t

  (* action *)
  val action   : t -> int -> int

  (* orbit *)
  val orbit    : t -> int -> Prelude.IntSet.t

  (* any point not fixed by the perm. *)
  val pick_from_support : t -> int option

  val of_cycles : int array list -> t
  val of_array  : int array -> t

  val print : t -> string

end

module CycleBased : PermSig

module ArrayBased : PermSig

module type PermType =
sig
  module Concrete : PermSig
  type permrec = { p : Concrete.t; invp : Concrete.t }
  type t = Perm of permrec | Prod of t * t | Inv of t

  val of_concrete : Concrete.t -> t
  val normalise : t -> t
  val identity : t
  val is_identity : t -> bool
  val invert : t -> t
  val power : t -> int -> t
  val action : t -> int -> int
  val invert_action : t -> int -> int
  val orbit : t list -> int list -> t Prelude.IntMap.t
  val of_cycles : int array list -> t
  val print : t -> string
  val print_orbit : t Prelude.IntMap.t -> string

  module Operators :
  sig 
    val ( *** ) : t -> t -> t 
    val ( ^^ ) : int -> t -> int 
  end
  
end

module Make : functor (Concrete : PermSig) -> PermType
