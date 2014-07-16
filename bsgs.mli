module Make :
  functor (Concrete : Perm.PermSig) ->
    sig
      module Perm : Perm.PermType

      type subgroup = {
        elt : int;
        mutable generators : Perm.t list;
        mutable transversal : Perm.t Prelude.IntMap.t;
      }

      type group = { base : int array; chain : subgroup array; }
      type sift_outcome = Ok of Perm.t list | DropOut of int * Perm.t
      val print_subgroup : subgroup -> string
      val print : group -> string
      val strip_aux :
        subgroup array -> Perm.t -> int -> Perm.t list -> sift_outcome
      val strip : subgroup array -> int -> Perm.t -> sift_outcome
      val base_is_stable : Perm.Concrete.t -> int list -> bool
      val partial_bsgs_aux :
        Perm.Concrete.t list -> int list -> int list * Perm.Concrete.t list
      val partial_bsgs : Perm.Concrete.t list -> int list * Perm.t list
      val orbit : Perm.t list -> int -> Perm.t list * Perm.t Prelude.IntMap.t
      val compute_partial_subgroup_chain : Perm.Concrete.t list -> group
      val repr : int -> 'a Prelude.IntMap.t -> Perm.t -> 'a
      val schreier_generators :
        int -> Perm.t Prelude.IntMap.t -> Perm.t list -> Perm.t list
      val schreier_sims_aux : subgroup array -> int -> unit
      val schreier_sims : Perm.Concrete.t list -> group
    end
