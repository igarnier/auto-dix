module Make :
  functor (NLab : Prelude.Ordered) ->
    functor (LLab : Prelude.Ordered) ->
      sig

        val is_automorphism : (NLab.t, LLab.t) Graph.t -> Perm.ArrayBased.t -> bool

        val timer : Prelude.timer
        val cmlt  : float ref

        val compute_automorphisms :
          (NLab.t, LLab.t) Graph.t -> Perm.ArrayBased.t list
      end
