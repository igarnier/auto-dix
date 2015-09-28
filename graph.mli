module Make : functor (VL : Aux.PrintableOrderedType) ->
              functor (EL : Aux.PrintableOrderedType) ->
              GraphSig.S
              with type V.L.t = VL.t
               and type E.L.t = EL.t


