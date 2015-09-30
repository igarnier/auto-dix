module Make : functor (VL : Autotools.PrintableOrderedType) ->
              functor (EL : Autotools.PrintableOrderedType) ->
              GraphSig.S
              with type V.L.t = VL.t
               and type E.L.t = EL.t


