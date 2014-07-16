open Prelude

(* module Make(Concrete : Perm.PermSig) = *)
(* struct *)

(*   (\* perm words *\) *)
(*   module Perm = Perm.Make(Perm.CycleBased) *)

(*   (\* ----------------------- *\) *)
(*   (\* Schreier-sims algorithm *\) *)

(*   (\* Record for the stabilizer subgroup of [elt] *\) *)
(*   type subgroup = { *)
(*     elt         : int;            (\* elt is stabilised by the subgroup *\) *)
(*     generators  : Perm.t list;    (\* Generators of G^[i] *\) *)
(*     transversal : Perm.t IntMap.t (\* the orbit is the domain of the map, the codomain are the coset repr. *\) *)
(*   } *)

(*   type group = { *)
(*     base      : int list; *)
(*     chain     : subgroup list *)
(*   } *)
    
(*   (\* A partial base and strong generating set (bsgs) is *)
(*      a set of generators containing the original set of *)
(*      generators of the group and s.t. no point of the *)
(*      base is fixed. So given a base and a preliminary *)
(*      set of elements (pre_bsgs), we want to extend it to a *)
(*      partial bsgs by adding points to the base, and possibly *)
(*      by closing [pre_bsgs]. *)

(*      Note: in the following function, we assume that [pre_bsgs] *)
(*      already contains the generators for the group. *)
(*   *\) *)

(*   let partial_bsgs_aux *)
(*       (pre_bsgs : Concrete.t list) *)
(*       (base : int list) = *)
(*     List.fold_left (fun (base, pre_bsgs) perm  -> *)
(*       (\* Close pre_bsgs by inversion, if needed *\) *)
(*       let pre_bsgs = *)
(*         if Concrete.prod perm perm <> Concrete.identity then *)
(*           (Concrete.inv perm) :: pre_bsgs *)
(*         else pre_bsgs *)
(*       in *)
(*       let base' = List.map (Concrete.action perm) base in *)
(*       if base = base' then *)
(*         match Concrete.pick_from_support perm with *)
(*         | None -> failwith "Bsgs.partial_bsgs: pre_bsgs contains the identity perm." *)
(*         | Some pt -> *)
(*           (pt :: base, pre_bsgs) *)
(*       else *)
(*         (base, pre_bsgs) *)
(*     ) (base, pre_bsgs) pre_bsgs *)

(*   let partial_bsgs (generators : Concrete.t list) = partial_bsgs_aux generators [] *)
  
(*   (\* Sifting, i.e. testing an element for inclusion in a group with *)
(*      strong generators *\) *)

(*   let rec sift_aux base_im chain = *)
(*     match base_im, chain with *)
(*     | [], [] -> true *)
(*     | [], _ *)
(*     | _, [] -> failwith "inconsistent chain" *)
(*     | x :: base', subgroup :: chain' -> *)
(*       if IntMap.mem x subgroup.transversal then *)
(*         sift_aux base' chain' *)
(*       else *)
(*         false *)

(*   let sift group perm = *)
(*     let base' = List.map (Perm.action perm) group.base in *)
(*     sift_aux base' group.chain *)

(*   (\* Schreier-Sims algorithm (from scratch) *\) *)

(*   exception EarlyExit of int *)

(*   (\* A nice invariant of our repesentation of permutations is that identity *)
(*      mappings are not stored. Hence any binding will do. *\) *)
(*   (\* let rec find_point (generators : Perm.t list) = *\) *)
(*   (\*   match generators with *\) *)
(*   (\*   | [] -> None *\) *)
(*   (\*   | (Perm.Perm g) :: gens -> *\) *)
(*   (\*     match Perm.Concrete.pick_from_support g.Perm.p with *\) *)
(*   (\*     | None -> *\) *)
(*   (\*       find_point gens *\) *)
(*   (\*     | Some () *\) *)
(*   (\*       Some (fst (IntMap.choose g.Perm.p)) *\) *)
(*   (\*   | _ -> failwith "Group.find_point: generators not in normal form" *\) *)

(*   (\* open Perm.Operators *\) *)

(*   (\* It is assumed that the generators are normalised. *\) *)
(*   (\* let rec schreier_sims_aux (generators : Perm.t list) acc = *\) *)
(*   (\*   (\\* Find a point that is not fixed by a generator *\\) *\) *)
(*   (\*   match find_point generators with *\) *)
(*   (\*   | None -> (\\* trivial group - end recursion *\\) *\) *)
(*   (\*     let points, subgroups = List.split acc in *\) *)
(*   (\*     { base  = List.rev points; *\) *)
(*   (\*       chain = List.rev subgroups } *\) *)
(*   (\*   | Some point -> *\) *)
(*   (\*     let orb  = Perm.orbit generators [point] in *\) *)
(*   (\*     let gens = (\\* Schreier generators for isotropy subgroup *\\) *\) *)
(*   (\*       IntMap.fold (fun point u acc -> *\) *)
(*   (\*         List.fold_left (fun acc g -> *\) *)
(*   (\*           let ug    = u *** g in *\) *)
(*   (\*           let ug_tr = *\) *)
(*   (\*             let img = Perm.action ug point in *\) *)
(*   (\*             try IntMap.find img orb with *\) *)
(*   (\*             | Not_found -> failwith "Group.schreier_sims: bug found, could not find transversal" *\) *)
(*   (\*           in *\) *)
(*   (\*           (\\* TODO: normalise these ... OR normalise as needed in [find_point] *\\) *\) *)
(*   (\*           (ug *** (Perm.Inv ug_tr)) :: acc *\) *)
(*   (\*         ) acc generators *\) *)
(*   (\*       ) orb [] in *\) *)
(*   (\*     let subgroup = { generators = gens;  transversal = orb } in *\) *)
(*   (\*     schreier_sims_aux gens ((point, subgroup) :: acc) *\) *)

(*   (\* let schreier_sims generators = schreier_sims_aux generators [] *\) *)

(* end *)



module Make(Concrete : Perm.PermSig) =
struct

  (* perm words *)
  module Perm = Perm.Make(Concrete)

  (* ----------------------- *)
  (* Schreier-sims algorithm *)

  (* Record for the stabilizer subgroup of [elt] *)
  type subgroup = {
            elt         : int;            (* elt is stabilised by the subgroup *)
    mutable generators  : Perm.t list;    (* Generators of G^[i] *)
    mutable transversal : Perm.t IntMap.t (* the orbit is the domain of the map, the codomain are the coset repr. *)
  }

  (* Invariant : chain.(0) is the whole group, chain.(0).elt and 
   * chain.(0).transversal are meaningless and set to default values *)
  type group = {
    base      : int array;
    chain     : subgroup array
  }

  type sift_outcome =
  | Ok of Perm.t list          (* multiplying the corresponding representatives in order yields the group element *)
  | DropOut of int * Perm.t (* DropOut(i, residue) -> residue is not in subgroup of index $i$ *)

  open Perm.Operators

  let print_subgroup { elt; generators; transversal } =
    let gens = Prelude.to_sseq Perm.print ", " generators in
    Printf.sprintf "< %s >" gens
    
  let print { base; chain } =
    let base = strof_ilist (Array.to_list base) in
    let subs = Prelude.to_sseq print_subgroup " " (Array.to_list chain) in
    Printf.sprintf "base = %s; chain = %s" base subs

  (* decompose perm against subgroup chain, return either decomposition or residue *)
  let rec strip_aux chain perm i acc =
    if i = Array.length chain then
      (if Perm.is_identity perm then
          Ok acc
       else
          DropOut(i, perm))
    else
      let { elt; transversal } = chain.(i) in
      let im = Perm.action perm elt in
      match IntMap.find_opt im transversal with
      | None ->
        DropOut(i, perm)
      | Some repr ->
        strip_aux chain (perm *** (Perm.invert repr)) (i+1) (repr :: acc)

  let strip chain index perm = strip_aux chain perm index []

  (* A partial base and strong generating set (bsgs) is
     a set of generators containing the original set of
     generators of the group and s.t. no point of the
     base is fixed. So given a base and a preliminary
     set of elements (pre_bsgs), we want to extend it to a
     partial bsgs by adding points to the base, and
     by closing [pre_bsgs] with inverses.

     Note: in the following function, we assume that [pre_bsgs]
     already contains the generators for the group as well as
     their inverses, but /not/ the identity. *)

  let rec base_is_stable perm base =
    match base with
    | [] -> true
    | b :: base' ->
      (Perm.Concrete.action perm b <> b)
      || (base_is_stable perm base')

  let partial_bsgs_aux
      (pre_bsgs : Perm.Concrete.t list)
      (base : int list)  (* We assume that [base] is given in reverse order *)
      =
    List.fold_right (fun perm (base, pre_bsgs) ->
      let pre_bsgs =
        let p = Perm.Concrete.prod perm perm in
        if p <> Perm.Concrete.identity then
          (Perm.Concrete.inv perm) :: pre_bsgs
        else pre_bsgs
      in
      if base_is_stable perm base then
        match Perm.Concrete.pick_from_support perm with
        | None -> failwith "Bsgs.partial_bsgs: pre_bsgs contains the identity perm."
        | Some pt ->
          (pt :: base, pre_bsgs)
      else
        (base, pre_bsgs)
    ) pre_bsgs (base, pre_bsgs)

  let partial_bsgs generators = 
    let (base, generators) = partial_bsgs_aux generators [] in
    (base, List.map Perm.of_concrete generators)

  (* let stabilizes_up_to base index gen = *)
  (*   let acc = ref true in *)
  (*   for i = 0 to index - 1 do *)
  (*     acc := !acc && (Concrete.action gen base.(i) = base.(i)) *)
  (*   done; *)
  (*   !acc *)

  let orbit group elt =
    let stabilizers = List.filter (fun g -> Perm.action g elt = elt) group in
    let stabilizers =
      match stabilizers with
      | [] -> [Perm.identity]
      | _  -> stabilizers
    in
    let transversal = Perm.orbit group [elt] in
    (stabilizers, transversal)
      
  let compute_partial_subgroup_chain generators =
    let (base, generators) = partial_bsgs generators in
    let base  = Array.of_list base in
    let len   = 1 + (Array.length base) in
    let chain = Array.make len { elt = 0; generators = []; transversal = IntMap.empty } in
    chain.(0) <- { elt = 0; generators; transversal = IntMap.empty };
    for i = 1 to len - 1 do
      chain.(i) <- { elt = base.(i-1); generators = []; transversal = IntMap.empty };
    done;
    let acc = ref generators in
    for i = 0 to Array.length base - 1 do
      let (stabilizers, transversal) = orbit !acc base.(i) in
      chain.(i+1).generators  <- stabilizers;
      chain.(i+1).transversal <- transversal;
      acc := stabilizers
    done;
    { base; chain }

  let repr point transversal elt =
    match IntMap.find_opt (Perm.action elt point) transversal with
    | None -> failwith "Bsgs.repr: bug found"
    | Some x -> x
    

  let schreier_generators point transversal generators =
    IntMap.fold (fun _ u acc ->
      List.fold_left (fun acc s ->
        let prod = u *** s in
        (prod *** (Perm.invert (repr point transversal prod))) :: acc
      ) acc generators
    ) transversal []

  (* Standard Schreier-Sims.
   * We have a base \beta_0 ... \beta_{k-1}
   * and subgroups  G=H0 ... H_{k} = <e>
   * Invariant: for all j in [i+1; k-1] we have that
   * H_{j+1} = Stab(H_{j}, \beta_j).
   * In particular, H_{i+2} = Stab(H_{i+1}, \beta_{i+1}).
   * We want to extend this to H_{i+1} = Stab(H_i, \beta_i).
   * In order to do that, we push all schreier generators into H_{i+1},
   * making sure to add only the nonredundant ones.
   *)
  let schreier_sims_aux chain i =
    let { elt; generators; transversal } = chain.(i+1) in
    let generators = schreier_generators elt transversal generators in
    IntMap.iter (fun key perm ->
      log (Printf.sprintf "for base elt %d, orbit %d %s" elt key (Perm.print perm))
    ) transversal;
    List.iter (fun schreier ->
      match strip chain (i+1) schreier with
      | Ok w ->
        let w = Prelude.to_sseq Perm.print "." w in 
        log (Printf.sprintf "for gen %s, decomposition %s" (Perm.print schreier) w)
      | DropOut(i, residue) ->
        log (Printf.sprintf "for gen %s, residue %s at %d" (Perm.print schreier) (Perm.print residue) i)
    ) generators;
    let _ = List.iter (fun x -> Prelude.log (Perm.print x)) generators in
    ()


  let schreier_sims generators =
    let group = compute_partial_subgroup_chain generators in
    (*let gens  = ref generators in*)
    for i = Array.length group.base - 1 downto 0 do
      schreier_sims_aux group.chain i
    done;
    group

end
