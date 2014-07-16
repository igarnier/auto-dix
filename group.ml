open Printf

open Prelude

(* Todd-Coxeter algorithm *)

(* The set of generators in a presentation does not contain inverses: they need to be added
 * by hand. We use a disjoint union of two copies of the generators as the alphabet of relators. *)
type 'a and_inverses =
| Elt of 'a
| Inv of 'a

type 'a word = 'a and_inverses array

module Group =
struct

  (* Finitely presented group, as given by the user of the module. *)
  type 'gen t = {
    generators : 'gen array;
    relators   : 'gen word array;
    subgroup   : 'gen word array
  }

  let generators { generators } = generators
  let relators { relators }     = relators
  let subgroup { subgroup }     = subgroup

end

module ToddCoxeter =
struct


  (* ------------------------------------------------------------------ *)
  (* Internal representation                                            *)
  (* The concrete cosets are integer codes. To each abstract generator
   * is associated its concrete counterpart, as another integer code,
   * as well as its /inverse/. If g is a concrete code for an abstract
   * generator, then g+1 is the code for its inverse. All in all,
   * concrete generators are the set of integers [0; 2N[ where N is
   * the number of abstract generators.
   * We give specific, restricted types to these codes in order to prevent bugs.
   *)
  (* ------------------------------------------------------------------ *)

  module Repr :
  sig

    type coset     = private int

    type generator = private int

    type 'gen t = {
      width      : int;
      forward    : (generator, 'gen and_inverses) Hashtbl.t;
      backward   : ('gen and_inverses, generator) Hashtbl.t;
      (*generators : generator array; *) (* superfluous ? TODO *)
      relators   : generator array array;
      subgroup   : generator array array;
    }

    val from_abstract : 'gen Group.t -> 'gen t

    val generator_to_abstract : 'gen t -> generator -> 'gen and_inverses

    (* val to_abstract   : t -> 'gen Group.t *)

    val first_coset : coset

    val undefined_coset : coset

    val next_coset : coset -> coset

    val coset_undefined : coset -> bool

    val coset_defined : coset -> bool

    val inverse : generator -> generator

    val first_generator : generator (* = 0 *)

    val next_generator : generator -> generator

    val generator_of_int : int -> generator

    val print_coset : coset -> string

    val print_generator : generator -> string

  end

    =
  struct

    type coset     = int
    type generator = int

    (* Internal representation of a finite presentation, along with 
       the data required to go back to the abstract one. *)
    type 'gen t = {
      width      : int;
      forward    : (generator, 'gen and_inverses) Hashtbl.t;
      backward   : ('gen and_inverses, generator) Hashtbl.t;
      (*generators : generator array; *) (* superfluous ? TODO *)
      relators   : generator array array;
      subgroup   : generator array array;
    }

    let from_abstract (group : 'gen Group.t) =
      let abs_gen   = Group.generators group in
      let gen_count = Array.length abs_gen in
      let forward   = Hashtbl.create (gen_count * 2) in
      let backward  = Hashtbl.create (gen_count * 2) in
      let c = ref 0 in
      (* Initialise translatin tables *)
      for i = 0 to gen_count - 1 do
        Hashtbl.add forward !c (Elt abs_gen.(i));
        Hashtbl.add backward (Elt abs_gen.(i)) !c;
        incr c;
        Hashtbl.add forward !c (Inv abs_gen.(i));
        Hashtbl.add backward (Inv abs_gen.(i)) !c;
        incr c
      done;
      (* Initialise concrete generators - TODO this is just for type safety and incurs an indirection for nothing *)
      (*let generators = Array.init (gen_count * 2) (fun i -> i)  in*)
      let width = gen_count * 2 in
      let relators   = Array.map ((Array.map (Hashtbl.find backward))) (Group.relators group) in
      let subgroup   = Array.map ((Array.map (Hashtbl.find backward))) (Group.subgroup group) in
      { width; forward; backward; (*generators;*) relators; subgroup }

    let generator_to_abstract (group : 'gen t) g =
      Hashtbl.find group.forward g
        
    (* Initial coset, needed by Todd-Coxeter in init *)
    let first_coset : coset   = 0

    let undefined_coset : coset = -1

    let next_coset x = x + 1

    let inverse x =
      if x mod 2 = 0 then x+1
      else x-1
        
    let first_generator = 0

    let next_generator x = x + 1

    let coset_undefined x = x < 0

    let coset_defined x = x >= 0

    let generator_of_int x = x

    let print_coset : coset -> string = string_of_int

(*    let print_generator_aux =
      function 
      | Inv e -> sprintf "%s^-" (GmodH.print e)
      | Elt e -> GmodH.print e *)

    let print_generator : generator -> string = string_of_int
(*      fun g ->
        print_generator_aux (to_abstract g) *)

  end (* Repr *)

  (* ------------------------------------------------------------------ *)
  (* Table of cosets, with unification built-in                         *)
  (* ------------------------------------------------------------------ *)

  module CosetTable =
  struct

    (* Imperative, non-persistent union-find for integers, built on
     * top of a Hashtable. *)

    type ('a, 'b) disj =
    | Inl of 'a
    | Inr of 'b

    type 'b tree = {
      mutable rank   : int;
      mutable uplink : ('b tree, 'b) disj
    }

    type row = Repr.coset array

    type t = (Repr.coset, row tree) Hashtbl.t

    let create sz = Hashtbl.create sz

    let add table key elt =
      Hashtbl.add table key { rank = 0; uplink = Inr elt }

    (* TODO: we can dispense with propagating the root data and
     * access it through the returned root itself. *)
    let rec find_aux node =
      match node.uplink with
      | Inl t -> 
        let ((_, up) as res) = find_aux t in
        node.uplink <- Inl up;
        res
      | Inr d ->
        (d, node)

    let find table key =
      let node = Hashtbl.find table key in
      fst (find_aux node)

    let lookup table (e : Repr.coset) (g : Repr.generator) =
      try (find table e).((g :> int))
      with
      | Not_found -> failwith "Group.ToddCoxeter.lookup: coset undefined"

    let mem table key =
      Hashtbl.mem table key

    (* We add a further invariant to [union] compared to standard
     * union-find: we conserve the data indexed by the minimal keys
     *)
    let rec union (table : t) key1 key2 =
      let (key1, key2) =
        if key1 > key2 then (key2, key1) else (key1, key2)
      in
      let (row1, root1) = find_aux (Hashtbl.find table key1) in
      let (row2, root2) = find_aux (Hashtbl.find table key2) in
      (* update tree *)
      begin
        if root1 == root2 then (* we're done *)          
          ()
        else if root1.rank < root2.rank then
          (root1.uplink <- Inl root2;
           root2.uplink <- Inr row1)
        else if root2.rank < root1.rank then
          root2.uplink <- Inl root1
        else
          (root2.uplink <- Inl root1;
           root1.rank <- root1.rank + 1)
      end;
      (* unify conflicting data, updating row1 when undefined *)
      union_rows table row1 row2

    and union_rows table row1 row2 =
      for i = 0 to Array.length row1 - 1 do
        let a = Repr.coset_undefined row1.(i)
        and b = Repr.coset_undefined row2.(i) in

        if a && not b then
          row1.(i) <- row2.(i)
        else if not a && b then
          row2.(i) <- row1.(i)
        else if not a && not b && row1.(i) <> row2.(i) then (* the inequality test here would deserve some abstraction *)
          union table row1.(i) row2.(i)
      done

    let roots (table : t) =
      Hashtbl.fold (fun coset elt acc ->
        match elt.uplink with
        | Inl _ -> acc
        | Inr _ -> coset :: acc
      ) table []

    (* ------------------------------------------------------------------ *)
    (* Populating the table with rows *)

    let create_row : int -> row =
      fun sz -> Array.make sz Repr.undefined_coset

    let insert_empty_row (table : t) (coset_index : Repr.coset) (width : int) =
      if mem table coset_index then
        failwith "Group.ToddCoxeter.table_alloc_row: row already allocated"
      else
        let a = create_row width in
        add table coset_index a;
        a

    (* ------------------------------------------------------------------ *)
    (* First undefined generator *)

    let rec first_undefined_generator (row : row) (g : Repr.generator) =
      if (g :> int) >= Array.length row then
        None
      else
        if Repr.coset_undefined row.((g :> int)) then 
          Some g
        else first_undefined_generator row (Repr.next_generator g)


    (* ------------------------------------------------------------------ *)
    (* printing stuff *)

    let print_row row =
      let acc = ref "" in
      for i = 0 to Array.length row - 1 do
        acc := !acc^(sprintf "%s " (Repr.print_coset row.(i)))
      done;
      !acc

    let print (table : t) =
      let l = ref [] in
      Hashtbl.iter (fun coset row ->
        let row = fst (find_aux row) in
        let s =
          sprintf "%s |-> %s\n%!" (Repr.print_coset coset) (print_row row)
        in
        l := (coset, s) :: !l
      ) table;
      let l = List.sort compare !l in
      List.iter (fun (_, s) -> print_string s) l

  end (* CosetTable *)

  (* ------------------------------------------------------------------ *)
  (* Todd-Coxeter *)


  (* Equation relating cosets (edges in the Cayley graph) *)
  (* r x = s -> r = s x^- *)
  type coset_equation = {
    mutable r : Repr.coset;
    mutable x : Repr.generator Subarray.t;
    mutable s : Repr.coset
  }

  (* Tables *)
  type 'gen t = {
    group          : 'gen Repr.t; (* Internal representation of the finetely presented group *)
    coset_table    : CosetTable.t;
    mutable relator_table  : coset_equation list;
    mutable subgroup_table : coset_equation list
  }

  (* A guess at how many cosets there will be *)
  let first_guess = 100

  (* allocate a fresh (coset or subgroup) equation *)
  let create_equation r word s =
    { r; x = Subarray.of_array word; s }

  (* populate relator tables with fresh coset identities *)
  let add_relator_identities (state : 'gen t) (coset : Repr.coset) =
    for i = 0 to Array.length state.group.Repr.relators - 1 do
      let relator_code = create_equation coset state.group.Repr.relators.(i) coset in
      state.relator_table <- relator_code :: state.relator_table
    done

  (* Initialise tables for Todd-Coxeter *)
  let initialise (presentation : 'gen Group.t) =
    let group = Repr.from_abstract presentation in
    let coset_table = CosetTable.create first_guess in
    let subgroup_table =
      (* Create one equation per generator word of the subgroup *)
      let subgroup = group.Repr.subgroup in
      let acc = ref [] in
      for i = 0 to Array.length subgroup - 1 do
        let subgroup_code = create_equation Repr.first_coset subgroup.(i) Repr.first_coset in
        acc := subgroup_code :: !acc
      done;
      !acc
    in
    { 
      group;
      coset_table;
      relator_table  = [];
      subgroup_table
    }

  (* ------------------------------------------------------------------ *)
  (* Equation & unification related stuff *)

  let print_word (w : Repr.generator Subarray.t) =
    let acc = ref "" in
    let data = Subarray.data w in
    for i = Subarray.left w to Subarray.right w do
      acc := !acc^"."^(Repr.print_generator data.(i))
    done;
    !acc

  let print_coset_equation { r; x; s } =
    sprintf "%s * %s = %s" 
      (Repr.print_coset r)
      (print_word x)
      (Repr.print_coset s)

  let print_relator_table relator_table =
    List.iter (fun x -> printf "%s\n" (print_coset_equation x)) relator_table

  let print_subgroup_table subgroup_table =
    List.iter (fun x -> printf "%s\n" (print_coset_equation x)) subgroup_table

  let rec propagate_equation coset_table coset_id (generator_id : Repr.generator) result_coset =
    (* invariant: coset_table.(coset_id).(generator_id) is undefined
       and can be safely set. *)
    let row = CosetTable.find coset_table coset_id in
    let target_coset = row.((generator_id :> int)) in
    (if Repr.coset_undefined target_coset then
      let _ = printf "assign in row %s col %s to %s\n%!" 
        (Repr.print_coset coset_id) 
        (Repr.print_generator generator_id)
        (Repr.print_coset result_coset)
      in
      row.((generator_id :> int)) <- result_coset
    else
      let _ = printf "unifying image of %s times %s = %s to %s\n%!" 
        (Repr.print_coset coset_id)
        (Repr.print_generator generator_id)
        (Repr.print_coset target_coset)
        (Repr.print_coset result_coset)
      in      
      CosetTable.union coset_table target_coset result_coset);
    (* Conjugate equation *)
    let row = CosetTable.find coset_table result_coset in
    let inv = Repr.inverse generator_id in
    let target_coset = row.((inv :> int)) in
    if Repr.coset_undefined target_coset then
      let _ = printf "assign in row %s col %s to %s\n%!" 
        (Repr.print_coset result_coset) 
        (Repr.print_generator inv)
        (Repr.print_coset coset_id)
      in
      row.((inv :> int)) <- coset_id
    else
      let _ = printf "unifying image of %s times %s = %s to %s\n%!" 
        (Repr.print_coset result_coset)
        (Repr.print_generator inv)
        (Repr.print_coset target_coset)
        (Repr.print_coset coset_id)
      in
      CosetTable.union coset_table target_coset coset_id


  let simplify_coset_equation coset_table (eq : coset_equation) =
    let _ = printf "simplify coset eqn\n" in
    let _ = CosetTable.print coset_table in
    let continue = ref true in
    let minimal  = ref false in
    let change   = ref false in
    while !continue do
      Printf.printf "iterating with equation %s\n" (print_coset_equation eq);
      continue := false;
      let lft = Subarray.left eq.x
      and rgt = Subarray.right eq.x in
      if lft = rgt then
        minimal := true
      else begin
        let front = Subarray.first eq.x in
        let _ = printf "fwd looking up %s * %s ..." (Repr.print_coset eq.r) (Repr.print_generator front) in
        let image_coset = CosetTable.lookup coset_table eq.r front in
        if Repr.coset_undefined image_coset then
          (printf "fail\n"; ())
        else begin
          printf "success: %s\n" (Repr.print_coset image_coset);
          Subarray.push_left eq.x;
          eq.r <- image_coset;
          continue := true;
          change   := true
        end
      end;
      let lft' = Subarray.left eq.x in        
      if lft' = rgt then
        minimal := true
      else begin
        let back = Subarray.last eq.x in
        let _ = printf "bwd looking up %s * %s ..." (Repr.print_coset eq.s) (Repr.print_generator (Repr.inverse back)) in
        let image_coset = CosetTable.lookup coset_table eq.s (Repr.inverse back) in
        if Repr.coset_undefined image_coset then
          (printf "fail\n"; ())
        else
          (printf "success: %s\n" (Repr.print_coset image_coset);
           Subarray.pull_right eq.x;
           eq.s <- image_coset;
           continue := true;
           change   := true
          )
        end;
    done;
    Printf.printf "  %s\n" (print_coset_equation eq);
    (!minimal, !change)

  (* PRECONDITION: in [equations], no equation is a deduction. 
     POSTCONDITION: in [eq_acc], no equation is a deduction. eq_acc = equations - deductions.
  *)
  let rec simplify_equations 
      (coset_table : CosetTable.t)
      (equations : coset_equation list)  (* list of equations to simplify *)
      (eq_acc : coset_equation list)     (* accumulator of equations not yet transformed into deductions *)
      (change_flag : bool)               (* boolean flag to check whether we reached a fixed point *)
      =
    match equations with
    | [] -> (eq_acc, change_flag)
    | coset_equation :: tail ->
      let (minimal, change) = simplify_coset_equation coset_table coset_equation in
      if minimal then
        (propagate_equation coset_table coset_equation.r (Subarray.first coset_equation.x) coset_equation.s;
         simplify_equations coset_table tail eq_acc true)
      else
        simplify_equations coset_table tail (coset_equation :: eq_acc) (change || change_flag)


  let simplify_until_fixed_point state =
    let _ = printf "simplify fixpoint iteration\n" in
    let continue = ref true in
    while !continue do
      print_string "coset table:\n";
      CosetTable.print state.coset_table;
      print_string "relators:\n";
      print_relator_table state.relator_table;
      print_string "subgroup:\n";
      print_subgroup_table state.subgroup_table;

      let (rel, rchange) = simplify_equations state.coset_table state.relator_table [] false in
      let (sub, schange) = simplify_equations state.coset_table state.subgroup_table [] false in
      state.relator_table <- rel;
      state.subgroup_table <- sub;
      continue := rchange || schange
    done;
    print_string "coset table:\n";
    CosetTable.print state.coset_table;
    print_string "relators:\n";
    print_relator_table state.relator_table;
    print_string "subgroup:\n";
    print_subgroup_table state.subgroup_table


  (* State of an iteration of the Todd-Coxeter algorithm, for fast resuming. Stores the
   * coordinates of the first element in the coset table that is undefined.
   * Also stores a queue of all the cosets left to be treated.
   *)
  type iter_state = {
    mutable last_row   : CosetTable.row; (* keep the row around to avoid a lookup *)
    mutable last_coset : Repr.coset;
    mutable last_col   : Repr.generator;
    mutable coset_id   : Repr.coset;
    queue              : (Repr.coset * CosetTable.row) Queue.t
  }

  type outcome =
  | Continue
  | Finished

  let todd_coxeter_iteration (state : 'gen t) (iter_state : iter_state) =
    (* invariant: there is a hole somewhere in the current row. *)
    let { last_row; last_coset; last_col; queue } = iter_state in
    iter_state.coset_id <- Repr.next_coset iter_state.coset_id;
    let coset_id = iter_state.coset_id in
    (* create fresh coset *)    
    let row = CosetTable.insert_empty_row state.coset_table coset_id (state.group.Repr.width) in
    (* add equations *)
    add_relator_identities state coset_id;
    (* add fresh row to work queue *)
    Queue.add (coset_id, row) queue;
    (* simplify store *)
    propagate_equation state.coset_table last_coset last_col coset_id;
    simplify_until_fixed_point state;
    (* check termintation or solve next problem *)
    begin match CosetTable.first_undefined_generator last_row last_col with
    | Some col ->
      (* go to next column in same row *)
      iter_state.last_col <- col;
      Continue
    | None -> 
      (* go to next row *)
      let next_row =
        Prelude.take_until queue (fun (coset, row) -> 
          let acc = ref false in 
          Array.iter (fun cos -> acc := !acc || Repr.coset_undefined cos) row;
          !acc
        )
      in
      (match next_row with
      | None ->              (* No holes. We're finished. *)
        Finished
      | Some (coset, row) -> (* Next row with holes *)
        begin
          iter_state.last_coset <- coset;
          iter_state.last_row   <- row;
          match CosetTable.first_undefined_generator row Repr.first_generator with
          | None     -> failwith "Group.todd_coxeter_iteration: bug found"
          | Some col ->
            (iter_state.last_col <- col;
             Continue)
        end)
    end
      
  let rec todd_coxeter_fixed_point (state : 'gen t) (iter_state : iter_state) =
    match todd_coxeter_iteration state iter_state with
    | Continue -> todd_coxeter_fixed_point state iter_state
    | Finished -> ()
    
  let todd_coxeter (presentation : 'gen Group.t) =
    let state   = initialise presentation in
    let row     = CosetTable.insert_empty_row state.coset_table Repr.first_coset state.group.Repr.width in
    add_relator_identities state Repr.first_coset;
    let initial_state = {
      last_row   = row;
      last_coset = Repr.first_coset;
      last_col   = Repr.first_generator;
      coset_id   = Repr.first_coset;
      queue      = Queue.create ()
    } in
    todd_coxeter_fixed_point state initial_state;
    (state, initial_state)

  (* Assumes the table is complete. The Cayley graph is connected, so we can simply do
   * an exhaustive search, marking each coset as it is explored. This is basically
   * a spanning tree construction for a Schreier coset graph. *)
  let abstract_representation { group; coset_table; relator_table; subgroup_table } =
    let first_row = CosetTable.find coset_table Repr.first_coset in
    let order     = Hashtbl.length coset_table in
    let table     = Hashtbl.create (order * 2) in
    let queue     = Queue.create () in
    Hashtbl.add table Repr.first_coset [];
    for i = 0 to Array.length first_row - 1 do
      let word = [Repr.generator_of_int i] in
      Hashtbl.add table first_row.(i) word;
      Queue.add (first_row.(i), word) queue;
    done;
    while not (Queue.is_empty queue) do
      let (coset, word) = Queue.take queue in
      let row           = CosetTable.find coset_table coset in
      for i = 0 to Array.length row - 1 do
        if Hashtbl.mem table row.(i) then 
          ()
        else
          (let word = (Repr.generator_of_int i) :: word in
           Hashtbl.add table row.(i) word;
           Queue.add (row.(i), word) queue)
      done
    done;
    Hashtbl.fold (fun coset word acc ->
      let word = List.fold_left (fun acc elt -> (Repr.generator_to_abstract group elt) :: acc) [] word in
      word :: acc
    ) table []

end (* ToddCoxeter *)
