type 'a and_inverses = Elt of 'a | Inv of 'a

type 'a word = 'a and_inverses array

module Group :
  sig
    type 'gen t = {
      generators : 'gen array;
      relators : 'gen word array;
      subgroup : 'gen word array;
    }
    val generators : 'a t -> 'a array
    val relators : 'a t -> 'a word array
    val subgroup : 'a t -> 'a word array
  end

module ToddCoxeter :
  sig
    module Repr :
      sig
        type coset = private int
        type generator = private int
        type 'gen t = {
          width : int;
          forward : (generator, 'gen and_inverses) Hashtbl.t;
          backward : ('gen and_inverses, generator) Hashtbl.t;
          relators : generator array array;
          subgroup : generator array array;
        }
        val from_abstract : 'gen Group.t -> 'gen t
        val generator_to_abstract : 'gen t -> generator -> 'gen and_inverses
        val first_coset : coset
        val undefined_coset : coset
        val next_coset : coset -> coset
        val coset_undefined : coset -> bool
        val coset_defined : coset -> bool
        val inverse : generator -> generator
        val first_generator : generator
        val next_generator : generator -> generator
        val generator_of_int : int -> generator
        val print_coset : coset -> string
        val print_generator : generator -> string
      end
    module CosetTable :
      sig
        type ('a, 'b) disj = Inl of 'a | Inr of 'b
        type 'b tree = {
          mutable rank : int;
          mutable uplink : ('b tree, 'b) disj;
        }
        type row = Repr.coset array
        type t = (Repr.coset, row tree) Hashtbl.t
        val create : int -> ('a, 'b) Hashtbl.t
        val add : ('a, 'b tree) Hashtbl.t -> 'a -> 'b -> unit
        val find_aux : 'a tree -> 'a * 'a tree
        val find : ('a, 'b tree) Hashtbl.t -> 'a -> 'b
        val lookup :
          (Repr.coset, 'a array tree) Hashtbl.t ->
          Repr.coset -> Repr.generator -> 'a
        val mem : ('a, 'b) Hashtbl.t -> 'a -> bool
        val union : t -> Repr.coset -> Repr.coset -> unit
        val union_rows : t -> row -> row -> unit
        val roots : t -> Repr.coset list
        val create_row : int -> row
        val insert_empty_row : t -> Repr.coset -> int -> row
        val first_undefined_generator :
          row -> Repr.generator -> Repr.generator option
        val print_row : Repr.coset array -> string
        val print : t -> unit
      end
    type coset_equation = {
      mutable r : Repr.coset;
      mutable x : Repr.generator Prelude.Subarray.t;
      mutable s : Repr.coset;
    }
    type 'gen t = {
      group : 'gen Repr.t;
      coset_table : CosetTable.t;
      mutable relator_table : coset_equation list;
      mutable subgroup_table : coset_equation list;
    }
    val first_guess : int
    val create_equation :
      Repr.coset -> Repr.generator array -> Repr.coset -> coset_equation
    val add_relator_identities : 'gen t -> Repr.coset -> unit
    val initialise : 'gen Group.t -> 'gen t
    val print_word : Repr.generator Prelude.Subarray.t -> string
    val print_coset_equation : coset_equation -> string
    val print_relator_table : coset_equation list -> unit
    val print_subgroup_table : coset_equation list -> unit
    val propagate_equation :
      CosetTable.t -> Repr.coset -> Repr.generator -> Repr.coset -> unit
    val simplify_coset_equation :
      CosetTable.t -> coset_equation -> bool * bool
    val simplify_equations :
      CosetTable.t ->
      coset_equation list ->
      coset_equation list -> bool -> coset_equation list * bool
    val simplify_until_fixed_point : 'a t -> unit
    type iter_state = {
      mutable last_row : CosetTable.row;
      mutable last_coset : Repr.coset;
      mutable last_col : Repr.generator;
      mutable coset_id : Repr.coset;
      queue : (Repr.coset * CosetTable.row) Queue.t;
    }
    type outcome = Continue | Finished
    val todd_coxeter_iteration : 'gen t -> iter_state -> outcome
    val todd_coxeter_fixed_point : 'gen t -> iter_state -> unit
    val todd_coxeter : 'gen Group.t -> 'gen t * iter_state
    val abstract_representation : 'a t -> 'a and_inverses list list
  end
