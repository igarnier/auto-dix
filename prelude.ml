(* (Totally) ordered printable structure. *)
module type Ordered =
sig

  type t

  val compare : t -> t -> int

  (* Some element from the ordered set. We do not expect anything
     of this element, besides it being fixed. *)
  val inhabited : t

  val print : t -> string

end

(* function composition f;g *)
let (++) f g = fun x -> g (f x)

(* Pretty printing of lists & arrays *)
let rec to_sseq f sep l =
  match l with
  | [] -> ""
  | [x] -> f x
  | x :: tl ->
    let res = List.fold_right (fun elt acc ->
      sep ^ (f elt) ^ acc
    ) tl "" in
    (f x) ^ res

let strof_ilist = to_sseq string_of_int ","

let strof_iarr x = strof_ilist (Array.to_list x)

(* duplicate an elt i times *)
let rec dup elt i =
  if i < 0 then
    failwith "dup: count < 0"
  else if i = 0 then
    []
  else
    elt :: (dup elt (i-1))

(* list of integers from i to j *)
let rec mk_ints i j =
  if i > j then []
  else
    i :: (mk_ints (i+1) j)

(* max of a list *)
let rec list_max m = function
  | [] -> m
  | x :: l ->
    list_max (max m x) l

(* min of a list *)
let rec list_min m = function
  | [] -> m
  | x :: l ->
    list_max (min m x) l

let rec filter_duplicates' = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: y :: l ->
    if x = y then
      filter_duplicates' (y :: l)
    else
      x :: (filter_duplicates' (y :: l))

let filter_duplicates l =
  filter_duplicates' (List.sort compare l)

let union l1 l2 =
  filter_duplicates (List.rev_append l1 l2)

(* Transforms a list of words into all possible
   concatenations. I.e. take all the ordered
   sections. *)
let rec sections lists acc =
  match lists with
  | [] -> acc
  | words :: tail ->
    let res = List.fold_left (fun new_acc w ->
      List.fold_left (fun acc accw ->
        (accw @ w) :: acc
      ) new_acc acc
    ) [] words
    in
    sections tail res

let sections l = 
  sections l [[]]


(* Integer map *)

module IntMap =
struct

  include Map.Make(struct

    type t = int

    let compare x y =
      if x < y then -1
      else if x > y then 1
      else 0

  end)

  let find_opt x m =
    try Some (find x m) with
      Not_found -> None

end


  

(* Integer set - we copy the stdlib implementation to add
   is_singleton *)

module IntSet =
struct

  type elt = int
  type t = Empty | Node of t * elt * t * int

  let compare_elt (x : int) (y : int) =
      if x < y then -1
      else if x > y then 1
      else 0

    (* Sets are represented by balanced binary trees (the heights of the
       children differ by at most 2 *)

  let height = function
  Empty -> 0
    | Node(_, _, _, h) -> h

    (* Creates a new node with left son l, value v and right son r.
       We must have all elements of l < v < all elements of r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. *)

  let create l v r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced and | height l - height r | <= 3.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. *)

  let bal l v r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Set.bal"
      | Node(ll, lv, lr, _) ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else begin
          match lr with
            Empty -> invalid_arg "Set.bal"
          | Node(lrl, lrv, lrr, _)->
            create (create ll lv lrl) lrv (create lrr v r)
        end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Set.bal"
      | Node(rl, rv, rr, _) ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "Set.bal"
          | Node(rll, rlv, rlr, _) ->
            create (create l v rll) rlv (create rlr rv rr)
        end
    end else
        Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Insertion of one element *)

  let rec add x = function
  Empty -> Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t ->
      let c = compare_elt x v in
      if c = 0 then t else
        if c < 0 then bal (add x l) v r else bal l v (add x r)

  let singleton x = Node(Empty, x, Empty, 1)

    (* Beware: those two functions assume that the added v is *strictly*
       smaller (or bigger) than all the present elements in the tree; it
       does not test for equality with the current min (or max) element.
       Indeed, they are only used during the "join" operation which
       respects this precondition.
    *)

  let rec add_min_element v = function
    | Empty -> singleton v
    | Node (l, x, r, h) ->
      bal (add_min_element v l) x r

  let rec add_max_element v = function
    | Empty -> singleton v
    | Node (l, x, r, h) ->
      bal l x (add_max_element v r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

  let rec join l v r =
    match (l, r) with
      (Empty, _) -> add_min_element v r
    | (_, Empty) -> add_max_element v l
    | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
          create l v r

    (* Smallest and greatest element of a set *)

  let rec min_elt = function
  Empty -> raise Not_found
    | Node(Empty, v, r, _) -> v
    | Node(l, v, r, _) -> min_elt l

  let rec max_elt = function
  Empty -> raise Not_found
    | Node(l, v, Empty, _) -> v
    | Node(l, v, r, _) -> max_elt r

    (* Remove the smallest element of the given set *)

  let rec remove_min_elt = function
  Empty -> invalid_arg "Set.remove_min_elt"
    | Node(Empty, v, r, _) -> r
    | Node(l, v, r, _) -> bal (remove_min_elt l) v r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2. *)

  let merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

  let concat t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

    (* Splitting.  split x s returns a triple (l, present, r) where
       - l is the set of elements of s that are < x
       - r is the set of elements of s that are > x
       - present is false if s contains no element equal to x,
       or true if s contains an element equal to x. *)

  let rec split x = function
  Empty ->
    (Empty, false, Empty)
    | Node(l, v, r, _) ->
      let c = compare_elt x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split x r in (join l v lr, pres, rr)

    (* Implementation of the set operations *)

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let is_singleton =
    function (Node(Empty, _, Empty, _)) -> true
    | _ -> false

  let rec mem x = function
  Empty -> false
    | Node(l, v, r, _) ->
      let c = compare_elt x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec remove x = function
  Empty -> Empty
    | Node(l, v, r, _) ->
      let c = compare_elt x v in
      if c = 0 then merge l r else
        if c < 0 then bal (remove x l) v r else bal l v (remove x r)

  let rec union s1 s2 =
    match (s1, s2) with
      (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      if h1 >= h2 then
        if h2 = 1 then add v2 s1 else begin
          let (l2, _, r2) = split v1 s2 in
          join (union l1 l2) v1 (union r1 r2)
        end
      else
        if h1 = 1 then add v1 s2 else begin
          let (l1, _, r1) = split v2 s1 in
          join (union l1 l2) v2 (union r1 r2)
        end

  let rec inter s1 s2 =
    match (s1, s2) with
      (Empty, t2) -> Empty
    | (t1, Empty) -> Empty
    | (Node(l1, v1, r1, _), t2) ->
      match split v1 t2 with
        (l2, false, r2) ->
          concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
        join (inter l1 l2) v1 (inter r1 r2)

  let rec diff s1 s2 =
    match (s1, s2) with
      (Empty, t2) -> Empty
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, _), t2) ->
      match split v1 t2 with
        (l2, false, r2) ->
          join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
        concat (diff l1 l2) (diff r1 r2)

  type enumeration = End | More of elt * t * enumeration

  let rec cons_enum s e =
    match s with
      Empty -> e
    | Node(l, v, r, _) -> cons_enum l (More(v, r, e))

  let rec compare_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, r1, e1), More(v2, r2, e2)) ->
      let c = compare_elt v1 v2 in
      if c <> 0
      then c
      else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

  let compare s1 s2 =
    compare_aux (cons_enum s1 End) (cons_enum s2 End)

  let equal s1 s2 =
    compare s1 s2 = 0

  let rec subset s1 s2 =
    match (s1, s2) with
      Empty, _ ->
        true
    | _, Empty ->
      false
    | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
      let c = compare_elt v1 v2 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
      else
        subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2

  let rec iter f = function
  Empty -> ()
    | Node(l, v, r, _) -> iter f l; f v; iter f r

  let rec fold f s accu =
    match s with
      Empty -> accu
    | Node(l, v, r, _) -> fold f r (f v (fold f l accu))

  let rec for_all p = function
  Empty -> true
    | Node(l, v, r, _) -> p v && for_all p l && for_all p r

  let rec exists p = function
  Empty -> false
    | Node(l, v, r, _) -> p v || exists p l || exists p r

  let rec filter p = function
  Empty -> Empty
    | Node(l, v, r, _) ->
          (* call [p] in the expected left-to-right order *)
      let l' = filter p l in
      let pv = p v in
      let r' = filter p r in
      if pv then join l' v r' else concat l' r'

  let rec partition p = function
  Empty -> (Empty, Empty)
    | Node(l, v, r, _) ->
          (* call [p] in the expected left-to-right order *)
      let (lt, lf) = partition p l in
      let pv = p v in
      let (rt, rf) = partition p r in
      if pv
      then (join lt v rt, concat lf rf)
      else (concat lt rt, join lf v rf)

  let rec cardinal = function
  Empty -> 0
    | Node(l, v, r, _) -> cardinal l + 1 + cardinal r

  let rec elements_aux accu = function
  Empty -> accu
    | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

  let elements s =
    elements_aux [] s

  let choose = min_elt

  let print s =
    let l = elements s in
    let str = to_sseq string_of_int ", " l in
    Printf.sprintf "{ %s }" str

  (* let rec fold2 f s1 s2 accu = *)
  (*   match s1, s2 with *)
  (*   | Empty, Empty ->  *)
  (*     accu *)
  (*   | Node(l1, v1, r1, _), Node(l2, v2, r2, _) -> *)
  (*     fold2 f r1 r2 (f v1 v2 (fold2 f l1 l2 accu)) *)
  (*   | _ -> failwith "" *)
        
end

(* Multiset *)
module Multiset(O : Ordered) =
struct

  module M = Map.Make(O)

  type t = int M.t

  let empty = M.empty

  let count x (m : t) =
    try M.find x m
    with Not_found -> 0

  let add x (m : t) =
    M.add x (count x m + 1) m

  let fold = M.fold

  let equal (m :t) (m' : t) =
    M.equal (fun x y -> x = y) m m'

  let compare (m : t) (m' : t) =
    M.compare (fun x y -> x - y) m m'

end

(* Dynamic array - exponential reallocator *)
  
module DynArray =
struct

  type 'a t = {
    mutable fill : int;
    mutable arr  : 'a array;
    default      : 'a
  }

  (* assumes full non-empty array in order to get default element *)
  let realloc ({ fill; arr; default } as a) =
    let size     = Array.length arr in
    let new_size = size * 2 + 1 in
    let arr'     = Array.create new_size default in
    (* copy old content to new array *)
    for i = 0 to fill - 1 do
      arr'.(i) <- arr.(i)
    done;
    a.arr <- arr'

  let create sz default = { 
    fill = 0;
    arr  = Array.create sz default;
    default = default
  }

  let get_unsafe { arr } i = arr.(i)

  let get { fill; arr } i =
    if i < fill then
      arr.(i)
    else
      failwith "DynArray.get: out of bounds"

  let set { fill; arr } i x =
    if i < fill then
      arr.(i) <- x
    else
      failwith "Prelude.DynArray.set: index out of range"

  let append a elt =
    if a.fill = Array.length a.arr then
      realloc a
    else ();
    a.arr.(a.fill) <- elt;
    a.fill <- a.fill + 1

end

(* Stuff on queues *)

(* Take the first element, if any, that verifies
   a predicate. *)
let rec take_until queue pred =
  if Queue.is_empty queue then
    None
  else
    let elt = Queue.take queue in
    if pred elt then
      Some elt
    else
      take_until queue pred

(* Subarray module: accessing a contiguous subset of an array *)
module Subarray =
struct
  
  type 'a t = {
    mutable left  : int; (* inclusive *) 
    mutable right : int; (* inclusive *)
    data          : 'a array
  }

  let create sz elt = {
    left  = 0;
    right = sz - 1;
    data  = Array.create sz elt
  }

  let data { data } = data
    
  let left { left } = left

  let right { right } = right

  let is_empty a =
    a.left > a.right

  let length a = a.right - a.left + 1

  let first a = a.data.(a.left)

  let set_first a x = a.data.(a.left) <- x

  let last a  = a.data.(a.right)

  let set_last a x = a.data.(a.right) <- x

  let push_left a =
    a.left <- a.left + 1

  let pull_right a =
    a.right <- a.right - 1

  let of_array a = {
    left  = 0;
    right = Array.length a - 1;
    data  = Array.copy a
  }

  let map f a = 
    { a with data = Array.map f a.data }

end

(* Unix-based timer *)

type timer = float ref

let create_timer () = ref 0.0

let start_timer timer = timer := (Unix.gettimeofday ())

let reset_timer timer = timer := 0.0

let get_timer timer   = 
  let t = Unix.gettimeofday () in
  t -. !timer


(* Logging *)

let log x = Printf.printf "log: %s\n%!" x
