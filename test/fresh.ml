type t = (* private *) int

let of_int x = x

let to_int x = x

let x = ref (0)
                 
let gen : unit -> t =
  fun () ->
  (incr x;
   of_int !x)

let status () = !x

let compare x y = x - y

let to_string = string_of_int
