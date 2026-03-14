(** Permutation group operations.

    Permutations of {0, ..., n-1} represented as arrays.
    Used in Galois group identification. *)

type t = int array

let identity n = Array.init n Fun.id

let compose a b =
  let n = Array.length a in
  Array.init n (fun i -> a.(b.(i)))

let inverse p =
  let n = Array.length p in
  let inv = Array.make n 0 in
  Array.iteri (fun i v -> inv.(v) <- i) p;
  inv

let apply p i = p.(i)

let order p =
  let id = identity (Array.length p) in
  let rec go current k =
    if current = id then k
    else go (compose p current) (k + 1)
  in
  go p 1

let equal a b = a = b

let to_string p =
  let n = Array.length p in
  let parts = List.init n (fun i -> string_of_int p.(i)) in
  "(" ^ String.concat " " parts ^ ")"
