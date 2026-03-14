(** Strictly positive integers (> 0).

    An opaque type that eliminates non-positive inputs at construction time.
    Functions like {!Prime_factors.factorise} accept only [Positive.t]. *)

type t = int

let of_int n =
  if n > 0 then Some n
  else None

let of_int_exn n =
  if n > 0 then n
  else invalid_arg (Printf.sprintf "Positive.of_int_exn: %d is not positive" n)

let to_int t = t

let one = 1

let succ t = t + 1

let ( + ) a b = a + b
let ( * ) a b = a * b

let compare (a : int) (b : int) = Int.compare a b
let equal (a : int) (b : int) = Int.equal a b

let to_string = string_of_int
