(** Transitive subgroups of S_n.

    Stub: provides the interface for Galois group identification.
    Full implementation would enumerate transitive subgroups
    up to conjugacy for small n. *)

type t = {
  degree : int;
  generators : Permutation.t list;
  order : int;
}

(** Symmetric group S_n. *)
let symmetric n =
  let transposition i j =
    Array.init n (fun k ->
      if k = i then j
      else if k = j then i
      else k)
  in
  let gens = if n <= 1 then []
    else [
      transposition 0 1;
      Array.init n (fun i -> (i + 1) mod n)
    ] in
  let rec factorial = function
    | 0 | 1 -> 1
    | n -> n * factorial (n - 1)
  in
  { degree = n; generators = gens; order = factorial n }
