(** Galois group identification.

    Stub: given a polynomial, identify its Galois group
    as a transitive subgroup of S_n. *)

module P = Poly.RatPoly

type galois_group =
  | Cyclic of int        (** Z/nZ *)
  | Dihedral of int      (** D_n *)
  | Symmetric of int     (** S_n *)
  | Alternating of int   (** A_n *)
  | Frobenius of int     (** F_n (Frobenius group) *)
  | Unknown

(** Identify the Galois group of an irreducible polynomial over Q. *)
let identify _f = Unknown

let to_string = function
  | Cyclic n -> Printf.sprintf "Z/%dZ" n
  | Dihedral n -> Printf.sprintf "D_%d" n
  | Symmetric n -> Printf.sprintf "S_%d" n
  | Alternating n -> Printf.sprintf "A_%d" n
  | Frobenius n -> Printf.sprintf "F_%d" n
  | Unknown -> "Unknown"
