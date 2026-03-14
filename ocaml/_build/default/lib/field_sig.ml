(** Algebraic signatures for rings, fields, and evaluation targets.

    These module types are the backbone of the OCaml port: functors
    parameterised by RING or FIELD replace Haskell's typeclasses. *)

(** A commutative ring with identity. *)
module type RING = sig
  type t

  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val neg : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_int : int -> t
  val to_string : t -> string
end

(** A field: a ring where every nonzero element has a multiplicative inverse. *)
module type FIELD = sig
  include RING

  val inv : t -> t
  val div : t -> t -> t
end

