(** Algebraic signatures for rings, fields, and evaluation targets.

    These module types are the backbone of the OCaml port: functors
    parameterised by RING or FIELD replace Haskell's typeclasses. *)

(** A commutative ring with identity. *)
module type RING = sig
  (** The carrier type. *)
  type t

  (** Additive identity: [zero + x = x]. *)
  val zero : t
  (** Multiplicative identity: [one * x = x]. *)
  val one : t
  (** Ring addition. *)
  val add : t -> t -> t
  (** Ring subtraction. *)
  val sub : t -> t -> t
  (** Ring multiplication. *)
  val mul : t -> t -> t
  (** Additive inverse: [neg x + x = zero]. *)
  val neg : t -> t
  (** Structural equality on ring elements. *)
  val equal : t -> t -> bool
  (** Total ordering on ring elements (for use as map/set keys). *)
  val compare : t -> t -> int
  (** Embed a machine integer into the ring. *)
  val of_int : int -> t
  (** Human-readable string representation. *)
  val to_string : t -> string
end

(** A field: a ring where every nonzero element has a multiplicative inverse. *)
module type FIELD = sig
  include RING

  (** Multiplicative inverse: [inv x * x = one].
      Behaviour on zero is unspecified. *)
  val inv : t -> t
  (** Field division: [div a b = mul a (inv b)]. *)
  val div : t -> t -> t
end

