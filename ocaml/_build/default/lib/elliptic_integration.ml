(** Elliptic integral reduction.

    Stub: reduces integrals involving square roots of cubics/quartics
    to standard elliptic integrals (Legendre or Weierstrass form). *)

(** Classification of elliptic integrals. *)
type kind =
  | First   (** K(k) = integral of 1/sqrt((1-t^2)(1-k^2*t^2)) *)
  | Second  (** E(k) = integral of sqrt((1-k^2*t^2)/(1-t^2)) *)
  | Third   (** Pi(n,k) *)

(** Result of an elliptic integral reduction. *)
type result = {
  kind : kind;
  modulus : Rational.t;  (** k *)
  description : string;
}

(** Attempt to reduce an integral to standard elliptic form. *)
let reduce _poly = None
