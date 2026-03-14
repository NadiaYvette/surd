(** Euler substitution for integrals involving square roots.

    Stub: implements Euler's substitution method for integrals of the form
    integral of R(x, sqrt(ax^2 + bx + c)) dx. *)

(** Result of an Euler substitution integral. *)
type result = {
  antiderivative : string;  (** symbolic description *)
  steps : string list;      (** computation steps *)
}

(** Compute an integral via Euler substitution. *)
let integrate _a _b _c = {
  antiderivative = "not yet implemented";
  steps = ["Euler substitution not yet implemented"];
}
