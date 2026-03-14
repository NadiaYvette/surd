(** Parametric evaluation of radical expressions.

    The core design idea: a single polymorphic evaluator parameterised
    by a first-class module.  Concrete evaluators for [float] and
    [Complex.t] are provided as instances. *)

open Rad_expr

(** Target for parametric evaluation of radical expressions.
    First-class modules of this type are passed to [eval]. *)
module type EVAL_TARGET = sig
  type t

  val of_rational : Rational.t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val neg : t -> t
  val inv : t -> t
  val nth_root : int -> t -> t
  val pow : t -> int -> t
end

(** Evaluate a radical expression using a first-class module as the
    evaluation target.  This replaces the four ad-hoc [eval*] functions
    in the Haskell version with one parametric function.

    {[
      let x = Eval.eval (module FloatEval) expr in ...
      let z = Eval.eval (module ComplexEval) expr in ...
    ]} *)
let eval (type a) (module E : EVAL_TARGET with type t = a)
    (expr : Rational.t Rad_expr.t) : a =
  let rec go = function
    | Lit r -> E.of_rational r
    | Neg x -> E.neg (go x)
    | Add (a, b) -> E.add (go a) (go b)
    | Mul (a, b) -> E.mul (go a) (go b)
    | Inv x -> E.inv (go x)
    | Root (n, x) -> E.nth_root n (go x)
    | Pow (x, n) -> E.pow (go x) n
  in
  go expr

(** {2 Concrete evaluators} *)

(** Evaluate to [float].  Fast but inexact; even roots of negative
    numbers produce [nan]. *)
module FloatEval : EVAL_TARGET with type t = float = struct
  type t = float

  let of_rational r = Rational.to_float r
  let add = Float.add
  let sub = Float.sub
  let mul = Float.mul
  let neg = Float.neg
  let inv x = 1.0 /. x

  let nth_root n x =
    if n = 2 then Float.sqrt x
    else x ** (1.0 /. Float.of_int n)

  let rec pow x n =
    if n >= 0 then
      let rec go acc k = if k = 0 then acc else go (acc *. x) (k - 1) in
      go 1.0 n
    else
      1.0 /. pow x (- n)
end

(** Evaluate to [Complex.t].  Handles complex intermediates correctly
    (e.g., cube roots via the casus irreducibilis). *)
module ComplexEval : EVAL_TARGET with type t = Complex.t = struct
  type t = Complex.t

  let of_rational r = Complex.{ re = Rational.to_float r; im = 0.0 }
  let add = Complex.add
  let sub = Complex.sub
  let mul = Complex.mul
  let neg = Complex.neg
  let inv z = Complex.div Complex.one z

  let nth_root n z =
    let r = Complex.norm z in
    let theta = Float.atan2 z.im z.re in
    let rn = r ** (1.0 /. Float.of_int n) in
    let an = theta /. Float.of_int n in
    Complex.{ re = rn *. Float.cos an; im = rn *. Float.sin an }

  let rec pow z n =
    if n >= 0 then
      let rec go acc k = if k = 0 then acc else go (Complex.mul acc z) (k - 1) in
      go Complex.one n
    else
      Complex.div Complex.one (pow z (- n))
end

(** Convenience wrappers. *)

let eval_float expr = eval (module FloatEval) expr
let eval_complex expr = eval (module ComplexEval) expr
