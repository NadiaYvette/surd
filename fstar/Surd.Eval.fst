/// Numerical evaluation of radical expressions.
///
/// Since F*'s Float module is nearly empty (just `assume new type float`),
/// we declare the needed floating-point operations as `assume val` and
/// provide eval_double and eval_complex (pair of doubles).
module Surd.Eval

open Surd.Ring
open Surd.Rational
open Surd.Types

// ---------------------------------------------------------------------------
// Float primitives (F*'s FStar.Float is a placeholder, so we assume these)
// ---------------------------------------------------------------------------

assume new type double : Type0

assume val double_of_int : int -> double
assume val double_add : double -> double -> double
assume val double_sub : double -> double -> double
assume val double_mul : double -> double -> double
assume val double_div : double -> double -> double
assume val double_neg : double -> double
assume val double_abs : double -> double
assume val double_pow : double -> double -> double
assume val double_sqrt : double -> double
assume val double_atan2 : double -> double -> double
assume val double_cos : double -> double
assume val double_sin : double -> double
assume val double_zero : double
assume val double_one : double
assume val double_lt : double -> double -> bool
assume val double_le : double -> double -> bool
assume val double_gt : double -> double -> bool

/// Convert a rational to a double.
let double_of_rational (r: rational) : double =
  double_div (double_of_int r.num) (double_of_int r.den)

// ---------------------------------------------------------------------------
// Complex double (pair of doubles)
// ---------------------------------------------------------------------------

noeq type complex_double = {
  re: double;
  im: double;
}

let cd_zero : complex_double = { re = double_zero; im = double_zero }

let cd_of_double (x: double) : complex_double = { re = x; im = double_zero }

let cd_of_rational (r: rational) : complex_double =
  { re = double_of_rational r; im = double_zero }

let cd_neg (a: complex_double) : complex_double =
  { re = double_neg a.re; im = double_neg a.im }

let cd_add (a b: complex_double) : complex_double =
  { re = double_add a.re b.re; im = double_add a.im b.im }

let cd_sub (a b: complex_double) : complex_double =
  { re = double_sub a.re b.re; im = double_sub a.im b.im }

let cd_mul (a b: complex_double) : complex_double =
  { re = double_sub (double_mul a.re b.re) (double_mul a.im b.im);
    im = double_add (double_mul a.re b.im) (double_mul a.im b.re) }

let cd_inv (a: complex_double) : complex_double =
  let d = double_add (double_mul a.re a.re) (double_mul a.im a.im) in
  { re = double_div a.re d; im = double_neg (double_div a.im d) }

/// Complex magnitude.
let cd_magnitude (a: complex_double) : double =
  double_sqrt (double_add (double_mul a.re a.re) (double_mul a.im a.im))

/// Principal nth root of a complex number using polar form.
let cd_nth_root (n: int{n >= 2}) (z: complex_double) : complex_double =
  let r = cd_magnitude z in
  let theta = double_atan2 z.im z.re in
  let inv_n = double_div double_one (double_of_int n) in
  let rn = double_pow r inv_n in
  let an = double_mul theta inv_n in
  { re = double_mul rn (double_cos an);
    im = double_mul rn (double_sin an) }

/// Complex power (non-negative exponent) by repeated squaring.
let rec cd_pow (z: complex_double) (n: nat) : Tot complex_double (decreases n) =
  if n = 0 then cd_of_double double_one
  else if n = 1 then z
  else
    let half = cd_pow z (n / 2) in
    let sq = cd_mul half half in
    if n % 2 = 0 then sq
    else cd_mul sq z

// ---------------------------------------------------------------------------
// Evaluate to double (real-valued, NaN for even roots of negatives)
// ---------------------------------------------------------------------------

/// Evaluate a radical expression to a double.
let rec eval_double (e: rad_expr rational) : Tot double (decreases e) =
  match e with
  | Lit r -> double_of_rational r
  | Neg a -> double_neg (eval_double a)
  | Add a b -> double_add (eval_double a) (eval_double b)
  | Mul a b -> double_mul (eval_double a) (eval_double b)
  | Inv a -> double_div double_one (eval_double a)
  | Root n a ->
    let inv_n = double_div double_one (double_of_int n) in
    double_pow (eval_double a) inv_n
  | Pow a n ->
    if n >= 0 then
      let base = eval_double a in
      double_pow base (double_of_int n)
    else
      let base = eval_double a in
      double_div double_one (double_pow base (double_of_int (0 - n)))

// ---------------------------------------------------------------------------
// Evaluate to complex double (handles complex intermediates)
// ---------------------------------------------------------------------------

/// Evaluate a radical expression to a complex double.
let rec eval_complex (e: rad_expr rational) : Tot complex_double (decreases e) =
  match e with
  | Lit r -> cd_of_rational r
  | Neg a -> cd_neg (eval_complex a)
  | Add a b -> cd_add (eval_complex a) (eval_complex b)
  | Mul a b -> cd_mul (eval_complex a) (eval_complex b)
  | Inv a -> cd_inv (eval_complex a)
  | Root n a -> cd_nth_root n (eval_complex a)
  | Pow a n ->
    if n >= 0 then cd_pow (eval_complex a) n
    else cd_inv (cd_pow (eval_complex a) (0 - n))

// ---------------------------------------------------------------------------
// Evaluate to rational interval (rigorous enclosure)
// ---------------------------------------------------------------------------

open Surd.Interval

/// Evaluate a radical expression to a rational interval enclosure.
let rec eval_interval (e: rad_expr rational) : Tot interval (decreases e) =
  match e with
  | Lit r -> from_rational r
  | Neg a ->
    let iv = eval_interval a in
    { lo = rat_neg iv.hi; hi = rat_neg iv.lo }
  | Add a b -> iadd (eval_interval a) (eval_interval b)
  | Mul a b -> imul (eval_interval a) (eval_interval b)
  | Inv a -> iinv (eval_interval a)
  | Root n a -> inth n (eval_interval a)
  | Pow a n ->
    if n >= 0 then ipow (eval_interval a) n
    else iinv (ipow (eval_interval a) (0 - n))

/// Evaluate a radical expression to a complex interval enclosure.
let rec eval_complex_interval (e: rad_expr rational) : Tot complex_interval (decreases e) =
  match e with
  | Lit r -> ci_from_rational r
  | Neg a -> ci_neg (eval_complex_interval a)
  | Add a b -> ci_add (eval_complex_interval a) (eval_complex_interval b)
  | Mul a b -> ci_mul (eval_complex_interval a) (eval_complex_interval b)
  | Inv a -> ci_inv (eval_complex_interval a)
  | Pow a n ->
    if n >= 0 then ci_pow (eval_complex_interval a) n
    else ci_inv (ci_pow (eval_complex_interval a) (0 - n))
  | Root n a ->
    let ci = eval_complex_interval a in
    let re_part = ci.ci_real in
    let im_part = ci.ci_imag in
    if rat_ge im_part.lo rat_zero
       && rat_le im_part.hi rat_zero
       && rat_ge re_part.lo rat_zero then
      (* Non-negative real: real nth root *)
      ci_from_real (inth n re_part)
    else if rat_ge im_part.lo rat_zero
            && rat_le im_part.hi rat_zero
            && rat_le re_part.hi rat_zero
            && n % 2 = 1 then
      (* Odd root of negative real *)
      let pos_iv = ineg re_part in
      let root_pos = inth n pos_iv in
      { ci_real = ineg root_pos; ci_imag = from_rational rat_zero }
    else if rat_ge im_part.lo rat_zero
            && rat_le im_part.hi rat_zero
            && rat_le re_part.hi rat_zero
            && n = 2 then
      (* sqrt of negative real: i * sqrt(|x|) *)
      let pos_iv : interval = { lo = rat_neg re_part.hi; hi = rat_neg re_part.lo } in
      let root_pos = isqrt pos_iv in
      { ci_real = from_rational rat_zero; ci_imag = root_pos }
    else
      (* General case: fall back to wide interval (rigorous but imprecise) *)
      let mag = ci_magnitude_sq ci in
      let r_bound = isqrt mag in
      let wide : interval = { lo = rat_neg r_bound.hi; hi = r_bound.hi } in
      { ci_real = wide; ci_imag = wide }
