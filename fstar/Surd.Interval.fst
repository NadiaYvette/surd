/// Interval arithmetic with rational endpoints for root isolation
/// and numerical evaluation.
module Surd.Interval

open Surd.Rational

/// A closed interval [lo, hi] with rational endpoints.
noeq type interval = {
  lo: rational;
  hi: rational;
}

/// Midpoint of an interval: (lo + hi) / 2.
let midpoint (iv: interval) : rational =
  rat_div_total (rat_add iv.lo iv.hi) (rat_of_int 2)

/// Width of an interval: hi - lo.
let width (iv: interval) : rational =
  rat_sub iv.hi iv.lo

/// Does the interval contain a point?
let contains (iv: interval) (x: rational) : bool =
  rat_le iv.lo x && rat_le x iv.hi

/// Do two intervals overlap?
let overlaps (a b : interval) : bool =
  rat_le a.lo b.hi && rat_le b.lo a.hi

/// Bisect an interval into two halves.
let bisect (iv: interval) : interval & interval =
  let m = midpoint iv in
  ({ lo = iv.lo; hi = m }, { lo = m; hi = iv.hi })

/// Narrow by bisection: keep the half where midpoint satisfies the predicate (left half)
/// or the other half (right half).
let refine (p: rational -> bool) (iv: interval) : interval =
  let (left, right) = bisect iv in
  let m = midpoint iv in
  if p m then left else right

/// Point interval from a rational.
let from_rational (x: rational) : interval = { lo = x; hi = x }

// ---------------------------------------------------------------------------
// Interval arithmetic
// ---------------------------------------------------------------------------

/// Addition: [a,b] + [c,d] = [a+c, b+d].
let iadd (a b : interval) : interval =
  { lo = rat_add a.lo b.lo; hi = rat_add a.hi b.hi }

/// Subtraction: [a,b] - [c,d] = [a-d, b-c].
let isub (a b : interval) : interval =
  { lo = rat_sub a.lo b.hi; hi = rat_sub a.hi b.lo }

/// Negation.
let ineg (iv: interval) : interval =
  { lo = rat_neg iv.hi; hi = rat_neg iv.lo }

/// Minimum of four rationals.
let min4 (a b c d : rational) : rational =
  rat_min (rat_min a b) (rat_min c d)

/// Maximum of four rationals.
let max4 (a b c d : rational) : rational =
  rat_max (rat_max a b) (rat_max c d)

/// Multiplication: [a,b] * [c,d] = [min(ac,ad,bc,bd), max(ac,ad,bc,bd)].
let imul (a b : interval) : interval =
  let p1 = rat_mul a.lo b.lo in
  let p2 = rat_mul a.lo b.hi in
  let p3 = rat_mul a.hi b.lo in
  let p4 = rat_mul a.hi b.hi in
  { lo = min4 p1 p2 p3 p4; hi = max4 p1 p2 p3 p4 }

/// Inverse of an interval that does not contain zero.
/// Precondition: lo > 0 or hi < 0.
let iinv (iv: interval) : interval =
  if rat_gt iv.lo rat_zero || rat_lt iv.hi rat_zero then
    { lo = rat_inv_total iv.hi; hi = rat_inv_total iv.lo }
  else
    (* Degenerate: interval contains zero. Return a wide interval. *)
    { lo = rat_neg (rat_of_int 1000000); hi = rat_of_int 1000000 }

/// Division: a / b = a * (1/b).
let idiv (a b : interval) : interval = imul a (iinv b)

/// Test if strictly positive: lo > 0.
let strictly_positive (iv: interval) : bool = rat_gt iv.lo rat_zero

/// Test if strictly negative: hi < 0.
let strictly_negative (iv: interval) : bool = rat_lt iv.hi rat_zero

/// Test if interval contains zero.
let contains_zero (iv: interval) : bool =
  rat_le iv.lo rat_zero && rat_ge iv.hi rat_zero

/// Absolute value of an interval.
let iabs (iv: interval) : interval =
  if rat_ge iv.lo rat_zero then iv
  else if rat_le iv.hi rat_zero then ineg iv
  else { lo = rat_zero; hi = rat_max (rat_neg iv.lo) iv.hi }

/// Integer power of an interval.
let rec ipow (iv: interval) (n: nat) : Tot interval (decreases n) =
  if n = 0 then from_rational rat_one
  else if n = 1 then iv
  else if n % 2 = 1 then
    imul iv (ipow iv (n - 1))
  else
    (* Even power: result is non-negative if interval spans zero *)
    let half = ipow iv (n / 2) in
    let sq = imul half half in
    if rat_le iv.lo rat_zero && rat_ge iv.hi rat_zero then
      { lo = rat_zero; hi = sq.hi }
    else sq

// ---------------------------------------------------------------------------
// Bisection-based nth root
// ---------------------------------------------------------------------------

/// Rational power: r^n for non-negative n.
let rec rat_pow_int (r: rational) (n: nat) : Tot rational (decreases n) =
  if n = 0 then rat_one
  else rat_mul r (rat_pow_int r (n - 1))

/// Bisect to find lower bound: largest r in [lo, hi] with r^n <= a.
let rec bisect_down (n: nat{n >= 1}) (a lo hi : rational) (iters: nat)
  : Tot rational (decreases iters) =
  match iters with
  | 0 -> lo
  | _ ->
    let iters' : nat = iters - 1 in
    let mid = midpoint { lo = lo; hi = hi } in
    if rat_le (rat_pow_int mid n) a then bisect_down n a mid hi iters'
    else bisect_down n a lo mid iters'

/// Bisect to find upper bound: smallest r in [lo, hi] with r^n >= a.
let rec bisect_up (n: nat{n >= 1}) (a lo hi : rational) (iters: nat)
  : Tot rational (decreases iters) =
  match iters with
  | 0 -> hi
  | _ ->
    let iters' : nat = iters - 1 in
    let mid = midpoint { lo = lo; hi = hi } in
    if rat_ge (rat_pow_int mid n) a then bisect_up n a lo mid iters'
    else bisect_up n a mid hi iters'

/// Lower bound on nth root of a non-negative rational, via 60 bisection steps.
let nth_root_lower (n: nat{n >= 1}) (a: rational) : rational =
  if rat_eq a rat_zero then rat_zero
  else
    let upper = rat_max a rat_one in
    bisect_down n a rat_zero upper 60

/// Upper bound on nth root of a non-negative rational, via 60 bisection steps.
let nth_root_upper (n: nat{n >= 1}) (a: rational) : rational =
  if rat_eq a rat_zero then rat_zero
  else
    let upper = rat_add (rat_max a rat_one) rat_one in
    bisect_up n a rat_zero upper 60

/// Interval enclosure of the square root of a non-negative interval.
let isqrt (iv: interval) : interval =
  if rat_lt iv.hi rat_zero then
    (* Negative interval: degenerate *)
    from_rational rat_zero
  else if rat_eq iv.hi rat_zero then from_rational rat_zero
  else
    let lo' = if rat_lt iv.lo rat_zero then rat_zero else nth_root_lower 2 iv.lo in
    { lo = lo'; hi = nth_root_upper 2 iv.hi }

/// Interval enclosure of the nth root of an interval.
/// For odd n, handles negative values. For even n, requires non-negative.
let inth (n: nat{n >= 2}) (iv: interval) : interval =
  if n = 2 then isqrt iv
  else if n % 2 = 0 && rat_lt iv.lo rat_zero then
    (* Even root of negative: degenerate *)
    from_rational rat_zero
  else if rat_eq iv.hi rat_zero && rat_eq iv.lo rat_zero then
    from_rational rat_zero
  else if n % 2 = 1 && rat_lt iv.hi rat_zero then
    (* Odd root of entirely negative interval: negate, root, negate *)
    let pos_iv = ineg iv in  (* flip to positive *)
    let lo' = nth_root_lower n pos_iv.lo in
    let hi' = nth_root_upper n pos_iv.hi in
    (* negate back *)
    { lo = rat_neg hi'; hi = rat_neg lo' }
  else if n % 2 = 1 && rat_lt iv.lo rat_zero then
    (* Odd root spanning zero: [-a, b] -> [-root(a), root(b)] *)
    let neg_part = nth_root_upper n (rat_neg iv.lo) in
    let pos_part = nth_root_upper n iv.hi in
    { lo = rat_neg neg_part; hi = pos_part }
  else
    { lo = nth_root_lower n iv.lo; hi = nth_root_upper n iv.hi }

// ---------------------------------------------------------------------------
// Complex intervals (rectangular: real part x imaginary part)
// ---------------------------------------------------------------------------

/// A complex interval [re_lo, re_hi] + i * [im_lo, im_hi].
noeq type complex_interval = {
  ci_real: interval;
  ci_imag: interval;
}

/// Complex interval from a rational (real, zero imaginary part).
let ci_from_rational (r: rational) : complex_interval =
  { ci_real = from_rational r; ci_imag = from_rational rat_zero }

/// Complex interval from a real interval.
let ci_from_real (r: interval) : complex_interval =
  { ci_real = r; ci_imag = from_rational rat_zero }

/// Complex addition.
let ci_add (a b : complex_interval) : complex_interval =
  { ci_real = iadd a.ci_real b.ci_real; ci_imag = iadd a.ci_imag b.ci_imag }

/// Complex subtraction.
let ci_sub (a b : complex_interval) : complex_interval =
  { ci_real = isub a.ci_real b.ci_real; ci_imag = isub a.ci_imag b.ci_imag }

/// Complex negation.
let ci_neg (a: complex_interval) : complex_interval =
  { ci_real = ineg a.ci_real; ci_imag = ineg a.ci_imag }

/// Complex multiplication: (a+bi)(c+di) = (ac-bd) + (ad+bc)i.
let ci_mul (a b : complex_interval) : complex_interval =
  { ci_real = isub (imul a.ci_real b.ci_real) (imul a.ci_imag b.ci_imag);
    ci_imag = iadd (imul a.ci_real b.ci_imag) (imul a.ci_imag b.ci_real) }

/// Complex inverse: 1/(a+bi) = (a-bi)/(a^2+b^2).
let ci_inv (z: complex_interval) : complex_interval =
  let mag_sq = iadd (imul z.ci_real z.ci_real) (imul z.ci_imag z.ci_imag) in
  { ci_real = idiv z.ci_real mag_sq;
    ci_imag = idiv (ineg z.ci_imag) mag_sq }

/// Complex power via repeated squaring (non-negative exponent).
let rec ci_pow (z: complex_interval) (n: nat) : Tot complex_interval (decreases n) =
  if n = 0 then ci_from_rational rat_one
  else if n = 1 then z
  else
    let half = ci_pow z (n / 2) in
    let sq = ci_mul half half in
    if n % 2 = 0 then sq
    else ci_mul sq z

/// |z|^2 = re^2 + im^2 as an interval.
let ci_magnitude_sq (z: complex_interval) : interval =
  iadd (imul z.ci_real z.ci_real) (imul z.ci_imag z.ci_imag)

/// Real part of a complex interval.
let ci_real_part (z: complex_interval) : interval = z.ci_real

/// Imaginary part of a complex interval.
let ci_imag_part (z: complex_interval) : interval = z.ci_imag
