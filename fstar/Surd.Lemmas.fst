/// Verified arithmetic properties of rational numbers.
///
/// These lemmas provide Z3-checkable proofs of algebraic identities.
/// SMTPat annotations allow automatic application during verification.
module Surd.Lemmas

open Surd.Rational

// ---------------------------------------------------------------------------
// Rational arithmetic commutativity
// ---------------------------------------------------------------------------

/// Addition is commutative (structurally, since mk_rational is deterministic).
val rat_add_comm : a:rational -> b:rational ->
  Lemma (rat_eq (rat_add a b) (rat_add b a))
  [SMTPat (rat_add a b)]
let rat_add_comm a b = ()

/// Multiplication is commutative (structurally).
val rat_mul_comm : a:rational -> b:rational ->
  Lemma (rat_eq (rat_mul a b) (rat_mul b a))
  [SMTPat (rat_mul a b)]
let rat_mul_comm a b = ()

// ---------------------------------------------------------------------------
// Rational negation properties
// ---------------------------------------------------------------------------

/// Double negation: -(-a) = a (structural).
val rat_neg_neg : a:rational ->
  Lemma (rat_eq (rat_neg (rat_neg a)) a)
  [SMTPat (rat_neg (rat_neg a))]
let rat_neg_neg a = ()

/// Negation of zero is zero (structural).
val rat_neg_zero : unit -> Lemma (rat_eq (rat_neg rat_zero) rat_zero)
let rat_neg_zero () = ()

/// Positive denominator is preserved by rat_neg.
val rat_neg_den : a:rational -> Lemma ((rat_neg a).den = a.den)
let rat_neg_den a = ()

// ---------------------------------------------------------------------------
// Rational order properties
// ---------------------------------------------------------------------------

/// rat_lt is irreflexive.
val rat_lt_irrefl : a:rational -> Lemma (not (rat_lt a a))
let rat_lt_irrefl a = ()

/// rat_le is reflexive.
val rat_le_refl : a:rational -> Lemma (rat_le a a)
let rat_le_refl a = ()

/// rat_eq is reflexive.
val rat_eq_refl : a:rational -> Lemma (rat_eq a a) [SMTPat (rat_eq a a)]
let rat_eq_refl a = ()

/// rat_eq is symmetric.
val rat_eq_sym : a:rational -> b:rational ->
  Lemma (requires rat_eq a b) (ensures rat_eq b a)
let rat_eq_sym a b = ()

/// rat_gt is the flip of rat_lt.
val rat_gt_lt : a:rational -> b:rational -> Lemma (rat_gt a b = rat_lt b a)
let rat_gt_lt a b = ()

// ---------------------------------------------------------------------------
// GCD properties
// ---------------------------------------------------------------------------

/// gcd_nat result is bounded by the second argument when it's positive.
val gcd_bounded : a:nat -> b:nat{b > 0} -> Lemma (gcd_nat a b <= b)
let gcd_bounded a b = gcd_nat_le_second a b

/// abs_int of a nonzero integer is positive.
val abs_int_pos : x:int{x <> 0} -> Lemma (abs_int x > 0)
let abs_int_pos x = ()

/// abs_int is always non-negative.
val abs_int_nonneg : x:int -> Lemma (abs_int x >= 0)
let abs_int_nonneg x = ()

// ---------------------------------------------------------------------------
// Rational sign / positivity
// ---------------------------------------------------------------------------

/// A rational is positive iff its numerator is positive (since den > 0).
val rat_pos_num : r:rational ->
  Lemma (rat_gt r rat_zero <==> r.num > 0)
let rat_pos_num r = ()

/// A rational is negative iff its numerator is negative (since den > 0).
val rat_neg_num : r:rational ->
  Lemma (rat_lt r rat_zero <==> r.num < 0)
let rat_neg_num r = ()

/// Denominator of any rational is positive.
val rat_den_pos : r:rational -> Lemma (r.den > 0)
let rat_den_pos r = ()
