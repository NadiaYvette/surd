/// Strictly positive integers as a refinement type.
/// F*'s refinement types let Z3 prove positivity statically — no runtime check needed.
module Surd.Positive

/// A strictly positive integer. Z3 enforces x > 0 at every construction site.
type positive = x:int{x > 0}

/// Unwrap to a plain int (always > 0 by construction).
let to_int (p: positive) : int = p

/// Addition of positives is positive (Z3 proves this automatically).
let pos_add (a b : positive) : positive = a + b

/// Multiplication of positives is positive.
let pos_mul (a b : positive) : positive = op_Multiply a b

/// Predecessor: returns a positive only if the result is > 0.
let pos_pred (a: positive{a > 1}) : positive = a - 1

/// Maximum of two positives.
let pos_max (a b : positive) : positive = if a > b then a else b

/// Minimum of two positives.
let pos_min (a b : positive) : positive = if a < b then a else b
