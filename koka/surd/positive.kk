// Abstract type for strictly positive integers (> 0).
//
// Encoding the positivity constraint prevents runtime errors for
// functions like factorise that require positive input.
module surd/positive

// A strictly positive integer. The internal int is guaranteed > 0.
// The constructor is exported, but users should prefer the smart constructors.
pub value struct positive
  value : int

// Smart constructor: returns Nothing for non-positive input.
pub fun positive(n : int) : maybe<positive>
  if n > 0 then Just(Positive(n))
  else Nothing

// Unwrap the positive integer to a plain int.
pub fun unwrap(p : positive) : int
  p.value

// Unsafe constructor for use when positivity is guaranteed by context.
// Throws on non-positive input.
pub fun unsafe-positive(n : int) : exn positive
  if n > 0 then Positive(n)
  else throw("unsafe-positive: non-positive value " ++ n.show)

// Addition of two positives is always positive.
pub fun positive/(+)(a : positive, b : positive) : positive
  Positive(a.value + b.value)

// Multiplication of two positives is always positive.
pub fun positive/(*)(a : positive, b : positive) : positive
  Positive(a.value * b.value)

// Show.
pub fun positive/show(p : positive) : string
  p.value.show

// Equality.
pub fun positive/(==)(a : positive, b : positive) : bool
  a.value == b.value

// Comparison.
pub fun positive/cmp(a : positive, b : positive) : order
  cmp(a.value, b.value)

// Less than.
pub fun positive/(<)(a : positive, b : positive) : bool
  a.value < b.value

// Greater than.
pub fun positive/(>)(a : positive, b : positive) : bool
  a.value > b.value

// Less than or equal.
pub fun positive/(<=)(a : positive, b : positive) : bool
  a.value <= b.value

// Greater than or equal.
pub fun positive/(>=)(a : positive, b : positive) : bool
  a.value >= b.value
