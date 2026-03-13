// Ring and field dictionary structs for ad-hoc polymorphism.
//
// Koka has no typeclasses, so we pass these dictionaries explicitly
// (or via implicit parameters using `with`) to polymorphic functions.
module surd/ring

import surd/rational

// A ring: zero, one, add, sub, mul, negate, from-int, eq, show.
pub struct ring<k>
  ring-zero : k
  ring-one : k
  ring-add : (k, k) -> k
  ring-sub : (k, k) -> k
  ring-mul : (k, k) -> k
  ring-neg : (k) -> k
  ring-from-int : (int) -> k
  ring-eq : (k, k) -> bool
  ring-show : (k) -> string

// A field extends a ring with division and inversion.
// The `field-div` and `field-inv` may throw on division by zero.
pub struct field<k>
  field-ring : ring<k>
  field-div : (k, k) -> exn k
  field-inv : (k) -> exn k

// Convenience: extract ring operations from a field.
pub fun to-ring(f : field<k>) : ring<k>
  f.field-ring

// ---------------------------------------------------------------
// Concrete instances
// ---------------------------------------------------------------

// Ring instance for int.
pub val int-ring : ring<int> = Ring(
  ring-zero = 0,
  ring-one = 1,
  ring-add = fn(a, b) a + b,
  ring-sub = fn(a, b) a - b,
  ring-mul = fn(a, b) a * b,
  ring-neg = fn(a) 0 - a,
  ring-from-int = fn(n) n,
  ring-eq = fn(a, b) a == b,
  ring-show = fn(a) a.show
)

// Ring instance for rational.
pub val rational-ring : ring<rational> = Ring(
  ring-zero = surd/rational/zero,
  ring-one = surd/rational/one,
  ring-add = fn(a, b) a + b,
  ring-sub = fn(a, b) a - b,
  ring-mul = fn(a, b) a * b,
  ring-neg = fn(a) a.negate,
  ring-from-int = fn(n) from-int(n),
  ring-eq = fn(a : rational, b : rational) a == b,
  ring-show = fn(a : rational) a.show
)

// Field instance for rational.
pub val rational-field : field<rational> = Field(
  field-ring = rational-ring,
  field-div = fn(a, b) a / b,
  field-inv = fn(a) a.inv
)
