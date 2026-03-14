// Ring and field operations as algebraic effects.
//
// Koka's effect system replaces dictionary-passing with effect operations
// and handlers. This is the idiomatic Koka approach: callers install a
// handler, and polymorphic code simply performs effect operations.
module surd/ring-effect

import surd/rational

// ---------------------------------------------------------------
// Ring effect: zero, one, add, sub, mul, neg, from-int, eq, show.
// ---------------------------------------------------------------

// A ring provides zero, one, add, sub, mul, neg, from-int, eq, and show.
// Code performs these operations as effect calls; handlers install the
// concrete implementation for a particular type (e.g., int, rational).
pub effect ring-ops<k>
  // Additive identity.
  fun ring-zero() : k
  // Multiplicative identity.
  fun ring-one() : k
  // Ring addition.
  fun ring-add(a : k, b : k) : k
  // Ring subtraction.
  fun ring-sub(a : k, b : k) : k
  // Ring multiplication.
  fun ring-mul(a : k, b : k) : k
  // Additive inverse.
  fun ring-neg(x : k) : k
  // Embed a machine integer into the ring.
  fun ring-from-int(n : int) : k
  // Equality test on ring elements.
  fun ring-eq(a : k, b : k) : bool
  // Human-readable representation of a ring element.
  fun ring-show(a : k) : string

// ---------------------------------------------------------------
// Field effect: division and inversion (extends ring).
// ---------------------------------------------------------------

// A field extends a ring with division and multiplicative inverse.
pub effect field-ops<k>
  // Field division: a / b.
  fun field-div(a : k, b : k) : k
  // Multiplicative inverse: 1/x.
  fun field-inv(x : k) : k

// ---------------------------------------------------------------
// Handlers for int
// ---------------------------------------------------------------

// Install ring operations for int. Within the handler, ring-zero() returns 0, etc.
pub fun with-int-ring(action : () -> <ring-ops<int>|e> a) : e a
  with handler
    fun ring-zero() 0
    fun ring-one() 1
    fun ring-add(a, b) int/(+)(a, b)
    fun ring-sub(a, b) int/(-)(a, b)
    fun ring-mul(a, b) int/(*)(a, b)
    fun ring-neg(x) int/(-)(0, x)
    fun ring-from-int(n) n
    fun ring-eq(a, b) int/(==)(a, b)
    fun ring-show(a) int/show(a)
  action()

// ---------------------------------------------------------------
// Handlers for rational
// ---------------------------------------------------------------

// Install ring operations for rational numbers.
pub fun with-rational-ring(action : () -> <ring-ops<rational>|e> a) : e a
  with handler
    fun ring-zero() surd/rational/zero
    fun ring-one() surd/rational/one
    fun ring-add(a, b) rational/(+)(a, b)
    fun ring-sub(a, b) rational/(-)(a, b)
    fun ring-mul(a, b) rational/(*)(a, b)
    fun ring-neg(x) rational/negate(x)
    fun ring-from-int(n) from-int(n)
    fun ring-eq(a, b) rational/(==)(a, b)
    fun ring-show(a) rational/show(a)
  action()

// Install field operations (div, inv) for rational numbers.
pub fun with-rational-field(action : () -> <field-ops<rational>,exn|e> a) : <exn|e> a
  with handler
    fun field-div(a, b) rational/(/)(a, b)
    fun field-inv(x) rational/inv(x)
  action()

// Convenience: install both ring and field handlers for rationals.
// Usage: with-rational-ring{ with-rational-field{ ... } }
// This avoids effect-row unification issues with nested handler composition.
