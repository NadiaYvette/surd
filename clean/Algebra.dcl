definition module Algebra

// Abstract algebraic structures: Ring and Field.
// These type classes centralise arithmetic operations that are
// repeated across Rational, Poly, and ExtElem, reducing boilerplate
// and enabling generic algorithm signatures.

import StdOverloaded

// A commutative ring with identity.
//
// Provides the additive identity (rzero), multiplicative identity (rone),
// ring addition (radd), ring multiplication (rmul), and additive inverse (rneg).
class Ring a where
    // Additive identity: radd rzero x = x.
    rzero :: a
    // Multiplicative identity: rmul rone x = x.
    rone  :: a
    // Ring addition.
    radd  :: !a !a -> a
    // Ring multiplication.
    rmul  :: !a !a -> a
    // Additive inverse: radd (rneg x) x = rzero.
    rneg  :: !a -> a

// A field: a ring where every nonzero element has a multiplicative inverse.
//
// Clean does not have superclass constraints in class definitions,
// so users should add both Ring and Field constraints where needed.
class Field a where
    // Multiplicative inverse: rmul (finv x) x = rone for x /= rzero.
    finv  :: !a -> a
    // Field division: fdiv a b = rmul a (finv b).
    fdiv  :: !a !a -> a

// Exponentiation by repeated squaring in any Ring.
// rpow x n computes x^n for non-negative n, rone for n=0.
rpow :: !a !Int -> a | Ring a
