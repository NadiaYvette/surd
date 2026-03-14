definition module Algebra

// Abstract algebraic structures: Ring and Field.
// These type classes centralise arithmetic operations that are
// repeated across Rational, Poly, and ExtElem, reducing boilerplate
// and enabling generic algorithm signatures.

import StdOverloaded

// A ring with additive identity, multiplicative identity,
// addition, multiplication, and additive inverse.
class Ring a where
    rzero :: a
    rone  :: a
    radd  :: !a !a -> a
    rmul  :: !a !a -> a
    rneg  :: !a -> a

// A field extends a ring with multiplicative inverse and division.
// Note: Clean does not have superclass constraints in class definitions.
// Users should add both Ring and Field constraints where needed.
class Field a where
    finv  :: !a -> a
    fdiv  :: !a !a -> a

// Convenience: repeated squaring exponentiation in any Ring.
rpow :: !a !Int -> a | Ring a
