definition module Poly

// Dense univariate polynomials over an arbitrary coefficient type.
// Stored as coefficient list, low-degree first.
// Invariant: no trailing zeros (leading coefficient is nonzero),
// except the zero polynomial which is [].

import StdOverloaded
from StdMaybe import :: Maybe

:: Poly k = Poly [k]

// Smart constructor: strip trailing zeros.
mkPoly :: [k] -> Poly k | == k & zero k

zeroPoly :: Poly k
constPoly :: k -> Poly k | == k & zero k
monoX :: Poly k | zero k & one k

// Structural queries
degree :: !(Poly k) -> Int
leadCoeff :: !(Poly k) -> Maybe k

// Evaluation
evalPoly :: !(Poly k) k -> k | + k & * k & zero k

// Scalar operations
scalePoly :: k !(Poly k) -> Poly k | == k & zero k & * k

// Arithmetic
addPoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & + k
subPoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & + k & ~ k
mulPoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & + k & * k
negatePoly :: !(Poly k) -> Poly k | ~ k

// Division (requires field coefficients)
divModPoly :: !(Poly k) !(Poly k) -> (Poly k, Poly k) | == k & zero k & one k & + k & ~ k & * k & / k
gcdPoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & one k & + k & ~ k & * k & / k
monicPoly :: !(Poly k) -> Poly k | == k & zero k & one k & * k & / k

// Calculus and composition
diffPoly :: !(Poly k) -> Poly k | == k & zero k & + k & * k & fromInt k
composePoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & one k & + k & * k

// Factoring
squareFree :: !(Poly k) -> [(Poly k, Int)] | == k & zero k & one k & + k & ~ k & * k & / k & fromInt k

instance == (Poly k) | == k
instance toString (Poly k) | toString k

// Algebraic structure instance (see Algebra module).
// Poly k is a Ring when k is a Ring with == and zero.
from Algebra import class Ring
