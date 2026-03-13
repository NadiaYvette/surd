definition module Cyclotomic

// Cyclotomic polynomial construction.
// The nth cyclotomic polynomial Phi_n(x) is the minimal polynomial
// of primitive nth roots of unity over Q.

from Poly import :: Poly
from Rational import :: Rational

// Compute the nth cyclotomic polynomial over Q.
cyclotomic :: !Int -> Poly Rational

// Euler's totient function.
eulerTotient :: !Int -> Int
