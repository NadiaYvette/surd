definition module AlgNum

// Algebraic number type: (minimal polynomial, isolating interval).

from Poly import :: Poly
from Rational import :: Rational
from Interval import :: Interval

:: AlgNum = { anMinPoly :: !(Poly Rational), anInterval :: !Interval }

// Construct an algebraic number.
mkAlgNum :: !(Poly Rational) !Interval -> AlgNum

// Evaluate an algebraic number to a rational approximation.
algNumApprox :: !AlgNum -> Rational

instance toString AlgNum
