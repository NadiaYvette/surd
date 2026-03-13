implementation module AlgNum

import StdEnv
import Poly
import Rational
import Interval

mkAlgNum :: !(Poly Rational) !Interval -> AlgNum
mkAlgNum p iv = { anMinPoly = p, anInterval = iv }

algNumApprox :: !AlgNum -> Rational
algNumApprox an = midpoint an.anInterval

instance toString AlgNum where
    toString an = "AlgNum(" +++ toString an.anMinPoly +++ ", " +++ toString an.anInterval +++ ")"
