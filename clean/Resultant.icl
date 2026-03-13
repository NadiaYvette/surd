implementation module Resultant

import StdEnv
import StdMaybe
import Poly
import Rational

// Resultant via the Euclidean PRS (simplified subresultant).
// res(f, g) = (-1)^(deg f * deg g) * res(g, f rem g) * lc(g)^(deg f - deg(f rem g)) / ...
// We use the simple definition via iterated pseudo-remainder.
polyResultant :: !(Poly Rational) !(Poly Rational) -> Rational
polyResultant f g
    | degree f < 0 || degree g < 0 = zero
    | degree f == 0 = ratPow (getLC f) (degree g)
    | degree g == 0 = ratPow (getLC g) (degree f)
    | degree f < degree g
        # s = if (isOdd (degree f) && isOdd (degree g)) (~ one) one
        = s * polyResultant g f
    | otherwise
        // res(f, g) = (-1)^(m*n) * lc(g)^(m-deg(r)) * res(g, r)
        // where r = f rem g, m = deg f, n = deg g
        # (_, r) = divModPoly f g
        | degree r < 0 = zero
        # m = degree f
        # n = degree g
        # s = if (isOdd m && isOdd n) (~ one) one
        # lcg = getLC g
        = s * ratPow lcg (m - degree r) * polyResultant g r

getLC :: !(Poly Rational) -> Rational
getLC p = case leadCoeff p of
    Just c  -> c
    Nothing -> zero
