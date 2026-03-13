implementation module Denest

import StdEnv
import RadExpr
import Rational
import Normalize

// Denesting dispatcher: try sqrt, then cube root, then return original.
denest :: !(RadExpr Rational) -> RadExpr Rational
denest expr = goD expr
where
    goD (Root 2 inner) = denestSqrt (Root 2 (goD inner))
    goD (Root 3 inner) = denestCubeRoot (Root 3 (goD inner))
    goD (Root n inner) = Root n (goD inner)
    goD (Neg a) = Neg (goD a)
    goD (Add a b) = Add (goD a) (goD b)
    goD (Mul a b) = Mul (goD a) (goD b)
    goD (Inv a) = Inv (goD a)
    goD (Pow a n) = Pow (goD a) n
    goD e = e

// Borodin sqrt denesting: sqrt(a + b*sqrt(c)) = sqrt(d) + sqrt(e)
// where d = (a + sqrt(a^2 - b^2*c)) / 2 and e = (a - sqrt(a^2 - b^2*c)) / 2
denestSqrt :: !(RadExpr Rational) -> RadExpr Rational
denestSqrt expr=:(Root 2 (Add (Lit a) (Mul (Lit b) (Root 2 (Lit c)))))
    # disc = a * a - b * b * c
    | disc >= zero
        # sqrtDisc = tryExactSqrt disc
        = case sqrtDisc of
            ?Just sd ->
                # d = (a + sd) / (ratFromInt 2)
                # e = (a - sd) / (ratFromInt 2)
                | d >= zero && e >= zero
                    # result = Add (Root 2 (Lit d)) (Root 2 (Lit e))
                    = if (b < zero) (Add (Root 2 (Lit d)) (Neg (Root 2 (Lit e)))) result
                | otherwise = expr
            ?None -> expr
    = expr
denestSqrt expr = expr

tryExactSqrt :: !Rational -> ?(Rational)
tryExactSqrt r
    | r < zero = ?None
    | r == zero = ?Just zero
    # n = toInt (numer r)
    # d = toInt (denom r)
    # sn = toInt (sqrt (toReal (abs n)))
    # sd = toInt (sqrt (toReal (abs d)))
    | sn * sn == abs n && sd * sd == abs d
        = ?Just (mkRational (toInteger sn) (toInteger sd))
    = ?None

import Data.Integer

// Cube root denesting (simplified stub)
denestCubeRoot :: !(RadExpr Rational) -> RadExpr Rational
denestCubeRoot expr = expr
