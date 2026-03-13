implementation module Eval

import StdEnv
import RadExpr
import Rational
import Interval

// Convert Rational to Real
ratToReal :: !Rational -> Real
ratToReal r = toReal (numer r) / toReal (denom r)
where
    toReal :: !a -> Real | toInt a
    toReal x = toReal (toInt x)

// We need Integer -> Real. Data.Integer provides toInt but that truncates
// for large values. For our purposes this is acceptable (eval is approximate).
import Data.Integer

integerToReal :: !Integer -> Real
integerToReal n = toReal (toInt n)

ratToRealI :: !Rational -> Real
ratToRealI r = integerToReal (numer r) / integerToReal (denom r)

eval :: !(RadExpr Rational) -> Real
eval (Lit r) = ratToRealI r
eval (Neg a) = ~ (eval a)
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Inv a) = 1.0 / eval a
eval (Root n a) = (eval a) ^ (1.0 / toReal n)
eval (Pow a n)
    | n >= 0 = (eval a) ^ (toReal n)
    = 1.0 / ((eval a) ^ (toReal (0 - n)))

// Complex arithmetic helpers
cadd :: !(Real, Real) !(Real, Real) -> (Real, Real)
cadd (a, b) (c, d) = (a + c, b + d)

csub :: !(Real, Real) !(Real, Real) -> (Real, Real)
csub (a, b) (c, d) = (a - c, b - d)

cmul :: !(Real, Real) !(Real, Real) -> (Real, Real)
cmul (a, b) (c, d) = (a*c - b*d, a*d + b*c)

cinv :: !(Real, Real) -> (Real, Real)
cinv (a, b) = (a / m, ~ b / m)
where m = a*a + b*b

cneg :: !(Real, Real) -> (Real, Real)
cneg (a, b) = (~ a, ~ b)

cpow :: !(Real, Real) !Int -> (Real, Real)
cpow _ 0 = (1.0, 0.0)
cpow z n
    | n < 0 = cinv (cpow z (0 - n))
    | isOdd n = cmul z (cpow z (n - 1))
    = let half = cpow z (n / 2) in cmul half half

// atan2 for Real: not in StdEnv, implement via atan
atan2r :: !Real !Real -> Real
atan2r y x
    | x > 0.0 = atan (y / x)
    | x < 0.0 && y >= 0.0 = atan (y / x) + pi
    | x < 0.0 && y < 0.0 = atan (y / x) - pi
    | x == 0.0 && y > 0.0 = pi / 2.0
    | x == 0.0 && y < 0.0 = ~ (pi / 2.0)
    = 0.0
where
    pi = 3.14159265358979323846

complexNthRoot :: !Int !(Real, Real) -> (Real, Real)
complexNthRoot n (re, im)
    # r = sqrt (re*re + im*im)
    # theta = atan2r im re
    # rn = r ^ (1.0 / toReal n)
    # an = theta / toReal n
    = (rn * cos an, rn * sin an)

evalComplex :: !(RadExpr Rational) -> (Real, Real)
evalComplex (Lit r) = (ratToRealI r, 0.0)
evalComplex (Neg a) = cneg (evalComplex a)
evalComplex (Add a b) = cadd (evalComplex a) (evalComplex b)
evalComplex (Mul a b) = cmul (evalComplex a) (evalComplex b)
evalComplex (Inv a) = cinv (evalComplex a)
evalComplex (Root n a) = complexNthRoot n (evalComplex a)
evalComplex (Pow a n) = cpow (evalComplex a) n

// Rational interval evaluation
ratToInterval :: !Rational -> Interval
ratToInterval r = pointInterval r

evalInterval :: !(RadExpr Rational) -> Interval
evalInterval (Lit r) = ratToInterval r
evalInterval (Neg a)
    # iv = evalInterval a
    = ineg iv
evalInterval (Add a b) = iadd (evalInterval a) (evalInterval b)
evalInterval (Mul a b) = imul (evalInterval a) (evalInterval b)
evalInterval (Inv a) = iinv (evalInterval a)
evalInterval (Root n a) = iNthRoot n (evalInterval a)
evalInterval (Pow a n) = ipow (evalInterval a) n

// Integer nth root for intervals via bisection.
// Only handles positive intervals; negative intervals with odd n are negated.
iNthRoot :: !Int !Interval -> Interval
iNthRoot n iv
    | strictlyNegative iv && isOdd n
        # pos = iNthRoot n (ineg iv)
        = ineg pos
    | strictlyNegative iv = abort "iNthRoot: even root of negative interval"
    // For non-negative intervals, use rational bisection.
    // We bracket: if iv = [lo, hi], result is [nthRootLo lo, nthRootHi hi]
    // where nthRootLo finds a lower bound and nthRootHi an upper bound.
    // Simple approach: use ipow-based Newton refinement.
    // For now, a simple bounding approach.
    # lo = ivLo iv
    # hi = ivHi iv
    # rLo = ratNthRootLo n lo
    # rHi = ratNthRootHi n hi
    = mkInterval rLo rHi

// Lower bound on nth root of a non-negative rational via bisection.
ratNthRootLo :: !Int !Rational -> Rational
ratNthRootLo _ r
    | r == zero = zero
ratNthRootLo n r = bisectLo n r zero (max one r) 40

bisectLo :: !Int !Rational !Rational !Rational !Int -> Rational
bisectLo _ _ lo _ 0 = lo
bisectLo n target lo hi iters
    # mid = (lo + hi) / (ratFromInt 2)
    | ratPow mid n < target = bisectLo n target mid hi (iters - 1)
    = bisectLo n target lo mid (iters - 1)
where
    max :: !Rational !Rational -> Rational
    max a b
        | a < b = b
        = a

// Upper bound on nth root of a non-negative rational via bisection.
ratNthRootHi :: !Int !Rational -> Rational
ratNthRootHi _ r
    | r == zero = zero
ratNthRootHi n r = bisectHi n r zero (max one r + one) 40

bisectHi :: !Int !Rational !Rational !Rational !Int -> Rational
bisectHi _ _ _ hi 0 = hi
bisectHi n target lo hi iters
    # mid = (lo + hi) / (ratFromInt 2)
    | target < ratPow mid n = bisectHi n target lo mid (iters - 1)
    = bisectHi n target mid hi (iters - 1)
where
    max :: !Rational !Rational -> Rational
    max a b
        | a < b = b
        = a
