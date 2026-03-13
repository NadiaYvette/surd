implementation module Resolvent

import StdEnv
import Poly
import Rational
import Factoring
import Resultant
import RootBound
import Data.Integer
import StdMaybe

// ─── Numerical root finding via Aberth-Ehrlich ───
complexRootsOf :: !(Poly Rational) -> [(Real, Real)]
complexRootsOf p
    | degree p <= 0 = []
    | degree p == 1
        # cs = getCoeffs p
        = [(~ (ratToR (hd cs) / ratToR (last cs)), 0.0)]
    # n = degree p
    # bound = ratToR (cauchyBound p)
    // Initial guesses: equally spaced on circle of radius bound
    # pi = 3.14159265358979323846
    # inits = [(bound * cos (2.0 * pi * toReal k / toReal n + 0.3),
                bound * sin (2.0 * pi * toReal k / toReal n + 0.3))
               \\ k <- [0 .. n - 1]]
    = aberthIterate p inits 200

ratToR :: !Rational -> Real
ratToR r = toReal (toInt (numer r)) / toReal (toInt (denom r))

getCoeffs :: !(Poly Rational) -> [Rational]
getCoeffs (Poly cs) = cs

// Evaluate polynomial at a complex point
evalPolyC :: !(Poly Rational) !(Real, Real) -> (Real, Real)
evalPolyC (Poly []) _ = (0.0, 0.0)
evalPolyC (Poly cs) z = foldr (\c acc -> cadd (ratToR c, 0.0) (cmulR z acc)) (0.0, 0.0) cs

cadd :: !(Real, Real) !(Real, Real) -> (Real, Real)
cadd (a, b) (c, d) = (a + c, b + d)

csub :: !(Real, Real) !(Real, Real) -> (Real, Real)
csub (a, b) (c, d) = (a - c, b - d)

cmulR :: !(Real, Real) !(Real, Real) -> (Real, Real)
cmulR (a, b) (c, d) = (a*c - b*d, a*d + b*c)

cinvR :: !(Real, Real) -> (Real, Real)
cinvR (a, b) = let m = a*a + b*b in (a/m, ~b/m)

cdivR :: !(Real, Real) !(Real, Real) -> (Real, Real)
cdivR a b = cmulR a (cinvR b)

cmagR :: !(Real, Real) -> Real
cmagR (a, b) = sqrt (a*a + b*b)

// Aberth-Ehrlich iteration
aberthIterate :: !(Poly Rational) ![(Real, Real)] !Int -> [(Real, Real)]
aberthIterate _ zs 0 = zs
aberthIterate p zs iters
    # zs` = [aberthStep p zs k \\ k <- [0 .. length zs - 1]]
    # maxCorr = maxList [cmagR (csub (zs` !! k) (zs !! k)) \\ k <- [0 .. length zs - 1]]
    | maxCorr < 0.000000000000001 = zs`
    = aberthIterate p zs` (iters - 1)

aberthStep :: !(Poly Rational) ![(Real, Real)] !Int -> (Real, Real)
aberthStep p zs k
    # zk = zs !! k
    # fz = evalPolyC p zk
    # fpz = evalPolyC (diffPoly p) zk
    # ratio = cdivR fz fpz
    # sumTerm = foldl (\acc j -> if (j == k) acc
                        (cadd acc (cinvR (csub zk (zs !! j)))))
                (0.0, 0.0) [0 .. length zs - 1]
    # denom = csub (1.0, 0.0) (cmulR ratio sumTerm)
    # correction = cdivR ratio denom
    = csub zk correction

maxList :: ![Real] -> Real
maxList [x] = x
maxList [x:xs] = max x (maxList xs)
maxList [] = 0.0

// ─── Rational root test ───
hasRationalRoot :: !(Poly Rational) -> Bool
hasRationalRoot p = not (isEmpty (rationalRoots p))

// ─── Perfect square test ───
isSquareRational :: !Rational -> Bool
isSquareRational r
    | r < zero = False
    | r == zero = True
    # n = toInt (numer r)
    # d = toInt (denom r)
    = isSquareInt (abs n) && isSquareInt (abs d)

isSquareInt :: !Int -> Bool
isSquareInt 0 = True
isSquareInt n
    # s = toInt (sqrt (toReal n))
    = s * s == n || (s + 1) * (s + 1) == n

// ─── Discriminant ───
// disc(f) = (-1)^(n(n-1)/2) * res(f, f') / a_n
discriminantOf :: !(Poly Rational) -> Rational
discriminantOf p
    | degree p <= 0 = zero
    # fp = diffPoly p
    # res = polyResultant p fp
    # an = case leadCoeff p of
        Just c -> c
        Nothing -> one
    # n = degree p
    # sign = if (isOdd (n * (n - 1) / 2)) (~ one) one
    = sign * res / an

// ─── Permutations of a list ───
perms :: ![a] -> [[a]]
perms [] = [[]]
perms xs = [[x : rest] \\ (x, ys) <- picks xs, rest <- perms ys]

picks :: ![a] -> [(a, [a])]
picks [] = []
picks [y:ys] = [(y, ys) : [(z, [y:zs]) \\ (z, zs) <- picks ys]]
