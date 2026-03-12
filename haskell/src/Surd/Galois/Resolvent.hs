{- | Resolvent polynomials for Galois group identification.

Given an irreducible polynomial f(x) of degree n over Q, this module
provides tools to compute resolvent polynomials whose factorization
patterns reveal the structure of the Galois group Gal(f/Q).

The main resolvents implemented:
  - Discriminant: disc(f) is a square ⟺ Gal(f) ≤ Aₙ
  - Sextic resolvent (degree 5): identifies F₂₀ and its subgroups
-}
module Surd.Galois.Resolvent (
    complexRootsOf,
    resolventFromRoots,
    hasRationalRoot,
    isSquareRational,
    discriminantOf,
)
where

import Data.Complex (Complex (..), magnitude)
import Data.Ratio (denominator, numerator, (%))
import Math.Polynomial.Factoring (rationalRoots)
import Math.Polynomial.Resultant (polyResultant)
import Math.Polynomial.Univariate

------------------------------------------------------------------------
-- Numerical root finding via Aberth–Ehrlich
------------------------------------------------------------------------

-- | Approximate all complex roots of a polynomial over Q.
complexRootsOf :: Poly Rational -> [Complex Double]
complexRootsOf p
    | degree p <= 0 = []
    | degree p == 1 =
        case map fromRational (unPoly p) of
            [c0, c1] -> [negate (c0 / c1) :+ 0]
            _ -> []
    | otherwise = aberthEhrlich p

aberthEhrlich :: Poly Rational -> [Complex Double]
aberthEhrlich poly =
    let n = degree poly
        r = cauchyBound poly
        -- Initial guesses on a circle, slightly perturbed
        z0 =
            [ mkPolar' r (2 * pi * fromIntegral k / fromIntegral n + 0.37)
            | k <- [0 .. n - 1]
            ]
     in go z0 (500 :: Int)
  where
    mkPolar' radius angle = (radius * cos angle) :+ (radius * sin angle)

    go zs 0 = zs
    go zs iter =
        let zs' = step zs
            maxShift = maximum [magnitude (a - b) | (a, b) <- zip zs zs']
         in if maxShift < 1e-15 then zs' else go zs' (iter - 1)

    step zs =
        [ let fz = evalC poly z
              f'z = evalC (diffPoly poly) z
           in if magnitude f'z < 1e-300
                then z
                else
                    let w = fz / f'z
                        s = sum [1 / (z - zj) | (j, zj) <- zip [0 :: Int ..] zs, j /= i]
                        denom = 1 - w * s
                     in if magnitude denom < 1e-300
                            then z - w
                            else z - w / denom
        | (i, z) <- zip [0 ..] zs
        ]

evalC :: Poly Rational -> Complex Double -> Complex Double
evalC p z =
    -- Horner's method: start from leading coefficient
    foldl' (\acc c -> (fromRational c :+ 0) + acc * z) (0 :+ 0) (reverse (unPoly p))

cauchyBound :: Poly Rational -> Double
cauchyBound p =
    let cs = unPoly p
        n = length cs - 1
        an = abs (fromRational (cs !! n) :: Double)
     in 1 + maximum [abs (fromRational (cs !! i)) / an | i <- [0 .. n - 1]]

------------------------------------------------------------------------
-- Resolvent construction from numerical roots
------------------------------------------------------------------------

{- | Compute a resolvent polynomial from numerical roots and an invariant.

Given:
  - Numerical roots α₁, ..., αₙ of f
  - An invariant function θ(x₁, ..., xₙ)
  - Permutations σ₁, ..., σₘ (as lists of indices) representing
    coset representatives

Returns the polynomial ∏ᵢ (x - θ(α_{σᵢ(1)}, ..., α_{σᵢ(n)}))
with rational coefficients (rounded from numerical values).
-}
resolventFromRoots ::
    [Complex Double] ->
    ([Complex Double] -> Complex Double) ->
    [[Int]] ->
    Maybe (Poly Rational)
resolventFromRoots roots theta perms =
    let values = [theta [roots !! j | j <- sigma] | sigma <- perms]
        polyC = polyFromRootsC values
     in roundPolyC polyC

-- | Build ∏(x - rᵢ) from complex values, expanding the product.
polyFromRootsC :: [Complex Double] -> [Complex Double]
polyFromRootsC = foldl mulLinear [1 :+ 0]
  where
    -- Multiply polynomial (ascending coeffs) by (x - r)
    mulLinear cs r =
        let shifted = (0 :+ 0) : cs
            scaled = map (negate r *) cs ++ [0 :+ 0]
         in zipWith (+) shifted scaled

{- | Round complex polynomial coefficients to rationals.
Fails if imaginary parts are too large.
-}
roundPolyC :: [Complex Double] -> Maybe (Poly Rational)
roundPolyC cs = do
    rs <- mapM roundCoeff cs
    Just (mkPoly rs)
  where
    roundCoeff (r :+ i)
        | abs i > 1e-4 * max 1 (abs r) = Nothing
        | otherwise = Just (bestRational r)

-- | Find the rational with smallest denominator (≤ 10000) closest to x.
bestRational :: Double -> Rational
bestRational x =
    let candidates =
            [ (abs (fromIntegral n / fromIntegral d - x), n % d)
            | d <- [1 :: Integer .. 10000]
            , let n = round (x * fromIntegral d) :: Integer
            ]
        best = snd $ minimum candidates
     in best

------------------------------------------------------------------------
-- Discriminant and rational tests
------------------------------------------------------------------------

{- | Discriminant of a polynomial.
disc(f) = (-1)^{n(n-1)/2} · Res(f, f') / aₙ
-}
discriminantOf :: Poly Rational -> Rational
discriminantOf f =
    let n = degree f
        cs = unPoly f
        lc = last cs
        f' = diffPoly f
        res = polyResultant f f'
        sign = if even (n * (n - 1) `div` 2) then 1 else -1
     in sign * res / lc

-- | Check if a rational number is a perfect square in Q.
isSquareRational :: Rational -> Bool
isSquareRational r
    | r < 0 = False
    | r == 0 = True
    | otherwise =
        let n = abs (numerator r)
            d = denominator r
         in isSquareInteger n && isSquareInteger d

isSquareInteger :: Integer -> Bool
isSquareInteger n
    | n < 0 = False
    | n == 0 = True
    | otherwise =
        let s = integerSquareRoot n
         in s * s == n

integerSquareRoot :: Integer -> Integer
integerSquareRoot n = go (max 1 (floor (sqrt (fromIntegral n :: Double))))
  where
    go x =
        let x' = (x + n `div` x) `div` 2
         in if x' >= x then x else go x'

-- | Check if a polynomial has a rational root.
hasRationalRoot :: Poly Rational -> Bool
hasRationalRoot p = not (null (rationalRoots p))
