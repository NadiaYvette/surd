{- |
Module      : Surd.Galois.Resolvent
Description : Resolvent polynomials for Galois group computation
Stability   : experimental
License     : BSD-3-Clause

Resolvent polynomials are the primary tool for computing Galois groups of
polynomials over \(\mathbb{Q}\).  Given an irreducible polynomial \(f(x)\) of
degree \(n\) with roots \(\alpha_1, \ldots, \alpha_n\), an /invariant/
\(\theta(x_1, \ldots, x_n) \in \mathbb{Q}[x_1, \ldots, x_n]\), and coset
representatives \(\sigma_1, \ldots, \sigma_m\) of
\(\mathrm{Stab}_{S_n}(\theta)\) in \(S_n\), the /resolvent polynomial/ is

\[
  R_\theta(x)
    = \prod_{i=1}^{m}
        \bigl(x - \theta(\alpha_{\sigma_i(1)}, \ldots, \alpha_{\sigma_i(n)})\bigr).
\]

Because the coefficients of \(R_\theta\) are symmetric functions of the
roots, they lie in \(\mathbb{Q}\).  The key group-theoretic property is:

__Theorem.__  \(\mathrm{Gal}(f) \le \mathrm{Stab}_{S_n}(\theta)\) if and
only if \(R_\theta\) has a rational root.

The simplest resolvent is the /discriminant/:

\[
  \Delta(f) = \prod_{i < j} (\alpha_i - \alpha_j)^2.
\]

Here \(\theta = \prod_{i<j}(\alpha_i - \alpha_j)\), whose stabiliser is
\(A_n\), so \(\Delta(f)\) is a perfect square in \(\mathbb{Q}\) iff
\(\mathrm{Gal}(f) \le A_n\).

This module computes resolvents /numerically/: roots of \(f\) are
approximated in \(\mathbb{C}\) via the Aberth–Ehrlich simultaneous iteration
method, the invariant is evaluated on permuted roots, and the resulting
polynomial is reconstructed with rational coefficients via bounded-denominator
rational approximation.

== References

* Stauduhar, R. P. (1973). "The determination of Galois groups."
  /Math. Comp./ __27__(124), 981–996.
  DOI: [10.1090\/S0025-5718-1973-0327712-4](https://doi.org/10.1090/S0025-5718-1973-0327712-4)

* Aberth, O. (1973). "Iteration methods for finding all zeros of a
  polynomial simultaneously."
  /Math. Comp./ __27__(122), 339–344.
  DOI: [10.1090\/S0025-5718-1973-0329236-7](https://doi.org/10.1090/S0025-5718-1973-0329236-7)

* Ehrlich, L. W. (1967). "A modified Newton method for polynomials."
  /CACM/ __10__(2), 107–108.
  DOI: [10.1145\/363067.363115](https://doi.org/10.1145/363067.363115)

* Soicher, L. H. & McKay, J. (1985). "Computing Galois groups over the
  rationals."
  /J. Number Theory/ __20__(3), 273–281.
  DOI: [10.1016\/0022-314X(85)90022-8](https://doi.org/10.1016/0022-314X(85)90022-8)
-}
module Surd.Galois.Resolvent (
    -- * Root finding
    complexRootsOf,

    -- * Resolvent construction
    resolventFromRoots,

    -- * Discriminant and rationality tests
    discriminantOf,
    isSquareRational,
    hasRationalRoot,
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

{- | Approximate all complex roots of a polynomial over \(\mathbb{Q}\) using
the Aberth–Ehrlich simultaneous iteration method.

For a degree-\(n\) polynomial, the method starts from \(n\) initial guesses
equally spaced on a circle of radius given by the Cauchy bound
('cauchyBound'), then iterates until all corrections are below \(10^{\-15}\)
or 500 iterations are exhausted.

For simple roots the method has convergence order \(n + 1\) (cubic for
quadratics, quartic for cubics, etc.), which is significantly faster than
independent Newton iterations.

Linear polynomials are solved directly; constants return the empty list.
-}
complexRootsOf :: Poly Rational -> [Complex Double]
complexRootsOf p
    | degree p <= 0 = []
    | degree p == 1 =
        case map fromRational (unPoly p) of
            [c0, c1] -> [negate (c0 / c1) :+ 0]
            _ -> []
    | otherwise = aberthEhrlich p

{- | Aberth–Ehrlich simultaneous iteration for all roots of a polynomial.

Each step updates all \(n\) approximations in parallel via

\[
  z_k^{(t+1)}
    = z_k^{(t)}
      - \frac{f(z_k^{(t)}) / f'(z_k^{(t)})}
             {1 \;-\; \dfrac{f(z_k^{(t)})}{f'(z_k^{(t)})}
                      \displaystyle\sum_{j \neq k}
                        \frac{1}{z_k^{(t)} - z_j^{(t)}}}.
\]

The Newton quotient \(f/f'\) is corrected by the repulsion term
\(\sum_{j \neq k} 1/(z_k - z_j)\), which prevents distinct roots from
converging to the same value.
-}
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

{- | Evaluate a polynomial with rational coefficients at a complex point
using Horner's method.

Coefficients are stored in ascending order @[a₀, a₁, …, aₙ]@, so the
fold processes them from @aₙ@ down:
\(f(z) = a_0 + z(a_1 + z(a_2 + \cdots + z \cdot a_n))\).
-}
evalC :: Poly Rational -> Complex Double -> Complex Double
evalC p z =
    -- Horner's method: start from leading coefficient
    foldl' (\acc c -> (fromRational c :+ 0) + acc * z) (0 :+ 0) (reverse (unPoly p))

{- | Cauchy's upper bound on the absolute values of the roots.

For a monic polynomial \(x^n + a_{n-1}x^{n-1} + \cdots + a_0\), every
root \(\alpha\) satisfies \(|\alpha| \le 1 + \max_i |a_i / a_n|\).
The initial guesses for 'aberthEhrlich' are placed on a circle of this
radius.
-}
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

  * Numerical roots \(\alpha_1, \ldots, \alpha_n\) of \(f\) (from
    'complexRootsOf')
  * An invariant function
    \(\theta \colon \mathbb{C}^n \to \mathbb{C}\)
  * Permutations \(\sigma_1, \ldots, \sigma_m\) (as index lists)
    representing coset representatives of
    \(\mathrm{Stab}_{S_n}(\theta)\) in \(S_n\)

Returns the polynomial

\[
  R_\theta(x)
    = \prod_{i=1}^{m}
        \bigl(x - \theta(\alpha_{\sigma_i(1)}, \ldots, \alpha_{\sigma_i(n)})\bigr)
\]

with rational coefficients recovered via bounded-denominator rational
approximation ('bestRational').  Returns 'Nothing' if the imaginary parts
of the coefficients are too large to be rounding error, indicating the
invariant or permutations are incorrect.
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

{- | Build \(\prod_i (x - r_i)\) from complex values, expanding the product
incrementally.  Coefficients are in ascending order.
-}
polyFromRootsC :: [Complex Double] -> [Complex Double]
polyFromRootsC = foldl mulLinear [1 :+ 0]
  where
    -- Multiply polynomial (ascending coeffs) by (x - r)
    mulLinear cs r =
        let shifted = (0 :+ 0) : cs
            scaled = map (negate r *) cs ++ [0 :+ 0]
         in zipWith (+) shifted scaled

{- | Round complex polynomial coefficients to rationals.
Fails ('Nothing') if any imaginary part exceeds \(10^{\-4}\) times the
real part magnitude, indicating the polynomial is not over \(\mathbb{Q}\).
-}
roundPolyC :: [Complex Double] -> Maybe (Poly Rational)
roundPolyC cs = do
    rs <- mapM roundCoeff cs
    Just (mkPoly rs)
  where
    roundCoeff (r :+ i)
        | abs i > 1e-4 * max 1 (abs r) = Nothing
        | otherwise = Just (bestRational r)

{- | Bounded-denominator rational approximation.

Finds the rational \(p/q\) with \(1 \le q \le 10000\) that minimises
\(|p/q - x|\).  This is a brute-force search over denominators, sufficient
for the resolvent coefficients encountered in practice (degree \(\le 120\),
coefficients with denominators \(\le 10000\)).
-}
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

{- | Discriminant of a polynomial over \(\mathbb{Q}\).

For a polynomial \(f(x) = a_n x^n + \cdots + a_0\) of degree \(n\),

\[
  \mathrm{disc}(f)
    = (-1)^{n(n-1)/2} \cdot \frac{\mathrm{Res}(f, f')}{a_n}
\]

where \(\mathrm{Res}(f, f')\) is the resultant of \(f\) and its formal
derivative.  Equivalently, up to sign,
\(\mathrm{disc}(f) = a_n^{2n-2} \prod_{i < j} (\alpha_i - \alpha_j)^2\).

The discriminant is the simplest resolvent: it is a perfect square in
\(\mathbb{Q}\) if and only if \(\mathrm{Gal}(f) \le A_n\).
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

{- | Test whether a rational number is a perfect square in \(\mathbb{Q}\).

A rational \(p/q\) (in lowest terms) is a perfect square iff both \(p\)
and \(q\) are perfect squares in \(\mathbb{Z}\).  Negative rationals are
not squares.
-}
isSquareRational :: Rational -> Bool
isSquareRational r
    | r < 0 = False
    | r == 0 = True
    | otherwise =
        let n = abs (numerator r)
            d = denominator r
         in isSquareInteger n && isSquareInteger d

{- | Test whether a non-negative integer is a perfect square, using
Newton's method for integer square root.
-}
isSquareInteger :: Integer -> Bool
isSquareInteger n
    | n < 0 = False
    | n == 0 = True
    | otherwise =
        let s = integerSquareRoot n
         in s * s == n

{- | Integer square root via Newton's method, starting from the 'Double'
approximation.
-}
integerSquareRoot :: Integer -> Integer
integerSquareRoot n = go (max 1 (floor (sqrt (fromIntegral n :: Double))))
  where
    go x =
        let x' = (x + n `div` x) `div` 2
         in if x' >= x then x else go x'

{- | Check whether a polynomial over \(\mathbb{Q}\) has at least one rational
root, by delegating to the rational root theorem implementation in
"Math.Polynomial.Factoring".
-}
hasRationalRoot :: Poly Rational -> Bool
hasRationalRoot p = not (null (rationalRoots p))
