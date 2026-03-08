-- | Exact symbolic evaluation of trigonometric functions at
-- rational multiples of π.
--
-- Every trig value at a rational multiple of π can be expressed in
-- radicals, since cyclotomic extensions have abelian (hence solvable)
-- Galois groups. The radical expressions may involve complex
-- intermediates (the casus irreducibilis) but the final cos/sin
-- values are always real.
--
-- The primary entry points are 'cosExact' and 'sinExact', which
-- compute cos(pπ/q) and sin(pπ/q) as radical expressions.
-- A 'MinPoly' fallback is provided for cases not yet handled
-- by the radical machinery.
module Surd.Trig
  ( cosExact
  , sinExact
  , tanExact
  , cosMinPoly
  , TrigResult(..)
  ) where

import Surd.Types
import Surd.Trig.RootOfUnity (cosOfUnity)
import Surd.Polynomial.Univariate (Poly)
import Surd.Polynomial.Cyclotomic (cyclotomic)
import Surd.Radical.Normalize (normalize)

-- | Result of exact trig evaluation.
data TrigResult
  = Radical (RadExpr Rational)
    -- ^ Exact radical expression (may involve complex intermediates
    -- like @√(-3)@ for non-constructible angles, but the final
    -- value is real)
  | MinPoly (Poly Rational)
    -- ^ Minimal polynomial (fallback when radical computation is
    -- not yet implemented for this case)
  deriving (Show)

-- | Compute cos(pπ/q) exactly.
--
-- Reduces to cos(2π/n) via standard identities, then uses the
-- root-of-unity machinery (Gauss period descent).
--
-- >>> cosExact 1 3   -- cos(π/3) = 1/2
-- Radical (Lit (1 % 2))
-- >>> cosExact 1 4   -- cos(π/4) = √2/2
-- Radical (Mul (Inv (Lit (2 % 1))) (Root 2 (Lit (2 % 1))))
cosExact :: Integer -> Integer -> TrigResult
cosExact p q
  | q <= 0    = error "cosExact: non-positive denominator"
  | otherwise =
      let g = gcd (abs p) q
          p' = p `div` g
          q' = q `div` g
      in cosReduced p' q'

-- | Compute sin(pπ/q) exactly.
sinExact :: Integer -> Integer -> TrigResult
sinExact p q
  | q <= 0    = error "sinExact: non-positive denominator"
  | otherwise =
      let g = gcd (abs p) q
          p' = p `div` g
          q' = q `div` g
      in sinReduced p' q'

-- | Compute tan(pπ/q) exactly, as sin/cos.
tanExact :: Integer -> Integer -> Maybe TrigResult
tanExact p q =
  case (sinExact p q, cosExact p q) of
    (Radical s, Radical c) ->
      Just $ Radical $ Mul s (Inv c)
    _ -> Nothing

cosReduced :: Integer -> Integer -> TrigResult
cosReduced p q =
  let p' = p `mod` (2 * q)
      p'' = if p' < 0 then p' + 2 * q else p'
  in cosInRange p'' q

sinReduced :: Integer -> Integer -> TrigResult
sinReduced p q =
  -- sin(pπ/q) = cos(π/2 - pπ/q) = cos((q - 2p)π/(2q))
  cosExact (q - 2 * p) (2 * q)

-- | cos(pπ/q) where 0 ≤ p ≤ 2q (i.e., angle in [0, 2π]).
cosInRange :: Integer -> Integer -> TrigResult
cosInRange p q
  | p == 0         = Radical (Lit 1)
  | 2 * p == q     = Radical (Lit 0)
  | p == q         = Radical (Lit (-1))
  | 2 * p == 3 * q = Radical (Lit 0)
  | 2 * p > q && p < q =
      case cosInRange (q - p) q of
        Radical e -> Radical (Neg e)
        other     -> other
  | p > q && 2 * p < 3 * q =
      case cosInRange (p - q) q of
        Radical e -> Radical (Neg e)
        other     -> other
  | 2 * p >= 3 * q =
      cosInRange (2 * q - p) q
  | otherwise = cosFirstQuadrant p q

cosFirstQuadrant :: Integer -> Integer -> TrigResult
cosFirstQuadrant p q =
  let g = gcd p (2 * q)
      n = fromIntegral (2 * q `div` g) :: Int
      k = fromIntegral (p `div` g) :: Int
  in if k == 1
     then
       case cosOfUnity n of
         Just e  -> Radical (normalize e)
         Nothing -> MinPoly (cyclotomic n)
     else
       case cosOfUnity n of
         Just base ->
           let cheb = chebyshev k base
           in Radical (normalize cheb)
         Nothing -> MinPoly (cyclotomic n)

-- | Chebyshev polynomial evaluation: T_k(x) computed symbolically.
-- T_0(x) = 1, T_1(x) = x, T_{n+1}(x) = 2x·T_n(x) - T_{n-1}(x)
chebyshev :: Int -> RadExpr Rational -> RadExpr Rational
chebyshev 0 _ = Lit 1
chebyshev 1 x = x
chebyshev k x = go 2 (Lit 1) x
  where
    go n t0 t1
      | n > k     = t1
      | otherwise  =
          let t2 = Add (Mul (Mul (Lit 2) x) t1) (Neg t0)
          in go (n + 1) t1 t2

-- | Return the minimal polynomial of cos(2π/n).
--
-- Note: this returns the cyclotomic polynomial Φₙ(x), which is an
-- annihilating polynomial but may not be the true minimal polynomial
-- of cos(2π/n) (which has degree φ(n)/2 for the real part).
cosMinPoly :: Int -> Poly Rational
cosMinPoly = cyclotomic
