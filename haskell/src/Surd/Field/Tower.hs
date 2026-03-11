-- | Tower of algebraic extensions.
--
-- A field tower Q ⊂ Q(α₁) ⊂ Q(α₁)(α₂) ⊂ ... is built by iterated
-- simple extensions. Using the 'Num'/'Fractional' instances on 'ExtElem',
-- towers are represented as nested types:
--
-- @
-- type K0 = Rational
-- type K1 = ExtElem K0       -- Q(α₁)
-- type K2 = ExtElem K1       -- Q(α₁)(α₂)
-- @
--
-- This module provides utilities for constructing towers from
-- radical expressions: each radical (nth root of a known algebraic
-- number) introduces a new extension layer.
module Surd.Field.Tower
  ( -- * Tower construction
    adjoinRoot,
    adjoinSqrt,

    -- * Evaluation in extension fields
    evalInField,
  )
where

import Math.Field.Extension
import Math.Polynomial.Univariate
import Surd.Types

-- | Adjoin an nth root to a field: given an element r ∈ K and
-- a degree n, construct the extension K(α) where α = ⁿ√r.
--
-- The minimal polynomial is xⁿ - r (which may not be irreducible,
-- but works for our purposes since reduction mod (xⁿ - r) still
-- gives correct arithmetic when r is a perfect nth power — the
-- element just simplifies).
--
-- Returns the new field and the element α (the generator).
adjoinRoot ::
  (Eq k, Fractional k) =>
  -- | root degree n
  Int ->
  -- | radicand r (element of current field)
  k ->
  (ExtField k, ExtElem k)
adjoinRoot n r =
  let -- Minimal polynomial: x^n - r
      minPoly = mkPoly $ [-r] ++ replicate (n - 1) 0 ++ [1]
      field = mkExtField minPoly ("α" ++ show n)
   in (field, generator field)

-- | Convenience: adjoin a square root.
adjoinSqrt :: (Eq k, Fractional k) => k -> (ExtField k, ExtElem k)
adjoinSqrt = adjoinRoot 2

-- | Evaluate a 'RadExpr' in a field, given a way to embed rationals
-- and resolve radicals.
--
-- This is the key function for converting radical expressions into
-- elements of extension fields. Each 'Root' node may extend the
-- field further (handled by the caller via 'adjoinRoot').
--
-- For a fixed tower (all radicals already adjoined), use this with
-- a lookup that maps each radical to its corresponding generator.
evalInField ::
  (Eq k, Fractional k) =>
  -- | embed a rational
  (Rational -> k) ->
  -- | resolve Root n x (given evaluated radicand)
  (Int -> k -> k) ->
  RadExpr Rational ->
  k
evalInField embedR resolveRoot = go
  where
    go (Lit r) = embedR r
    go (Neg a) = negate (go a)
    go (Add a b) = go a + go b
    go (Mul a b) = go a * go b
    go (Inv a) = recip (go a)
    go (Root n a) = resolveRoot n (go a)
    go (Pow a n)
      | n >= 0 = go a ^ n
      | otherwise = recip (go a ^ negate n)

-- | Evaluate a 'RadExpr' in a simple extension Q(α₁,...,αₖ),
-- where each αᵢ is a root of some element in the previous field.
-- This requires that all necessary roots have already been adjoined.
--
-- For the common case of a single-layer extension Q(ⁿ√r):
--
-- @
-- let (field, alpha) = adjoinRoot n (r :: Rational)
--     embedQ x = embed field (fromRational x)
--     resolve _ _ = alpha  -- only one radical
-- in evalInField embedQ resolve expr
-- @
