-- |
-- Module      : Surd.Radical.Denest
-- Description : Top-level radical denesting dispatcher
-- Stability   : experimental
--
-- Provides the main entry point for radical denesting. The dispatcher
-- applies the appropriate algorithm based on the root index and
-- expression structure:
--
-- 1. For square roots (@n = 2@): tries "Surd.Radical.Denest.Sqrt"
--    (Borodin's algorithm for @sqrt(a + b*sqrt r)@) first, then falls
--    back to Landau's algorithm.
-- 2. For higher roots: tries "Surd.Radical.Denest.NthRoot" (cube root
--    denesting, perfect power extraction, root composition) first,
--    then falls back to Landau's algorithm.
-- 3. Landau's algorithm ("Surd.Radical.Denest.Landau") is the most
--    general fallback: it factors @x^n - a@ over the radical extension
--    field using Trager's algorithm and extracts lower-degree roots.
--
-- All algorithms are applied recursively, bottom-up through the
-- expression tree.
module Surd.Radical.Denest
  ( denest,
    denestFull,
  )
where

import Surd.Radical.Denest.Landau (denestRadical)
import Surd.Radical.Denest.NthRoot (denestNthRoot)
import Surd.Radical.Denest.Sqrt (denestSqrt)
import Surd.Radical.Eval (evalExact)
import Surd.Radical.Normalize (normalize)
import Surd.Types

-- | Denest a radical expression: normalize first, then apply all
-- available denesting algorithms recursively.
--
-- This is the primary entry point for denesting. It normalizes the
-- expression first (to canonicalize like terms, extract perfect powers,
-- etc.) and then runs the full denesting pass.
denest :: RadExpr Rational -> RadExpr Rational
denest = denestFull . normalize

-- | Full denesting pass without pre-normalization.
--
-- Applies specialized algorithms first (Borodin for square roots,
-- cube root denesting for cubic roots), then falls back to Landau's
-- general algorithm. Results are verified via exact real evaluation
-- to ensure correctness.
denestFull :: RadExpr Rational -> RadExpr Rational
denestFull expr = case expr of
  Root 2 inner ->
    let specialized = denestSqrt expr
     in if changed specialized expr
          then specialized
          else tryLandau 2 inner expr
  Root n inner ->
    let specialized = denestNthRoot expr
     in if changed specialized expr
          then specialized
          else tryLandau n inner expr
  Neg a -> Neg (denestFull a)
  Add a b -> Add (denestFull a) (denestFull b)
  Mul a b -> Mul (denestFull a) (denestFull b)
  Inv a -> Inv (denestFull a)
  Pow a n -> Pow (denestFull a) n
  e -> e
  where
    -- Try Landau denesting as a fallback when specialized algorithms don't help
    tryLandau n inner original =
      let inner' = denestFull inner
       in case denestRadical n inner' of
            Just denested ->
              let dv = evalExact denested
                  ov = evalExact original
               in if abs (dv - ov) < 1e-40
                    then denestFull denested -- recurse on result
                    else original
            Nothing -> case original of
              Root _ _ -> Root n (denestFull inner)
              _ -> original
    -- Check if denesting actually changed anything
    changed new old = show new /= show old
