-- | Top-level radical denesting dispatcher.
--
-- Applies the appropriate denesting algorithm based on the
-- root index and expression structure.
module Surd.Radical.Denest
  ( denest
  , denestFull
  ) where

import Surd.Types
import Surd.Radical.Denest.Sqrt (denestSqrt)
import Surd.Radical.Denest.NthRoot (denestNthRoot)
import Surd.Radical.Denest.Landau (denestRadical)
import Surd.Radical.Normalize (normalize)
import Surd.Radical.Eval (evalExact)

-- | Denest a radical expression: apply all available denesting
-- algorithms recursively.
denest :: RadExpr Rational -> RadExpr Rational
denest = denestFull . normalize

-- | Full denesting pass without pre-normalization.
denestFull :: RadExpr Rational -> RadExpr Rational
denestFull expr = case expr of
  Root 2 inner ->
    let specialized = denestSqrt expr
    in if changed specialized expr then specialized
       else tryLandau 2 inner expr
  Root n inner ->
    let specialized = denestNthRoot expr
    in if changed specialized expr then specialized
       else tryLandau n inner expr
  Neg a    -> Neg (denestFull a)
  Add a b  -> Add (denestFull a) (denestFull b)
  Mul a b  -> Mul (denestFull a) (denestFull b)
  Inv a    -> Inv (denestFull a)
  Pow a n  -> Pow (denestFull a) n
  e        -> e
  where
    -- Try Landau denesting as a fallback when specialized algorithms don't help
    tryLandau n inner original =
      let inner' = denestFull inner
      in case denestRadical n inner' of
           Just denested ->
             let dv = evalExact denested
                 ov = evalExact original
             in if abs (dv - ov) < 1e-40
                then denestFull denested  -- recurse on result
                else original
           Nothing -> case original of
             Root _ _ -> Root n (denestFull inner)
             _        -> original
    -- Check if denesting actually changed anything
    changed new old = show new /= show old
