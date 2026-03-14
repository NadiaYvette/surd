--- Top-level interface for solving polynomials via Galois group computation.
---
--- For degree <= 4, returns Nothing (caller delegates to Cardano/Ferrari).
--- For prime degree >= 5, identifies the Galois group and, if solvable,
--- solves via radical tower.
module GaloisSolve
  ( solveAlgNum
  , solvePoly
  , identifyAndSolve
  ) where

import Rational
import Poly
import RadExpr
import Eval (evalDouble)
import AlgNum (AlgNum(..), algMinPoly, algApprox)
import Identify (GaloisResult(..), identifyGaloisGroup)
import RadicalTower (solveViaTowerN)
import TransitiveGroup (tgSolvable)

--- Local alias.
rZero :: Rational
rZero = Rational.fromInt 0

--- Solve an algebraic number: if its minimal polynomial has solvable
--- Galois group, express it as a radical.
solveAlgNum :: AlgNum -> Maybe (RadExpr Rational)
solveAlgNum a =
  let p = algMinPoly a
      d = degree p
  in if d <= 4
     then Nothing  -- Caller uses quadratic/Cardano/Ferrari
     else case identifyAndSolve p of
            Just roots ->
              -- Pick the root closest to our algebraic number's approximation
              let approx = algApprox a
                  scored = map (\r -> (r, absF (evalDouble r - approx))) roots
              in case scored of
                   [] -> Nothing
                   _  -> Just (fst (foldl1 (\(r1,d1) (r2,d2) ->
                            if d1 <= d2 then (r1,d1) else (r2,d2)) scored))
            Nothing -> Nothing

--- Solve a polynomial: return radical expressions for all roots,
--- or Nothing if not solvable by radicals.
solvePoly :: Poly -> Maybe [RadExpr Rational]
solvePoly = identifyAndSolve

--- Identify Galois group and solve if solvable.
---
--- Handles all prime degrees >= 5 via generalized Galois group
--- identification and Lagrange resolvent descent.
identifyAndSolve :: Poly -> Maybe [RadExpr Rational]
identifyAndSolve p
  | degree p <= 4 = Nothing
  | otherwise =
      case identifyGaloisGroup p of
        Identified g ->
          if tgSolvable g
          then solveViaTowerN p g
          else Nothing
        NotSupported _ -> Nothing

--- Absolute value of Float.
absF :: Float -> Float
absF x = if x < 0.0 then negate x else x
