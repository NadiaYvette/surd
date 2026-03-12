{- | Top-level interface for solving polynomials via Galois group computation.

Extends 'algNumToRadExpr' to handle degree > 4 when the Galois group
is solvable. The pipeline:

  1. Check if the minimal polynomial is irreducible of degree > 4
  2. Identify the Galois group (currently degree 5)
  3. If solvable, construct radical expressions via Lagrange resolvents
  4. Pick the root matching the algebraic number's isolating interval

For degrees 1–4, delegates to the existing Cardano/Ferrari machinery
in 'Surd.Algebraic.Convert'.
-}
module Surd.Galois.Solve (
    solveAlgNum,
    solvePoly,
    identifyAndSolve,
)
where

import Data.Complex (Complex (..), imagPart, magnitude, realPart)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Math.Polynomial.Univariate (Poly, degree)
import Surd.Algebraic.Number (AlgNum (..), algApprox)
import Surd.Galois.Identify (GaloisResult (..), identifyGaloisGroup5)
import Surd.Galois.RadicalTower (solveViaTower)
import Surd.Galois.TransitiveGroup (tgSolvable)
import Surd.Radical.DAG (dagEvalComplex, toDAG)
import Surd.Types

{- | Try to express an algebraic number as a radical expression.

This is the extension of 'algNumToRadExpr' for degree > 4.
Currently handles degree 5 with solvable Galois group.

Returns Nothing if:
  - The degree is unsupported (> 5)
  - The Galois group is not solvable (A₅ or S₅)
  - Numerical matching fails
-}
solveAlgNum :: AlgNum -> Maybe (RadExpr Rational)
solveAlgNum a = do
    let p = anMinPoly a
        d = degree p
    if d <= 4
        then Nothing -- delegate to existing algNumToRadExpr
        else do
            -- Get all radical root expressions
            allRoots <- solvePoly p
            -- Pick the one matching the isolating interval
            let approxVal = fromRational (algApprox (1 / (10 ^ (15 :: Int))) a) :: Double
            pickClosestReal allRoots approxVal

{- | Solve an irreducible polynomial, returning radical expressions
for all roots (or Nothing if unsolvable/unsupported).
-}
solvePoly :: Poly Rational -> Maybe [RadExpr Rational]
solvePoly f = do
    result <- identifyAndSolve f
    Just (snd result)

{- | Identify the Galois group and solve if solvable.
Returns the group name and radical expressions for all roots.
-}
identifyAndSolve :: Poly Rational -> Maybe (String, [RadExpr Rational])
identifyAndSolve f
    | degree f /= 5 = Nothing
    | otherwise = do
        gr <- identifyGaloisGroup5 f
        let tg = grGroup gr
        if not (tgSolvable tg)
            then Nothing
            else do
                roots <- solveViaTower gr f
                Just (show (grGroup gr), roots)

{- | Pick the radical expression whose numerical value is closest
to the target (real) value.
-}
pickClosestReal :: [RadExpr Rational] -> Double -> Maybe (RadExpr Rational)
pickClosestReal exprs target =
    let scored = [(e, evalDist e target) | e <- exprs]
        valid = [(e, d) | (e, d) <- scored, d < 1e-4]
     in case valid of
            [] ->
                -- All expressions might be complex; try anyway
                let best = minimumBy (comparing snd) scored
                 in if snd best < 1.0 then Just (fst best) else Nothing
            _ -> Just (fst $ minimumBy (comparing snd) valid)

evalDist :: RadExpr Rational -> Double -> Double
evalDist e target =
    let v = dagEvalComplex (toDAG e)
     in if abs (imagPart v) < 1e-6
            then abs (realPart v - target)
            else magnitude (v - (target :+ 0))
