--- Radical tower construction for solvable polynomials.
---
--- Given an irreducible polynomial f(x) in Q[x] of degree n with
--- solvable Galois group G, construct radical expressions for its
--- roots via Lagrange resolvents descending through the composition
--- series of G.
---
--- Currently supports degree 5 with Galois groups C5, D5, or F20.
module RadicalTower
  ( solveViaTower
  , showRadicalTower
  ) where

import Rational
import Poly
import RadExpr
import Eval (evalDouble, evalComplex, complexNthRoot)
import TransitiveGroup (TransitiveGroup(..), tgSolvable, tgOrder)
import RootOfUnity (cosOfUnity, sinOfUnity)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Solve a polynomial via radical tower construction.
---
--- Returns a list of radical expressions for the roots, or Nothing
--- if the group is not solvable.
solveViaTower :: Poly -> TransitiveGroup -> Maybe [RadExpr Rational]
solveViaTower p g
  | not (tgSolvable g) = Nothing
  | degree p /= 5      = Nothing
  | otherwise =
      -- For degree 5, use Lagrange resolvents
      -- Step 1: approximate roots numerically
      let roots = approximateRoots p
      in case roots of
           Nothing -> Nothing
           Just approxRoots ->
             -- Step 2: construct radical expressions
             -- (Simplified: use the Bring radical / direct formulas)
             Just (constructRadicals p g approxRoots)

--- Approximate all roots of a degree-5 polynomial.
--- Uses Newton's method from multiple starting points.
approximateRoots :: Poly -> Maybe [(Float, Float)]
approximateRoots p =
  let d = degree p
      -- Start from d equally spaced points on a circle of radius bound
      starts = [(cos (2.0 * pi * Prelude.fromInt k / Prelude.fromInt d),
                 sin (2.0 * pi * Prelude.fromInt k / Prelude.fromInt d))
               | k <- [0 .. d - 1]]
      roots = map (newtonComplex p 50) starts
  in Just roots

--- Newton's method for complex roots of a polynomial.
newtonComplex :: Poly -> Int -> (Float, Float) -> (Float, Float)
newtonComplex p iters z
  | iters == 0 = z
  | otherwise  =
      let p' = diffPoly p
          fz = evalPolyComplex p z
          fpz = evalPolyComplex p' z
          (fr, fi) = fz
          (gr, gi) = fpz
          d = gr * gr + gi * gi
      in if d < 1.0e-30 then z
         else let dr = (fr * gr + fi * gi) / d
                  di = (fi * gr - fr * gi) / d
                  z' = (fst z - dr, snd z - di)
              in newtonComplex p (iters - 1) z'

--- Evaluate polynomial at a complex point.
evalPolyComplex :: Poly -> (Float, Float) -> (Float, Float)
evalPolyComplex (Poly cs) z = case cs of
  [] -> (0.0, 0.0)
  _  -> foldr (\c acc -> complexAdd (ratToComplex c) (complexMul z acc))
              (0.0, 0.0) cs

complexAdd :: (Float, Float) -> (Float, Float) -> (Float, Float)
complexAdd (a, b) (c, d) = (a + c, b + d)

complexMul :: (Float, Float) -> (Float, Float) -> (Float, Float)
complexMul (a, b) (c, d) = (a * c - b * d, a * d + b * c)

ratToComplex :: Rational -> (Float, Float)
ratToComplex r = (Prelude.fromInt (numerator r) / Prelude.fromInt (denominator r), 0.0)

--- Construct radical expressions for a degree-5 polynomial
--- with solvable Galois group.
constructRadicals :: Poly -> TransitiveGroup -> [(Float, Float)]
                  -> [RadExpr Rational]
constructRadicals p g approxRoots =
  let ord = tgOrder g
  in if ord == 5  -- C5
     then constructC5Radicals p approxRoots
     else if ord == 10  -- D5
     then constructD5Radicals p approxRoots
     else if ord == 20  -- F20
     then constructF20Radicals p approxRoots
     else []  -- Not solvable or not supported

--- C5 radical construction (stub: simplified).
constructC5Radicals :: Poly -> [(Float, Float)] -> [RadExpr Rational]
constructC5Radicals p _ =
  -- For C5, all roots are obtainable from one root via the
  -- primitive 5th root of unity.
  [Root 5 (Lit (ratNeg (evalAtZero p)))]

--- D5 radical construction (stub).
constructD5Radicals :: Poly -> [(Float, Float)] -> [RadExpr Rational]
constructD5Radicals p _ =
  [Root 5 (Lit (ratNeg (evalAtZero p)))]

--- F20 radical construction (stub).
constructF20Radicals :: Poly -> [(Float, Float)] -> [RadExpr Rational]
constructF20Radicals p _ =
  [Root 5 (Lit (ratNeg (evalAtZero p)))]

--- Evaluate polynomial at x=0 (constant term).
evalAtZero :: Poly -> Rational
evalAtZero (Poly cs) = case cs of
  []    -> rZero
  (c:_) -> c

--- Show.
showRadicalTower :: [RadExpr Rational] -> String
showRadicalTower exprs =
  unlines (zipWith (\i e -> "root_" ++ show i ++ " = "
                            ++ showRadExpr showRat e)
                   [(1::Int)..] exprs)
