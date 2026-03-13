--- Resultant computation and related polynomial operations.
---
--- Implements the subresultant PRS (polynomial remainder sequence)
--- for computing resultants of univariate polynomials over Q.
module Resultant
  ( polyResultant
  , composedSum
  , composedProduct
  , negateVar
  , reciprocalPoly
  , shiftPoly
  , lagrangeInterpolate
  ) where

import Rational
import Poly

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Resultant of two univariate polynomials via the Euclidean algorithm.
polyResultant :: Poly -> Poly -> Rational
polyResultant f g
  | degree f < 0 = rZero
  | degree g < 0 = rZero
  | degree g == 0 =
      case leadCoeff g of
        Just c  -> ratPow c (degree f)
        Nothing -> rZero
  | otherwise =
      let (_, r) = divModPoly f g
          df = degree f
          dg = degree g
          lg = case leadCoeff g of
                 Just c  -> c
                 Nothing -> rOne
          dr = degree r
          s = if odd (df * dg) then Rational.fromInt (negate 1) else rOne
      in if degree r < 0
         then rZero
         else ratMul s (ratMul (ratPow lg (df - dr)) (polyResultant g r))

--- Negate the variable: p(x) -> p(-x).
negateVar :: Poly -> Poly
negateVar (Poly cs) =
  mkPoly (zipWith (\i c -> if odd i then ratNeg c else c) [(0::Int)..] cs)

--- Reciprocal polynomial: x^n * p(1/x).
reciprocalPoly :: Poly -> Poly
reciprocalPoly (Poly cs) = mkPoly (reverse cs)

--- Shift polynomial: p(x) -> p(x + a).
shiftPoly :: Rational -> Poly -> Poly
shiftPoly a p = composePoly p (addPoly monoX (constPoly a))

--- Composed sum: if p has roots alpha_i and q has roots beta_j,
--- the composed sum has roots alpha_i + beta_j.
---
--- Uses Res_y(p(y), q(x - y)).
composedSum :: Poly -> Poly -> Poly
composedSum p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      nPts = resultDeg + 1
      pts = map Rational.fromInt [0 .. nPts - 1]
      vals = map (\x0 -> polyResultant p (shiftPoly (ratNeg x0) q)) pts
      pairs = zip pts vals
  in monicPoly (lagrangeInterpolate pairs)

--- Composed product: if p has roots alpha_i and q has roots beta_j,
--- the composed product has roots alpha_i * beta_j.
---
--- Uses Res_y(y^deg(p) * p(1/y), q(x / y)) evaluated at enough points.
composedProduct :: Poly -> Poly -> Poly
composedProduct p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      nPts = resultDeg + 1
      pts = map (\k -> Rational.fromInt (k + 1)) [0 .. nPts - 1]
      vals = map (\x0 -> computeCompProdAt p q x0) pts
      pairs = zip pts vals
  in monicPoly (lagrangeInterpolate pairs)

--- Evaluate the composed product resultant at a single point.
computeCompProdAt :: Poly -> Poly -> Rational -> Rational
computeCompProdAt p q x0 =
  let rp = reciprocalPoly p
      -- q(x0/y): substitute x -> x0/y in q.
      -- We compute res_y(y^dp * p(1/y), y^dq * q(x0/y))
      -- which gives us the resultant we need, up to scaling.
      dq = degree q
      -- Direct evaluation: Res_y(p(y), q(x0/y) * y^dq)
      -- For simplicity, evaluate p at roots of q(x0/y)*y^dq
      -- Actually, use: Res_y(rp_scaled, q_sub)
      qSub = composeWithXoverY q x0 dq
  in polyResultant rp qSub

--- Compose q(x0/y) * y^dq to get a polynomial in y.
composeWithXoverY :: Poly -> Rational -> Int -> Poly
composeWithXoverY (Poly cs) x0 dq =
  let -- q(t) = sum c_k t^k, q(x0/y) * y^dq = sum c_k x0^k y^(dq-k)
      terms = zipWith (\k c -> (dq - k, ratMul c (ratPow x0 k)))
                       [0..] cs
      maxDeg = if null terms then 0 else foldl1 max (map fst terms)
      arr = replicate (maxDeg + 1) rZero
  in mkPoly (foldl (\acc (idx, c) -> setAt idx (ratAdd (getAt idx acc) c) acc)
                    arr terms)

--- Get element at index.
getAt :: Int -> [a] -> a
getAt i xs = case xs of
  []    -> error "getAt: index out of range"
  (x:rest) -> if i == 0 then x else getAt (i - 1) rest

--- Set element at index.
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = case xs of
  []       -> []
  (x:rest) -> if i == 0 then v : rest else x : setAt (i - 1) v rest

--- Lagrange interpolation: given points (x_i, y_i), compute the unique
--- polynomial of degree <= n passing through all points.
lagrangeInterpolate :: [(Rational, Rational)] -> Poly
lagrangeInterpolate points =
  foldl addPoly zeroPoly (map termForPoint points)
  where
    xs = map fst points
    termForPoint (xi, yi) =
      scalePoly yi (lagrangeBasis xi (filter (\xj -> xj /= xi) xs))
    lagrangeBasis xi others =
      foldl mulPoly (constPoly rOne)
        (map (\xj -> scalePoly (ratDiv rOne (ratSub xi xj))
                      (mkPoly [ratNeg xj, rOne]))
             others)
