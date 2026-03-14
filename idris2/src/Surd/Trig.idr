module Surd.Trig

import Surd.Rational
import Surd.Types
import Surd.Poly
import Surd.Cyclotomic
import Surd.Normalize
import Surd.Eval
import Surd.RootOfUnity
import Surd.DAG
import Surd.NormalForm
import Surd.Expr
import Surd.GCD

import Data.Nat
import Data.List

%default covering

------------------------------------------------------------------------
-- Trig result type
------------------------------------------------------------------------

||| Result of exact trig evaluation.
public export
data TrigResult : Type where
  ||| Exact radical expression.
  Radical : RadExpr Rational -> TrigResult
  ||| Minimal polynomial (fallback).
  MinPoly : Poly Rational -> TrigResult

export
Show TrigResult where
  show (Radical e) = "Radical(" ++ show e ++ ")"
  show (MinPoly p) = "MinPoly(" ++ show p ++ ")"

------------------------------------------------------------------------
-- Chebyshev polynomial (symbolic)
------------------------------------------------------------------------

||| Chebyshev polynomial T_k(x): T_0 = 1, T_1 = x,
||| T_{n+1} = 2x*T_n - T_{n-1}.
chebyshev : Int -> RadExpr Rational -> RadExpr Rational
chebyshev 0 _ = Lit Rational.one
chebyshev 1 x = x
chebyshev k x = assert_total $ go 2 (Lit Rational.one) x
  where
    go : Int -> RadExpr Rational -> RadExpr Rational -> RadExpr Rational
    go n t0 t1 =
      if n > k then t1
      else let t2 = Add (Mul (Mul (Lit (Rational.fromInteger 2)) x) t1) (Neg t0)
           in go (n + 1) t1 t2

------------------------------------------------------------------------
-- Core cos computation
------------------------------------------------------------------------

cosFirstQuadrant : Integer -> Integer -> TrigResult
cosFirstQuadrant p q =
  let g = gcdInteger p (2 * q)
      n = cast {to = Int} (div (2 * q) g)
      k = cast {to = Int} (div p g)
  in if k == 1 then
       case cosOfUnity n of
         Just e => Radical (normalize e)
         Nothing => MinPoly (cyclotomic n)
     else
       case cosOfUnity n of
         Just base =>
           let cheb = chebyshev k base
           in Radical (normalize cheb)
         Nothing => MinPoly (cyclotomic n)

||| cos(p*pi/q) where 0 <= p <= 2q.
cosInRange : Integer -> Integer -> TrigResult
cosInRange p q =
  if p == 0 then Radical (Lit Rational.one)
  else if 2 * p == q then Radical (Lit Rational.zero)
  else if p == q then Radical (Lit (negate Rational.one))
  else if 2 * p == 3 * q then Radical (Lit Rational.zero)
  else if 2 * p > q && p < q then
    case cosInRange (q - p) q of
      Radical e => Radical (Neg e)
      other => other
  else if p > q && 2 * p < 3 * q then
    case cosInRange (p - q) q of
      Radical e => Radical (Neg e)
      other => other
  else if 2 * p >= 3 * q then
    cosInRange (2 * q - p) q
  else cosFirstQuadrant p q

cosReduced : Integer -> Integer -> TrigResult
cosReduced p q =
  let p' = mod p (2 * q)
      p'' = if p' < 0 then p' + 2 * q else p'
  in cosInRange p'' q

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

||| Compute cos(p*pi/q) exactly as a radical expression.
|||
||| Reduces to cos(2*pi/n) via standard identities, then uses
||| the root-of-unity machinery.
export
cosExact : Integer -> Integer -> TrigResult
cosExact p q =
  if q <= 0 then Radical (Lit Rational.zero)
  else
    let g = gcdInteger (abs p) q
        p' = div p g
        q' = div q g
    in cosReduced p' q'

||| Compute sin(p*pi/q) exactly.
||| sin(p*pi/q) = +/- sqrt(1 - cos^2(p*pi/q)).
export
sinExact : Integer -> Integer -> TrigResult
sinExact p q =
  if q <= 0 then Radical (Lit Rational.zero)
  else
    let g = gcdInteger (abs p) q
        p' = div p g
        q' = div q g
        p'' = mod p' (2 * q')
        p''' = if p'' < 0 then p'' + 2 * q' else p''
        positive = p''' >= 0 && p''' <= q'
    in if p''' == 0 || p''' == q'
         then Radical (Lit Rational.zero)
         else case cosExact p q of
                Radical c =>
                  let sin2 = Add (Lit Rational.one) (Neg (Mul c c))
                      sinExpr = Root 2 (normalize sin2)
                      signed = if positive then sinExpr else Neg sinExpr
                  in Radical (normalize signed)
                other => other

||| Compute tan(p*pi/q) exactly as sin/cos.
export
tanExact : Integer -> Integer -> Maybe TrigResult
tanExact p q =
  case (sinExact p q, cosExact p q) of
    (Radical s, Radical c) => Just (Radical (Mul s (Inv c)))
    _ => Nothing

------------------------------------------------------------------------
-- Simplification
------------------------------------------------------------------------

||| Simplify a trig result for display.
export
simplifyTrigResult : TrigResult -> TrigResult
simplifyTrigResult (Radical e) = Radical (normalize e)
simplifyTrigResult other = other
