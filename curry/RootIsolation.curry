--- Real root isolation for polynomials with rational coefficients.
---
--- Uses Sturm's theorem with bisection to separate and refine roots
--- of square-free polynomials.
---
--- Note on Curry nondeterminism: ideally, root finding could be expressed
--- as a constraint narrowing problem:
---
---   findRootIn :: Poly -> Rational -> Rational -> Rational
---   findRootIn p lo hi = x
---     where x free
---           ratLe lo x =:= True
---           ratLe x hi =:= True
---           evalPoly p x =:= rZero
---
--- However, PAKCS does not support constraint solving over rationals
--- (free variables must be bound before evaluation, and the search space
--- is infinite). The bisection approach below is therefore necessary.
--- Sturm counting remains the right tool for isolating roots in intervals.
module RootIsolation
  ( IsolatingInterval(..)
  , isolateRealRoots
  , refineRoot
  , rootInInterval
  , sturmCount
  ) where

import Rational
import Poly
import Interval (Interval(..))
import RootBound (rootBound)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

rTwo :: Rational
rTwo = Rational.fromInt 2

--- An isolating interval for a real root of a polynomial.
--- The polynomial has exactly one root in the open interval (lo, hi),
--- unless lo == hi in which case it's an exact rational root.
data IsolatingInterval = IsolatingInterval Poly Interval

--- Isolate all real roots of a polynomial.
--- Returns isolating intervals ordered by increasing root value.
--- The polynomial is made square-free internally.
isolateRealRoots :: Poly -> [IsolatingInterval]
isolateRealRoots p
  | degree p <= 0 = []
  | degree p == 1 =
      case polyCoeffs p of
        [a, b] ->
          let r = ratNeg (ratDiv a b)
          in [IsolatingInterval (monicPoly p) (IV r r)]
        _ -> []
  | otherwise =
      let p' = monicPoly p
          bound = rootBound p'
          iv = IV (ratNeg bound) bound
      in map (IsolatingInterval p') (isolateIn p' iv)

--- Refine an isolating interval until its width is less than epsilon.
refineRoot :: Rational -> IsolatingInterval -> IsolatingInterval
refineRoot eps (IsolatingInterval p iv) =
  IsolatingInterval p (refineInterval p eps iv)

--- Check if a specific rational is a root within the interval.
rootInInterval :: IsolatingInterval -> Maybe Rational
rootInInterval (IsolatingInterval p (IV l h)) =
  if l == h && evalPoly p l == rZero then Just l else Nothing

--- Count real roots in (a, b] using Sturm's theorem.
sturmCount :: Poly -> Rational -> Rational -> Int
sturmCount p a b =
  let chain = sturmChain p
      va = signChangesAt chain a
      vb = signChangesAt chain b
  in va - vb

--- Build the Sturm chain: p0 = p, p1 = p', pk+1 = -rem(pk-1, pk).
sturmChain :: Poly -> [Poly]
sturmChain p = p : p' : go p p'
  where
    p' = diffPoly p
    go a b
      | degree b < 0 = []
      | otherwise =
      let (_, r) = divModPoly a b
          nr = scalePoly (Rational.fromInt (negate 1)) r
      in if degree nr < 0 then [] else nr : go b nr

--- Count sign changes in the Sturm chain evaluated at x.
signChangesAt :: [Poly] -> Rational -> Int
signChangesAt chain x =
  let vals = filter (\v -> v /= rZero) (map (\p -> evalPoly p x) chain)
      pairs = zip vals (drop 1 vals)
  in length (filter (\(a, b) -> ratSignum a /= ratSignum b) pairs)

--- Isolate roots within a given interval using bisection.
--- Precondition: p is square-free.
isolateIn :: Poly -> Interval -> [Interval]
isolateIn p iv@(IV l h) =
  let sc = sturmCount p l h
  in if sc == 0 then []
     else if sc == 1 then [iv]
     else let m = ratDiv (ratAdd l h) rTwo
          in if evalPoly p m == rZero
             then IV m m : isolateIn p (IV l m) ++ isolateIn p (IV m h)
             else isolateIn p (IV l m) ++ isolateIn p (IV m h)

--- Refine an interval by bisection until width < eps.
refineInterval :: Poly -> Rational -> Interval -> Interval
refineInterval p eps iv@(IV l h)
  | l == h = iv
  | ratLt (ratSub h l) eps = iv
  | otherwise =
      let m = ratDiv (ratAdd l h) rTwo
          fm = evalPoly p m
      in if fm == rZero
         then IV m m
         else let fl = evalPoly p l
              in if ratSignum fl /= ratSignum fm
                 then refineInterval p eps (IV l m)
                 else refineInterval p eps (IV m h)

--- Extract coefficient list from Poly.
polyCoeffs :: Poly -> [Rational]
polyCoeffs (Poly cs) = cs

instance Eq IsolatingInterval where
  (IsolatingInterval p1 iv1) == (IsolatingInterval p2 iv2) =
    p1 == p2 && iv1 == iv2

instance Show IsolatingInterval where
  show (IsolatingInterval p iv) =
    "IsolatingInterval (" ++ show p ++ ") " ++ show iv
