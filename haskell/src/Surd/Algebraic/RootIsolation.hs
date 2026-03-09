-- | Real root isolation for polynomials with rational coefficients.
--
-- Uses Sturm's theorem with bisection to separate and refine roots
-- of square-free polynomials.
module Surd.Algebraic.RootIsolation
  ( IsolatingInterval(..)
  , isolateRealRoots
  , refineRoot
  , rootInInterval
  , sturmCount
  ) where

import Surd.Polynomial.Univariate
import Surd.Internal.Interval (Interval(..))

-- | An isolating interval for a real root of a polynomial.
-- The polynomial has exactly one root in the open interval (lo, hi),
-- unless lo == hi in which case it's an exact rational root.
data IsolatingInterval = IsolatingInterval
  { iiPoly     :: !(Poly Rational)  -- ^ The (square-free) polynomial
  , iiInterval :: !Interval         -- ^ Isolating interval
  } deriving (Show)

-- | Isolate all real roots of a polynomial.
-- Returns isolating intervals ordered by increasing root value.
-- The polynomial is made square-free internally.
isolateRealRoots :: Poly Rational -> [IsolatingInterval]
isolateRealRoots p
  | degree p <= 0 = []
  | degree p == 1 =
      case unPoly p of
        [a, b] ->
          let r = -a / b
          in [IsolatingInterval (monicPoly p) (Interval r r)]
        _ -> []
  | otherwise =
      let p' = monicPoly p
          bound = rootBound p'
          iv = Interval (-bound) bound
      in map (IsolatingInterval p') $ isolateIn p' iv

-- | Refine an isolating interval until its width is less than epsilon.
refineRoot :: Rational -> IsolatingInterval -> IsolatingInterval
refineRoot eps (IsolatingInterval p iv) =
  IsolatingInterval p (refineInterval p eps iv)

-- | Check if a specific rational is a root within the interval.
rootInInterval :: IsolatingInterval -> Maybe Rational
rootInInterval (IsolatingInterval p (Interval l h))
  | l == h && evalPoly p l == 0 = Just l
  | otherwise = Nothing

-- Internal

-- | Cauchy bound: all roots satisfy |r| ≤ bound.
rootBound :: Poly Rational -> Rational
rootBound (Poly []) = 0
rootBound (Poly cs) =
  let lc = last cs
      ratios = map (\c -> abs (c / lc)) (init cs)
  in 1 + maximum (0 : ratios)

-- | Isolate roots within a given interval using bisection.
-- Precondition: p is square-free.
isolateIn :: Poly Rational -> Interval -> [Interval]
isolateIn p iv@(Interval l h)
  | sc == 0 = []
  | sc == 1 = [iv]
  | otherwise =
      let m = (l + h) / 2
      in if evalPoly p m == 0
         then Interval m m : isolateIn p (Interval l m) ++ isolateIn p (Interval m h)
         else isolateIn p (Interval l m) ++ isolateIn p (Interval m h)
  where
    sc = sturmCount p l h

-- | Count real roots in (a, b] using Sturm's theorem.
sturmCount :: Poly Rational -> Rational -> Rational -> Int
sturmCount p a b =
  let chain = sturmChain p
      va = signChangesAt chain a
      vb = signChangesAt chain b
  in va - vb

-- | Build the Sturm chain: p₀ = p, p₁ = p', pₖ₊₁ = -rem(pₖ₋₁, pₖ).
sturmChain :: Poly Rational -> [Poly Rational]
sturmChain p = p : p' : go p p'
  where
    p' = diffPoly p
    go _ b | degree b < 0 = []
    go a b =
      let (_, r) = divModPoly a b
          nr = scalePoly (-1) r
      in if degree nr < 0 then [] else nr : go b nr

-- | Count sign changes in the Sturm chain evaluated at x.
signChangesAt :: [Poly Rational] -> Rational -> Int
signChangesAt chain x =
  let vals = filter (/= 0) $ map (`evalPoly` x) chain
      pairs = zip vals (drop 1 vals)
  in length $ filter (\(a, b) -> signum a /= signum b) pairs

-- | Refine an interval by bisection until width < eps.
refineInterval :: Poly Rational -> Rational -> Interval -> Interval
refineInterval _ _ iv@(Interval l h) | l == h = iv
refineInterval p eps iv@(Interval l h)
  | h - l < eps = iv
  | otherwise =
      let m = (l + h) / 2
          fm = evalPoly p m
      in if fm == 0
         then Interval m m
         else let fl = evalPoly p l
              in if signum fl /= signum fm
                 then refineInterval p eps (Interval l m)
                 else refineInterval p eps (Interval m h)
