module Surd.RootIsolation

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- for Ring/Field instances
import Surd.Interval
import Surd.RootBound

import Data.List

%default covering

------------------------------------------------------------------------
-- Sturm sequences
------------------------------------------------------------------------

||| Compute the Sturm sequence of a polynomial.
||| Sturm(f) = [f, f', -rem(f, f'), -rem(f', prev), ...]
export
sturmSequence : Poly Rational -> List (Poly Rational)
sturmSequence (MkPoly []) = []
sturmSequence f =
  let f' = diffPoly f
  in case degreeInt f' of
       -1 => [f]
       _  => go [f', f]
  where
    go : List (Poly Rational) -> List (Poly Rational)
    go [] = []
    go [p] = [p]
    go (p :: q :: rest) =
      let r = negPoly (snd (divModPoly q p))
      in case degreeInt r of
           -1 => reverse (p :: q :: rest)
           _  => go (r :: p :: q :: rest)

||| Count sign changes in a list of rationals (ignoring zeros).
countSignChanges : List Int -> Nat
countSignChanges [] = 0
countSignChanges [_] = 0
countSignChanges (a :: b :: rest) =
  (if a /= b then 1 else 0) + countSignChanges (b :: rest)

signChanges : List Rational -> Nat
signChanges xs =
  let nonzero = filter (\r => not (Surd.Rational.isZero r)) xs
      signs : List Int
      signs = map (\r => if r > Rational.zero then 1 else -1) nonzero
  in countSignChanges signs

||| Evaluate a Sturm sequence at a point, returning the number of sign changes.
sturmSignChangesAt : List (Poly Rational) -> Rational -> Nat
sturmSignChangesAt seq x = signChanges (map (\p => evalPoly p x) seq)

||| Count the number of real roots of f in the interval (a, b]
||| using Sturm's theorem.
export
sturmCount : Poly Rational -> Rational -> Rational -> Nat
sturmCount f a b =
  let seq = sturmSequence f
  in minus (sturmSignChangesAt seq a) (sturmSignChangesAt seq b)

------------------------------------------------------------------------
-- Isolating intervals
------------------------------------------------------------------------

||| An isolating interval for a real root of a polynomial.
public export
record IsolatingInterval where
  constructor MkIsolatingInterval
  iiPoly : Poly Rational
  iiInterval : Interval

export
Show IsolatingInterval where
  show ii = "IsolatingInterval(" ++ show (iiPoly ii) ++ ", " ++ show (iiInterval ii) ++ ")"

------------------------------------------------------------------------
-- Root isolation via Sturm + bisection
------------------------------------------------------------------------

||| Isolate roots within a given interval using bisection.
isolateIn : Poly Rational -> Interval -> List Interval
isolateIn p iv =
  let l = lo iv
      h = hi iv
      sc = sturmCount p l h
  in if sc == 0 then []
     else if sc == 1 then [iv]
     else
       let m = midpoint iv
       in if Surd.Rational.isZero (evalPoly p m)
            then MkInterval m m :: isolateIn p (MkInterval l m) ++ isolateIn p (MkInterval m h)
            else isolateIn p (MkInterval l m) ++ isolateIn p (MkInterval m h)

||| Isolate all real roots of a polynomial.
||| Returns isolating intervals ordered by increasing root value.
export
isolateRealRoots : Poly Rational -> List IsolatingInterval
isolateRealRoots p =
  case degreeInt p of
    d =>
      if d <= 0 then []
      else if d == 1 then
        case coeffs p of
          [a, b] =>
            let r = negate a / b
            in [MkIsolatingInterval (monicPoly p) (MkInterval r r)]
          _ => []
      else
        let p' = monicPoly p
            bound = cauchyBound p'
            iv = MkInterval (negate bound) bound
            intervals = isolateIn p' iv
        in map (MkIsolatingInterval p') (sortBy (\a, b => compare (lo a) (lo b)) intervals)

||| Refine an isolating interval until width < epsilon.
export
refineRoot : Rational -> IsolatingInterval -> IsolatingInterval
refineRoot eps ii =
  let iv = iiInterval ii
      p = iiPoly ii
  in MkIsolatingInterval p (refineIv p eps iv 100)
  where
    refineIv : Poly Rational -> Rational -> Interval -> Nat -> Interval
    refineIv _ _ iv Z = iv
    refineIv p eps iv (S k) =
      if width iv < eps then iv
      else
        let m = midpoint iv
            l = lo iv
            h = hi iv
        in if Surd.Rational.isZero (evalPoly p m) then MkInterval m m
           else
             let scLeft = sturmCount p l m
             in if scLeft > 0 then refineIv p eps (MkInterval l m) k
                else refineIv p eps (MkInterval m h) k

||| Check if an exact rational root exists in the interval.
export
rootInInterval : IsolatingInterval -> Maybe Rational
rootInInterval ii =
  let iv = iiInterval ii
  in if lo iv == hi iv && Surd.Rational.isZero (evalPoly (iiPoly ii) (lo iv))
     then Just (lo iv)
     else Nothing
