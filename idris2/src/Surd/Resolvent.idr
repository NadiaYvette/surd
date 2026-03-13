module Surd.Resolvent

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Permutation
import Surd.Eval

import Data.List

%default covering

------------------------------------------------------------------------
-- Numerical root finding (Durand-Kerner)
------------------------------------------------------------------------

||| Complex number (pair of Doubles).
public export
CNum : Type
CNum = (Double, Double)

cAdd : CNum -> CNum -> CNum
cAdd (a, b) (c, d) = (a + c, b + d)

cSub : CNum -> CNum -> CNum
cSub (a, b) (c, d) = (a - c, b - d)

cMul : CNum -> CNum -> CNum
cMul (a, b) (c, d) = (a * c - b * d, a * d + b * c)

cDiv : CNum -> CNum -> CNum
cDiv (a, b) (c, d) =
  let den = c * c + d * d
  in ((a * c + b * d) / den, (b * c - a * d) / den)

cMag : CNum -> Double
cMag (a, b) = sqrt (a * a + b * b)

ratD : Rational -> Double
ratD r = cast (numer r) / cast (denom r)

||| Evaluate a rational polynomial at a complex point.
evalPolyC : Poly Rational -> CNum -> CNum
evalPolyC (MkPoly []) _ = (0.0, 0.0)
evalPolyC (MkPoly cs) z =
  let rcs = map (\c => (ratD c, 0.0)) cs
  in foldr (\c, acc => cAdd c (cMul z acc)) (0.0, 0.0) rcs

zipIdx : List a -> List (Nat, a)
zipIdx xs = go 0 xs
  where
    go : Nat -> List a -> List (Nat, a)
    go _ [] = []
    go n (x :: rest) = (n, x) :: go (S n) rest

updateRoot : Poly Rational -> List CNum -> (Nat, CNum) -> CNum
updateRoot p roots (idx, z) =
  let fz = evalPolyC p z
      initDen : CNum
      initDen = (1.0, 0.0)
      den = foldl (\acc, rp =>
        let i = fst rp
            r = snd rp
        in if i == idx then acc
           else cMul acc (cSub z r))
        initDen
        (zipIdx roots)
  in if cMag den < 1.0e-300 then z
     else cSub z (cDiv fz den)

iterateRoots : Nat -> Poly Rational -> List CNum -> List CNum
iterateRoots Z _ roots = roots
iterateRoots (S k) p roots =
  let newRoots = map (updateRoot p roots) (zipIdx roots)
  in iterateRoots k p newRoots

||| Find all complex roots of a polynomial numerically.
||| Uses Durand-Kerner iteration.
export
findRoots : Poly Rational -> List CNum
findRoots (MkPoly []) = []
findRoots p =
  case degreeInt p of
    d =>
      if d <= 0 then []
      else
        let n = cast {to = Nat} d
            indices : List Nat
            indices = iterateN n (+ 1) 0
            initRoots : List CNum
            initRoots = map (\i =>
              ( 0.4 * cos (2.0 * pi * cast i / cast n + 0.1)
              , 0.4 * sin (2.0 * pi * cast i / cast n + 0.1))) indices
        in iterateRoots 50 p initRoots

------------------------------------------------------------------------
-- Discriminant
------------------------------------------------------------------------

||| Compute the discriminant of a polynomial numerically.
export
discriminant : Poly Rational -> Double
discriminant p =
  let roots = findRoots p
      ixRoots = zipIdx roots
      pairs = [(snd a, snd b) | a <- ixRoots, b <- ixRoots, fst a < fst b]
  in foldl (\acc, pair =>
       let diff = cSub (fst pair) (snd pair)
       in acc * (fst diff * fst diff + snd diff * snd diff))
     1.0
     pairs

||| Check if the discriminant is a perfect square in Q.
export
isDiscriminantSquare : Poly Rational -> Bool
isDiscriminantSquare p =
  let d = discriminant p
  in d >= 0.0 && abs (sqrt d - cast (the Int (cast (sqrt d)))) < 1.0e-6

------------------------------------------------------------------------
-- Sextic resolvent for degree 5
------------------------------------------------------------------------

||| Compute the sextic resolvent for the F20 test.
export
sexticResolvent : Poly Rational -> Poly Rational
sexticResolvent p = mkPoly [Rational.one]  -- stub

||| Check if the sextic resolvent has a rational root.
export
hasSexticRationalRoot : Poly Rational -> Bool
hasSexticRationalRoot p = False  -- stub
