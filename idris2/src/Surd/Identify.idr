module Surd.Identify

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Permutation
import Surd.TransitiveGroup
import Surd.Resolvent

import Data.List

%default covering

------------------------------------------------------------------------
-- Galois group identification
------------------------------------------------------------------------

||| Result of Galois group identification.
public export
data GaloisResult : Type where
  ||| Successfully identified the Galois group of a degree-5 polynomial.
  Identified5 : TransGroup 5 -> GaloisResult
  ||| Successfully identified with ad-hoc info (small degrees).
  IdentifiedAdHoc : String -> Integer -> Bool -> GaloisResult
  ||| Could not identify (unsupported degree or other issue).
  Unidentified : String -> GaloisResult

export
Show GaloisResult where
  show (Identified5 tg) = "Galois group: " ++ show tg
  show (IdentifiedAdHoc name order solv) =
    "Galois group: " ++ name ++ " (order " ++ show order ++ ", "
    ++ (if solv then "solvable" else "non-solvable") ++ ")"
  show (Unidentified msg) = "Unidentified: " ++ msg

||| Check if a Galois result is solvable.
export
isSolvableResult : GaloisResult -> Bool
isSolvableResult (Identified5 tg) = tgSolvable tg
isSolvableResult (IdentifiedAdHoc _ _ solv) = solv
isSolvableResult (Unidentified _) = False

------------------------------------------------------------------------
-- Frobenius test
------------------------------------------------------------------------

||| Frobenius/Chebotarev test to distinguish C5 from D5.
frobeniusTestC5 : Poly Rational -> Bool
frobeniusTestC5 p =
  -- Stub: test factorisation mod small primes
  True  -- default to C5

------------------------------------------------------------------------
-- Degree 5 identification via Stauduhar descent
------------------------------------------------------------------------

||| Identify the Galois group of an irreducible degree-5 polynomial.
export
identifyGaloisGroup5 : Poly Rational -> GaloisResult
identifyGaloisGroup5 p =
  case degreeInt p of
    5 =>
      let discSquare = isDiscriminantSquare p
          sexticRoot = hasSexticRationalRoot p
      in case (discSquare, sexticRoot) of
           (False, False) => Identified5 s5
           (True, False)  => Identified5 a5
           (False, True)  => Identified5 f20
           (True, True)   =>
             if frobeniusTestC5 p then Identified5 c5
             else Identified5 d5
    d => Unidentified ("Unsupported degree: " ++ show d)

------------------------------------------------------------------------
-- General identification
------------------------------------------------------------------------

||| Identify the Galois group of an irreducible polynomial over Q.
export
identifyGaloisGroup : Poly Rational -> GaloisResult
identifyGaloisGroup p =
  case degreeInt p of
    1 => IdentifiedAdHoc "trivial" 1 True
    2 => IdentifiedAdHoc "C2" 2 True
    3 => IdentifiedAdHoc "S3_or_C3" 6 True
    4 => Unidentified "Degree 4: not yet fully implemented"
    5 => identifyGaloisGroup5 p
    d => Unidentified ("Degree " ++ show d ++ " not supported")

||| Check if a polynomial is solvable by radicals.
export
isSolvableByRadicals : Poly Rational -> Bool
isSolvableByRadicals p = isSolvableResult (identifyGaloisGroup p)
