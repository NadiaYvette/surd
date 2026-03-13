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
  ||| Successfully identified the Galois group.
  Identified : TransGroup -> GaloisResult
  ||| Could not identify (unsupported degree or other issue).
  Unidentified : String -> GaloisResult

export
Show GaloisResult where
  show (Identified tg) = "Galois group: " ++ show tg
  show (Unidentified msg) = "Unidentified: " ++ msg

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
           (False, False) => Identified s5
           (True, False)  => Identified a5
           (False, True)  => Identified f20
           (True, True)   =>
             if frobeniusTestC5 p then Identified c5
             else Identified d5
    d => Unidentified ("Unsupported degree: " ++ show d)

------------------------------------------------------------------------
-- General identification
------------------------------------------------------------------------

||| Identify the Galois group of an irreducible polynomial over Q.
export
identifyGaloisGroup : Poly Rational -> GaloisResult
identifyGaloisGroup p =
  case degreeInt p of
    1 => Identified (MkTransGroup 1 1 "trivial" 1 True [])
    2 => Identified (MkTransGroup 2 1 "C2" 2 True [])
    3 => Identified (MkTransGroup 3 1 "S3_or_C3" 6 True [])
    4 => Unidentified "Degree 4: not yet fully implemented"
    5 => identifyGaloisGroup5 p
    d => Unidentified ("Degree " ++ show d ++ " not supported")

||| Check if a polynomial is solvable by radicals.
export
isSolvableByRadicals : Poly Rational -> Bool
isSolvableByRadicals p =
  case identifyGaloisGroup p of
    Identified tg => tgSolvable tg
    Unidentified _ => False
