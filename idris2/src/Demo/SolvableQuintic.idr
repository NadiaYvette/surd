module Demo.SolvableQuintic

import Surd.Rational
import Surd.Poly
import Surd.Cyclotomic  -- Ring/Field instances
import Surd.Types
import Surd.Pretty
import Surd.Eval
import Surd.Identify
import Surd.GaloisSolve
import Surd.RadicalTower
import Surd.Resolvent
import Surd.TransitiveGroup

import Data.List

%default covering

------------------------------------------------------------------------
-- Demo: solving solvable polynomials of prime degree
------------------------------------------------------------------------

||| Example 1: x^5 - 1 (cyclotomic, Galois group C4)
||| Roots are the 5th roots of unity.
example1 : (String, Poly Rational)
example1 =
  ( "x^5 - 1"
  , mkPoly [negate Rational.one, Rational.zero, Rational.zero, Rational.zero, Rational.zero, Rational.one]
  )

||| Example 2: x^5 - 5x + 12
||| A quintic that may or may not be solvable.
example2 : (String, Poly Rational)
example2 =
  ( "x^5 - 5x + 12"
  , mkPoly [Rational.fromInteger 12, negate (Rational.fromInteger 5), Rational.zero, Rational.zero, Rational.zero, Rational.one]
  )

||| Example 3: x^5 - 5x^3 + 4x (factors as x(x^2-1)(x^2-4))
||| Reducible: roots are 0, +/-1, +/-2.
example3 : (String, Poly Rational)
example3 =
  ( "x^5 - 5x^3 + 4x"
  , mkPoly [Rational.zero, Rational.fromInteger 4, Rational.zero, negate (Rational.fromInteger 5), Rational.zero, Rational.one]
  )

||| Example 4: x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1 = 0
||| This is the minimal polynomial of 2*cos(2*pi/11), solvable with C5 group.
example4 : (String, Poly Rational)
example4 =
  ( "x^5 + x^4 - 4x^3 - 3x^2 + 3x + 1  (minpoly of 2cos(2pi/11))"
  , mkPoly [Rational.one, Rational.fromInteger 3, negate (Rational.fromInteger 3),
            negate (Rational.fromInteger 4), Rational.one, Rational.one]
  )

||| Example 5: x^7 - 1 (7th cyclotomic, degree 6 is not prime but
||| the minimal polynomial Phi_7(x) = x^6+x^5+...+1 has degree 6 which
||| is composite). Instead use x^7 - 7x + 3 as a degree-7 example.
example5 : (String, Poly Rational)
example5 =
  ( "x^7 - 7x + 3  (degree 7)"
  , mkPoly [Rational.fromInteger 3, negate (Rational.fromInteger 7),
            Rational.zero, Rational.zero, Rational.zero, Rational.zero,
            Rational.zero, Rational.one]
  )

||| Example 6: x^3 - 2 (degree 3, Galois group S3)
example6 : (String, Poly Rational)
example6 =
  ( "x^3 - 2  (degree 3)"
  , mkPoly [negate (Rational.fromInteger 2), Rational.zero, Rational.zero, Rational.one]
  )

joinLines : List String -> String
joinLines [] = ""
joinLines (x :: xs) = x ++ "\n" ++ joinLines xs

showResult : SolveResult -> String
showResult (Solved roots) =
  let rootStrs = map (\r => "    " ++ pretty r ++ "  (~" ++ show (eval r) ++ ")") roots
  in "  Solved! " ++ show (length roots) ++ " root(s):\n" ++ joinLines rootStrs
showResult NotSolvable = "  Not solvable by radicals (Galois group is non-solvable)"
showResult (Unsupported msg) = "  " ++ msg

export
main : IO ()
main = do
  putStrLn "=== Solvable Polynomial Demo (all prime degrees) ==="
  putStrLn ""

  -- Show transitive groups for degrees 3, 5, 7
  putStrLn "--- Transitive groups ---"
  for_ [3, 5, 7] $ \deg => do
    let groups = transGroupsOfDegreeRT deg
    putStrLn ("  Degree " ++ show deg ++ ": " ++ show (length groups) ++ " groups")
    traverse_ (\g => putStrLn ("    " ++ show g)) groups
  putStrLn ""

  -- Solve examples
  let examples = [example1, example2, example3, example4, example5, example6]
  traverse_ (\ex => do
    let (desc, p) = ex
    putStrLn ("Polynomial: " ++ desc)
    putStrLn ("  Degree: " ++ show (degreeInt p))

    -- Identify Galois group
    let galois = identifyGaloisGroup p
    putStrLn ("  Galois group: " ++ show galois)

    -- Try to solve
    let result = solveByRadicals p
    putStrLn (showResult result)

    -- Show numerical roots
    let numRoots = findRoots p
    putStrLn ("  Numerical roots: " ++
      show (map (\r => "(" ++ show (Builtin.fst r) ++ ", " ++ show (Builtin.snd r) ++ ")") numRoots))

    putStrLn ""
    ) examples
