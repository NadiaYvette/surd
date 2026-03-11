module Main where

import Test.Algebraic qualified
import Test.Denest qualified
import Test.DynTower qualified
import Test.Groebner qualified
import Test.Interval qualified
import Test.Multivariate qualified
import Test.NormalForm qualified
import Test.Polynomial qualified
import Test.Tasty
import Test.TragerFactoring qualified
import Test.Trig qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "surd"
      [ Test.Polynomial.tests,
        Test.Denest.tests,
        Test.NormalForm.tests,
        Test.Trig.tests,
        Test.Algebraic.tests,
        Test.Interval.tests,
        Test.Multivariate.tests,
        Test.TragerFactoring.tests,
        Test.Groebner.tests,
        Test.DynTower.tests
      ]
