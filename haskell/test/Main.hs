module Main where

import Test.Tasty
import qualified Test.Polynomial
import qualified Test.Denest
import qualified Test.NormalForm
import qualified Test.Trig
import qualified Test.Algebraic

main :: IO ()
main = defaultMain $ testGroup "surd"
  [ Test.Polynomial.tests
  , Test.Denest.tests
  , Test.NormalForm.tests
  , Test.Trig.tests
  , Test.Algebraic.tests
  ]
