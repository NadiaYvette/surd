module Main where

import Test.Tasty
import qualified Test.Polynomial
import qualified Test.Denest
import qualified Test.NormalForm
import qualified Test.Trig

main :: IO ()
main = defaultMain $ testGroup "surd"
  [ Test.Polynomial.tests
  , Test.Denest.tests
  , Test.NormalForm.tests
  , Test.Trig.tests
  ]
