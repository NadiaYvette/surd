--- Arithmetic in simple algebraic extension fields K(alpha),
--- where alpha is a root of an irreducible polynomial over K.
---
--- Elements of K(alpha) are represented as polynomials in alpha
--- of degree less than the degree of the minimal polynomial.
module Extension
  ( ExtField(..)
  , ExtElem(..)
  , mkExtField
  , embed
  , generator
  , extAdd
  , extSub
  , extNeg
  , extMul
  , extInv
  , extDiv
  , extPow
  , extEq
  , showExtElem
  ) where

import Rational
import Poly

--- A simple algebraic extension K(alpha)/K defined by the minimal
--- polynomial of alpha over K.
data ExtField = ExtField Poly Int String

--- An element of an extension field, represented as a polynomial
--- in the generator of degree < extDegree.
data ExtElem = ExtElem Poly ExtField

--- Extract the minimal polynomial from an ExtField.
fieldMinPoly :: ExtField -> Poly
fieldMinPoly (ExtField p _ _) = p

--- Extract the degree from an ExtField.
fieldDegree :: ExtField -> Int
fieldDegree (ExtField _ d _) = d

--- Extract the name from an ExtField.
fieldName :: ExtField -> String
fieldName (ExtField _ _ n) = n

--- Construct an extension field from an irreducible polynomial.
mkExtField :: Poly -> String -> ExtField
mkExtField p name =
  let mp = monicPoly p
  in ExtField mp (degree mp) name

--- Embed a base field element (rational) into the extension.
embed :: ExtField -> Rational -> ExtElem
embed field c = ExtElem (constPoly c) field

--- The generator alpha of the extension.
generator :: ExtField -> ExtElem
generator field = ExtElem monoX field

--- Reduce a polynomial modulo the minimal polynomial.
reduce :: ExtField -> Poly -> Poly
reduce field p
  | fieldDegree field <= 0 = p
  | otherwise = snd (divModPoly p (fieldMinPoly field))

--- Addition.
extAdd :: ExtElem -> ExtElem -> ExtElem
extAdd (ExtElem a f) (ExtElem b _) = ExtElem (reduce f (addPoly a b)) f

--- Subtraction.
extSub :: ExtElem -> ExtElem -> ExtElem
extSub (ExtElem a f) (ExtElem b _) = ExtElem (reduce f (subPoly a b)) f

--- Negation.
extNeg :: ExtElem -> ExtElem
extNeg (ExtElem a f) =
  ExtElem (scalePoly (Rational.fromInt (negate 1)) a) f

--- Multiplication.
extMul :: ExtElem -> ExtElem -> ExtElem
extMul (ExtElem a f) (ExtElem b _) = ExtElem (reduce f (mulPoly a b)) f

--- Inverse via extended GCD.
---
--- To invert a(alpha), find u, v such that u*a + v*minpoly = gcd.
--- Then u*a = gcd (mod minpoly), and u/gcd is the inverse.
extInv :: ExtElem -> ExtElem
extInv (ExtElem a f) =
  let mp = fieldMinPoly f
      (g, u, _) = extGcd a mp
  in case leadCoeff g of
       Just lc ->
         let scale = ratDiv (Rational.fromInt 1) lc
         in ExtElem (reduce f (scalePoly scale u)) f
       Nothing -> error "extInv: zero element"

--- Division: a / b = a * (1/b).
extDiv :: ExtElem -> ExtElem -> ExtElem
extDiv a b = extMul a (extInv b)

--- Integer power.
extPow :: ExtElem -> Int -> ExtElem
extPow x n
  | n == 0    = embed (elemField x) (Rational.fromInt 1)
  | n < 0     = extPow (extInv x) (negate n)
  | n == 1    = x
  | even n    = let half = extPow x (n `div` 2)
                in extMul half half
  | otherwise = extMul x (extPow x (n - 1))

--- Extract the field from an element.
elemField :: ExtElem -> ExtField
elemField (ExtElem _ f) = f

--- Equality.
extEq :: ExtElem -> ExtElem -> Bool
extEq (ExtElem a _) (ExtElem b _) = a == b

--- Extended GCD for polynomials: returns (gcd, u, v) such that
--- u*a + v*b = gcd.
extGcd :: Poly -> Poly -> (Poly, Poly, Poly)
extGcd a b
  | degree b < 0 = (a, constPoly (Rational.fromInt 1), zeroPoly)
  | otherwise =
      let (q, r) = divModPoly a b
          (g, u', v') = extGcd b r
          -- u' * b + v' * r = g
          -- r = a - q*b
          -- u' * b + v' * (a - q*b) = g
          -- v' * a + (u' - v'*q) * b = g
          u = v'
          v = subPoly u' (mulPoly v' q)
      in (g, u, v)

--- Show an extension field element.
showExtElem :: ExtElem -> String
showExtElem (ExtElem p f) =
  "ExtElem (" ++ showPoly p ++ ") in " ++ fieldName f

instance Eq ExtField where
  (ExtField p1 d1 _) == (ExtField p2 d2 _) = p1 == p2 && d1 == d2

instance Eq ExtElem where
  (==) = extEq

instance Show ExtField where
  show (ExtField p d n) = "ExtField " ++ show p ++ " " ++ show d
                          ++ " " ++ show n

instance Show ExtElem where
  show = showExtElem
