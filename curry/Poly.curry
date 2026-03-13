--- Dense univariate polynomials over an arbitrary coefficient type.
---
--- Polynomials are represented as coefficient lists, low-degree first:
--- [a0, a1, ..., an] means a0 + a1*x + ... + an*x^n.
--- Invariant: trailing zeros are stripped; the zero polynomial is [].
---
--- Since Curry (PAKCS) typeclasses don't support Haskell-style Num
--- polymorphism easily, we parameterize operations by explicit
--- zero/add/mul/neg/inv functions where needed, or work concretely
--- with Rational coefficients where appropriate.
module Poly
  ( Poly(..)
  , mkPoly
  , zeroPoly
  , constPoly
  , monoX
  , degree
  , leadCoeff
  , evalPoly
  , scalePoly
  , addPoly
  , subPoly
  , mulPoly
  , divModPoly
  , gcdPoly
  , monicPoly
  , diffPoly
  , composePoly
  , squareFree
  , showPoly
  ) where

import Rational

--- Convert Int to Rational (local alias to avoid ambiguity with Prelude.fromInt).
ratFromInt :: Int -> Rational
ratFromInt = Rational.fromInt

--- Last element of a non-empty list.
lastElem :: [a] -> a
lastElem xs = case xs of
  [x]    -> x
  (_:ys) -> lastElem ys
  []     -> error "lastElem: empty list"

--- A univariate polynomial with rational coefficients.
--- Stored as a list of coefficients, low-degree first.
data Poly = Poly [Rational]

--- Smart constructor: strip trailing zeros.
mkPoly :: [Rational] -> Poly
mkPoly = Poly . stripZeros

stripZeros :: [Rational] -> [Rational]
stripZeros = reverseList . dropWhile (\x -> x == ratFromInt 0) . reverseList

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

--- The zero polynomial.
zeroPoly :: Poly
zeroPoly = Poly []

--- A constant polynomial.
constPoly :: Rational -> Poly
constPoly c
  | c == ratFromInt 0 = zeroPoly
  | otherwise         = Poly [c]

--- The polynomial x.
monoX :: Poly
monoX = Poly [ratFromInt 0, ratFromInt 1]

--- Degree of the polynomial. Returns -1 for the zero polynomial.
degree :: Poly -> Int
degree (Poly cs) = case cs of
  [] -> negate 1
  _  -> length cs - 1

--- Leading coefficient. Returns Nothing for the zero polynomial.
leadCoeff :: Poly -> Maybe Rational
leadCoeff (Poly cs) = case cs of
  [] -> Nothing
  _  -> Just (lastElem cs)

--- Evaluate a polynomial at a point using Horner's method.
evalPoly :: Poly -> Rational -> Rational
evalPoly (Poly cs) x = case cs of
  [] -> ratFromInt 0
  _  -> foldr (\c acc -> ratAdd c (ratMul x acc)) (ratFromInt 0) cs

--- Multiply by a scalar.
scalePoly :: Rational -> Poly -> Poly
scalePoly s (Poly cs)
  | s == ratFromInt 0 = zeroPoly
  | otherwise         = mkPoly (map (ratMul s) cs)

--- Addition.
addPoly :: Poly -> Poly -> Poly
addPoly (Poly as) (Poly bs) = mkPoly (zipWithDefault (ratFromInt 0) ratAdd as bs)

--- Subtraction.
subPoly :: Poly -> Poly -> Poly
subPoly (Poly as) (Poly bs) = mkPoly (zipWithDefault (ratFromInt 0) ratSub as bs)

zipWithDefault :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
zipWithDefault def f xs ys = case (xs, ys) of
  ([], [])         -> []
  (a:as, [])       -> f a def : zipWithDefault def f as []
  ([], b:bs)       -> f def b : zipWithDefault def f [] bs
  (a:as, b:bs)     -> f a b   : zipWithDefault def f as bs

--- Polynomial multiplication via schoolbook algorithm.
mulPoly :: Poly -> Poly -> Poly
mulPoly (Poly as) (Poly bs)
  | null as   = zeroPoly
  | null bs   = zeroPoly
  | otherwise =
      mkPoly (foldl addCoeffs (replicate rlen (ratFromInt 0)) terms)
  where
    rlen = length as + length bs - 1
    terms = [ (i + j, ratMul a b)
            | (i, a) <- zip [0..] as
            , (j, b) <- zip [0..] bs
            ]
    addCoeffs cs (idx, c) =
      let (pre, rest) = splitAt idx cs
      in case rest of
           (v:post) -> pre ++ (ratAdd v c : post)
           _        -> cs

--- Polynomial division with remainder.
--- divModPoly f g = (q, r) such that f = g*q + r and degree r < degree g.
divModPoly :: Poly -> Poly -> (Poly, Poly)
divModPoly f (Poly gs)
  | null gs   = error "divModPoly: division by zero polynomial"
  | degree f < degree (Poly gs) = (zeroPoly, f)
  | otherwise = go zeroPoly f
  where
    g  = Poly gs
    dg = degree g
    lc = case leadCoeff g of
           Just c  -> c
           Nothing -> error "impossible"

    go q r
      | degree r < dg = (q, r)
      | otherwise =
          let dr  = degree r
              lr  = case leadCoeff r of Just lrc -> lrc; Nothing -> error "impossible"
              c   = ratDiv lr lc
              d   = dr - dg
              term = Poly (replicate d (ratFromInt 0) ++ [c])
              r'   = subPoly r (mulPoly term g)
          in go (addPoly q term) r'

--- GCD via the Euclidean algorithm, made monic.
gcdPoly :: Poly -> Poly -> Poly
gcdPoly a (Poly bs) = case bs of
  [] -> monicPoly a
  _  -> gcdPoly (Poly bs) (snd (divModPoly a (Poly bs)))

--- Make a polynomial monic (leading coefficient 1).
monicPoly :: Poly -> Poly
monicPoly p@(Poly cs) = case cs of
  [] -> zeroPoly
  _  -> case leadCoeff p of
          Nothing -> zeroPoly
          Just lc -> Poly (map (\c -> ratDiv c lc) cs)

--- Formal derivative.
diffPoly :: Poly -> Poly
diffPoly (Poly cs) = case cs of
  []    -> zeroPoly
  (_:rest) -> mkPoly (zipWith (\i c -> ratMul (ratFromInt i) c) [1..] rest)

--- Compose two polynomials: composePoly f g = f(g(x)).
composePoly :: Poly -> Poly -> Poly
composePoly (Poly cs) g = case cs of
  [] -> zeroPoly
  _  -> foldr (\c acc -> addPoly (constPoly c) (mulPoly g acc)) zeroPoly cs

--- Square-free factorization via Yun's algorithm.
--- Returns a list of (factor, multiplicity) pairs.
squareFree :: Poly -> [(Poly, Int)]
squareFree (Poly cs) = case cs of
  [] -> []
  _  ->
    let f   = Poly cs
        f'  = diffPoly f
        c   = gcdPoly f f'
        w   = fst (divModPoly f c)
    in go w c 1
  where
    go w c i
      | degree w == 0 =
          if degree c > 0 then [(c, i)]
          else []
      | otherwise =
          let y    = gcdPoly w c
              z    = fst (divModPoly w y)
              c'   = fst (divModPoly c y)
              rest = go y c' (i + 1)
          in if degree z > 0 then (z, i) : rest else rest

--- Show a polynomial in human-readable form.
showPoly :: Poly -> String
showPoly (Poly cs) = case cs of
  [] -> "0"
  _  -> "Poly " ++ show cs

instance Eq Poly where
  (Poly as) == (Poly bs) = as == bs

instance Show Poly where
  show = showPoly
