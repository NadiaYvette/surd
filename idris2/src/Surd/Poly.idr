||| Dense univariate polynomial arithmetic over rings and fields.
|||
||| Polynomials are stored as coefficient lists, low-degree first.
||| Invariant: trailing zeros are stripped (leading coefficient is nonzero),
||| except for the zero polynomial which is [].
|||
||| Includes Ring/Field interfaces for coefficient abstraction,
||| plus Euclidean division, GCD, square-free factorisation (Yun),
||| and composition.
module Surd.Poly

import Data.List

%default covering

------------------------------------------------------------------------
-- Coefficient interfaces
------------------------------------------------------------------------

||| A ring: addition, multiplication, zero, one, negation.
public export
interface Eq k => Ring k where
  rZero : k
  rOne  : k
  rAdd  : k -> k -> k
  rMul  : k -> k -> k
  rNeg  : k -> k
  rSub  : k -> k -> k
  rSub a b = rAdd a (rNeg b)
  rFromInteger : Integer -> k

||| A field extends a ring with division.
public export
interface Ring k => Field k where
  rDiv : k -> k -> k
  rInv : k -> k
  rInv x = rDiv rOne x

------------------------------------------------------------------------
-- Polynomial type
------------------------------------------------------------------------

||| A univariate polynomial with coefficients in k, stored as a list
||| of coefficients, low-degree first. Invariant: no trailing zeros
||| (the leading coefficient is nonzero), except for the zero polynomial
||| which is [].
public export
record Poly k where
  constructor MkPoly
  coeffs : List k

||| Strip trailing zeros from a coefficient list.
stripZeros : Ring k => List k -> List k
stripZeros = reverse . dropWhile (\c => c == rZero) . reverse

||| Smart constructor: strip trailing zeros.
export
mkPoly : Ring k => List k -> Poly k
mkPoly cs = MkPoly (stripZeros cs)

||| The zero polynomial.
export
zeroPoly : Poly k
zeroPoly = MkPoly []

||| A constant polynomial.
export
constPoly : Ring k => k -> Poly k
constPoly c = if c == rZero then zeroPoly else MkPoly [c]

||| The polynomial x.
export
monoX : Ring k => Poly k
monoX = MkPoly [rZero, rOne]

||| Degree of the polynomial. Returns Nothing for the zero polynomial.
export
degree : Poly k -> Maybe Nat
degree (MkPoly []) = Nothing
degree (MkPoly cs) = Just (minus (length cs) 1)

||| Degree as Integer, returning -1 for zero polynomial (matches Haskell API).
export
degreeInt : Poly k -> Integer
degreeInt (MkPoly []) = -1
degreeInt (MkPoly cs) = cast (minus (length cs) 1)

||| Leading coefficient. Returns Nothing for the zero polynomial.
export
leadCoeff : Poly k -> Maybe k
leadCoeff (MkPoly []) = Nothing
leadCoeff (MkPoly cs) = last' cs

------------------------------------------------------------------------
-- Arithmetic helpers
------------------------------------------------------------------------

zipWithDefault : a -> (a -> a -> a) -> List a -> List a -> List a
zipWithDefault _ _ [] [] = []
zipWithDefault d f (a :: as) [] = f a d :: zipWithDefault d f as []
zipWithDefault d f [] (b :: bs) = f d b :: zipWithDefault d f [] bs
zipWithDefault d f (a :: as) (b :: bs) = f a b :: zipWithDefault d f as bs

------------------------------------------------------------------------
-- Polynomial arithmetic
------------------------------------------------------------------------

||| Add two polynomials.
export
addPoly : Ring k => Poly k -> Poly k -> Poly k
addPoly (MkPoly as) (MkPoly bs) = mkPoly (zipWithDefault rZero rAdd as bs)

||| Subtract two polynomials.
export
subPoly : Ring k => Poly k -> Poly k -> Poly k
subPoly (MkPoly as) (MkPoly bs) = mkPoly (zipWithDefault rZero rSub as bs)

||| Negate a polynomial.
export
negPoly : Ring k => Poly k -> Poly k
negPoly (MkPoly cs) = MkPoly (map rNeg cs)

||| Multiply a polynomial by a scalar.
export
scalePoly : Ring k => k -> Poly k -> Poly k
scalePoly s (MkPoly []) = zeroPoly
scalePoly s p =
  if s == rZero then zeroPoly
  else mkPoly (map (rMul s) (coeffs p))

||| Update a list at a given index.
updateAt' : Nat -> (a -> a) -> List a -> List a
updateAt' _ _ [] = []
updateAt' Z f (x :: xs) = f x :: xs
updateAt' (S k) f (x :: xs) = x :: updateAt' k f xs

||| Multiply two polynomials via schoolbook algorithm.
export
mulPoly : Ring k => Poly k -> Poly k -> Poly k
mulPoly (MkPoly []) _ = zeroPoly
mulPoly _ (MkPoly []) = zeroPoly
mulPoly (MkPoly as) (MkPoly bs) =
  let rlen = minus (length as + length bs) 1
      blank : List k
      blank = replicate rlen (the k rZero)
      terms : List (Nat, k)
      terms = do (i, a) <- zip (iterateN (length as) (+ 1) 0) as
                 (j, b) <- zip (iterateN (length bs) (+ 1) 0) bs
                 pure (i + j, rMul a b)
      addTerm : List k -> (Nat, k) -> List k
      addTerm cs (idx, c) = updateAt' idx (rAdd c) cs
      result : List k
      result = foldl addTerm blank terms
  in mkPoly result

||| Evaluate a polynomial at a point using Horner's method.
export
evalPoly : Ring k => Poly k -> k -> k
evalPoly (MkPoly []) _ = rZero
evalPoly (MkPoly cs) x = foldr (\c, acc => rAdd c (rMul x acc)) rZero cs

||| Make a polynomial monic (leading coefficient 1).
export
monicPoly : Field k => Poly k -> Poly k
monicPoly (MkPoly []) = zeroPoly
monicPoly p =
  case leadCoeff p of
    Nothing => zeroPoly
    Just lc => MkPoly (map (\c => rDiv c lc) (coeffs p))

||| Polynomial division with remainder: divModPoly f g = (q, r)
||| such that f = g*q + r and degree r < degree g.
||| Requires field coefficients.
export
divModPoly : Field k => Poly k -> Poly k -> (Poly k, Poly k)
divModPoly _ (MkPoly []) = (zeroPoly, zeroPoly) -- division by zero
divModPoly f g =
  if degreeInt f < degreeInt g
    then (zeroPoly, f)
    else go zeroPoly f
  where
    dg : Integer
    dg = degreeInt g
    lc : k
    lc = case leadCoeff g of
           Just c  => c
           Nothing => rOne  -- unreachable

    go : Poly k -> Poly k -> (Poly k, Poly k)
    go q r =
      if degreeInt r < dg then (q, r)
      else
        let dr = degreeInt r
            lr = case leadCoeff r of Just c => c; Nothing => rZero
            c  = rDiv lr lc
            d  = cast {to = Nat} (dr - dg)
            term = MkPoly (replicate d (the k rZero) ++ [c])
            r'   = subPoly r (mulPoly term g)
        in go (addPoly q term) r'

||| GCD of two polynomials via the Euclidean algorithm, made monic.
export
gcdPoly : Field k => Poly k -> Poly k -> Poly k
gcdPoly a (MkPoly []) = monicPoly a
gcdPoly a b = gcdPoly b (snd (divModPoly a b))

||| Formal derivative.
export
diffPoly : Ring k => Poly k -> Poly k
diffPoly (MkPoly []) = zeroPoly
diffPoly (MkPoly (_ :: cs)) =
  mkPoly $ zipWith (\i, c => rMul (rFromInteger i) c)
                   (iterateN (length cs) (+ 1) 1)
                   cs

||| Compose two polynomials: composePoly f g = f(g(x)).
export
composePoly : Ring k => Poly k -> Poly k -> Poly k
composePoly (MkPoly []) _ = zeroPoly
composePoly (MkPoly cs) g =
  foldr (\c, acc => addPoly (constPoly c) (mulPoly g acc)) zeroPoly cs

||| Square-free factorisation via Yun's algorithm.
||| Returns a list of (factor, multiplicity) pairs.
export
squareFree : Field k => Poly k -> List (Poly k, Nat)
squareFree (MkPoly []) = []
squareFree f =
  let f'  = diffPoly f
      c   = gcdPoly f f'
      w   = fst (divModPoly f c)
  in go w c 1
  where
    go : Poly k -> Poly k -> Nat -> List (Poly k, Nat)
    go w c i =
      case degree w of
        Just Z =>
          case degree c of
            Just d => if d > 0 then [(c, i)] else []
            Nothing => []
        Nothing => []
        _ =>
          let y    = gcdPoly w c
              z    = fst (divModPoly w y)
              c'   = fst (divModPoly c y)
              rest = go y c' (S i)
          in case degree z of
               Just d => if d > 0 then (z, i) :: rest else rest
               _ => rest

------------------------------------------------------------------------
-- Eq, Show
------------------------------------------------------------------------

export
(Eq k) => Eq (Poly k) where
  (MkPoly as) == (MkPoly bs) = as == bs

export
(Show k) => Show (Poly k) where
  show (MkPoly cs) = "Poly " ++ show cs
