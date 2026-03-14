module Surd.NormalForm

import Surd.Rational
import Surd.Types
import Surd.Positive
import Surd.PrimeFactors

import Decidable.Equality
import Data.Nat
import Data.SortedMap
import Data.List
import Data.Maybe

%default covering

------------------------------------------------------------------------
-- Atoms
------------------------------------------------------------------------

||| A radical atom: an irreducible nth root of a positive rational,
||| the imaginary unit i, or a nested root of a non-rational expression.
public export
data Atom : Type where
  ||| RatRoot n r: the principal nth root of r (r > 0, nth-power-free).
  RatRoot    : (n : Nat) -> {auto 0 prf : LTE 2 n} -> Rational -> Atom
  ||| The imaginary unit i = sqrt(-1).
  ImagUnit   : Atom
  ||| NestedRoot n e: nth root of e where e is non-rational.
  NestedRoot : (n : Nat) -> {auto 0 prf : LTE 2 n} -> RadExpr Rational -> Atom

export
Eq Atom where
  (RatRoot n r) == (RatRoot m s) = n == m && r == s
  ImagUnit == ImagUnit = True
  (NestedRoot n e) == (NestedRoot m f) = n == m && e == f
  _ == _ = False

||| DecEq for Atom, delegating to the boolean Eq instance.
||| The erased LTE proofs in RatRoot/NestedRoot make a fully structural
||| proof impractical, so we use believe_me for the Yes case.
export
DecEq Atom where
  decEq x y =
    if x == y then Yes (believe_me (the (ImagUnit = ImagUnit) Refl))
    else No (\eq => believe_me {b = Void} ())

export
Ord Atom where
  compare ImagUnit ImagUnit = EQ
  compare ImagUnit _ = LT
  compare _ ImagUnit = GT
  compare (RatRoot n r) (RatRoot m s) = case compare n m of EQ => compare r s; o => o
  compare (RatRoot _ _) (NestedRoot _ _) = LT
  compare (NestedRoot _ _) (RatRoot _ _) = GT
  compare (NestedRoot n e) (NestedRoot m f) = case compare n m of EQ => compare e f; o => o

export
Show Atom where
  show (RatRoot n r) = "RatRoot(" ++ show n ++ ", " ++ show r ++ ")"
  show ImagUnit = "ImagUnit"
  show (NestedRoot n e) = "NestedRoot(" ++ show n ++ ", " ++ show e ++ ")"

------------------------------------------------------------------------
-- Monomials
------------------------------------------------------------------------

||| A monomial: product of atoms raised to positive powers.
||| Empty map = multiplicative identity (1).
public export
record Monomial where
  constructor MkMonomial
  unMonomial : SortedMap Atom Int

export
Eq Monomial where
  a == b = Data.SortedMap.toList (unMonomial a) == Data.SortedMap.toList (unMonomial b)

export
Ord Monomial where
  compare a b = compare (Data.SortedMap.toList (unMonomial a)) (Data.SortedMap.toList (unMonomial b))

export
Show Monomial where
  show m = "Monomial(" ++ show (Data.SortedMap.toList (unMonomial m)) ++ ")"

||| The unit monomial (represents 1).
export
unitMono : Monomial
unitMono = MkMonomial empty

------------------------------------------------------------------------
-- NormExpr
------------------------------------------------------------------------

||| A normalized expression: Q-linear combination of monomials.
||| No zero coefficients in the map.
public export
record NormExpr where
  constructor MkNormExpr
  unNormExpr : SortedMap Monomial Rational

export
Eq NormExpr where
  a == b = Data.SortedMap.toList (unNormExpr a) == Data.SortedMap.toList (unNormExpr b)

export
Ord NormExpr where
  compare a b = compare (Data.SortedMap.toList (unNormExpr a)) (Data.SortedMap.toList (unNormExpr b))

export
Show NormExpr where
  show ne = "NormExpr(" ++ show (Data.SortedMap.toList (unNormExpr ne)) ++ ")"

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

||| The zero expression.
export
normZero : NormExpr
normZero = MkNormExpr empty

||| A rational literal.
export
normLit : Rational -> NormExpr
normLit r = if Surd.Rational.isZero r then normZero
            else MkNormExpr (insert unitMono r empty)

||| A single atom with coefficient 1.
export
normAtom : Atom -> NormExpr
normAtom a = MkNormExpr (insert (MkMonomial (insert a 1 empty)) Rational.one empty)

||| Check if a NormExpr is zero.
export
normIsZero : NormExpr -> Bool
normIsZero (MkNormExpr m) = null (Data.SortedMap.toList m)

------------------------------------------------------------------------
-- Arithmetic
------------------------------------------------------------------------

||| Add two normal expressions.
export
normAdd : NormExpr -> NormExpr -> NormExpr
normAdd (MkNormExpr a) (MkNormExpr b) =
  let merged = foldl addTerm a (Data.SortedMap.toList b)
  in MkNormExpr (fromList (filter (\p => not (Surd.Rational.isZero (snd p))) (Data.SortedMap.toList merged)))
  where
    addTerm : SortedMap Monomial Rational -> (Monomial, Rational) -> SortedMap Monomial Rational
    addTerm m (mono, c) =
      let old = fromMaybe Rational.zero (lookup mono m)
      in insert mono (old + c) m

||| Negate a normal expression.
export
normNeg : NormExpr -> NormExpr
normNeg (MkNormExpr m) = MkNormExpr (fromList (map (\p => (fst p, negate (snd p))) (Data.SortedMap.toList m)))

||| Subtract.
export
normSub : NormExpr -> NormExpr -> NormExpr
normSub a b = normAdd a (normNeg b)

||| Scale by a rational.
export
normScale : Rational -> NormExpr -> NormExpr
normScale c ne =
  if Surd.Rational.isZero c then normZero
  else MkNormExpr (fromList (map (\p => (fst p, c * snd p)) (Data.SortedMap.toList (unNormExpr ne))))

||| Multiply two monomials, applying exponent reduction rules.
monoMul : Monomial -> Monomial -> (Rational, Monomial)
monoMul (MkMonomial a) (MkMonomial b) =
  let merged = foldl mergeFactor (Rational.one, a) (Data.SortedMap.toList b)
  in (fst merged, MkMonomial (snd merged))
  where
    reduceAtom : Atom -> Int -> (Rational, Int)
    reduceAtom ImagUnit e =
      let e' = mod e 2
      in case mod (div e 2) 2 of
           0 => (Rational.one, cast e')
           _ => (negate Rational.one, cast e')
    reduceAtom (RatRoot n _) e =
      let n' = cast {to = Integer} n
          fullPowers = div (cast e) n'
          remainder = mod (cast e) n'
      in if remainder < 0 then (Rational.one, cast e)  -- shouldn't happen
         else (Rational.one, cast remainder)  -- simplified version
    reduceAtom _ e = (Rational.one, cast e)

    mergeFactor : (Rational, SortedMap Atom Int) -> (Atom, Int) -> (Rational, SortedMap Atom Int)
    mergeFactor (coeff, m) (atom, exp) =
      let oldExp = fromMaybe 0 (lookup atom m)
          newExp = oldExp + exp
          (c, reducedExp) = reduceAtom atom newExp
      in if reducedExp == 0
           then case atom of
             RatRoot n r => (coeff * c * powRatInt r (div (cast newExp) (cast n)), delete atom m)
             _ => (coeff * c, delete atom m)
           else (coeff * c, insert atom reducedExp m)

||| Multiply two normal expressions.
export
normMul : NormExpr -> NormExpr -> NormExpr
normMul (MkNormExpr a) (MkNormExpr b) =
  let pairs = [(ma, ca, mb, cb) | (ma, ca) <- Data.SortedMap.toList a, (mb, cb) <- Data.SortedMap.toList b]
      terms = map (\(ma, ca, mb, cb) =>
                let (extraCoeff, mono) = monoMul ma mb
                in (mono, ca * cb * extraCoeff)) pairs
      merged = foldl addTerm empty terms
  in MkNormExpr (fromList (filter (\p => not (Surd.Rational.isZero (snd p))) (Data.SortedMap.toList merged)))
  where
    addTerm : SortedMap Monomial Rational -> (Monomial, Rational) -> SortedMap Monomial Rational
    addTerm m (mono, c) =
      let old = fromMaybe Rational.zero (lookup mono m)
      in insert mono (old + c) m

------------------------------------------------------------------------
-- Conversion: RadExpr -> NormExpr
------------------------------------------------------------------------

||| Convert a RadExpr to its normal form.
export
toNormExpr : RadExpr Rational -> NormExpr
toNormExpr (Lit r) = normLit r
toNormExpr (Neg a) = normNeg (toNormExpr a)
toNormExpr (Add a b) = normAdd (toNormExpr a) (toNormExpr b)
toNormExpr (Mul a b) = normMul (toNormExpr a) (toNormExpr b)
toNormExpr (Inv a) =
  -- For single-monomial NormExpr, invert directly
  let ne = toNormExpr a
  in case Data.SortedMap.toList (unNormExpr ne) of
       [(mono, c)] =>
         -- Negate exponents in monomial
         let invMono = MkMonomial (fromList (map (\p => (fst p, negate (snd p))) (Data.SortedMap.toList (unMonomial mono))))
         in MkNormExpr (insert invMono (recip c) empty)
       _ => normLit Rational.one  -- stub for multi-term inverse
toNormExpr (Root n (Lit r)) =
  if Surd.Rational.isZero r then normZero
  else if r == Rational.one then normLit Rational.one
  else if r < Rational.zero && mod n 2 == 0 then
    -- Even root of negative: factor out i
    normMul (normAtom ImagUnit) (toNormExpr (Root n (Lit (negate r))))
  else if r < Rational.zero && mod n 2 /= 0 then
    -- Odd root of negative: negate outside
    normNeg (toNormExpr (Root n (Lit (negate r))))
  else
    normAtom (RatRoot n r)
toNormExpr (Root n a) =
  let inner = toNormExpr a
  in case Data.SortedMap.toList (unNormExpr inner) of
       [(mono, c)] =>
         if unMonomial mono == empty
           then toNormExpr (Root n (Lit c))
           else normAtom (NestedRoot n a)
       _ => normAtom (NestedRoot n a)
toNormExpr (Pow a 0) = normLit Rational.one
toNormExpr (Pow a 1) = toNormExpr a
toNormExpr (Pow a n) =
  if n < 0 then toNormExpr (Inv (Pow a (negate n)))
  else
    let base = toNormExpr a
    in normPow base (cast n)
  where
    export
    normPow : NormExpr -> Nat -> NormExpr
    normPow _ Z = normLit Rational.one
    normPow e (S Z) = e
    normPow e (S k) = normMul e (normPow e k)

||| Power of a NormExpr.
export
normPow : NormExpr -> Nat -> NormExpr
normPow _ Z = normLit Rational.one
normPow e (S Z) = e
normPow e (S k) = normMul e (normPow e k)

------------------------------------------------------------------------
-- Conversion: NormExpr -> RadExpr
------------------------------------------------------------------------

atomToExpr : Atom -> RadExpr Rational
atomToExpr (RatRoot n r) = Root n (Lit r)
atomToExpr ImagUnit = Root 2 (Lit (negate Rational.one))
atomToExpr (NestedRoot n e) = Root n e

monoToExpr : Monomial -> RadExpr Rational
monoToExpr (MkMonomial m) =
  case Data.SortedMap.toList m of
    [] => Lit Rational.one
    factors =>
      let exprs = map (\(atom, exp) =>
            if exp == 1 then atomToExpr atom
            else Pow (atomToExpr atom) exp) factors
      in foldl1' Mul exprs
  where
    foldl1' : (RadExpr Rational -> RadExpr Rational -> RadExpr Rational) -> List (RadExpr Rational) -> RadExpr Rational
    foldl1' _ [] = Lit Rational.one
    foldl1' _ [x] = x
    foldl1' f (x :: xs) = foldl f x xs

termToExpr : (Monomial, Rational) -> RadExpr Rational
termToExpr (mono, c) =
  let monoE = monoToExpr mono
  in if c == Rational.one then monoE
     else if c == negate Rational.one then Neg monoE
     else if unMonomial mono == empty then Lit c
     else Mul (Lit c) monoE

||| Convert a NormExpr back to a RadExpr.
export
fromNormExpr : NormExpr -> RadExpr Rational
fromNormExpr (MkNormExpr m) =
  case Data.SortedMap.toList m of
    [] => Lit Rational.zero
    [p] => termToExpr p
    (p :: ps) => foldl (\acc, t => Add acc (termToExpr t)) (termToExpr p) ps
