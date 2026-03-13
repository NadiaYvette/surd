--- Normal form for radical expressions: Q-linear combinations of
--- products of radical atoms.
---
--- Every expression in Q[sqrt(2), sqrt(3), cbrt(5), ...] has a unique
--- representation as a sum of (rational coefficient * monomial), where
--- each monomial is a product of radical atoms raised to bounded powers.
module NormalForm
  ( Atom(..)
  , Monomial(..)
  , NormExpr(..)
  , normLit
  , normAtom
  , normRoot
  , normAdd
  , normSub
  , normNeg
  , normMul
  , normScale
  , normPow
  , normInv
  , toNormExpr
  , fromNormExpr
  , normIsZero
  ) where

import Rational
import RadExpr
import Positive (unsafePositive)
import PrimeFactors (factorise)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- A radical atom: an irreducible nth root of a positive rational.
data Atom
  = RatRoot Int Rational    -- nth root of r (r > 0, reduced)
  | ImagUnit               -- sqrt(-1) = i
  | NestedRoot Int (RadExpr Rational)  -- nth root of non-rational expr

--- A monomial: product of atoms raised to positive powers.
--- Represented as an association list (Atom -> exponent).
data Monomial = Monomial [(Atom, Int)]

--- A normalized expression: Q-linear combination of monomials.
--- Represented as an association list (Monomial -> coefficient).
data NormExpr = NormExpr [(Monomial, Rational)]

--- The unit monomial (product of zero atoms = 1).
unitMono :: Monomial
unitMono = Monomial []

--- Literal constant.
normLit :: Rational -> NormExpr
normLit r
  | r == rZero = NormExpr []
  | otherwise  = NormExpr [(unitMono, r)]

--- Single atom as a NormExpr.
normAtom :: Atom -> NormExpr
normAtom a = NormExpr [(Monomial [(a, 1)], rOne)]

--- Root construction.
normRoot :: Int -> NormExpr -> NormExpr
normRoot n ne =
  -- For single-monomial NormExprs with rational coefficient,
  -- create atom directly.
  case ne of
    NormExpr [(Monomial [], c)] ->
      if ratLt c rZero && even n
      then -- sqrt(-r) = i * sqrt(r)
        normMul (normAtom ImagUnit) (normRoot n (normLit (ratNeg c)))
      else if ratLt c rZero && odd n
      then normNeg (normRoot n (normLit (ratNeg c)))
      else -- Positive rational: create RatRoot atom
        let (out, inn) = extractNthPower n c
        in if inn == rOne
           then normLit out
           else normMul (normLit out) (normAtom (RatRoot n inn))
    _ ->
      -- Multi-term or complex: create NestedRoot
      normAtom (NestedRoot n (fromNormExpr ne))

--- Extract the largest nth power from a rational.
extractNthPower :: Int -> Rational -> (Rational, Rational)
extractNthPower n r =
  let num = numerator r
      den = denominator r
      (numOut, numIn) = extractNthPowerInt n (absInt num)
      (denOut, denIn) = extractNthPowerInt n (absInt den)
      sign = if num < 0 then Rational.fromInt (negate 1) else rOne
  in if denIn == 1
     then (ratMul sign (ratDiv (Rational.fromInt numOut)
                               (Rational.fromInt denOut)),
           Rational.fromInt numIn)
     else let newIn = numIn * intPow denIn (n - 1)
              newOut = ratDiv (ratMul sign (Rational.fromInt numOut))
                              (ratMul (Rational.fromInt denOut)
                                      (Rational.fromInt denIn))
          in (newOut, Rational.fromInt newIn)

extractNthPowerInt :: Int -> Int -> (Int, Int)
extractNthPowerInt n m =
  let fs = factorise (unsafePositive (absInt m))
      extracted = foldl (\acc (p, e) -> acc * intPow p (e `div` n)) 1 fs
      remainder = foldl (\acc (p, e) -> acc * intPow p (e `mod` n)) 1 fs
  in (extracted, remainder)

intPow :: Int -> Int -> Int
intPow b e
  | e == 0    = 1
  | otherwise = b * intPow b (e - 1)

absInt :: Int -> Int
absInt x = if x < 0 then negate x else x

--- Addition.
normAdd :: NormExpr -> NormExpr -> NormExpr
normAdd (NormExpr as) (NormExpr bs) =
  NormExpr (mergeTerms as bs)

--- Subtraction.
normSub :: NormExpr -> NormExpr -> NormExpr
normSub a b = normAdd a (normNeg b)

--- Negation.
normNeg :: NormExpr -> NormExpr
normNeg (NormExpr ts) = NormExpr (map (\(m, c) -> (m, ratNeg c)) ts)

--- Scalar multiplication.
normScale :: Rational -> NormExpr -> NormExpr
normScale s (NormExpr ts)
  | s == rZero = NormExpr []
  | otherwise  = NormExpr (filterZero (map (\(m, c) -> (m, ratMul s c)) ts))

--- Multiplication.
normMul :: NormExpr -> NormExpr -> NormExpr
normMul (NormExpr as) (NormExpr bs) =
  let terms = [mulMonoTerms a b | a <- as, b <- bs]
  in NormExpr (foldl mergeTerms [] (map (\t -> [t]) terms))

mulMonoTerms :: (Monomial, Rational) -> (Monomial, Rational)
              -> (Monomial, Rational)
mulMonoTerms (m1, c1) (m2, c2) =
  let (m', extra) = mulMonomials m1 m2
  in (m', ratMul (ratMul c1 c2) extra)

--- Multiply two monomials, reducing exponents.
--- Returns (product monomial, extra rational coefficient from reductions).
mulMonomials :: Monomial -> Monomial -> (Monomial, Rational)
mulMonomials (Monomial as) (Monomial bs) =
  let combined = mergeAtoms as bs
      (reduced, extra) = reduceMonomial combined
  in (Monomial reduced, extra)

--- Merge atom lists, adding exponents for matching atoms.
mergeAtoms :: [(Atom, Int)] -> [(Atom, Int)] -> [(Atom, Int)]
mergeAtoms [] bs = bs
mergeAtoms (a:as) bs = mergeAtoms as (insertAtom a bs)

insertAtom :: (Atom, Int) -> [(Atom, Int)] -> [(Atom, Int)]
insertAtom (a, e) [] = [(a, e)]
insertAtom (a, e) ((a', e'):rest) =
  if atomEq a a'
  then (a', e + e') : rest
  else (a', e') : insertAtom (a, e) rest

--- Reduce a monomial: i^2 = -1, (nthroot(r))^n = r, etc.
--- Returns (reduced atoms, extra coefficient).
reduceMonomial :: [(Atom, Int)] -> ([(Atom, Int)], Rational)
reduceMonomial atoms = foldl reduceOne ([], rOne) atoms
  where
    reduceOne (acc, coeff) (atom, e) =
      case atom of
        ImagUnit ->
          let e' = e `mod` 4
              -- i^0=1, i^1=i, i^2=-1, i^3=-i
              (c, newE) = case e' of
                            0 -> (rOne, 0)
                            1 -> (rOne, 1)
                            2 -> (Rational.fromInt (negate 1), 0)
                            3 -> (Rational.fromInt (negate 1), 1)
                            _ -> (rOne, e')
          in if newE == 0
             then (acc, ratMul coeff c)
             else ((ImagUnit, newE) : acc, ratMul coeff c)
        RatRoot n r ->
          let fullPowers = e `div` n
              remainder  = e `mod` n
              -- (nthroot(r))^n = r
              c = ratPow r fullPowers
          in if remainder == 0
             then (acc, ratMul coeff c)
             else ((RatRoot n r, remainder) : acc, ratMul coeff c)
        NestedRoot n _ ->
          -- For nested roots, exponent reduction: e >= n means we
          -- extract the radicand.  For simplicity, just keep as-is
          -- when e < n.
          if e < n
          then ((atom, e) : acc, coeff)
          else ((atom, e `mod` n) : acc, coeff)  -- simplified

--- Merge term lists (adding coefficients for matching monomials).
mergeTerms :: [(Monomial, Rational)] -> [(Monomial, Rational)]
           -> [(Monomial, Rational)]
mergeTerms as [] = as
mergeTerms as (b:bs) = mergeTerms (insertTerm b as) bs

insertTerm :: (Monomial, Rational) -> [(Monomial, Rational)]
           -> [(Monomial, Rational)]
insertTerm (m, c) [] = if c == rZero then [] else [(m, c)]
insertTerm (m, c) ((m', c'):rest) =
  if monoEq m m'
  then let s = ratAdd c c'
       in if s == rZero then rest else (m', s) : rest
  else (m', c') : insertTerm (m, c) rest

--- Filter out zero-coefficient terms.
filterZero :: [(Monomial, Rational)] -> [(Monomial, Rational)]
filterZero = filter (\(_, c) -> c /= rZero)

--- Integer power.
normPow :: NormExpr -> Int -> NormExpr
normPow e n
  | n == 0    = normLit rOne
  | n == 1    = e
  | n < 0     = normInv (normPow e (negate n))
  | even n    = let half = normPow e (n `div` 2)
                in normMul half half
  | otherwise = normMul e (normPow e (n - 1))

--- Inverse of a NormExpr.
--- Only handles single-monomial case (negates exponents).
normInv :: NormExpr -> NormExpr
normInv (NormExpr ts) = case ts of
  [(Monomial atoms, c)] ->
    let c' = ratDiv rOne c
        atoms' = map (\(a, e) -> (a, negate e)) atoms
        (reduced, extra) = reduceMonomial atoms'
    in NormExpr [(Monomial reduced, ratMul c' extra)]
  _ -> error "normInv: multi-term inversion not supported"

--- Convert a RadExpr to NormExpr.
toNormExpr :: RadExpr Rational -> NormExpr
toNormExpr expr = case expr of
  Lit r     -> normLit r
  Neg a     -> normNeg (toNormExpr a)
  Add a b   -> normAdd (toNormExpr a) (toNormExpr b)
  Mul a b   -> normMul (toNormExpr a) (toNormExpr b)
  Inv a     -> normInv (toNormExpr a)
  Root n a  -> normRoot n (toNormExpr a)
  Pow a n   -> normPow (toNormExpr a) n

--- Convert a NormExpr back to RadExpr.
fromNormExpr :: NormExpr -> RadExpr Rational
fromNormExpr (NormExpr ts) = case ts of
  [] -> Lit rZero
  _  -> foldl1 Add (map termToExpr ts)

termToExpr :: (Monomial, Rational) -> RadExpr Rational
termToExpr (Monomial atoms, c) =
  let atomExprs = map atomToExpr atoms
      body = case atomExprs of
               [] -> Lit rOne
               _  -> foldl1 Mul atomExprs
  in if c == rOne
     then body
     else if ratEq c rNegOne
     then Neg body
     else Mul (Lit c) body
  where
    rNegOne = Rational.fromInt (negate 1)

atomToExpr :: (Atom, Int) -> RadExpr Rational
atomToExpr (atom, e) =
  let base = case atom of
               RatRoot n r      -> Root n (Lit r)
               ImagUnit         -> Root 2 (Lit (Rational.fromInt (negate 1)))
               NestedRoot n ex  -> Root n ex
  in if e == 1 then base else Pow base e

--- Check if a NormExpr is zero.
normIsZero :: NormExpr -> Bool
normIsZero (NormExpr ts) = null ts

--- Atom equality.
atomEq :: Atom -> Atom -> Bool
atomEq a b = case (a, b) of
  (RatRoot n1 r1, RatRoot n2 r2)     -> n1 == n2 && r1 == r2
  (ImagUnit, ImagUnit)               -> True
  (NestedRoot n1 e1, NestedRoot n2 e2) -> n1 == n2 && e1 == e2
  _                                    -> False

--- Monomial equality.
monoEq :: Monomial -> Monomial -> Bool
monoEq (Monomial as) (Monomial bs) =
  length as == length bs && all (\(a, e) -> elemAtom a e bs) as

elemAtom :: Atom -> Int -> [(Atom, Int)] -> Bool
elemAtom _ _ [] = False
elemAtom a e ((a', e'):rest) =
  if atomEq a a' && e == e' then True else elemAtom a e rest

instance Eq Atom where
  (==) = atomEq

instance Ord Atom where
  compare a b = case (a, b) of
    (RatRoot n1 r1, RatRoot n2 r2) ->
      case compare n1 n2 of
        EQ -> compare r1 r2
        o  -> o
    (RatRoot _ _, _) -> LT
    (ImagUnit, RatRoot _ _) -> GT
    (ImagUnit, ImagUnit) -> EQ
    (ImagUnit, NestedRoot _ _) -> LT
    (NestedRoot _ _, RatRoot _ _) -> GT
    (NestedRoot _ _, ImagUnit) -> GT
    (NestedRoot n1 e1, NestedRoot n2 e2) ->
      case compare n1 n2 of
        EQ -> compare e1 e2
        o  -> o

instance Eq Monomial where
  (==) = monoEq

instance Ord Monomial where
  compare (Monomial as) (Monomial bs) = compare as bs

instance Show Atom where
  show atom = case atom of
    RatRoot n r     -> "RatRoot " ++ show n ++ " " ++ show r
    ImagUnit        -> "ImagUnit"
    NestedRoot n e  -> "NestedRoot " ++ show n ++ " (" ++ show e ++ ")"

instance Show Monomial where
  show (Monomial as) = "Monomial " ++ show as

instance Show NormExpr where
  show (NormExpr ts) = "NormExpr " ++ show ts
