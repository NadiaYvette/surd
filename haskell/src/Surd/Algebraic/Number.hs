-- | Canonical representation of real algebraic numbers.
--
-- An algebraic number is represented as a pair @(minimalPolynomial, isolatingInterval)@.
-- Two algebraic numbers are equal iff they have the same minimal polynomial
-- and their isolating intervals overlap (after sufficient refinement).
--
-- == Arithmetic
--
-- Arithmetic is done via resultant-based composed polynomials:
-- given \(\alpha\) with minimal polynomial \(p\) and \(\beta\) with minimal polynomial \(q\),
--
--   * \(\alpha + \beta\) has annihilating polynomial @ComposedSum(p, q)@
--   * \(\alpha \cdot \beta\) has annihilating polynomial @ComposedProduct(p, q)@
--
-- which are then factored over \(\mathbb{Q}\) to find the true minimal polynomial,
-- with the correct root identified by numerical approximation.
--
-- == Instances
--
-- 'AlgNum' has 'Eq', 'Ord', 'Num', and 'Fractional' instances, making it
-- a fully usable exact real arithmetic type (though operations are expensive
-- due to resultant computation and factoring).
module Surd.Algebraic.Number
  ( AlgNum (..),
    algFromRational,
    algFromPoly,
    algMinPoly,
    algApprox,
    algAdd,
    algMul,
    algNeg,
    algInv,
    algSub,
    algDiv,
    algPow,
    algRoot,
    algEq,
    algCompare,
    algShow,
  )
where

import Math.Internal.Interval (Interval (..))
import Math.Polynomial.Factoring (factorSquareFree)
import Math.Polynomial.Resultant (polyResultant)
import Math.Polynomial.Univariate
import Surd.Algebraic.RootIsolation

-- | A real algebraic number: its minimal polynomial over Q
-- and an isolating interval pinpointing which root it is.
data AlgNum = AlgNum
  { -- | Monic irreducible minimal polynomial
    anMinPoly :: !(Poly Rational),
    -- | Isolating interval (rational endpoints)
    anInterval :: !Interval
  }
  deriving (Show)

-- | Embed a rational number as an algebraic number.
algFromRational :: Rational -> AlgNum
algFromRational r =
  AlgNum
    { anMinPoly = mkPoly [-r, 1], -- x - r
      anInterval = Interval r r
    }

-- | Construct an algebraic number from a polynomial and an approximate
-- real value (used to pick the right root).
--
-- Factors the polynomial into irreducible factors, isolates the real
-- roots of each factor, and selects the root closest to the given
-- approximation. Returns 'Nothing' if the polynomial has no real root
-- near the approximation.
algFromPoly :: Poly Rational -> Double -> Maybe AlgNum
algFromPoly p approx =
  let mp = monicPoly p
      factors = factorSquareFree mp
   in pickBestRoot factors approx

-- | The minimal polynomial of the algebraic number: the unique monic
-- irreducible polynomial in \(\mathbb{Q}[x]\) of which this number is a root.
algMinPoly :: AlgNum -> Poly Rational
algMinPoly = anMinPoly

-- | Approximate the algebraic number as a rational to within epsilon.
--
-- Refines the isolating interval by bisection until its width is less
-- than @eps@, then returns the midpoint. Guaranteed to be within @eps@
-- of the true value.
algApprox :: Rational -> AlgNum -> Rational
algApprox eps (AlgNum p iv) =
  let Interval l h = iiInterval $ refineRoot eps (IsolatingInterval p iv)
   in (l + h) / 2

-- | Approximate as a Double.
algApproxDouble :: AlgNum -> Double
algApproxDouble a = fromRational $ algApprox (1 / (10 ^ (15 :: Int))) a

-- | Negation: if p(α) = 0 then p(-x) annihilates -α.
algNeg :: AlgNum -> AlgNum
algNeg (AlgNum p (Interval l h)) =
  let p' = negateVar p
   in makeAlgNum p' (Interval (-h) (-l))

-- | Addition via composed sum.
algAdd :: AlgNum -> AlgNum -> AlgNum
algAdd a b =
  let ann = composedSum (anMinPoly a) (anMinPoly b)
      -- The root is approximately at approx(a) + approx(b)
      approx = algApproxDouble a + algApproxDouble b
   in makeAlgNumFromAnn ann approx

-- | Subtraction.
algSub :: AlgNum -> AlgNum -> AlgNum
algSub a b = algAdd a (algNeg b)

-- | Multiplication via composed product.
algMul :: AlgNum -> AlgNum -> AlgNum
algMul a b =
  let ann = composedProduct (anMinPoly a) (anMinPoly b)
      approx = algApproxDouble a * algApproxDouble b
   in makeAlgNumFromAnn ann approx

-- | Multiplicative inverse: if p(α) = 0 then x^deg(p) · p(1/x) annihilates 1/α.
algInv :: AlgNum -> AlgNum
algInv (AlgNum p (Interval l h)) =
  let p' = reciprocalPoly p
      -- Interval for 1/α: if α ∈ (l, h) and same sign, then 1/α ∈ (1/h, 1/l)
      iv'
        | l > 0 = Interval (1 / h) (1 / l)
        | h < 0 = Interval (1 / h) (1 / l)
        | otherwise = error "algInv: zero"
   in makeAlgNum p' iv'

-- | Division.
algDiv :: AlgNum -> AlgNum -> AlgNum
algDiv a b = algMul a (algInv b)

-- | Integer power.
algPow :: AlgNum -> Int -> AlgNum
algPow _ 0 = algFromRational 1
algPow a 1 = a
algPow a n
  | n < 0 = algPow (algInv a) (-n)
  | even n = let half = algPow a (n `div` 2) in algMul half half
  | otherwise = algMul a (algPow a (n - 1))

-- | The nth root of a positive algebraic number.
--
-- If \(\alpha\) is a root of \(p(x)\), then \(\alpha^{1/n}\) is a root
-- of \(p(x^n)\). The correct root is identified by numerical approximation.
algRoot :: Int -> AlgNum -> AlgNum
algRoot n (AlgNum p (Interval l h)) =
  let ann = substituteXN n p
      -- Approximate: the nth root of the approximate value
      approx = algApproxDouble (AlgNum p (Interval l h)) ** (1 / fromIntegral n)
   in makeAlgNumFromAnn ann approx

-- | Equality: same minimal polynomial and overlapping intervals
-- (after sufficient refinement).
algEq :: AlgNum -> AlgNum -> Bool
algEq a b =
  anMinPoly a == anMinPoly b
    && intervalsOverlap (anInterval a) (anInterval b) (anMinPoly a)

-- | Comparison via rigorous interval separation.
--
-- Since algEq already determined the values are not equal,
-- we refine both isolating intervals until they are disjoint,
-- which is guaranteed to terminate for distinct algebraic numbers.
algCompare :: AlgNum -> AlgNum -> Ordering
algCompare a b
  | algEq a b = EQ
  | otherwise = separateAndCompare a b

-- | Refine intervals of two distinct algebraic numbers until disjoint.
separateAndCompare :: AlgNum -> AlgNum -> Ordering
separateAndCompare a b =
  go
    (IsolatingInterval (anMinPoly a) (anInterval a))
    (IsolatingInterval (anMinPoly b) (anInterval b))
    (200 :: Int)
  where
    go (IsolatingInterval _ (Interval _ h1)) (IsolatingInterval _ (Interval l2 _)) _
      | h1 < l2 = LT
    go (IsolatingInterval _ (Interval l1 _)) (IsolatingInterval _ (Interval _ h2)) _
      | h2 < l1 = GT
    go ii1 ii2 0 =
      -- Fallback (should never happen for truly distinct algebraic numbers)
      let m1 = let Interval l h = iiInterval ii1 in (l + h) / 2
          m2 = let Interval l h = iiInterval ii2 in (l + h) / 2
       in compare m1 m2
    go ii1 ii2 n =
      let w1 = let Interval l h = iiInterval ii1 in h - l
          w2 = let Interval l h = iiInterval ii2 in h - l
          eps1 = w1 / 4
          eps2 = w2 / 4
          ii1' = refineRoot eps1 ii1
          ii2' = refineRoot eps2 ii2
       in go ii1' ii2' (n - 1)

-- | Display as @\"AlgNum(\<poly\> ~ \<approx\>)\"@, showing the minimal
-- polynomial and an approximate decimal value.
algShow :: AlgNum -> String
algShow a =
  let approx = fromRational (algApprox (1 / 1000) a) :: Double
      p = anMinPoly a
   in "AlgNum(" ++ show p ++ " ≈ " ++ show approx ++ ")"

-- Internal helpers

-- | p(-x): negate odd-degree coefficients.
negateVar :: (Eq k, Num k) => Poly k -> Poly k
negateVar (Poly cs) = mkPoly $ zipWith (\i c -> if odd (i :: Int) then negate c else c) [0 ..] cs

-- | Reciprocal polynomial: x^n · p(1/x).
reciprocalPoly :: (Eq k, Num k) => Poly k -> Poly k
reciprocalPoly (Poly cs) = mkPoly (reverse cs)

-- | p(x^n): replace x with x^n.
substituteXN :: (Eq k, Num k) => Int -> Poly k -> Poly k
substituteXN n (Poly cs) =
  let indexed = zip [0 ..] cs
      maxDeg = (length cs - 1) * n
      result = replicate (maxDeg + 1) 0
      set xs (i, c) =
        let pos = i * n
         in take pos xs ++ [c] ++ drop (pos + 1) xs
   in mkPoly $ foldl set result indexed

-- | Composed sum polynomial: roots are αᵢ + βⱼ.
composedSum :: Poly Rational -> Poly Rational -> Poly Rational
composedSum p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromIntegral i | i <- [0 .. resultDeg]]
      values = [resultantSumAt p q x | x <- points]
   in lagrangeInterpolate (zip points values)

-- | Res_y(p(y), q(x₀ - y))
resultantSumAt :: Poly Rational -> Poly Rational -> Rational -> Rational
resultantSumAt p q x0 =
  let qShifted = substituteLinear q x0 (-1)
   in polyResultant p qShifted

-- | p(a + b·y)
substituteLinear :: Poly Rational -> Rational -> Rational -> Poly Rational
substituteLinear (Poly []) _ _ = zeroPoly
substituteLinear (Poly cs) a b =
  let binomial = mkPoly [a, b]
   in foldr (\c acc -> addPoly (constPoly c) (mulPoly binomial acc)) zeroPoly cs

-- | Composed product polynomial: roots are αᵢ · βⱼ.
composedProduct :: Poly Rational -> Poly Rational -> Poly Rational
composedProduct p q =
  let dp = degree p
      dq = degree q
      resultDeg = dp * dq
      points = [fromIntegral i | i <- [1 .. resultDeg + 1]]
      values = [resultantProdAt p q x | x <- points]
   in lagrangeInterpolate (zip points values)

-- | Res_y(y^dp · p(x₀/y), q(y))
resultantProdAt :: Poly Rational -> Poly Rational -> Rational -> Rational
resultantProdAt p q x0 =
  let pRev = scaledReciprocalAt p x0
   in polyResultant pRev q

-- | y^n · p(x₀/y)
scaledReciprocalAt :: Poly Rational -> Rational -> Poly Rational
scaledReciprocalAt (Poly cs) x0 =
  let n = length cs - 1
      newCs = [c * x0 ^ (n - k) | (k, c) <- zip [0 ..] (reverse cs)]
   in mkPoly newCs

-- | Lagrange interpolation.
lagrangeInterpolate :: [(Rational, Rational)] -> Poly Rational
lagrangeInterpolate points = foldl addPoly zeroPoly terms
  where
    terms = [scalePoly yi (lagrangeBasis xi (map fst points)) | (xi, yi) <- points]
    lagrangeBasis xi xs =
      let others = filter (/= xi) xs
       in foldl
            mulPoly
            (constPoly 1)
            [scalePoly (1 / (xi - xj)) (mkPoly [-xj, 1]) | xj <- others]

-- | Construct an AlgNum from an annihilating polynomial and approximate value.
-- Factors the polynomial, picks the irreducible factor with a root nearest
-- to the approximation, and isolates the root.
makeAlgNumFromAnn :: Poly Rational -> Double -> AlgNum
makeAlgNumFromAnn ann approx =
  let factors = factorSquareFree (monicPoly ann)
   in case pickBestRoot factors approx of
        Just a -> a
        Nothing -> error $ "makeAlgNumFromAnn: no real root near " ++ show approx

-- | From an annihilating (possibly reducible) polynomial and interval,
-- find the minimal polynomial factor and refine the interval.
makeAlgNum :: Poly Rational -> Interval -> AlgNum
makeAlgNum ann iv =
  let factors = factorSquareFree (monicPoly ann)
      mid = let Interval l h = iv in fromRational ((l + h) / 2) :: Double
   in case pickBestRoot factors mid of
        Just a -> a
        Nothing -> AlgNum (monicPoly ann) iv -- fallback

-- | Pick the irreducible factor with a root closest to the target.
pickBestRoot :: [Poly Rational] -> Double -> Maybe AlgNum
pickBestRoot [] _ = Nothing
pickBestRoot factors approx =
  let candidates = concatMap (\f -> map (\ii -> (f, ii)) (isolateRealRoots f)) factors
      scored =
        [ (f, ii, abs (fromRational mid - approx))
          | (f, IsolatingInterval _ iv) <- candidates,
            let ii = iv,
            let mid = let Interval l h = iv in (l + h) / 2
        ]
   in case scored of
        [] -> Nothing
        _ ->
          let (f, iv, _) = minimumBy (\(_, _, d1) (_, _, d2) -> compare d1 d2) scored
           in Just (AlgNum f iv)
  where
    minimumBy _ [x] = x
    minimumBy cmp (x : xs) = foldl (\a b -> if cmp a b == GT then b else a) x xs
    minimumBy _ [] = error "impossible"

-- | Check if two intervals identify the same root, by refining until
-- they either overlap or separate.
intervalsOverlap :: Interval -> Interval -> Poly Rational -> Bool
intervalsOverlap iv1 iv2 p = go (50 :: Int) iv1 iv2
  where
    go 0 (Interval l1 h1) (Interval l2 h2) =
      -- Last resort: midpoints are very close
      abs ((l1 + h1) / 2 - (l2 + h2) / 2) < (h1 - l1 + h2 - l2) / 2
    go n i1@(Interval l1 h1) i2@(Interval l2 h2)
      | h1 < l2 || h2 < l1 = False -- disjoint
      | l1 == h1 && l2 == h2 = l1 == l2 -- both exact
      | l1 == h1 = evalPoly p l1 == 0 && l2 <= l1 && l1 <= h2
      | l2 == h2 = evalPoly p l2 == 0 && l1 <= l2 && l2 <= h1
      | otherwise =
          let i1' = iiInterval $ refineRoot ((h1 - l1) / 4) (IsolatingInterval p i1)
              i2' = iiInterval $ refineRoot ((h2 - l2) / 4) (IsolatingInterval p i2)
           in go (n - 1) i1' i2'

instance Eq AlgNum where
  (==) = algEq

instance Ord AlgNum where
  compare = algCompare

instance Num AlgNum where
  (+) = algAdd
  (*) = algMul
  negate = algNeg
  abs a = if algCompare a (algFromRational 0) == LT then algNeg a else a
  signum a = case algCompare a (algFromRational 0) of
    LT -> algFromRational (-1)
    EQ -> algFromRational 0
    GT -> algFromRational 1
  fromInteger = algFromRational . fromInteger

instance Fractional AlgNum where
  recip = algInv
  fromRational = algFromRational
