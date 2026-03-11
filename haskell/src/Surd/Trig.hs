-- | Exact symbolic evaluation of trigonometric functions at
-- rational multiples of π.
--
-- Every trig value at a rational multiple of π can be expressed in
-- radicals, since cyclotomic extensions have abelian (hence solvable)
-- Galois groups. The radical expressions may involve complex
-- intermediates (the casus irreducibilis) but the final cos/sin
-- values are always real.
--
-- The primary entry points are 'cosExact' and 'sinExact', which
-- compute cos(pπ/q) and sin(pπ/q) as radical expressions.
-- A 'MinPoly' fallback is provided for cases not yet handled
-- by the radical machinery.
module Surd.Trig
  ( cosExact
  , sinExact
  , tanExact
  , cosMinPoly
  , TrigResult(..)
  , simplifyTrigResult
  , simplifiedSin
  ) where

import Control.Exception (SomeException, evaluate, try)
import System.Timeout (timeout)
import Surd.Radical.Expr (collectRadicals)
import Surd.Types
import Data.Map.Strict qualified as Map
import Surd.Trig.RootOfUnity (cosOfUnity)
import Surd.Trig.Galois (allPeriodsViaGauss)
import Math.Polynomial.Univariate (Poly)
import Math.Polynomial.Cyclotomic (cyclotomic)
import Surd.Radical.Normalize (normalize)
import Surd.Radical.NormalForm (toNormExpr, fromNormExpr)
import Surd.Radical.Denest (denest)
import Surd.Radical.Eval (evalInterval)
import Surd.Algebraic.Convert (simplifyViaCanonical)
import Surd.Radical.DAG (toDAG, fromDAG, dagSize, dagDepth, dagFoldConstants)
import Surd.Radical.EvalMP (dagEvalComplexMP)
import Math.Internal.Interval (Interval, ComplexInterval(..), width, overlaps)
import System.IO.Unsafe (unsafePerformIO)

-- | Result of exact trig evaluation.
data TrigResult
  = Radical (RadExpr Rational)
    -- ^ Exact radical expression (may involve complex intermediates
    -- like @√(-3)@ for non-constructible angles, but the final
    -- value is real)
  | MinPoly (Poly Rational)
    -- ^ Minimal polynomial (fallback when radical computation is
    -- not yet implemented for this case)
  deriving (Show)

-- | Compute cos(pπ/q) exactly.
--
-- Reduces to cos(2π/n) via standard identities, then uses the
-- root-of-unity machinery (Gauss period descent).
--
-- >>> cosExact 1 3   -- cos(π/3) = 1/2
-- Radical (Lit (1 % 2))
-- >>> cosExact 1 4   -- cos(π/4) = √2/2
-- Radical (Mul (Inv (Lit (2 % 1))) (Root 2 (Lit (2 % 1))))
cosExact :: Integer -> Integer -> TrigResult
cosExact p q
  | q <= 0    = error "cosExact: non-positive denominator"
  | otherwise =
      let g = gcd (abs p) q
          p' = p `div` g
          q' = q `div` g
      in cosReduced p' q'

-- | Compute sin(pπ/q) exactly.
sinExact :: Integer -> Integer -> TrigResult
sinExact p q
  | q <= 0    = error "sinExact: non-positive denominator"
  | otherwise =
      let g = gcd (abs p) q
          p' = p `div` g
          q' = q `div` g
      in sinReduced p' q'

-- | Compute tan(pπ/q) exactly, as sin/cos.
tanExact :: Integer -> Integer -> Maybe TrigResult
tanExact p q =
  case (sinExact p q, cosExact p q) of
    (Radical s, Radical c) ->
      Just $ Radical $ Mul s (Inv c)
    _ -> Nothing

cosReduced :: Integer -> Integer -> TrigResult
cosReduced p q =
  let p' = p `mod` (2 * q)
      p'' = if p' < 0 then p' + 2 * q else p'
  in cosInRange p'' q

sinReduced :: Integer -> Integer -> TrigResult
sinReduced p q =
  let p' = p `mod` (2 * q)
      p'' = if p' < 0 then p' + 2 * q else p'
      -- p''/q is in [0, 2) so p''π/q is in [0, 2π)
      positive = p'' >= 0 && p'' <= q  -- sin ≥ 0 for angle in [0, π]
  in if p'' == 0 || p'' == q then Radical (Lit 0)  -- sin(0) = sin(π) = 0
     else
       -- Try direct period lookup: sin(2πk/n) = (ζ^k − ζ^{n-k})/(2i)
       let g = gcd p'' (2 * q)
           n = fromIntegral (2 * q `div` g) :: Int
           k = fromIntegral (p'' `div` g) :: Int
       in case directPeriodSin n k of
         Just sinExpr ->
           -- NF already simplified; just fold constants (denest hangs on
           -- complex intermediates from casus irreducibilis cube roots).
           Radical (fromDAG (dagFoldConstants (toDAG sinExpr)))
         Nothing ->
           -- Fall back to ±√(1 - cos²)
           case cosExact p q of
             Radical c ->
               let sin2 = fromNormExpr (toNormExpr (Add (Lit 1) (Neg (Mul c c))))
                   sin2folded = fromDAG (dagFoldConstants (toDAG sin2))
                   sinExpr = Root 2 sin2folded
                   signed = if positive then sinExpr else Neg sinExpr
               in Radical (safeDenestAndNormalize signed)
             minpoly -> minpoly

-- | Compute sin(2πk/n) directly from Gauss period expressions.
--
-- Pre-simplifies individual period expressions through NormalForm,
-- then forms sin = (-i/2)(ζ^k − ζ^{n-k}) and runs NF to cancel
-- the imaginary unit. Returns Nothing if periods are unavailable
-- or have too many radicals for NF.
directPeriodSin :: Int -> Int -> Maybe (RadExpr Rational)
directPeriodSin n k = do
  periods <- allPeriodsViaGauss n
  pk  <- Map.lookup k periods
  pnk <- Map.lookup (n - k) periods
  -- Pre-simplify individual periods through NF to reduce expression size.
  -- This makes the subsequent NF on the sin formula tractable.
  let nRadsPk = length (collectRadicals pk)
      nRadsPnk = length (collectRadicals pnk)
  -- Guard: skip if periods have too many radicals for NF (denom ≥ 11)
  if nRadsPk > 10 || nRadsPnk > 10
    then Nothing
    else
      let pk'  = fromNormExpr (toNormExpr pk)
          pnk' = fromNormExpr (toNormExpr pnk)
          -- sin(2πk/n) = (ζ^k − ζ^{n-k}) / (2i) = (-i/2)(ζ^k − ζ^{n-k})
          i = Root 2 (Lit (-1))
          sinForm = Mul (Mul (Inv (Lit 2)) (Neg i)) (Add pk' (Neg pnk'))
          -- NF round-trip cancels i factors (i·√(-x) → -√x via isRadicandNegative)
          sinNF = fromNormExpr (toNormExpr sinForm)
      in Just sinNF

-- | cos(pπ/q) where 0 ≤ p ≤ 2q (i.e., angle in [0, 2π]).
cosInRange :: Integer -> Integer -> TrigResult
cosInRange p q
  | p == 0         = Radical (Lit 1)
  | 2 * p == q     = Radical (Lit 0)
  | p == q         = Radical (Lit (-1))
  | 2 * p == 3 * q = Radical (Lit 0)
  | 2 * p > q && p < q =
      case cosInRange (q - p) q of
        Radical e -> Radical (Neg e)
        other     -> other
  | p > q && 2 * p < 3 * q =
      case cosInRange (p - q) q of
        Radical e -> Radical (Neg e)
        other     -> other
  | 2 * p >= 3 * q =
      cosInRange (2 * q - p) q
  | otherwise = cosFirstQuadrant p q

cosFirstQuadrant :: Integer -> Integer -> TrigResult
cosFirstQuadrant p q =
  let g = gcd p (2 * q)
      n = fromIntegral (2 * q `div` g) :: Int
      k = fromIntegral (p `div` g) :: Int
  in if k == 1
     then
       case cosOfUnity n of
         Just e  -> Radical (safeDenestAndNormalize e)
         Nothing -> MinPoly (cyclotomic n)
     else
       case cosOfUnity n of
         Just base ->
           let cheb = chebyshev k base
               nRads = length (collectRadicals cheb)
           in if nRads > 3
              then
                -- Complex base (Cardano etc.): NormalForm would expand
                -- (sum)*(sum) products into thousands of monomials. Return
                -- the compact Chebyshev tree; simplifyTrigResult can handle it.
                Radical (safeDenestAndNormalize cheb)
              else
                -- Simple base: NormalForm distributes products cleanly
                let simplified = fromNormExpr (toNormExpr cheb)
                in Radical (safeDenestAndNormalize simplified)
         Nothing -> MinPoly (cyclotomic n)

-- | Try denesting followed by normalization.
-- Only attempts denesting on small expressions where Landau's algorithm
-- has a chance of simplifying. Falls back to normalization-only for
-- medium expressions, and skips entirely for large expression DAGs.
--
-- Three tiers:
-- - Small (d ≤ 20, s ≤ 500): dagFoldConstants → denest → tree normalize
-- - Medium (d ≤ 50, s ≤ 5000): dagFoldConstants only
-- - Large (d > 50 or s > 5000): dagFoldConstants only
safeDenestAndNormalize :: RadExpr Rational -> RadExpr Rational
safeDenestAndNormalize e =
  let dag = toDAG e
      d   = dagDepth dag
      s   = dagSize dag
  in if d > 50 || s > 5000
     then fromDAG (dagFoldConstants dag)
     else if d <= 20 && s <= 500
     then
       -- Small: try denesting, then tree normalize with verification
       let folded = fromDAG (dagFoldConstants dag)
           denested = denest folded
           sDen = dagSize (toDAG denested)
           best = if sDen < s then denested else folded
       in safeNormalize best
     else
       -- Medium: DAG simplification (constant folding, power simplification,
       -- perfect power extraction). dagNormalize is available but not used
       -- here because Gauss period output rarely has like terms to collect.
       fromDAG (dagFoldConstants dag)

-- | Try simplifyViaCanonical if the expression is moderately large.
-- Returns the simpler of the original and simplified forms.
-- Skips expressions that are too small (already fine) or too large
-- (canonical simplification would be prohibitively expensive).
tryCanonicalSimplify :: RadExpr Rational -> RadExpr Rational
tryCanonicalSimplify e =
  let s = dagSize (toDAG e)
      nRads = length (collectRadicals e)
  in if s <= 30 || s > 200 || nRads > 5
     then e
     else unsafePerformIO $ do
       -- 5 second timeout: minimalPolyTower can hang on complex towers
       result <- timeout 5000000 $ tryAny (evaluate (simplifyViaCanonical e))
       case result of
         Nothing -> return e  -- timed out
         Just (Left _) -> return e  -- exception
         Just (Right simplified) ->
           let s' = dagSize (toDAG simplified)
           in return (if s' < s then simplified else e)

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

-- | Simplify a trig result for display. Applies canonical simplification
-- (minimal polynomial → Cardano/Ferrari) to reduce complex Gauss period
-- or Chebyshev expressions to compact radical forms. This is separate from
-- 'cosExact'/'sinExact' because the simplified form, while more readable,
-- can be harder for downstream algebraic operations (e.g., minimalPolyTower).
simplifyTrigResult :: TrigResult -> TrigResult
simplifyTrigResult (Radical e) =
  -- First try canonical simplification (minimal polynomial → Cardano/Ferrari),
  -- then NF round-trip to canonicalize radicands (integral form, coprime coefficients).
  -- Skip NF round-trip for expressions with many radicals (>5) to avoid
  -- exponential blowup in toNormExpr (16 radicals for denom 11, etc.).
  let simplified = tryCanonicalSimplify e
      nRads = length (collectRadicals simplified)
  in if nRads > 5
     then Radical simplified
     else Radical (fromNormExpr (toNormExpr simplified))
simplifyTrigResult other = other

-- | Compute simplified sin from simplified cos for display.
-- sin(x) = ±√(1 - cos²(x)), using the already-simplified cos value.
simplifiedSin :: Integer -> Integer -> TrigResult -> TrigResult
simplifiedSin p q (Radical c) =
  let p' = (p `mod` (2 * q))
      p'' = if p' < 0 then p' + 2 * q else p'
      positive = p'' >= 0 && p'' <= q
      rads = collectRadicals c
      hasComplex = any (\(_, r) -> r == Lit (-1)) rads
  in if p'' == 0 || p'' == q then Radical (Lit 0)
     -- For complex cos forms (containing i), NF round-trip of 1-cos² explodes
     -- because squaring complex monomials creates exponentially many terms.
     -- Skip NF and use basic DAG fold instead.
     else if hasComplex
     then let oneMinusCos2 = Add (Lit 1) (Neg (Mul c c))
              folded = fromDAG (dagFoldConstants (toDAG oneMinusCos2))
              sinExpr = Root 2 folded
              signed = if positive then sinExpr else Neg sinExpr
          in Radical (fromDAG (dagFoldConstants (toDAG signed)))
     else let sin2 = fromNormExpr (toNormExpr (Add (Lit 1) (Neg (Mul c c))))
              sin2folded = fromDAG (dagFoldConstants (toDAG sin2))
              sinExpr = Root 2 sin2folded
              signed = if positive then sinExpr else Neg sinExpr
              -- NF round-trip canonicalizes NestedRoot radicands
              -- (clears denominators, extracts content, coprime integer coefficients)
              canonical = fromNormExpr (toNormExpr signed)
              s = dagSize (toDAG canonical)
              result
                | s <= 30   = safeNormalize canonical
                | otherwise = tryCanonicalSimplify canonical
          in Radical result
simplifiedSin _ _ other = other

-- | Normalize, but verify the result. If normalization changes the
-- numerical value (can happen with complex intermediates from Gauss
-- period descent), return the original expression.
--
-- Uses interval arithmetic for rigorous verification when possible.
-- Falls back to MPBall evaluation (500-bit precision) when rational
-- interval eval fails (complex intermediates like √(-3)).
safeNormalize :: RadExpr Rational -> RadExpr Rational
safeNormalize e
  | let dag = toDAG e
        d   = dagDepth dag
        s   = dagSize dag
    in d > 30 || s > 500 = e
  | otherwise =
      let normed = normalize e
      in case tryEvalInterval e of
           Nothing ->
             -- Interval evaluation failed (complex intermediates).
             -- Fall back to MPBall verification at 500-bit precision.
             let origCI = dagEvalComplexMP 500 (toDAG e)
                 normCI = dagEvalComplexMP 500 (toDAG normed)
                 origRe = ciReal origCI
                 normRe = ciReal normCI
             in if overlaps origRe normRe
                then normed
                else e
           Just origIv ->
             case tryEvalInterval normed of
               Nothing -> e
               Just normIv ->
                 if overlaps origIv normIv && width origIv >= width normIv
                 then normed
                 else e

-- | Try to evaluate an expression via interval arithmetic, returning
-- Nothing if it fails (e.g., due to negative radicands from complex
-- intermediates in Gauss period descent).
tryEvalInterval :: RadExpr Rational -> Maybe Interval
tryEvalInterval e = unsafePerformIO $ do
  result <- try (evaluate (evalInterval e))
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right iv -> return (Just iv)

-- | Chebyshev polynomial evaluation: T_k(x) computed symbolically.
-- T_0(x) = 1, T_1(x) = x, T_{n+1}(x) = 2x·T_n(x) - T_{n-1}(x)
chebyshev :: Int -> RadExpr Rational -> RadExpr Rational
chebyshev 0 _ = Lit 1
chebyshev 1 x = x
chebyshev k x = go 2 (Lit 1) x
  where
    go n t0 t1
      | n > k     = t1
      | otherwise  =
          let t2 = Add (Mul (Mul (Lit 2) x) t1) (Neg t0)
          in go (n + 1) t1 t2

-- | Return the minimal polynomial of cos(2π/n).
--
-- Note: this returns the cyclotomic polynomial Φₙ(x), which is an
-- annihilating polynomial but may not be the true minimal polynomial
-- of cos(2π/n) (which has degree φ(n)/2 for the real part).
cosMinPoly :: Int -> Poly Rational
cosMinPoly = cyclotomic
