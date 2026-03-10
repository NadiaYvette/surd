-- | Gauss period computation for expressing cos(2π/n) in radicals.
--
-- Every root of unity can be expressed in radicals, since cyclotomic
-- extensions have abelian (hence solvable) Galois groups. This is
-- NOT limited to compass-and-straightedge constructible cases — those
-- are the special case where only square roots are needed.
--
-- For non-constructible angles (e.g., cos(2π/7), cos(2π/9)), the
-- radical expressions may involve complex intermediate values
-- (the casus irreducibilis), but the final cos/sin values are real.
--
-- The algorithm descends through the subgroup chain of (Z/nZ)*,
-- solving a period equation at each step. Each step introduces
-- radicals of degree equal to the prime index of that step.
module Surd.Trig.Galois
  ( cosOfUnityViaGauss
  , gaussPeriods
  , primitiveRoot
  , subgroupChain
  , modExp
  ) where

import Data.Complex (Complex(..), magnitude, realPart, imagPart, mkPolar, phase)
import Data.List (nub, sort, sortBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Ord (comparing, Down(..))
import Surd.Types
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)
import Surd.Radical.DAG (toDAG, fromDAG, dagFoldConstants, dagEvalComplex)
import Surd.Radical.EvalMP (dftCoeffsMP, dagEvalComplexMP)
import Math.Internal.Interval (Interval(..), ComplexInterval(..))

-- | DAG-aware constant folding. Converts to explicit DAG (detecting thunk
-- sharing via StableName), folds constants in O(n) where n = unique nodes,
-- then reconstructs RadExpr preserving sharing.
-- Safe on exponentially-shared DAGs (unlike tree-walking foldConstants).
dagFold :: RadExpr Rational -> RadExpr Rational
dagFold = fromDAG . dagFoldConstants . toDAG

-- | DAG-aware evalComplex. Each unique node evaluated exactly once.
-- Avoids the need for StableName-based memoization in the evaluator.
dagEval :: RadExpr Rational -> Complex Double
dagEval = dagEvalComplex . toDAG

-- | Compute cos(2π/n) as a radical expression via Gauss period descent.
--
-- Works for any n where (Z/nZ)* is cyclic (primes, odd prime powers,
-- and twice odd prime powers). Uses the Gauss period descent: factor
-- φ(n) into primes, descend through the subgroup chain, and solve
-- the period equation at each step.
--
-- The resulting expression may involve complex intermediates
-- (e.g., √(-3) for cubic steps), but evaluates to a real number.
cosOfUnityViaGauss :: Int -> Maybe (RadExpr Rational)
cosOfUnityViaGauss n
  | n <= 2    = Nothing  -- handled elsewhere
  | otherwise =
      let n' = fromIntegral n :: Integer
      in case primitiveRootMod n' of
        Nothing -> Nothing  -- (Z/nZ)* is not cyclic; needs different approach
        Just g  ->
          let phi = eulerTotient n'
              -- Factor phi into primes (with multiplicity), giving the descent steps.
              -- For prime powers p^k, put the factor of p first to avoid
              -- degenerate sub-period sums. For primes, use natural order.
              fs = factorise (fromInteger phi :: Positive)
              steps = concatMap (\(q, e) -> replicate e (fromIntegral q :: Int))
                        (reorderFactors n' fs)
              -- Start: the single "period" containing all phi elements.
              -- Sum of all ζ^k for k coprime to n.
              -- For prime n: Σ_{k=1}^{n-1} ζ^k = -1
              -- For general n: Σ_{k coprime to n} ζ^k = μ(n) (Ramanujan sum)
              coprimeElems = [modExp g k n' | k <- [0..phi-1]]
              initSum = sum [cos (2 * pi * fromIntegral k / fromIntegral n') | k <- coprimeElems] :: Double
              initExpr = Lit (toRational (fromIntegral (round initSum :: Integer) :: Double))
              initPeriod = PeriodState
                { periodExpr  = initExpr
                , periodElems = coprimeElems
                , periodP     = n'
                }
              -- Descend through each prime step
              finalPeriods = foldl descendStep [initPeriod] steps
              -- Find the period containing the element 1 (since ζ^1 = e^{2πi/n})
              target = case filter (\ps -> 1 `elem` periodElems ps) finalPeriods of
                         (ps:_) -> periodExpr ps
                         []     -> error "cosOfUnityViaGauss: element 1 not found"
              -- ζ^{-1} = ζ^{n-1} for prime n, but for general n we need the
              -- element k such that k·1 ≡ -1 (mod n), i.e., k = n-1.
              -- But n-1 might not be coprime to n. Instead, find the element
              -- whose angle is the conjugate: for ζ^a, its conjugate is ζ^{n-a}.
              conjElem = n' - 1
              conjugate = case filter (\ps -> conjElem `elem` periodElems ps) finalPeriods of
                            (ps:_) -> periodExpr ps
                            []     ->
                              -- n-1 not in our coprime set (e.g., n=9, n-1=8, gcd(8,9)=1, so it should be)
                              -- If somehow not found, compute cos(2π/n) via exact real
                              Lit (toRational (cos (2 * pi / fromIntegral n') :: Double))
          in Just $ Mul (Inv (Lit 2)) (Add target conjugate)

-- | State of a Gauss period during the descent.
data PeriodState = PeriodState
  { periodExpr  :: !(RadExpr Rational)  -- ^ Radical expression for this period
  , periodElems :: ![Integer]           -- ^ Which exponents k (in ζ^k) this period sums
  , periodP     :: !Integer             -- ^ The prime p
  } deriving (Show)

-- | Perform one step of the descent, splitting each period into q sub-periods.
--
-- At a step of prime index q, each current period η splits into
-- q sub-periods η₀, ..., η_{q-1}. These satisfy a polynomial of
-- degree q whose coefficients are expressible in terms of the
-- current-level periods.
descendStep :: [PeriodState] -> Int -> [PeriodState]
descendStep periods q =
  concatMap (splitPeriod periods q) periods

-- | Split a single period into q sub-periods and solve the period equation.
splitPeriod :: [PeriodState]  -- ^ All periods at the current level
            -> Int             -- ^ Prime index q
            -> PeriodState     -- ^ The period to split
            -> [PeriodState]
splitPeriod allPeriods q parent =
  let p     = periodP parent
      elems = periodElems parent
      f     = length elems  -- current period size
      subF  = f `div` q     -- sub-period size

      -- Partition elems into q sub-periods.
      -- elems is ordered as g^0, g^q, g^{2q}, ..., g^{(f-1)·q} (relative to the coset).
      -- Sub-period k contains elements at indices k, k+q, k+2q, ...
      -- But actually, the elements are in the order determined by the
      -- primitive root descent. We partition by taking every q-th element.
      subPeriodElems = [ [elems !! (k + q * j) | j <- [0..subF-1]]
                       | k <- [0..q-1]
                       ]

      -- Compute the elementary symmetric functions of the sub-periods.
      -- These are expressible in terms of the current-level periods.
      -- For the period equation: t^q - e₁·t^{q-1} + e₂·t^{q-2} - ... = 0
      --
      -- e₁ = sum of sub-periods = parent period (known)
      -- e₂, e₃, ... = computable from the period multiplication table
      --
      -- For implementation: compute the symmetric functions numerically,
      -- then express them as linear combinations of current-level periods.
      -- Compute sub-period values as Complex Double (Gauss periods are complex)
      subPeriodValues = map (sumRootsOfUnity p) subPeriodElems

      -- Solve the period equation to get radical expressions for sub-periods.
      subPeriodExprs
        | q <= 3 =
            -- Quadratic/cubic: use elementary symmetric functions approach
            let symFuncs = elementarySymmetricC subPeriodValues
                coeffExprs = map (matchToPeriodExpr allPeriods p) symFuncs
            in solvePeriodEquation q (periodExpr parent) coeffExprs subPeriodValues
        | otherwise =
            -- q >= 5: use Lagrange resolvent approach (DFT-based)
            solvePeriodViaResolvent q allPeriods p (periodExpr parent) subPeriodElems subPeriodValues

  in [ PeriodState { periodExpr = expr, periodElems = elms, periodP = p }
     | (expr, elms) <- zip subPeriodExprs subPeriodElems
     ]

-- | Sum of ζ^k for a list of exponents k, where ζ = e^{2πi/n}.
sumRootsOfUnity :: Integer -> [Integer] -> Complex Double
sumRootsOfUnity n ks =
  sum [exp (0 :+ (2 * pi * fromIntegral k / fromIntegral n)) | k <- ks]


-- | Compute elementary symmetric polynomials from a list of complex values.
elementarySymmetricC :: [Complex Double] -> [Complex Double]
elementarySymmetricC xs = [elemSym k xs | k <- [1..length xs]]
  where
    elemSym 1 ys = sum ys
    elemSym k ys = sum [product combo | combo <- choose k ys]
    choose 0 _      = [[]]
    choose _ []     = []
    choose k' (y:ys) = map (y:) (choose (k'-1) ys) ++ choose k' ys

-- | Try to match a complex numerical value to a linear combination of period
-- expressions. Returns 'Just' the expression on success, 'Nothing' when the
-- matching fails (indicating that ExactComplex precision may be needed).
tryMatchToPeriodExpr :: [PeriodState] -> Integer -> Complex Double -> Maybe (RadExpr Rational)
tryMatchToPeriodExpr periods p target =
  let nearestInt = round (realPart target) :: Integer
      relTol err = err / max 1 (magnitude target) < 1e-6
  in if magnitude (target - (fromIntegral nearestInt :+ 0)) < 1e-8
     then Just (Lit (fromIntegral nearestInt))
     else
       let periodVals = map (sumRootsOfUnity p . periodElems) periods
           -- Try single-period matching first (fast: O(n) for n periods)
           singleMatches = mapMaybe (\(i, pv) ->
             case matchSinglePeriod target pv of
               Just (c, a) ->
                 let err = magnitude (target - (fromIntegral c :+ 0) - (fromIntegral a :+ 0) * pv)
                 in if relTol err
                    then Just (c, replicate i 0 ++ [a] ++ replicate (length periods - i - 1) 0, err)
                    else Nothing
               Nothing -> Nothing
             ) (zip [0..] periodVals)
           buildExpr (constant, coeffs, _err) =
             foldl Add (Lit (fromIntegral constant))
               [ if c == 0 then Lit 0
                 else if c == 1 then periodExpr pi'
                 else Mul (Lit (fromIntegral c)) (periodExpr pi')
               | (c, pi') <- zip coeffs periods
               ]
       -- Short-circuit: if single-period matching found something, use it.
       -- This avoids the expensive multi-period backtracking search.
       in case singleMatches of
            (m:ms) ->
              let best = foldl (\a b -> if maxCoeff a <= maxCoeff b then a else b) m ms
              in Just (buildExpr best)
            [] ->
              -- Fall back to multi-period matching (backtracking search)
              case findIntegerComboC target periodVals of
                Just (c, as) ->
                  let err = magnitude (target - (fromIntegral c :+ 0) -
                              sum (zipWith (\a pv -> (fromIntegral a :+ 0) * pv) as periodVals))
                  in if relTol err then Just (buildExpr (c, as, err)) else Nothing
                Nothing -> Nothing
  where
    maxCoeff (c, as, _) = maximum (abs c : map abs as)

-- | Match a complex numerical value to a linear combination of period expressions.
-- Falls back to rational approximation if matching fails.
matchToPeriodExpr :: [PeriodState] -> Integer -> Complex Double -> RadExpr Rational
matchToPeriodExpr periods p target =
  case tryMatchToPeriodExpr periods p target of
    Just expr -> expr
    Nothing ->
      let re = toRational (realPart target)
          im = toRational (imagPart target)
      in if abs (imagPart target) < 1e-12
         then Lit re
         else Add (Lit re) (Mul (Lit im) (Root 2 (Lit (-1))))

-- | Try to express target = c + a·v for integer c, a using a single period value.
-- Exactly determined: a from Im, c from Re.
matchSinglePeriod :: Complex Double -> Complex Double -> Maybe (Int, Int)
matchSinglePeriod target v =
  let a = if abs (imagPart v) > 1e-10
          then round (imagPart target / imagPart v) :: Int
          else if abs (realPart v) > 1e-10
               then round (realPart target / realPart v)
               else 0
      remainder = target - (fromIntegral a :+ 0) * v
      c = round (realPart remainder) :: Int
      recon = (fromIntegral c :+ 0) + (fromIntegral a :+ 0) * v
      err = magnitude (recon - target)
      relErr = err / max 1 (magnitude target)
  in if relErr < 1e-6
     then Just (c, a)
     else Nothing

-- | Try to express a complex target as c + Σ aᵢ·xᵢ for integer c, aᵢ.
--
-- Uses direct linear algebra: solve the system
--   Re(target) = c + Σ aᵢ · Re(xᵢ)
--   Im(target) = Σ aᵢ · Im(xᵢ)
-- For n period values, we have n+1 unknowns (c, a₁, ..., aₙ) and 2 equations.
-- For 1 period value, the system is exactly determined.
-- For 2+ periods, we use iterative solving (process imaginary part first).
findIntegerComboC :: Complex Double -> [Complex Double] -> Maybe (Int, [Int])
findIntegerComboC target vals =
  solveLinearIntegerC (toRational (realPart target), toRational (imagPart target))
                      [(toRational (realPart v), toRational (imagPart v)) | v <- vals]

-- | Solve target = c + Σ aᵢ·xᵢ for integer c, aᵢ using the complex structure.
--
-- Uses Rational arithmetic for full precision (1000-bit MPBall midpoints).
-- This is critical for large q where d_s ~ 10^8 and coefficients ~ 10^7,
-- exceeding Double's 15-digit precision.
--
-- Algorithm: greedy O(n log n):
-- 1. Sort periods by |Im(xᵢ)| descending (most distinctive first)
-- 2. Greedily assign each coefficient from Im(residual)/Im(xᵢ)
-- 3. Determine constant c from Re(residual)
-- 4. Verify, with local perturbation fallback if needed
solveLinearIntegerC :: (Rational, Rational) -> [(Rational, Rational)] -> Maybe (Int, [Int])
solveLinearIntegerC (tRe, tIm) [] =
  let c = round tRe :: Int
      errRe = tRe - fromIntegral c
      errIm = tIm
      errSq = errRe * errRe + errIm * errIm
      magSq = max 1 (tRe * tRe + tIm * tIm)
  in if errSq < magSq * 1e-12
     then Just (c, [])
     else Nothing
solveLinearIntegerC (tRe, tIm) vals =
  let n = length vals

      -- Pair each value with its original index
      indexed = zip [0 :: Int ..] vals

      -- Separate into imaginary-heavy and real-only periods
      (imHeavy, realOnly) = foldr (\iv@(_, (_, im)) (ih, ro) ->
        if abs im > 1e-30 then (iv : ih, ro) else (ih, iv : ro)) ([], []) indexed

      -- Sort imaginary-heavy by |Im| descending for numerical stability
      sortedIm = sortBy (comparing (Down . abs . snd . snd)) imHeavy

      -- Greedy assignment: process imaginary-heavy periods first
      ((resRe1, resIm1), coeffs1) = foldl (\((rRe, rIm), cs) (i, (vRe, vIm)) ->
        let a = round (rIm / vIm) :: Int
            aR = fromIntegral a
        in ((rRe - aR * vRe, rIm - aR * vIm), (i, a) : cs)
        ) ((tRe, tIm), []) sortedIm

      -- Then real-only periods
      ((resRe2, _resIm2), coeffs2) = foldl (\((rRe, rIm), cs) (i, (vRe, _vIm)) ->
        let a = if abs vRe > 1e-30
                then round (rRe / vRe) :: Int
                else 0
            aR = fromIntegral a
        in ((rRe - aR * vRe, rIm), (i, a) : cs)
        ) ((resRe1, resIm1), coeffs1) realOnly

      c = round resRe2 :: Int

      -- Build coefficient array in original order
      buildCoeffs cs = map snd $ sort cs
      coeffs = buildCoeffs coeffs2

      -- Verify using Rational arithmetic
      reconRe = fromIntegral c + sum (zipWith (\a (vRe, _) -> fromIntegral a * vRe) coeffs vals)
      reconIm = sum (zipWith (\a (_, vIm) -> fromIntegral a * vIm) coeffs vals)
      errSq = (reconRe - tRe)^(2 :: Int) + (reconIm - tIm)^(2 :: Int)
      magSq = max 1 (tRe * tRe + tIm * tIm)

      -- Reject decompositions with unreasonably large coefficients.
      -- For Gauss periods, d_s = c + Σ a_i η_i should have small integer
      -- coefficients. When |d_s| >> |η_i| (e.g., p=89 q=11: d_s ~ 10^8,
      -- η_i ~ 1-5), the system is underdetermined and greedy matching finds
      -- spurious solutions with huge coefficients (10^7+).
      -- Bound: max coefficient should be < 10000 * number of periods.
      coeffBound = 10000 * max 1 n
      coeffsOK = all (\a -> abs a <= coeffBound) coeffs && abs c <= coeffBound * coeffBound

  in if errSq < magSq * 1e-12 && coeffsOK
     then Just (c, coeffs)
     else -- Perturbation fallback: try ±1 on each coefficient
          tryPerturbation (tRe, tIm) vals magSq n coeffBound c coeffs

-- | Try perturbing each coefficient by ±1 to find a valid decomposition.
-- O(n) instead of O(3^n) — handles rounding ambiguity for at most one coefficient.
tryPerturbation :: (Rational, Rational) -> [(Rational, Rational)] -> Rational
               -> Int -> Int -> Int -> [Int] -> Maybe (Int, [Int])
tryPerturbation (tRe, tIm) vals magSq n coeffBound _c0 coeffs0 =
  let verify c cs =
        let reconRe = fromIntegral c + sum (zipWith (\a (vRe, _) -> fromIntegral a * vRe) cs vals)
            reconIm = sum (zipWith (\a (_, vIm) -> fromIntegral a * vIm) cs vals)
            errSq = (reconRe - tRe)^(2 :: Int) + (reconIm - tIm)^(2 :: Int)
            cOK = all (\a -> abs a <= coeffBound) cs && abs c <= coeffBound * coeffBound
        in errSq < magSq * 1e-12 && cOK

      -- For each coefficient position, try ±1 perturbation
      perturb i delta =
        let cs = [ if j == i then a + delta else a | (j, a) <- zip [0..] coeffs0 ]
            residRe = tRe - sum (zipWith (\a (vRe, _) -> fromIntegral a * vRe) cs vals)
            c = round residRe :: Int
        in if verify c cs then Just (c, cs) else Nothing

      attempts = [ perturb i d | i <- [0..n-1], d <- [-1, 1] ]

  in case mapMaybe id attempts of
       (x:_) -> Just x
       []    -> Nothing

-- | Solve a period equation of degree q to get radical expressions
-- for the sub-periods.
--
-- The period equation is: t^q - e₁·t^{q-1} + e₂·t^{q-2} - ... ± eₙ = 0
-- where eᵢ are the elementary symmetric functions (given as RadExpr).
solvePeriodEquation :: Int             -- ^ Degree q (prime)
                    -> RadExpr Rational -- ^ e₁ (sum of sub-periods = parent)
                    -> [RadExpr Rational] -- ^ [e₁, e₂, ..., eₙ] as radical exprs
                    -> [Complex Double]   -- ^ Numerical values of sub-periods (complex)
                    -> [RadExpr Rational]
solvePeriodEquation 2 _e1 coeffExprs numVals =
  -- Quadratic: t² - e₁·t + e₂ = 0
  -- t = (e₁ ± √(e₁² - 4e₂)) / 2
  let (e1, e2) = case coeffExprs of
        [a, b] -> (a, b)
        _      -> error "solvePeriodEquation: quadratic needs exactly 2 coefficients"
      disc = Add (Mul e1 e1) (Neg (Mul (Lit 4) e2))
      sqrtDisc = Root 2 disc
      root1 = Mul (Inv (Lit 2)) (Add e1 sqrtDisc)
      root2 = Mul (Inv (Lit 2)) (Add e1 (Neg sqrtDisc))
  in assignByValue (map dagFold [root1, root2]) numVals

solvePeriodEquation 3 _e1 coeffExprs numVals =
  -- Cubic: t³ - e₁·t² + e₂·t - e₃ = 0
  -- Use Cardano's formula.
  -- First depress: substitute t = u + e₁/3
  -- u³ + pu + q = 0 where
  --   p = e₂ - e₁²/3
  --   q = e₃ - e₁·e₂/3 + 2e₁³/27  ... wait, signs.
  -- Actually for t³ - s₁t² + s₂t - s₃ = 0, substituting t = u + s₁/3:
  -- u³ + (s₂ - s₁²/3)u + (s₃ ... ) = 0... let me just use the direct approach.
  --
  -- The period equation for a cyclic cubic is:
  --   t³ - e₁·t² + e₂·t - e₃ = 0
  -- Depressed: t = u + e₁/3, giving u³ + pu + q = 0 where
  --   p = e₂ - e₁²/3
  --   q = -e₃ + e₁·e₂/3 - 2·e₁³/27
  let (e1, e2, e3) = case coeffExprs of
        [a, b, c] -> (a, b, c)
        _         -> error "solvePeriodEquation: cubic needs exactly 3 coefficients"
      -- p = e₂ - e₁²/3
      pExpr = Add e2 (Neg (Mul (Inv (Lit 3)) (Mul e1 e1)))
      -- q = -e₃ + e₁·e₂/3 - 2·e₁³/27
      qExpr = Add (Add (Neg e3)
                       (Mul (Inv (Lit 3)) (Mul e1 e2)))
                  (Neg (Mul (Lit (2/27)) (Mul e1 (Mul e1 e1))))
      -- Cardano: u₁ = ∛(-q/2 + √(q²/4 + p³/27)), u₂ = -p/(3u₁)
      -- The constraint u₁·u₂ = -p/3 determines u₂ once u₁ is chosen.
      -- The discriminant term: Δ = q²/4 + p³/27
      delta = Add (Mul (Inv (Lit 4)) (Mul qExpr qExpr))
                  (Mul (Inv (Lit 27)) (Mul pExpr (Mul pExpr pExpr)))
      sqrtDelta = Root 2 delta
      negQHalf = Mul (Inv (Lit (-2))) qExpr
      u1Arg = Add negQHalf sqrtDelta
      u1 = Root 3 u1Arg
      -- u₂ is NOT an independent cube root — it's determined by u₁·u₂ = -p/3
      u2 = Mul (Neg pExpr) (Inv (Mul (Lit 3) u1))
      -- t = u + e₁/3
      shift = Mul (Inv (Lit 3)) e1
      -- For the three roots, we need ω = e^{2πi/3} = (-1+√(-3))/2
      -- t_k = ω^k · u₁ + ω^{-k} · u₂ + e₁/3
      omega = Mul (Inv (Lit 2)) (Add (Lit (-1)) (Root 2 (Lit (-3))))
      omegaBar = Mul (Inv (Lit 2)) (Add (Lit (-1)) (Neg (Root 2 (Lit (-3)))))
      root0 = Add (Add u1 u2) shift
      root1 = Add (Add (Mul omega u1) (Mul omegaBar u2)) shift
      root2 = Add (Add (Mul omegaBar u1) (Mul omega u2)) shift
  in assignByValue (map dagFold [root0, root1, root2]) numVals

solvePeriodEquation _q _e1 _coeffExprs numVals =
  -- Fallback for any q not handled above (shouldn't be reached
  -- since splitPeriod dispatches q >= 5 to solvePeriodViaResolvent)
  map (\v -> Lit (toRational (realPart v))) numVals

-- | Solve the period equation of prime degree q ≥ 5 via Lagrange resolvents.
--
-- For a cyclic extension of prime degree q, the Lagrange resolvent is:
--   R_j = Σ_{k=0}^{q-1} ω^{jk} · η_k
-- where ω = e^{2πi/q} is a primitive q-th root of unity.
--
-- Key property: η_k = (1/q) Σ_{j=0}^{q-1} ω^{-jk} · R_j  (DFT inversion)
--
-- R_j^q lives in K(ω_q) where K is the period field. We decompose it as:
--   R_j^q = Σ_{s=0}^{q-1} d_s · ω^{js}
-- where d_s = (1/q) Σ_j ω^{-js} R_j^q (inverse DFT of {R_j^q}).
-- By Galois theory, each d_s ∈ K (the period field), so it can be
-- matched to an integer linear combination of period expressions.
solvePeriodViaResolvent :: Int             -- ^ Degree q (prime, ≥ 5)
                        -> [PeriodState]   -- ^ All current-level periods
                        -> Integer         -- ^ p (the prime)
                        -> RadExpr Rational -- ^ Parent period expression (= R_0)
                        -> [[Integer]]     -- ^ Sub-period element lists
                        -> [Complex Double] -- ^ Numerical sub-period values
                        -> [RadExpr Rational]
solvePeriodViaResolvent q allPeriods p parentExpr subPeriodElems _numVals =
  let -- Compute high-precision DFT via MPBall (1000-bit precision).
      -- Always used for q ≥ 5 because Double precision is insufficient for
      -- large d_s values (e.g., q=11: d_s ~ 10^6, Double error ~ 1e-8,
      -- which can cause round() to give wrong integer coefficients even
      -- though the reconstruction passes the 1e-6 tolerance check).
      -- Compute DFT and period values using the same 1000-bit ζ powers.
      -- Computing period values independently causes rounding mismatch with
      -- dCoeffsHP, leading to wrong integer coefficients in solveLinearIntegerC.
      (dCoeffsHP, resolventValsHP, _resolventPowersHP, periodValsD) =
        dftCoeffsMP q p subPeriodElems (map periodElems allPeriods)

      -- Match d_s to integer linear combinations of period values.
      dExprs = [ case solveLinearIntegerC (dCoeffsHP !! s) periodValsD of
                   Just (c, coeffs) ->
                     foldl Add (Lit (fromIntegral c))
                       [ if a == 0 then Lit 0
                         else if a == 1 then periodExpr pi'
                         else Mul (Lit (fromIntegral a)) (periodExpr pi')
                       | (a, pi') <- zip coeffs allPeriods
                       ]
                   Nothing ->
                     -- MPBall matching failed. Use literal approximation.
                     let (re, im) = dCoeffsHP !! s
                     in if abs im < 1e-30
                        then Lit re
                        else Add (Lit re) (Mul (Lit im) (Root 2 (Lit (-1))))
               | s <- [0..q-1] ]

      -- Get cos(2π/q) as a RadExpr
      cosBaseExpr = case cosOfUnityViaGauss q of
        Just e  -> e
        Nothing -> error $ "solvePeriodViaResolvent: can't compute cos(2π/" ++ show q ++ ")"

      -- Build ω^m as RadExpr for all needed m = 0, ..., q-1.
      -- Uses Chebyshev polynomials: O(m) depth per power but pure tree structure
      -- avoids the exponential blowup from iterative cos/sin addition.
      omegaPowers = map (omegaPowerExpr q cosBaseExpr) [0..q-1]

      -- R_j^q = Σ_s d_s · ω^{js} as RadExpr
      resolventPowerExprs =
        [ foldl1 Add [ Mul (dExprs !! s) (omegaPowers !! ((j * s) `mod` q))
                      | s <- [0..q-1] ]
        | j <- [1..q-1] ]

      -- Select correct branch of q-th root for each resolvent.
      -- Uses high-precision resolvent values from MPBall DFT.
      resolventExprs = [ selectResolventBranchFast q omegaPowers
                           (resolventPowerExprs !! (j-1))
                           (resolventValsHP !! j)
                       | j <- [1..q-1] ]

      -- All resolvents: R_0 = parent, R_1, ..., R_{q-1}
      allResolvents = parentExpr : resolventExprs

      -- Recover sub-periods: η_k = (1/q) Σ_{j=0}^{q-1} ω^{-jk} · R_j
      -- Skip dagFold here; the output goes through safeDenestAndNormalize.
      subPeriodExprs =
        [ Mul (Inv (Lit (fromIntegral q)))
              (foldl1 Add [ Mul (omegaPowers !! ((q - ((j * k) `mod` q)) `mod` q))
                                (allResolvents !! j)
                          | j <- [0..q-1] ])
        | k <- [0..q-1] ]

      -- Sub-period expressions are already in correct order by DFT construction:
      -- η_k = (1/q) Σ ω^{-jk} R_j directly produces the k-th sub-period.
      -- No numerical matching needed (and evalComplex on deep trees is imprecise).
  in subPeriodExprs

-- | Express ω^m = e^{2πim/q} as a RadExpr, given cos(2π/q).
--
-- ω^m = cos(2πm/q) + i·sin(2πm/q)
-- where cos(2πm/q) = T_m(cos(2π/q))  (Chebyshev polynomial)
-- and   sin(2πm/q) = ±√(1 - cos²(2πm/q))
-- with sign positive when 0 < 2πm/q < π (i.e., 2m < q).
omegaPowerExpr :: Int -> RadExpr Rational -> Int -> RadExpr Rational
omegaPowerExpr _ _ 0 = Lit 1
omegaPowerExpr q cosBase m =
  let m' = m `mod` q
  in if m' == 0 then Lit 1
     else
       let cosM = chebyshevLocal m' cosBase
           sin2M = Add (Lit 1) (Neg (Mul cosM cosM))
           sinM = Root 2 sin2M
           -- sin(2πm'/q) > 0 when 2m' < q (angle in first half of circle)
           sinMSigned = if 2 * m' < q then sinM else Neg sinM
           i = Root 2 (Lit (-1))
       in Add cosM (Mul i sinMSigned)

-- | Chebyshev polynomial T_k(x): T_0(x) = 1, T_1(x) = x,
-- T_{n+1}(x) = 2x·T_n(x) - T_{n-1}(x).
-- Local copy to avoid circular import with Surd.Trig.
chebyshevLocal :: Int -> RadExpr Rational -> RadExpr Rational
chebyshevLocal 0 _ = Lit 1
chebyshevLocal 1 x = x
chebyshevLocal k x = go 2 (Lit 1) x
  where
    go n t0 t1
      | n > k     = t1
      | otherwise  =
          let t2 = Add (Mul (Mul (Lit 2) x) t1) (Neg t0)
          in go (n + 1) t1 t2



-- | Select the correct branch of the q-th root.
--
-- Uses Double evaluation for branch selection (matching the evaluator),
-- with MPBall fallback when the Double result is ambiguous (near-zero
-- R_j^q values where the phase is dominated by rounding errors).
selectResolventBranchFast :: Int
                          -> [RadExpr Rational]   -- ^ ω^k for k = 0, ..., q-1
                          -> RadExpr Rational     -- ^ R_j^q expression
                          -> Complex Double       -- ^ Target numerical value of R_j
                          -> RadExpr Rational
selectResolventBranchFast q omegaPowers rjqExpr targetVal =
  let principalRoot = Root q rjqExpr
      -- Try Double evaluation first.
      rjqExprVal = dagEval rjqExpr
      (bestK, confidence) = selectBranch rjqExprVal
  in if confidence > 0.5
     then
       -- Double is ambiguous (best ≈ second). Use MPBall for accurate phase.
       let rjqExprHP = ciToComplex (dagEvalComplexMP 500 (toDAG rjqExpr))
           (bestKHP, _) = selectBranch rjqExprHP
       in if bestKHP == 0 then principalRoot
          else Mul (omegaPowers !! bestKHP) principalRoot
     else
       if bestK == 0 then principalRoot
       else Mul (omegaPowers !! bestK) principalRoot
  where
    selectBranch rjqVal =
      let omegaC = exp (0 :+ (2 * pi / fromIntegral q))
          principalVal = mkPolar (magnitude rjqVal ** (1 / fromIntegral q))
                                 (phase rjqVal / fromIntegral q)
          scored = [ (k, magnitude (omegaC ^ k * principalVal - targetVal))
                   | k <- [0..q-1] ]
          sorted = NE.sortBy (comparing snd) (NE.fromList scored)
          best = snd (NE.head sorted)
          second = snd (sorted NE.!! 1)
      in (fst (NE.head sorted), best / max 1e-20 second)

-- | Convert ComplexInterval midpoints to Complex Double.
ciToComplex :: ComplexInterval -> Complex Double
ciToComplex ci =
  let midR iv = fromRational ((lo iv + hi iv) / 2) :: Double
  in midR (ciReal ci) :+ midR (ciImag ci)


-- | Assign radical expressions to numerical values by matching.
-- Each expression is evaluated numerically (as ExactComplex) and
-- paired with the closest target value using complex magnitude.
assignByValue :: [RadExpr Rational] -> [Complex Double] -> [RadExpr Rational]
assignByValue exprs vals =
  let exprVals = [(e, dagEval e) | e <- exprs]
  in map (pickClosest exprVals) vals
  where
    pickClosest evs target =
      case evs of
        []     -> error "assignByValue: empty expression list"
        (e:es) -> fst $ NE.head $ NE.sortBy (comparing (\(_, v) -> magnitude (v - target))) (e :| es)

-- | Reorder prime factors of φ(n) for the Gauss period descent.
--
-- For prime n, keep the natural (ascending) order.
-- For prime powers p^k, put the factor p first. This avoids degenerate
-- sub-period sums where equally-spaced roots cancel to zero.
reorderFactors :: Integer -> [(Integer, Int)] -> [(Integer, Int)]
reorderFactors n fs =
  let nfs = factorise (fromInteger n :: Positive)
  in case nfs of
    [(p, k)] | k > 1 ->
      -- n = p^k. φ(n) = p^{k-1}(p-1). Put p factors first.
      let pFactor = [(p, e) | (q, e) <- fs, q == p]
          rest    = [(q, e) | (q, e) <- fs, q /= p]
      in pFactor ++ rest
    _ -> fs  -- primes, 2*p^k, etc.: keep natural order

-- | Find a primitive root modulo a prime p.
-- A primitive root g has order p-1 in (Z/pZ)*.
primitiveRoot :: Integer -> Maybe Integer
primitiveRoot p
  | p <= 1    = Nothing
  | p == 2    = Just 1
  | otherwise =
      let phi = p - 1
          factors = map fst (factorise (fromInteger phi :: Positive))
          isPrimRoot g = all (\q -> modExp g (phi `div` q) p /= 1) factors
      in case filter isPrimRoot [2..p-1] of
        []    -> Nothing
        (g:_) -> Just g

-- | Find a primitive root modulo n, if one exists.
-- Primitive roots exist for n = 1, 2, 4, p^k, 2p^k (odd prime p).
primitiveRootMod :: Integer -> Maybe Integer
primitiveRootMod n
  | n <= 0    = Nothing
  | n <= 2    = Just 1
  | n == 4    = Just 3
  | otherwise =
      let fs = factorise (fromInteger n :: Positive)
      in case fs of
        [(p, _)]       | p > 2 -> findPrimRootMod n
        [(2, 1), (p, _)] | p > 2 -> findPrimRootMod n
        _              -> Nothing  -- no primitive root exists

-- | Find a primitive root mod n by brute force (n has primitive roots).
findPrimRootMod :: Integer -> Maybe Integer
findPrimRootMod n =
  let phi = eulerTotient n
      factors = map fst (factorise (fromInteger phi :: Positive))
      isPrimRoot g = gcd g n == 1
                  && all (\q -> modExp g (phi `div` q) n /= 1) factors
  in case filter isPrimRoot [2..n-1] of
    []    -> Nothing
    (g:_) -> Just g

-- | Euler's totient function.
eulerTotient :: Integer -> Integer
eulerTotient n
  | n <= 0    = error "eulerTotient: non-positive"
  | n == 1    = 1
  | otherwise =
      let fs = factorise (fromInteger n :: Positive)
      in product [(p - 1) * p ^ (e - 1) | (p, e) <- fs]

-- | Modular exponentiation: base^exp mod m.
modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp b e m
  | even e    = let half = modExp b (e `div` 2) m in (half * half) `mod` m
  | otherwise = (b * modExp b (e - 1) m) `mod` m

-- | Compute the subgroup chain of (Z/pZ)* for descending
-- through Gauss periods.
--
-- Given p-1 = q₁^a₁ · q₂^a₂ · ... , the chain of subgroups is:
-- (Z/pZ)* ⊃ H₁ ⊃ H₂ ⊃ ... ⊃ {1}
-- where each step has prime index.
subgroupChain :: Integer -> [[Integer]]
subgroupChain p =
  let phi = p - 1
      fs = factorise (fromInteger phi :: Positive)
      g = case primitiveRoot p of
            Just g' -> g'
            Nothing -> error "subgroupChain: no primitive root"
      primeFactorList = concatMap (\(q, e) -> replicate e q) fs
  in buildChain g p phi primeFactorList

buildChain :: Integer -> Integer -> Integer -> [Integer] -> [[Integer]]
buildChain _ _ _ [] = [[1]]
buildChain g p phi (q:qs) =
  let subOrder = phi `div` q
      subGen = modExp g q p
      subGroup = nub $ sort [modExp subGen k p | k <- [0..subOrder-1]]
      rest = buildChain (modExp g q p) p subOrder qs
  in subGroup : rest

-- | Compute Gauss periods for a prime p with e periods of f elements each,
-- where p-1 = e*f.
--
-- The kth Gauss period is η_k = Σ ζ^(g^(ej+k)) for j = 0..f-1,
-- where g is a primitive root mod p and ζ = e^(2πi/p).
--
-- Returns the periods as lists of exponents (the actual evaluation
-- into radical expressions happens via the descent).
gaussPeriods :: Integer -> Int -> [[Integer]]
gaussPeriods p e =
  let phi = p - 1
      f = fromIntegral phi `div` e
      g = case primitiveRoot p of
            Just g' -> g'
            Nothing -> error "gaussPeriods: no primitive root"
  in [ [modExp g (fromIntegral (e * j + k)) p | j <- [0..f-1]]
     | k <- [0..e-1]
     ]
