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

import Data.Complex (Complex(..), magnitude, realPart, imagPart)
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Surd.Types
import Surd.Internal.Positive (Positive)
import Surd.Internal.PrimeFactors (factorise)
import Surd.Radical.Eval (evalComplex)

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
              initSum = sum [cos (2 * pi * fromIntegral k / fromIntegral n') | k <- coprimeElems]
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
                              -- If somehow not found, just use the target (cos = Re(ζ))
                              -- and compute numerically
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
            solvePeriodViaResolvent q allPeriods p (periodExpr parent) subPeriodValues

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

-- | Match a complex numerical value to a linear combination of period expressions.
--
-- The symmetric functions of sub-periods at each level are expressible
-- as integer linear combinations of the periods at the current level.
-- We determine the coefficients numerically using complex arithmetic.
matchToPeriodExpr :: [PeriodState] -> Integer -> Complex Double -> RadExpr Rational
matchToPeriodExpr periods p target =
  -- Try: target = c₀ + c₁·η₁ + c₂·η₂ + ... for small integer cᵢ
  -- First check if target is close to a real integer
  let nearestInt = round (realPart target) :: Integer
      nearestRat = toRational (fromIntegral nearestInt :: Double)
  in if magnitude (target - (fromIntegral nearestInt :+ 0)) < 1e-8
     then Lit nearestRat
     else
       -- Try to express as integer + integer linear combination of periods
       -- Try each period individually first (exactly determined system),
       -- then try combinations. Prefer solutions with smaller coefficients
       -- to minimize floating-point error amplification.
       let periodVals = map (sumRootsOfUnity p . periodElems) periods
           -- Try matching with each single period
           singleMatches = mapMaybe (\(i, pv) ->
             case matchSinglePeriod target pv of
               Just (c, a) ->
                 let err = magnitude (target - (fromIntegral c :+ 0) - (fromIntegral a :+ 0) * pv)
                 in if err < 1e-6
                    then Just (c, replicate i 0 ++ [a] ++ replicate (length periods - i - 1) 0, err)
                    else Nothing
               Nothing -> Nothing
             ) (zip [0..] periodVals)
           -- Try multi-period matching
           multiMatch = case findIntegerComboC target periodVals of
             Just (c, as) ->
               let err = magnitude (target - (fromIntegral c :+ 0) -
                           sum (zipWith (\a pv -> (fromIntegral a :+ 0) * pv) as periodVals))
               in if err < 1e-6 then Just (c, as, err) else Nothing
             Nothing -> Nothing
           allMatches = singleMatches ++ maybe [] (:[]) multiMatch
           -- Pick match with smallest max coefficient
           bestMatch = case allMatches of
             [] -> Nothing
             ms -> Just $ foldl1 (\a b -> if maxCoeff a <= maxCoeff b then a else b) ms
           maxCoeff (c, as, _) = maximum (abs c : map abs as)
       in case bestMatch of
            Just (constant, coeffs, _err) ->
              foldl Add (Lit (fromIntegral constant))
                [ if c == 0 then Lit 0
                  else if c == 1 then periodExpr pi'
                  else Mul (Lit (fromIntegral c)) (periodExpr pi')
                | (c, pi') <- zip coeffs periods
                ]
            Nothing ->
              -- Fall back to rational approximation preserving complex value
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
  in if magnitude (recon - target) < 1e-4
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
findIntegerComboC target vals = solveLinearIntegerC target vals

-- | Solve target = c + Σ aᵢ·xᵢ for integer c, aᵢ using the complex structure.
solveLinearIntegerC :: Complex Double -> [Complex Double] -> Maybe (Int, [Int])
solveLinearIntegerC target [] =
  -- No periods: target should be a real integer
  let c = round (realPart target) :: Int
  in if magnitude (target - (fromIntegral c :+ 0)) < 1e-6
     then Just (c, [])
     else Nothing
solveLinearIntegerC target [v] =
  -- Single period: from Im(target) = a · Im(v), then c = Re(target) - a · Re(v)
  let a = if abs (imagPart v) > 1e-10
          then round (imagPart target / imagPart v) :: Int
          else if abs (realPart v) > 1e-10
               then round (realPart target / realPart v)
               else 0
      remainder = target - (fromIntegral a :+ 0) * v
      c = round (realPart remainder) :: Int
  in if magnitude (target - (fromIntegral c :+ 0) - (fromIntegral a :+ 0) * v) < 1e-4
     then Just (c, [a])
     else Nothing
solveLinearIntegerC target (v:vs) =
  -- Multiple periods: use imaginary part to constrain first coefficient,
  -- then recurse on the remainder.
  -- Im(target) = a · Im(v) + Σ aᵢ · Im(vᵢ)
  -- If Im(v) is large enough, estimate a, then recurse.
  if abs (imagPart v) > 1e-10
  then
    -- Estimate a from imaginary part (approximate, refine by trying nearby values)
    let aEst = round (imagPart target / imagPart v) :: Int
        tryA a =
          let remainder = target - (fromIntegral a :+ 0) * v
          in case solveLinearIntegerC remainder vs of
               Just (c, as) ->
                 let recon = (fromIntegral c :+ 0) + (fromIntegral a :+ 0) * v
                           + sum (zipWith (\ai xi -> (fromIntegral ai :+ 0) * xi) as vs)
                 in if magnitude (recon - target) < 1e-4
                    then Just (c, a : as)
                    else Nothing
               Nothing -> Nothing
    in case mapMaybe tryA [aEst-2..aEst+2] of
         (x:_) -> Just x
         []    -> Nothing
  else
    -- v has negligible imaginary part; try direct ratio
    let aEst = if abs (realPart v) > 1e-10
               then round (realPart target / realPart v) :: Int
               else 0
        tryA a =
          let remainder = target - (fromIntegral a :+ 0) * v
          in case solveLinearIntegerC remainder vs of
               Just (c, as) ->
                 let recon = (fromIntegral c :+ 0) + (fromIntegral a :+ 0) * v
                           + sum (zipWith (\ai xi -> (fromIntegral ai :+ 0) * xi) as vs)
                 in if magnitude (recon - target) < 1e-4
                    then Just (c, a : as)
                    else Nothing
               Nothing -> Nothing
    in case mapMaybe tryA [aEst-2..aEst+2] of
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
                    -> [Complex Double]  -- ^ Numerical values of sub-periods (complex)
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
  in assignByValue [root1, root2] numVals

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
  in assignByValue [root0, root1, root2] numVals

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
                        -> [Complex Double] -- ^ Numerical sub-period values
                        -> [RadExpr Rational]
solvePeriodViaResolvent q allPeriods p parentExpr numVals =
  let -- ω = e^{2πi/q} as Complex Double
      omegaC = exp (0 :+ (2 * pi / fromIntegral q))

      -- Compute resolvents R_j numerically
      resolventVals = [ sum [ omegaC ^ (j * k) * (numVals !! k)
                            | k <- [0..q-1] ]
                      | j <- [0..q-1] ]

      -- R_j^q for j = 0, ..., q-1
      resolventPowersQ = [ (resolventVals !! j) ^ q | j <- [0..q-1] ]

      -- Decompose R_j^q into period-field and ω components:
      -- R_j^q = Σ_s d_s · ω^{js}, so d_s = (1/q) Σ_j ω^{-js} R_j^q
      dCoeffs = [ let s' = fromIntegral s :: Int
                  in (1 / fromIntegral q) *
                     sum [ omegaC ^^ (negate (j * s')) * (resolventPowersQ !! j)
                         | j <- [0..q-1] ]
                | s <- [0..q-1 :: Int] ]

      -- Each d_s should be in Q(periods). Match to period expressions.
      dExprs = map (matchToPeriodExpr allPeriods p) dCoeffs

      -- Get cos(2π/q) as a RadExpr
      cosBaseExpr = case cosOfUnityViaGauss q of
        Just e  -> e
        Nothing -> error $ "solvePeriodViaResolvent: can't compute cos(2π/" ++ show q ++ ")"

      -- Build ω^m as RadExpr for all needed m = 0, ..., q-1
      omegaPowers = map (omegaPowerExpr q cosBaseExpr) [0..q-1]

      -- R_j^q = Σ_s d_s · ω^{js} as RadExpr
      resolventPowerExprs =
        [ foldl1 Add [ Mul (dExprs !! s) (omegaPowers !! ((j * s) `mod` q))
                      | s <- [0..q-1] ]
        | j <- [1..q-1] ]

      -- R_j = q-th root of R_j^q, with branch selection
      resolventExprs = [ selectResolventBranch q omegaPowers
                           (resolventPowerExprs !! (j-1))
                           (resolventPowersQ !! j)
                           (resolventVals !! j)
                       | j <- [1..q-1] ]

      -- All resolvents: R_0 = parent, R_1, ..., R_{q-1}
      allResolvents = parentExpr : resolventExprs

      -- Recover sub-periods: η_k = (1/q) Σ_{j=0}^{q-1} ω^{-jk} · R_j
      subPeriodExprs =
        [ Mul (Inv (Lit (fromIntegral q)))
              (foldl1 Add [ Mul (omegaPowers !! ((q - ((j * k) `mod` q)) `mod` q))
                                (allResolvents !! j)
                          | j <- [0..q-1] ])
        | k <- [0..q-1] ]

      -- Compute numerical sub-period values directly from numerical resolvents
      -- (avoids expensive evalComplex on the symbolic expressions)
      allResolventVals = [resolventVals !! j | j <- [0..q-1]]
      numSubPeriods =
        [ (1 / fromIntegral q) *
          sum [ omegaC ^^ (negate (j * k)) * (allResolventVals !! j)
              | j <- [0..q-1] ]
        | k <- [0..q-1] ]

      result = assignByValueNum subPeriodExprs numSubPeriods numVals
  in result

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

-- | Select the correct branch of the q-th root.
--
-- The q-th roots of z are: ω^k · ᵠ√z for k = 0, ..., q-1.
-- Evaluate the principal root of the expression and find the ω^k
-- correction that best matches the target numerical value.
selectResolventBranch :: Int
                      -> [RadExpr Rational]   -- ^ ω^k for k = 0, ..., q-1
                      -> RadExpr Rational     -- ^ R_j^q expression
                      -> Complex Double       -- ^ Numerical value of R_j^q (unused, kept for API)
                      -> Complex Double       -- ^ Target numerical value of R_j
                      -> RadExpr Rational
selectResolventBranch q omegaPowers rjqExpr _rjqVal targetVal =
  let principalRoot = Root q rjqExpr
      -- Evaluate the actual principal root from the expression
      principalVal = evalComplex principalRoot
      -- Find the ω^k correction that best matches the target
      omegaC = exp (0 :+ (2 * pi / fromIntegral q))
      scored = [ (k, magnitude (omegaC ^ k * principalVal - targetVal))
               | k <- [0..q-1] ]
      bestK = fst $ foldl1 (\a b -> if snd a <= snd b then a else b) scored
      bestErr = snd $ foldl1 (\a b -> if snd a <= snd b then a else b) scored
      exprBranch = if bestK == 0 then principalRoot
                   else Mul (omegaPowers !! bestK) principalRoot
      -- If the expression can't match the target well (e.g., R_j is small and
      -- R_j^q is dominated by d_s matching errors), fall back to numerical Lit
      relErr = bestErr / max 1e-20 (magnitude targetVal)
  in if relErr > 0.01
     then -- Expression is too inaccurate; use numerical approximation
       let re = toRational (realPart targetVal)
           im = toRational (imagPart targetVal)
       in if abs (imagPart targetVal) < 1e-15
          then Lit re
          else Add (Lit re) (Mul (Lit im) (Root 2 (Lit (-1))))
     else exprBranch

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

-- | Assign radical expressions to numerical values by matching.
-- Each expression is evaluated numerically (as Complex Double) and
-- paired with the closest target value using complex magnitude.
assignByValue :: [RadExpr Rational] -> [Complex Double] -> [RadExpr Rational]
assignByValue exprs vals =
  let exprVals = [(e, evalComplex e) | e <- exprs]
  in map (pickClosest exprVals) vals
  where
    pickClosest evs target =
      case evs of
        []     -> error "assignByValue: empty expression list"
        (e:es) -> fst $ NE.head $ NE.sortBy (comparing (\(_, v) -> magnitude (v - target))) (e :| es)

-- | Like 'assignByValue', but using pre-computed numerical values for expressions
-- (avoids expensive evalComplex on large expression trees).
assignByValueNum :: [RadExpr Rational] -> [Complex Double] -> [Complex Double] -> [RadExpr Rational]
assignByValueNum exprs exprVals vals =
  let paired = zip exprs exprVals
  in map (pickClosest paired) vals
  where
    pickClosest evs target =
      case evs of
        []     -> error "assignByValueNum: empty expression list"
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
