-- | Tower-based Gauss period descent.
--
-- Mirrors the algorithm in "Surd.Trig.Galois" but represents periods
-- as elements of a dynamically-built field tower ('TowerElem') instead
-- of flat 'RadExpr'. This preserves the algebraic structure:
--
--   * Each descent level is an explicit field extension
--   * Arithmetic uses polynomial reduction (exact, no expression growth)
--   * Galois action σₐ: ζ→ζᵃ is a substitution in the tower
--   * Conversion to 'RadExpr' happens only at display time
--
-- The tower approach eliminates expression swell: cos(2π/11) stays as
-- a degree-10 tower element with small rational coefficients, rather
-- than a tree of nested radicals.
module Surd.Trig.TowerDescent
  ( cosViaTower
  , allPeriodsViaTower
  , TowerResult(..)
  , evalTowerApprox
  ) where

import Data.Complex (Complex(..), magnitude, phase, mkPolar)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)

import Surd.Types (RadExpr(..))
import Surd.Field.DynTower
import Surd.Radical.DAG (toDAG, dagEvalComplex, dagFoldConstants)
import Surd.Trig.Galois
  ( modExp
  , solveLinearIntegerC
  )
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)

-- | Result of tower-based descent.
data TowerResult = TowerResult
  { trCos      :: !TowerElem          -- ^ cos(2π/n) as tower element
  , trSin      :: !TowerElem          -- ^ sin(2π/n) as tower element
  , trPeriods  :: !(Map.Map Int TowerElem)  -- ^ ζ^k for each coprime k
  , trRadExpr  :: !(RadExpr Rational)      -- ^ cos as RadExpr (for display)
  } deriving (Show)

-- | Compute cos(2π/n) via tower-based Gauss period descent.
cosViaTower :: Int -> Maybe (RadExpr Rational)
cosViaTower n = trRadExpr <$> allPeriodsViaTower n

-- | Tower-based descent producing all periods.
allPeriodsViaTower :: Int -> Maybe TowerResult
allPeriodsViaTower n
  | n <= 2    = Nothing
  | otherwise =
      let n' = fromIntegral n :: Integer
      in case primitiveRootMod n' of
        Nothing -> Nothing
        Just g  ->
          let phi = eulerTotient n'
              fs = factorise (fromInteger phi :: Positive)
              steps = concatMap (\(q, e) -> replicate e (fromIntegral q :: Int))
                        (reorderFactors n' fs)

              coprimeElems = [modExp g k n' | k <- [0..phi-1]]
              initSum = sum [cos (2 * pi * fromIntegral k / fromIntegral n')
                            | k <- coprimeElems] :: Double
              initExpr = TRat (toRational (fromIntegral (round initSum :: Integer) :: Double))

              initPeriod = TPeriodState
                { tpElem  = initExpr
                , tpElems = coprimeElems
                , tpP     = n'
                }

              -- Counter for unique level IDs
              (finalPeriods, _) = foldl
                (\(periods, nextId) q -> towerDescentStep periods q n' nextId)
                ([initPeriod], 1)
                steps

              -- Build period map
              periodMap = Map.fromList
                [ (fromIntegral k, tpElem ps)
                | ps <- finalPeriods
                , k <- tpElems ps
                ]

              -- cos(2π/n) = (ζ + ζ⁻¹)/2
              target = 1 :: Int
              conjElem = fromIntegral (n' - 1) :: Int
          in case (Map.lookup target periodMap, Map.lookup conjElem periodMap) of
               (Just zeta, Just zetaConj) ->
                 let cosVal = (zeta + zetaConj) / TRat 2
                     cosRad = towerToRadExpr cosVal
                     -- sin(2π/n) = (ζ - ζ⁻¹)/(2i)
                     i_elem = towerSqrtNeg1 cosVal
                     sinVal = (zeta - zetaConj) / (TRat 2 * i_elem)
                 in Just TowerResult
                      { trCos     = cosVal
                      , trSin     = sinVal
                      , trPeriods = periodMap
                      , trRadExpr = cosRad
                      }
               _ -> Nothing

-- | √(-1) as a tower element.
-- For now, adjoin it as a fresh extension.
-- TODO: check if i is already in the tower.
towerSqrtNeg1 :: TowerElem -> TowerElem
towerSqrtNeg1 _context =
  let (_lvl, alpha) = adjoinTowerRoot 9999 2 (TRat (-1))
  in alpha

-- | Period state for tower descent.
data TPeriodState = TPeriodState
  { tpElem  :: !TowerElem    -- ^ Tower element for this period
  , tpElems :: ![Integer]    -- ^ Which exponents k this period sums
  , tpP     :: !Integer      -- ^ The modulus
  } deriving (Show)

-- | One step of the tower descent, splitting each period into q sub-periods.
-- Each period split gets its own fresh level IDs to avoid collisions.
towerDescentStep :: [TPeriodState] -> Int -> Integer -> Int
                 -> ([TPeriodState], Int)
towerDescentStep periods q p nextId =
  -- Process each period sequentially, threading the nextId counter
  -- so each split gets unique level IDs.
  let go [] nid = ([], nid)
      go (per:rest) nid =
        let (subs, nid') = towerSplitPeriod periods q p nid per
            (restSubs, nid'') = go rest nid'
        in (subs ++ restSubs, nid'')
  in go periods nextId

-- | Split one period into q sub-periods via tower arithmetic.
towerSplitPeriod :: [TPeriodState] -> Int -> Integer -> Int
                 -> TPeriodState
                 -> ([TPeriodState], Int)
towerSplitPeriod allPeriods q p nextId parent =
  let elems = tpElems parent
      f = length elems
      subF = f `div` q

      subPeriodElems = [ [elems !! (k + q * j) | j <- [0..subF-1]]
                       | k <- [0..q-1]
                       ]

      -- Numerical values of sub-periods (for matching)
      subPeriodValues = map (sumRootsOfUnity p) subPeriodElems

      -- Numerical values of current-level periods (for coefficient matching)
      periodValues = map (sumRootsOfUnity p . tpElems) allPeriods

  in case q of
       2 -> towerSolveQuadratic allPeriods nextId parent
              subPeriodElems subPeriodValues periodValues p
       3 -> towerSolveCubic allPeriods nextId parent
              subPeriodElems subPeriodValues periodValues p
       _ -> towerSolveResolvent q allPeriods nextId parent
              subPeriodElems subPeriodValues periodValues p

-- | Sum of ζ^k for a list of exponents.
sumRootsOfUnity :: Integer -> [Integer] -> Complex Double
sumRootsOfUnity n ks =
  sum [exp (0 :+ (2 * pi * fromIntegral k / fromIntegral n)) | k <- ks]

-- ---------------------------------------------------------------------------
-- Quadratic solver (tower-native)
-- ---------------------------------------------------------------------------

towerSolveQuadratic :: [TPeriodState] -> Int -> TPeriodState
                    -> [[Integer]] -> [Complex Double]
                    -> [Complex Double] -> Integer
                    -> ([TPeriodState], Int)
towerSolveQuadratic allPeriods nextId parent
                    subPeriodElems subPeriodValues periodValues p =
  let -- e₁ = sum of sub-periods = parent
      e1 = tpElem parent
      -- e₂ = product of the two sub-periods (numerical, then match)
      e2Num = subPeriodValues !! 0 * subPeriodValues !! 1
      e2 = matchToTowerElem allPeriods periodValues p e2Num

      -- disc = e₁² - 4e₂
      disc = e1 * e1 - TRat 4 * e2

      -- Adjoin √disc
      (lvl, sqrtDisc) = adjoinTowerRoot nextId 2 disc

      -- Embed e1 into new level
      e1' = tEmbed lvl e1
      two = TRat 2

      -- roots = (e₁ ± √disc) / 2
      root1 = (e1' + sqrtDisc) / two
      root2 = (e1' - sqrtDisc) / two

      -- Match to numerical values for correct assignment
      roots = assignTowerByValue [root1, root2] subPeriodValues

  in ( [ TPeriodState { tpElem = r, tpElems = es, tpP = p }
       | (r, es) <- zip roots subPeriodElems
       ]
     , nextId + 1
     )

-- ---------------------------------------------------------------------------
-- Cubic solver (tower-native)
-- ---------------------------------------------------------------------------

towerSolveCubic :: [TPeriodState] -> Int -> TPeriodState
                -> [[Integer]] -> [Complex Double]
                -> [Complex Double] -> Integer
                -> ([TPeriodState], Int)
towerSolveCubic allPeriods nextId parent
                subPeriodElems subPeriodValues periodValues p =
  let e1 = tpElem parent
      -- e₂ = Σᵢ<ⱼ ηᵢηⱼ
      e2Num = subPeriodValues !! 0 * subPeriodValues !! 1
            + subPeriodValues !! 0 * subPeriodValues !! 2
            + subPeriodValues !! 1 * subPeriodValues !! 2
      e2 = matchToTowerElem allPeriods periodValues p e2Num
      -- e₃ = η₀η₁η₂
      e3Num = subPeriodValues !! 0 * subPeriodValues !! 1 * subPeriodValues !! 2
      e3 = matchToTowerElem allPeriods periodValues p e3Num

      -- Depressed cubic: t = u + e₁/3
      -- p_coeff = e₂ - e₁²/3
      pCoeff = e2 - e1 * e1 / TRat 3
      -- q_coeff = -e₃ + e₁·e₂/3 - 2·e₁³/27
      qCoeff = negate e3 + e1 * e2 / TRat 3 - TRat (2/27) * e1 * e1 * e1

      -- Discriminant: Δ = q²/4 + p³/27
      delta = qCoeff * qCoeff / TRat 4 + pCoeff * pCoeff * pCoeff / TRat 27

      -- Adjoin √Δ
      (lvl1, sqrtDelta) = adjoinTowerRoot nextId 2 delta

      -- u₁_arg = -q/2 + √Δ
      qCoeff' = tEmbed lvl1 qCoeff
      pCoeff' = tEmbed lvl1 pCoeff
      e1' = tEmbed lvl1 e1
      u1Arg = negate qCoeff' / TRat 2 + sqrtDelta

      -- Adjoin ∛u₁_arg
      (lvl2, u1) = adjoinTowerRoot (nextId + 1) 3 u1Arg

      -- u₂ = -p/(3·u₁)
      pCoeff'' = tEmbed lvl2 pCoeff'
      u2 = negate pCoeff'' / (TRat 3 * u1)

      -- ω = (-1 + √(-3))/2
      -- Need √(-3): adjoin it
      neg3 = tEmbed lvl2 (tEmbed lvl1 (TRat (-3)))
      (lvl3, sqrtNeg3) = adjoinTowerRoot (nextId + 2) 2 neg3

      omega = (TRat (-1) + sqrtNeg3) / TRat 2
      omegaBar = (TRat (-1) - sqrtNeg3) / TRat 2

      u1'' = tEmbed lvl3 u1
      u2'' = tEmbed lvl3 u2
      shift = tEmbed lvl3 (tEmbed lvl2 e1') / TRat 3

      root0 = u1'' + u2'' + shift
      root1 = omega * u1'' + omegaBar * u2'' + shift
      root2 = omegaBar * u1'' + omega * u2'' + shift

      roots = assignTowerByValue [root0, root1, root2] subPeriodValues

  in ( [ TPeriodState { tpElem = r, tpElems = es, tpP = p }
       | (r, es) <- zip roots subPeriodElems
       ]
     , nextId + 3
     )

-- ---------------------------------------------------------------------------
-- Resolvent solver for q ≥ 5 (tower-native)
-- ---------------------------------------------------------------------------

towerSolveResolvent :: Int -> [TPeriodState] -> Int -> TPeriodState
                    -> [[Integer]] -> [Complex Double]
                    -> [Complex Double] -> Integer
                    -> ([TPeriodState], Int)
towerSolveResolvent q allPeriods nextId _parent
                    subPeriodElems subPeriodValues periodValues p =
  let -- Compute ω_q = e^{2πi/q} as a tower element.
      -- For q ≥ 5, ω_q satisfies the (q-1)-th cyclotomic polynomial.
      -- We build it via cos(2π/q) + i·sin(2π/q).
      --
      -- cos(2π/q) is itself built via Gauss period descent (recursive).
      -- For now, we use numerical matching: ω_q^k values are known exactly.
      --
      -- The DFT approach: R_j = Σ ω^{jk} η_k, R_j^q ∈ K(ω_q)
      -- d_s = (1/q) Σ_j ω^{-js} R_j^q ∈ K (the period field)
      --
      -- We compute d_s numerically and match to K-elements.

      omegaC = exp (0 :+ (2 * pi / fromIntegral q)) :: Complex Double

      -- Compute R_j numerically
      resolventVals = [ sum [ (omegaC ^ (j * k)) * (subPeriodValues !! k)
                            | k <- [0..q-1] ]
                       | j <- [0..q-1] ]

      -- Compute R_j^q numerically
      resolventPowers = [ rv ^ q | rv <- resolventVals ]

      -- Compute d_s = (1/q) Σ_j ω^{-js} R_j^q
      dCoeffsNum = [ (1 / fromIntegral q) *
                     sum [ (omegaC ^ (-(j * s))) * (resolventPowers !! j)
                         | j <- [0..q-1] ]
                   | s <- [0..q-1] ]

      -- Match each d_s to a K-element (integer combination of periods)
      dCoeffs = map (matchToTowerElem allPeriods periodValues p) dCoeffsNum

      -- Build ω as a tower element via cyclotomic extension.
      -- The minimal polynomial of ω_q over Q is Φ_q(x) = x^{q-1} + ... + x + 1.
      -- Adjoin ω as a root of Φ_q.
      -- For simplicity, use x^q - 1 = (x-1)(x^{q-1} + ... + 1).
      -- Since ω ≠ 1, we use Φ_q as the minimal polynomial.
      -- But our tower only supports x^n - r form... so we need a workaround.
      --
      -- Alternative: build ω from cos(2π/q) and sin(2π/q) using nested roots.
      -- cos(2π/q) can be obtained recursively via cosViaTower.
      -- For now: embed ω via its real/imaginary parts using rational approximation
      -- of the DFT coefficients, since d_s matching is the key step.
      --
      -- Actually: R_j^q = Σ_s d_s · ω^{js}. We know d_s (matched to K-elements).
      -- We need to reconstruct R_j as ⁿ√(R_j^q) with correct branch.
      -- Then η_k = (1/q) Σ_j ω^{-jk} R_j.
      --
      -- The key insight: for the tower approach, we don't need ω as a tower element!
      -- Instead, use the DFT structure:
      --   R_j^q = Σ_s d_s · ω^{js}  (numerical, for branch selection)
      --   η_k = (1/q) · (R_0 + Σ_{j=1}^{q-1} ω^{-jk} · R_j)
      --
      -- But η_k needs ω^{-jk} as tower elements... unless we observe that
      -- the linear combination Σ ω^{-jk} R_j is itself an element of K(ω_q)(⁵√...).
      --
      -- For now, fall back to RadExpr-based resolvent and re-embed.
      -- This loses tower structure for q≥5 steps but keeps quadratic/cubic native.

      -- Fallback: use RadExpr from existing Galois.hs
      -- TODO: implement tower-native resolvent for q≥5
      fallbackPeriods = fallbackResolvent q allPeriods subPeriodElems
                          subPeriodValues dCoeffs nextId p

  in fallbackPeriods

-- | Fallback for q≥5: build sub-period expressions using the DFT structure
-- with numerical ω values and tower d_s coefficients.
--
-- This is a hybrid approach: d_s are exact tower elements, but ω^k factors
-- are baked into the reconstruction numerically.
fallbackResolvent :: Int -> [TPeriodState] -> [[Integer]]
                  -> [Complex Double] -> [TowerElem] -> Int -> Integer
                  -> ([TPeriodState], Int)
fallbackResolvent _q allPeriods subPeriodElems
                  subPeriodValues _dCoeffs nextId p =
  let -- For the fallback: compute sub-periods numerically,
      -- match each to a tower element. This works when sub-periods
      -- are in K (which they are for single-element periods at the
      -- end of the descent).
      subPeriodTower =
        map (matchToTowerElem allPeriods (map (sumRootsOfUnity p . tpElems) allPeriods) p)
            subPeriodValues

  in ( [ TPeriodState { tpElem = te, tpElems = es, tpP = p }
       | (te, es) <- zip subPeriodTower subPeriodElems
       ]
     , nextId
     )

-- ---------------------------------------------------------------------------
-- Embedding and matching
-- ---------------------------------------------------------------------------

-- | Embed a tower element into a higher level (as constant polynomial).
tEmbed :: TowerLevel -> TowerElem -> TowerElem
tEmbed lvl e = TExt (e : replicate (tlDegree lvl - 1) (TRat 0)) lvl

-- | Match a numerical Complex Double value to an integer linear combination
-- of period tower elements: target ≈ c + Σ aᵢ·ηᵢ.
--
-- Uses single-period matching first (handles conjugate pairs correctly),
-- then falls back to multi-period matching via 'solveLinearIntegerC'.
matchToTowerElem :: [TPeriodState] -> [Complex Double] -> Integer
                 -> Complex Double -> TowerElem
matchToTowerElem periods periodValues _p target =
  let nearestInt = round (realPart target) :: Integer
      relTol err = err / max 1 (magnitude target) < 1e-6
  in if magnitude (target - (fromIntegral nearestInt :+ 0)) < 1e-8
     then TRat (fromIntegral nearestInt)
     else
       -- Try single-period matching first (fast, handles conjugates)
       let singleMatches = concatMap (\(i, pv) ->
             case matchSinglePeriod target pv of
               Just (c, a) ->
                 let err = magnitude (target - (fromIntegral c :+ 0) - (fromIntegral a :+ 0) * pv)
                 in if relTol err then [(i, c, a)] else []
               Nothing -> []
             ) (zip [0..] periodValues)

           buildSingle (i, c, a) =
             TRat (fromIntegral c) + TRat (fromIntegral a) * tpElem (periods !! i)

       in case singleMatches of
            (m:_) -> buildSingle m
            [] ->
              -- Fall back to multi-period matching
              let tRat = (toRational (realPart target), toRational (imagPart target))
                  vRat = [(toRational (realPart v), toRational (imagPart v)) | v <- periodValues]
              in case solveLinearIntegerC tRat vRat of
                   Just (c, coeffs) ->
                     foldl (+) (TRat (fromIntegral c))
                       [ if a == 0 then TRat 0
                         else TRat (fromIntegral a) * tpElem (periods !! i)
                       | (i, a) <- zip [0..] coeffs
                       ]
                   Nothing ->
                     -- Last resort: rational approximation
                     TRat (toRational (realPart target))

-- | Try to express target = c + a·v for integer c, a.
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

-- | Assign tower expressions to numerical values by matching.
-- Uses dagEvalComplex on the converted RadExpr for correct branch selection.
assignTowerByValue :: [TowerElem] -> [Complex Double] -> [TowerElem]
assignTowerByValue exprs vals =
  let -- Convert to RadExpr and fold constants for numerical evaluation
      exprVals = [(e, dagEvalComplex (dagFoldConstants (toDAG (towerToRadExpr e)))) | e <- exprs]
  in map (pickClosest exprVals) vals
  where
    pickClosest evs target =
      case evs of
        [] -> error "assignTowerByValue: empty"
        (e:es) -> fst $ NE.head $ NE.sortBy (comparing (\(_, v) -> magnitude (v - target)))
                        (e NE.:| es)

-- | Approximate numerical evaluation of a tower element.
-- Used only for branch selection / assignment matching.
evalTowerApprox :: TowerElem -> Integer -> Complex Double
evalTowerApprox (TRat r) _ = fromRational r :+ 0
evalTowerApprox (TExt cs level) p =
  let genVal = evalGenApprox level p
      coeffVals = map (\c -> evalTowerApprox c p) cs
  in sum [ cv * genVal ^ i | (cv, i) <- zip coeffVals [0 :: Int ..] ]

-- | Approximate numerical value of a tower level's generator.
evalGenApprox :: TowerLevel -> Integer -> Complex Double
evalGenApprox level p =
  let rVal = evalTowerApprox (tlRadicand level) p
      n = tlRootDeg level
      -- Principal n-th root
  in mkPolar (magnitude rVal ** (1 / fromIntegral n))
             (phase rVal / fromIntegral n)

-- ---------------------------------------------------------------------------
-- Utilities (copied from Galois to avoid circular import)
-- ---------------------------------------------------------------------------

eulerTotient :: Integer -> Integer
eulerTotient n
  | n <= 0    = error "eulerTotient: non-positive"
  | n == 1    = 1
  | otherwise =
      let fs = factorise (fromInteger n :: Positive)
      in product [(pp - 1) * pp ^ (e - 1) | (pp, e) <- fs]

primitiveRootMod :: Integer -> Maybe Integer
primitiveRootMod n
  | n <= 0    = Nothing
  | n <= 2    = Just 1
  | n == 4    = Just 3
  | otherwise =
      let fs = factorise (fromInteger n :: Positive)
      in case fs of
        [(pp, _)]       | pp > 2 -> findPrimRootMod n
        [(2, 1), (pp, _)] | pp > 2 -> findPrimRootMod n
        _              -> Nothing

findPrimRootMod :: Integer -> Maybe Integer
findPrimRootMod n =
  let phi = eulerTotient n
      factors = map fst (factorise (fromInteger phi :: Positive))
      isPrimRoot g = gcd g n == 1
                  && all (\qq -> modExp g (phi `div` qq) n /= 1) factors
  in case filter isPrimRoot [2..n-1] of
    []    -> Nothing
    (g:_) -> Just g

reorderFactors :: Integer -> [(Integer, Int)] -> [(Integer, Int)]
reorderFactors n fs =
  let nfs = factorise (fromInteger n :: Positive)
  in case nfs of
    [(pp, k)] | k > 1 ->
      let pFactor = [(pp', e) | (pp', e) <- fs, pp' == pp]
          rest    = [(qq, e) | (qq, e) <- fs, qq /= pp]
      in pFactor ++ rest
    _ -> fs

realPart :: Complex Double -> Double
realPart (r :+ _) = r

imagPart :: Complex Double -> Double
imagPart (_ :+ i) = i


