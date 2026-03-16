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
  ( cosViaTower,
    allPeriodsViaTower,
    TowerResult (..),
    evalTowerApprox,
  )
where

import Data.Complex (Complex (..), magnitude, mkPolar, phase)
import Data.List (minimumBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set
import Math.Internal.Positive (Positive)
import Math.Internal.PrimeFactors (factorise)
import Surd.Field.DynTower
import Surd.Radical.DAG (dagEvalComplex, dagFoldConstants, toDAG)
import Surd.Trig.Galois
  ( modExp,
    solveLinearIntegerC,
  )
import Surd.Types (RadExpr (..))

-- | Result of tower-based descent.
--
-- Contains both the tower-element representations (for exact arithmetic
-- in subsequent computations) and the converted 'RadExpr' (for display).
data TowerResult = TowerResult
  { -- | cos(2π\/n) as a tower element in the field tower built by the descent
    trCos :: !TowerElem,
    -- | sin(2π\/n) as a tower element, computed as (ζ - ζ^{-1}) \/ (2i)
    trSin :: !TowerElem,
    -- | Map from exponent k to the tower element for ζ^k = e^{2πik\/n},
    -- for all k coprime to n
    trPeriods :: !(Map.Map Int TowerElem),
    -- | cos(2π\/n) converted to a 'RadExpr' for display and further symbolic manipulation
    trRadExpr :: !(RadExpr Rational)
  }
  deriving (Show)

-- | Compute cos(2π\/n) via tower-based Gauss period descent.
--
-- Returns the cosine as a 'RadExpr' by converting the tower element
-- from 'allPeriodsViaTower'. Returns 'Nothing' if n <= 2 or if
-- (Z\/nZ)* is not cyclic (no primitive root exists).
cosViaTower :: Int -> Maybe (RadExpr Rational)
cosViaTower n = trRadExpr <$> allPeriodsViaTower n

-- | Full tower-based Gauss period descent producing all primitive nth
-- roots of unity as tower elements.
--
-- Builds a dynamically-nested field tower by descending through the
-- subgroup chain of (Z\/nZ)*, solving a period equation at each step.
-- Each step introduces a new extension level in the tower (quadratic,
-- cubic, or quintic depending on the prime factor).
--
-- Returns 'Nothing' if n <= 2 or (Z\/nZ)* has no primitive root.
allPeriodsViaTower :: Int -> Maybe TowerResult
allPeriodsViaTower n
  | n <= 2 = Nothing
  | otherwise =
      let n' = fromIntegral n :: Integer
       in case primitiveRootMod n' of
            Nothing -> Nothing
            Just g ->
              let phi = eulerTotient n'
                  fs = factorise (fromInteger phi :: Positive)
                  steps =
                    concatMap
                      (\(q, e) -> replicate e (fromIntegral q :: Int))
                      (reorderFactors n' fs)

                  coprimeElems = [modExp g k n' | k <- [0 .. phi - 1]]
                  initSum =
                    sum
                      [ cos (2 * pi * fromIntegral k / fromIntegral n')
                        | k <- coprimeElems
                      ] ::
                      Double
                  initExpr = TRat (toRational (fromIntegral (round initSum :: Integer) :: Double))

                  initPeriod =
                    TPeriodState
                      { tpElem = initExpr,
                        tpElems = coprimeElems,
                        tpP = n'
                      }

                  -- Counter for unique level IDs
                  (finalPeriods, _) =
                    foldl
                      (\(periods, nextId) q -> towerDescentStep periods q n' nextId)
                      ([initPeriod], 1)
                      steps

                  -- Build period map
                  periodMap =
                    Map.fromList
                      [ (fromIntegral k, tpElem ps)
                        | ps <- finalPeriods,
                          k <- tpElems ps
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
                       in Just
                            TowerResult
                              { trCos = cosVal,
                                trSin = sinVal,
                                trPeriods = periodMap,
                                trRadExpr = cosRad
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
  { -- | Tower element for this period
    tpElem :: !TowerElem,
    -- | Which exponents k this period sums
    tpElems :: ![Integer],
    -- | The modulus
    tpP :: !Integer
  }
  deriving (Show)

-- | One step of the tower descent, splitting each period into q sub-periods.
-- Each period split gets its own fresh level IDs to avoid collisions.
towerDescentStep ::
  [TPeriodState] ->
  Int ->
  Integer ->
  Int ->
  ([TPeriodState], Int)
towerDescentStep periods q p nextId =
  -- Process each period sequentially, threading the nextId counter
  -- so each split gets unique level IDs.
  let go [] nid = ([], nid)
      go (per : rest) nid =
        let (subs, nid') = towerSplitPeriod periods q p nid per
            (restSubs, nid'') = go rest nid'
         in (subs ++ restSubs, nid'')
   in go periods nextId

-- | Split one period into q sub-periods via tower arithmetic.
towerSplitPeriod ::
  [TPeriodState] ->
  Int ->
  Integer ->
  Int ->
  TPeriodState ->
  ([TPeriodState], Int)
towerSplitPeriod allPeriods q p nextId parent =
  let elems = tpElems parent
      f = length elems
      subF = f `div` q

      subPeriodElems =
        [ [elems !! (k + q * j) | j <- [0 .. subF - 1]]
          | k <- [0 .. q - 1]
        ]

      -- Numerical values of sub-periods (for matching)
      subPeriodValues = map (sumRootsOfUnity p) subPeriodElems

      -- Numerical values of current-level periods (for coefficient matching)
      periodValues = map (sumRootsOfUnity p . tpElems) allPeriods
   in case q of
        2 ->
          towerSolveQuadratic
            allPeriods
            nextId
            parent
            subPeriodElems
            subPeriodValues
            periodValues
            p
        3 ->
          towerSolveCubic
            allPeriods
            nextId
            parent
            subPeriodElems
            subPeriodValues
            periodValues
            p
        _ ->
          towerSolveResolvent
            q
            allPeriods
            nextId
            parent
            subPeriodElems
            subPeriodValues
            periodValues
            p

-- | Sum of ζ^k for a list of exponents.
sumRootsOfUnity :: Integer -> [Integer] -> Complex Double
sumRootsOfUnity n ks =
  sum [exp (0 :+ (2 * pi * fromIntegral k / fromIntegral n)) | k <- ks]

-- ---------------------------------------------------------------------------
-- Quadratic solver (tower-native)
-- ---------------------------------------------------------------------------

towerSolveQuadratic ::
  [TPeriodState] ->
  Int ->
  TPeriodState ->
  [[Integer]] ->
  [Complex Double] ->
  [Complex Double] ->
  Integer ->
  ([TPeriodState], Int)
towerSolveQuadratic
  allPeriods
  nextId
  parent
  subPeriodElems
  subPeriodValues
  periodValues
  p =
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
     in ( [ TPeriodState {tpElem = r, tpElems = es, tpP = p}
            | (r, es) <- zip roots subPeriodElems
          ],
          nextId + 1
        )

-- ---------------------------------------------------------------------------
-- Cubic solver (tower-native)
-- ---------------------------------------------------------------------------

towerSolveCubic ::
  [TPeriodState] ->
  Int ->
  TPeriodState ->
  [[Integer]] ->
  [Complex Double] ->
  [Complex Double] ->
  Integer ->
  ([TPeriodState], Int)
towerSolveCubic
  allPeriods
  nextId
  parent
  subPeriodElems
  subPeriodValues
  periodValues
  p =
    let e1 = tpElem parent
        -- e₂ = Σᵢ<ⱼ ηᵢηⱼ
        e2Num =
          subPeriodValues !! 0 * subPeriodValues !! 1
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
        qCoeff = negate e3 + e1 * e2 / TRat 3 - TRat (2 / 27) * e1 * e1 * e1

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
     in ( [ TPeriodState {tpElem = r, tpElems = es, tpP = p}
            | (r, es) <- zip roots subPeriodElems
          ],
          nextId + 3
        )

-- ---------------------------------------------------------------------------
-- Resolvent solver for q ≥ 5
-- ---------------------------------------------------------------------------

towerSolveResolvent ::
  Int ->
  [TPeriodState] ->
  Int ->
  TPeriodState ->
  [[Integer]] ->
  [Complex Double] ->
  [Complex Double] ->
  Integer ->
  ([TPeriodState], Int)
towerSolveResolvent
  q
  allPeriods
  nextId
  parent
  subPeriodElems
  subPeriodValues
  periodValues
  p
    | q == 5 =
        towerSolveQ5
          allPeriods
          nextId
          parent
          subPeriodElems
          subPeriodValues
          periodValues
          p
    | otherwise =
        towerSolveQGeneral
          q
          allPeriods
          nextId
          parent
          subPeriodElems
          subPeriodValues
          periodValues
          p

-- ---------------------------------------------------------------------------
-- Tower-native quintic resolvent (q = 5)
-- ---------------------------------------------------------------------------

-- | Tower-native resolvent for q = 5.
--
-- Builds ω₅ = cos(2π/5) + i·sin(2π/5) as a tower element using:
--   cos(2π/5) = (√5 - 1)/4
--   sin(2π/5) = √(10 + 2√5)/4
--
-- Then computes Lagrange resolvents R_j and recovers sub-periods via
-- inverse DFT, all in exact tower arithmetic.
towerSolveQ5 ::
  [TPeriodState] ->
  Int ->
  TPeriodState ->
  [[Integer]] ->
  [Complex Double] ->
  [Complex Double] ->
  Integer ->
  ([TPeriodState], Int)
towerSolveQ5
  allPeriods
  nextId
  parent
  subPeriodElems
  subPeriodValues
  periodValues
  p =
    let q = 5 :: Int

        -- Numerical DFT to get d_s coefficients
        omegaC = exp (0 :+ (2 * pi / 5)) :: Complex Double
        resolventVals =
          [ sum
              [ omegaC ^ ((j * k) `mod` q) * subPeriodValues !! k
                | k <- [0 .. q - 1]
              ]
            | j <- [0 .. q - 1]
          ]
        resolventPowers = [rv ^ q | rv <- resolventVals]
        dCoeffsNum =
          [ (1 / 5)
              * sum
                [ omegaC ^ (negate (j * s) `mod` q) * resolventPowers !! j
                  | j <- [0 .. q - 1]
                ]
            | s <- [0 .. q - 1]
          ]
        dCoeffs = map (matchToTowerElem allPeriods periodValues p) dCoeffsNum

        -- Build ω₅ as a tower element via square root adjunctions:
        --   √5, √(10 + 2√5), √(-1)
        (_, sqrt5) = adjoinTowerRoot nextId 2 (TRat 5)
        cos2pi5 = (sqrt5 - TRat 1) / TRat 4
        sinRadicand = TRat 10 + TRat 2 * sqrt5
        (_, sqrtSinRad) = adjoinTowerRoot (nextId + 1) 2 sinRadicand
        sin2pi5 = sqrtSinRad / TRat 4
        (_, iUnit) = adjoinTowerRoot (nextId + 2) 2 (TRat (-1))
        omega = cos2pi5 + iUnit * sin2pi5

        -- Powers ω₅^k, k = 0..4
        omPows = take q $ iterate (* omega) (TRat 1)

        -- R_j^5 = Σ_s d_s · ω₅^{js} as tower elements
        rjq =
          [ sum [dCoeffs !! s * omPows !! ((j * s) `mod` q) | s <- [0 .. q - 1]]
            | j <- [0 .. q - 1]
          ]

        -- R₀ = parent (known); adjoin ⁵√(R_j^5) for j = 1..4
        -- with branch selection via numerical matching.
        r0 = tpElem parent

        adjoinAndSelect nid j =
          let radicand = rjq !! j
              (_, alpha) = adjoinTowerRoot nid 5 radicand
              -- Five candidate roots: ω₅^m · α for m = 0..4
              candidates = [omPows !! m * alpha | m <- [0 .. q - 1]]
              target = resolventVals !! j
              candVals = map (`evalTowerApprox` p) candidates
              bestM =
                snd $
                  minimumBy
                    (comparing fst)
                    [ (magnitude (cv - target), m)
                      | (cv, m) <- zip candVals [0 :: Int ..]
                    ]
           in (candidates !! bestM, nid + 1)

        (r1, nid1) = adjoinAndSelect (nextId + 3) 1
        (r2, nid2) = adjoinAndSelect nid1 2
        (r3, nid3) = adjoinAndSelect nid2 3
        (r4, nid4) = adjoinAndSelect nid3 4

        rjs = [r0, r1, r2, r3, r4]

        -- Inverse DFT: η_k = (1/5) Σ_j ω₅^{-jk} · R_j
        subPeriodTower =
          [ TRat (1 / fromIntegral q)
              * sum
                [ omPows !! (negate (j * k) `mod` q) * rjs !! j
                  | j <- [0 .. q - 1]
                ]
            | k <- [0 .. q - 1]
          ]

        assigned = assignTowerByValue subPeriodTower subPeriodValues
     in ( [ TPeriodState {tpElem = r, tpElems = es, tpP = p}
            | (r, es) <- zip assigned subPeriodElems
          ],
          nid4
        )

-- ---------------------------------------------------------------------------
-- General resolvent solver for arbitrary prime q
-- ---------------------------------------------------------------------------

-- | Tower-native resolvent for general prime q.
--
-- Recursively uses 'allPeriodsViaTower' to build ω_q as a tower element,
-- then applies the same Lagrange resolvent pipeline as 'towerSolveQ5':
-- compute DFT coefficients, match to tower elements, reconstruct R_j^q,
-- adjoin q-th roots with branch selection, and recover sub-periods via
-- inverse DFT.
towerSolveQGeneral ::
  Int ->
  [TPeriodState] ->
  Int ->
  TPeriodState ->
  [[Integer]] ->
  [Complex Double] ->
  [Complex Double] ->
  Integer ->
  ([TPeriodState], Int)
towerSolveQGeneral
  q
  allPeriods
  nextId
  parent
  subPeriodElems
  subPeriodValues
  periodValues
  p =
    let -- Build all powers ω_q^k (k = 0..q-1) as tower elements by
        -- recursively using the tower descent machinery. allPeriodsViaTower q
        -- produces all ζ_q^k via Gauss period descent, so we use the
        -- precomputed map directly (avoiding expensive iterated multiplication).
        (omPows, nextId') = case allPeriodsViaTower q of
          Just tr ->
            let -- Count distinct levels across all period elements
                allTowerElems = Map.elems (trPeriods tr)
                nLevels = Set.size $ Set.unions $ map collectAllLevelIds allTowerElems
                renumber = renumberTowerIds nextId
                -- ω^0 = 1, ω^k = ζ_q^k from the period map
                pows =
                  [ if k == 0
                      then TRat 1
                      else case Map.lookup k (trPeriods tr) of
                        Just te -> renumber te
                        Nothing -> error $ "towerSolveQGeneral: missing ζ^" ++ show k
                    | k <- [0 .. q - 1]
                  ]
             in (pows, nextId + nLevels)
          Nothing ->
            let omega = fallbackOmegaTower q nextId
             in (take q $ iterate (* omega) (TRat 1), nextId + 1)

        -- Numerical DFT to get d_s coefficients
        omegaC = exp (0 :+ (2 * pi / fromIntegral q)) :: Complex Double
        resolventVals =
          [ sum
              [ omegaC ^ ((j * k) `mod` q) * subPeriodValues !! k
                | k <- [0 .. q - 1]
              ]
            | j <- [0 .. q - 1]
          ]
        resolventPowers = [rv ^ q | rv <- resolventVals]
        dCoeffsNum =
          [ (1 / fromIntegral q)
              * sum
                [ omegaC ^ (negate (j * s) `mod` q) * resolventPowers !! j
                  | j <- [0 .. q - 1]
                ]
            | s <- [0 .. q - 1]
          ]
        dCoeffs = map (matchToTowerElem allPeriods periodValues p) dCoeffsNum

        -- R_j^q = Σ_s d_s · ω_q^{js} as tower elements
        rjq =
          [ sum [dCoeffs !! s * omPows !! ((j * s) `mod` q) | s <- [0 .. q - 1]]
            | j <- [0 .. q - 1]
          ]

        -- R₀ = parent (known); adjoin q-th roots for j = 1..q-1
        r0 = tpElem parent

        adjoinAndSelect nid j =
          let radicand = rjq !! j
              (_, alpha) = adjoinTowerRoot nid q radicand
              -- q candidate roots: ω_q^m · α for m = 0..q-1
              candidates = [omPows !! m * alpha | m <- [0 .. q - 1]]
              target = resolventVals !! j
              candVals = map (`evalTowerApprox` p) candidates
              bestM =
                snd $
                  minimumBy
                    (comparing fst)
                    [ (magnitude (cv - target), m)
                      | (cv, m) <- zip candVals [0 :: Int ..]
                    ]
           in (candidates !! bestM, nid + 1)

        -- Adjoin all q-1 resolvent roots
        (rjRest, finalNid) =
          foldl
            (\(acc, nid) j ->
              let (rj, nid') = adjoinAndSelect nid j
               in (acc ++ [rj], nid'))
            ([], nextId')
            [1 .. q - 1]

        rjs = r0 : rjRest

        -- Inverse DFT: η_k = (1/q) Σ_j ω_q^{-jk} · R_j
        subPeriodTower =
          [ TRat (1 / fromIntegral q)
              * sum
                [ omPows !! (negate (j * k) `mod` q) * rjs !! j
                  | j <- [0 .. q - 1]
                ]
            | k <- [0 .. q - 1]
          ]

        assigned = assignTowerByValue subPeriodTower subPeriodValues
     in ( [ TPeriodState {tpElem = r, tpElems = es, tpP = p}
            | (r, es) <- zip assigned subPeriodElems
          ],
          finalNid
        )

-- | Shift all tower level IDs in a 'TowerElem' by a given offset.
-- This avoids ID collisions when embedding a tower element from a
-- recursive 'allPeriodsViaTower' call into an outer tower.
renumberTowerIds :: Int -> TowerElem -> TowerElem
renumberTowerIds _ t@(TRat _) = t
renumberTowerIds offset (TExt cs level) =
  let cs' = map (renumberTowerIds offset) cs
      radicand' = renumberTowerIds offset (tlRadicand level)
      level' = level { tlId = tlId level + offset, tlRadicand = radicand' }
   in TExt cs' level'

-- | Collect all distinct tower level IDs in a 'TowerElem'.
collectAllLevelIds :: TowerElem -> Set.Set Int
collectAllLevelIds (TRat _) = Set.empty
collectAllLevelIds (TExt cs level) =
  Set.insert (tlId level) $
    Set.unions (collectAllLevelIds (tlRadicand level) : map collectAllLevelIds cs)

-- | Count the number of distinct tower level IDs in a 'TowerElem'.
countTowerLevels :: TowerElem -> Int
countTowerLevels = Set.size . collectAllLevelIds

-- | Fallback ω_q as cos(2π/q) + i·sin(2π/q) using tower adjunctions
-- for cos and sin from rational approximations. Used when
-- allPeriodsViaGauss returns Nothing.
fallbackOmegaTower :: Int -> Int -> TowerElem
fallbackOmegaTower q nextId =
  let theta = 2 * pi / fromIntegral q
      cosApprox = TRat (toRational (cos theta :: Double))
      sinApprox = TRat (toRational (sin theta :: Double))
      (_, iUnit) = adjoinTowerRoot nextId 2 (TRat (-1))
   in cosApprox + iUnit * sinApprox

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
matchToTowerElem ::
  [TPeriodState] ->
  [Complex Double] ->
  Integer ->
  Complex Double ->
  TowerElem
matchToTowerElem periods periodValues _p target =
  let nearestInt = round (realPart target) :: Integer
      relTol err = err / max 1 (magnitude target) < 1e-6
   in if magnitude (target - (fromIntegral nearestInt :+ 0)) < 1e-8
        then TRat (fromIntegral nearestInt)
        else
          -- Try single-period matching first (fast, handles conjugates)
          let singleMatches =
                concatMap
                  ( \(i, pv) ->
                      case matchSinglePeriod target pv of
                        Just (c, a) ->
                          let err = magnitude (target - (fromIntegral c :+ 0) - (fromIntegral a :+ 0) * pv)
                           in [(i, c, a) | relTol err]
                        Nothing -> []
                  )
                  (zip [0 ..] periodValues)

              buildSingle (i, c, a) =
                TRat (fromIntegral c) + TRat (fromIntegral a) * tpElem (periods !! i)
           in case singleMatches of
                (m : _) -> buildSingle m
                [] ->
                  -- Fall back to multi-period matching
                  let tRat = (toRational (realPart target), toRational (imagPart target))
                      vRat = [(toRational (realPart v), toRational (imagPart v)) | v <- periodValues]
                   in case solveLinearIntegerC tRat vRat of
                        Just (c, coeffs) ->
                          foldl
                            (+)
                            (TRat (fromIntegral c))
                            [ if a == 0
                                then TRat 0
                                else TRat (fromIntegral a) * tpElem (periods !! i)
                              | (i, a) <- zip [0 ..] coeffs
                            ]
                        Nothing ->
                          -- Last resort: rational approximation
                          TRat (toRational (realPart target))

-- | Try to express target = c + a·v for integer c, a.
matchSinglePeriod :: Complex Double -> Complex Double -> Maybe (Int, Int)
matchSinglePeriod target v =
  let a
        | abs (imagPart v) > 1e-10 = round (imagPart target / imagPart v) :: Int
        | abs (realPart v) > 1e-10 = round (realPart target / realPart v)
        | otherwise = 0
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
        (e : es) ->
          fst $
            minimumBy
              (comparing (\(_, v) -> magnitude (v - target)))
              (e : es)

-- | Approximate numerical evaluation of a tower element as a 'Complex' 'Double'.
--
-- Recursively evaluates the tower structure: rational elements become
-- real values, and extension generators are evaluated as principal
-- nth roots of their radicands (via polar form). The @Integer@ argument
-- is the modulus p used to evaluate roots of unity in the radicand chain.
--
-- Used internally for branch selection and assignment matching during
-- the descent. Not suitable for high-precision computation.
evalTowerApprox :: TowerElem -> Integer -> Complex Double
evalTowerApprox (TRat r) _ = fromRational r :+ 0
evalTowerApprox (TExt cs level) p =
  let genVal = evalGenApprox level p
      coeffVals = map (`evalTowerApprox` p) cs
   in sum [cv * genVal ^ i | (cv, i) <- zip coeffVals [0 :: Int ..]]

-- | Approximate numerical value of a tower level's generator.
evalGenApprox :: TowerLevel -> Integer -> Complex Double
evalGenApprox level p =
  let rVal = evalTowerApprox (tlRadicand level) p
      n = tlRootDeg level
   in -- Principal n-th root
      mkPolar
        (magnitude rVal ** (1 / fromIntegral n))
        (phase rVal / fromIntegral n)

-- ---------------------------------------------------------------------------
-- Utilities (copied from Galois to avoid circular import)
-- ---------------------------------------------------------------------------

eulerTotient :: Integer -> Integer
eulerTotient n
  | n <= 0 = error "eulerTotient: non-positive"
  | n == 1 = 1
  | otherwise =
      let fs = factorise (fromInteger n :: Positive)
       in product [(pp - 1) * pp ^ (e - 1) | (pp, e) <- fs]

primitiveRootMod :: Integer -> Maybe Integer
primitiveRootMod n
  | n <= 0 = Nothing
  | n <= 2 = Just 1
  | n == 4 = Just 3
  | otherwise =
      let fs = factorise (fromInteger n :: Positive)
       in case fs of
            [(pp, _)] | pp > 2 -> findPrimRootMod n
            [(2, 1), (pp, _)] | pp > 2 -> findPrimRootMod n
            _ -> Nothing

findPrimRootMod :: Integer -> Maybe Integer
findPrimRootMod n =
  let phi = eulerTotient n
      factors = map fst (factorise (fromInteger phi :: Positive))
      isPrimRoot g =
        gcd g n == 1
          && all (\qq -> modExp g (phi `div` qq) n /= 1) factors
   in case filter isPrimRoot [2 .. n - 1] of
        [] -> Nothing
        (g : _) -> Just g

reorderFactors :: Integer -> [(Integer, Int)] -> [(Integer, Int)]
reorderFactors n fs =
  let nfs = factorise (fromInteger n :: Positive)
   in case nfs of
        [(pp, k)]
          | k > 1 ->
              let pFactor = [(pp', e) | (pp', e) <- fs, pp' == pp]
                  rest = [(qq, e) | (qq, e) <- fs, qq /= pp]
               in pFactor ++ rest
        _ -> fs

realPart :: Complex Double -> Double
realPart (r :+ _) = r

imagPart :: Complex Double -> Double
imagPart (_ :+ i) = i
