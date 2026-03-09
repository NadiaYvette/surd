-- | Conversion between radical expressions and algebraic numbers.
--
-- Forward direction (RadExpr → AlgNum): compute the minimal polynomial
-- of a radical expression and isolate the correct root.
--
-- Backward direction (AlgNum → RadExpr): given a simplified algebraic
-- number (low-degree minimal polynomial), express it as a radical.
--
-- The round-trip RadExpr → AlgNum → RadExpr is the simplification
-- pipeline: complex nested radical expressions may simplify to lower-degree
-- algebraic numbers with simpler radical representations.
module Surd.Algebraic.Convert
  ( radExprToAlgNum
  , algNumToRadExpr
  , simplifyViaCanonical
  , algNumInfo
  ) where

import Data.Ratio (numerator, denominator)
import Surd.Types
import Surd.Polynomial.Univariate
import Surd.Radical.LaTeX (latex)
import Surd.Polynomial.MinimalPoly (minimalPoly, annihilatingPoly)
import Surd.Polynomial.MinimalPolyTower (minimalPolyTower)
import Surd.Polynomial.Factoring (rationalRoots)
import Surd.Radical.Eval (evalComplex, evalExact, evalComplexExact, ExactReal)
import Surd.Radical.Normalize (normalize)
import Data.Complex (realPart)
import Surd.Algebraic.Number
import Surd.Internal.Interval (Interval(..))

-- | Convert a radical expression to its canonical algebraic number form.
--
-- Computes the minimal polynomial and isolates the correct root using
-- numerical evaluation to pick the right factor.
radExprToAlgNum :: RadExpr Rational -> AlgNum
radExprToAlgNum expr =
  let -- Try tower-based approach first (fast for shared radicals)
      mp = minimalPolyTower expr
      approx = realPart (evalComplex expr)
  in case algFromPoly mp approx of
       Just a  -> a
       Nothing ->
         -- Fallback: use the old resultant-based approach
         let mp2 = minimalPoly expr
         in case algFromPoly mp2 approx of
              Just a  -> a
              Nothing ->
                let ann = annihilatingPoly expr
                in case algFromPoly ann approx of
                     Just a  -> a
                     Nothing ->
                       AlgNum mp (Interval (toRational (approx - 1)) (toRational (approx + 1)))

-- | Convert an algebraic number back to a radical expression,
-- if the degree is small enough for closed-form solutions.
--
-- Handles:
--   degree 1: rational number
--   degree 2: quadratic formula
--   degree 3: Cardano's formula (real case only for now)
--   degree 4: Ferrari's formula
--
-- Returns Nothing if the degree is too high or the formula
-- would require complex intermediates we can't simplify.
algNumToRadExpr :: AlgNum -> Maybe (RadExpr Rational)
algNumToRadExpr a =
  let p = anMinPoly a
      d = degree p
      cs = unPoly p
      approx = fromRational (algApprox (1/(10^(20 :: Int))) a) :: ExactReal
  in case d of
       1 -> case cs of
              [c0, _] -> Just (Lit (-c0))
              _       -> Nothing
       2 -> solveQuadratic cs approx
       3 -> solveCubic cs approx
       4 -> solveQuartic cs approx
       _ -> Nothing

-- | Simplify a radical expression by converting to canonical form
-- and back. If the algebraic number has a simpler radical representation,
-- return it; otherwise return the original.
simplifyViaCanonical :: RadExpr Rational -> RadExpr Rational
simplifyViaCanonical expr =
  let algNum = radExprToAlgNum expr
  in case algNumToRadExpr algNum of
       Just simplified -> normalize simplified
       Nothing         -> expr  -- can't simplify further

-- | Get a human-readable summary of the algebraic number form
-- of a radical expression: minimal polynomial, degree, approximate value.
algNumInfo :: RadExpr Rational -> String
algNumInfo expr =
  let algNum = radExprToAlgNum expr
      mp = anMinPoly algNum
      d = degree mp
      approx = algApprox (1/(10^(10 :: Int))) algNum
      approxD = fromRational approx :: Double
  in unlines
       [ "Minimal polynomial: " ++ showPoly mp
       , "Degree: " ++ show d
       , "Approximate value: " ++ show approxD
       , case algNumToRadExpr algNum of
           Just e  -> let simplified = normalize e
                      in "Radical form: " ++ show simplified
                         ++ "\nLaTeX: " ++ latex simplified
           Nothing -> "Radical form: (degree too high)"
       ]

-- Internal

-- | Solve ax² + bx + c = 0 (monic: a=1), picking the root closest to approx.
solveQuadratic :: [Rational] -> ExactReal -> Maybe (RadExpr Rational)
solveQuadratic [c, b, _a] approx =
  -- x = (-b ± √(b²-4c)) / 2   (since a=1, polynomial is x² + bx + c)
  let disc = b*b - 4*c
  in if disc < 0
     then Nothing  -- complex roots (polynomial has no real roots)
     else
       let sqrtDisc = Root 2 (Lit disc)
           r1 = Mul (Inv (Lit 2)) (Add (Neg (Lit b)) sqrtDisc)
           r2 = Mul (Inv (Lit 2)) (Add (Neg (Lit b)) (Neg sqrtDisc))
           v1 = evalExact r1
           v2 = evalExact r2
       in Just $ if abs (v1 - approx) < abs (v2 - approx) then r1 else r2
solveQuadratic _ _ = Nothing

-- | Solve x³ + px + q = 0 (depressed cubic), picking the real root
-- closest to approx. Uses Cardano when discriminant ≥ 0.
solveCubic :: [Rational] -> ExactReal -> Maybe (RadExpr Rational)
solveCubic [d, c, b, _a] _approx =
  -- Polynomial is x³ + bx² + cx + d (monic)
  -- Depress: substitute x = t - b/3
  let p = c - b*b/3
      q = d - b*c/3 + 2*b*b*b/27
      disc = -(4*p*p*p + 27*q*q)
      shift = Neg (Lit (b/3))
  in if disc >= 0
     then
       -- Three real roots (or repeated). Try rational roots first.
       let poly = mkPoly [d, c, b, 1]
           rats = rationalRoots poly
       in case rats of
            (r:_) -> Just (Lit r)
            []    ->
              -- Casus irreducibilis: Cardano with complex intermediates.
              -- The formula gives real roots as Re(∛(complex)) + Re(∛(conj)).
              -- Use Cardano: u³ = -q/2 + √(q²/4 + p³/27) where p and q
              -- are from the depressed cubic t³ + pt + q = 0.
              let halfQ = q / 2
                  innerDisc = halfQ * halfQ + p*p*p / 27
                  -- innerDisc < 0 in casus irreducibilis
                  sqrtNegD = Root 2 (Lit (negate innerDisc))
                  -- u³ = -q/2 + i·√(-innerDisc)
                  -- Cube root radicand: -q/2 ± √(innerDisc)
                  -- Since innerDisc < 0, √(innerDisc) = i·√(-innerDisc)
                  -- We express: u = ∛(-q/2 + i·√(-innerDisc))
                  -- Then root = u + ū + shift = 2·Re(u) - b/3
                  -- Re(u) = Re(∛(complex))
                  -- Use: u = ∛(-q/2 + i·√|disc|) where disc = q²/4 + p³/27
                  -- u + v = u + conjugate(u) = 2·Re(u)
                  -- We express as:  ∛(a + b·√(-D)) + ∛(a - b·√(-D)) - b/3
                  -- where a = -q/2, b·√(-D) = √(innerDisc)
                  cubeRadPos = Add (Neg (Lit halfQ)) (Mul (Lit (1/1)) sqrtNegD)
                  cubeRadNeg = Add (Neg (Lit halfQ)) (Neg sqrtNegD)
                  u1 = Root 3 cubeRadPos
                  u2 = Root 3 cubeRadNeg
                  -- All three roots: u1+u2-b/3, ω·u1+ω²·u2-b/3, ω²·u1+ω·u2-b/3
                  -- where ω = (-1+i√3)/2
                  -- But u2 = -p/(3·u1) (product relation)
                  -- Use numerical eval to pick the right root
                  root0 = Add (Add u1 u2) shift
                  -- For the other two roots, use ω multiplication
                  -- ω = (-1 + √(-3))/2, ω² = (-1 - √(-3))/2
                  omega = Mul (Inv (Lit 2)) (Add (Lit (-1)) (Root 2 (Lit (-3))))
                  omega2 = Mul (Inv (Lit 2)) (Add (Lit (-1)) (Neg (Root 2 (Lit (-3)))))
                  root1 = Add (Add (Mul omega u1) (Mul omega2 u2)) shift
                  root2 = Add (Add (Mul omega2 u1) (Mul omega u2)) shift
                  roots = [root0, root1, root2]
                  -- Evaluate all roots via exact complex and pick closest to approx
                  scored = [(r, abs (realPart (evalComplexExact r) - _approx)) | r <- roots]
                  best = fst $ foldl1 (\a' b' -> if snd a' <= snd b' then a' else b') scored
              in Just best
     else
       -- One real root, two complex conjugates. Cardano works directly.
       let halfQ = q / 2
           innerDisc = halfQ * halfQ + p*p*p / 27
           sqrtD = Root 2 (Lit innerDisc)
           u = Root 3 (Add (Neg (Lit halfQ)) sqrtD)
           v = Root 3 (Add (Neg (Lit halfQ)) (Neg sqrtD))
           root = Add (Add u v) shift
       in Just root
solveCubic _ _ = Nothing

-- | Solve x⁴ + ax³ + bx² + cx + d = 0 (monic quartic) via Ferrari's method.
--
-- 1. Depress: substitute x = t - a/4 to get t⁴ + pt² + qt + r = 0
-- 2. Solve the resolvent cubic for y
-- 3. Factor the depressed quartic into two quadratics using y
-- 4. Solve each quadratic, pick the root closest to approx
solveQuartic :: [Rational] -> ExactReal -> Maybe (RadExpr Rational)
solveQuartic [e, d, c, b, _a] approx =
  -- Polynomial is x⁴ + bx³ + cx² + dx + e (monic)
  -- Depress: x = t - b/4
  let p = c - 3*b*b/8
      q = d - b*c/2 + b*b*b/8
      r = e - b*d/4 + b*b*c/16 - 3*b*b*b*b/256
      shiftBack expr = Add expr (Neg (Lit (b/4)))
  in if q == 0
     then -- Biquadratic: t⁴ + pt² + r = 0, solve as quadratic in t²
       let disc = p*p - 4*r
       in if disc < 0
          then Nothing
          else
            let sqrtDisc = Root 2 (Lit disc)
                t2_1 = Mul (Inv (Lit 2)) (Add (Neg (Lit p)) sqrtDisc)
                t2_2 = Mul (Inv (Lit 2)) (Add (Neg (Lit p)) (Neg sqrtDisc))
                -- t = ±√(t²)
                roots = [ shiftBack (Root 2 t2_1)
                        , shiftBack (Neg (Root 2 t2_1))
                        , shiftBack (Root 2 t2_2)
                        , shiftBack (Neg (Root 2 t2_2))
                        ]
                scored = [(root, abs (realPart (evalComplexExact root) - approx)) | root <- roots]
                best = fst $ foldl1 (\x y -> if snd x <= snd y then x else y) scored
            in Just best
     else
       -- General case: solve resolvent cubic y³ - py² - 4ry + (4pr - q²) = 0
       let rcCs = [4*p*r - q*q, -4*r, -p, 1]
           -- Use solveCubic to get a resolvent root
           -- We need any root, so use a dummy approx (we'll try all later)
       in case solveCubic rcCs 0 of
            Nothing -> Nothing
            Just yExpr ->
              -- Evaluate y via exact real
              let y = realPart (evalComplexExact yExpr)
                  -- y² - 4r should be non-negative for the factoring to work
                  -- If not, try another resolvent root
                  disc2 = y*y - 4 * fromRational r
              in if disc2 < -1e-40
                 then trySolveQuarticAlternate p q r b approx
                 else solveQuarticWithY p q r b yExpr approx
solveQuartic _ _ = Nothing

-- | Given the depressed quartic t⁴ + pt² + qt + r = 0 and a resolvent root y,
-- factor into two quadratics and solve.
solveQuarticWithY :: Rational -> Rational -> Rational -> Rational
                  -> RadExpr Rational -> ExactReal
                  -> Maybe (RadExpr Rational)
solveQuarticWithY p q _r b yExpr approx =
  -- We need s² = y - p, so s = √(y - p)
  -- Then the quartic factors as:
  -- (t² + s·t + (y/2 - q/(2s)))(t² - s·t + (y/2 + q/(2s))) = 0
  --
  -- In radical expressions:
  let sSquared = Add yExpr (Neg (Lit p))  -- y - p
      s = Root 2 sSquared                  -- √(y - p)
      halfY = Mul (Inv (Lit 2)) yExpr     -- y/2
      -- q/(2s) = q / (2·√(y-p))
      qOver2s = Mul (Lit (q/2)) (Inv s)   -- q/(2·√(y-p))
      -- Quadratic 1: t² + s·t + (y/2 - q/(2s)) = 0
      -- roots: t = (-s ± √(s² - 4(y/2 - q/(2s)))) / 2
      c1 = Add halfY (Neg qOver2s)         -- y/2 - q/(2s)
      -- Quadratic 2: t² - s·t + (y/2 + q/(2s)) = 0
      -- roots: t = (s ± √(s² - 4(y/2 + q/(2s)))) / 2
      c2 = Add halfY qOver2s               -- y/2 + q/(2s)

      -- Discriminants
      disc1 = Add sSquared (Mul (Lit (-4)) c1)  -- s² - 4c1 = (y-p) - 4(y/2 - q/(2s))
      disc2 = Add sSquared (Mul (Lit (-4)) c2)  -- s² - 4c2

      sqD1 = Root 2 disc1
      sqD2 = Root 2 disc2

      shift expr = Add expr (Neg (Lit (b/4)))

      -- Four roots (before un-depressing):
      -- t1,t2 from quadratic 1: t = (-s ± √disc1) / 2
      -- t3,t4 from quadratic 2: t = (s ± √disc2) / 2
      t1 = Mul (Inv (Lit 2)) (Add (Neg s) sqD1)
      t2 = Mul (Inv (Lit 2)) (Add (Neg s) (Neg sqD1))
      t3 = Mul (Inv (Lit 2)) (Add s sqD2)
      t4 = Mul (Inv (Lit 2)) (Add s (Neg sqD2))

      roots = map shift [t1, t2, t3, t4]

      -- Pick the root closest to the target
      scored = [(root, abs (realPart (evalComplexExact root) - approx)) | root <- roots]
      best = fst $ foldl1 (\x y -> if snd x <= snd y then x else y) scored
  in Just best

-- | Alternative quartic solving when the first resolvent root gives y < p.
-- Try all three resolvent roots.
trySolveQuarticAlternate :: Rational -> Rational -> Rational -> Rational -> ExactReal
                        -> Maybe (RadExpr Rational)
trySolveQuarticAlternate p q r b approx =
  -- Resolvent cubic: y³ - py² - 4ry + (4pr - q²) = 0
  let rcCs = [4*p*r - q*q, -4*r, -p, 1]
      poly = mkPoly rcCs
      -- Try rational roots first
      rats = rationalRoots poly
  in case rats of
       (rat:_) ->
         let yExpr = Lit rat
         in solveQuarticWithY p q r b yExpr approx
       [] ->
         -- Get all three cubic roots and try each
         let approxRoots = [0, 10, -10] :: [ExactReal]
             attempts = [do yE <- solveCubic rcCs a
                            solveQuarticWithY p q r b yE approx
                        | a <- approxRoots]
         in case [r' | Just r' <- attempts] of
              (x:_) -> Just x
              []    -> Nothing

-- | Display a polynomial in a readable format.
showPoly :: Poly Rational -> String
showPoly (Poly []) = "0"
showPoly (Poly cs) =
  let terms = [(i, c) | (i, c) <- zip [0..] cs, c /= 0]
  in case terms of
       [] -> "0"
       _  -> unwords $ zipWith (\idx (i, c) -> showTerm idx i c) [0..] terms
  where
    showTerm :: Int -> Int -> Rational -> String
    showTerm termIdx power coeff =
      let sign | termIdx == 0 && coeff < 0 = "-"
               | termIdx == 0              = ""
               | coeff < 0                 = "- "
               | otherwise                 = "+ "
          ac = abs coeff
          coeffStr | ac == 1 && power > 0 = ""
                   | denominator ac == 1  = show (numerator ac)
                   | otherwise            = show (numerator ac) ++ "/" ++ show (denominator ac)
          varStr | power == 0 = if coeffStr == "" then "1" else coeffStr
                 | power == 1 = coeffStr ++ "x"
                 | otherwise  = coeffStr ++ "x^" ++ show power
      in sign ++ varStr
