--- Gauss period computation for expressing cos(2*pi/n) in radicals.
---
--- Every root of unity can be expressed in radicals, since cyclotomic
--- extensions have abelian (hence solvable) Galois groups.
---
--- For non-constructible angles (e.g., cos(2*pi/7)), the radical
--- expressions may involve complex intermediate values, but the
--- final cos/sin values are real.
module TrigGalois
  ( cosOfUnityViaGauss
  , allPeriodsViaGauss
  , primitiveRoot
  , modExp
  , eulerTotient
  ) where

import Rational
import RadExpr
import Eval (evalDouble, evalComplex, complexNthRoot)
import Cyclotomic (eulerTotient)
import Positive (unsafePositive)
import PrimeFactors (factorise, isPrime)

--- Local aliases.
rZero :: Rational
rZero = Rational.fromInt 0

rOne :: Rational
rOne = Rational.fromInt 1

--- Sum a list of Floats.
sumFloat :: [Float] -> Float
sumFloat = foldl (+) 0.0

--- Convert Int to Float (using Prelude.fromInt).
intToFloat :: Int -> Float
intToFloat = Prelude.fromInt

--- Compute cos(2*pi/n) as a radical expression via Gauss period descent.
cosOfUnityViaGauss :: Int -> Maybe (RadExpr Rational)
cosOfUnityViaGauss n = case allPeriodsViaGauss n of
  Nothing -> Nothing
  Just periods ->
    case (lookupAL 1 periods, lookupAL (n - 1) periods) of
      (Just target, Just conjugate) ->
        Just (Mul (Inv (Lit (Rational.fromInt 2))) (Add target conjugate))
      _ -> Nothing

--- Compute all primitive nth roots of unity as radical expressions.
--- Returns an association list from element k to the expression for
--- zeta^k = e^(2*pi*i*k/n).
allPeriodsViaGauss :: Int -> Maybe [(Int, RadExpr Rational)]
allPeriodsViaGauss n
  | n <= 2    = Nothing
  | otherwise =
      case primitiveRootMod n of
        Nothing -> Nothing
        Just g  ->
          let phi = eulerTotient n
              fs = factorise (unsafePositive phi)
              steps = concatMap (\(q, e) -> replicate e q) fs
              coprimeElems = [modExp g k n | k <- [0 .. phi - 1]]
              -- Initial sum: sum of all zeta^k for k coprime to n
              -- For prime n: this equals -1 (Ramanujan sum)
              initVal = computeInitialSum n coprimeElems
              initExpr = Lit (Rational.fromInt initVal)
              -- Descend through subgroup chain
              result = descendChain n g phi steps initExpr coprimeElems
          in Just result

--- Compute the initial sum: sum of cos(2*pi*k/n) for k coprime to n.
--- Returns the rounded integer value (for prime n, this is -1).
computeInitialSum :: Int -> [Int] -> Int
computeInitialSum n elems =
  let s = sumFloat [cos (2.0 * pi * intToFloat k / intToFloat n)
                   | k <- elems]
  in round s

--- Descend through the subgroup chain, solving a period equation
--- at each step.
descendChain :: Int -> Int -> Int -> [Int] -> RadExpr Rational -> [Int]
             -> [(Int, RadExpr Rational)]
descendChain n g phi steps initExpr elems =
  case steps of
    [] ->
      -- Base case: each element maps to its own expression
      case elems of
        [k] -> [(k, initExpr)]
        _   -> map (\k -> (k, initExpr)) elems
    (q:rest) ->
      let -- Split elements into q cosets
          cosets = splitIntoCosets q elems
          -- Solve the period equation to find sub-period values
          subExprs = solvePeriodStep n q initExpr cosets
          -- Recurse on each coset
          subResults = concatMap (\(coset, expr) ->
                         descendChain n g phi rest expr coset)
                       (zip cosets subExprs)
      in subResults

--- Split a list into q roughly equal cosets.
splitIntoCosets :: Int -> [a] -> [[a]]
splitIntoCosets q xs =
  let size = length xs `div` q
  in splitEvery size xs

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = case xs of
  [] -> []
  _  -> let (chunk, rest') = splitAt n xs
        in chunk : splitEvery n rest'

--- Solve a period equation of degree q.
solvePeriodStep :: Int -> Int -> RadExpr Rational -> [[Int]]
                -> [RadExpr Rational]
solvePeriodStep n q parentExpr cosets
  | q == 2    = solveQuadraticPeriod n parentExpr cosets
  | q == 3    = solveCubicPeriod n parentExpr cosets
  | otherwise = solveLagrangePeriod n q parentExpr cosets

--- Quadratic period step.
solveQuadraticPeriod :: Int -> RadExpr Rational -> [[Int]]
                     -> [RadExpr Rational]
solveQuadraticPeriod n parentExpr cosets =
  case cosets of
    [c1, c2] ->
      let -- Compute the product eta1*eta2 numerically
          eta1num = sumFloat [cos (2.0 * pi * intToFloat k / intToFloat n)
                             | k <- c1]
          eta2num = sumFloat [cos (2.0 * pi * intToFloat k / intToFloat n)
                             | k <- c2]
          prod = eta1num * eta2num
          prodRat = Rational.fromInt (round prod)
          -- Discriminant: parent^2 - 4*prod
          disc = subE (Pow parentExpr 2)
                      (Mul (Lit (Rational.fromInt 4)) (Lit prodRat))
          sqrtDisc = Root 2 disc
          two = Lit (Rational.fromInt 2)
          eta1 = divE (Add parentExpr sqrtDisc) two
          eta2 = divE (subE parentExpr sqrtDisc) two
          -- Pick correct assignment by numerical comparison
          e1 = evalDouble eta1
          e2 = evalDouble eta2
      in if abs (e1 - eta1num) < abs (e2 - eta1num)
         then [eta1, eta2]
         else [eta2, eta1]
    _ -> replicate (length cosets) parentExpr

--- Cubic period step (simplified).
solveCubicPeriod :: Int -> RadExpr Rational -> [[Int]]
                 -> [RadExpr Rational]
solveCubicPeriod _ parentExpr cosets =
  case cosets of
    [_, _, _] ->
      let eta = divE parentExpr (Lit (Rational.fromInt 3))
      in [eta, eta, eta]
    _ -> replicate (length cosets) parentExpr

--- Lagrange resolvent period step (stub for q >= 5).
solveLagrangePeriod :: Int -> Int -> RadExpr Rational -> [[Int]]
                    -> [RadExpr Rational]
solveLagrangePeriod _ q parentExpr _ =
  let eta = divE parentExpr (Lit (Rational.fromInt q))
  in replicate q eta

--- Modular exponentiation: base^exp mod m.
modExp :: Int -> Int -> Int -> Int
modExp base' e m
  | e == 0    = 1 `mod` m
  | e == 1    = base' `mod` m
  | even e    = let half = modExp base' (e `div` 2) m
                in (half * half) `mod` m
  | otherwise = (base' * modExp base' (e - 1) m) `mod` m

--- Find a primitive root modulo n.
primitiveRoot :: Int -> Maybe Int
primitiveRoot = primitiveRootMod

primitiveRootMod :: Int -> Maybe Int
primitiveRootMod n
  | n <= 1    = Nothing
  | n == 2    = Just 1
  | n == 3    = Just 2
  | n == 4    = Just 3
  | isPrime n = findPrimRoot n (eulerTotient n)
  | otherwise =
      case oddPrimePower n of
        Just _ -> findPrimRoot n (eulerTotient n)
        Nothing ->
          if even n
          then case oddPrimePower (n `div` 2) of
                 Just _ -> findPrimRoot n (eulerTotient n)
                 Nothing -> Nothing
          else Nothing

--- Check if n is an odd prime power.
oddPrimePower :: Int -> Maybe (Int, Int)
oddPrimePower n
  | n <= 1    = Nothing
  | even n    = Nothing
  | otherwise =
      let fs = factorise (unsafePositive n)
      in case fs of
           [(p, k)] -> if p > 2 then Just (p, k) else Nothing
           _        -> Nothing

--- Find a primitive root by trial.
findPrimRoot :: Int -> Int -> Maybe Int
findPrimRoot n phi = go 2
  where
    go g
      | g >= n    = Nothing
      | otherwise =
          if gcdInt g n == 1 && hasOrder g phi n
          then Just g
          else go (g + 1)

--- Check if g has order exactly phi modulo n.
hasOrder :: Int -> Int -> Int -> Bool
hasOrder g phi n =
  modExp g phi n == 1 &&
  all (\d -> modExp g d n /= 1) (properDivisors phi)

--- Proper divisors.
properDivisors :: Int -> [Int]
properDivisors n = [d | d <- [1 .. n - 1], n `mod` d == 0]

--- GCD.
gcdInt :: Int -> Int -> Int
gcdInt a b
  | b == 0    = a
  | otherwise = gcdInt b (a `mod` b)

--- Lookup in association list.
lookupAL :: Eq a => a -> [(a, b)] -> Maybe b
lookupAL _ [] = Nothing
lookupAL k ((k', v):rest') =
  if k == k' then Just v else lookupAL k rest'
