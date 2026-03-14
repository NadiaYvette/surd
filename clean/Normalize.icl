implementation module Normalize

import StdEnv
import Data.Integer
import RadExpr
import Rational
import Positive
import PrimeFactors
from Data.Map import :: Map(..), newMap, get, put, foldrWithKey

// Helper: convert Map to sorted association list
mapToList :: !(Map k v) -> [(k, v)]
mapToList m = foldrWithKey (\k v acc -> [(k, v) : acc]) [] m

// ---------------------------------------------------------------------------
// Fixed-point iteration
// ---------------------------------------------------------------------------

fixN :: !Int ((RadExpr Rational) -> RadExpr Rational) !(RadExpr Rational) -> RadExpr Rational
fixN 0 _ x = x
fixN n f x
    # x` = f x
    | x` == x = x
    = fixN (n - 1) f x`

normalize :: !(RadExpr Rational) -> RadExpr Rational
normalize expr = fixN 10 normalizeOnce expr

normalizeOnce :: !(RadExpr Rational) -> RadExpr Rational
normalizeOnce expr
    = collectTerms
        (collectCoefficients
            (distribute
                (sortCommutative
                    (extractPerfectPowers
                        (simplifyPowers
                            (foldConstants
                                (flattenArith expr)))))))

// ---------------------------------------------------------------------------
// flattenArith: cancel double negations and double inverses
// ---------------------------------------------------------------------------

flattenArith :: !(RadExpr k) -> RadExpr k
flattenArith (Neg (Neg a)) = flattenArith a
flattenArith (Neg a) = Neg (flattenArith a)
flattenArith (Add a b) = Add (flattenArith a) (flattenArith b)
flattenArith (Mul a b) = Mul (flattenArith a) (flattenArith b)
flattenArith (Inv (Inv a)) = flattenArith a
flattenArith (Inv a) = Inv (flattenArith a)
flattenArith (Root n a) = Root n (flattenArith a)
flattenArith (Pow a n) = Pow (flattenArith a) n
flattenArith e = e

// ---------------------------------------------------------------------------
// foldConstants: evaluate pure-literal subtrees
// ---------------------------------------------------------------------------

foldConstants :: !(RadExpr Rational) -> RadExpr Rational
foldConstants (Lit r) = Lit r
foldConstants (Neg a) = case foldConstants a of
    Lit r   -> Lit (~ r)
    Neg a`  -> a`
    a`      -> Neg a`
foldConstants (Add a b) = foldConstantsAdd (foldConstants a) (foldConstants b)
foldConstants (Mul a b) = foldConstantsMul (foldConstants a) (foldConstants b)
foldConstants (Inv a) = case foldConstants a of
    Lit r  | not (r == zero) -> Lit (one / r)
    Inv a` -> a`
    a`     -> Inv a`
foldConstants (Root n a) = case foldConstants a of
    Lit r | r == zero -> Lit zero
    Lit r | r == one  -> Lit one
    a`    -> Root n a`
foldConstants (Pow a n) = foldConstantsPow (foldConstants a) n

foldConstantsAdd :: !(RadExpr Rational) !(RadExpr Rational) -> RadExpr Rational
foldConstantsAdd (Lit r) (Lit s) = Lit (r + s)
foldConstantsAdd (Lit r) b | r == zero = b
foldConstantsAdd a (Lit s) | s == zero = a
foldConstantsAdd a (Neg b) | a == b = Lit zero
foldConstantsAdd a b = Add a b

foldConstantsMul :: !(RadExpr Rational) !(RadExpr Rational) -> RadExpr Rational
foldConstantsMul (Lit r) (Lit s) = Lit (r * s)
foldConstantsMul (Lit r) _ | r == zero = Lit zero
foldConstantsMul _ (Lit s) | s == zero = Lit zero
foldConstantsMul (Lit r) b | r == one  = b
foldConstantsMul a (Lit s) | s == one  = a
foldConstantsMul (Lit r) b | r == ~ one = Neg b
foldConstantsMul a (Lit s) | s == ~ one = Neg a
foldConstantsMul a b = Mul a b

foldConstantsPow :: !(RadExpr Rational) !Int -> RadExpr Rational
foldConstantsPow (Lit r) n = Lit (ratPow r n)
foldConstantsPow a 0 = Lit one
foldConstantsPow a 1 = a
foldConstantsPow a n = Pow a n

// ---------------------------------------------------------------------------
// simplifyPowers
// ---------------------------------------------------------------------------

simplifyPowers :: !(RadExpr Rational) -> RadExpr Rational
simplifyPowers (Mul (Root 2 a) (Root 2 b))
    | a == b = simplifyPowers a
simplifyPowers (Pow (Pow a m) n) = simplifyPowers (Pow a (m * n))
simplifyPowers (Pow (Root n a) m)
    | m == n = simplifyPowers a
simplifyPowers (Root n (Pow a m))
    | m == n = simplifyPowers a
simplifyPowers (Root m (Root n a)) = Root (m * n) (simplifyPowers a)
simplifyPowers (Neg a) = Neg (simplifyPowers a)
simplifyPowers (Add a b) = Add (simplifyPowers a) (simplifyPowers b)
simplifyPowers (Mul a b) = Mul (simplifyPowers a) (simplifyPowers b)
simplifyPowers (Inv a) = Inv (simplifyPowers a)
simplifyPowers (Root n a) = Root n (simplifyPowers a)
simplifyPowers (Pow a n) = Pow (simplifyPowers a) n
simplifyPowers e = e

// ---------------------------------------------------------------------------
// extractPerfectPowers
// ---------------------------------------------------------------------------

extractPerfectPowers :: !(RadExpr Rational) -> RadExpr Rational
extractPerfectPowers (Root n (Lit r))
    | r == zero = Lit zero
    | zero < r
        # numI = toInt (numer r)
        # denI = toInt (denom r)
        # (numOut, numIn) = extractNthPower n numI
        # (denOut, denIn) = extractNthPower n denI
        # (outerCoeff, innerRat) = rationalizeRoot n numOut numIn denOut denIn
        = buildRootResult n outerCoeff innerRat
    | r < zero && isOdd n
        = Neg (extractPerfectPowers (Root n (Lit (~ r))))
    = Root n (Lit r)
extractPerfectPowers (Root n a) = Root n (extractPerfectPowers a)
extractPerfectPowers (Neg a) = Neg (extractPerfectPowers a)
extractPerfectPowers (Add a b) = Add (extractPerfectPowers a) (extractPerfectPowers b)
extractPerfectPowers (Mul a b) = Mul (extractPerfectPowers a) (extractPerfectPowers b)
extractPerfectPowers (Inv a) = Inv (extractPerfectPowers a)
extractPerfectPowers (Pow a n) = Pow (extractPerfectPowers a) n
extractPerfectPowers e = e

rationalizeRoot :: !Int !Rational !Rational !Rational !Rational -> (Rational, Rational)
rationalizeRoot n numOut numIn denOut denIn
    | denIn == one = (numOut / denOut, numIn)
    # newInner = numIn * ratPow denIn (n - 1)
    # newOuter = numOut / (denOut * denIn)
    // Re-extract in case denIn^(n-1) introduced new powers
    # newInnerI = toInt (numer newInner)
    # (numOut2, numIn2) = extractNthPower n newInnerI
    = (newOuter * numOut2, numIn2)

buildRootResult :: !Int !Rational !Rational -> RadExpr Rational
buildRootResult n outerCoeff innerRat
    | outerCoeff == one && innerRat == one = Lit one
    | outerCoeff == one = Root n (Lit innerRat)
    | innerRat == one = Lit outerCoeff
    = Mul (Lit outerCoeff) (Root n (Lit innerRat))

// Extract the largest nth power dividing a positive integer.
// Returns (extracted, remainder) such that m = extracted^n * remainder.
extractNthPower :: !Int !Int -> (Rational, Rational)
extractNthPower _ m
    | m <= 0 = (one, ratFromInt (abs m))
extractNthPower n m
    # fs = factorise (unsafePositive m)
    # extracted = prodList [p ^ (e / n) \\ (p, e) <- fs]
    # remainder = prodList [p ^ (e rem n) \\ (p, e) <- fs]
    = (ratFromInt extracted, ratFromInt remainder)

prodList :: ![Int] -> Int
prodList [] = 1
prodList [x:xs] = x * prodList xs

// ---------------------------------------------------------------------------
// collectCoefficients: flatten Mul chains, merge Lit factors
// ---------------------------------------------------------------------------

collectCoefficients :: !(RadExpr Rational) -> RadExpr Rational
collectCoefficients expr = goCC expr

goCC :: !(RadExpr Rational) -> RadExpr Rational
goCC e=:(Mul _ _)
    # factors = flattenMulCC e
    # processed = map goCC factors
    # (lits, rest) = partitionLits processed
    # coeff = prodRat lits
    # body = buildMulCC rest
    = applyCoeffMul coeff body
goCC (Neg a) = case goCC a of
    Lit r -> Lit (~ r)
    a`    -> Neg a`
goCC (Add a b) = Add (goCC a) (goCC b)
goCC (Inv a) = Inv (goCC a)
goCC (Root n a) = Root n (goCC a)
goCC (Pow a n) = Pow (goCC a) n
goCC e = e

flattenMulCC :: !(RadExpr Rational) -> [RadExpr Rational]
flattenMulCC (Mul a b) = flattenMulCC a ++ flattenMulCC b
flattenMulCC e = [e]

partitionLits :: ![RadExpr Rational] -> ([Rational], [RadExpr Rational])
partitionLits [] = ([], [])
partitionLits [Lit r : xs]
    # (ls, rs) = partitionLits xs
    = ([r : ls], rs)
partitionLits [Inv (Lit r) : xs]
    | not (r == zero)
        # (ls, rs) = partitionLits xs
        = ([one / r : ls], rs)
partitionLits [x : xs]
    # (ls, rs) = partitionLits xs
    = (ls, [x : rs])

prodRat :: ![Rational] -> Rational
prodRat [] = one
prodRat [x:xs] = x * prodRat xs

buildMulCC :: ![RadExpr Rational] -> RadExpr Rational
buildMulCC [] = Lit one
buildMulCC [x] = x
buildMulCC [x:xs] = Mul x (buildMulCC xs)

applyCoeffMul :: !Rational !(RadExpr Rational) -> RadExpr Rational
applyCoeffMul c _     | c == zero   = Lit zero
applyCoeffMul c body  | c == one    = body
applyCoeffMul c body  | c == ~ one  = Neg body
applyCoeffMul c (Lit r) = Lit (c * r)
applyCoeffMul c body = Mul (Lit c) body

// ---------------------------------------------------------------------------
// collectTerms: group like terms in sums, combine coefficients
// ---------------------------------------------------------------------------

collectTerms :: !(RadExpr Rational) -> RadExpr Rational
collectTerms expr = goCT expr

goCT :: !(RadExpr Rational) -> RadExpr Rational
goCT e=:(Add _ _)
    # terms = flattenAddCT e
    # processed = map goCT terms
    # grouped = groupTerms processed
    # rebuilt = [applyCoeffAdd c base \\ (base, c) <- grouped | not (c == zero)]
    = buildAddCT rebuilt
goCT (Neg a) = Neg (goCT a)
goCT (Mul a b) = Mul (goCT a) (goCT b)
goCT (Inv a) = Inv (goCT a)
goCT (Root n a) = Root n (goCT a)
goCT (Pow a n) = Pow (goCT a) n
goCT e = e

flattenAddCT :: !(RadExpr Rational) -> [RadExpr Rational]
flattenAddCT (Add a b) = flattenAddCT a ++ flattenAddCT b
flattenAddCT e = [e]

// Split a term into (coefficient, base).
splitCoeff :: !(RadExpr Rational) -> (Rational, RadExpr Rational)
splitCoeff (Mul (Lit c) body) = (c, body)
splitCoeff (Neg e)
    # (c, b) = splitCoeff e
    = (~ c, b)
splitCoeff (Lit r) = (r, Lit one)
splitCoeff e = (one, e)

// Group terms by base, summing coefficients.
// PERFORMANCE NOTE: The accumulator Map is used linearly (each intermediate
// result consumed once by the next foldl step). With a unique-capable map
// (*Map), the put operations would be destructive in-place updates.
groupTerms :: ![RadExpr Rational] -> [(RadExpr Rational, Rational)]
groupTerms terms = mapToList (foldl addTerm newMap terms)
where
    addTerm :: !(Map (RadExpr Rational) Rational) !(RadExpr Rational) -> Map (RadExpr Rational) Rational
    addTerm m term
        # (c, base) = splitCoeff term
        = case get base m of
            ?Just old -> put base (old + c) m
            ?None     -> put base c m

applyCoeffAdd :: !Rational !(RadExpr Rational) -> RadExpr Rational
applyCoeffAdd c (Lit l) | l == one = Lit c
applyCoeffAdd c body    | c == one = body
applyCoeffAdd c body    | c == ~ one = Neg body
applyCoeffAdd c body = Mul (Lit c) body

buildAddCT :: ![RadExpr Rational] -> RadExpr Rational
buildAddCT [] = Lit zero
buildAddCT [x] = x
buildAddCT [x:xs] = foldl Add x xs

// ---------------------------------------------------------------------------
// sortCommutative: canonical ordering of Add/Mul children
// ---------------------------------------------------------------------------

sortCommutative :: !(RadExpr Rational) -> RadExpr Rational
sortCommutative expr = goSC expr

goSC :: !(RadExpr Rational) -> RadExpr Rational
goSC e=:(Add _ _)
    # terms = flattenAddSC e
    # sorted = sort (map goSC terms)
    = buildAddSC sorted
goSC e=:(Mul _ _)
    # factors = flattenMulSC e
    # sorted = sort (map goSC factors)
    = buildMulSC sorted
goSC (Neg a) = Neg (goSC a)
goSC (Inv a) = Inv (goSC a)
goSC (Root n a) = Root n (goSC a)
goSC (Pow a n) = Pow (goSC a) n
goSC e = e

flattenAddSC :: !(RadExpr Rational) -> [RadExpr Rational]
flattenAddSC (Add a b) = flattenAddSC a ++ flattenAddSC b
flattenAddSC e = [e]

flattenMulSC :: !(RadExpr Rational) -> [RadExpr Rational]
flattenMulSC (Mul a b) = flattenMulSC a ++ flattenMulSC b
flattenMulSC e = [e]

buildAddSC :: ![RadExpr Rational] -> RadExpr Rational
buildAddSC [] = Lit zero
buildAddSC [x] = x
buildAddSC [x:xs] = foldl Add x xs

buildMulSC :: ![RadExpr Rational] -> RadExpr Rational
buildMulSC [] = Lit one
buildMulSC [x] = x
buildMulSC [x:xs] = foldl Mul x xs

// ---------------------------------------------------------------------------
// distribute: scalar multiplication over addition
// ---------------------------------------------------------------------------

distribute :: !(RadExpr Rational) -> RadExpr Rational
distribute expr = goD expr

goD :: !(RadExpr Rational) -> RadExpr Rational
// Lit * (a + b + ...) -> Lit*a + Lit*b + ...
goD (Mul (Lit c) r)
    | isSum r
        # terms = flattenS r
        = rebuildAddNE (map (\t -> Mul (Lit c) t) terms)
goD (Mul l (Lit c))
    | isSum l
        # terms = flattenS l
        = rebuildAddNE (map (\t -> Mul t (Lit c)) terms)
// (a + b) * x -> a*x + b*x (when sum is tiny)
goD (Mul l r)
    | isTinySum l && not (isSum r)
        # terms = flattenS l
        = rebuildAddNE (map (\t -> goD (Mul t r)) terms)
goD (Mul l r)
    | not (isSum l) && isTinySum r
        # terms = flattenS r
        = rebuildAddNE (map (\t -> goD (Mul l t)) terms)
// c * (-a) -> -(c*a)
goD (Mul (Lit c) (Neg a)) = Neg (goD (Mul (Lit c) a))
goD (Mul (Neg a) (Lit c)) = Neg (goD (Mul a (Lit c)))
// Recurse
goD (Neg a) = Neg (goD a)
goD (Add a b) = Add (goD a) (goD b)
goD (Mul a b) = Mul (goD a) (goD b)
goD (Inv a) = Inv (goD a)
goD (Root n a) = Root n (goD a)
goD (Pow a n) = Pow (goD a) n
goD e = e

isSum :: !(RadExpr k) -> Bool
isSum (Add _ _) = True
isSum (Neg a) = isSum a
isSum _ = False

isTinySum :: !(RadExpr k) -> Bool
isTinySum e = isSum e && length (flattenS e) <= 2

flattenS :: !(RadExpr k) -> [RadExpr k]
flattenS (Add a b) = flattenS a ++ flattenS b
flattenS (Neg a) = map Neg (flattenS a)
flattenS e = [e]

rebuildAddNE :: ![RadExpr k] -> RadExpr k
rebuildAddNE [x] = x
rebuildAddNE [x:xs] = foldl Add x xs
rebuildAddNE [] = abort "rebuildAddNE: empty list"
