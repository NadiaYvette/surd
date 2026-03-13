implementation module RootOfUnity

import StdEnv
import RadExpr
import Rational
import Positive
import PrimeFactors
import TrigGalois
import Data.Integer

fermatPrimes :: [Int]
fermatPrimes = [3, 5, 17, 257, 65537]

isConstructible :: !Int -> Bool
isConstructible n
    # fs = factorise (unsafePositive n)
    # oddFactors = [(p, e) \\ (p, e) <- fs | p <> 2]
    = all (\(p, e) -> e == 1 && isMember p fermatPrimes) oddFactors

cosOfUnity :: !Int -> ?((RadExpr Rational))
cosOfUnity n
    | n <= 0 = ?None
    | n == 1 = ?Just (Lit (ratFromInt 1))
    | n == 2 = ?Just (Lit (ratFromInt (~1)))
    | n == 3 = ?Just (Lit (mkRational (toInteger (~1)) (toInteger 2)))
    | n == 4 = ?Just (Lit (ratFromInt 0))
    | n == 5 = ?Just cos2piOver5
    | n == 6 = ?Just (Lit (mkRational (toInteger 1) (toInteger 2)))
    | n == 8 = ?Just (Mul (Inv (Lit (ratFromInt 2))) (Root 2 (Lit (ratFromInt 2))))
    | n == 10 = ?Just cos2piOver10
    | n == 12 = ?Just (Mul (Inv (Lit (ratFromInt 2))) (Root 2 (Lit (ratFromInt 3))))
    | isPowerOf2 n = ?Just (cosOfPow2 n)
    | isPrime n = cosOfUnityViaGauss n
    | otherwise = cosOfUnityComposite n

sinOfUnity :: !Int -> ?((RadExpr Rational))
sinOfUnity n
    | n <= 0 = ?None
    | n == 1 = ?Just (Lit (ratFromInt 0))
    | n == 2 = ?Just (Lit (ratFromInt 0))
    | n == 3 = ?Just (Mul (Lit (mkRational (toInteger 1) (toInteger 2))) (Root 2 (Lit (ratFromInt 3))))
    | n == 4 = ?Just (Lit (ratFromInt 1))
    | n == 6 = ?Just (Mul (Lit (mkRational (toInteger 1) (toInteger 2))) (Root 2 (Lit (ratFromInt 3))))
    | n == 8 = ?Just (Mul (Inv (Lit (ratFromInt 2))) (Root 2 (Lit (ratFromInt 2))))
    | n == 12 = ?Just (Lit (mkRational (toInteger 1) (toInteger 2)))
    | otherwise
        = case cosOfUnity n of
            ?None -> ?None
            ?Just c -> ?Just (Root 2 (Add (Lit (ratFromInt 1)) (Neg (Mul c c))))

// cos(2pi/5) = (sqrt(5) - 1) / 4
cos2piOver5 :: RadExpr Rational
cos2piOver5 = Mul (Inv (Lit (ratFromInt 4))) (Add (Root 2 (Lit (ratFromInt 5))) (Lit (ratFromInt (~1))))

// cos(2pi/10) = (1 + sqrt(5)) / 4
cos2piOver10 :: RadExpr Rational
cos2piOver10 = Mul (Inv (Lit (ratFromInt 4))) (Add (Lit (ratFromInt 1)) (Root 2 (Lit (ratFromInt 5))))

cosOfPow2 :: !Int -> RadExpr Rational
cosOfPow2 1 = Lit (ratFromInt 1)
cosOfPow2 2 = Lit (ratFromInt (~1))
cosOfPow2 4 = Lit (ratFromInt 0)
cosOfPow2 k
    # half = cosOfPow2 (k / 2)
    = Root 2 (Mul (Inv (Lit (ratFromInt 2))) (Add (Lit (ratFromInt 1)) half))

isPowerOf2 :: !Int -> Bool
isPowerOf2 n = n > 0 && (n bitand (n - 1)) == 0

cosOfUnityComposite :: !Int -> ?((RadExpr Rational))
cosOfUnityComposite n
    # fs = factorise (unsafePositive n)
    | isEmpty fs = ?None
    | length fs == 1 = cosOfUnityViaGauss n
    # (p1, e1) = hd fs
    # n1 = intPow p1 e1
    # n2 = n / n1
    # (a, b) = extGcdI n1 n2
    # a` = ((a rem n2) + n2) rem n2
    # b` = ((b rem n1) + n1) rem n1
    # sA = sinSign a` n2
    # sB = sinSign b` n1
    # signFactor = sA * sB
    = compositeFromBases (cosOfUnity n2) (cosOfUnity n1) a` b` signFactor

compositeFromBases :: !(?(RadExpr Rational)) !(?(RadExpr Rational)) !Int !Int !Int -> ?(RadExpr Rational)
compositeFromBases (?Just cosBase1) (?Just cosBase2) a b signFactor
    # cosAE = chebyshevSimple a cosBase1
    # cosBE = chebyshevSimple b cosBase2
    | signFactor == 0 = ?Just (Mul cosAE cosBE)
    # sinAbsA = sinFromCos cosAE
    # sinAbsB = sinFromCos cosBE
    # sinProduct = Mul sinAbsA sinAbsB
    | signFactor == 1 = ?Just (Add (Mul cosAE cosBE) (Neg sinProduct))
    = ?Just (Add (Mul cosAE cosBE) sinProduct)
compositeFromBases _ _ _ _ _ = ?None

sinSign :: !Int !Int -> Int
sinSign k m
    # k` = k rem m
    | k` == 0 = 0
    | 2 * k` < m = 1
    | 2 * k` == m = 0
    = ~1

extGcdI :: !Int !Int -> (Int, Int)
extGcdI 0 _ = (0, 1)
extGcdI x y
    # (q, r) = (y / x, y rem x)
    # (a, b) = extGcdI r x
    = (b - q * a, a)

chebyshevSimple :: !Int !(RadExpr Rational) -> RadExpr Rational
chebyshevSimple 0 _ = Lit (ratFromInt 1)
chebyshevSimple 1 x = x
chebyshevSimple k x = go 2 (Lit (ratFromInt 1)) x
where
    go n t0 t1
        | n > k = t1
        # t2 = Add (Mul (Mul (Lit (ratFromInt 2)) x) t1) (Neg t0)
        = go (n + 1) t1 t2

sinFromCos :: !(RadExpr Rational) -> RadExpr Rational
sinFromCos c = Root 2 (Add (Lit (ratFromInt 1)) (Neg (Mul c c)))

intPow :: !Int !Int -> Int
intPow _ 0 = 1
intPow b e
    | isOdd e = b * intPow b (e - 1)
    = let half = intPow b (e / 2) in half * half
