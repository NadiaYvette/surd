implementation module Rational

import StdEnv
import Data.Integer

:: Rational = { num :: !Integer, den :: !Integer }

// GCD for Integer (Euclidean algorithm)
gcdInteger :: !Integer !Integer -> Integer
gcdInteger a b
    | b == zero = abs a
    = gcdInteger b (a `irem` b)
where
    (`irem`) infixl 7 :: !Integer !Integer -> Integer
    (`irem`) x y = x - (x / y) * y

// Normalise: ensure denominator > zero, reduce by GCD
normalize :: !Integer !Integer -> Rational
normalize n d
    | d == zero = abort "Rational: zero denominator"
    | d < zero  = normalize (~ n) (~ d)
    | g == one  = { num = n, den = d }
    = { num = n / g, den = d / g }
where
    g = gcdInteger (abs n) d

mkRational :: !Integer !Integer -> Rational
mkRational n d = normalize n d

ratFromInt :: !Int -> Rational
ratFromInt n = { num = toInteger n, den = one }

ratFromInteger :: !Integer -> Rational
ratFromInteger n = { num = n, den = one }

numer :: !Rational -> Integer
numer r = r.num

denom :: !Rational -> Integer
denom r = r.den

instance + Rational where
    (+) a b = normalize (a.num * b.den + b.num * a.den) (a.den * b.den)

instance - Rational where
    (-) a b = normalize (a.num * b.den - b.num * a.den) (a.den * b.den)

instance * Rational where
    (*) a b = normalize (a.num * b.num) (a.den * b.den)

instance / Rational where
    (/) a b
        | b.num == zero = abort "Rational: division by zero"
        = normalize (a.num * b.den) (a.den * b.num)

instance zero Rational where
    zero = { num = zero, den = one }

instance one Rational where
    one = { num = one, den = one }

instance ~ Rational where
    ~ a = { num = ~ a.num, den = a.den }

instance abs Rational where
    abs a = { num = abs a.num, den = a.den }

instance sign Rational where
    sign a = sign a.num

instance == Rational where
    (==) a b = a.num == b.num && a.den == b.den

instance < Rational where
    (<) a b = a.num * b.den < b.num * a.den

instance toString Rational where
    toString a
        | a.den == one = toString a.num
        = toString a.num +++ "/" +++ toString a.den

instance fromInt Rational where
    fromInt n = ratFromInt n

ratPow :: !Rational !Int -> Rational
ratPow _ 0 = one
ratPow r n
    | n < 0 = ratPow (one / r) (0 - n)
    | isOdd n = r * ratPow r (n - 1)
    = let half = ratPow r (n / 2) in half * half
