implementation module Poly

import StdEnv, StdMaybe

// Strip trailing zeros from coefficient list.
stripZeros :: [k] -> [k] | == k & zero k
stripZeros cs = reverse (dropWhile (\c -> c == zero) (reverse cs))

mkPoly :: [k] -> Poly k | == k & zero k
mkPoly cs = Poly (stripZeros cs)

zeroPoly :: Poly k
zeroPoly = Poly []

constPoly :: k -> Poly k | == k & zero k
constPoly c
    | c == zero = Poly []
    = Poly [c]

monoX :: Poly k | zero k & one k
monoX = Poly [zero, one]

degree :: !(Poly k) -> Int
degree (Poly []) = ~1
degree (Poly cs) = length cs - 1

leadCoeff :: !(Poly k) -> Maybe k
leadCoeff (Poly []) = Nothing
leadCoeff (Poly cs) = Just (last cs)

evalPoly :: !(Poly k) k -> k | + k & * k & zero k
evalPoly (Poly []) _ = zero
evalPoly (Poly cs) x = foldr (\c acc -> c + x * acc) zero cs

scalePoly :: k !(Poly k) -> Poly k | == k & zero k & * k
scalePoly s (Poly cs)
    | s == zero = Poly []
    = mkPoly (map (\c -> s * c) cs)

addPoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & + k
addPoly (Poly as) (Poly bs) = mkPoly (zipWithDefault zero (+) as bs)

subPoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & + k & ~ k
subPoly (Poly as) (Poly bs) = mkPoly (zipWithDefault zero (\a b -> a + ~ b) as bs)

negatePoly :: !(Poly k) -> Poly k | ~ k
negatePoly (Poly cs) = Poly (map (~) cs)

// Zip two lists, extending the shorter with a default value.
zipWithDefault :: a (a a -> a) [a] [a] -> [a]
zipWithDefault _ _ [] [] = []
zipWithDefault d f [a:as] [] = [f a d : zipWithDefault d f as []]
zipWithDefault d f [] [b:bs] = [f d b : zipWithDefault d f [] bs]
zipWithDefault d f [a:as] [b:bs] = [f a b : zipWithDefault d f as bs]

// Zip two lists with a function (like Haskell's zipWith).
myZipWith :: (a b -> c) [a] [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f [a:as] [b:bs] = [f a b : myZipWith f as bs]

// Polynomial multiplication via schoolbook algorithm.
mulPoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & + k & * k
mulPoly (Poly []) _ = Poly []
mulPoly _ (Poly []) = Poly []
mulPoly (Poly as) (Poly bs) = mkPoly (mulCoeffs as bs)

mulCoeffs :: [k] [k] -> [k] | zero k & + k & * k
mulCoeffs as bs = foldl addTerm (repeatn rlen zero) terms
where
    rlen = length as + length bs - 1
    terms = [(i + j, a * b) \\ (i, a) <- zip2 [0..] as, (j, b) <- zip2 [0..] bs]

addTerm :: [k] (Int, k) -> [k] | + k
addTerm cs (idx, c) = take idx cs ++ [hd (drop idx cs) + c : tl (drop idx cs)]

// Polynomial division with remainder.
// f = g*q + r, degree r < degree g.
divModPoly :: !(Poly k) !(Poly k) -> (Poly k, Poly k) | == k & zero k & one k & + k & ~ k & * k & / k
divModPoly _ (Poly []) = abort "divModPoly: division by zero polynomial"
divModPoly f g
    | degree f < degree g = (zeroPoly, f)
    = divLoop zeroPoly f
where
    dg = degree g
    lc = case leadCoeff g of
        Just c  -> c
        Nothing -> abort "impossible"

    divLoop q r
        | degree r < dg = (q, r)
        = divLoop (addPoly q term) (subPoly r (mulPoly term g))
    where
        dr = degree r
        lr = case leadCoeff r of
            Just lrc -> lrc
            Nothing  -> abort "impossible"
        c = lr / lc
        d = dr - dg
        term = Poly (repeatn d zero ++ [c])

// GCD via Euclidean algorithm, made monic.
gcdPoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & one k & + k & ~ k & * k & / k
gcdPoly a (Poly []) = monicPoly a
gcdPoly a b = gcdPoly b (snd (divModPoly a b))

// Make leading coefficient 1.
monicPoly :: !(Poly k) -> Poly k | == k & zero k & one k & * k & / k
monicPoly (Poly []) = Poly []
monicPoly p=:(Poly cs)
    = case leadCoeff p of
        Nothing -> Poly []
        Just lc -> Poly (map (\c -> c / lc) cs)

// Formal derivative.
diffPoly :: !(Poly k) -> Poly k | == k & zero k & + k & * k & fromInt k
diffPoly (Poly []) = Poly []
diffPoly (Poly [_:cs]) = mkPoly (myZipWith (\i c -> fromInt i * c) [1..] cs)

// Compose f(g(x)).
composePoly :: !(Poly k) !(Poly k) -> Poly k | == k & zero k & one k & + k & * k
composePoly (Poly []) _ = Poly []
composePoly (Poly cs) g = foldr (\c acc -> addPoly (constPoly c) (mulPoly g acc)) zeroPoly cs

// Square-free factorisation via Yun's algorithm.
squareFree :: !(Poly k) -> [(Poly k, Int)] | == k & zero k & one k & + k & ~ k & * k & / k & fromInt k
squareFree (Poly []) = []
squareFree f = yunLoop w0 c0 1
where
    fd = diffPoly f
    c0 = gcdPoly f fd
    w0 = fst (divModPoly f c0)

    yunLoop w c i
        | degree w == 0
            | degree c > 0 = [(c, i)]
            = []
        = factors ++ yunLoop wn cn (i + 1)
    where
        y = gcdPoly w c
        wn = fst (divModPoly w y)
        cn = fst (divModPoly c y)
        factors
            | degree wn > 0 = [(wn, i)]
            = []

instance == (Poly k) | == k where
    (==) (Poly as) (Poly bs) = as == bs

instance toString (Poly k) | toString k where
    toString (Poly []) = "0"
    toString (Poly cs) = "Poly " +++ listToString cs

listToString :: [k] -> {#Char} | toString k
listToString [] = "[]"
listToString [x:xs] = "[" +++ toString x +++ concatStrs (map (\e -> "," +++ toString e) xs) +++ "]"

concatStrs :: [{#Char}] -> {#Char}
concatStrs [] = ""
concatStrs [s:ss] = s +++ concatStrs ss

import Algebra

instance Ring (Poly Rational) where
    rzero     = zeroPoly
    rone      = constPoly one
    radd  a b = addPoly a b
    rmul  a b = mulPoly a b
    rneg  a   = negatePoly a
