implementation module Extension

import StdEnv
import Poly
import Rational

// Reduce a polynomial modulo the minimal polynomial
reduce :: !(Poly Rational) !(Poly Rational) -> Poly Rational
reduce p minP
    | degree minP <= 0 = p  // sentinel field (no reduction)
    = snd (divModPoly p minP)

mkExtElem :: !(Poly Rational) !(Poly Rational) -> ExtElem
mkExtElem p minP = { extPoly = reduce p minP, extMinPoly = minP }

extAlpha :: !(Poly Rational) -> ExtElem
extAlpha minP = mkExtElem monoX minP

extAdd :: !ExtElem !ExtElem -> ExtElem
extAdd a b = mkExtElem (addPoly a.extPoly b.extPoly) a.extMinPoly

extSub :: !ExtElem !ExtElem -> ExtElem
extSub a b = mkExtElem (subPoly a.extPoly b.extPoly) a.extMinPoly

extMul :: !ExtElem !ExtElem -> ExtElem
extMul a b = mkExtElem (mulPoly a.extPoly b.extPoly) a.extMinPoly

extNeg :: !ExtElem -> ExtElem
extNeg a = { extPoly = negatePoly a.extPoly, extMinPoly = a.extMinPoly }

extFromRat :: !Rational !(Poly Rational) -> ExtElem
extFromRat r minP = mkExtElem (constPoly r) minP

// Inverse via extended GCD: find s such that s*a = 1 mod minpoly
extInv :: !ExtElem -> ExtElem
extInv a
    | degree a.extPoly < 0 = abort "extInv: zero element"
    # (g, s, _) = extGcdPoly a.extPoly a.extMinPoly
    // g should be constant (since minpoly is irreducible)
    # gc = case leadCoeff g of
        ?Just c -> c
        ?None   -> abort "extInv: GCD is zero"
    = mkExtElem (scalePoly (one / gc) s) a.extMinPoly

// Extended GCD: returns (g, s, t) with g = s*a + t*b
extGcdPoly :: !(Poly Rational) !(Poly Rational) -> (Poly Rational, Poly Rational, Poly Rational)
extGcdPoly a b
    | degree b < 0 = (a, constPoly one, zeroPoly)
    # (q, r) = divModPoly a b
    # (g, s`, t`) = extGcdPoly b r
    = (g, t`, subPoly s` (mulPoly q t`))

instance == ExtElem where
    (==) a b = a.extPoly == b.extPoly && a.extMinPoly == b.extMinPoly

instance toString ExtElem where
    toString a = toString a.extPoly +++ " mod " +++ toString a.extMinPoly

import Algebra

// Ring instance: ExtElem forms a ring under polynomial addition and
// multiplication modulo the minimal polynomial.
// Note: rzero and rone require a sentinel minimal polynomial; users must
// ensure all ExtElems in an expression share the same extMinPoly.
// For generic algorithms, prefer extFromRat and extAdd/extMul directly.
instance Ring ExtElem where
    rzero     = { extPoly = zeroPoly, extMinPoly = Poly [] }
    rone      = { extPoly = constPoly one, extMinPoly = Poly [] }
    radd  a b = extAdd a b
    rmul  a b = extMul a b
    rneg  a   = extNeg a

// Field instance: inversion via extended GCD (minpoly must be irreducible).
instance Field ExtElem where
    finv  a   = extInv a
    fdiv  a b = extMul a (extInv b)
