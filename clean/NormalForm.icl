implementation module NormalForm

import StdEnv
import RadExpr
import Rational
import Normalize

// NormalForm round-trip: normalize, then convert to canonical form.
// For now this delegates to the tree normalizer, which handles
// like-term collection, coefficient merging, power simplification,
// and perfect power extraction.

toNormExpr :: !(RadExpr Rational) -> RadExpr Rational
toNormExpr expr = normalize expr

fromNormExpr :: !(RadExpr Rational) -> RadExpr Rational
fromNormExpr expr = expr

normalFormRoundTrip :: !(RadExpr Rational) -> RadExpr Rational
normalFormRoundTrip e = fromNormExpr (toNormExpr e)
