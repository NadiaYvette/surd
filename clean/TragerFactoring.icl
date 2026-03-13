implementation module TragerFactoring

import StdEnv
import Poly
import Rational
import Extension

// Stub: return the polynomial unfactored.
factorOverExtension :: !(Poly Rational) !(Poly Rational) -> [(Poly Rational, Int)]
factorOverExtension f _ = [(f, 1)]
