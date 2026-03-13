definition module Positive

// Strictly positive integers (> 0).
// Encodes positivity in the type to prevent runtime errors for
// functions requiring positive input (factorise, cyclotomic, etc.).

import StdOverloaded
from StdMaybe import :: Maybe

:: Positive

// Smart constructor: returns Nothing for non-positive values.
positive :: !Int -> Maybe Positive

// Unsafe constructor: aborts for non-positive values.
// Use only when positivity is guaranteed by context.
unsafePositive :: !Int -> Positive

// Extract the underlying Int.
unPositive :: !Positive -> Int

instance == Positive
instance < Positive
instance toString Positive
