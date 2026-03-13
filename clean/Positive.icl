implementation module Positive

import StdEnv, StdMaybe

:: Positive = Positive !Int

positive :: !Int -> Maybe Positive
positive n
    | n > 0 = Just (Positive n)
    = Nothing

unsafePositive :: !Int -> Positive
unsafePositive n
    | n > 0 = Positive n
    = abort "unsafePositive: non-positive value"

unPositive :: !Positive -> Int
unPositive (Positive n) = n

instance == Positive where
    (==) (Positive a) (Positive b) = a == b

instance < Positive where
    (<) (Positive a) (Positive b) = a < b

instance toString Positive where
    toString (Positive n) = toString n
