-- | Sanity-check demo: Surd.Rational alone (38 defs) runs cleanly
-- through Frankenstein.  Used as the lower bound during bisection
-- of the surd-trig segfault.  Prints "3/4" then "done".
module Demo.SmallTest

import Surd.Rational

main : IO ()
main = do
  putStrLn "start"
  let r = mkRat 3 4
  putStrLn (show r)
  putStrLn "done"
