-- | Smallest demo that reproduces the Frankenstein surd-trig segfault
-- at startup (SEGV_MAPERR on a stack address, exit 139, no output).
-- The cosExact pattern match forces ~267 definitions from the Surd.Trig
-- transitive closure into the OrganIR JSON.  See
-- ~/.claude/projects/-home-nyc-src-neopoplog/memory/idris2_shim.md
-- for the build incantation and current state.
module Demo.TrigMin

import Surd.Trig
import Surd.Types

main : IO ()
main = do
  putStrLn "start"
  let r = cosExact 1 4
  case r of
    Radical _ => putStrLn "got radical"
    MinPoly _ => putStrLn "got minpoly"
  putStrLn "done"
