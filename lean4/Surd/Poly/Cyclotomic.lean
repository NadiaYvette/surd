/-
  Surd.Poly.Cyclotomic — Cyclotomic polynomial computation.

  Φₙ(x) = (x^n − 1) / ∏_{d|n, d<n} Φ_d(x)
-/
import Surd.Poly.Univariate
import Surd.PrimeFactors
import Std.Internal.Rat

open Std.Internal

namespace Surd

/-- Proper divisors of n (divisors d where 1 ≤ d < n). -/
private def properDivisors (n : Nat) : List Nat :=
  if n ≤ 1 then []
  else (List.range n).filter fun d => d ≥ 1 && n % d == 0

/-- Build x^n - 1 as a polynomial over Rat. -/
private def xnMinus1 (n : Nat) : Poly Rat :=
  let cs := Array.mkArray (n + 1) (0 : Rat)
  let cs := cs.set! 0 (-1)
  let cs := cs.set! n 1
  Poly.mkPoly cs

/-- Compute the nth cyclotomic polynomial Φₙ(x) over Rat. -/
partial def cyclotomic (n : Nat) : Poly Rat :=
  if n == 0 then Poly.const 1
  else if n == 1 then
    -- Φ₁(x) = x - 1
    Poly.mkPoly #[-1, 1]
  else
    let divs := properDivisors n
    let product := divs.foldl (fun acc d => Poly.mul acc (cyclotomic d)) (Poly.const 1)
    (Poly.divMod (xnMinus1 n) product).1

/-- Compute all cyclotomic polynomials Φ₁ through Φₙ efficiently. -/
partial def allCyclotomic (n : Nat) : Array (Poly Rat) :=
  go 1 #[]
where
  go (i : Nat) (acc : Array (Poly Rat)) : Array (Poly Rat) :=
    if i > n then acc
    else
      let phi := cyclotomic i
      go (i + 1) (acc.push phi)

/-- Möbius function μ(n). Returns 0 if n has a squared prime factor,
    otherwise (-1)^k where k is the number of distinct prime factors. -/
def moebiusMu (n : Nat) : Int :=
  if n ≤ 0 then 0
  else
    let facts := factoriseNat n
    if facts.any (fun (_, e) => e > 1) then 0
    else if facts.length % 2 == 0 then 1
    else -1

end Surd
