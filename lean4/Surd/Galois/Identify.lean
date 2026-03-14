/-
  Surd.Galois.Identify — Galois group identification for irreducible
  polynomials over Q.

  For degree 5: Stauduhar descent via discriminant + sextic resolvent
  + Frobenius test.

  For other prime degrees p: generalized Stauduhar descent through
  the lattice of AGL(1,p) subgroups using Frobenius/Chebotarev
  factorisation patterns.

  Decision tree (degree 5):
    disc square?  sextic root?   result
    no            no             S₅
    yes           no             A₅
    no            yes            F₂₀
    yes           yes            D₅ or C₅ (Frobenius test)
-/
import Surd.Poly.Univariate
import Surd.Poly.Factoring
import Surd.Galois.Resolvent
import Surd.Galois.TransitiveGroup
import Surd.PrimeFactors
import Surd.Rat
import Std.Internal.Rat

open Std.Internal

namespace Surd

-- ---------------------------------------------------------------------------
-- Result type
-- ---------------------------------------------------------------------------

/-- Result of Galois group identification. -/
structure GaloisResult where
  grGroup : TransitiveGroup
  grRoots : List CFloat
  deriving Inhabited

-- ---------------------------------------------------------------------------
-- Complex helpers (using CFloat from Resolvent, now public)
-- ---------------------------------------------------------------------------

private def cidMag (a : CFloat) : Float := Float.sqrt (a.re * a.re + a.im * a.im)
private def cidSub (a b : CFloat) : CFloat := ⟨a.re - b.re, a.im - b.im⟩
private def cidAdd (a b : CFloat) : CFloat := ⟨a.re + b.re, a.im + b.im⟩
private def cidMul (a b : CFloat) : CFloat :=
  ⟨a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re⟩
private def cidNeg (a : CFloat) : CFloat := ⟨-a.re, -a.im⟩
private def cidPow (a : CFloat) (n : Nat) : CFloat :=
  (List.range n).foldl (fun acc _ => cidMul acc a) ⟨1, 0⟩

-- ---------------------------------------------------------------------------
-- Permutations
-- ---------------------------------------------------------------------------

/-- Generate all permutations of a list. -/
private partial def perms {α : Type} : List α → List (List α)
  | [] => [[]]
  | xs => xs.enum.flatMap fun (i, x) =>
      let rest := xs.take i ++ xs.drop (i + 1)
      (perms rest).map (x :: ·)

-- ---------------------------------------------------------------------------
-- Clustering
-- ---------------------------------------------------------------------------

private def floatMin (a b : Float) : Float := if a ≤ b then a else b

/-- Greedy single-linkage clustering by distance tolerance. -/
private def clusterByDistance (vals : List CFloat) (tol : Float) : List (List CFloat) :=
  vals.foldl (fun clusters v =>
    let findBest := clusters.enum.foldl (fun best (i, cl) =>
      let minDist := cl.foldl (fun d c => floatMin d (cidMag (cidSub v c))) 1e30
      match best with
      | none => if minDist < tol then some (i, minDist) else none
      | some (_, bd) => if minDist < bd then some (i, minDist) else best
    ) none
    match findBest with
    | some (idx, _) =>
        clusters.enum.map fun (j, cl) => if j == idx then v :: cl else cl
    | none => [v] :: clusters
  ) []

private def clusterCenter (cs : List CFloat) : CFloat :=
  let n := cs.length.toFloat
  let s := cs.foldl cidAdd ⟨0, 0⟩
  ⟨s.re / n, s.im / n⟩

-- ---------------------------------------------------------------------------
-- Rational approximation
-- ---------------------------------------------------------------------------

private def approxRatId (x : Float) : Rat :=
  let sign : Float := if x < 0 then -1 else 1
  let ax := Float.abs x
  let (_, bestN, bestD) := (List.range 10000).foldl (fun (bestErr, bestN, bestD) d' =>
    let d := d' + 1
    let n := Float.round (ax * d.toFloat)
    let approx := n / d.toFloat
    let err := Float.abs (approx - ax)
    if err < bestErr then (err, n, d) else (bestErr, bestN, bestD)
  ) (1e20, 0.0, 1)
  let nAbs : Int := (Float.abs (sign * bestN)).toUInt64.toNat |> Int.ofNat
  let nFinal : Int := if x < 0 then -nAbs else nAbs
  (nFinal : Rat) / (Int.ofNat bestD : Rat)

-- ---------------------------------------------------------------------------
-- Round complex polynomial to rational
-- ---------------------------------------------------------------------------

private def roundToRatPoly (roots : List CFloat) : Option (Poly Rat) :=
  let poly := roots.foldl (fun cs r =>
    let shifted := (⟨(0 : Float), 0⟩ : CFloat) :: cs
    let scaled := cs.map (cidMul (cidNeg r)) ++ [(⟨0, 0⟩ : CFloat)]
    shifted.zip scaled |>.map fun (a, b) => cidAdd a b
  ) [⟨1, 0⟩]
  let results := poly.map fun c =>
    let threshold := if Float.abs c.re > 1 then Float.abs c.re else 1
    if Float.abs c.im > 1e-4 * threshold then none
    else some (approxRatId c.re)
  if results.any (·.isNone) then none
  else some (Poly.mkPoly (results.filterMap id |>.toArray))

-- ---------------------------------------------------------------------------
-- Sextic resolvent (degree 5 only)
-- ---------------------------------------------------------------------------

/-- F₂₀-invariant θ(x₀,...,x₄) = Σ xᵢ²(x_{i+1}x_{i+4} + x_{i+2}x_{i+3}). -/
private def theta5 (xs : List CFloat) : CFloat :=
  (List.range 5).foldl (fun acc i =>
    let xi := xs.get! i
    let xi2 := cidMul xi xi
    let t1 := cidMul (xs.get! ((i + 1) % 5)) (xs.get! ((i + 4) % 5))
    let t2 := cidMul (xs.get! ((i + 2) % 5)) (xs.get! ((i + 3) % 5))
    cidAdd acc (cidMul xi2 (cidAdd t1 t2))
  ) ⟨0, 0⟩

/-- Construct sextic resolvent from 5 complex roots. -/
private def sexticResolvent5 (roots : List CFloat) : Option (Poly Rat) :=
  let allPerms := perms [0, 1, 2, 3, 4]
  let vals := allPerms.map fun p => theta5 (p.map fun j => roots.get! j)
  let clusters := clusterByDistance vals 1e-4
  if clusters.length != 6 then none
  else roundToRatPoly (clusters.map clusterCenter)

-- ---------------------------------------------------------------------------
-- F_p polynomial arithmetic
-- ---------------------------------------------------------------------------

private def fpTrim (cs : List Int) : List Int :=
  cs.reverse.dropWhile (· == 0) |>.reverse

private def fpDeg (cs : List Int) : Int :=
  (fpTrim cs).length - 1

private def fpAdd (a b : List Int) (p : Int) : List Int :=
  let n := max a.length b.length
  let a' := a ++ List.replicate (n - a.length) 0
  let b' := b ++ List.replicate (n - b.length) 0
  fpTrim (a'.zip b' |>.map fun (x, y) => (x + y) % p)

private def fpSub (a b : List Int) (p : Int) : List Int :=
  let n := max a.length b.length
  let a' := a ++ List.replicate (n - a.length) 0
  let b' := b ++ List.replicate (n - b.length) 0
  fpTrim (a'.zip b' |>.map fun (x, y) => ((x - y) % p + p) % p)

private def fpMul (a b : List Int) (p : Int) : List Int :=
  if a.isEmpty || b.isEmpty then []
  else
    let na := a.length; let nb := b.length
    fpTrim ((List.range (na + nb - 1)).map fun i =>
      (List.range (i + 1)).foldl (fun acc j =>
        if j < na && (i - j) < nb
        then (acc + a.get! j * b.get! (i - j)) % p
        else acc
      ) 0 % p)

private partial def eGcdInt (a b : Int) : (Int × Int × Int) :=
  if a == 0 then (b, 0, 1)
  else
    let (g, x, y) := eGcdInt (b % a) a
    (g, y - (b / a) * x, x)

private def fpInv (a p : Int) : Int :=
  let (_, x, _) := eGcdInt a p
  ((x % p) + p) % p

private def fpMakeMonic (cs : List Int) (p : Int) : List Int :=
  match cs with
  | [] => []
  | _ =>
    let lc := cs.getLast!
    let lcInv := fpInv lc p
    cs.map fun c => (c * lcInv) % p

private partial def fpMod (a b : List Int) (p : Int) : List Int :=
  if fpDeg a < fpDeg b then fpTrim (a.map fun x => ((x % p) + p) % p)
  else
    let ta := fpTrim a; let tb := fpTrim b
    let lcBInv := fpInv tb.getLast! p
    let shift := (fpDeg a - fpDeg b).toNat
    let fac := (ta.getLast! * lcBInv) % p
    let padded := ta ++ List.replicate (shift + tb.length - ta.length) 0
    let sub := padded.enum.map fun (i, ta_i) =>
      let bi := if i >= shift && (i - shift) < tb.length
                then tb.get! (i - shift) else 0
      ((ta_i - fac * bi) % p + p) % p
    fpMod (fpTrim sub) tb p

private partial def fpDiv (a b : List Int) (p : Int) : List Int :=
  let db := fpDeg b; let tb := fpTrim b; let lcBInv := fpInv tb.getLast! p
  go [] (fpTrim a) db tb lcBInv p
where
  go (q r : List Int) (db : Int) (tb : List Int) (lcBInv : Int) (p : Int) : List Int :=
    if fpDeg r < db then fpTrim q
    else
      let tr := fpTrim r
      let dr := fpDeg r
      let shift := (dr - db).toNat
      let fac := (tr.getLast! * lcBInv) % p
      let q' := fpAdd q (List.replicate shift 0 ++ [fac]) p
      let sub := (List.range tr.length).map fun i =>
        if i >= shift && (i - shift) < tb.length
        then (fac * tb.get! (i - shift)) % p else 0
      let r' := fpTrim (tr.zip sub |>.map fun (x, y) => ((x - y) % p + p) % p)
      go q' r' db tb lcBInv p

private partial def fpGcd (a b : List Int) (p : Int) : List Int :=
  if (fpTrim b).isEmpty || fpDeg b < 0 then fpMakeMonic (fpTrim a) p
  else fpGcd b (fpMod a b p) p

private partial def fpPowMod (base : List Int) (expo : Int) (modulus : List Int) (p : Int)
    : List Int :=
  go [1] base expo
where
  go (res b : List Int) (e : Int) : List Int :=
    if e <= 0 then res
    else
      let res' := if e % 2 == 1 then fpMod (fpMul res b p) modulus p else res
      let b' := fpMod (fpMul b b p) modulus p
      go res' b' (e / 2)

-- ---------------------------------------------------------------------------
-- Frobenius/Chebotarev test
-- ---------------------------------------------------------------------------

/-- Distinct-degree factorization pattern of f mod p. -/
private partial def factorPattern (fcs : List Int) (p : Int) : List Int :=
  go [] 1 (fpTrim fcs) [0, 1]
where
  go (degs : List Int) (k : Int) (f h : List Int) : List Int :=
    if fpDeg f <= 0 then degs.mergeSort (· ≤ ·)
    else
      let h' := fpPowMod h p f p
      let hx := fpSub h' [0, 1] p
      let g := fpGcd hx f p
      let gd := fpDeg g
      if gd == 0 then go degs (k + 1) f h'
      else
        let nf := gd / k
        let f' := fpDiv f g p
        go (degs ++ List.replicate nf.toNat k) (k + 1) f' h'

private def smallPrimes : List Int :=
  [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113]

private def hasNonCyclicPatternDeg (intCs : List Int) (p : Int) (deg : Nat) : Bool :=
  let cs := intCs.map fun c => ((c % p) + p) % p
  let pat := factorPattern cs p
  pat != [deg] && pat != List.replicate deg 1

/-- Generalized Frobenius test for degree-n polynomial.
    Returns true if the Galois group has only cyclic-pattern factorisations. -/
private def isCyclicByFrobeniusDeg (f : Poly Rat) (deg : Nat) : Bool :=
  let cs := f.coeffs.toList
  let lcmDen := cs.foldl (fun acc c => Nat.lcm acc c.den) 1
  let intCs : List Int := cs.map fun c =>
    let scaled := c * (Int.ofNat lcmDen : Rat)
    scaled.num
  let lc := intCs.getLast!
  let disc := discriminantOf f
  let discN := disc.num
  let discD := Int.ofNat disc.den
  let goodPrime (p : Int) : Bool :=
    lc % p != 0 && discN % p != 0 && discD % p != 0
  let testPs := (smallPrimes.filter goodPrime).take 20
  !(testPs.any (hasNonCyclicPatternDeg intCs · deg))

/-- Frobenius/Chebotarev test: true if C₅, false if D₅. -/
private def isCyclicByFrobenius (f : Poly Rat) : Bool :=
  isCyclicByFrobeniusDeg f 5

-- ---------------------------------------------------------------------------
-- Degree-5 identification (original fast path)
-- ---------------------------------------------------------------------------

/-- Identify the Galois group of a degree-5 polynomial over Q. -/
partial def identifyGaloisGroup5 (f : Poly Rat) : Option GaloisResult :=
  if f.coeffs.size - 1 != 5 then none
  else
    let disc := discriminantOf f
    let discSq := isSquareRational disc
    let roots := complexRootsOf f
    match sexticResolvent5 roots with
    | none => none
    | some sextic =>
      let hasSexticRoot := hasRationalRoot sextic
      let name :=
        if !hasSexticRoot && !discSq then "S5"
        else if !hasSexticRoot && discSq then "A5"
        else if hasSexticRoot && !discSq then "F20"
        else if isCyclicByFrobenius f then "C5"
        else "D5"
      match (transGroupsOfDegree 5).find? (fun g => g.tgName == name) with
      | some group => some ⟨group, roots⟩
      | none => none

-- ---------------------------------------------------------------------------
-- General prime-degree identification
-- ---------------------------------------------------------------------------

/-- Identify the Galois group of a prime-degree polynomial via
    generalized Stauduhar descent using Frobenius/Chebotarev patterns.

    Strategy:
    1. Check factorisation patterns mod small primes.
    2. AGL-consistent patterns are: [p], [1,...,1], or [1, k, k, ...k].
    3. If any non-AGL pattern observed → Aₚ (if disc square) or Sₚ.
    4. Within AGL: min stabiliser order = lcm of non-trivial cycle lengths. -/
private partial def identifyGaloisGroupPrime (f : Poly Rat) : Option GaloisResult :=
  let n := f.coeffs.size - 1
  let p := n
  let disc := discriminantOf f
  let discSq := isSquareRational disc
  let roots := complexRootsOf f
  let groups := transGroupsOfDegree n

  -- Compute integer coefficients for Frobenius tests
  let cs := f.coeffs.toList
  let lcmDen := cs.foldl (fun acc c => Nat.lcm acc c.den) 1
  let intCs : List Int := cs.map fun c =>
    let scaled := c * (Int.ofNat lcmDen : Rat)
    scaled.num
  let lc := intCs.getLast!
  let discN := disc.num
  let discD := Int.ofNat disc.den
  let goodPrime (pr : Int) : Bool :=
    lc % pr != 0 && discN % pr != 0 && discD % pr != 0
  let testPrimes := (smallPrimes.filter goodPrime).take 50

  -- Collect factorisation patterns
  let patterns := testPrimes.map fun pr =>
    let csM := intCs.map fun c => ((c % pr) + pr) % pr
    (factorPattern csM pr).mergeSort (· ≤ ·)

  -- Check if pattern is consistent with AGL(1,p)
  let nI : Int := Int.ofNat n
  let isAGLPattern (pat : List Int) : Bool :=
    pat == [nI]                              -- translation: [p]
    || pat == List.replicate n 1             -- identity: [1,...,1]
    || (pat.length >= 2                      -- non-translation: [1, k, k, ...]
        && pat.min? == some 1
        && (pat.filter (· == 1)).length == 1
        && match pat.filter (· != 1) with
           | [] => true
           | k :: ks => ks.all (· == k))

  let insideAGL := patterns.all isAGLPattern

  -- Find minimum d: lcm of all observed non-trivial cycle lengths
  let nonTrivCycleLengths : List Nat := patterns.flatMap fun pat =>
    if pat == [nI] || pat == List.replicate n 1 then []
    else (pat.filter (· != 1)).map Int.toNat

  let minD : Nat := if nonTrivCycleLengths.isEmpty then 1
    else nonTrivCycleLengths.foldl Nat.lcm 1

  if !insideAGL then
    -- Not in AGL(1,p): either Aₚ or Sₚ
    let finalGroup :=
      if discSq then
        match groups.find? (fun g => g.tgName == "A" ++ toString p) with
        | some g => g
        | none => groups.getLast!
      else groups.getLast!  -- Sₚ
    some ⟨finalGroup, roots⟩
  else
    -- Inside AGL(1,p): find the smallest d that is ≥ minD and divides p-1
    let divs := (groups.filter (·.tgSolvable)).map (fun g => g.tgOrder / p)
      |>.mergeSort (· ≤ ·)
    let groupD := match divs.find? (· >= minD) with
      | some d => d
      | none => p - 1  -- fallback to full AGL(1,p)
    let finalGroup := match groups.find? (fun g => g.tgOrder == p * groupD) with
      | some g => g
      | none => match groups.find? (fun g => g.tgName == "AGL(1," ++ toString p ++ ")") with
        | some g => g
        | none => groups.getLast!
    some ⟨finalGroup, roots⟩

-- ---------------------------------------------------------------------------
-- Unified entry point
-- ---------------------------------------------------------------------------

/-- Identify the Galois group of an irreducible polynomial over Q
    of any supported degree.

    For degree 5, delegates to the optimised identifyGaloisGroup5.
    For other prime degrees, uses generalized Stauduhar descent.
    Returns none for unsupported degrees. -/
partial def identifyGaloisGroup (f : Poly Rat) : Option GaloisResult :=
  let deg := f.coeffs.size - 1
  if deg == 5 then identifyGaloisGroup5 f
  else if deg >= 3 && isPrime deg then identifyGaloisGroupPrime f
  else none

end Surd
