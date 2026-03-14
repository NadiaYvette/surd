FSTAR=$(HOME)/opt/fstar/bin/fstar.exe
FSTAR_FLAGS=--cache_checked_modules --z3rlimit 300

MODULES=Surd.Ring Surd.Rational Surd.Positive Surd.PrimeFactors Surd.Poly Surd.Types Surd.Interval \
        Surd.Normalize Surd.Lemmas Surd.Pretty Surd.LaTeX Surd.Eval Surd.Expr \
        Surd.Cyclotomic Surd.Resultant Surd.RootBound Surd.Factoring Surd.Extension \
        Surd.DAG Surd.NormalForm Surd.Denest \
        Surd.RootIsolation Surd.AlgNum Surd.MinimalPoly Surd.Convert \
        Surd.TrigGalois Surd.RootOfUnity Surd.Trig \
        Surd.Permutation Surd.TransitiveGroup Surd.Resolvent Surd.Identify \
        Surd.RadicalTower Surd.GaloisSolve \
        Surd.EulerIntegration Surd.EllipticIntegration \
        Surd.Demo.TrigTable Surd.Demo.EulerIntegral \
        Surd.Demo.EllipticIntegral Surd.Demo.SolvableQuintic \
        Surd.Test

.PHONY: all clean verify

all: verify

verify: $(addsuffix .checked,$(MODULES))

# Dependencies: encode module dependency order
Surd.Ring.checked: Surd.Ring.fst
	$(FSTAR) $(FSTAR_FLAGS) $<
	touch $@

Surd.Positive.checked: Surd.Positive.fst
	$(FSTAR) $(FSTAR_FLAGS) $<
	touch $@

Surd.Rational.checked: Surd.Rational.fst Surd.Ring.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.PrimeFactors.checked: Surd.PrimeFactors.fst Surd.Positive.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Poly.checked: Surd.Poly.fst Surd.Ring.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Types.checked: Surd.Types.fst Surd.Ring.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Interval.checked: Surd.Interval.fst Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Normalize.checked: Surd.Normalize.fst Surd.Types.checked Surd.Rational.checked Surd.PrimeFactors.checked Surd.Positive.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Pretty.checked: Surd.Pretty.fst Surd.Types.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.LaTeX.checked: Surd.LaTeX.fst Surd.Types.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Eval.checked: Surd.Eval.fst Surd.Types.checked Surd.Rational.checked Surd.Interval.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Expr.checked: Surd.Expr.fst Surd.Types.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

# Phase E: Core polynomial extensions
Surd.Cyclotomic.checked: Surd.Cyclotomic.fst Surd.Poly.checked Surd.PrimeFactors.checked Surd.Positive.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Resultant.checked: Surd.Resultant.fst Surd.Poly.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.RootBound.checked: Surd.RootBound.fst Surd.Poly.checked Surd.Rational.checked Surd.Resultant.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Factoring.checked: Surd.Factoring.fst Surd.Poly.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Extension.checked: Surd.Extension.fst Surd.Poly.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

# Phase E: Radical infrastructure
Surd.DAG.checked: Surd.DAG.fst Surd.Types.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.NormalForm.checked: Surd.NormalForm.fst Surd.Types.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Denest.checked: Surd.Denest.fst Surd.Types.checked Surd.Rational.checked Surd.Normalize.checked Surd.Expr.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

# Phase E: Algebraic numbers
Surd.RootIsolation.checked: Surd.RootIsolation.fst Surd.Poly.checked Surd.Rational.checked Surd.Interval.checked Surd.RootBound.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.AlgNum.checked: Surd.AlgNum.fst Surd.Poly.checked Surd.Rational.checked Surd.Interval.checked Surd.RootIsolation.checked Surd.Resultant.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.MinimalPoly.checked: Surd.MinimalPoly.fst Surd.Poly.checked Surd.Rational.checked Surd.Types.checked Surd.Resultant.checked Surd.Expr.checked Surd.Factoring.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Convert.checked: Surd.Convert.fst Surd.Types.checked Surd.Poly.checked Surd.Rational.checked Surd.AlgNum.checked Surd.MinimalPoly.checked Surd.Eval.checked Surd.Interval.checked Surd.RootIsolation.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

# Phase E: Trig
Surd.TrigGalois.checked: Surd.TrigGalois.fst Surd.Types.checked Surd.Poly.checked Surd.Rational.checked Surd.PrimeFactors.checked Surd.Positive.checked Surd.Cyclotomic.checked Surd.Convert.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.RootOfUnity.checked: Surd.RootOfUnity.fst Surd.Types.checked Surd.Rational.checked Surd.Positive.checked Surd.PrimeFactors.checked Surd.TrigGalois.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Trig.checked: Surd.Trig.fst Surd.Types.checked Surd.Rational.checked Surd.Positive.checked Surd.PrimeFactors.checked Surd.Normalize.checked Surd.RootOfUnity.checked Surd.TrigGalois.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

# Phase E: Galois
Surd.Permutation.checked: Surd.Permutation.fst
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.TransitiveGroup.checked: Surd.TransitiveGroup.fst Surd.Permutation.checked Surd.PrimeFactors.checked Surd.Positive.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Resolvent.checked: Surd.Resolvent.fst Surd.Poly.checked Surd.Rational.checked Surd.Resultant.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Identify.checked: Surd.Identify.fst Surd.Poly.checked Surd.Rational.checked Surd.Factoring.checked Surd.Resolvent.checked Surd.TransitiveGroup.checked Surd.PrimeFactors.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.RadicalTower.checked: Surd.RadicalTower.fst Surd.Types.checked Surd.Poly.checked Surd.Rational.checked Surd.Eval.checked Surd.TransitiveGroup.checked Surd.PrimeFactors.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.GaloisSolve.checked: Surd.GaloisSolve.fst Surd.Types.checked Surd.Poly.checked Surd.Rational.checked Surd.Identify.checked Surd.TransitiveGroup.checked Surd.RadicalTower.checked Surd.Convert.checked Surd.PrimeFactors.checked Surd.Eval.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

# Phase E: Integration
Surd.EulerIntegration.checked: Surd.EulerIntegration.fst Surd.Types.checked Surd.Poly.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.EllipticIntegration.checked: Surd.EllipticIntegration.fst Surd.Types.checked Surd.Poly.checked Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

# Phase F: Demos
Surd.Demo.TrigTable.checked: Surd.Demo.TrigTable.fst Surd.Trig.checked Surd.Pretty.checked Surd.LaTeX.checked Surd.Normalize.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Demo.EulerIntegral.checked: Surd.Demo.EulerIntegral.fst Surd.EulerIntegration.checked Surd.Pretty.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Demo.EllipticIntegral.checked: Surd.Demo.EllipticIntegral.fst Surd.EllipticIntegration.checked Surd.Pretty.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Demo.SolvableQuintic.checked: Surd.Demo.SolvableQuintic.fst Surd.GaloisSolve.checked Surd.TransitiveGroup.checked Surd.Identify.checked Surd.Pretty.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

# Lemmas and tests
Surd.Lemmas.checked: Surd.Lemmas.fst Surd.Rational.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

Surd.Test.checked: Surd.Test.fst Surd.Rational.checked Surd.Lemmas.checked Surd.Positive.checked Surd.Types.checked
	$(FSTAR) $(FSTAR_FLAGS) --include . $<
	touch $@

clean:
	rm -f *.checked
