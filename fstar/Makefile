FSTAR=$(HOME)/opt/fstar/bin/fstar.exe
FSTAR_FLAGS=--cache_checked_modules

MODULES=Surd.Ring Surd.Rational Surd.Positive Surd.PrimeFactors Surd.Poly Surd.Types Surd.Interval \
        Surd.Normalize Surd.Pretty Surd.LaTeX Surd.Eval Surd.Expr

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

clean:
	rm -f *.checked
