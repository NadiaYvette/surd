# Mercury stdlib `integer.m`: `mul_by_digit(0, Y)` produces denormalized zeros

## Summary

`integer.mul_by_digit(0, Y)` produces a denormalized zero value `i(Len, [0, 0, ...])` instead of the canonical `integer.zero = i(0, [])`. Since `is_zero/1` only matches the canonical form, all downstream code that checks for zero via structural equality (`= integer.zero`) or `is_zero/1` silently treats these denormalized zeros as nonzero. This causes incorrect results in `rational.m` and any other code that depends on zero detection.

## Affected file

`library/integer.m` (tested against Mercury 22.01.8 and ROTD 2024-06-15; confirmed present in current `master` at `library/integer.m`)

## Root cause

`mul_by_digit/2` has no guard for `Digit = 0`:

```mercury
mul_by_digit(Digit, i(Len, Digits0)) = Out :-
    mul_by_digit_2(Digit, Mod, Digits0, Digits),
    ( if Mod = 0 then
        Out = i(Len, Digits)
    else
        Out = i(Len + 1, [Mod | Digits])
    ).
```

When `Digit = 0`, `mul_by_digit_2` multiplies every limb by 0, producing `Mod = 0` and `Digits = [0, 0, ...]` (same length as input). The result is `i(Len, [0, 0, ...])` -- a non-canonical representation of zero. Since `is_zero/1` is defined as:

```mercury
is_zero(i(0, [])).
```

it does **not** match this denormalized form.

## When `mul_by_digit(0, _)` is reached

`mul_by_digit` is called from `pos_mul_list` during multi-word multiplication (`big_mul` path). When multiplying two multi-word integers where one has a zero limb (e.g., `10000 * anything` in the base-10000 representation), `pos_mul_list` calls `mul_by_digit(0, Y)`, producing the denormalized value.

## Consequences

### In `rational.m`

`rational_norm`, `gcd_2`, `lcm`, `signum`, and `is_zero` all use structural equality (`= integer.zero`) to check for zero. When a denormalized zero is produced by arithmetic, these checks fail:

- **`rational_norm`**: Fails to detect zero numerator, producing `r(i(N, [0,...]), ...)` instead of `r(integer.zero, integer.one)`. Fails to detect zero denominator, skipping the "division by zero" error.
- **`gcd_2`**: The Euclidean algorithm never terminates (or terminates incorrectly) because the base case `is_zero(B)` doesn't fire for denormalized zero remainders.
- **`lcm`**: Fails to detect zero arguments.
- **`signum`**: Returns `integer.one` for denormalized zero (since the zero check fails and `i(N, [0,...])` with `N > 0` is not `< integer.zero`).

**Observable failure**: Computing with rationals derived from large integer arithmetic (e.g., Gauss period descent for cos(2pi/13)) produces `integer.big_quot_rem: division by zero` crash or silently incorrect results.

### In any user code

Any code that uses `is_zero/1` or compares against `integer.zero` is affected. The denormalized zero compares as greater than zero, sorts incorrectly, and breaks invariants.

## Reproducer

```mercury
:- module test_denorm_zero.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module integer.
:- import_module rational.
:- import_module list.
:- import_module string.

main(!IO) :-
    % Force multi-word multiplication that hits mul_by_digit(0, _).
    % 10000 in base 10000 is i(1, [0, 1]) -- the zero limb triggers the bug.
    A = integer(10000),
    B = integer(10000),
    C = A * B,            % = 100000000, fine
    D = C - C,            % = 0, but may be denormalized after arithmetic

    % More reliable trigger: rational arithmetic that produces zero
    % through the mul_by_digit(0, Y) path.
    R1 = rational(1, 13),
    R2 = rational(1, 13),
    Diff = R1 - R2,       % Should be zero

    io.format("D = %s\n", [s(integer.to_string(D))], !IO),
    ( if integer.is_zero(D)
    then io.write_string("is_zero(D): yes\n", !IO)
    else io.write_string("is_zero(D): no  *** BUG ***\n", !IO)
    ),

    io.format("Diff numer = %s\n",
        [s(integer.to_string(numer(Diff)))], !IO),
    io.format("Diff denom = %s\n",
        [s(integer.to_string(denom(Diff)))], !IO),

    % The denormalized zero is most easily triggered by:
    % 1. Creating a multi-word integer (>= 10000 in absolute value)
    % 2. Multiplying it in a context where a zero limb participates
    % 3. The resulting zero has non-canonical representation
    %
    % In rational arithmetic, this happens naturally when computing
    % (a/b - a/b) where a*b involves multi-word intermediates.
    ok.

:- pred ok(io::di, io::uo) is det.
ok(!IO) :- io.write_string("done\n", !IO).
```

## Proposed fix

### Fix 1: Guard `mul_by_digit` against zero digit (minimal, targeted)

```mercury
mul_by_digit(Digit, i(Len, Digits0)) = Out :-
    ( if Digit = 0 then
        Out = integer.zero
    else
        mul_by_digit_2(Digit, Mod, Digits0, Digits),
        ( if Mod = 0 then
            Out = i(Len, Digits)
        else
            Out = i(Len + 1, [Mod | Digits])
        )
    ).
```

This is the minimal fix: catch the zero digit before it produces a denormalized result. It also short-circuits unnecessary work.

### Fix 2: Make `is_zero` robust to denormalized representations (belt-and-suspenders)

```mercury
is_zero(i(0, [])).
is_zero(i(_, [D | Ds])) :-
    D = 0,
    list.all_true(pred(0::in) is semidet, Ds).
```

This makes `is_zero` recognize all-zero digit lists regardless of the `Len` field. This is a defensive fix -- it's good to have, but Fix 1 addresses the root cause.

### Fix 3: Also fix `rational.m` to use `is_zero` instead of structural equality

`rational.m` uses `= integer.zero` (structural equality) in several places. Even with Fix 1, this is fragile since other arithmetic paths could hypothetically produce denormalized zeros. The fix is to replace all `= integer.zero` checks with `integer.is_zero()`:

```mercury
% In rational_norm:
( if integer.is_zero(Den) then ...
  else if integer.is_zero(Num) then ...

% In gcd_2:
gcd_2(A, B) = ( if integer.is_zero(B) then A else gcd_2(B, A rem B) ).

% In lcm:
( if integer.is_zero(A) then ...
  else if integer.is_zero(B) then ...

% In signum:
( if integer.is_zero(N) then ...

% In is_zero (the rational predicate):
is_zero(r(Num, _)) :- integer.is_zero(Num).
```

## Recommended approach

Apply all three fixes. Fix 1 is the root cause fix. Fix 2 is defense in depth. Fix 3 makes `rational.m` robust regardless.

## Discovery context

Found while implementing Gauss period descent for exact trigonometric values in radical form. Computing cos(2pi/13) involves tower field extensions with rational arithmetic on large intermediates. The denominator 13 (a prime) causes multi-word integer multiplication in `lcm(13, ...)` computations, which hits the `mul_by_digit(0, ...)` path, producing denormalized zeros that cascade into a `division by zero` crash in `big_quot_rem`.
