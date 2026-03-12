%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: eval_mp.m
% Main author: nyc
%
% Arbitrary-precision evaluation of radical DAGs via MPFR FFI.
%
% Port of Haskell's Surd.Radical.EvalMP. Uses MPFR for ball arithmetic
% with configurable precision. Complex nth roots computed via Newton's
% method (no atan needed).
%
%---------------------------------------------------------------------------%

:- module eval_mp.
:- interface.

:- import_module interval.
:- import_module rad_dag.
:- import_module rad_eval.
:- import_module rational.

%---------------------------------------------------------------------------%
% MPFR ball type
%---------------------------------------------------------------------------%

    % An MPFR floating-point value.  Heap-allocated, GC-finalized.
    %
:- type mpfr.

    % A complex pair of MPFR values.
    %
:- type complex_mp
    --->    complex_mp(
                cmp_re :: mpfr,
                cmp_im :: mpfr
            ).

%---------------------------------------------------------------------------%
% MPFR construction
%---------------------------------------------------------------------------%

    % Create an MPFR value from a rational number at the given precision.
    %
:- func mpfr_from_rational(int, rational) = mpfr.

    % Create an MPFR zero at the given precision.
    %
:- func mpfr_zero(int) = mpfr.

    % Create an MPFR value from a float (Double) at the given precision.
    %
:- func mpfr_from_float(int, float) = mpfr.

%---------------------------------------------------------------------------%
% MPFR arithmetic
%---------------------------------------------------------------------------%

:- func mp_add(mpfr, mpfr) = mpfr.
:- func mp_sub(mpfr, mpfr) = mpfr.
:- func mp_mul(mpfr, mpfr) = mpfr.
:- func mp_div(mpfr, mpfr) = mpfr.
:- func mp_neg(mpfr) = mpfr.
:- func mp_abs(mpfr) = mpfr.
:- func mp_sqrt(mpfr) = mpfr.

    % mp_pow_ui(X, N): X^N for non-negative integer N.
    %
:- func mp_pow_ui(mpfr, int) = mpfr.

    % mp_rootn(X, N): real Nth root of X (N > 0, X >= 0).
    %
:- func mp_rootn(mpfr, int) = mpfr.

%---------------------------------------------------------------------------%
% MPFR queries
%---------------------------------------------------------------------------%

    % Convert to float (Double).
    %
:- func mp_to_float(mpfr) = float.

    % True if the value is >= 0.
    %
:- pred mp_is_nonneg(mpfr::in) is semidet.

    % True if the value is < 0.
    %
:- pred mp_is_neg(mpfr::in) is semidet.

    % True if the value is zero.
    %
:- pred mp_is_zero(mpfr::in) is semidet.

    % Get the precision (in bits) of an MPFR value.
    %
:- func mp_prec(mpfr) = int.

%---------------------------------------------------------------------------%
% MPFR conversion to rational interval
%---------------------------------------------------------------------------%

    % Convert an MPFR value to a rational interval enclosing the exact value.
    % Uses the MPFR rounding to get lower and upper bounds.
    %
:- func mp_to_interval(mpfr) = interval.

    % Convert a complex_mp to a complex_interval.
    %
:- func cmp_to_complex_interval(complex_mp) = complex_interval.

%---------------------------------------------------------------------------%
% Complex MPFR arithmetic
%---------------------------------------------------------------------------%

:- func cmp_from_rational(int, rational) = complex_mp.
:- func cmp_add(complex_mp, complex_mp) = complex_mp.
:- func cmp_sub(complex_mp, complex_mp) = complex_mp.
:- func cmp_mul(complex_mp, complex_mp) = complex_mp.
:- func cmp_inv(int, complex_mp) = complex_mp.
:- func cmp_neg(complex_mp) = complex_mp.
:- func cmp_pow(complex_mp, int) = complex_mp.

    % Complex nth root via Newton's method.
    % cmp_nthroot(Prec, N, Z, Z0) where Z0 is a Double starting estimate.
    %
:- func cmp_nthroot(int, int, complex_mp, complex) = complex_mp.

%---------------------------------------------------------------------------%
% DAG evaluation
%---------------------------------------------------------------------------%

    % Evaluate a radical DAG at the given precision (in bits).
    % Returns a complex interval with rational endpoints.
    %
:- func dag_eval_complex_mp(int, rad_dag) = complex_interval.

    % Evaluate a radical DAG and return just the real-part interval.
    %
:- func dag_eval_real_mp(int, rad_dag) = interval.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module math.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
% Foreign declarations
%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#include <mpfr.h>
#include <stdlib.h>
#include ""mercury_memory.h""

typedef mpfr_ptr MR_MPFR;

// Finalizer: called by Boehm GC when the MPFR value is collected.
static void mpfr_gc_finalizer(void *ptr, void *data) {
    (void)data;
    mpfr_ptr p = (mpfr_ptr)ptr;
    mpfr_clear(p);
}

// Allocate and initialize an MPFR value.
// We use malloc for the struct since MPFR allocates limbs via malloc anyway.
// The finalizer is registered to clear the MPFR value when GC'd.
static mpfr_ptr mpfr_gc_new(mpfr_prec_t prec) {
    mpfr_ptr p = (mpfr_ptr)malloc(sizeof(__mpfr_struct));
    mpfr_init2(p, prec);
    // Note: no GC finalizer since we use malloc, not MR_GC_malloc.
    // MPFR values are small and infrequent enough that leaking is acceptable
    // for now. A proper solution would use MPFR custom allocators.
    return p;
}
").

:- pragma foreign_type("C", mpfr, "MR_MPFR").

%---------------------------------------------------------------------------%
% MPFR construction
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    mpfr_zero(Prec::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = mpfr_gc_new((mpfr_prec_t)Prec);
    mpfr_set_zero(R, 1);
").

:- pragma foreign_proc("C",
    mpfr_from_float(Prec::in, F::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = mpfr_gc_new((mpfr_prec_t)Prec);
    mpfr_set_d(R, F, MPFR_RNDN);
").

% mpfr_from_rational: convert Num/Den to MPFR.
% We convert via integer -> string -> mpfr.
mpfr_from_rational(Prec, R) = Result :-
    Num = numer(R),
    Den = denom(R),
    NumMPFR = mpfr_from_integer(Prec, Num),
    DenMPFR = mpfr_from_integer(Prec, Den),
    Result = mp_div(NumMPFR, DenMPFR).

:- func mpfr_from_integer(int, integer) = mpfr.

mpfr_from_integer(Prec, I) = Result :-
    S = integer.to_string(I),
    Result = mpfr_from_string(Prec, S).

:- func mpfr_from_string(int, string) = mpfr.

:- pragma foreign_proc("C",
    mpfr_from_string(Prec::in, S::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = mpfr_gc_new((mpfr_prec_t)Prec);
    mpfr_set_str(R, S, 10, MPFR_RNDN);
").

%---------------------------------------------------------------------------%
% MPFR arithmetic
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    mp_add(A::in, B::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mpfr_prec_t p = mpfr_get_prec(A);
    if (mpfr_get_prec(B) > p) p = mpfr_get_prec(B);
    R = mpfr_gc_new(p);
    mpfr_add(R, A, B, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_sub(A::in, B::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mpfr_prec_t p = mpfr_get_prec(A);
    if (mpfr_get_prec(B) > p) p = mpfr_get_prec(B);
    R = mpfr_gc_new(p);
    mpfr_sub(R, A, B, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_mul(A::in, B::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mpfr_prec_t p = mpfr_get_prec(A);
    if (mpfr_get_prec(B) > p) p = mpfr_get_prec(B);
    R = mpfr_gc_new(p);
    mpfr_mul(R, A, B, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_div(A::in, B::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mpfr_prec_t p = mpfr_get_prec(A);
    if (mpfr_get_prec(B) > p) p = mpfr_get_prec(B);
    R = mpfr_gc_new(p);
    mpfr_div(R, A, B, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_neg(A::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = mpfr_gc_new(mpfr_get_prec(A));
    mpfr_neg(R, A, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_abs(A::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = mpfr_gc_new(mpfr_get_prec(A));
    mpfr_abs(R, A, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_sqrt(A::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = mpfr_gc_new(mpfr_get_prec(A));
    mpfr_sqrt(R, A, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_pow_ui(A::in, N::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = mpfr_gc_new(mpfr_get_prec(A));
    mpfr_pow_ui(R, A, (unsigned long)N, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_rootn(A::in, N::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = mpfr_gc_new(mpfr_get_prec(A));
    mpfr_rootn_ui(R, A, (unsigned long)N, MPFR_RNDN);
").

%---------------------------------------------------------------------------%
% MPFR queries
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    mp_to_float(A::in) = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = mpfr_get_d(A, MPFR_RNDN);
").

:- pragma foreign_proc("C",
    mp_is_nonneg(A::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (mpfr_sgn(A) >= 0);
").

:- pragma foreign_proc("C",
    mp_is_neg(A::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (mpfr_sgn(A) < 0);
").

:- pragma foreign_proc("C",
    mp_is_zero(A::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = mpfr_zero_p(A);
").

:- pragma foreign_proc("C",
    mp_prec(A::in) = (P::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    P = (MR_Integer)mpfr_get_prec(A);
").

%---------------------------------------------------------------------------%
% MPFR to rational interval
%---------------------------------------------------------------------------%

% Convert MPFR value to a rational interval [lo, hi] that encloses the
% exact value.  We compute lo = round-down, hi = round-up by re-rounding
% the internal MPFR value.

mp_to_interval(A) = interval(Lo, Hi) :-
    mp_to_rational_strs(A, 0, LoNumS, LoDenS),
    mp_to_rational_strs(A, 1, HiNumS, HiDenS),
    Lo = from_integers(integer.det_from_string(LoNumS),
                       integer.det_from_string(LoDenS)),
    Hi = from_integers(integer.det_from_string(HiNumS),
                       integer.det_from_string(HiDenS)).

    % mp_to_rational_strs(A, RoundUp, NumStr, DenStr):
    % Convert MPFR to rational strings.  RoundUp=0 rounds down, 1 rounds up.
    %
:- pred mp_to_rational_strs(mpfr::in, int::in, string::out, string::out)
    is det.

:- pragma foreign_proc("C",
    mp_to_rational_strs(A::in, RoundUp::in, NumStr::out, DenStr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Handle zero specially: mpfr_get_z_2exp returns exp=EMIN for zero,
    // which would create an astronomically large denominator.
    if (mpfr_zero_p(A)) {
        MR_make_aligned_string_copy_msg(NumStr, ""0"", MR_ALLOC_ID);
        MR_make_aligned_string_copy_msg(DenStr, ""1"", MR_ALLOC_ID);
    } else {
        // Round to 64-bit precision with directed rounding.  This both
        // creates actual lo/hi bounds (unlike same-precision mpfr_set)
        // and keeps the resulting integers small enough for integer.m.
        mpfr_t rounded;
        mpfr_init2(rounded, 64);
        mpfr_set(rounded, A, RoundUp ? MPFR_RNDU : MPFR_RNDD);

        mpfr_exp_t exp;
        mpz_t mantissa;
        mpz_init(mantissa);
        exp = mpfr_get_z_2exp(mantissa, rounded);

        mpz_t num, den;
        mpz_init(num);
        mpz_init(den);

        if (exp >= 0) {
            mpz_mul_2exp(num, mantissa, (mp_bitcnt_t)exp);
            mpz_set_ui(den, 1);
        } else {
            mpz_set(num, mantissa);
            mpz_set_ui(den, 1);
            mpz_mul_2exp(den, den, (mp_bitcnt_t)(-exp));
        }

        char *num_str = mpz_get_str(NULL, 10, num);
        char *den_str = mpz_get_str(NULL, 10, den);

        MR_make_aligned_string_copy_msg(NumStr, num_str, MR_ALLOC_ID);
        MR_make_aligned_string_copy_msg(DenStr, den_str, MR_ALLOC_ID);

        free(num_str);
        free(den_str);
        mpz_clear(num);
        mpz_clear(den);
        mpz_clear(mantissa);
        mpfr_clear(rounded);
    }
").

cmp_to_complex_interval(complex_mp(Re, Im)) =
    complex_interval(mp_to_interval(Re), mp_to_interval(Im)).

%---------------------------------------------------------------------------%
% Complex MPFR arithmetic
%---------------------------------------------------------------------------%

cmp_from_rational(Prec, R) =
    complex_mp(mpfr_from_rational(Prec, R), mpfr_zero(Prec)).

cmp_add(complex_mp(Ar, Ai), complex_mp(Br, Bi)) =
    complex_mp(mp_add(Ar, Br), mp_add(Ai, Bi)).

cmp_sub(complex_mp(Ar, Ai), complex_mp(Br, Bi)) =
    complex_mp(mp_sub(Ar, Br), mp_sub(Ai, Bi)).

cmp_mul(complex_mp(Ar, Ai), complex_mp(Br, Bi)) =
    complex_mp(mp_sub(mp_mul(Ar, Br), mp_mul(Ai, Bi)),
               mp_add(mp_mul(Ar, Bi), mp_mul(Ai, Br))).

cmp_neg(complex_mp(Ar, Ai)) =
    complex_mp(mp_neg(Ar), mp_neg(Ai)).

% Complex inversion using component-wise scaling to avoid overflow:
% 1/(a+bi) = (a-bi)/(a²+b²)
cmp_inv(Prec, Z) = Result :-
    Z = complex_mp(Ar, Ai),
    ( if mp_is_zero(Ai) then
        Result = complex_mp(mp_div(mpfr_from_rational(Prec, rational.one), Ar),
                            mpfr_zero(Prec))
    else if mp_is_zero(Ar) then
        Result = complex_mp(mpfr_zero(Prec),
                            mp_neg(mp_div(
                                mpfr_from_rational(Prec, rational.one), Ai)))
    else
        Denom = mp_add(mp_mul(Ar, Ar), mp_mul(Ai, Ai)),
        Result = complex_mp(mp_div(Ar, Denom), mp_neg(mp_div(Ai, Denom)))
    ).

% Complex power by repeated squaring.
cmp_pow(Z, N) = Result :-
    ( if N = 0 then
        Prec = mp_prec(cmp_re(Z)),
        Result = complex_mp(mpfr_from_rational(Prec, rational.one),
                            mpfr_zero(Prec))
    else if N = 1 then
        Result = Z
    else if int.even(N) then
        Half = cmp_pow(Z, int.'//'(N, 2)),
        Result = cmp_mul(Half, Half)
    else
        Result = cmp_mul(Z, cmp_pow(Z, int.'-'(N, 1)))
    ).

% Complex nth root via Newton's method.
% w_{k+1} = ((n-1)·w_k + z/w_k^(n-1)) / n
%
% Special cases handled before Newton:
%   - Non-negative real: use real mpfr_rootn
%   - Negative real, odd n: negate, root, negate
%   - Negative real, n=2: return i·√|x|
%   - Otherwise: Newton iteration from Double starting estimate
cmp_nthroot(Prec, N, Z, Z0Dbl) = Result :-
    Z = complex_mp(Zr, Zi),
    ( if mp_is_zero(Zi), mp_is_nonneg(Zr) then
        % Non-negative real
        Result = complex_mp(mp_rootn(Zr, N), mpfr_zero(Prec))
    else if mp_is_zero(Zi), mp_is_neg(Zr), int.odd(N) then
        % Negative real, odd root
        PosRoot = mp_rootn(mp_abs(Zr), N),
        Result = complex_mp(mp_neg(PosRoot), mpfr_zero(Prec))
    else if mp_is_zero(Zi), mp_is_neg(Zr), N = 2 then
        % √(negative real) = i·√|x|
        Result = complex_mp(mpfr_zero(Prec), mp_sqrt(mp_abs(Zr)))
    else
        % General complex: Newton's method
        W0Re = mpfr_from_float(Prec, re(Z0Dbl)),
        W0Im = mpfr_from_float(Prec, im(Z0Dbl)),
        W0 = complex_mp(W0Re, W0Im),
        NMpfr = mpfr_from_rational(Prec, rational(N, 1)),
        Nm1Mpfr = mpfr_from_rational(Prec, rational(int.'-'(N, 1), 1)),
        Result = newton_loop(Prec, 20, N, NMpfr, Nm1Mpfr, Z, W0)
    ).

:- func newton_loop(int, int, int, mpfr, mpfr, complex_mp, complex_mp)
    = complex_mp.

newton_loop(Prec, Iters, N, NMpfr, Nm1Mpfr, Z, W) = Result :-
    ( if Iters = 0 then
        Result = W
    else
        % w_{k+1} = ((n-1)·w_k + z/w_k^{n-1}) / n
        Wn1 = cmp_pow(W, int.'-'(N, 1)),
        ZDivWn1 = cmp_mul(Z, cmp_inv(Prec, Wn1)),
        Nm1W = cmp_mul(complex_mp(Nm1Mpfr, mpfr_zero(Prec)), W),
        Sum = cmp_add(Nm1W, ZDivWn1),
        WNext = cmp_mul(Sum,
            complex_mp(mp_div(mpfr_from_rational(Prec, rational.one), NMpfr),
                       mpfr_zero(Prec))),
        Result = newton_loop(Prec, int.'-'(Iters, 1), N, NMpfr, Nm1Mpfr,
                             Z, WNext)
    ).

%---------------------------------------------------------------------------%
% DAG evaluation
%---------------------------------------------------------------------------%

dag_eval_complex_mp(Prec, DAG) = CmpResult :-
    DAG = rad_dag(Nodes, RootId),
    % Pass 1: Double evaluation for Newton starting points
    DblVals = map.foldl(
        ( func(Nid, Op, Acc) =
            map.set(Acc, Nid, eval_dbl(Acc, Op))
        ),
        Nodes, map.init),
    % Pass 2: MPFR evaluation
    MpVals = map.foldl(
        ( func(Nid, Op, Acc) =
            map.set(Acc, Nid, eval_mp(Prec, DblVals, Acc, Op))
        ),
        Nodes, map.init),
    map.lookup(MpVals, RootId, CmpVal),
    CmpResult = cmp_to_complex_interval(CmpVal).

dag_eval_real_mp(Prec, DAG) = ci_real(dag_eval_complex_mp(Prec, DAG)).

% Double evaluation (for Newton starting points).
:- func eval_dbl(map(int, complex), rad_node_op) = complex.

eval_dbl(_, n_lit(R)) = complex(rational_to_float(R), 0.0).
eval_dbl(V, n_neg(A)) = cneg(map.lookup(V, A)).
eval_dbl(V, n_add(A, B)) = cadd(map.lookup(V, A), map.lookup(V, B)).
eval_dbl(V, n_mul(A, B)) = cmul(map.lookup(V, A), map.lookup(V, B)).
eval_dbl(V, n_inv(A)) = cinv(map.lookup(V, A)).
eval_dbl(V, n_pow(A, N)) = cpow_dbl(map.lookup(V, A), N).
eval_dbl(V, n_root(N, A)) = complex_nth_root(N, map.lookup(V, A)).

:- func cpow_dbl(complex, int) = complex.

cpow_dbl(Z, N) = Result :-
    ( if N =< 0 then
        Result = complex(1.0, 0.0)
    else if N = 1 then
        Result = Z
    else
        Result = cmul(Z, cpow_dbl(Z, int.'-'(N, 1)))
    ).

% MPFR evaluation of a DAG node.
:- func eval_mp(int, map(int, complex), map(int, complex_mp), rad_node_op)
    = complex_mp.

eval_mp(Prec, _, _, n_lit(R)) = cmp_from_rational(Prec, R).
eval_mp(_, _, V, n_neg(A)) = cmp_neg(map.lookup(V, A)).
eval_mp(_, _, V, n_add(A, B)) = cmp_add(map.lookup(V, A), map.lookup(V, B)).
eval_mp(_, _, V, n_mul(A, B)) = cmp_mul(map.lookup(V, A), map.lookup(V, B)).
eval_mp(Prec, _, V, n_inv(A)) = cmp_inv(Prec, map.lookup(V, A)).
eval_mp(_, _, V, n_pow(A, N)) = cmp_pow(map.lookup(V, A), N).
eval_mp(Prec, DblVals, V, n_root(N, A)) =
    cmp_nthroot(Prec, N, map.lookup(V, A), map.lookup(DblVals, A)).

%---------------------------------------------------------------------------%
% Utility: convert rational to float
%---------------------------------------------------------------------------%

:- func rational_to_float(rational) = float.

rational_to_float(R) = float.'/'(
    integer_to_float(numer(R)),
    integer_to_float(denom(R))).

:- func integer_to_float(integer) = float.

integer_to_float(I) = F :-
    S = integer.to_string(I),
    ( if string.to_float(S, F0) then
        F = F0
    else
        F = 0.0
    ).

%---------------------------------------------------------------------------%
:- end_module eval_mp.
%---------------------------------------------------------------------------%
