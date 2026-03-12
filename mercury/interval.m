%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: interval.m
% Main author: nyc
%
% Interval arithmetic with rational endpoints for root isolation
% and numerical evaluation.
%
%---------------------------------------------------------------------------%

:- module interval.
:- interface.

:- import_module rational.

%---------------------------------------------------------------------------%
% Real intervals
%---------------------------------------------------------------------------%

    % A closed interval [lo, hi] with rational endpoints.
    %
:- type interval
    --->    interval(
                iv_lo :: rational,
                iv_hi :: rational
            ).

:- func midpoint(interval) = rational.
:- func width(interval) = rational.
:- pred contains(interval::in, rational::in) is semidet.
:- pred iv_overlaps(interval::in, interval::in) is semidet.
:- func bisect_lo(interval) = interval.
:- func bisect_hi(interval) = interval.
:- func from_rational(rational) = interval.

    % Interval arithmetic.
    %
:- func iadd(interval, interval) = interval.
:- func isub(interval, interval) = interval.
:- func imul(interval, interval) = interval.
:- func iinv(interval) = interval.
:- func idiv(interval, interval) = interval.
:- func ipow(interval, int) = interval.
:- func isqrt(interval) = interval.
:- func inth(int, interval) = interval.
:- func iabs(interval) = interval.

:- pred strictly_positive(interval::in) is semidet.
:- pred strictly_negative(interval::in) is semidet.
:- pred contains_zero(interval::in) is semidet.

%---------------------------------------------------------------------------%
% Complex intervals
%---------------------------------------------------------------------------%

    % A complex interval: real part × imaginary part.
    %
:- type complex_interval
    --->    complex_interval(
                ci_real :: interval,
                ci_imag :: interval
            ).

:- func ci_from_rational(rational) = complex_interval.
:- func ci_from_real(interval) = complex_interval.
:- func ciadd(complex_interval, complex_interval) = complex_interval.
:- func cisub(complex_interval, complex_interval) = complex_interval.
:- func cimul(complex_interval, complex_interval) = complex_interval.
:- func ciinv(complex_interval) = complex_interval.
:- func cineg(complex_interval) = complex_interval.
:- func cipow(complex_interval, int) = complex_interval.
:- func ci_magnitude_sq(complex_interval) = interval.
:- func ci_real_part(complex_interval) = interval.
:- func ci_imag_part(complex_interval) = interval.

%---------------------------------------------------------------------------%
% Trigonometric interval functions
%---------------------------------------------------------------------------%

:- func iatan(interval) = interval.
:- func icos(interval) = interval.
:- func isin(interval) = interval.
:- func iatan2(interval, interval) = interval.
:- func pi_interval = interval.
:- func cinthroot(int, complex_interval) = complex_interval.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module integer.
:- import_module list.
:- import_module require.

%---------------------------------------------------------------------------%
% Real interval basics
%---------------------------------------------------------------------------%

midpoint(interval(L, H)) = rational.'/'(rational.'+'(L, H), rational(2)).

width(interval(L, H)) = rational.'-'(H, L).

contains(interval(L, H), X) :-
    rational.'=<'(L, X),
    rational.'=<'(X, H).

iv_overlaps(interval(L1, H1), interval(L2, H2)) :-
    rational.'=<'(L1, H2),
    rational.'=<'(L2, H1).

bisect_lo(Iv) = interval(iv_lo(Iv), midpoint(Iv)).
bisect_hi(Iv) = interval(midpoint(Iv), iv_hi(Iv)).

from_rational(X) = interval(X, X).

%---------------------------------------------------------------------------%
% Interval arithmetic
%---------------------------------------------------------------------------%

iadd(interval(L1, H1), interval(L2, H2)) =
    interval(rational.'+'(L1, L2), rational.'+'(H1, H2)).

isub(interval(L1, H1), interval(L2, H2)) =
    interval(rational.'-'(L1, H2), rational.'-'(H1, L2)).

imul(interval(L1, H1), interval(L2, H2)) = interval(Min, Max) :-
    A = rational.'*'(L1, L2),
    B = rational.'*'(L1, H2),
    C = rational.'*'(H1, L2),
    D = rational.'*'(H1, H2),
    Min = rational_min(rational_min(A, B), rational_min(C, D)),
    Max = rational_max(rational_max(A, B), rational_max(C, D)).

iinv(interval(L, H)) = Result :-
    ( if rational.'>'(L, rational.zero) then
        Result = interval(rational.'/'(rational.one, H),
                          rational.'/'(rational.one, L))
    else if rational.'<'(H, rational.zero) then
        Result = interval(rational.'/'(rational.one, H),
                          rational.'/'(rational.one, L))
    else
        unexpected($pred, "interval contains zero")
    ).

idiv(A, B) = imul(A, iinv(B)).

ipow(Iv, N) = Result :-
    ( if N = 0 then
        Result = interval(rational.one, rational.one)
    else if int.'<'(N, 0) then
        Result = iinv(ipow(Iv, int.'-'(0, N)))
    else if int.odd(N) then
        Result = fold_imul(N, Iv)
    else
        % Even power: result is non-negative
        L = iv_lo(Iv),
        H = iv_hi(Iv),
        PL = rat_pow(L, N),
        PH = rat_pow(H, N),
        ( if rational.'=<'(L, rational.zero),
             rational.'=<'(rational.zero, H)
        then
            Result = interval(rational.zero, rational_max(PL, PH))
        else
            Result = interval(rational_min(PL, PH), rational_max(PL, PH))
        )
    ).

isqrt(interval(L, H)) = Result :-
    ( if rational.'<'(L, rational.zero) then
        unexpected($pred, "negative interval")
    else if H = rational.zero then
        Result = interval(rational.zero, rational.zero)
    else
        Result = interval(nth_root_lower(2, L), nth_root_upper(2, H))
    ).

inth(N, interval(L, H)) = Result :-
    ( if int.'=<'(N, 0) then
        unexpected($pred, "non-positive root index")
    else if N = 1 then
        Result = interval(L, H)
    else if N = 2 then
        Result = isqrt(interval(L, H))
    else if int.even(N), rational.'<'(L, rational.zero) then
        unexpected($pred, "even root of negative interval")
    else if H = rational.zero, L = rational.zero then
        Result = interval(rational.zero, rational.zero)
    else if int.odd(N), rational.'<'(H, rational.zero) then
        % Odd root of entirely negative: negate, root, negate
        interval(L1, H1) = inth(N,
            interval(rational.'-'(H), rational.'-'(L))),
        Result = interval(rational.'-'(H1), rational.'-'(L1))
    else if int.odd(N), rational.'<'(L, rational.zero) then
        % Odd root spanning zero
        NegPart = nth_root_upper(N, rational.'-'(L)),
        PosPart = nth_root_upper(N, H),
        Result = interval(rational.'-'(NegPart), PosPart)
    else
        Result = interval(nth_root_lower(N, L), nth_root_upper(N, H))
    ).

iabs(interval(L, H)) = Result :-
    ( if rational.'>='(L, rational.zero) then
        Result = interval(L, H)
    else if rational.'=<'(H, rational.zero) then
        Result = interval(rational.'-'(H), rational.'-'(L))
    else
        Result = interval(rational.zero,
            rational_max(rational.'-'(L), H))
    ).

strictly_positive(interval(L, _)) :- rational.'>'(L, rational.zero).
strictly_negative(interval(_, H)) :- rational.'<'(H, rational.zero).
contains_zero(interval(L, H)) :-
    rational.'=<'(L, rational.zero),
    rational.'>='(H, rational.zero).

%---------------------------------------------------------------------------%
% Nth root bisection helpers
%---------------------------------------------------------------------------%

:- func nth_root_lower(int, rational) = rational.

nth_root_lower(N, A) = Result :-
    ( if A = rational.zero then
        Result = rational.zero
    else
        Upper = rational_max(A, rational.one),
        Result = bisect_down(N, A, rational.zero, Upper, 60)
    ).

:- func nth_root_upper(int, rational) = rational.

nth_root_upper(N, A) = Result :-
    ( if A = rational.zero then
        Result = rational.zero
    else
        Upper = rational.'+'(rational_max(A, rational.one), rational.one),
        Result = bisect_up(N, A, rational.zero, Upper, 60)
    ).

:- func bisect_down(int, rational, rational, rational, int) = rational.

bisect_down(N, A, Lo, Hi, Iters) = Result :-
    ( if int.'=<'(Iters, 0) then
        Result = Lo
    else
        Mid = rational.'/'(rational.'+'(Lo, Hi), rational(2)),
        ( if rational.'=<'(rat_pow(Mid, N), A) then
            Result = bisect_down(N, A, Mid, Hi, int.'-'(Iters, 1))
        else
            Result = bisect_down(N, A, Lo, Mid, int.'-'(Iters, 1))
        )
    ).

:- func bisect_up(int, rational, rational, rational, int) = rational.

bisect_up(N, A, Lo, Hi, Iters) = Result :-
    ( if int.'=<'(Iters, 0) then
        Result = Hi
    else
        Mid = rational.'/'(rational.'+'(Lo, Hi), rational(2)),
        ( if rational.'>='(rat_pow(Mid, N), A) then
            Result = bisect_up(N, A, Lo, Mid, int.'-'(Iters, 1))
        else
            Result = bisect_up(N, A, Mid, Hi, int.'-'(Iters, 1))
        )
    ).

%---------------------------------------------------------------------------%
% Complex intervals
%---------------------------------------------------------------------------%

ci_from_rational(R) =
    complex_interval(from_rational(R), from_rational(rational.zero)).

ci_from_real(R) =
    complex_interval(R, from_rational(rational.zero)).

ciadd(complex_interval(R1, I1), complex_interval(R2, I2)) =
    complex_interval(iadd(R1, R2), iadd(I1, I2)).

cisub(complex_interval(R1, I1), complex_interval(R2, I2)) =
    complex_interval(isub(R1, R2), isub(I1, I2)).

cineg(complex_interval(R, I)) =
    complex_interval(
        interval(rational.'-'(iv_hi(R)), rational.'-'(iv_lo(R))),
        interval(rational.'-'(iv_hi(I)), rational.'-'(iv_lo(I)))).

cimul(complex_interval(R1, I1), complex_interval(R2, I2)) =
    complex_interval(isub(imul(R1, R2), imul(I1, I2)),
                     iadd(imul(R1, I2), imul(I1, R2))).

ciinv(complex_interval(R, I)) = Result :-
    MagSq = iadd(imul(R, R), imul(I, I)),
    NegI = interval(rational.'-'(iv_hi(I)), rational.'-'(iv_lo(I))),
    Result = complex_interval(idiv(R, MagSq), idiv(NegI, MagSq)).

cipow(Z, N) = Result :-
    ( if N = 0 then
        Result = ci_from_rational(rational.one)
    else if N = 1 then
        Result = Z
    else if int.'<'(N, 0) then
        Result = cipow(ciinv(Z), int.'-'(0, N))
    else if int.even(N) then
        Half = cipow(Z, int.'//'(N, 2)),
        Result = cimul(Half, Half)
    else
        Result = cimul(Z, cipow(Z, int.'-'(N, 1)))
    ).

ci_magnitude_sq(complex_interval(R, I)) = iadd(imul(R, R), imul(I, I)).

ci_real_part(complex_interval(R, _)) = R.
ci_imag_part(complex_interval(_, I)) = I.

%---------------------------------------------------------------------------%
% Trigonometric interval functions
%---------------------------------------------------------------------------%

pi_interval = Result :-
    A = iatan_small(rational.'/'(rational.one, rational(5))),
    B = iatan_small(rational.'/'(rational.one, rational(239))),
    Four = interval(rational(4), rational(4)),
    Result = imul(Four, isub(imul(Four, A), B)).

:- func iatan_small(rational) = interval.

iatan_small(R) = interval(
        rational.'-'(PartialSum, RemBound),
        rational.'+'(PartialSum, RemBound)) :-
    NTerms = 50,
    partial_sum_atan(R, 0, NTerms, rational.zero, PartialSum),
    AR = rational.abs(R),
    RemBound = rational.'/'(
        rat_pow(AR, int.'+'(int.'*'(2, NTerms), 1)),
        rat_from_int(int.'+'(int.'*'(2, NTerms), 1))).

:- pred partial_sum_atan(rational::in, int::in, int::in,
    rational::in, rational::out) is det.

partial_sum_atan(R, K, N, Acc0, Result) :-
    ( if int.'>='(K, N) then
        Result = Acc0
    else
    Exp = int.'+'(int.'*'(2, K), 1),
    Sign = ( if int.even(K) then rational.one
             else rational.'-'(rational.one) ),
    Term = rational.'*'(Sign,
        rational.'/'(rat_pow(R, Exp), rat_from_int(Exp))),
    partial_sum_atan(R, int.'+'(K, 1), N, rational.'+'(Acc0, Term), Result)
    ).

iatan(interval(L, H)) =
    interval(iv_lo(iatan_point(L)), iv_hi(iatan_point(H))).

:- func iatan_point(rational) = interval.

iatan_point(R) = Result :-
    ( if R = rational.zero then
        Result = interval(rational.zero, rational.zero)
    else if rational.'=<'(rational.abs(R), rational.one) then
        Result = iatan_small(R)
    else if rational.'>'(R, rational.zero) then
        PiHalf = idiv(pi_interval, interval(rational(2), rational(2))),
        Result = isub(PiHalf,
            iatan_small(rational.'/'(rational.one, R)))
    else
        PiHalf = idiv(pi_interval, interval(rational(2), rational(2))),
        NegPiHalf = interval(rational.'-'(iv_hi(PiHalf)),
                             rational.'-'(iv_lo(PiHalf))),
        Result = iadd(NegPiHalf,
            iatan_small(rational.'/'(rational.one, rational.'-'(R))))
    ).

:- func icos_point(rational) = interval.

icos_point(X) = interval(
        rational.'-'(PartialSum, RemBound),
        rational.'+'(PartialSum, RemBound)) :-
    NTerms = 30,
    partial_sum_cos(X, 0, NTerms, rational.zero, PartialSum),
    RemBound = rational.'/'(
        rat_pow(rational.abs(X), int.'*'(2, NTerms)),
        factorial(int.'*'(2, NTerms))).

:- pred partial_sum_cos(rational::in, int::in, int::in,
    rational::in, rational::out) is det.

partial_sum_cos(X, K, N, Acc0, Result) :-
    ( if int.'>='(K, N) then
        Result = Acc0
    else
    Exp = int.'*'(2, K),
    Sign = ( if int.even(K) then rational.one
             else rational.'-'(rational.one) ),
    Term = rational.'*'(Sign,
        rational.'/'(rat_pow(X, Exp), factorial(Exp))),
    partial_sum_cos(X, int.'+'(K, 1), N, rational.'+'(Acc0, Term), Result)
    ).

:- func factorial(int) = rational.

factorial(N) = rat_from_int_factorial(N).

:- func rat_from_int_factorial(int) = rational.

rat_from_int_factorial(N) = Result :-
    factorial_acc(N, integer.one, Prod),
    Result = rational.rational(integer.det_to_int(Prod)).

:- pred factorial_acc(int::in, integer::in, integer::out) is det.

factorial_acc(N, Acc, Result) :-
    ( if int.'=<'(N, 0) then
        Result = Acc
    else
        factorial_acc(int.'-'(N, 1),
            integer.'*'(Acc, integer.integer(N)), Result)
    ).

icos(Iv) = Result :-
    L = iv_lo(Iv),
    H = iv_hi(Iv),
    ( if L = H then
        Result = icos_point(L)
    else
        CosL = icos_point(L),
        CosH = icos_point(H),
        BaseLo = rational_min(iv_lo(CosL), iv_lo(CosH)),
        BaseHi = rational_max(iv_hi(CosL), iv_hi(CosH)),
        PiLo = iv_lo(pi_interval),
        PiHi = iv_hi(pi_interval),
        % Check if any kπ ∈ [L, H]
        KMin = rat_ceiling(rational.'/'(L, PiHi)),
        KMax = rat_floor(rational.'/'(H, PiLo)),
        has_even_in_range(KMin, KMax, HasMax1),
        has_odd_in_range(KMin, KMax, HasMinNeg1),
        FinalLo = ( if HasMinNeg1 = yes then
            rational_min(BaseLo, rational(-1)) else BaseLo ),
        FinalHi = ( if HasMax1 = yes then
            rational_max(BaseHi, rational.one) else BaseHi ),
        ClampedLo = rational_max(rational(-1), FinalLo),
        ClampedHi = rational_min(rational.one, FinalHi),
        ( if rational.'>'(width(Iv),
                rational.'*'(iv_hi(pi_interval), rational(2)))
        then
            Result = interval(rational(-1), rational.one)
        else
            Result = interval(ClampedLo, ClampedHi)
        )
    ).

isin(Iv) = Result :-
    PiHalf = idiv(pi_interval, interval(rational(2), rational(2))),
    Shifted = isub(Iv, PiHalf),
    Result = icos(Shifted).

iatan2(Y, X) = Result :-
    ( if strictly_positive(X) then
        ThetaMin = iatan_point(rational.'/'(iv_lo(Y), iv_hi(X))),
        ThetaMax = iatan_point(rational.'/'(iv_hi(Y), iv_lo(X))),
        Result = interval(iv_lo(ThetaMin), iv_hi(ThetaMax))
    else if strictly_negative(X),
            rational.'>='(iv_lo(Y), rational.zero) then
        ThetaMin = iadd(iatan_point(
            rational.'/'(iv_hi(Y), iv_lo(X))), pi_interval),
        ThetaMax = iadd(iatan_point(
            rational.'/'(iv_lo(Y), iv_hi(X))), pi_interval),
        Result = interval(iv_lo(ThetaMin), iv_hi(ThetaMax))
    else if strictly_negative(X),
            rational.'=<'(iv_hi(Y), rational.zero) then
        NegPi = interval(rational.'-'(iv_hi(pi_interval)),
                         rational.'-'(iv_lo(pi_interval))),
        ThetaMin = iadd(iatan_point(
            rational.'/'(iv_hi(Y), iv_lo(X))), NegPi),
        ThetaMax = iadd(iatan_point(
            rational.'/'(iv_lo(Y), iv_hi(X))), NegPi),
        Result = interval(iv_lo(ThetaMin), iv_hi(ThetaMax))
    else if strictly_negative(X) then
        NegPi = interval(rational.'-'(iv_hi(pi_interval)),
                         rational.'-'(iv_lo(pi_interval))),
        ThetaLow = iadd(iatan_point(
            rational.'/'(iv_lo(Y), iv_lo(X))), NegPi),
        ThetaHigh = iadd(iatan_point(
            rational.'/'(iv_hi(Y), iv_hi(X))), pi_interval),
        Result = interval(iv_lo(ThetaLow), iv_hi(ThetaHigh))
    else
        Result = interval(rational.'-'(iv_hi(pi_interval)),
                          iv_hi(pi_interval))
    ).

cinthroot(N, Z) = Result :-
    ( if int.'=<'(N, 0) then
        unexpected($pred, "non-positive root index")
    else if N = 1 then
        Result = Z
    else
        Re = ci_real(Z),
        Im = ci_imag(Z),
        MagSq = iadd(imul(Re, Re), imul(Im, Im)),
        Mag = isqrt(MagSq),
        MagRoot = inth(N, Mag),
        Theta = iatan2(Im, Re),
        NIv = interval(rat_from_int(N), rat_from_int(N)),
        ThetaN = idiv(Theta, NIv),
        CosTheta = icos(ThetaN),
        SinTheta = isin(ThetaN),
        Result = complex_interval(imul(MagRoot, CosTheta),
                                  imul(MagRoot, SinTheta))
    ).

%---------------------------------------------------------------------------%
% Utility
%---------------------------------------------------------------------------%

:- func rational_min(rational, rational) = rational.
rational_min(A, B) = ( if rational.'=<'(A, B) then A else B ).

:- func rational_max(rational, rational) = rational.
rational_max(A, B) = ( if rational.'>='(A, B) then A else B ).

:- func rat_pow(rational, int) = rational.
rat_pow(X, N) = Result :-
    ( if N = 0 then
        Result = rational.one
    else
        Result = rational.'*'(X, rat_pow(X, int.'-'(N, 1)))
    ).

:- func fold_imul(int, interval) = interval.
fold_imul(N, Iv) = Result :-
    ( if int.'=<'(N, 1) then
        Result = Iv
    else
        Result = imul(Iv, fold_imul(int.'-'(N, 1), Iv))
    ).

:- func rat_from_int(int) = rational.
rat_from_int(N) = rational.rational(N).

    % Ceiling of a rational: smallest integer >= r.
    %
:- func rat_ceiling(rational) = int.
rat_ceiling(R) = Result :-
    N = numer(R),
    D = denom(R),
    Q = integer.'//'(N, D),
    Rem = integer.rem(N, D),
    ( if Rem = integer.zero then
        Result = integer.det_to_int(Q)
    else if integer.'>'(Rem, integer.zero) then
        Result = integer.det_to_int(integer.'+'(Q, integer.one))
    else
        Result = integer.det_to_int(Q)
    ).

    % Floor of a rational: largest integer <= r.
    %
:- func rat_floor(rational) = int.
rat_floor(R) = Result :-
    N = numer(R),
    D = denom(R),
    Q = integer.'//'(N, D),
    Rem = integer.rem(N, D),
    ( if Rem = integer.zero then
        Result = integer.det_to_int(Q)
    else if integer.'<'(Rem, integer.zero) then
        Result = integer.det_to_int(integer.'-'(Q, integer.one))
    else
        Result = integer.det_to_int(Q)
    ).

:- type yes_no ---> yes ; no.

:- pred has_even_in_range(int::in, int::in, yes_no::out) is det.
has_even_in_range(Lo, Hi, Result) :-
    ( if int.'>'(Lo, Hi) then
        Result = no
    else
        % Smallest even >= Lo
        EvenLo = ( if int.even(Lo) then Lo else int.'+'(Lo, 1) ),
        ( if int.'=<'(EvenLo, Hi) then
            Result = yes
        else
            Result = no
        )
    ).

:- pred has_odd_in_range(int::in, int::in, yes_no::out) is det.
has_odd_in_range(Lo, Hi, Result) :-
    ( if int.'>'(Lo, Hi) then
        Result = no
    else
        OddLo = ( if int.odd(Lo) then Lo else int.'+'(Lo, 1) ),
        ( if int.'=<'(OddLo, Hi) then
            Result = yes
        else
            Result = no
        )
    ).

%---------------------------------------------------------------------------%
:- end_module interval.
%---------------------------------------------------------------------------%
