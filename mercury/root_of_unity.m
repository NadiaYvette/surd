%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: root_of_unity.m
% Main author: nyc
%
% Express roots of unity as radical expressions.
%
% A primitive nth root of unity ζₙ = e^(2πi/n) can ALWAYS be expressed
% in radicals, since cyclotomic extensions have abelian Galois groups.
%
% The compass-and-straightedge constructible case (n = 2^a · distinct
% Fermat primes) is the special case where only square roots are needed.
%
%---------------------------------------------------------------------------%

:- module root_of_unity.
:- interface.

:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Compute cos(2π/n) as a radical expression.
    %
:- func cos_of_unity(int) = maybe(rad_expr(rational)).

    % Compute sin(2π/n) as a radical expression.
    %
:- func sin_of_unity(int) = maybe(rad_expr(rational)).

    % All cos(2πk/n) for k coprime to n, from Gauss periods.
    %
:- func all_cos_of_unity(int) = maybe(map(int, rad_expr(rational))).

    % All sin(2πk/n) for k coprime to n, from Gauss periods.
    %
:- func all_sin_of_unity(int) = maybe(map(int, rad_expr(rational))).

    % Check if cos(2π/n) is constructible (only square roots needed).
    %
:- pred is_constructible(int::in) is semidet.

    % Known Fermat primes.
    %
:- func fermat_primes = list(integer).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module positive.
:- import_module prime_factors.
:- import_module trig_galois.

%---------------------------------------------------------------------------%
% Fermat primes
%---------------------------------------------------------------------------%

fermat_primes = [integer(3), integer(5), integer(17),
                 integer(257), integer(65537)].

is_constructible(N) :-
    Fs = factorise(det_positive(integer(N))),
    list.all_true(
        ( pred(PP::in) is semidet :-
            ( if pp_prime(PP) = integer(2) then
                true
            else
                pp_exponent(PP) = 1,
                list.member(pp_prime(PP), fermat_primes)
            )
        ), Fs).

%---------------------------------------------------------------------------%
% cos(2π/n)
%---------------------------------------------------------------------------%

cos_of_unity(N) = Result :-
    ( if int.'=<'(N, 0) then
        Result = no
    else if N = 1 then
        Result = yes(re_lit(rational.one))
    else if N = 2 then
        Result = yes(re_lit(rational.'-'(rational.zero, rational.one)))
    else if N = 3 then
        Result = yes(re_lit(from_integers(integer.'-'(integer.zero,
            integer.one), integer(2))))
    else if N = 4 then
        Result = yes(re_lit(rational.zero))
    else if N = 5 then
        Result = yes(cos_2pi_over_5)
    else if N = 6 then
        Result = yes(re_lit(from_integers(integer.one, integer(2))))
    else if N = 8 then
        Result = yes(re_mul(re_inv(re_lit(rational(2))),
                            re_root(2, re_lit(rational(2)))))
    else if N = 10 then
        Result = yes(cos_2pi_over_10)
    else if N = 12 then
        Result = yes(re_mul(re_inv(re_lit(rational(2))),
                            re_root(2, re_lit(rational(3)))))
    else if N = 15 then
        Result = yes(cos_2pi_over_15)
    else if N = 16 then
        Result = yes(cos_2pi_over_16)
    else if N = 17 then
        Result = yes(cos_2pi_over_17)
    else if N = 20 then
        Result = yes(cos_2pi_over_20)
    else if N = 24 then
        Result = yes(cos_2pi_over_24)
    else if is_power_of_2(N) then
        Result = yes(cos_of_pow2(N))
    else if is_prime(integer(N)) then
        Result = cos_of_unity_via_gauss(N)
    else if prime_power_decomp(N) = yes({P, K}),
            int.'>'(P, 2), int.'>'(K, 1) then
        Result = cos_of_unity_via_gauss(N)
    else
        Result = cos_of_unity_composite(N)
    ).

%---------------------------------------------------------------------------%
% sin(2π/n)
%---------------------------------------------------------------------------%

sin_of_unity(N) = Result :-
    ( if int.'=<'(N, 0) then
        Result = no
    else if N = 1 then
        Result = yes(re_lit(rational.zero))
    else if N = 2 then
        Result = yes(re_lit(rational.zero))
    else if N = 3 then
        Result = yes(re_mul(re_lit(from_integers(integer.one, integer(2))),
                            re_root(2, re_lit(rational(3)))))
    else if N = 4 then
        Result = yes(re_lit(rational.one))
    else if N = 6 then
        Result = yes(re_mul(re_lit(from_integers(integer.one, integer(2))),
                            re_root(2, re_lit(rational(3)))))
    else if N = 8 then
        Result = yes(re_mul(re_inv(re_lit(rational(2))),
                            re_root(2, re_lit(rational(2)))))
    else if N = 12 then
        Result = yes(re_lit(from_integers(integer.one, integer(2))))
    else
        ( if cos_of_unity(N) = yes(C) then
            Sin2 = re_add(re_lit(rational.one),
                          re_neg(re_mul(C, C))),
            Result = yes(re_root(2, Sin2))
        else
            Result = no
        )
    ).

%---------------------------------------------------------------------------%
% Hardcoded small cases
%---------------------------------------------------------------------------%

:- func cos_2pi_over_5 = rad_expr(rational).
cos_2pi_over_5 = re_mul(re_inv(re_lit(rational(4))),
    re_add(re_root(2, re_lit(rational(5))),
           re_lit(rational(-1)))).

:- func cos_2pi_over_10 = rad_expr(rational).
cos_2pi_over_10 = re_mul(re_inv(re_lit(rational(4))),
    re_add(re_lit(rational.one), re_root(2, re_lit(rational(5))))).

:- func cos_2pi_over_15 = rad_expr(rational).
cos_2pi_over_15 =
    re_mul(re_inv(re_lit(rational(8))),
        re_add(
            re_add(re_lit(rational.one), re_root(2, re_lit(rational(5)))),
            re_root(2, re_add(re_lit(rational(30)),
                re_neg(re_mul(re_lit(rational(6)),
                              re_root(2, re_lit(rational(5))))))))).

:- func cos_2pi_over_16 = rad_expr(rational).
cos_2pi_over_16 =
    re_mul(re_inv(re_lit(rational(2))),
        re_root(2, re_add(re_lit(rational(2)),
                           re_root(2, re_lit(rational(2)))))).

:- func cos_2pi_over_17 = rad_expr(rational).
cos_2pi_over_17 =
    re_mul(re_inv(re_lit(rational(16))),
        re_add(
            re_add(
                re_add(re_lit(rational(-1)), S17),
                A17),
            re_mul(re_lit(rational(2)),
                re_root(2,
                    re_add(
                        re_add(re_lit(rational(17)),
                               re_mul(re_lit(rational(3)), S17)),
                        re_add(re_neg(A17),
                               re_neg(re_mul(re_lit(rational(2)), B17))))))
        )) :-
    S17 = re_root(2, re_lit(rational(17))),
    A17 = re_root(2, re_add(re_lit(rational(34)),
                              re_neg(re_mul(re_lit(rational(2)), S17)))),
    B17 = re_root(2, re_add(re_lit(rational(34)),
                              re_mul(re_lit(rational(2)), S17))).

:- func cos_2pi_over_20 = rad_expr(rational).
cos_2pi_over_20 =
    re_mul(re_inv(re_lit(rational(4))),
        re_root(2, re_add(re_lit(rational(10)),
            re_mul(re_lit(rational(2)),
                   re_root(2, re_lit(rational(5))))))).

:- func cos_2pi_over_24 = rad_expr(rational).
cos_2pi_over_24 =
    re_mul(re_inv(re_lit(rational(4))),
        re_add(re_root(2, re_lit(rational(6))),
               re_root(2, re_lit(rational(2))))).

%---------------------------------------------------------------------------%
% Powers of 2
%---------------------------------------------------------------------------%

:- pred is_power_of_2(int::in) is semidet.

is_power_of_2(N) :-
    int.'>'(N, 0),
    int.'/\\'(N, int.'-'(N, 1)) = 0.

:- func cos_of_pow2(int) = rad_expr(rational).

cos_of_pow2(K) = Result :-
    ( if K = 1 then
        Result = re_lit(rational.one)
    else if K = 2 then
        Result = re_lit(rational.'-'(rational.zero, rational.one))
    else if K = 4 then
        Result = re_lit(rational.zero)
    else
        Half = cos_of_pow2(int.'/'(K, 2)),
        Result = re_root(2, re_mul(re_inv(re_lit(rational(2))),
                                    re_add(re_lit(rational.one), Half)))
    ).

%---------------------------------------------------------------------------%
% Prime power decomposition
%---------------------------------------------------------------------------%

:- func prime_power_decomp(int) = maybe({int, int}).

prime_power_decomp(N) = Result :-
    Fs = factorise(det_positive(integer(N))),
    ( if Fs = [prime_power(P, K)] then
        Result = yes({integer.det_to_int(P), K})
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Composite decomposition via CRT
%---------------------------------------------------------------------------%

:- func cos_of_unity_composite(int) = maybe(rad_expr(rational)).

cos_of_unity_composite(N) = Result :-
    Fs = factorise(det_positive(integer(N))),
    ( if Fs = [_ | _],
         int.'>='(list.length(Fs), 2) then
        Fs = [prime_power(P1, E1) | _],
        N1 = integer.det_to_int(integer_pow_int(P1, E1)),
        N2 = int.'/'(N, N1),
        ext_gcd_int(N1, N2, A0, B0),
        A = int.'mod'(A0, N2),
        B = int.'mod'(B0, N1),
        SA = sin_sign_from_angle(A, N2),
        SB = sin_sign_from_angle(B, N1),
        SignFactor = int.'*'(SA, SB),
        ( if cos_of_unity(N2) = yes(CosBase1),
             cos_of_unity(N1) = yes(CosBase2) then
            CosAE = chebyshev(A, CosBase1),
            CosBE = chebyshev(B, CosBase2),
            ( if SignFactor = 0 then
                Result = yes(re_mul(CosAE, CosBE))
            else
                SinAbsA = sin_from_cos(CosAE),
                SinAbsB = sin_from_cos(CosBE),
                SinProduct = re_mul(SinAbsA, SinAbsB),
                ( if SignFactor = 1 then
                    Result = yes(re_add(re_mul(CosAE, CosBE),
                                        re_neg(SinProduct)))
                else
                    Result = yes(re_add(re_mul(CosAE, CosBE),
                                        SinProduct))
                )
            )
        else
            Result = no
        )
    else
        Result = no
    ).

:- func integer_pow_int(integer, int) = integer.

integer_pow_int(B, E) = Result :-
    ( if int.'=<'(E, 0) then
        Result = integer.one
    else
        Result = integer.'*'(B, integer_pow_int(B, int.'-'(E, 1)))
    ).

:- func sin_sign_from_angle(int, int) = int.

sin_sign_from_angle(K, M) = Result :-
    K1 = int.'mod'(K, M),
    ( if K1 = 0 then
        Result = 0
    else if int.'<'(int.'*'(2, K1), M) then
        Result = 1
    else if int.'*'(2, K1) = M then
        Result = 0
    else
        Result = -1
    ).

:- pred ext_gcd_int(int::in, int::in, int::out, int::out) is det.

ext_gcd_int(X, Y, A, B) :-
    ( if X = 0 then
        A = 0,
        B = 1
    else
        Q = int.'/'(Y, X),
        R = int.'-'(Y, int.'*'(Q, X)),
        ext_gcd_int(R, X, A1, B1),
        A = int.'-'(B1, int.'*'(Q, A1)),
        B = A1
    ).

:- func sin_from_cos(rad_expr(rational)) = rad_expr(rational).

sin_from_cos(C) = re_root(2, re_add(re_lit(rational.one),
                                      re_neg(re_mul(C, C)))).

%---------------------------------------------------------------------------%
% All cos/sin of unity from Gauss periods
%---------------------------------------------------------------------------%

all_cos_of_unity(N) = Result :-
    ( if all_periods_via_gauss(N) = yes(Periods) then
        Result = yes(map.map_values(
            ( func(K, Pk) =
                re_mul(re_inv(re_lit(rational(2))),
                       re_add(Pk, Pnk)) :-
                NK = int.'-'(N, K),
                map.lookup(Periods, NK, Pnk)
            ), Periods))
    else
        Result = no
    ).

all_sin_of_unity(N) = Result :-
    ( if all_periods_via_gauss(N) = yes(Periods) then
        I = re_root(2, re_lit(rational.'-'(rational.zero, rational.one))),
        NegIOver2 = re_mul(re_inv(re_lit(rational(2))), re_neg(I)),
        Result = yes(map.map_values(
            ( func(K, Pk) =
                re_mul(NegIOver2, Diff) :-
                NK = int.'-'(N, K),
                map.lookup(Periods, NK, Pnk),
                Diff = re_add(Pk, re_neg(Pnk))
            ), Periods))
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
:- end_module root_of_unity.
%---------------------------------------------------------------------------%
