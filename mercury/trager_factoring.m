%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: trager_factoring.m
% Main author: nyc
%
% Factoring polynomials over algebraic extension fields Q(α)
% using Trager's algorithm (1976).
%
% Given f(x) in Q(α)[x] where α is a root of an irreducible m(t) in Q[t],
% compute the norm N(x) = Res_t(m(t), f(x - s*t)) in Q[x] for suitable s,
% factor N over Q, and lift factors back via GCD.
%
%---------------------------------------------------------------------------%

:- module trager_factoring.
:- interface.

:- import_module extension.
:- import_module list.
:- import_module poly.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Factor a polynomial over Q(α) into irreducible factors.
    % Returns a list of (factor, multiplicity) pairs.
    %
:- func factor_over_extension(ext_field(rational),
    poly(ext_elem(rational))) = list({poly(ext_elem(rational)), int}).

    % Factor a square-free polynomial over Q(α) into irreducible factors.
    %
:- func factor_sf_over_extension(ext_field(rational),
    poly(ext_elem(rational))) = list(poly(ext_elem(rational))).

    % Generalized: factor a square-free polynomial over K(α) given a
    % factoring function for K.
    %
:- func factor_sf_over_extension_k(
    (func(poly(K)) = list(poly(K))),
    ext_field(K),
    poly(ext_elem(K))) = list(poly(ext_elem(K))) <= field(K).

    % Compute the norm of f(x) in Q(α)[x]:
    % N(x) = Res_t(m(t), f_lifted(x, t)) in Q[x].
    %
:- func norm_poly(ext_field(rational),
    poly(ext_elem(rational))) = poly(rational).

    % Generalized norm over any base field K.
    %
:- func norm_poly_k(ext_field(K), poly(ext_elem(K))) = poly(K) <= field(K).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module factoring.
:- import_module require.
:- import_module resultant.

%---------------------------------------------------------------------------%
% Factor over Q(α)
%---------------------------------------------------------------------------%

factor_over_extension(Field, F) = Result :-
    SFFactors = square_free(F),
    Result = list.condense(list.map(
        ( func(SFF) = list.map(
            ( func(H) = {H, sff_multiplicity(SFF)} ),
            factor_sf_over_extension(Field, sff_factor(SFF)))
        ), SFFactors)).

factor_sf_over_extension(Field, F) = Result :-
    D = degree(F),
    ( if int.'=<'(D, 0) then
        Result = []
    else if D = 1 then
        Result = [monic(F)]
    else
        Result = trager_factor(Field, F, 0)
    ).

:- func trager_factor(ext_field(rational),
    poly(ext_elem(rational)), int) = list(poly(ext_elem(rational))).

trager_factor(Field, F, S) = Result :-
    ( if int.'>'(S, 20) then
        Result = [F]
    else
        Alpha = generator(Field),
        SAlpha = ext_mul(embed(Field, rational(S, 1)), Alpha),
        FShifted = shift_poly(F, SAlpha),
        N = norm_poly(Field, FShifted),
        ( if is_square_free_q(N) then
            NFactors = factor_square_free(monic(N)),
            ( if int.'=<'(list.length(NFactors), 1) then
                Result = [monic(F)]
            else
                Result = lift_factors(Field, F, SAlpha, NFactors)
            )
        else
            Result = trager_factor(Field, F, int.'+'(S, 1))
        )
    ).

%---------------------------------------------------------------------------%
% Norm computation
%---------------------------------------------------------------------------%

norm_poly(Field, F) = Result :-
    DF = degree(F),
    DM = ext_degree(Field),
    ResultDeg = int.'*'(DF, DM),
    make_points(0, ResultDeg, Points),
    Values = list.map(( func(X0) = norm_at_point(Field, F, X0) ), Points),
    list.map_corresponding(( pred(P::in, V::in, PV::out) is det :-
        PV = {P, V}
    ), Points, Values, PVs),
    Result = lagrange_interpolate(PVs).

:- pred make_points(int::in, int::in,
    list(rational)::out) is det.

make_points(I, Max, Points) :-
    ( if int.'>'(I, Max) then
        Points = []
    else
        make_points(int.'+'(I, 1), Max, RestPoints),
        Points = [rational(I, 1) | RestPoints]
    ).

:- func norm_at_point(ext_field(rational),
    poly(ext_elem(rational)), rational) = rational.

norm_at_point(Field, F, X0) = Result :-
    M = gen_min_poly(Field),
    % f_lifted(x0, t) = Σ c_i(t) * x0^i
    Cs = coeffs(F),
    eval_lifted(Cs, X0, 0, zero_poly, FAtX0),
    Result = poly_resultant(M, FAtX0).

:- pred eval_lifted(list(ext_elem(rational))::in, rational::in,
    int::in, poly(rational)::in, poly(rational)::out) is det.

eval_lifted([], _, _, Acc, Acc).
eval_lifted([C | Cs], X0, I, Acc0, Acc) :-
    CP = elem_poly(C),
    X0I = rat_pow_q(X0, I),
    Term = scale(X0I, CP),
    Acc1 = add(Acc0, Term),
    eval_lifted(Cs, X0, int.'+'(I, 1), Acc1, Acc).

:- func rat_pow_q(rational, int) = rational.

rat_pow_q(X, N) = Result :-
    ( if N = 0 then
        Result = rational.one
    else if int.even(N) then
        Half = rat_pow_q(X, int.'//'(N, 2)),
        Result = rational.'*'(Half, Half)
    else
        Rest = rat_pow_q(X, int.'-'(N, 1)),
        Result = rational.'*'(X, Rest)
    ).

%---------------------------------------------------------------------------%
% Square-free test
%---------------------------------------------------------------------------%

:- pred is_square_free_q(poly(rational)::in) is semidet.

is_square_free_q(P) :-
    P1 = diff(P),
    G = gcd(P, P1),
    int.'=<'(degree(G), 0).

%---------------------------------------------------------------------------%
% Lifting
%---------------------------------------------------------------------------%

:- func lift_factors(ext_field(rational),
    poly(ext_elem(rational)),
    ext_elem(rational),
    list(poly(rational))) = list(poly(ext_elem(rational))).

lift_factors(Field, F, SAlpha, QFactors) =
    lift_factors_loop(Field, F, SAlpha, QFactors).

:- func lift_factors_loop(ext_field(rational),
    poly(ext_elem(rational)),
    ext_elem(rational),
    list(poly(rational))) = list(poly(ext_elem(rational))).

lift_factors_loop(_, _, _, []) = [].
lift_factors_loop(Field, Remaining, SAlpha, [G | Gs]) = Result :-
    ( if int.'=<'(degree(Remaining), 0) then
        Result = []
    else
        GLifted = lift_q_poly(Field, G),
        GUnshifted = shift_poly(GLifted, ext_neg(SAlpha)),
        H = gcd_poly_ext(Remaining, GUnshifted),
        ( if int.'>'(degree(H), 0) then
            div_mod(Remaining, H, Q, _),
            Result = [monic(H) |
                lift_factors_loop(Field, Q, SAlpha, Gs)]
        else
            Result = lift_factors_loop(Field, Remaining, SAlpha, Gs)
        )
    ).

:- func lift_q_poly(ext_field(rational), poly(rational))
    = poly(ext_elem(rational)).

lift_q_poly(Field, P) =
    poly(list.map(embed(Field), coeffs(P))).

:- func gcd_poly_ext(poly(ext_elem(rational)),
    poly(ext_elem(rational))) = poly(ext_elem(rational)).

gcd_poly_ext(A, B) = Result :-
    ( if is_zero_poly_ext(B) then
        Result = monic(A)
    else
        div_mod(A, B, _, R),
        Result = gcd_poly_ext(B, R)
    ).

:- pred is_zero_poly_ext(poly(ext_elem(rational))::in) is semidet.

is_zero_poly_ext(P) :-
    list.all_true(( pred(C::in) is semidet :-
        ring_is_zero(C)
    ), coeffs(P)).

%---------------------------------------------------------------------------%
% Generalized Trager over any K
%---------------------------------------------------------------------------%

factor_sf_over_extension_k(FactorBaseK, Field, F) = Result :-
    D = degree(F),
    ( if int.'=<'(D, 0) then
        Result = []
    else if D = 1 then
        Result = [monic(F)]
    else
        Result = trager_factor_k(FactorBaseK, Field, F, 0)
    ).

:- func trager_factor_k(
    (func(poly(K)) = list(poly(K))),
    ext_field(K),
    poly(ext_elem(K)),
    int) = list(poly(ext_elem(K))) <= field(K).

trager_factor_k(FactorBaseK, Field, F, S) = Result :-
    ( if int.'>'(S, 20) then
        Result = [F]
    else
        Alpha = generator(Field),
        SAlpha = ext_mul(
            embed(Field, ring_from_int(S)),
            Alpha),
        FShifted = shift_poly(F, SAlpha),
        N = norm_poly_k(Field, FShifted),
        ( if is_square_free_k(N) then
            NFactors = FactorBaseK(monic(N)),
            ( if int.'=<'(list.length(NFactors), 1) then
                Result = [monic(F)]
            else
                Result = lift_factors_k(Field, F, SAlpha, NFactors)
            )
        else
            Result = trager_factor_k(FactorBaseK, Field, F,
                int.'+'(S, 1))
        )
    ).

norm_poly_k(Field, F) = Result :-
    DF = degree(F),
    DM = ext_degree(Field),
    ResultDeg = int.'*'(DF, DM),
    M = gen_min_poly(Field),
    make_points_k(0, ResultDeg, Points),
    Values = list.map(( func(X0) = R :-
        FAtX0 = eval_poly_ext_k(F, X0),
        R = poly_resultant(M, FAtX0)
    ), Points),
    list.map_corresponding(( pred(P::in, V::in, PV::out) is det :-
        PV = {P, V}
    ), Points, Values, PVs),
    Result = lagrange_interpolate(PVs).

:- pred make_points_k(int::in, int::in,
    list(K)::out) is det <= field(K).

make_points_k(I, Max, Points) :-
    ( if int.'>'(I, Max) then
        Points = []
    else
        make_points_k(int.'+'(I, 1), Max, RestPoints),
        Points = [ring_from_int(I) | RestPoints]
    ).

:- func eval_poly_ext_k(poly(ext_elem(K)), K) = poly(K) <= field(K).

eval_poly_ext_k(F, X0) = Result :-
    Cs = coeffs(F),
    eval_lifted_k(Cs, X0, 0, zero_poly, Result).

:- pred eval_lifted_k(list(ext_elem(K))::in, K::in,
    int::in, poly(K)::in, poly(K)::out) is det <= field(K).

eval_lifted_k([], _, _, Acc, Acc).
eval_lifted_k([C | Cs], X0, I, Acc0, Acc) :-
    CP = elem_poly(C),
    X0I = pow_k(X0, I),
    Term = scale(X0I, CP),
    Acc1 = add(Acc0, Term),
    eval_lifted_k(Cs, X0, int.'+'(I, 1), Acc1, Acc).

:- func pow_k(K, int) = K <= field(K).

pow_k(X, N) = Result :-
    ( if N = 0 then
        Result = ring_one
    else if int.even(N) then
        Half = pow_k(X, int.'//'(N, 2)),
        Result = ring_mul(Half, Half)
    else
        Result = ring_mul(X, pow_k(X, int.'-'(N, 1)))
    ).

:- pred is_square_free_k(poly(K)::in) is semidet <= field(K).

is_square_free_k(P) :-
    P1 = diff(P),
    G = gcd(P, P1),
    int.'=<'(degree(G), 0).

:- func lift_factors_k(ext_field(K),
    poly(ext_elem(K)),
    ext_elem(K),
    list(poly(K))) = list(poly(ext_elem(K))) <= field(K).

lift_factors_k(_, _, _, []) = [].
lift_factors_k(Field, Remaining, SAlpha, [G | Gs]) = Result :-
    ( if int.'=<'(degree(Remaining), 0) then
        Result = []
    else
        GLifted = lift_k_poly(Field, G),
        GUnshifted = shift_poly(GLifted, ext_neg(SAlpha)),
        H = gcd_poly_generic(Remaining, GUnshifted),
        ( if int.'>'(degree(H), 0) then
            div_mod(Remaining, H, Q, _),
            Result = [monic(H) |
                lift_factors_k(Field, Q, SAlpha, Gs)]
        else
            Result = lift_factors_k(Field, Remaining, SAlpha, Gs)
        )
    ).

:- func lift_k_poly(ext_field(K), poly(K)) = poly(ext_elem(K)) <= field(K).

lift_k_poly(Field, P) =
    poly(list.map(embed(Field), coeffs(P))).

:- func gcd_poly_generic(poly(ext_elem(K)),
    poly(ext_elem(K))) = poly(ext_elem(K)) <= field(K).

gcd_poly_generic(A, B) = Result :-
    ( if is_zero_poly_ext_k(B) then
        Result = monic(A)
    else
        div_mod(A, B, _, R),
        Result = gcd_poly_generic(B, R)
    ).

:- pred is_zero_poly_ext_k(poly(ext_elem(K))::in) is semidet <= field(K).

is_zero_poly_ext_k(P) :-
    list.all_true(( pred(C::in) is semidet :-
        ring_is_zero(C)
    ), coeffs(P)).

%---------------------------------------------------------------------------%
:- end_module trager_factoring.
%---------------------------------------------------------------------------%
