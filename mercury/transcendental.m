%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: transcendental.m
% Main author: nyc
%
% Transcendental extension field Q(x1,...,xn).
%
% Elements are rational functions (ratios of multivariate polynomials).
% When the coefficient type is rational, fractions can be reduced to
% lowest terms using reduce_frac.
%
%---------------------------------------------------------------------------%

:- module transcendental.
:- interface.

:- import_module multivariate.
:- import_module poly.
:- import_module rational.

%---------------------------------------------------------------------------%
% Types
%---------------------------------------------------------------------------%

    % An element of Q(x1,...,xn): a ratio of multivariate polynomials.
    % Invariant: denominator is non-zero.
    %
:- type rat_func(K)
    --->    rat_func(
                rf_num :: mpoly(K),
                rf_den :: mpoly(K)
            ).

%---------------------------------------------------------------------------%
% Construction
%---------------------------------------------------------------------------%

    % Smart constructor: errors on zero denominator.
    %
:- func mk_rat_func(mpoly(K), mpoly(K)) = rat_func(K).

    % Constant rational function.
    %
:- func const_rf(K) = rat_func(K) <= ring(K).

    % A transcendental variable as a rational function.
    %
:- func var_rf(var) = rat_func(K) <= ring(K).

%---------------------------------------------------------------------------%
% Queries
%---------------------------------------------------------------------------%

    % Test if a rational function is constant.
    %
:- pred is_const_rf(rat_func(K)::in) is semidet.

%---------------------------------------------------------------------------%
% Evaluation
%---------------------------------------------------------------------------%

    % Evaluate a rational function by substituting values for variables.
    %
:- func eval_rf((func(var) = K), rat_func(K)) = K <= (ring(K), field(K)).

%---------------------------------------------------------------------------%
% Reduction
%---------------------------------------------------------------------------%

    % Reduce a rational function over rational to lowest terms.
    %
:- func reduce_frac(rat_func(rational)) = rat_func(rational).

%---------------------------------------------------------------------------%
% Arithmetic (ring/field instances)
%---------------------------------------------------------------------------%

:- instance ring(rat_func(K)) <= ring(K).
:- instance field(rat_func(K)) <= ring(K).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module require.

%---------------------------------------------------------------------------%
% Construction
%---------------------------------------------------------------------------%

mk_rat_func(N, D) = Result :-
    ( if mp_is_zero(D) then
        unexpected($pred, "zero denominator in rat_func")
    else
        Result = rat_func(N, D)
    ).

const_rf(C) = rat_func(mp_const(C), mp_one).

var_rf(V) = rat_func(mp_var(V), mp_one).

%---------------------------------------------------------------------------%
% Queries
%---------------------------------------------------------------------------%

is_const_rf(rat_func(N, D)) :-
    int.'=<'(num_terms(N), 1),
    num_terms(D) = 1,
    total_degree(N) = 0,
    total_degree(D) = 0.

%---------------------------------------------------------------------------%
% Evaluation
%---------------------------------------------------------------------------%

eval_rf(Env, rat_func(N, D)) =
    field_div(mp_eval(Env, N), mp_eval(Env, D)).

%---------------------------------------------------------------------------%
% Reduction
%---------------------------------------------------------------------------%

reduce_frac(rat_func(N, D)) = Result :-
    ( if mp_is_zero(N) then
        Result = rat_func(mp_zero, mp_one)
    else
        G = gcd_mpoly(N, D),
        ( if mp_is_zero(G) then
            Result = rat_func(N, D)
        else if num_terms(G) = 1, total_degree(G) = 0 then
            Result = rat_func(N, D)
        else
            Result = rat_func(exact_div_mpoly(N, G), exact_div_mpoly(D, G))
        )
    ).

%---------------------------------------------------------------------------%
% Typeclass instances
%---------------------------------------------------------------------------%

:- instance ring(rat_func(K)) <= ring(K) where [
    (ring_zero = rat_func(mp_zero, mp_one)),
    (ring_one = rat_func(mp_one, mp_one)),
    % p1/q1 + p2/q2 = (p1*q2 + p2*q1) / (q1*q2)
    (ring_add(rat_func(N1, D1), rat_func(N2, D2)) =
        rat_func(mp_add(mp_mul(N1, D2), mp_mul(N2, D1)),
                 mp_mul(D1, D2))),
    % p1/q1 - p2/q2 = (p1*q2 - p2*q1) / (q1*q2)
    (ring_sub(rat_func(N1, D1), rat_func(N2, D2)) =
        rat_func(mp_sub(mp_mul(N1, D2), mp_mul(N2, D1)),
                 mp_mul(D1, D2))),
    % p1/q1 * p2/q2 = (p1*p2) / (q1*q2)
    (ring_mul(rat_func(N1, D1), rat_func(N2, D2)) =
        rat_func(mp_mul(N1, N2), mp_mul(D1, D2))),
    (ring_negate(rat_func(N, D)) = rat_func(mp_neg(N), D)),
    (ring_from_int(I) = rat_func(mp_const(ring_from_int(I)), mp_one)),
    (ring_is_zero(rat_func(N, _)) :- mp_is_zero(N))
].

:- instance field(rat_func(K)) <= ring(K) where [
    (field_div(A, rat_func(N2, D2)) = R :-
        ( if mp_is_zero(N2) then
            unexpected($pred, "division by zero in rat_func")
        else
            R = ring_mul(A, rat_func(D2, N2))
        ))
].

%---------------------------------------------------------------------------%
:- end_module transcendental.
%---------------------------------------------------------------------------%
