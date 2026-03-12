%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: alg_num.m
% Main author: nyc
%
% Canonical representation of real algebraic numbers.
%
% An algebraic number is represented as (minimal polynomial, isolating
% interval).  Two algebraic numbers are equal iff they have the same
% minimal polynomial and their isolating intervals overlap (after
% refinement).
%
% Arithmetic is done via resultant-based composed polynomials:
% given α with minpoly p and β with minpoly q,
%   α + β has annihilating poly ComposedSum(p, q)
%   α · β has annihilating poly ComposedProduct(p, q)
% which are then factored to find the true minimal polynomial.
%
%---------------------------------------------------------------------------%

:- module alg_num.
:- interface.

:- import_module interval.
:- import_module poly.
:- import_module rational.

%---------------------------------------------------------------------------%

:- type alg_num
    --->    alg_num(
                an_min_poly :: poly(rational),
                an_interval :: interval
            ).

    % Embed a rational number as an algebraic number.
    %
:- func alg_from_rational(rational) = alg_num.

    % Construct from a polynomial and approximate real value.
    %
:- func alg_from_poly(poly(rational), float) = alg_num.

    % The minimal polynomial.
    %
:- func alg_min_poly(alg_num) = poly(rational).

    % Approximate the algebraic number as a rational within epsilon.
    %
:- func alg_approx(rational, alg_num) = rational.

    % Negation.
    %
:- func alg_neg(alg_num) = alg_num.

    % Addition.
    %
:- func alg_add(alg_num, alg_num) = alg_num.

    % Subtraction.
    %
:- func alg_sub(alg_num, alg_num) = alg_num.

    % Multiplication.
    %
:- func alg_mul(alg_num, alg_num) = alg_num.

    % Multiplicative inverse.
    %
:- func alg_inv(alg_num) = alg_num.

    % Division.
    %
:- func alg_div(alg_num, alg_num) = alg_num.

    % Integer power.
    %
:- func alg_pow(alg_num, int) = alg_num.

    % nth root of a positive algebraic number.
    %
:- func alg_root(int, alg_num) = alg_num.

    % Equality test.
    %
:- pred alg_eq(alg_num::in, alg_num::in) is semidet.

    % Comparison.
    %
:- func alg_compare(alg_num, alg_num) = comparison_result.

    % Display as string.
    %
:- func alg_show(alg_num) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module factoring.
:- import_module float.
:- import_module integer.
:- use_module int.
:- import_module list.
:- import_module math.
:- import_module one_or_more.
:- import_module require.
:- import_module resultant.
:- import_module root_bound.
:- import_module root_isolation.
:- use_module string.

%---------------------------------------------------------------------------%
% Construction
%---------------------------------------------------------------------------%

alg_from_rational(R) = alg_num(
    mk_poly([rational.'-'(rational.zero, R), rational.one]),
    interval.interval(R, R)).

alg_from_poly(P, Approx) = Result :-
    MP = monic(P),
    Factors = factor_square_free(MP),
    Result = pick_best_root(Factors, Approx).

alg_min_poly(A) = A ^ an_min_poly.

%---------------------------------------------------------------------------%
% Approximation
%---------------------------------------------------------------------------%

alg_approx(Eps, alg_num(P, IV)) = Result :-
    Refined = refine_root(Eps,
        isolating_interval(P, IV)),
    interval(L, H) = ii_interval(Refined),
    Two = rational(2, 1),
    Result = rational.'/'(rational.'+'(L, H), Two).

:- func alg_approx_double(alg_num) = float.

alg_approx_double(A) = rational_to_float(
    alg_approx(
        rational.'/'(rational.one,
            rational.from_integer(
                integer.pow(integer.integer(10), integer.integer(15)))),
        A)).

:- func rational_to_float(rational) = float.

rational_to_float(R) =
    float.'/'(
        integer.float(numer(R)),
        integer.float(denom(R))).

%---------------------------------------------------------------------------%
% Arithmetic
%---------------------------------------------------------------------------%

alg_neg(alg_num(P, interval(L, H))) = Result :-
    P1 = negate_var(P),
    IV1 = interval(rational.'-'(rational.zero, H),
                   rational.'-'(rational.zero, L)),
    Result = make_alg_num(P1, IV1).

alg_add(A, B) = Result :-
    Ann = composed_sum(an_min_poly(A), an_min_poly(B)),
    Approx = float.'+'(alg_approx_double(A), alg_approx_double(B)),
    Result = make_alg_num_from_ann(Ann, Approx).

alg_sub(A, B) = alg_add(A, alg_neg(B)).

alg_mul(A, B) = Result :-
    Ann = composed_product(an_min_poly(A), an_min_poly(B)),
    Approx = float.'*'(alg_approx_double(A), alg_approx_double(B)),
    Result = make_alg_num_from_ann(Ann, Approx).

alg_inv(alg_num(P, interval(L, H))) = Result :-
    P1 = reciprocal_poly(P),
    ( if rational.'>'(L, rational.zero) then
        IV1 = interval(rational.'/'(rational.one, H),
                       rational.'/'(rational.one, L))
    else if rational.'<'(H, rational.zero) then
        IV1 = interval(rational.'/'(rational.one, H),
                       rational.'/'(rational.one, L))
    else
        require.error("alg_inv: zero")
    ),
    Result = make_alg_num(P1, IV1).

alg_div(A, B) = alg_mul(A, alg_inv(B)).

alg_pow(A, N) = Result :-
    ( if N = 0 then
        Result = alg_from_rational(rational.one)
    else if N = 1 then
        Result = A
    else if int.'<'(N, 0) then
        Result = alg_pow(alg_inv(A), int.'-'(0, N))
    else if int.even(N) then
        Half = alg_pow(A, int.'//'(N, 2)),
        Result = alg_mul(Half, Half)
    else
        Result = alg_mul(A, alg_pow(A, int.'-'(N, 1)))
    ).

alg_root(N, alg_num(P, IV)) = Result :-
    Ann = substitute_x_n(N, P),
    Approx = math.pow(alg_approx_double(alg_num(P, IV)),
                      float.'/'(1.0, float.float(N))),
    Result = make_alg_num_from_ann(Ann, Approx).

%---------------------------------------------------------------------------%
% Equality and comparison
%---------------------------------------------------------------------------%

alg_eq(A, B) :-
    an_min_poly(A) = an_min_poly(B),
    intervals_overlap(an_interval(A), an_interval(B), an_min_poly(A)).

alg_compare(A, B) = Result :-
    ( if alg_eq(A, B) then
        Result = (=)
    else
        Result = separate_and_compare(A, B)
    ).

:- func separate_and_compare(alg_num, alg_num) = comparison_result.

separate_and_compare(A, B) = Result :-
    II1 = isolating_interval(an_min_poly(A), an_interval(A)),
    II2 = isolating_interval(an_min_poly(B), an_interval(B)),
    Result = sep_loop(200, II1, II2).

:- func sep_loop(int, isolating_interval, isolating_interval)
    = comparison_result.

sep_loop(N, II1, II2) = Result :-
    interval(_, H1) = ii_interval(II1),
    interval(L2, _) = ii_interval(II2),
    interval(L1, _) = ii_interval(II1),
    interval(_, H2) = ii_interval(II2),
    ( if rational.'<'(H1, L2) then
        Result = (<)
    else if rational.'<'(H2, L1) then
        Result = (>)
    else if N = 0 then
        % Fallback: compare midpoints
        Two = rational(2, 1),
        M1 = rational.'/'(rational.'+'(L1, H1), Two),
        M2 = rational.'/'(rational.'+'(L2, H2), Two),
        Result = rational_compare(M1, M2)
    else
        W1 = rational.'-'(H1, L1),
        W2 = rational.'-'(H2, L2),
        Four = rational(4, 1),
        Eps1 = rational.'/'(W1, Four),
        Eps2 = rational.'/'(W2, Four),
        II1P = refine_root(Eps1, II1),
        II2P = refine_root(Eps2, II2),
        Result = sep_loop(int.'-'(N, 1), II1P, II2P)
    ).

:- func rational_compare(rational, rational) = comparison_result.

rational_compare(A, B) = Result :-
    ( if rational.'<'(A, B) then
        Result = (<)
    else if rational.'>'(A, B) then
        Result = (>)
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%
% Interval overlap check
%---------------------------------------------------------------------------%

:- pred intervals_overlap(interval::in, interval::in,
    poly(rational)::in) is semidet.

intervals_overlap(IV1, IV2, P) :-
    overlap_loop(50, IV1, IV2, P).

:- pred overlap_loop(int::in, interval::in, interval::in,
    poly(rational)::in) is semidet.

overlap_loop(N, interval(L1, H1), interval(L2, H2), P) :-
    ( if rational.'<'(H1, L2) then
        fail
    else if rational.'<'(H2, L1) then
        fail
    else if L1 = H1, L2 = H2 then
        L1 = L2
    else if L1 = H1 then
        poly.eval(P, L1) = rational.zero,
        rational.'=<'(L2, L1),
        rational.'=<'(L1, H2)
    else if L2 = H2 then
        poly.eval(P, L2) = rational.zero,
        rational.'=<'(L1, L2),
        rational.'=<'(L2, H1)
    else if N = 0 then
        % Last resort: midpoints close enough
        Two = rational(2, 1),
        M1 = rational.'/'(rational.'+'(L1, H1), Two),
        M2 = rational.'/'(rational.'+'(L2, H2), Two),
        Diff = rational.abs(rational.'-'(M1, M2)),
        Threshold = rational.'/'(
            rational.'+'(rational.'-'(H1, L1), rational.'-'(H2, L2)),
            Two),
        rational.'<'(Diff, Threshold)
    else
        Four = rational(4, 1),
        Eps1 = rational.'/'(rational.'-'(H1, L1), Four),
        Eps2 = rational.'/'(rational.'-'(H2, L2), Four),
        I1P = ii_interval(
            refine_root(Eps1, isolating_interval(P, interval(L1, H1)))),
        I2P = ii_interval(
            refine_root(Eps2, isolating_interval(P, interval(L2, H2)))),
        overlap_loop(int.'-'(N, 1), I1P, I2P, P)
    ).

%---------------------------------------------------------------------------%
% Internal: construct AlgNum from annihilating poly
%---------------------------------------------------------------------------%

:- func make_alg_num_from_ann(poly(rational), float) = alg_num.

make_alg_num_from_ann(Ann, Approx) = Result :-
    Factors = factor_square_free(monic(Ann)),
    Result = pick_best_root(Factors, Approx).

:- func make_alg_num(poly(rational), interval) = alg_num.

make_alg_num(Ann, IV) = Result :-
    Factors = factor_square_free(monic(Ann)),
    interval(L, H) = IV,
    Two = rational(2, 1),
    Mid = rational_to_float(rational.'/'(rational.'+'(L, H), Two)),
    Best = pick_best_root(Factors, Mid),
    % If pick_best_root found a factor, use it; otherwise fallback
    ( if an_min_poly(Best) = mk_poly([]) then
        Result = alg_num(monic(Ann), IV)
    else
        Result = Best
    ).

:- func pick_best_root(list(poly(rational)), float) = alg_num.

pick_best_root(Factors, Approx) = Result :-
    Candidates = list.condense(
        list.map(
            ( func(F) =
                list.map(
                    ( func(II) = {F, II} ),
                    isolate_real_roots(F))
            ),
            Factors)),
    Scored = list.map(
        ( func({F, II}) = {F, ii_interval(II), Score} :-
            interval(L, H) = ii_interval(II),
            Two = rational(2, 1),
            Mid = rational_to_float(rational.'/'(rational.'+'(L, H), Two)),
            Score = float.abs(float.'-'(Mid, Approx))
        ),
        Candidates),
    ( if Scored = [] then
        % No real roots found — return a degenerate result
        Result = alg_num(mk_poly([rational.one]),
                        interval.interval(rational.zero, rational.zero))
    else if Scored = [_|_] then
        {BF, BIV, _} = find_min_score(Scored),
        Result = alg_num(BF, BIV)
    else
        require.error("pick_best_root: impossible")
    ).

:- func find_min_score(list({poly(rational), interval, float}))
    = {poly(rational), interval, float}.

find_min_score(Xs) = Result :-
    ( if Xs = [{F, IV, S}] then
        Result = {F, IV, S}
    else if Xs = [{F, IV, S} | Rest], Rest = [_ | _] then
        {BF, BIV, BS} = find_min_score(Rest),
        ( if float.'=<'(S, BS) then
            Result = {F, IV, S}
        else
            Result = {BF, BIV, BS}
        )
    else
        require.error("find_min_score: empty list")
    ).

%---------------------------------------------------------------------------%
% Display
%---------------------------------------------------------------------------%

alg_show(A) = Result :-
    Approx = alg_approx_double(A),
    P = an_min_poly(A),
    Result = string.append_list(["AlgNum(",
        show_rat_poly(P), " ≈ ",
        string.float_to_string(Approx), ")"]).

:- func show_rat_poly(poly(rational)) = string.

show_rat_poly(P) = Result :-
    Cs = coeffs(P),
    Strs = list.map(
        ( func(C) = S :-
            N = numer(C), D = denom(C),
            ( if D = integer.one then
                S = integer.to_string(N)
            else
                S = string.append_list([integer.to_string(N),
                    "/", integer.to_string(D)])
            )
        ),
        Cs),
    Result = string.append_list(["[",
        string.join_list(", ", Strs), "]"]).

%---------------------------------------------------------------------------%
:- end_module alg_num.
%---------------------------------------------------------------------------%
