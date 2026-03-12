%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: minimal_poly.m
% Main author: nyc
%
% Compute the minimal polynomial of a radical expression over Q.
%
% The basic approach: given a radical expression α, build a polynomial
% that α satisfies (annihilating polynomial), then factor it to find
% the irreducible factor that α is actually a root of.
%
%---------------------------------------------------------------------------%

:- module minimal_poly.
:- interface.

:- import_module poly.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Compute the minimal polynomial of a radical expression over Q.
    % Returns the monic irreducible polynomial in Q[x] of smallest degree
    % that the expression satisfies.
    %
:- func minimal_poly(rad_expr(rational)) = poly(rational).

    % Compute an annihilating polynomial (not necessarily minimal) for
    % a radical expression.  The expression is guaranteed to be a root
    % of this polynomial, but it may be reducible.
    %
:- func annihilating_poly(rad_expr(rational)) = poly(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module factoring.
:- import_module float.
:- import_module list.
:- import_module one_or_more.
:- import_module rad_eval.
:- import_module resultant.
:- import_module root_bound.
:- use_module int.

%---------------------------------------------------------------------------%

minimal_poly(Expr) = Result :-
    Ann = annihilating_poly(Expr),
    V = eval_float(Expr),
    Factors = factor_square_free(Ann),
    ( if Factors = [F | Fs] then
        Result = monic(pick_closest(V, one_or_more(F, Fs)))
    else
        Result = Ann
    ).

%---------------------------------------------------------------------------%

annihilating_poly(Expr) = Result :-
    ( if Expr = re_lit(R) then
        % Minimal poly of r ∈ Q is (x - r)
        Result = mk_poly([rational.'-'(rational.zero, R), rational.one])
    else if Expr = re_neg(E) then
        Result = negate_var(annihilating_poly(E))
    else if Expr = re_add(A, B) then
        Result = composed_sum(annihilating_poly(A), annihilating_poly(B))
    else if Expr = re_mul(A, B) then
        Result = composed_product(annihilating_poly(A), annihilating_poly(B))
    else if Expr = re_inv(E) then
        Result = reciprocal_poly(annihilating_poly(E))
    else if Expr = re_root(N, re_lit(R)) then
        % x^n - r
        Zeros = list.duplicate(int.'-'(N, 1), rational.zero),
        Result = mk_poly([rational.'-'(rational.zero, R) | Zeros] ++ [rational.one])
    else if Expr = re_root(N, E) then
        Result = substitute_x_n(N, annihilating_poly(E))
    else if Expr = re_pow(E, N) then
        P = annihilating_poly(E),
        ( if int.'>='(N, 0) then
            Result = annihilating_poly_of_pow(P, N)
        else
            PR = reciprocal_poly(P),
            Result = annihilating_poly_of_pow(PR, int.'-'(0, N))
        )
    else
        % Fallback: x
        Result = mk_poly([rational.zero, rational.one])
    ).

%---------------------------------------------------------------------------%
% Annihilating polynomial for e^n given annihilating poly for e.
%---------------------------------------------------------------------------%

:- func annihilating_poly_of_pow(poly(rational), int) = poly(rational).

annihilating_poly_of_pow(P, N) = Result :-
    ( if N = 0 then
        % x - 1
        Result = mk_poly([rational.'-'(rational.zero, rational.one),
                          rational.one])
    else if N = 1 then
        Result = P
    else
        % Res_y(p(y), y^n - x₀) for interpolation points
        DP = degree(P),
        Points = make_points(0, DP),
        Values = list.map(
            ( func(X) = pow_resultant_at(P, N, X) ),
            Points),
        Result = lagrange_interpolate(zip_pairs(Points, Values))
    ).

:- func pow_resultant_at(poly(rational), int, rational) = rational.

pow_resultant_at(P, N, X0) = Result :-
    % y^n - x₀
    Zeros = list.duplicate(int.'-'(N, 1), rational.zero),
    YnMinusX0 = mk_poly([rational.'-'(rational.zero, X0) | Zeros]
                         ++ [rational.one]),
    Result = poly_resultant(P, YnMinusX0).

:- func make_points(int, int) = list(rational).

make_points(I, Max) = Result :-
    ( if int.'>'(I, Max) then
        Result = []
    else
        Result = [rational(I, 1) |
                  make_points(int.'+'(I, 1), Max)]
    ).


%---------------------------------------------------------------------------%
% Utility: zip into pair list
%---------------------------------------------------------------------------%

:- func zip_pairs(list(K), list(K)) = list({K, K}).

zip_pairs([], _) = [].
zip_pairs([_ | _], []) = [].
zip_pairs([X | Xs], [Y | Ys]) = [{X, Y} | zip_pairs(Xs, Ys)].

%---------------------------------------------------------------------------%
:- end_module minimal_poly.
%---------------------------------------------------------------------------%
