%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: resultant.m
% Main author: nyc
%
% Resultant computation, Lagrange interpolation, and composed-sum/product
% polynomials over an arbitrary field.
%
%---------------------------------------------------------------------------%

:- module resultant.
:- interface.

:- import_module list.
:- import_module poly.

%---------------------------------------------------------------------------%

    % Resultant of two univariate polynomials via the Euclidean algorithm.
    %
:- func poly_resultant(poly(K), poly(K)) = K <= field(K).

    % Lagrange interpolation: given points {(x_i, y_i)}, compute the unique
    % polynomial of degree <= n passing through all points.
    %
:- func lagrange_interpolate(list({K, K})) = poly(K) <= field(K).

    % Composed sum: if p has roots alpha_i and q has roots beta_j,
    % the result has roots alpha_i + beta_j.
    %
:- func composed_sum(poly(K), poly(K)) = poly(K) <= field(K).

    % Composed product: if p has roots alpha_i and q has roots beta_j,
    % the result has roots alpha_i * beta_j.
    %
:- func composed_product(poly(K), poly(K)) = poly(K) <= field(K).

    % p(-x): negate odd-degree coefficients.
    %
:- func negate_var(poly(K)) = poly(K) <= ring(K).

    % Reciprocal polynomial: x^n * p(1/x), i.e. reverse coefficients.
    %
:- func reciprocal_poly(poly(K)) = poly(K) <= ring(K).

    % Substitute x^n for x in p: p(x^n).
    %
:- func substitute_x_n(int, poly(K)) = poly(K) <= ring(K).

    % Shift a polynomial: f(x + a).
    %
:- func shift_poly(poly(K), K) = poly(K) <= field(K).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

poly_resultant(F, G) = Result :-
    DF = degree(F) : int,
    DG = degree(G) : int,
    ( if int.'<'(DF, 0) then
        Result = ring_zero
    else if int.'<'(DG, 0) then
        Result = ring_zero
    else if DG = (0 : int) then
        ( if lead_coeff(G, C) then
            Result = pow_k(C, DF)
        else
            Result = ring_zero
        )
    else
        div_mod(F, G, _, R),
        ( if lead_coeff(G, LG0) then
            LG = LG0
        else
            LG = ring_one
        ),
        DR = degree(R) : int,
        DFDG = int.'*'(DF, DG),
        Sign = ( if odd(DFDG) then ring_negate(ring_one) else ring_one ),
        ( if int.'<'(DR, 0) then
            Result = ring_zero
        else
            Result = ring_mul(ring_mul(Sign, pow_k(LG, int.'-'(DF, DR))),
                poly_resultant(G, R))
        )
    ).

    % Exponentiation by squaring for ring elements.
    %
:- func pow_k(K, int) = K <= ring(K).

pow_k(X, N) = Result :-
    ( if N = 0 then
        Result = ring_one
    else if int.'>'(N, 0) then
        ( if int.rem(N, 2) = 0 then
            Half = pow_k(X, int.'//'(N, 2)),
            Result = ring_mul(Half, Half)
        else
            Result = ring_mul(X, pow_k(X, int.'-'(N, 1)))
        )
    else
        unexpected($pred, "negative exponent: " ++ string.int_to_string(N))
    ).

:- pred odd(int::in) is semidet.

odd(N) :- int.rem(N, 2) \= 0.

%---------------------------------------------------------------------------%

lagrange_interpolate(Points) = Result :-
    Xs = list.map(fst, Points),
    Terms = list.map(
        ( func({Xi, Yi}) = scale(Yi, lagrange_basis(Xi, Xs)) ),
        Points),
    Result = list.foldl(add, Terms, zero_poly).

:- func lagrange_basis(K, list(K)) = poly(K) <= field(K).

lagrange_basis(Xi, Xs) = Result :-
    Others = list.filter(( pred(X::in) is semidet :- X \= Xi ), Xs),
    Result = list.foldl(
        ( func(Xj, Acc) = mul(Acc, scale(
            field_div(ring_one, ring_sub(Xi, Xj)),
            mk_poly([ring_negate(Xj), ring_one]))) ),
        Others, const_poly(ring_one)).

%---------------------------------------------------------------------------%

composed_sum(P, Q) = Result :-
    DP = degree(P) : int,
    DQ = degree(Q) : int,
    ResultDeg = int.'*'(DP, DQ),
    Points = make_points(0, ResultDeg),
    Values = list.map(
        ( func(X) = resultant_sum_at(P, Q, X) ),
        Points),
    Result = lagrange_interpolate(zip_pairs(Points, Values)).

:- func resultant_sum_at(poly(K), poly(K), K) = K <= field(K).

resultant_sum_at(P, Q, X0) = poly_resultant(P, QShifted) :-
    QShifted = substitute_linear(Q, X0, ring_negate(ring_one)).

    % substitute_linear(P, A, B) = P(A + B*y).
    %
:- func substitute_linear(poly(K), K, K) = poly(K) <= field(K).

substitute_linear(poly(Cs), A, B) = Result :-
    Binomial = mk_poly([A, B]),
    list.reverse(Cs, RevCs),
    list.foldl(
        ( pred(C::in, Acc0::in, Acc1::out) is det :-
            Acc1 = add(const_poly(C), mul(Binomial, Acc0))
        ), RevCs, zero_poly, Result).

%---------------------------------------------------------------------------%

composed_product(P, Q) = Result :-
    DP = degree(P) : int,
    DQ = degree(Q) : int,
    ResultDeg = int.'*'(DP, DQ),
    Points = make_points(1, int.'+'(ResultDeg, 1)),
    Values = list.map(
        ( func(X) = product_resultant_at(P, Q, X) ),
        Points),
    Result = lagrange_interpolate(zip_pairs(Points, Values)).

:- func product_resultant_at(poly(K), poly(K), K) = K <= field(K).

product_resultant_at(P, Q, X0) = poly_resultant(PRev, Q) :-
    PRev = scaled_reciprocal_at(P, X0).

    % y^n * p(x0/y) as a polynomial in y.
    %
:- func scaled_reciprocal_at(poly(K), K) = poly(K) <= ring(K).

scaled_reciprocal_at(poly(Cs), X0) = mk_poly(NewCs) :-
    N = int.'-'(list.length(Cs), 1),
    list.reverse(Cs, RevCs),
    NewCs = list.map_corresponding(
        ( func(Idx, C) = ring_mul(C, pow_k(X0, int.'-'(N, Idx))) ),
        make_int_list(0, N), RevCs).

%---------------------------------------------------------------------------%

negate_var(poly(Cs)) = mk_poly(NegCs) :-
    negate_odd(Cs, (0 : int), NegCs).

:- pred negate_odd(list(K)::in, int::in, list(K)::out) is det <= ring(K).

negate_odd([], _, []).
negate_odd([C | Cs], I, [C1 | Rest]) :-
    ( if odd(I) then C1 = ring_negate(C) else C1 = C ),
    negate_odd(Cs, int.'+'(I, 1), Rest).

reciprocal_poly(poly(Cs)) = mk_poly(list.reverse(Cs)).

substitute_x_n(N, poly(Cs)) = mk_poly(Result) :-
    ( if Cs = [] then
        Result = []
    else
        MaxDeg = int.'*'(int.'-'(list.length(Cs), 1), N),
        Zeros = list.duplicate(int.'+'(MaxDeg, 1), ring_zero),
        set_spaced(Cs, 0, N, Zeros, Result)
    ).

:- pred set_spaced(list(K)::in, int::in, int::in,
    list(K)::in, list(K)::out) is det <= ring(K).

set_spaced([], _, _, !Acc).
set_spaced([C | Cs], Pos, Stride, !Acc) :-
    list.det_replace_nth(!.Acc, int.'+'(Pos, 1), C, !:Acc),
    set_spaced(Cs, int.'+'(Pos, Stride), Stride, !Acc).

shift_poly(poly(Cs), A) = Result :-
    ( if Cs = [] then
        Result = zero_poly
    else
        XPlusA = mk_poly([A, ring_one]),
        list.reverse(Cs, RevCs),
        list.foldl(
            ( pred(C::in, Acc0::in, Acc1::out) is det :-
                Acc1 = add(const_poly(C), mul(XPlusA, Acc0))
            ), RevCs, zero_poly, Result)
    ).

%---------------------------------------------------------------------------%
% Utility
%---------------------------------------------------------------------------%

    % make_points(Start, End) = list of ring_from_int(Start..End).
    %
:- func make_points(int, int) = list(K) <= ring(K).

make_points(Start, End) = Result :-
    ( if int.'>'(Start, End) then
        Result = []
    else
        Result = [ring_from_int(Start) | make_points(int.'+'(Start, 1), End)]
    ).

:- func make_int_list(int, int) = list(int).

make_int_list(Start, End) =
    ( if int.'>'(Start, End) then
        []
    else
        [Start | make_int_list(int.'+'(Start, 1), End)]
    ).

:- func zip_pairs(list(K), list(K)) = list({K, K}).

zip_pairs(As, Bs) =
    ( if As = [A | At], Bs = [B | Bt] then
        [{A, B} | zip_pairs(At, Bt)]
    else
        []
    ).

:- func fst({A, B}) = A.
fst({X, _}) = X.

%---------------------------------------------------------------------------%
:- end_module resultant.
%---------------------------------------------------------------------------%
