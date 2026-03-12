%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: minimal_poly_tower.m
% Main author: nyc
%
% Fast minimal polynomial computation via extension tower.
%
% Instead of computing resultants bottom-up at every AST node (which
% causes exponential blowup when radicals are shared), this module:
%
% 1. Collects distinct radicals in the expression
% 2. Builds an extension tower Q(α₁)(α₂)...
% 3. Evaluates the expression in the tower
% 4. Computes the annihilating polynomial via iterated norms
%    (one resultant per tower level)
% 5. Factors over Q to get the minimal polynomial
%
%---------------------------------------------------------------------------%

:- module minimal_poly_tower.
:- interface.

:- import_module poly.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Compute the minimal polynomial of a radical expression over Q,
    % using the extension tower approach.
    %
:- func minimal_poly_tower(rad_expr(rational)) = poly(rational).

    % Compute an annihilating polynomial (not necessarily minimal) via
    % the extension tower approach.
    %
:- func annihilating_poly_tower(rad_expr(rational)) = poly(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module extension.
:- import_module factoring.
:- use_module float.
:- use_module int.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module pslq.
:- import_module rad_eval.
:- import_module rad_normalize.
:- import_module resultant.
:- import_module root_bound.

%---------------------------------------------------------------------------%
% Top-level entry points
%---------------------------------------------------------------------------%

minimal_poly_tower(Expr) = Result :-
    Ann = annihilating_poly_tower(Expr),
    V = re(eval_complex(Expr)),
    Factors = factor_square_free(monic(Ann)),
    ( if Factors = [] then
        Result = Ann
    else
        Result = monic(pick_closest(V, Factors))
    ).

annihilating_poly_tower(Expr) = Result :-
    Expr1 = normalize(Expr),
    Radicals = collect_radicals(Expr1),
    ( if Radicals = [] then
        ( if eval_rational(Expr1) = yes(R) then
            Result = mk_poly([rational.'-'(rational.zero, R),
                rational.one])
        else
            Result = mk_poly([rational.one])
        )
    else
        Result = compute_via_tower(Expr1, Radicals)
    ).

%---------------------------------------------------------------------------%
% Evaluate as rational
%---------------------------------------------------------------------------%

:- func eval_rational(rad_expr(rational)) = maybe(rational).

eval_rational(re_lit(R)) = yes(R).
eval_rational(re_neg(A)) = Result :-
    ( if eval_rational(A) = yes(R) then
        Result = yes(rational.'-'(rational.zero, R))
    else Result = no ).
eval_rational(re_add(A, B)) = Result :-
    ( if eval_rational(A) = yes(RA), eval_rational(B) = yes(RB) then
        Result = yes(rational.'+'(RA, RB))
    else Result = no ).
eval_rational(re_mul(A, B)) = Result :-
    ( if eval_rational(A) = yes(RA), eval_rational(B) = yes(RB) then
        Result = yes(rational.'*'(RA, RB))
    else Result = no ).
eval_rational(re_inv(A)) = Result :-
    ( if eval_rational(A) = yes(R), R \= rational.zero then
        Result = yes(rational.'/'(rational.one, R))
    else Result = no ).
eval_rational(re_pow(A, N)) = Result :-
    ( if eval_rational(A) = yes(R) then
        Result = yes(rat_pow(R, N))
    else Result = no ).
eval_rational(re_root(_, _)) = no.

:- func rat_pow(rational, int) = rational.

rat_pow(R, N) = Result :-
    ( if N = 0 then Result = rational.one
    else if int.'<'(N, 0) then
        Result = rational.'/'(rational.one, rat_pow(R, int.'-'(0, N)))
    else if int.even(N) then
        H = rat_pow(R, int.'//'(N, 2)),
        Result = rational.'*'(H, H)
    else
        Result = rational.'*'(R, rat_pow(R, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Tower dispatch
%---------------------------------------------------------------------------%

:- func compute_via_tower(rad_expr(rational),
    list({int, rad_expr(rational)})) = poly(rational).

compute_via_tower(Expr, Radicals) = Result :-
    NRads = list.length(Radicals),
    ( if int.'>'(NRads, 3) then
        % For many radicals, try numerical approach first
        TowerDeg = list.foldl(
            ( func({D, _}, Acc) = int.'*'(D, Acc) ), Radicals, 1),
        MaybePoly = numeric_min_poly(Expr, TowerDeg),
        ( if MaybePoly = yes(P) then
            Result = P
        else
            Result = compute_iterative(Expr, Radicals)
        )
    else
        Result = compute_iterative(Expr, Radicals)
    ).

:- func compute_iterative(rad_expr(rational),
    list({int, rad_expr(rational)})) = poly(rational).

compute_iterative(Expr, Radicals) = Result :-
    ( if Radicals = [{D1, R1}] then
        Result = compute_depth1(Expr, {D1, R1})
    else if Radicals = [{D1, R1}, {D2, R2}] then
        Result = compute_depth2(Expr, {D1, R1}, {D2, R2})
    else if Radicals = [{D1, R1}, {D2, R2}, {D3, R3}] then
        Result = compute_depth3(Expr, {D1, R1}, {D2, R2}, {D3, R3})
    else
        Result = compute_deep_fallback(Expr, Radicals)
    ).

%---------------------------------------------------------------------------%
% Depth 1: one radical
%---------------------------------------------------------------------------%

:- func compute_depth1(rad_expr(rational), {int, rad_expr(rational)})
    = poly(rational).

compute_depth1(Expr, Rad) = Result :-
    Rad = {N, Radicand},
    ( if eval_rational(Radicand) = yes(R) then
        NegR = rational.'-'(rational.zero, R),
        Zeros = list.duplicate(int.'-'(N, 1), rational.zero),
        MP = mk_poly([NegR | Zeros] ++ [rational.one]),
        Field = mk_ext_field(MP, "α"),
        Alpha = generator(Field),
        Elem = eval_in_ext(Field, Alpha, Rad, Expr),
        FPoly = mk_poly([ext_neg(Elem), embed(Field, rational.one)]),
        Result = norm_down1(Field, FPoly)
    else
        Result = simple_annihilating(Expr)
    ).

%---------------------------------------------------------------------------%
% Depth 2: two radicals
%---------------------------------------------------------------------------%

:- func compute_depth2(rad_expr(rational),
    {int, rad_expr(rational)}, {int, rad_expr(rational)})
    = poly(rational).

compute_depth2(Expr, Rad1, Rad2) = Result :-
    Rad1 = {N1, Radicand1},
    Rad2 = {N2, Radicand2},
    ( if eval_rational(Radicand1) = yes(R1) then
        NegR1 = rational.'-'(rational.zero, R1),
        Zeros1 = list.duplicate(int.'-'(N1, 1), rational.zero),
        MP1 = mk_poly([NegR1 | Zeros1] ++ [rational.one]),
        Field1 = mk_ext_field(MP1, "α₁"),
        Alpha1 = generator(Field1),
        R2InF1 = eval_in_ext(Field1, Alpha1, Rad1, Radicand2),
        NegR2InF1 = ext_neg(R2InF1),
        Zeros2 = list.duplicate(int.'-'(N2, 1),
            embed(Field1, rational.zero)),
        MP2 = mk_poly([NegR2InF1 | Zeros2] ++
            [embed(Field1, rational.one)]),
        Field2 = mk_ext_field(MP2, "α₂"),
        Alpha2 = generator(Field2),
        Alpha1InF2 = embed(Field2, Alpha1),
        Elem = eval_in_ext2(Field1, Field2, Alpha1InF2, Alpha2,
            Rad1, Rad2, Expr),
        One2 = embed(Field2, embed(Field1, rational.one)),
        FPoly2 = mk_poly([ext_neg(Elem), One2]),
        Result = norm_down1(Field1, norm_down1(Field2, FPoly2))
    else
        Result = simple_annihilating(Expr)
    ).

%---------------------------------------------------------------------------%
% Depth 3: three radicals
%---------------------------------------------------------------------------%

:- func compute_depth3(rad_expr(rational),
    {int, rad_expr(rational)}, {int, rad_expr(rational)},
    {int, rad_expr(rational)}) = poly(rational).

compute_depth3(Expr, Rad1, Rad2, Rad3) = Result :-
    Rad1 = {N1, Radicand1},
    Rad2 = {N2, Radicand2},
    Rad3 = {N3, Radicand3},
    ( if eval_rational(Radicand1) = yes(R1) then
        NegR1 = rational.'-'(rational.zero, R1),
        Zeros1 = list.duplicate(int.'-'(N1, 1), rational.zero),
        MP1 = mk_poly([NegR1 | Zeros1] ++ [rational.one]),
        Field1 = mk_ext_field(MP1, "α₁"),
        Alpha1 = generator(Field1),

        R2InF1 = eval_in_ext(Field1, Alpha1, Rad1, Radicand2),
        NegR2 = ext_neg(R2InF1),
        Zeros2 = list.duplicate(int.'-'(N2, 1),
            embed(Field1, rational.zero)),
        MP2 = mk_poly([NegR2 | Zeros2] ++
            [embed(Field1, rational.one)]),
        Field2 = mk_ext_field(MP2, "α₂"),
        Alpha2 = generator(Field2),
        Alpha1InF2 = embed(Field2, Alpha1),

        R3InF2 = eval_in_ext2(Field1, Field2, Alpha1InF2, Alpha2,
            Rad1, Rad2, Radicand3),
        NegR3 = ext_neg(R3InF2),
        Zeros3 = list.duplicate(int.'-'(N3, 1),
            embed(Field2, embed(Field1, rational.zero))),
        MP3 = mk_poly([NegR3 | Zeros3] ++
            [embed(Field2, embed(Field1, rational.one))]),
        Field3 = mk_ext_field(MP3, "α₃"),
        Alpha3 = generator(Field3),
        Alpha1InF3 = embed(Field3, embed(Field2, Alpha1)),
        Alpha2InF3 = embed(Field3, Alpha2),

        Elem = eval_in_ext3(Field1, Field2, Field3,
            Alpha1InF3, Alpha2InF3, Alpha3,
            Rad1, Rad2, Rad3, Expr),
        One3 = embed(Field3, embed(Field2, embed(Field1, rational.one))),
        FPoly3 = mk_poly([ext_neg(Elem), One3]),
        Result = norm_down1(Field1,
            norm_down1(Field2,
                norm_down1(Field3, FPoly3)))
    else
        Result = simple_annihilating(Expr)
    ).

%---------------------------------------------------------------------------%
% Deep fallback: build chain + try depths or use simple
%---------------------------------------------------------------------------%

:- func compute_deep_fallback(rad_expr(rational),
    list({int, rad_expr(rational)})) = poly(rational).

compute_deep_fallback(Expr, Radicals) = Result :-
    Chain = build_chain(topo_sort_radicals(Radicals)),
    ( if Chain = [] then
        Result = simple_annihilating(Expr)
    else if Chain = [R1] then
        Result = compute_depth1(Expr, R1)
    else if Chain = [R1, R2] then
        Result = compute_depth2(Expr, R1, R2)
    else if Chain = [R1, R2, R3] then
        Result = compute_depth3(Expr, R1, R2, R3)
    else
        Result = simple_annihilating(Expr)
    ).

:- func build_chain(list({int, rad_expr(rational)}))
    = list({int, rad_expr(rational)}).

build_chain(Sorted) = build_chain_acc(Sorted, []).

:- func build_chain_acc(list({int, rad_expr(rational)}),
    list({int, rad_expr(rational)}))
    = list({int, rad_expr(rational)}).

build_chain_acc([], Resolved) = Resolved.
build_chain_acc([R | Rs], Resolved) = Result :-
    R = {_, Radicand},
    ( if all_roots_resolved(Resolved, Radicand) then
        Result = build_chain_acc(Rs, Resolved ++ [R])
    else
        Result = Resolved
    ).

%---------------------------------------------------------------------------%
% Evaluate in extension fields
%---------------------------------------------------------------------------%

:- func eval_in_ext(ext_field(rational), ext_elem(rational),
    {int, rad_expr(rational)}, rad_expr(rational))
    = ext_elem(rational).

eval_in_ext(Field, Alpha, Rad, Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = embed(Field, R)
    else if Expr = re_neg(A) then
        Result = ext_neg(eval_in_ext(Field, Alpha, Rad, A))
    else if Expr = re_add(A, B) then
        Result = ext_add(eval_in_ext(Field, Alpha, Rad, A),
            eval_in_ext(Field, Alpha, Rad, B))
    else if Expr = re_mul(A, B) then
        Result = ext_mul(eval_in_ext(Field, Alpha, Rad, A),
            eval_in_ext(Field, Alpha, Rad, B))
    else if Expr = re_inv(A) then
        Result = ext_inv(eval_in_ext(Field, Alpha, Rad, A))
    else if Expr = re_root(N, A), {N, A} = Rad then
        Result = Alpha
    else if Expr = re_pow(A, N) then
        V = eval_in_ext(Field, Alpha, Rad, A),
        ( if int.'>='(N, 0) then
            Result = ext_pow(V, N)
        else
            Result = ext_inv(ext_pow(V, int.'-'(0, N)))
        )
    else
        Result = embed(Field, rational.zero)
    ).

:- func eval_in_ext2(
    ext_field(rational),
    ext_field(ext_elem(rational)),
    ext_elem(ext_elem(rational)),
    ext_elem(ext_elem(rational)),
    {int, rad_expr(rational)},
    {int, rad_expr(rational)},
    rad_expr(rational)) = ext_elem(ext_elem(rational)).

eval_in_ext2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, Expr)
        = Result :-
    ( if Expr = re_lit(R) then
        Result = embed(Field2, embed(Field1, R))
    else if Expr = re_neg(A) then
        Result = ext_neg(eval_in_ext2(Field1, Field2, Alpha1, Alpha2,
            Rad1, Rad2, A))
    else if Expr = re_add(A, B) then
        Result = ext_add(
            eval_in_ext2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, A),
            eval_in_ext2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, B))
    else if Expr = re_mul(A, B) then
        Result = ext_mul(
            eval_in_ext2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, A),
            eval_in_ext2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, B))
    else if Expr = re_inv(A) then
        Result = ext_inv(eval_in_ext2(Field1, Field2, Alpha1, Alpha2,
            Rad1, Rad2, A))
    else if Expr = re_root(M, A), {M, A} = Rad1 then
        Result = Alpha1
    else if Expr = re_root(M, A), {M, A} = Rad2 then
        Result = Alpha2
    else if Expr = re_pow(A, M) then
        V = eval_in_ext2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, A),
        ( if int.'>='(M, 0) then
            Result = ext_pow(V, M)
        else
            Result = ext_inv(ext_pow(V, int.'-'(0, M)))
        )
    else
        Result = embed(Field2, embed(Field1, rational.zero))
    ).

:- func eval_in_ext3(
    ext_field(rational),
    ext_field(ext_elem(rational)),
    ext_field(ext_elem(ext_elem(rational))),
    ext_elem(ext_elem(ext_elem(rational))),
    ext_elem(ext_elem(ext_elem(rational))),
    ext_elem(ext_elem(ext_elem(rational))),
    {int, rad_expr(rational)},
    {int, rad_expr(rational)},
    {int, rad_expr(rational)},
    rad_expr(rational)) = ext_elem(ext_elem(ext_elem(rational))).

eval_in_ext3(F1, F2, F3, A1, A2, A3, R1, R2, R3, Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = embed(F3, embed(F2, embed(F1, R)))
    else if Expr = re_neg(A) then
        Result = ext_neg(eval_in_ext3(F1, F2, F3, A1, A2, A3,
            R1, R2, R3, A))
    else if Expr = re_add(A, B) then
        Result = ext_add(
            eval_in_ext3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A),
            eval_in_ext3(F1, F2, F3, A1, A2, A3, R1, R2, R3, B))
    else if Expr = re_mul(A, B) then
        Result = ext_mul(
            eval_in_ext3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A),
            eval_in_ext3(F1, F2, F3, A1, A2, A3, R1, R2, R3, B))
    else if Expr = re_inv(A) then
        Result = ext_inv(
            eval_in_ext3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A))
    else if Expr = re_root(M, A), {M, A} = R1 then
        Result = A1
    else if Expr = re_root(M, A), {M, A} = R2 then
        Result = A2
    else if Expr = re_root(M, A), {M, A} = R3 then
        Result = A3
    else if Expr = re_pow(A, M) then
        V = eval_in_ext3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A),
        ( if int.'>='(M, 0) then
            Result = ext_pow(V, M)
        else
            Result = ext_inv(ext_pow(V, int.'-'(0, M)))
        )
    else
        Result = embed(F3, embed(F2, embed(F1, rational.zero)))
    ).

%---------------------------------------------------------------------------%
% Norm down one level
%---------------------------------------------------------------------------%

:- func norm_down1(ext_field(K), poly(ext_elem(K))) = poly(K)
    <= field(K).

norm_down1(Field, F) = Result :-
    DF = degree(F),
    DM = ext_degree(Field),
    ResultDeg = int.'*'(DF, DM),
    MP = gen_min_poly(Field),
    Points = list.map(
        ( func(I) = int_to_k(I) ),
        int_range(0, ResultDeg)),
    Values = list.map(
        ( func(X0) = poly_resultant(MP, eval_poly_ext(F, X0)) ),
        Points),
    Result = lagrange_interpolate(zip_tuples(Points, Values)).

:- func int_range(int, int) = list(int).

int_range(Lo, Hi) = Result :-
    ( if int.'>'(Lo, Hi) then Result = []
    else Result = [Lo | int_range(int.'+'(Lo, 1), Hi)]
    ).

:- func zip_tuples(list(K), list(K)) = list({K, K}).

zip_tuples(As, Bs) = Result :-
    ( if As = [A | At], Bs = [B | Bt] then
        Result = [{A, B} | zip_tuples(At, Bt)]
    else
        Result = []
    ).

:- func int_to_k(int) = K <= field(K).

int_to_k(N) = Result :-
    ( if N = 0 then Result = ring_zero
    else if N = 1 then Result = ring_one
    else if int.'>'(N, 0) then
        Result = ring_add(ring_one, int_to_k(int.'-'(N, 1)))
    else
        Result = ring_sub(ring_zero, int_to_k(int.'-'(0, N)))
    ).

%---------------------------------------------------------------------------%
% Evaluate f(x₀) where f has ExtElem coefficients
%---------------------------------------------------------------------------%

:- func eval_poly_ext(poly(ext_elem(K)), K) = poly(K) <= field(K).

eval_poly_ext(P, X0) = Result :-
    Cs = coeffs(P),
    Result = eval_poly_ext_acc(Cs, X0, ring_one, zero_poly).

:- func eval_poly_ext_acc(list(ext_elem(K)), K, K, poly(K)) = poly(K)
    <= field(K).

eval_poly_ext_acc([], _, _, Acc) = Acc.
eval_poly_ext_acc([C | Cs], X0, XPow, Acc) = Result :-
    CPoly = elem_poly(C),
    Term = scale(XPow, CPoly),
    Result = eval_poly_ext_acc(Cs, X0,
        ring_mul(XPow, X0), add(Acc, Term)).

%---------------------------------------------------------------------------%
% Simple annihilating polynomial (non-tower fallback)
%---------------------------------------------------------------------------%

:- func simple_annihilating(rad_expr(rational)) = poly(rational).

simple_annihilating(Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = mk_poly([rational.'-'(rational.zero, R), rational.one])
    else if Expr = re_neg(E) then
        Result = negate_var(simple_annihilating(E))
    else if Expr = re_add(A, B) then
        Result = composed_sum(simple_annihilating(A),
            simple_annihilating(B))
    else if Expr = re_mul(A, B) then
        Result = composed_product(simple_annihilating(A),
            simple_annihilating(B))
    else if Expr = re_inv(E) then
        Result = reciprocal_poly(simple_annihilating(E))
    else if Expr = re_root(N, re_lit(R)) then
        NegR = rational.'-'(rational.zero, R),
        Zeros = list.duplicate(int.'-'(N, 1), rational.zero),
        Result = mk_poly([NegR | Zeros] ++ [rational.one])
    else if Expr = re_root(N, E) then
        Result = substitute_x_n(N, simple_annihilating(E))
    else if Expr = re_pow(E, N) then
        ( if int.'>='(N, 0) then
            Result = annih_pow(simple_annihilating(E), N)
        else
            Result = annih_pow(
                reciprocal_poly(simple_annihilating(E)),
                int.'-'(0, N))
        )
    else
        Result = mk_poly([rational.one])
    ).

:- func annih_pow(poly(rational), int) = poly(rational).

annih_pow(P, N) = Result :-
    ( if N = 0 then
        Result = mk_poly([rational.'-'(rational.zero, rational.one),
            rational.one])
    else if N = 1 then
        Result = P
    else
        DP = degree(P),
        Points = list.map(
            ( func(I) = rational.rational(I, 1) ),
            int_range(0, DP)),
        Values = list.map(
            ( func(X0) = pow_res_at(P, N, X0) ), Points),
        Result = lagrange_interpolate(zip_tuples(Points, Values))
    ).

:- func pow_res_at(poly(rational), int, rational) = rational.

pow_res_at(P, N, X0) = Result :-
    NegX0 = rational.'-'(rational.zero, X0),
    Zeros = list.duplicate(int.'-'(N, 1), rational.zero),
    YnMinusX0 = mk_poly([NegX0 | Zeros] ++ [rational.one]),
    Result = poly_resultant(P, YnMinusX0).

%---------------------------------------------------------------------------%
% Numerical minimal polynomial via PSLQ
%---------------------------------------------------------------------------%

:- func numeric_min_poly(rad_expr(rational), int) = maybe(poly(rational)).

numeric_min_poly(Expr, MaxDeg) = Result :-
    Alpha = re(eval_complex(Expr)),
    SearchDeg = int.min(MaxDeg, 20),
    ( if find_min_poly(Alpha, SearchDeg) = yes(Coeffs) then
        Result = yes(mk_poly(list.map(
            ( func(C) = rational.rational(C, 1) ), Coeffs)))
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Pick closest factor
%---------------------------------------------------------------------------%

:- func pick_closest(float, list(poly(rational))) = poly(rational).

pick_closest(Target, Factors) = Result :-
    ( if Factors = [F] then
        Result = F
    else if Factors = [_ | Rest], Rest = [_ | _] then
        Scored = list.map(
            ( func(F) = {F, min_root_dist(Target, approx_roots(F))} ),
            Factors),
        Result = fst(list.foldl(
            ( func(Pair, Best) = R :-
                {_, S} = Pair,
                {_, BS} = Best,
                ( if float.'=<'(S, BS) then R = Pair
                else R = Best )
            ),
            list.det_tail(Scored), list.det_head(Scored)))
    else
        Result = list.det_head(Factors)
    ).

:- func fst({A, B}) = A.
fst({X, _}) = X.

:- func min_root_dist(float, list(rational)) = float.

min_root_dist(Target, Roots) = Result :-
    Dists = list.map(
        ( func(R) =
            float.abs(float.'-'(rational_to_float(R), Target)) ),
        Roots),
    ( if Dists = [D | Ds] then
        Result = list.foldl(float.min, Ds, D)
    else
        Result = 1.0e30
    ).

:- func rational_to_float(rational) = float.

rational_to_float(R) =
    float.'/'(
        float.float(integer.det_to_int(numer(R))),
        float.float(integer.det_to_int(denom(R)))).

%---------------------------------------------------------------------------%
:- end_module minimal_poly_tower.
%---------------------------------------------------------------------------%
