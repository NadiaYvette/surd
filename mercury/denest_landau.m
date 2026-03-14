%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: denest_landau.m
% Main author: nyc
%
% Landau's denesting algorithm for nested radicals.
%
% Given ⁿ√a where a is in a radical extension K/Q, determines whether
% ⁿ√a can be expressed using radicals of lower nesting depth by
% factoring x^n - a over K using Trager's algorithm.
%
% If x^n - a has a factor of degree d < n over K, then ⁿ√a can be
% expressed using d-th roots instead of n-th roots.
%
%---------------------------------------------------------------------------%

:- module denest_landau.
:- interface.

:- import_module maybe.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Try to denest all radicals in an expression using Landau's algorithm.
    % Applies denesting recursively, bottom-up.
    %
:- func denest_landau(rad_expr(rational)) = rad_expr(rational).

    % Try to denest a specific radical: ⁿ√a.
    % Returns yes(simplified) or no.
    %
:- func denest_radical(int, rad_expr(rational)) = maybe(rad_expr(rational)).

    % Enumerate all possible Landau denestings of an expression.
    % Produces alternatives via backtracking (nondet).
    %
:- pred denest_landau_multi(rad_expr(rational)::in,
    rad_expr(rational)::out) is nondet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module extension.
:- use_module float.
:- use_module int.
:- import_module integer.
:- import_module list.
:- import_module poly.
:- import_module rad_eval.
:- import_module trager_factoring.

%---------------------------------------------------------------------------%
% Top-level denesting
%---------------------------------------------------------------------------%

denest_landau(Expr) = go(Expr).

:- func go(rad_expr(rational)) = rad_expr(rational).

go(re_lit(R)) = re_lit(R).
go(re_neg(A)) = re_neg(go(A)).
go(re_add(A, B)) = re_add(go(A), go(B)).
go(re_mul(A, B)) = re_mul(go(A), go(B)).
go(re_inv(A)) = re_inv(go(A)).
go(re_pow(A, N)) = re_pow(go(A), N).
go(re_root(N, A)) = Result :-
    A1 = go(A),
    DR = denest_radical(N, A1),
    ( if DR = yes(Simplified) then
        Result = go(Simplified)
    else
        Result = re_root(N, A1)
    ).

%---------------------------------------------------------------------------%
% Denest a specific radical
%---------------------------------------------------------------------------%

denest_radical(N, Radicand) = Result :-
    Simple = try_simple_denest(N, Radicand),
    ( if Simple = yes(_) then
        Result = Simple
    else
        Result = try_trager_denest(N, Radicand)
    ).

:- func try_simple_denest(int, rad_expr(rational))
    = maybe(rad_expr(rational)).

try_simple_denest(_, _) = no.

%---------------------------------------------------------------------------%
% Trager-based denesting
%---------------------------------------------------------------------------%

:- func try_trager_denest(int, rad_expr(rational))
    = maybe(rad_expr(rational)).

try_trager_denest(N, Radicand) = Result :-
    Radicals = topo_sort_radicals(collect_radicals(Radicand)),
    ( if Radicals = [] then
        Result = no
    else if Radicals = [{D1, R1}] then
        Result = try_denest_depth1(N, Radicand, D1, R1)
    else if Radicals = [{D1, R1}, {D2, R2}] then
        Result = try_denest_depth2(N, Radicand, {D1, R1}, {D2, R2})
    else if Radicals = [{D1, R1}, {D2, R2}, {D3, R3}] then
        Result = try_denest_depth3(N, Radicand,
            {D1, R1}, {D2, R2}, {D3, R3})
    else if Radicals = [{D1, R1}, {D2, R2}, {D3, R3}, {D4, R4}] then
        Result = try_denest_depth4(N, Radicand,
            {D1, R1}, {D2, R2}, {D3, R3}, {D4, R4})
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Depth 1: single radical
%---------------------------------------------------------------------------%

:- func try_denest_depth1(int, rad_expr(rational), int, rad_expr(rational))
    = maybe(rad_expr(rational)).

try_denest_depth1(N, Radicand, Deg, Rad) = Result :-
    MaybeR = eval_rat(Rad),
    ( if MaybeR = yes(R) then
        NegR = rational.'-'(rational.zero, R),
        Zeros = list.duplicate(int.'-'(Deg, 1), rational.zero),
        MP = mk_poly([NegR | Zeros] ++ [rational.one]),
        Field = mk_ext_field(MP, "α"),
        Alpha = generator(Field),
        AInK = eval_in_k(Field, Alpha, {Deg, Rad}, Radicand),
        NegAInK = ext_neg(AInK),
        ZerosK = list.duplicate(int.'-'(N, 1),
            embed(Field, rational.zero)),
        XnMinusA = mk_poly([NegAInK | ZerosK] ++
            [embed(Field, rational.one)]),
        Factors = factor_sf_over_extension(Field, XnMinusA),
        LowDeg = list.filter(
            ( pred(Fac::in) is semidet :-
                int.'>'(poly.degree(Fac), 0),
                int.'<'(poly.degree(Fac), N)
            ),
            Factors),
        ( if LowDeg = [F | _] then
            Result = extract_root(Field, Alpha, {Deg, Rad}, F)
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Root extraction from low-degree factors
%---------------------------------------------------------------------------%

:- func extract_root(ext_field(rational), ext_elem(rational),
    {int, rad_expr(rational)}, poly(ext_elem(rational)))
    = maybe(rad_expr(rational)).

extract_root(Field, Alpha, Rad, F) =
    extract_root_generic(ext_elem_to_rad_expr(Field, Alpha, Rad), F).

:- func extract_root_generic(
    (func(K) = rad_expr(rational)),
    poly(K)) = maybe(rad_expr(rational)) <= field(K).

extract_root_generic(ToExpr, F) = Result :-
    D = poly.degree(F),
    Cs = poly.coeffs(F),
    ( if D = 1 then
        ( if Cs = [C, _] then
            Result = yes(ToExpr(field_neg(C)))
        else
            Result = no
        )
    else if D = 2 then
        ( if Cs = [C, B, _] then
            Disc = field_sub(field_mul(B, B),
                field_scale(rational(4, 1), C)),
            SqrtDisc = ToExpr(Disc),
            NegB = ToExpr(field_neg(B)),
            R1 = re_mul(re_inv(re_lit(rational(2, 1))),
                re_add(NegB, re_root(2, SqrtDisc))),
            R2 = re_mul(re_inv(re_lit(rational(2, 1))),
                re_add(NegB, re_neg(re_root(2, SqrtDisc)))),
            Result = yes(pick_closest_to_real([R1, R2]))
        else
            Result = no
        )
    else if D = 3 then
        ( if Cs = [DD, C, B, _] then
            % Depressed cubic: t³ + pt + q = 0
            P = field_sub(C, field_scale(rational(1, 3), field_mul(B, B))),
            Q = field_sub(field_add(DD,
                    field_scale(rational(2, 27), field_mul(B, field_mul(B, B)))),
                field_scale(rational(1, 3), field_mul(B, C))),
            Shift = re_neg(ToExpr(field_scale(rational(1, 3), B))),
            HalfQ = ToExpr(field_scale(rational(1, 2), Q)),
            InnerDisc = field_add(
                field_scale(rational(1, 4), field_mul(Q, Q)),
                field_scale(rational(1, 27), field_mul(P, field_mul(P, P)))),
            SqrtD = re_root(2, ToExpr(InnerDisc)),
            U = re_root(3, re_add(re_neg(HalfQ), SqrtD)),
            V = re_root(3, re_add(re_neg(HalfQ), re_neg(SqrtD))),
            Omega = re_mul(re_inv(re_lit(rational(2, 1))),
                re_add(re_lit(rational(-1, 1)),
                    re_root(2, re_lit(rational(-3, 1))))),
            Omega2 = re_mul(re_inv(re_lit(rational(2, 1))),
                re_add(re_lit(rational(-1, 1)),
                    re_neg(re_root(2, re_lit(rational(-3, 1)))))),
            Root0 = re_add(re_add(U, V), Shift),
            Root1 = re_add(re_add(re_mul(Omega, U),
                re_mul(Omega2, V)), Shift),
            Root2 = re_add(re_add(re_mul(Omega2, U),
                re_mul(Omega, V)), Shift),
            Result = yes(pick_closest_to_real([Root0, Root1, Root2]))
        else
            Result = no
        )
    else if D = 4 then
        ( if Cs = [EE, DD, C, B, _] then
            P = field_sub(C,
                field_scale(rational(3, 8), field_mul(B, B))),
            Q = field_sub(field_add(DD,
                    field_scale(rational(1, 8),
                        field_mul(B, field_mul(B, B)))),
                field_scale(rational(1, 2), field_mul(B, C))),
            R = field_sub(field_add(EE,
                    field_scale(rational(1, 16), field_mul(B, field_mul(B, C)))),
                field_sub(
                    field_scale(rational(1, 4), field_mul(B, DD)),
                    field_scale(rational(3, 256),
                        field_mul(B, field_mul(B, field_mul(B, B)))))),
            BE = ToExpr(B),
            ShiftBack = ( func(Expr) =
                re_add(Expr, re_neg(re_mul(
                    re_inv(re_lit(rational(4, 1))), BE))) ),
            ( if field_is_zero(Q) then
                % Biquadratic
                Disc2 = field_sub(field_mul(P, P),
                    field_scale(rational(4, 1), R)),
                SqrtDisc2 = re_root(2, ToExpr(Disc2)),
                PE = ToExpr(P),
                T2_1 = re_mul(re_inv(re_lit(rational(2, 1))),
                    re_add(re_neg(PE), SqrtDisc2)),
                T2_2 = re_mul(re_inv(re_lit(rational(2, 1))),
                    re_add(re_neg(PE), re_neg(SqrtDisc2))),
                Roots = [
                    ShiftBack(re_root(2, T2_1)),
                    ShiftBack(re_neg(re_root(2, T2_1))),
                    ShiftBack(re_root(2, T2_2)),
                    ShiftBack(re_neg(re_root(2, T2_2)))
                ],
                Result = yes(pick_closest_to_real(Roots))
            else
                % General quartic: resolvent cubic
                RcD = field_sub(field_scale(rational(4, 1),
                    field_mul(P, R)), field_mul(Q, Q)),
                RcC = field_scale(rational(-4, 1), R),
                RcB = field_neg(P),
                RcPoly = mk_poly([RcD, RcC, RcB, field_one]),
                MaybeY = extract_root_generic(ToExpr, RcPoly),
                ( if MaybeY = yes(YExpr) then
                    SSquared = re_add(YExpr, re_neg(ToExpr(P))),
                    S = re_root(2, SSquared),
                    HalfY = re_mul(re_inv(re_lit(rational(2, 1))), YExpr),
                    QOver2S = re_mul(ToExpr(field_scale(rational(1, 2), Q)),
                        re_inv(S)),
                    C1 = re_add(HalfY, re_neg(QOver2S)),
                    C2 = re_add(HalfY, QOver2S),
                    Disc1 = re_add(SSquared,
                        re_mul(re_lit(rational(-4, 1)), C1)),
                    Disc2B = re_add(SSquared,
                        re_mul(re_lit(rational(-4, 1)), C2)),
                    SqD1 = re_root(2, Disc1),
                    SqD2 = re_root(2, Disc2B),
                    T1 = re_mul(re_inv(re_lit(rational(2, 1))),
                        re_add(re_neg(S), SqD1)),
                    T2 = re_mul(re_inv(re_lit(rational(2, 1))),
                        re_add(re_neg(S), re_neg(SqD1))),
                    T3 = re_mul(re_inv(re_lit(rational(2, 1))),
                        re_add(S, SqD2)),
                    T4 = re_mul(re_inv(re_lit(rational(2, 1))),
                        re_add(S, re_neg(SqD2))),
                    Roots = list.map(ShiftBack, [T1, T2, T3, T4]),
                    Result = yes(pick_closest_to_real(Roots))
                else
                    Result = no
                )
            )
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Field arithmetic helpers (generic over field K)
%---------------------------------------------------------------------------%

:- func field_neg(K) = K <= field(K).
field_neg(X) = ring_sub(ring_zero, X).

:- func field_add(K, K) = K <= field(K).
field_add(X, Y) = ring_add(X, Y).

:- func field_sub(K, K) = K <= field(K).
field_sub(X, Y) = ring_sub(X, Y).

:- func field_mul(K, K) = K <= field(K).
field_mul(X, Y) = ring_mul(X, Y).

:- func field_scale(rational, K) = K <= field(K).
field_scale(R, X) = Result :-
    ( if R = rational.zero then
        Result = ring_zero
    else if R = rational.one then
        Result = X
    else
        % Embed rational as field element and multiply
        N = numer(R),
        D = denom(R),
        NE = int_to_field(integer.det_to_int(N)),
        DE = int_to_field(integer.det_to_int(D)),
        Result = ring_mul(field_div(NE, DE), X)
    ).

:- func field_one = K <= field(K).
field_one = ring_one.

:- pred field_is_zero(K::in) is semidet <= field(K).
field_is_zero(X) :- X = ring_zero.

:- func int_to_field(int) = K <= field(K).
int_to_field(N) = Result :-
    ( if N = 0 then
        Result = ring_zero
    else if N = 1 then
        Result = ring_one
    else if int.'>'(N, 0) then
        Result = ring_add(ring_one, int_to_field(int.'-'(N, 1)))
    else
        Result = ring_sub(ring_zero, int_to_field(int.'-'(0, N)))
    ).

%---------------------------------------------------------------------------%
% Evaluate RadExpr as rational
%---------------------------------------------------------------------------%

:- func eval_rat(rad_expr(rational)) = maybe(rational).

eval_rat(re_lit(R)) = yes(R).
eval_rat(re_neg(A)) = Result :-
    ( if eval_rat(A) = yes(R) then
        Result = yes(rational.'-'(rational.zero, R))
    else
        Result = no
    ).
eval_rat(re_add(A, B)) = Result :-
    ( if eval_rat(A) = yes(RA), eval_rat(B) = yes(RB) then
        Result = yes(rational.'+'(RA, RB))
    else
        Result = no
    ).
eval_rat(re_mul(A, B)) = Result :-
    ( if eval_rat(A) = yes(RA), eval_rat(B) = yes(RB) then
        Result = yes(rational.'*'(RA, RB))
    else
        Result = no
    ).
eval_rat(re_inv(A)) = Result :-
    ( if eval_rat(A) = yes(R), R \= rational.zero then
        Result = yes(rational.'/'(rational.one, R))
    else
        Result = no
    ).
eval_rat(re_pow(A, N)) = Result :-
    ( if eval_rat(A) = yes(R) then
        Result = yes(rat_pow(R, N))
    else
        Result = no
    ).
eval_rat(re_root(_, _)) = no.

:- func rat_pow(rational, int) = rational.

rat_pow(R, N) = Result :-
    ( if N = 0 then
        Result = rational.one
    else if int.'<'(N, 0) then
        Result = rational.'/'(rational.one, rat_pow(R, int.'-'(0, N)))
    else if int.even(N) then
        H = rat_pow(R, int.'//'(N, 2)),
        Result = rational.'*'(H, H)
    else
        Result = rational.'*'(R, rat_pow(R, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Evaluate RadExpr in Q(α)
%---------------------------------------------------------------------------%

:- func eval_in_k(ext_field(rational), ext_elem(rational),
    {int, rad_expr(rational)}, rad_expr(rational)) = ext_elem(rational).

eval_in_k(Field, Alpha, Rad, Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = embed(Field, R)
    else if Expr = re_neg(A) then
        Result = ext_neg(eval_in_k(Field, Alpha, Rad, A))
    else if Expr = re_add(A, B) then
        Result = ext_add(eval_in_k(Field, Alpha, Rad, A),
            eval_in_k(Field, Alpha, Rad, B))
    else if Expr = re_mul(A, B) then
        Result = ext_mul(eval_in_k(Field, Alpha, Rad, A),
            eval_in_k(Field, Alpha, Rad, B))
    else if Expr = re_inv(A) then
        Result = ext_inv(eval_in_k(Field, Alpha, Rad, A))
    else if Expr = re_root(N, A), {N, A} = Rad then
        Result = Alpha
    else if Expr = re_pow(A, N) then
        V = eval_in_k(Field, Alpha, Rad, A),
        ( if int.'>='(N, 0) then
            Result = ext_pow(V, N)
        else
            Result = ext_inv(ext_pow(V, int.'-'(0, N)))
        )
    else
        Result = embed(Field, rational.zero)
    ).

%---------------------------------------------------------------------------%
% Convert ExtElem to RadExpr
%---------------------------------------------------------------------------%

:- func ext_elem_to_rad_expr(ext_field(rational), ext_elem(rational),
    {int, rad_expr(rational)}, ext_elem(rational)) = rad_expr(rational).

ext_elem_to_rad_expr(_, _, {Deg, Rad}, Elem) = Result :-
    Radical = re_root(Deg, Rad),
    Cs = poly.coeffs(elem_poly(Elem)),
    Terms = make_ext_terms(Cs, Radical, 0),
    ( if Terms = [] then
        Result = re_lit(rational.zero)
    else if Terms = [T] then
        Result = T
    else
        Result = list.foldl(
            ( func(X, Acc) = re_add(Acc, X) ),
            list.det_tail(Terms), list.det_head(Terms))
    ).

:- func make_ext_terms(list(rational), rad_expr(rational), int)
    = list(rad_expr(rational)).

make_ext_terms([], _, _) = [].
make_ext_terms([C | Cs], Gen, I) = Result :-
    ( if C = rational.zero then
        Result = make_ext_terms(Cs, Gen, int.'+'(I, 1))
    else
        Term = mk_coeff_term(C, Gen, I),
        Result = [Term | make_ext_terms(Cs, Gen, int.'+'(I, 1))]
    ).

:- func mk_coeff_term(rational, rad_expr(rational), int)
    = rad_expr(rational).

mk_coeff_term(C, A, I) = Result :-
    ( if I = 0 then
        Result = re_lit(C)
    else
        GenPow = ( if I = 1 then A else re_pow(A, I) ),
        ( if C = rational.one then
            Result = GenPow
        else
            Result = re_mul(re_lit(C), GenPow)
        )
    ).

%---------------------------------------------------------------------------%
% Pick closest root to real axis
%---------------------------------------------------------------------------%

:- func pick_closest_to_real(list(rad_expr(rational))) = rad_expr(rational).

pick_closest_to_real(Roots) = Result :-
    ( if Roots = [R] then
        Result = R
    else if Roots = [_ | Rest], Rest = [_ | _] then
        Scored = list.map(
            ( func(R) = {R, float.abs(im(eval_complex(R)))} ),
            Roots),
        Result = fst(list.foldl(
            ( func(Pair, Best) = R :-
                {_, S} = Pair,
                {_, BS} = Best,
                ( if float.'=<'(S, BS) then R = Pair
                else R = Best )
            ),
            list.det_tail(Scored), list.det_head(Scored)))
    else
        Result = re_lit(rational.zero)
    ).

:- func fst({A, B}) = A.
fst({X, _}) = X.

%---------------------------------------------------------------------------%
% Depth 2: two radicals
%---------------------------------------------------------------------------%

:- func try_denest_depth2(int, rad_expr(rational),
    {int, rad_expr(rational)}, {int, rad_expr(rational)})
    = maybe(rad_expr(rational)).

try_denest_depth2(N, Radicand, Rad1, Rad2) = Result :-
    Rad1 = {D1, R1},
    Rad2 = {D2, R2},
    MaybeQ1 = eval_rat(R1),
    MaybeQ2 = eval_rat(R2),
    ( if MaybeQ1 = yes(Q1) then
        % Build Q(α₁)
        NegQ1 = rational.'-'(rational.zero, Q1),
        Zeros1 = list.duplicate(int.'-'(D1, 1), rational.zero),
        MP1 = mk_poly([NegQ1 | Zeros1] ++ [rational.one]),
        Field1 = mk_ext_field(MP1, "α₁"),
        Alpha1 = generator(Field1),
        % Build Q(α₁)(α₂)
        R2InF1 = ( if MaybeQ2 = yes(Q2) then embed(Field1, Q2)
                    else eval_in_k(Field1, Alpha1, Rad1, R2) ),
        NegR2InF1 = ext_neg(R2InF1),
        Zeros2 = list.duplicate(int.'-'(D2, 1),
            embed(Field1, rational.zero)),
        MP2 = mk_poly([NegR2InF1 | Zeros2] ++
            [embed(Field1, rational.one)]),
        Field2 = mk_ext_field(MP2, "α₂"),
        Alpha2 = generator(Field2),
        Alpha1InF2 = embed(Field2, Alpha1),
        % Evaluate radicand in Q(α₁)(α₂)
        AInK = eval_in_k2(Field1, Field2, Alpha1InF2, Alpha2,
            Rad1, Rad2, Radicand),
        One2 = embed(Field2, embed(Field1, rational.one)),
        NegAInK = ext_neg(AInK),
        ZerosN = list.duplicate(int.'-'(N, 1),
            embed(Field2, embed(Field1, rational.zero))),
        XnMinusA = mk_poly([NegAInK | ZerosN] ++ [One2]),
        FactorOverK = ( func(P) =
            factor_sf_over_extension(Field1, P) ),
        Factors = factor_sf_over_extension_k(FactorOverK,
            Field2, XnMinusA),
        LowDeg = list.filter(
            ( pred(Fac::in) is semidet :-
                int.'>'(poly.degree(Fac), 0),
                int.'<'(poly.degree(Fac), N)
            ),
            Factors),
        ( if LowDeg = [F | _] then
            ToExpr = ext_elem_to_rad_expr2(Field1,
                Alpha1InF2, Alpha2, Rad1, Rad2),
            Result = extract_root_generic(ToExpr, F)
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Evaluate in Q(α₁)(α₂)
%---------------------------------------------------------------------------%

:- func eval_in_k2(
    ext_field(rational),
    ext_field(ext_elem(rational)),
    ext_elem(ext_elem(rational)),
    ext_elem(ext_elem(rational)),
    {int, rad_expr(rational)},
    {int, rad_expr(rational)},
    rad_expr(rational)) = ext_elem(ext_elem(rational)).

eval_in_k2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = embed(Field2, embed(Field1, R))
    else if Expr = re_neg(A) then
        Result = ext_neg(eval_in_k2(Field1, Field2, Alpha1, Alpha2,
            Rad1, Rad2, A))
    else if Expr = re_add(A, B) then
        Result = ext_add(
            eval_in_k2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, A),
            eval_in_k2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, B))
    else if Expr = re_mul(A, B) then
        Result = ext_mul(
            eval_in_k2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, A),
            eval_in_k2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, B))
    else if Expr = re_inv(A) then
        Result = ext_inv(eval_in_k2(Field1, Field2, Alpha1, Alpha2,
            Rad1, Rad2, A))
    else if Expr = re_root(M, A), {M, A} = Rad1 then
        Result = Alpha1
    else if Expr = re_root(M, A), {M, A} = Rad2 then
        Result = Alpha2
    else if Expr = re_pow(A, M) then
        V = eval_in_k2(Field1, Field2, Alpha1, Alpha2, Rad1, Rad2, A),
        ( if int.'>='(M, 0) then
            Result = ext_pow(V, M)
        else
            Result = ext_inv(ext_pow(V, int.'-'(0, M)))
        )
    else
        Result = embed(Field2, embed(Field1, rational.zero))
    ).

%---------------------------------------------------------------------------%
% Convert ExtElem(ExtElem(Rational)) to RadExpr
%---------------------------------------------------------------------------%

:- func ext_elem_to_rad_expr2(
    ext_field(rational),
    ext_elem(ext_elem(rational)),
    ext_elem(ext_elem(rational)),
    {int, rad_expr(rational)},
    {int, rad_expr(rational)},
    ext_elem(ext_elem(rational))) = rad_expr(rational).

ext_elem_to_rad_expr2(_, _, _, {D1, R1}, {D2, R2}, Elem) = Result :-
    Rad1Expr = re_root(D1, R1),
    Rad2Expr = re_root(D2, R2),
    OuterCs = poly.coeffs(elem_poly(Elem)),
    AllTerms = collect_terms_2(OuterCs, 0),
    ( if AllTerms = [] then
        Result = re_lit(rational.zero)
    else
        Result = list.foldl(
            ( func({C, I, J}, Acc) =
                re_add(Acc, mk_term2(C, I, J, Rad1Expr, Rad2Expr)) ),
            list.det_tail(AllTerms),
            mk_term2_first(list.det_head(AllTerms), Rad1Expr, Rad2Expr))
    ).

:- func collect_terms_2(list(ext_elem(rational)), int)
    = list({rational, int, int}).

collect_terms_2([], _) = [].
collect_terms_2([OuterC | Rest], J) =
    list.append(
        collect_inner(poly.coeffs(elem_poly(OuterC)), 0, J),
        collect_terms_2(Rest, int.'+'(J, 1))).

:- func collect_inner(list(rational), int, int) = list({rational, int, int}).

collect_inner([], _, _) = [].
collect_inner([C | Cs], I, J) = Result :-
    ( if C = rational.zero then
        Result = collect_inner(Cs, int.'+'(I, 1), J)
    else
        Result = [{C, I, J} | collect_inner(Cs, int.'+'(I, 1), J)]
    ).

:- func mk_term2_first({rational, int, int},
    rad_expr(rational), rad_expr(rational)) = rad_expr(rational).

mk_term2_first({C, I, J}, A, B) = mk_term2(C, I, J, A, B).

:- func mk_term2(rational, int, int,
    rad_expr(rational), rad_expr(rational)) = rad_expr(rational).

mk_term2(C, I, J, A, B) = Result :-
    ( if I = 0, J = 0 then
        Result = re_lit(C)
    else if J = 0 then
        Result = mul_coeff(C, pow_expr(A, I))
    else if I = 0 then
        Result = mul_coeff(C, pow_expr(B, J))
    else
        Result = mul_coeff(C, re_mul(pow_expr(A, I), pow_expr(B, J)))
    ).

:- func mul_coeff(rational, rad_expr(rational)) = rad_expr(rational).

mul_coeff(C, E) = ( if C = rational.one then E
                     else re_mul(re_lit(C), E) ).

:- func pow_expr(rad_expr(rational), int) = rad_expr(rational).

pow_expr(E, K) = ( if K = 1 then E else re_pow(E, K) ).

%---------------------------------------------------------------------------%
% Depth 3: three radicals
%---------------------------------------------------------------------------%

:- func try_denest_depth3(int, rad_expr(rational),
    {int, rad_expr(rational)}, {int, rad_expr(rational)},
    {int, rad_expr(rational)}) = maybe(rad_expr(rational)).

try_denest_depth3(N, Radicand, Rad1, Rad2, Rad3) = Result :-
    Rad1 = {D1, R1},
    Rad2 = {D2, R2},
    Rad3 = {D3, R3},
    ( if eval_rat(R1) = yes(Q1) then
        % Level 1
        NegQ1 = rational.'-'(rational.zero, Q1),
        Zeros1 = list.duplicate(int.'-'(D1, 1), rational.zero),
        MP1 = mk_poly([NegQ1 | Zeros1] ++ [rational.one]),
        Field1 = mk_ext_field(MP1, "α₁"),
        Alpha1 = generator(Field1),
        % Level 2
        R2InF1 = eval_in_k(Field1, Alpha1, Rad1, R2),
        NegR2 = ext_neg(R2InF1),
        Zeros2 = list.duplicate(int.'-'(D2, 1),
            embed(Field1, rational.zero)),
        MP2 = mk_poly([NegR2 | Zeros2] ++
            [embed(Field1, rational.one)]),
        Field2 = mk_ext_field(MP2, "α₂"),
        Alpha2 = generator(Field2),
        Alpha1InF2 = embed(Field2, Alpha1),
        % Level 3
        R3InF2 = eval_in_k2(Field1, Field2, Alpha1InF2, Alpha2,
            Rad1, Rad2, R3),
        NegR3 = ext_neg(R3InF2),
        Zeros3 = list.duplicate(int.'-'(D3, 1),
            embed(Field2, embed(Field1, rational.zero))),
        MP3 = mk_poly([NegR3 | Zeros3] ++
            [embed(Field2, embed(Field1, rational.one))]),
        Field3 = mk_ext_field(MP3, "α₃"),
        Alpha3 = generator(Field3),
        Alpha1InF3 = embed(Field3, embed(Field2, Alpha1)),
        Alpha2InF3 = embed(Field3, Alpha2),
        % Evaluate radicand
        AInK = eval_in_k3(Field1, Field2, Field3,
            Alpha1InF3, Alpha2InF3, Alpha3,
            Rad1, Rad2, Rad3, Radicand),
        One3 = embed(Field3, embed(Field2, embed(Field1, rational.one))),
        NegAInK = ext_neg(AInK),
        ZerosN = list.duplicate(int.'-'(N, 1),
            embed(Field3, embed(Field2, embed(Field1, rational.zero)))),
        XnMinusA = mk_poly([NegAInK | ZerosN] ++ [One3]),
        FactorOverK1 = ( func(P1) =
            factor_sf_over_extension(Field1, P1) ),
        FactorOverK2 = ( func(P2) =
            factor_sf_over_extension_k(FactorOverK1, Field2, P2) ),
        Factors = factor_sf_over_extension_k(FactorOverK2, Field3,
            XnMinusA),
        LowDeg = list.filter(
            ( pred(Fac::in) is semidet :-
                int.'>'(poly.degree(Fac), 0),
                int.'<'(poly.degree(Fac), N)
            ),
            Factors),
        ( if LowDeg = [F | _] then
            ToExpr = ext_elem_to_rad_expr3(Field1,
                Alpha1InF3, Alpha2InF3, Alpha3,
                Rad1, Rad2, Rad3),
            Result = extract_root_generic(ToExpr, F)
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Evaluate in Q(α₁)(α₂)(α₃)
%---------------------------------------------------------------------------%

:- func eval_in_k3(
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

eval_in_k3(F1, F2, F3, A1, A2, A3, R1, R2, R3, Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = embed(F3, embed(F2, embed(F1, R)))
    else if Expr = re_neg(A) then
        Result = ext_neg(eval_in_k3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A))
    else if Expr = re_add(A, B) then
        Result = ext_add(
            eval_in_k3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A),
            eval_in_k3(F1, F2, F3, A1, A2, A3, R1, R2, R3, B))
    else if Expr = re_mul(A, B) then
        Result = ext_mul(
            eval_in_k3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A),
            eval_in_k3(F1, F2, F3, A1, A2, A3, R1, R2, R3, B))
    else if Expr = re_inv(A) then
        Result = ext_inv(
            eval_in_k3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A))
    else if Expr = re_root(M, A), {M, A} = R1 then
        Result = A1
    else if Expr = re_root(M, A), {M, A} = R2 then
        Result = A2
    else if Expr = re_root(M, A), {M, A} = R3 then
        Result = A3
    else if Expr = re_pow(A, M) then
        V = eval_in_k3(F1, F2, F3, A1, A2, A3, R1, R2, R3, A),
        ( if int.'>='(M, 0) then
            Result = ext_pow(V, M)
        else
            Result = ext_inv(ext_pow(V, int.'-'(0, M)))
        )
    else
        Result = embed(F3, embed(F2, embed(F1, rational.zero)))
    ).

%---------------------------------------------------------------------------%
% Convert depth-3 element to RadExpr
%---------------------------------------------------------------------------%

:- func ext_elem_to_rad_expr3(
    ext_field(rational),
    ext_elem(ext_elem(ext_elem(rational))),
    ext_elem(ext_elem(ext_elem(rational))),
    ext_elem(ext_elem(ext_elem(rational))),
    {int, rad_expr(rational)},
    {int, rad_expr(rational)},
    {int, rad_expr(rational)},
    ext_elem(ext_elem(ext_elem(rational)))) = rad_expr(rational).

ext_elem_to_rad_expr3(_, _, _, _, {D1, R1}, {D2, R2}, {D3, R3}, Elem) =
    Result :-
    Rad1 = re_root(D1, R1),
    Rad2 = re_root(D2, R2),
    Rad3 = re_root(D3, R3),
    OuterCs = poly.coeffs(elem_poly(Elem)),
    AllTerms = collect_terms_3(OuterCs, 0),
    ( if AllTerms = [] then
        Result = re_lit(rational.zero)
    else
        Result = list.foldl(
            ( func({C, I, J, K}, Acc) =
                re_add(Acc, mk_term3(C, I, J, K, Rad1, Rad2, Rad3)) ),
            list.det_tail(AllTerms),
            mk_term3(fst4(list.det_head(AllTerms)),
                snd4(list.det_head(AllTerms)),
                thd4(list.det_head(AllTerms)),
                fth4(list.det_head(AllTerms)),
                Rad1, Rad2, Rad3))
    ).

:- func fst4({A, B, C, D}) = A.
fst4({X, _, _, _}) = X.
:- func snd4({A, B, C, D}) = B.
snd4({_, X, _, _}) = X.
:- func thd4({A, B, C, D}) = C.
thd4({_, _, X, _}) = X.
:- func fth4({A, B, C, D}) = D.
fth4({_, _, _, X}) = X.

:- func collect_terms_3(list(ext_elem(ext_elem(rational))), int)
    = list({rational, int, int, int}).

collect_terms_3([], _) = [].
collect_terms_3([OuterC | Rest], K) =
    list.append(
        collect_mid_terms(poly.coeffs(elem_poly(OuterC)), 0, K),
        collect_terms_3(Rest, int.'+'(K, 1))).

:- func collect_mid_terms(list(ext_elem(rational)), int, int)
    = list({rational, int, int, int}).

collect_mid_terms([], _, _) = [].
collect_mid_terms([MidC | Rest], J, K) =
    list.append(
        collect_inner_3(poly.coeffs(elem_poly(MidC)), 0, J, K),
        collect_mid_terms(Rest, int.'+'(J, 1), K)).

:- func collect_inner_3(list(rational), int, int, int)
    = list({rational, int, int, int}).

collect_inner_3([], _, _, _) = [].
collect_inner_3([C | Cs], I, J, K) = Result :-
    ( if C = rational.zero then
        Result = collect_inner_3(Cs, int.'+'(I, 1), J, K)
    else
        Result = [{C, I, J, K} |
            collect_inner_3(Cs, int.'+'(I, 1), J, K)]
    ).

:- func mk_term3(rational, int, int, int,
    rad_expr(rational), rad_expr(rational), rad_expr(rational))
    = rad_expr(rational).

mk_term3(C, I, J, K, A, B, D) = Result :-
    Factors = list.filter(( pred(X::in) is semidet :- X \= re_lit(rational.one) ),
        [( if I = 0 then re_lit(rational.one) else pow_expr(A, I) ),
         ( if J = 0 then re_lit(rational.one) else pow_expr(B, J) ),
         ( if K = 0 then re_lit(rational.one) else pow_expr(D, K) )]),
    ( if Factors = [] then
        Result = re_lit(C)
    else if Factors = [F1] then
        Result = mul_coeff(C, F1)
    else
        Base = list.foldl(
            ( func(X, Acc) = re_mul(Acc, X) ),
            list.det_tail(Factors), list.det_head(Factors)),
        Result = mul_coeff(C, Base)
    ).

%---------------------------------------------------------------------------%
% Depth 4: four radicals (stub — same pattern as depth 3)
%---------------------------------------------------------------------------%

:- func try_denest_depth4(int, rad_expr(rational),
    {int, rad_expr(rational)}, {int, rad_expr(rational)},
    {int, rad_expr(rational)}, {int, rad_expr(rational)})
    = maybe(rad_expr(rational)).

try_denest_depth4(_, _, _, _, _, _) = no.
    % Depth 4 denesting requires 4-level tower nesting which is
    % extremely expensive. Deferred — depth 1-3 covers most practical cases.

%---------------------------------------------------------------------------%
% Multi-determinism: enumerate Landau denesting alternatives
%---------------------------------------------------------------------------%

denest_landau_multi(Expr, Result) :-
    % Try Landau denesting at each root node
    Expr = re_root(N, Radicand),
    denest_radical(N, Radicand) = yes(Result).
denest_landau_multi(Expr, Result) :-
    % Recurse into sub-expressions
    Expr = re_root(N, Inner),
    denest_landau_multi(Inner, DenestedInner),
    Result = re_root(N, DenestedInner).
denest_landau_multi(Expr, Result) :-
    Expr = re_add(A, B),
    denest_landau_multi(A, DA),
    Result = re_add(DA, B).
denest_landau_multi(Expr, Result) :-
    Expr = re_add(A, B),
    denest_landau_multi(B, DB),
    Result = re_add(A, DB).
denest_landau_multi(Expr, Result) :-
    Expr = re_mul(A, B),
    denest_landau_multi(A, DA),
    Result = re_mul(DA, B).
denest_landau_multi(Expr, Result) :-
    Expr = re_mul(A, B),
    denest_landau_multi(B, DB),
    Result = re_mul(A, DB).

%---------------------------------------------------------------------------%
:- end_module denest_landau.
%---------------------------------------------------------------------------%
