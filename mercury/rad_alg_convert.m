%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: rad_alg_convert.m
% Main author: nyc
%
% Conversion between radical expressions and algebraic numbers.
%
% Forward direction (RadExpr → AlgNum): compute the minimal polynomial
% and isolate the correct root.
%
% Backward direction (AlgNum → RadExpr): given a low-degree minimal
% polynomial, express as a radical (degree ≤ 4 via quadratic/Cardano/
% Ferrari formulas).
%
% The round-trip RadExpr → AlgNum → RadExpr is the simplification
% pipeline for complex nested radical expressions.
%
%---------------------------------------------------------------------------%

:- module rad_alg_convert.
:- interface.

:- import_module alg_num.
:- import_module maybe.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Convert a radical expression to its canonical algebraic number form.
    %
:- func rad_expr_to_alg_num(rad_expr(rational)) = alg_num.

    % Convert an algebraic number back to a radical expression,
    % if the degree is small enough for closed-form solutions.
    %
    % Handles degree 1 (rational), degree 2 (quadratic formula),
    % degree 3 (Cardano), degree 4 (Ferrari).
    %
:- func alg_num_to_rad_expr(alg_num) = maybe(rad_expr(rational)).

    % Simplify a radical expression by converting to canonical form
    % and back.
    %
:- func simplify_via_canonical(rad_expr(rational)) = rad_expr(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module factoring.
:- import_module float.
:- import_module integer.
:- use_module int.
:- import_module interval.
:- import_module list.
:- import_module math.
:- import_module minimal_poly.
:- import_module poly.
:- import_module rad_eval.
:- import_module rad_normalize.
:- import_module require.
:- use_module string.

%---------------------------------------------------------------------------%
% RadExpr → AlgNum
%---------------------------------------------------------------------------%

rad_expr_to_alg_num(Expr) = Result :-
    MP = minimal_poly(Expr),
    Approx = eval_float(Expr),
    Result = alg_from_poly(MP, Approx).

%---------------------------------------------------------------------------%
% AlgNum → RadExpr
%---------------------------------------------------------------------------%

alg_num_to_rad_expr(A) = Result :-
    P = alg_min_poly(A),
    D = degree(P),
    Cs = coeffs(P),
    Approx = rational_to_float(alg_approx(
        rational.'/'(rational.one,
            rational.from_integer(
                integer.pow(integer.integer(10), integer.integer(15)))),
        A)),
    ( if D = 1 then
        ( if Cs = [C0, _] then
            Result = yes(re_lit(rational.'-'(rational.zero, C0)))
        else
            Result = no
        )
    else if D = 2 then
        Result = solve_quadratic(Cs, Approx)
    else if D = 3 then
        Result = solve_cubic(Cs, Approx)
    else if D = 4 then
        Result = solve_quartic(Cs, Approx)
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Simplify via canonical
%---------------------------------------------------------------------------%

simplify_via_canonical(Expr) = Result :-
    AlgN = rad_expr_to_alg_num(Expr),
    MaybeRad = alg_num_to_rad_expr(AlgN),
    ( if MaybeRad = yes(E) then
        Result = normalize(E)
    else
        Result = Expr
    ).

%---------------------------------------------------------------------------%
% Quadratic solver: x² + bx + c = 0  (monic)
%---------------------------------------------------------------------------%

:- func solve_quadratic(list(rational), float) = maybe(rad_expr(rational)).

solve_quadratic(Cs, Approx) = Result :-
    ( if Cs = [C, B, _A] then
        % disc = b² - 4c
        Disc = rational.'-'(rational.'*'(B, B),
                            rational.'*'(rational(4, 1), C)),
        ( if rational.'<'(Disc, rational.zero) then
            Result = no
        else
            SqrtD = re_root(2, re_lit(Disc)),
            NegB = re_neg(re_lit(B)),
            Two = re_lit(rational(2, 1)),
            R1 = re_mul(re_inv(Two), re_add(NegB, SqrtD)),
            R2 = re_mul(re_inv(Two), re_add(NegB, re_neg(SqrtD))),
            V1 = eval_float(R1),
            V2 = eval_float(R2),
            ( if float.'=<'(float.abs(float.'-'(V1, Approx)),
                            float.abs(float.'-'(V2, Approx))) then
                Result = yes(R1)
            else
                Result = yes(R2)
            )
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Cubic solver: x³ + bx² + cx + d = 0  (monic, Cardano)
%---------------------------------------------------------------------------%

:- func solve_cubic(list(rational), float) = maybe(rad_expr(rational)).

solve_cubic(Cs, Approx) = Result :-
    ( if Cs = [D, C, B, _A] then
        % Depress: x = t - b/3
        Three = rational(3, 1),
        TwentySeven = rational(27, 1),
        P = rational.'-'(C,
            rational.'/'(rational.'*'(B, B), Three)),
        Q = rational.'-'(
            rational.'+'(D,
                rational.'/'(
                    rational.'*'(rational(2, 1),
                        rational.'*'(B, rational.'*'(B, B))),
                    TwentySeven)),
            rational.'/'(rational.'*'(B, C), Three)),
        Shift = re_neg(re_lit(rational.'/'(B, Three))),
        % Discriminant: -(4p³ + 27q²)
        Disc = rational.'-'(rational.zero,
            rational.'+'(
                rational.'*'(rational(4, 1),
                    rational.'*'(P, rational.'*'(P, P))),
                rational.'*'(TwentySeven,
                    rational.'*'(Q, Q)))),
        ( if rational.'<'(Disc, rational.zero) then
            % One real root, two complex conjugates
            Result = solve_cubic_one_real(P, Q, Shift, Approx)
        else
            % Three real roots or repeated
            % Try rational roots first
            Poly = mk_poly([D, C, B, rational.one]),
            Rats = rational_roots(Poly),
            ( if Rats = [R | _] then
                Result = yes(re_lit(R))
            else
                % Casus irreducibilis: Cardano with complex intermediates
                Result = solve_cubic_casus(P, Q, Shift, Approx)
            )
        )
    else
        Result = no
    ).

:- func solve_cubic_one_real(rational, rational, rad_expr(rational), float)
    = maybe(rad_expr(rational)).

solve_cubic_one_real(P, Q, Shift, Approx) = Result :-
    HalfQ = rational.'/'(Q, rational(2, 1)),
    InnerDisc = rational.'+'(
        rational.'*'(HalfQ, HalfQ),
        rational.'/'(rational.'*'(P, rational.'*'(P, P)),
                     rational(27, 1))),
    SqrtD = re_root(2, re_lit(InnerDisc)),
    NegHalfQ = re_neg(re_lit(HalfQ)),
    U = re_root(3, re_add(NegHalfQ, SqrtD)),
    V = re_root(3, re_add(NegHalfQ, re_neg(SqrtD))),
    Root0 = re_add(re_add(U, V), Shift),
    _ = Approx,
    Result = yes(Root0).

:- func solve_cubic_casus(rational, rational, rad_expr(rational), float)
    = maybe(rad_expr(rational)).

solve_cubic_casus(P, Q, Shift, Approx) = Result :-
    HalfQ = rational.'/'(Q, rational(2, 1)),
    InnerDisc = rational.'+'(
        rational.'*'(HalfQ, HalfQ),
        rational.'/'(rational.'*'(P, rational.'*'(P, P)),
                     rational(27, 1))),
    SqrtD = re_root(2, re_lit(InnerDisc)),
    NegHalfQ = re_neg(re_lit(HalfQ)),
    CubeRadPos = re_add(NegHalfQ, SqrtD),
    CubeRadNeg = re_add(NegHalfQ, re_neg(SqrtD)),
    U1 = re_root(3, CubeRadPos),
    U2 = re_root(3, CubeRadNeg),
    % ω = (-1 + √(-3))/2, ω² = (-1 - √(-3))/2
    SqrtNeg3 = re_root(2, re_lit(rational(-3, 1))),
    Omega = re_mul(re_inv(re_lit(rational(2, 1))),
        re_add(re_lit(rational(-1, 1)), SqrtNeg3)),
    Omega2 = re_mul(re_inv(re_lit(rational(2, 1))),
        re_add(re_lit(rational(-1, 1)), re_neg(SqrtNeg3))),
    Root0 = re_add(re_add(U1, U2), Shift),
    Root1 = re_add(re_add(re_mul(Omega, U1), re_mul(Omega2, U2)), Shift),
    Root2 = re_add(re_add(re_mul(Omega2, U1), re_mul(Omega, U2)), Shift),
    % Use complex evaluation to pick the best real root
    Roots = [Root0, Root1, Root2],
    Scored = list.map(
        ( func(R) = {R, Score} :-
            CV = eval_complex(R),
            RealPart = complex_real(CV),
            Score = float.abs(float.'-'(RealPart, Approx))
        ),
        Roots),
    Best = find_best(Scored),
    Result = yes(Best).

:- func complex_real(complex) = float.
complex_real(complex(R, _)) = R.

:- func find_best(list({rad_expr(rational), float})) = rad_expr(rational).

find_best(Xs) = Result :-
    ( if Xs = [{R, _}] then
        Result = R
    else if Xs = [{R1, S1}, X2 | Rest] then
        BestRest = find_best([X2 | Rest]),
        BestRestScore = find_best_score([X2 | Rest]),
        ( if float.'=<'(S1, BestRestScore) then
            Result = R1
        else
            Result = BestRest
        )
    else
        require.error("find_best: empty list")
    ).

:- func find_best_score(list({rad_expr(rational), float})) = float.

find_best_score(Xs) = Result :-
    ( if Xs = [{_, S}] then
        Result = S
    else if Xs = [{_, S1}, X2 | Rest] then
        S2 = find_best_score([X2 | Rest]),
        Result = float.min(S1, S2)
    else
        require.error("find_best_score: empty")
    ).

%---------------------------------------------------------------------------%
% Quartic solver: x⁴ + bx³ + cx² + dx + e = 0  (monic, Ferrari)
%---------------------------------------------------------------------------%

:- func solve_quartic(list(rational), float) = maybe(rad_expr(rational)).

solve_quartic(Cs, Approx) = Result :-
    ( if Cs = [E, D, C, B, _A] then
        % Depress: x = t - b/4
        Four = rational(4, 1),
        Eight = rational(8, 1),
        Sixteen = rational(16, 1),
        TFS = rational(256, 1),
        P = rational.'-'(C,
            rational.'/'(rational.'*'(rational(3, 1),
                rational.'*'(B, B)), Eight)),
        Q = rational.'-'(
            rational.'+'(D,
                rational.'/'(rational.'*'(B, rational.'*'(B, B)),
                             Eight)),
            rational.'/'(rational.'*'(B, C),
                         rational(2, 1))),
        R = rational.'-'(
            rational.'+'(E,
                rational.'/'(rational.'*'(B, rational.'*'(B, C)),
                             Sixteen)),
            rational.'+'(
                rational.'/'(rational.'*'(B, D), Four),
                rational.'/'(rational.'*'(rational(3, 1),
                    rational.'*'(B, rational.'*'(B,
                        rational.'*'(B, B)))), TFS))),
        ShiftVal = rational.'/'(B, Four),
        ( if Q = rational.zero then
            % Biquadratic: t⁴ + pt² + r = 0
            Result = solve_biquadratic(P, R, ShiftVal, Approx)
        else
            % General case: resolvent cubic
            Result = solve_quartic_general(P, Q, R, ShiftVal, Approx)
        )
    else
        Result = no
    ).

:- func solve_biquadratic(rational, rational, rational, float)
    = maybe(rad_expr(rational)).

solve_biquadratic(P, R, ShiftVal, Approx) = Result :-
    % t⁴ + pt² + r = 0 → solve as quadratic in t²
    Disc = rational.'-'(rational.'*'(P, P),
                        rational.'*'(rational(4, 1), R)),
    ( if rational.'<'(Disc, rational.zero) then
        Result = no
    else
        SqrtD = re_root(2, re_lit(Disc)),
        NegP = re_neg(re_lit(P)),
        InvTwo = re_inv(re_lit(rational(2, 1))),
        T2_1 = re_mul(InvTwo, re_add(NegP, SqrtD)),
        T2_2 = re_mul(InvTwo, re_add(NegP, re_neg(SqrtD))),
        Shift = re_neg(re_lit(ShiftVal)),
        Roots = [
            re_add(re_root(2, T2_1), Shift),
            re_add(re_neg(re_root(2, T2_1)), Shift),
            re_add(re_root(2, T2_2), Shift),
            re_add(re_neg(re_root(2, T2_2)), Shift)
        ],
        Scored = list.map(
            ( func(Root) = {Root, Score} :-
                CV = eval_complex(Root),
                RealPart = complex_real(CV),
                Score = float.abs(float.'-'(RealPart, Approx))
            ),
            Roots),
        Best = find_best(Scored),
        Result = yes(Best)
    ).

:- func solve_quartic_general(rational, rational, rational, rational, float)
    = maybe(rad_expr(rational)).

solve_quartic_general(P, Q, R, ShiftVal, Approx) = Result :-
    % Resolvent cubic: y³ - py² - 4ry + (4pr - q²) = 0
    RCCs = [rational.'-'(rational.'*'(rational(4, 1),
                                       rational.'*'(P, R)),
                          rational.'*'(Q, Q)),
            rational.'*'(rational(-4, 1), R),
            rational.'-'(rational.zero, P),
            rational.one],
    YResult = solve_cubic(RCCs, 0.0),
    ( if YResult = yes(YExpr) then
        % s² = y - p, s = √(y - p)
        SSquared = re_add(YExpr, re_neg(re_lit(P))),
        S = re_root(2, SSquared),
        HalfY = re_mul(re_inv(re_lit(rational(2, 1))), YExpr),
        QOver2S = re_mul(re_lit(rational.'/'(Q, rational(2, 1))), re_inv(S)),
        C1 = re_add(HalfY, re_neg(QOver2S)),
        C2 = re_add(HalfY, QOver2S),
        Disc1 = re_add(SSquared, re_mul(re_lit(rational(-4, 1)), C1)),
        Disc2 = re_add(SSquared, re_mul(re_lit(rational(-4, 1)), C2)),
        SqD1 = re_root(2, Disc1),
        SqD2 = re_root(2, Disc2),
        InvTwo = re_inv(re_lit(rational(2, 1))),
        Shift = re_neg(re_lit(ShiftVal)),
        T1 = re_add(re_mul(InvTwo, re_add(re_neg(S), SqD1)), Shift),
        T2 = re_add(re_mul(InvTwo, re_add(re_neg(S), re_neg(SqD1))), Shift),
        T3 = re_add(re_mul(InvTwo, re_add(S, SqD2)), Shift),
        T4 = re_add(re_mul(InvTwo, re_add(S, re_neg(SqD2))), Shift),
        Roots = [T1, T2, T3, T4],
        Scored = list.map(
            ( func(Root) = {Root, Score} :-
                CV = eval_complex(Root),
                RealPart = complex_real(CV),
                Score = float.abs(float.'-'(RealPart, Approx))
            ),
            Roots),
        Best = find_best(Scored),
        Result = yes(Best)
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Utility
%---------------------------------------------------------------------------%

:- func rational_to_float(rational) = float.

rational_to_float(R) =
    float.'/'(
        integer.float(numer(R)),
        integer.float(denom(R))).

%---------------------------------------------------------------------------%
:- end_module rad_alg_convert.
%---------------------------------------------------------------------------%
