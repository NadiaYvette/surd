%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: trig.m
% Main author: nyc
%
% Exact symbolic evaluation of trigonometric functions at
% rational multiples of π.
%
% Every trig value at a rational multiple of π can be expressed in
% radicals, since cyclotomic extensions have abelian (hence solvable)
% Galois groups.
%
% The primary entry points are cos_exact and sin_exact, which
% compute cos(pπ/q) and sin(pπ/q) as radical expressions.
%
%---------------------------------------------------------------------------%

:- module trig.
:- interface.

:- import_module integer.
:- import_module maybe.
:- import_module poly.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

:- type trig_result
    --->    radical(rad_expr(rational))
    ;       min_poly(poly(rational)).

    % Compute cos(pπ/q) exactly.
    %
:- func cos_exact(integer, integer) = trig_result.

    % Compute sin(pπ/q) exactly.
    %
:- func sin_exact(integer, integer) = trig_result.

    % Compute tan(pπ/q) exactly, as sin/cos.
    %
:- func tan_exact(integer, integer) = maybe(trig_result).

    % Return the minimal polynomial of cos(2π/n) (actually Φₙ).
    %
:- func cos_min_poly(int) = poly(rational).

    % Simplify a trig result for display.
    %
:- func simplify_trig_result(trig_result) = trig_result.

    % Compute simplified sin from simplified cos for display.
    %
:- func simplified_sin(integer, integer, trig_result) = trig_result.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module cyclotomic.
:- import_module denest.
:- import_module interval.
:- import_module list.
:- import_module map.
:- import_module normal_form.
:- import_module require.
:- import_module rad_alg_convert.
:- import_module rad_dag.
:- import_module rad_normalize.
:- import_module root_of_unity.
:- import_module trig_galois.

%---------------------------------------------------------------------------%
% Entry points
%---------------------------------------------------------------------------%

cos_exact(P, Q) = Result :-
    ( if integer.'=<'(Q, integer.zero) then
        unexpected($module, $pred, "non-positive denominator")
    else
        G = integer_gcd(integer.abs(P), Q),
        P1 = integer.'//'(P, G),
        Q1 = integer.'//'(Q, G),
        Result = cos_reduced(P1, Q1)
    ).

sin_exact(P, Q) = Result :-
    ( if integer.'=<'(Q, integer.zero) then
        unexpected($module, $pred, "non-positive denominator")
    else
        G = integer_gcd(integer.abs(P), Q),
        P1 = integer.'//'(P, G),
        Q1 = integer.'//'(Q, G),
        Result = sin_reduced(P1, Q1)
    ).

tan_exact(P, Q) = Result :-
    SinR = sin_exact(P, Q),
    CosR = cos_exact(P, Q),
    ( if SinR = radical(S), CosR = radical(C) then
        Result = yes(radical(re_mul(S, re_inv(C))))
    else
        Result = no
    ).

cos_min_poly(N) = cyclotomic(N).

%---------------------------------------------------------------------------%
% cos reduced
%---------------------------------------------------------------------------%

:- func cos_reduced(integer, integer) = trig_result.

cos_reduced(P, Q) = Result :-
    TwoQ = integer.'*'(integer(2), Q),
    P1 = integer.'mod'(P, TwoQ),
    P2 = ( if integer.'<'(P1, integer.zero) then
               integer.'+'(P1, TwoQ) else P1 ),
    Result = cos_in_range(P2, Q).

:- func sin_reduced(integer, integer) = trig_result.

sin_reduced(P, Q) = Result :-
    TwoQ = integer.'*'(integer(2), Q),
    P1 = integer.'mod'(P, TwoQ),
    P2 = ( if integer.'<'(P1, integer.zero) then
               integer.'+'(P1, TwoQ) else P1 ),
    Positive = integer.'>='(P2, integer.zero),
    PLeQ = integer.'=<'(P2, Q),
    ( if P2 = integer.zero then
        Result = radical(re_lit(rational.zero))
    else if P2 = Q then
        Result = radical(re_lit(rational.zero))
    else
        N0 = integer.det_to_int(integer.'//'(TwoQ,
            integer_gcd(P2, TwoQ))),
        K0 = integer.det_to_int(integer.'//'(P2,
            integer_gcd(P2, TwoQ))),
        ( if direct_period_sin(N0, K0) = yes(SinExpr) then
            Result = radical(dag_fold_expr(SinExpr))
        else
            ( if cos_exact(P, Q) = radical(C) then
                Sin2NF = from_norm_expr(to_norm_expr(
                    re_add(re_lit(rational.one),
                           re_neg(re_mul(C, C))))),
                Sin2Folded = dag_fold_expr(Sin2NF),
                SinExpr = re_root(2, Sin2Folded),
                Signed = ( if Positive, PLeQ then SinExpr
                           else re_neg(SinExpr) ),
                Result = radical(safe_denest_and_normalize(Signed))
            else
                Result = cos_exact(P, Q)
            )
        )
    ).

%---------------------------------------------------------------------------%
% Direct period sin
%---------------------------------------------------------------------------%

:- func direct_period_sin(int, int) = maybe(rad_expr(rational)).

direct_period_sin(N, K) = Result :-
    ( if all_periods_via_gauss(N) = yes(Periods),
         map.search(Periods, K, Pk),
         map.search(Periods, int.'-'(N, K), Pnk),
         NRadsPk = list.length(collect_radicals(Pk)),
         NRadsPnk = list.length(collect_radicals(Pnk)),
         int.'=<'(NRadsPk, 10),
         int.'=<'(NRadsPnk, 10) then
        Pk1 = from_norm_expr(to_norm_expr(Pk)),
        Pnk1 = from_norm_expr(to_norm_expr(Pnk)),
        I = re_root(2, re_lit(rational.'-'(rational.zero, rational.one))),
        SinForm = re_mul(re_mul(re_inv(re_lit(rational(2))),
                                re_neg(I)),
                         re_add(Pk1, re_neg(Pnk1))),
        SinNF = from_norm_expr(to_norm_expr(SinForm)),
        Result = yes(SinNF)
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% cos_in_range: cos(pπ/q) where 0 ≤ p ≤ 2q
%---------------------------------------------------------------------------%

:- func cos_in_range(integer, integer) = trig_result.

cos_in_range(P, Q) = Result :-
    TwoP = integer.'*'(integer(2), P),
    ThreeQ = integer.'*'(integer(3), Q),
    ( if P = integer.zero then
        Result = radical(re_lit(rational.one))
    else if TwoP = Q then
        Result = radical(re_lit(rational.zero))
    else if P = Q then
        Result = radical(re_lit(rational.'-'(rational.zero, rational.one)))
    else if integer.'*'(integer(2), P) = integer.'*'(integer(3), Q) then
        Result = radical(re_lit(rational.zero))
    else if integer.'>'(TwoP, Q), integer.'<'(P, Q) then
        ( if cos_in_range(integer.'-'(Q, P), Q) = radical(E) then
            Result = radical(re_neg(E))
        else
            Result = cos_in_range(integer.'-'(Q, P), Q)
        )
    else if integer.'>'(P, Q),
            integer.'<'(TwoP, ThreeQ) then
        ( if cos_in_range(integer.'-'(P, Q), Q) = radical(E) then
            Result = radical(re_neg(E))
        else
            Result = cos_in_range(integer.'-'(P, Q), Q)
        )
    else if integer.'>='(TwoP, ThreeQ) then
        TwoQ = integer.'*'(integer(2), Q),
        Result = cos_in_range(integer.'-'(TwoQ, P), Q)
    else
        Result = cos_first_quadrant(P, Q)
    ).

:- func cos_first_quadrant(integer, integer) = trig_result.

cos_first_quadrant(P, Q) = Result :-
    TwoQ = integer.'*'(integer(2), Q),
    G = integer_gcd(P, TwoQ),
    N = integer.det_to_int(integer.'//'(TwoQ, G)),
    K = integer.det_to_int(integer.'//'(P, G)),
    ( if K = 1 then
        ( if cos_of_unity(N) = yes(E) then
            Result = radical(safe_denest_and_normalize(E))
        else
            Result = min_poly(cyclotomic(N))
        )
    else
        ( if cos_of_unity(N) = yes(Base) then
            Cheb = chebyshev(K, Base),
            NRads = list.length(collect_radicals(Cheb)),
            ( if int.'>'(NRads, 3) then
                Result = radical(safe_denest_and_normalize(Cheb))
            else
                Simplified = from_norm_expr(to_norm_expr(Cheb)),
                Result = radical(safe_denest_and_normalize(Simplified))
            )
        else
            Result = min_poly(cyclotomic(N))
        )
    ).

%---------------------------------------------------------------------------%
% safe_denest_and_normalize
%---------------------------------------------------------------------------%

:- func safe_denest_and_normalize(rad_expr(rational)) = rad_expr(rational).

safe_denest_and_normalize(E) = Result :-
    Dag = to_dag(E),
    D = dag_depth(Dag),
    S = dag_size(Dag),
    ( if int.'>'(D, 50) ; int.'>'(S, 5000) then
        Result = from_dag(dag_fold_constants(Dag))
    else if int.'=<'(D, 20), int.'=<'(S, 500) then
        Folded = from_dag(dag_fold_constants(Dag)),
        Denested = denest.denest(Folded),
        SDen = dag_size(to_dag(Denested)),
        Best = ( if int.'<'(SDen, S) then Denested else Folded ),
        Result = safe_normalize(Best)
    else
        Result = from_dag(dag_fold_constants(Dag))
    ).

:- func dag_fold_expr(rad_expr(rational)) = rad_expr(rational).

dag_fold_expr(E) = from_dag(dag_fold_constants(to_dag(E))).

%---------------------------------------------------------------------------%
% safe_normalize with interval verification
%---------------------------------------------------------------------------%

:- func safe_normalize(rad_expr(rational)) = rad_expr(rational).

safe_normalize(E) = Result :-
    Dag = to_dag(E),
    D = dag_depth(Dag),
    S = dag_size(Dag),
    ( if int.'>'(D, 30) ; int.'>'(S, 500) then
        Result = E
    else
        Normed = normalize(E),
        OrigCI = dag_eval_complex_interval(to_dag(E)),
        NormCI = dag_eval_complex_interval(to_dag(Normed)),
        OrigRe = ci_real(OrigCI),
        NormRe = ci_real(NormCI),
        ( if iv_overlaps(OrigRe, NormRe) then
            Result = Normed
        else
            Result = E
        )
    ).

%---------------------------------------------------------------------------%
% try_canonical_simplify
%---------------------------------------------------------------------------%

:- func try_canonical_simplify(rad_expr(rational)) = rad_expr(rational).

try_canonical_simplify(E) = Result :-
    S = dag_size(to_dag(E)),
    NRads = list.length(collect_radicals(E)),
    ( if int.'=<'(S, 30) ; int.'>'(S, 200) ; int.'>'(NRads, 5) then
        Result = E
    else
        Simplified = simplify_via_canonical(E),
        S1 = dag_size(to_dag(Simplified)),
        Result = ( if int.'<'(S1, S) then Simplified else E )
    ).

%---------------------------------------------------------------------------%
% simplify_trig_result
%---------------------------------------------------------------------------%

simplify_trig_result(radical(E)) = Result :-
    Simplified = try_canonical_simplify(E),
    NRads = list.length(collect_radicals(Simplified)),
    ( if int.'>'(NRads, 5) then
        Result = radical(Simplified)
    else
        Result = radical(from_norm_expr(to_norm_expr(Simplified)))
    ).
simplify_trig_result(min_poly(P)) = min_poly(P).

simplified_sin(P, Q, radical(C)) = Result :-
    TwoQ = integer.'*'(integer(2), Q),
    P1 = integer.'mod'(P, TwoQ),
    P2 = ( if integer.'<'(P1, integer.zero) then
               integer.'+'(P1, TwoQ) else P1 ),
    Positive = integer.'>='(P2, integer.zero),
    PLeQ = integer.'=<'(P2, Q),
    ( if P2 = integer.zero then
        Result = radical(re_lit(rational.zero))
    else if P2 = Q then
        Result = radical(re_lit(rational.zero))
    else
        Rads = collect_radicals(C),
        HasComplex = list.any_true(
            ( pred({_, R}::in) is semidet :-
                R = re_lit(rational.'-'(rational.zero, rational.one))
            ), Rads),
        ( if HasComplex then
            OneMinusCos2 = re_add(re_lit(rational.one),
                                   re_neg(re_mul(C, C))),
            Folded = dag_fold_expr(OneMinusCos2),
            SinExpr = re_root(2, Folded),
            Signed = ( if Positive, PLeQ then SinExpr
                       else re_neg(SinExpr) ),
            Result = radical(dag_fold_expr(Signed))
        else
            Sin2 = from_norm_expr(to_norm_expr(
                re_add(re_lit(rational.one),
                       re_neg(re_mul(C, C))))),
            Sin2Folded = dag_fold_expr(Sin2),
            SinExpr = re_root(2, Sin2Folded),
            Signed = ( if Positive, PLeQ then SinExpr
                       else re_neg(SinExpr) ),
            Canonical = from_norm_expr(to_norm_expr(Signed)),
            S0 = dag_size(to_dag(Canonical)),
            ( if int.'=<'(S0, 30) then
                Result = radical(safe_normalize(Canonical))
            else
                Result = radical(try_canonical_simplify(Canonical))
            )
        )
    ).
simplified_sin(_, _, min_poly(P)) = min_poly(P).

%---------------------------------------------------------------------------%
% Helpers
%---------------------------------------------------------------------------%

:- func integer_gcd(integer, integer) = integer.

integer_gcd(A, B) = Result :-
    AA = integer.abs(A),
    BB = integer.abs(B),
    ( if integer.is_zero(BB) then
        Result = AA
    else
        Result = integer_gcd(BB, integer.'mod'(AA, BB))
    ).

%---------------------------------------------------------------------------%
:- end_module trig.
%---------------------------------------------------------------------------%
