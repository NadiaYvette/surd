%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: radical_tower.m
% Main author: nyc
%
% Radical tower construction for solvable polynomials via Lagrange
% resolvent descent.
%
% Given an irreducible polynomial f(x) ∈ Q[x] of degree n with solvable
% Galois group G, construct radical expressions for its roots.
%
% Currently supports degree 5 with Galois groups C5, D5, or F20.
%
% Algorithm pipeline:
%   [1] Depress: shift to eliminate x^4 term
%   [2] findCyclicOrdering: permute numerical roots so σ acts as (0 1 2 3 4)
%   [3] Compute R_j^5 numerically via Lagrange resolvents
%   [4] DFT: compute d_s from R_j^5
%   [5] matchDs: recognise d_s in the coefficient field
%   [6] Reconstruct R_j^5 as RadExpr, take 5th root, select branch
%   [7] Inverse DFT: α_k = (1/5) Σ ω^{-jk} R_j
%   [8] Un-depress and match to original root ordering
%
%---------------------------------------------------------------------------%

:- module radical_tower.
:- interface.

:- import_module galois_identify.
:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module rad_eval.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Solve a solvable polynomial via Lagrange resolvent descent.
    % Returns radical expressions for all n roots, matched to the
    % numerical roots from galois_result.
    %
    % Returns no if the group is not solvable or degree != 5.
    %
:- func solve_via_tower(galois_result, poly(rational)) =
    maybe(list(rad_expr(rational))).

    % Find a cyclic ordering of n numerical roots such that the
    % Galois generator acts as the cyclic shift (0 1 2 ... n-1).
    %
:- func find_cyclic_ordering(list(complex), int, string) =
    maybe(list(int)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- use_module float.
:- use_module int.
:- use_module math.
:- import_module integer.
:- import_module map.
:- import_module rad_dag.
:- import_module require.
:- import_module resolvent.
:- import_module string.
:- import_module transitive_group.

%---------------------------------------------------------------------------%
% Entry point
%---------------------------------------------------------------------------%

solve_via_tower(GR, F) = Result :-
    TG = gr_group(GR),
    ( if
        tg_solvable(TG) = bool.yes,
        poly.degree(F) = 5
    then
        Result = solve_solvable_quintic(TG, F, gr_roots(GR))
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Solvable quintic solver
%---------------------------------------------------------------------------%

:- func solve_solvable_quintic(transitive_group, poly(rational),
    list(complex)) = maybe(list(rad_expr(rational))).

solve_solvable_quintic(TG, F, NumRoots) = Result :-
    % Make monic and depress
    Cs = coeffs(F),
    ( if list.last(Cs, LC0), LC0 \= rational.zero then
        LC = LC0
    else
        unexpected($module, $pred, "zero leading coefficient")
    ),
    MonicCs = list.map(func(C) = rational.'/'(C, LC), Cs),
    A4 = list.det_index0(MonicCs, 4),
    ShiftVal = rational.'-'(rational.zero, rational.'/'(A4, rational(5))),
    ShiftC = complex(rational_to_float(ShiftVal), 0.0),
    DepRoots = list.map(func(R) = csub(R, ShiftC), NumRoots),

    GroupName = tg_name(TG),

    % Find cyclic ordering
    ( if find_cyclic_ordering(DepRoots, 5, GroupName) = yes(Ordering) then
        OrderedRoots = list.map(func(I) = list.det_index0(DepRoots, I),
                                Ordering),

        % Compute R_j and R_j^5 numerically
        RjVals = list.map(func(J) = compute_rj(OrderedRoots, J), 0 `..` 4),
        RjPows = list.map(func(R) = complex_pow_n(R, 5), RjVals),

        % DFT: d_s = (1/5) Σ_j ω5^{-js} R_j^5
        DVals = list.map(func(S) = compute_ds(RjPows, S), 0 `..` 4),

        % Match d_s to the coefficient field
        ( if match_ds(GroupName, DVals) = yes(DExprs) then
            % R_j^5 = Σ_s d_s · ω5^{js}
            RjPowExprs = list.map(
                func(J) = build_rj_pow_expr(DExprs, J),
                1 `..` 4),

            % Select correct branch: R_j = ω5^{b_j} · ⁵√(R_j^5)
            RjExprs = list.map_corresponding(
                select_branch5,
                RjPowExprs,
                list.det_tail(RjVals)),

            % R_0 = sum of depressed roots = 0
            AllR = [re_lit(rational.zero) | RjExprs],

            % Roots: α_k = (1/5) Σ_{j=0}^4 ω5^{-jk} R_j
            RootExprs = list.map(
                func(K) = dag_fold_expr(
                    re_mul(re_inv(re_lit(rational(5))),
                           build_inv_dft(AllR, K))),
                0 `..` 4),

            % Un-depress
            FinalExprs = list.map(
                func(E) = dag_fold_expr(
                    re_add(E, re_lit(ShiftVal))),
                RootExprs),

            % Match to original root ordering
            Result = yes(match_to_original(FinalExprs, NumRoots))
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Lagrange resolvent computation
%---------------------------------------------------------------------------%

:- func compute_rj(list(complex), int) = complex.

compute_rj(Roots, J) = Result :-
    Terms = list.map(
        func(K) = cmul(omega5_c(int.'*'(J, K)),
                        list.det_index0(Roots, K)),
        0 `..` 4),
    Result = list.foldl(cadd, Terms, complex(0.0, 0.0)).

:- func compute_ds(list(complex), int) = complex.

compute_ds(RjPows, S) = Result :-
    Terms = list.map(
        func(J) = cmul(omega5_c(
                        int.'-'(5, int.'mod'(int.'*'(J, S), 5))),
                        list.det_index0(RjPows, J)),
        0 `..` 4),
    Sum = list.foldl(cadd, Terms, complex(0.0, 0.0)),
    Result = complex(float.'/'(re(Sum), 5.0), float.'/'(im(Sum), 5.0)).

%---------------------------------------------------------------------------%
% R_j^5 expression reconstruction
%---------------------------------------------------------------------------%

:- func build_rj_pow_expr(list(rad_expr(rational)), int) =
    rad_expr(rational).

build_rj_pow_expr(DExprs, J) = Result :-
    Terms = list.map(
        func(S) = re_mul(list.det_index0(DExprs, S),
                          omega_pow5(int.'mod'(int.'*'(J, S), 5))),
        0 `..` 4),
    Result = foldl1_add(Terms).

:- func build_inv_dft(list(rad_expr(rational)), int) = rad_expr(rational).

build_inv_dft(AllR, K) = Result :-
    Terms = list.map(
        func(J) = re_mul(omega_pow5(
                          int.'mod'(int.'-'(5, int.'mod'(int.'*'(J, K), 5)),
                                    5)),
                          list.det_index0(AllR, J)),
        0 `..` 4),
    Result = foldl1_add(Terms).

:- func foldl1_add(list(rad_expr(rational))) = rad_expr(rational).

foldl1_add([]) = re_lit(rational.zero).
foldl1_add([H | T]) = list.foldl(func(X, Acc) = re_add(Acc, X), T, H).

%---------------------------------------------------------------------------%
% Finding the cyclic ordering
%---------------------------------------------------------------------------%

find_cyclic_ordering(Roots, N, GroupName) = Result :-
    ( if N = 5 then
        % Fix root 0 in position 0, try all 4! = 24 permutations of rest
        Rest = [1, 2, 3, 4],
        Perms = permutations(Rest),
        Orderings = list.map(func(P) = [0 | P], Perms),
        Scored = list.map(
            func(O) = {O, score_ordering(Roots, O, GroupName)},
            Orderings),
        SortedScored = list.sort(
            func({_, S1}, {_, S2}) = float_compare(S1, S2),
            Scored),
        ( if
            SortedScored = [{BestO, BestScore} | RestScored],
            RestScored = [{_, SecondScore} | _]
        then
            ( if
                float.'<'(BestScore, 2.0 `float.'*'` 5.0),
                float.'<'(BestScore, 0.5 `float.'*'` SecondScore)
            then
                Result = yes(BestO)
            else if float.'<'(BestScore, 5.0) then
                Result = yes(BestO)
            else
                Result = no
            )
        else
            Result = no
        )
    else
        Result = no
    ).

:- func float_compare(float, float) = comparison_result.

float_compare(A, B) = Result :-
    ( if float.'<'(A, B) then Result = (<)
    else if float.'>'(A, B) then Result = (>)
    else Result = (=)
    ).

%---------------------------------------------------------------------------%
% Scoring (orbit-based)
%---------------------------------------------------------------------------%

:- func score_ordering(list(complex), list(int), string) = float.

score_ordering(Roots, Ordering, GroupName) = Score :-
    Ordered = list.map(func(I) = list.det_index0(Roots, I), Ordering),
    RjPows = list.map(
        func(J) = complex_pow_n(compute_rj(Ordered, J), 5),
        0 `..` 4),
    DVals = list.map(func(S) = compute_ds(RjPows, S), 0 `..` 4),
    D0 = list.det_index0(DVals, 0),
    D1 = list.det_index0(DVals, 1),
    D2 = list.det_index0(DVals, 2),
    D3 = list.det_index0(DVals, 3),
    D4 = list.det_index0(DVals, 4),
    ( if GroupName = "C5" then
        Score = score_rational(D0) `float.'+'`
                score_rational(D1) `float.'+'`
                score_rational(D2) `float.'+'`
                score_rational(D3) `float.'+'`
                score_rational(D4)
    else if GroupName = "D5" then
        % τ = (14)(23): orbits {d0}, {d1,d4}, {d2,d3}
        Score = score_rational(D0) `float.'+'`
                score_rational(cadd(D1, D4)) `float.'+'`
                score_rational(cmul(D1, D4)) `float.'+'`
                score_rational(cadd(D2, D3)) `float.'+'`
                score_rational(cmul(D2, D3))
    else if GroupName = "F20" then
        % τ = (1243): orbits {d0}, {d1,d2,d3,d4}
        E1 = cadd(cadd(D1, D2), cadd(D3, D4)),
        E2 = cadd(cadd(cmul(D1, D2), cmul(D1, D3)),
                  cadd(cadd(cmul(D1, D4), cmul(D2, D3)),
                       cadd(cmul(D2, D4), cmul(D3, D4)))),
        E3 = cadd(cadd(cmul(D1, cmul(D2, D3)),
                       cmul(D1, cmul(D2, D4))),
                  cadd(cmul(D1, cmul(D3, D4)),
                       cmul(D2, cmul(D3, D4)))),
        E4 = cmul(cmul(D1, D2), cmul(D3, D4)),
        Score = score_rational(D0) `float.'+'`
                score_rational(E1) `float.'+'`
                score_rational(E2) `float.'+'`
                score_rational(E3) `float.'+'`
                score_rational(E4)
    else
        Score = 1.0e10
    ).

:- func score_rational(complex) = float.

score_rational(complex(Re, Im)) =
    float.'+'(float.abs(Im), frac_part(Re)).

:- func frac_part(float) = float.

frac_part(X) = float.abs(float.'-'(X,
    float.float(float.round_to_int(X)))).

%---------------------------------------------------------------------------%
% DFT coefficient matching
%---------------------------------------------------------------------------%

:- func match_ds(string, list(complex)) = maybe(list(rad_expr(rational))).

match_ds(GroupName, DVals) = Result :-
    ( if GroupName = "C5" then
        Result = match_ds_to_q(DVals)
    else if GroupName = "D5" then
        Result = match_ds_d5(DVals)
    else if GroupName = "F20" then
        Result = match_ds_f20(DVals)
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% C5: all d_s ∈ Q
%---------------------------------------------------------------------------%

:- func match_ds_to_q(list(complex)) = maybe(list(rad_expr(rational))).

match_ds_to_q(DVals) = Result :-
    ( if
        list.map(match_rat_c, DVals) = MaybeExprs,
        all_yes(MaybeExprs, Exprs)
    then
        Result = yes(Exprs)
    else
        Result = no
    ).

:- func match_rat_c(complex) = maybe(rad_expr(rational)).

match_rat_c(complex(Re, Im)) = Result :-
    ( if float.'>'(float.abs(Im), 0.1) then
        Result = no
    else
        Result = yes(re_lit(best_rational(Re)))
    ).

:- pred all_yes(list(maybe(T))::in, list(T)::out) is semidet.

all_yes([], []).
all_yes([yes(X) | Rest], [X | Ys]) :-
    all_yes(Rest, Ys).

%---------------------------------------------------------------------------%
% D5: conjugate pairs {d1,d4}, {d2,d3} satisfy quadratics
%---------------------------------------------------------------------------%

:- func match_ds_d5(list(complex)) = maybe(list(rad_expr(rational))).

match_ds_d5(DVals) = Result :-
    D0 = list.det_index0(DVals, 0),
    D1 = list.det_index0(DVals, 1),
    D2 = list.det_index0(DVals, 2),
    D3 = list.det_index0(DVals, 3),
    D4 = list.det_index0(DVals, 4),
    ( if
        match_rat_c(D0) = yes(D0Expr),

        % {d1, d4}: t² - s1·t + p1 = 0
        S1C = cadd(D1, D4),
        P1C = cmul(D1, D4),
        match_rat_c(S1C) = yes(S1Expr),
        match_rat_c(P1C) = yes(P1Expr),

        % {d2, d3}: t² - s2·t + p2 = 0
        S2C = cadd(D2, D3),
        P2C = cmul(D2, D3),
        match_rat_c(S2C) = yes(S2Expr),
        match_rat_c(P2C) = yes(P2Expr)
    then
        S1R = best_rational(re(S1C)),
        P1R = best_rational(re(P1C)),
        Disc1R = rational.'-'(rational.'*'(S1R, S1R),
                              rational.'*'(rational(4), P1R)),
        Disc1Expr = re_add(re_mul(S1Expr, S1Expr),
                           re_neg(re_mul(re_lit(rational(4)), P1Expr))),
        SqrtDisc1 = re_root(2, Disc1Expr),
        D1Plus = re_mul(re_inv(re_lit(rational(2))),
                        re_add(S1Expr, SqrtDisc1)),
        D1Minus = re_mul(re_inv(re_lit(rational(2))),
                         re_add(S1Expr, re_neg(SqrtDisc1))),

        % Determine branch numerically
        Disc1F = rational_to_float(Disc1R),
        SqrtDisc1Val = ( if float.'>='(Disc1F, 0.0) then
                            complex(math.sqrt(Disc1F), 0.0)
                         else
                            complex(0.0, math.sqrt(float.abs(Disc1F))) ),
        D1PlusVal = complex(
            float.'/'(float.'+'(re(S1C), re(SqrtDisc1Val)), 2.0),
            float.'/'(float.'+'(im(S1C), im(SqrtDisc1Val)), 2.0)),
        DistPlus1 = cmag(csub(D1PlusVal, D1)),
        DistPlus4 = cmag(csub(D1PlusVal, D4)),
        {D1Expr, D4Expr} = ( if float.'<'(DistPlus1, DistPlus4) then
                                {D1Plus, D1Minus}
                             else
                                {D1Minus, D1Plus} ),

        % Same for {d2, d3}
        S2R = best_rational(re(S2C)),
        P2R = best_rational(re(P2C)),
        Disc2R = rational.'-'(rational.'*'(S2R, S2R),
                              rational.'*'(rational(4), P2R)),
        Disc2Expr = re_add(re_mul(S2Expr, S2Expr),
                           re_neg(re_mul(re_lit(rational(4)), P2Expr))),
        SqrtDisc2 = re_root(2, Disc2Expr),
        D2Plus = re_mul(re_inv(re_lit(rational(2))),
                        re_add(S2Expr, SqrtDisc2)),
        D2Minus = re_mul(re_inv(re_lit(rational(2))),
                         re_add(S2Expr, re_neg(SqrtDisc2))),

        Disc2F = rational_to_float(Disc2R),
        SqrtDisc2Val = ( if float.'>='(Disc2F, 0.0) then
                            complex(math.sqrt(Disc2F), 0.0)
                         else
                            complex(0.0, math.sqrt(float.abs(Disc2F))) ),
        D2PlusVal = complex(
            float.'/'(float.'+'(re(S2C), re(SqrtDisc2Val)), 2.0),
            float.'/'(float.'+'(im(S2C), im(SqrtDisc2Val)), 2.0)),
        DistPlus2 = cmag(csub(D2PlusVal, D2)),
        DistPlus3 = cmag(csub(D2PlusVal, D3)),
        {D2Expr, D3Expr} = ( if float.'<'(DistPlus2, DistPlus3) then
                                {D2Plus, D2Minus}
                             else
                                {D2Minus, D2Plus} ),

        Result = yes([D0Expr, D1Expr, D2Expr, D3Expr, D4Expr])
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% F20: d_s ∈ Q(ω5)
%---------------------------------------------------------------------------%

:- func match_ds_f20(list(complex)) = maybe(list(rad_expr(rational))).

match_ds_f20(DVals) = Result :-
    D0 = list.det_index0(DVals, 0),
    D1 = list.det_index0(DVals, 1),
    D2 = list.det_index0(DVals, 2),
    D3 = list.det_index0(DVals, 3),
    D4 = list.det_index0(DVals, 4),
    ( if
        match_rat_c(D0) = yes(D0Expr),
        match_q_omega5(D1) = yes(D1Expr),
        match_q_omega5(D2) = yes(D2Expr),
        match_q_omega5(D3) = yes(D3Expr),
        match_q_omega5(D4) = yes(D4Expr)
    then
        Result = yes([D0Expr, D1Expr, D2Expr, D3Expr, D4Expr])
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Q(ω5) matching: progressive decomposition
%---------------------------------------------------------------------------%

:- func match_q_omega5(complex) = maybe(rad_expr(rational)).

match_q_omega5(D) = Result :-
    ( if match_single_omega5(D) = yes(E) then
        Result = yes(E)
    else if match_two_term_omega5(D) = yes(E) then
        Result = yes(E)
    else
        Result = match_general_q_omega5(D)
    ).

%---------------------------------------------------------------------------%
% Single-term: d ≈ r · ω5^k
%---------------------------------------------------------------------------%

:- func match_single_omega5(complex) = maybe(rad_expr(rational)).

match_single_omega5(D) = Result :-
    % Try d / ω5^k for k = 0..4
    Candidates = list.map(
        func(K) = {K, cmul(D, omega5_c(int.'-'(0, K)))},
        0 `..` 4),
    Scored = list.map(
        func({K, V}) = {K, V, score_rational(V)},
        Candidates),
    {BestK, BestV, BestScore} = find_min_score3(Scored),
    ( if float.'<'(BestScore, 0.01) then
        R = best_rational(re(BestV)),
        ( if BestK = 0 then
            Result = yes(re_lit(R))
        else
            Result = yes(re_mul(re_lit(R), omega_pow5(BestK)))
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Two-term: d ≈ a + b · ω5^k
%---------------------------------------------------------------------------%

:- func match_two_term_omega5(complex) = maybe(rad_expr(rational)).

match_two_term_omega5(D) = Result :-
    % For each k in 1..4, try d - b·ω5^k is rational
    Tries = list.condense(list.map(
        func(K) = try_two_term(D, K),
        1 `..` 4)),
    ( if Tries = [] then
        Result = no
    else
        {_, BestK, BestR, _, BestScore} = find_min_score5(Tries),
        ( if float.'<'(BestScore, 0.01) then
            A = best_rational(re(csub(D,
                    cmul(complex(rational_to_float(BestR), 0.0),
                         omega5_c(BestK))))),
            Result = yes(re_add(re_lit(A),
                                re_mul(re_lit(BestR), omega_pow5(BestK))))
        else
            Result = no
        )
    ).

:- func try_two_term(complex, int) = list({float, int, rational, complex, float}).

try_two_term(D, K) = Result :-
    % b ≈ Re(d · ω5^{-k})
    BApprox = re(cmul(D, omega5_c(int.'-'(0, K)))),
    BR = best_rational(BApprox),
    Remainder = csub(D, cmul(complex(rational_to_float(BR), 0.0),
                              omega5_c(K))),
    S = score_rational(Remainder),
    Result = [{S, K, BR, Remainder, S}].

%---------------------------------------------------------------------------%
% General Q(ω5): d = a0 + a1·ω5 + a2·ω5² + a3·ω5³
%---------------------------------------------------------------------------%

:- func match_general_q_omega5(complex) = maybe(rad_expr(rational)).

match_general_q_omega5(complex(Re, Im)) = Result :-
    S1 = math.sin(float.'*'(2.0, float.'*'(math.pi, float.'/'(1.0, 5.0)))),
    S2 = math.sin(float.'*'(2.0, float.'*'(math.pi, float.'/'(2.0, 5.0)))),
    C1 = math.cos(float.'*'(2.0, float.'*'(math.pi, float.'/'(1.0, 5.0)))),
    C2 = math.cos(float.'*'(2.0, float.'*'(math.pi, float.'/'(2.0, 5.0)))),

    % im = a1·sin(2π/5) + v·sin(4π/5) where v = a2 - a3
    % re = a0 + a1·cos(2π/5) + u·cos(4π/5) where u = a2 + a3
    Candidates = list.condense(list.map(
        func(A1Cand) = candidate_general(Re, Im, A1Cand, S1, S2, C1, C2),
        candidate_rats(float.'/'(Im, S1)))),

    ( if Candidates = [] then
        Result = no
    else
        {BestErr, A0R, A1R, A2R, A3R} = find_min_err(Candidates),
        ( if float.'<'(BestErr, 0.01) then
            Result = yes(build_q_omega5_expr(A0R, A1R, A2R, A3R))
        else
            Result = no
        )
    ).

:- func candidate_general(float, float, rational,
    float, float, float, float) = list({float, rational, rational,
    rational, rational}).

candidate_general(Re, Im, A1R, S1, S2, C1, C2) = Result :-
    A1 = rational_to_float(A1R),
    V = float.'/'(float.'-'(Im, float.'*'(A1, S1)), S2),
    VRs = candidate_rats(V),
    Result = list.condense(list.map(
        func(VR) = candidate_general_inner(Re, Im, A1R, VR, A1,
                                           S1, S2, C1, C2),
        VRs)).

:- func candidate_general_inner(float, float, rational, rational,
    float, float, float, float, float) =
    list({float, rational, rational, rational, rational}).

candidate_general_inner(Re, Im, A1R, VR, A1, S1, S2, C1, C2) = Result :-
    URest = float.'/'(float.'-'(Re, float.'*'(A1, C1)), C2),
    URs0 = candidate_rats(URest),
    URs = [rational.zero | URs0],
    list.filter_map(
        (pred(UR::in, R::out) is semidet :-
            A0D = float.'-'(Re, float.'+'(float.'*'(A1, C1),
                   float.'*'(rational_to_float(UR), C2))),
            candidate_rats(A0D) = [A0R | _],
            A2R = rational.'/'(rational.'+'(UR, VR), rational(2)),
            A3R = rational.'/'(rational.'-'(UR, VR), rational(2)),
            Recon = float.'+'(rational_to_float(A0R),
                    float.'+'(float.'*'(rational_to_float(A1R), C1),
                    float.'*'(float.'+'(rational_to_float(A2R),
                                        rational_to_float(A3R)), C2))),
            ReconIm = float.'+'(float.'*'(rational_to_float(A1R), S1),
                      float.'*'(float.'-'(rational_to_float(A2R),
                                          rational_to_float(A3R)), S2)),
            Err = float.'+'(float.abs(float.'-'(Recon, Re)),
                            float.abs(float.'-'(ReconIm, Im))),
            R = {Err, A0R, A1R, A2R, A3R}
        ), URs, Result).

:- func candidate_rats(float) = list(rational).

candidate_rats(X) = Result :-
    R = best_rational(X),
    ( if float.'<'(float.abs(float.'-'(rational_to_float(R), X)), 0.01) then
        Result = [R]
    else
        Result = []
    ).

:- func build_q_omega5_expr(rational, rational, rational, rational) =
    rad_expr(rational).

build_q_omega5_expr(A0, A1, A2, A3) = Result :-
    Terms0 = [{A0, 0}, {A1, 1}, {A2, 2}, {A3, 3}],
    NonZero = list.filter(
        (pred({C, _}::in) is semidet :- C \= rational.zero),
        Terms0),
    ( if NonZero = [] then
        Result = re_lit(rational.zero)
    else
        Exprs = list.map(mk_omega5_term, NonZero),
        Result = foldl1_add(Exprs)
    ).

:- func mk_omega5_term({rational, int}) = rad_expr(rational).

mk_omega5_term({C, K}) = Result :-
    ( if K = 0 then
        Result = re_lit(C)
    else
        Result = re_mul(re_lit(C), omega_pow5(K))
    ).

%---------------------------------------------------------------------------%
% ω5 expressions
%---------------------------------------------------------------------------%

    % ω5 = cos(2π/5) + i·sin(2π/5) as RadExpr.
    % cos(2π/5) = (√5 - 1)/4
    % sin(2π/5) = √(10 + 2√5)/4
    %
:- func omega5_expr = rad_expr(rational).

omega5_expr = Result :-
    Cos5 = re_mul(re_inv(re_lit(rational(4))),
                  re_add(re_root(2, re_lit(rational(5))),
                         re_neg(re_lit(rational.one)))),
    Sin5 = re_mul(re_inv(re_lit(rational(4))),
                  re_root(2, re_add(re_lit(rational(10)),
                                    re_mul(re_lit(rational(2)),
                                           re_root(2, re_lit(rational(5))))))),
    I = re_root(2, re_lit(rational.'-'(rational.zero, rational.one))),
    Result = re_add(Cos5, re_mul(I, Sin5)).

    % ω5^k as RadExpr, reduced mod 5.
    %
:- func omega_pow5(int) = rad_expr(rational).

omega_pow5(K) = Result :-
    KMod = int.'mod'(K, 5),
    ( if KMod = 0 then
        Result = re_lit(rational.one)
    else if KMod = 1 then
        Result = omega5_expr
    else
        Result = re_pow(omega5_expr, KMod)
    ).

    % ω5^k as Complex Double for numerical evaluation.
    %
:- func omega5_c(int) = complex.

omega5_c(K) = complex(
    math.cos(float.'*'(2.0, float.'*'(math.pi,
        float.'/'(float.float(K), 5.0)))),
    math.sin(float.'*'(2.0, float.'*'(math.pi,
        float.'/'(float.float(K), 5.0))))).

%---------------------------------------------------------------------------%
% Branch selection for ⁵√
%---------------------------------------------------------------------------%

:- func select_branch5(rad_expr(rational), complex) = rad_expr(rational).

select_branch5(Rj5Expr, TargetVal) = Result :-
    % Evaluate R_j^5 numerically
    Rj5Val = eval_complex(Rj5Expr),
    % Principal 5th root via exp/ln
    Mag = cmag(Rj5Val),
    Phase = math.atan2(im(Rj5Val), re(Rj5Val)),
    ( if float.'<'(Mag, 1.0e-300) then
        Result = re_root(5, Rj5Expr)
    else
        RootMag = math.exp(float.'*'(0.2, math.ln(Mag))),
        RootPhase = float.'/'(Phase, 5.0),
        PrincipalVal = complex(
            float.'*'(RootMag, math.cos(RootPhase)),
            float.'*'(RootMag, math.sin(RootPhase))),
        PrincipalRoot = re_root(5, Rj5Expr),
        % Try all branches
        Scored = list.map(
            func(K) = {K, cmag(csub(cmul(omega5_c(K), PrincipalVal),
                                     TargetVal))},
            0 `..` 4),
        {BestK, _} = find_min_dist(Scored),
        ( if BestK = 0 then
            Result = PrincipalRoot
        else
            Result = re_mul(omega_pow5(BestK), PrincipalRoot)
        )
    ).

%---------------------------------------------------------------------------%
% Match radical expressions to original root ordering by proximity
%---------------------------------------------------------------------------%

:- func match_to_original(list(rad_expr(rational)), list(complex)) =
    list(rad_expr(rational)).

match_to_original(Exprs, NumRoots) = Result :-
    ExprVals = list.map(func(E) = {E, eval_complex(E)}, Exprs),
    Result = list.map(
        func(T) = closest_expr(ExprVals, T),
        NumRoots).

:- func closest_expr(list({rad_expr(rational), complex}), complex) =
    rad_expr(rational).

closest_expr(ExprVals, Target) = Result :-
    ( if ExprVals = [{E0, V0} | Rest] then
        closest_expr_loop(Rest, Target, E0, cmag(csub(V0, Target)), Result)
    else
        unexpected($module, $pred, "empty expression list")
    ).

:- pred closest_expr_loop(list({rad_expr(rational), complex})::in,
    complex::in, rad_expr(rational)::in, float::in,
    rad_expr(rational)::out) is det.

closest_expr_loop([], _, BestE, _, BestE).
closest_expr_loop([{E, V} | Rest], Target, BestE, BestD, Result) :-
    D = cmag(csub(V, Target)),
    ( if float.'<'(D, BestD) then
        closest_expr_loop(Rest, Target, E, D, Result)
    else
        closest_expr_loop(Rest, Target, BestE, BestD, Result)
    ).

%---------------------------------------------------------------------------%
% DAG fold helper
%---------------------------------------------------------------------------%

:- func dag_fold_expr(rad_expr(rational)) = rad_expr(rational).

dag_fold_expr(E) = from_dag(dag_fold_constants(to_dag(E))).

%---------------------------------------------------------------------------%
% Complex arithmetic helpers
%---------------------------------------------------------------------------%

:- func cmag(complex) = float.

cmag(complex(R, I)) = math.sqrt(float.'+'(float.'*'(R, R),
                                            float.'*'(I, I))).

:- func complex_pow_n(complex, int) = complex.

complex_pow_n(Z, N) = Result :-
    ( if N = 0 then
        Result = complex(1.0, 0.0)
    else if int.'<'(N, 0) then
        Result = cinv(complex_pow_n(Z, int.'-'(0, N)))
    else if int.even(N) then
        Half = complex_pow_n(Z, int.'//'(N, 2)),
        Result = cmul(Half, Half)
    else
        Result = cmul(Z, complex_pow_n(Z, int.'-'(N, 1)))
    ).

:- func rational_to_float(rational) = float.

rational_to_float(R) = Result :-
    N = integer.to_string(numer(R)),
    D = integer.to_string(denom(R)),
    ( if string.to_float(N, NF), string.to_float(D, DF) then
        Result = float.'/'(NF, DF)
    else
        Result = 0.0
    ).

%---------------------------------------------------------------------------%
% Permutations of a list (all orderings)
%---------------------------------------------------------------------------%

:- func permutations(list(T)) = list(list(T)).

permutations([]) = [[]].
permutations(Xs) = Result :-
    Xs = [_ | _],
    Result = list.condense(list.map(
        func(X) = list.map(
            func(P) = [X | P],
            permutations(list.delete_all(Xs, X))),
        Xs)).

%---------------------------------------------------------------------------%
% Min-finding helpers
%---------------------------------------------------------------------------%

:- func find_min_score3(list({int, complex, float})) = {int, complex, float}.

find_min_score3(Xs) = Result :-
    ( if Xs = [H | T] then
        Result = list.foldl(
            func({K, V, S}, {BK, BV, BS}) = R :-
                ( if float.'<'(S, BS) then R = {K, V, S}
                else R = {BK, BV, BS} ),
            T, H)
    else
        unexpected($module, $pred, "empty list")
    ).

:- func find_min_score5(list({float, int, rational, complex, float})) =
    {float, int, rational, complex, float}.

find_min_score5(Xs) = Result :-
    ( if Xs = [H | T] then
        Result = list.foldl(
            func({E1, K1, R1, V1, S1}, {_, BK, BR, BV, BS}) = R :-
                ( if float.'<'(E1, BS) then R = {E1, K1, R1, V1, S1}
                else R = {BS, BK, BR, BV, BS} ),
            T, H)
    else
        unexpected($module, $pred, "empty list")
    ).

:- func find_min_dist(list({int, float})) = {int, float}.

find_min_dist(Xs) = Result :-
    ( if Xs = [H | T] then
        Result = list.foldl(
            func({K, D}, {BK, BD}) = R :-
                ( if float.'<'(D, BD) then R = {K, D}
                else R = {BK, BD} ),
            T, H)
    else
        unexpected($module, $pred, "empty list")
    ).

:- func find_min_err(list({float, rational, rational, rational, rational})) =
    {float, rational, rational, rational, rational}.

find_min_err(Xs) = Result :-
    ( if Xs = [H | T] then
        Result = list.foldl(
            func({E1, A, B, C, D}, {BE, BA, BB, BC, BD}) = R :-
                ( if float.'<'(E1, BE) then R = {E1, A, B, C, D}
                else R = {BE, BA, BB, BC, BD} ),
            T, H)
    else
        unexpected($module, $pred, "empty list")
    ).

%---------------------------------------------------------------------------%
:- end_module radical_tower.
%---------------------------------------------------------------------------%
