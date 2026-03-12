%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: pslq.m
% Main author: nyc
%
% PSLQ algorithm for finding integer relations.
%
% Given a vector x = (x₁, ..., xₙ) of real numbers, PSLQ finds
% an integer vector m = (m₁, ..., mₙ) such that m·x = 0, or
% determines that no such relation exists within an iteration limit.
%
% Primary application: given α (a real algebraic number), find its
% minimal polynomial by searching for an integer relation among
% (1, α, α², ..., αᵈ).
%
% Reference: Ferguson & Bailey, "A Polynomial Time, Numerically
% Stable Integer Relation Algorithm" (1999).
%
% Matrix operations use flat arrays for O(1) indexing and
% Givens rotations for numerical stability.
%
%---------------------------------------------------------------------------%

:- module pslq.
:- interface.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % pslq(Xs, MaxIter) = MaybeRelation.
    % Find an integer relation among the reals in Xs.
    % Returns yes(Coeffs) if found, no otherwise.
    %
:- func pslq(list(float), int) = maybe(list(int)).

    % find_min_poly(Alpha, MaxDeg) = MaybeCoeffs.
    % Find the minimal polynomial of a real algebraic number.
    % Returns the coefficient list [c₀, c₁, ..., cₐ] (constant first).
    %
:- func find_min_poly(float, int) = maybe(list(int)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module float.
:- import_module int.
:- import_module math.
:- import_module poly.
:- import_module factoring.
:- import_module rational.

%---------------------------------------------------------------------------%
% Dense matrix type (row-major, 0-indexed)
%---------------------------------------------------------------------------%

:- type matrix
    --->    matrix(
                mat_rows :: int,
                mat_cols :: int,
                mat_data :: array(float)
            ).

:- func new_matrix(int, int, float) = matrix.

new_matrix(R, C, Val) = matrix(R, C, array.init(R * C, Val)).

:- func mat_get(matrix, int, int) = float.

mat_get(M, I, J) = array.lookup(mat_data(M), I * mat_cols(M) + J).

:- func mat_set(matrix, int, int, float) = matrix.

mat_set(M, I, J, V) = M ^ mat_data :=
    array.slow_set(mat_data(M), I * mat_cols(M) + J, V).

%---------------------------------------------------------------------------%
% Dense vector type (0-indexed)
%---------------------------------------------------------------------------%

:- type vec == array(float).

:- func vec_get(vec, int) = float.

vec_get(V, I) = array.lookup(V, I).

:- func vec_set(vec, int, float) = vec.

vec_set(V, I, X) = array.slow_set(V, I, X).

:- func vec_from_list(list(float)) = vec.

vec_from_list(Xs) = array.from_list(Xs).

%---------------------------------------------------------------------------%
% Givens rotation
%---------------------------------------------------------------------------%

:- pred givens_rotation(float::in, float::in, float::out, float::out) is det.

givens_rotation(A, B, Co, Si) :-
    ( if float.abs(A) < 1.0e-30, float.abs(B) < 1.0e-30 then
        Co = 1.0, Si = 0.0
    else
        R = math.sqrt(A * A + B * B),
        Co = A / R,
        Si = B / R
    ).

%---------------------------------------------------------------------------%
% PSLQ core
%---------------------------------------------------------------------------%

pslq(Xs, MaxIter) = Result :-
    N = list.length(Xs),
    ( if N < 2 then
        Result = no
    else if has_nan(Xs) then
        Result = no
    else if has_inf(Xs) then
        Result = no
    else
        NormX = math.sqrt(list.foldl(
            ( func(X, Acc) = Acc + X * X ), Xs, 0.0)),
        ( if NormX < 1.0e-20 then
            Result = no
        else
            Result = pslq_go(Xs, MaxIter, N, NormX)
        )
    ).

:- pred has_nan(list(float)::in) is semidet.
has_nan([X | Xs]) :-
    ( if float.is_nan(X) then true else has_nan(Xs) ).

:- pred has_inf(list(float)::in) is semidet.
has_inf([X | Xs]) :-
    ( if float.is_inf(X) then true else has_inf(Xs) ).

:- func pslq_go(list(float), int, int, float) = maybe(list(int)).

pslq_go(Xs, MaxIter, N, NormX) = Result :-
    % Normalize
    XNorm = list.map(( func(X) = X / NormX ), Xs),
    XArr = vec_from_list(XNorm),

    % Partial sums of squares
    SS = init_partial_sums(XArr, N),

    % Initial y = normalized x
    Y0 = XArr,

    % Initial H: n × (n-1) lower trapezoidal
    NH = N - 1,
    H0 = init_h(XArr, SS, N, NH),

    % Initial A = I, B = I
    A0 = identity_matrix(N),
    B0 = identity_matrix(N),

    % Run the loop
    Result = pslq_loop(Xs, Y0, H0, A0, B0, N, NH, MaxIter, 0).

:- func init_partial_sums(vec, int) = vec.

init_partial_sums(XArr, N) = SS :-
    % ss[j] = sqrt(sum_{k=j}^{n-1} x_k^2)
    SS0 = array.init(N, 0.0),
    SS = init_ss_loop(XArr, N, N - 1, SS0).

:- func init_ss_loop(vec, int, int, vec) = vec.

init_ss_loop(XArr, N, J, SS) = Result :-
    ( if J < 0 then
        Result = SS
    else
        SumSq = sum_squares_from(XArr, J, N - 1, 0.0),
        Result = init_ss_loop(XArr, N, J - 1,
            vec_set(SS, J, math.sqrt(SumSq)))
    ).

:- func sum_squares_from(vec, int, int, float) = float.

sum_squares_from(XArr, From, To, Acc) = Result :-
    ( if From > To then
        Result = Acc
    else
        X = vec_get(XArr, From),
        Result = sum_squares_from(XArr, From + 1, To, Acc + X * X)
    ).

:- func init_h(vec, vec, int, int) = matrix.

init_h(XArr, SS, N, NH) = H :-
    H0 = new_matrix(N, NH, 0.0),
    H = init_h_ij(XArr, SS, N, NH, 0, 0, H0).

:- func init_h_ij(vec, vec, int, int, int, int, matrix) = matrix.

init_h_ij(XArr, SS, N, NH, I, J, H) = Result :-
    ( if I >= N then
        Result = H
    else if J >= NH then
        Result = init_h_ij(XArr, SS, N, NH, I + 1, 0, H)
    else
        ( if I < J then
            Val = 0.0
        else if I = J then
            SJ = vec_get(SS, J),
            SJ1 = vec_get(SS, J + 1),
            ( if SJ < 1.0e-20 then Val = 0.0
            else Val = SJ1 / SJ )
        else
            SJ = vec_get(SS, J),
            SJ1 = vec_get(SS, J + 1),
            XI = vec_get(XArr, I),
            XJ = vec_get(XArr, J),
            ( if SJ < 1.0e-20 then Val = 0.0
            else if SJ1 < 1.0e-20 then Val = 0.0
            else Val = -(XI * XJ) / (SJ * SJ1) )
        ),
        Result = init_h_ij(XArr, SS, N, NH, I, J + 1,
            mat_set(H, I, J, Val))
    ).

:- func identity_matrix(int) = matrix.

identity_matrix(N) = M :-
    M0 = new_matrix(N, N, 0.0),
    M = set_diagonal(M0, N, 0).

:- func set_diagonal(matrix, int, int) = matrix.

set_diagonal(M, N, I) = Result :-
    ( if I >= N then Result = M
    else Result = set_diagonal(mat_set(M, I, I, 1.0), N, I + 1)
    ).

%---------------------------------------------------------------------------%
% Main PSLQ iteration loop
%---------------------------------------------------------------------------%

:- func pslq_loop(list(float), vec, matrix, matrix, matrix,
    int, int, int, int) = maybe(list(int)).

pslq_loop(OrigXs, Y, H, A, B, N, NH, MaxIter, Iter) = Result :-
    ( if Iter >= MaxIter then
        Result = no
    else
        Gam = math.sqrt(2.0),

        % Step 1: Select pivot m to maximise γ^(j+1) |H_jj|
        PivotM = find_best_pivot(H, N, NH, Gam, 1,
            0, Gam * float.abs(mat_get(H, 0, 0))),

        % Step 2: Exchange rows pivotM ↔ pivotM+1
        Y1 = swap_vec(Y, PivotM, PivotM + 1),
        H1 = swap_rows(H, PivotM, PivotM + 1, NH),
        A1 = swap_rows(A, PivotM, PivotM + 1, N),
        B1 = swap_cols(B, PivotM, PivotM + 1, N),

        % Step 3: Givens rotation to restore H upper Hessenberg
        H2 = maybe_givens(H1, N, NH, PivotM),

        % Step 4: Hermite reduction
        {H3, A2, B2, Y2} = hermite_reduce(N, NH, H2, A1, B1, Y1),

        % Step 5: Check termination
        MaybeRel = check_columns(OrigXs, B2, N, 0),
        ( if MaybeRel = yes(_) then
            Result = MaybeRel
        else
            Result = pslq_loop(OrigXs, Y2, H3, A2, B2,
                N, NH, MaxIter, Iter + 1)
        )
    ).

:- func maybe_givens(matrix, int, int, int) = matrix.

maybe_givens(H1, N, NH, PivotM) = H2 :-
    ( if PivotM < N - 2 then
        HA = mat_get(H1, PivotM, PivotM),
        HB = safe_h_get(H1, PivotM, PivotM + 1, NH),
        givens_rotation(HA, HB, Co, Si),
        H2 = apply_givens_cols(H1, N, NH, PivotM, Co, Si)
    else
        H2 = H1
    ).

:- func find_best_pivot(matrix, int, int, float, int,
    int, float) = int.

find_best_pivot(H, N, NH, Gam, J, BestIdx, BestVal) = Result :-
    ( if J >= N - 1 then
        Result = BestIdx
    else
        Val = math.pow(Gam, float.float(J + 1)) *
            float.abs(mat_get(H, J, J)),
        ( if Val > BestVal then
            Result = find_best_pivot(H, N, NH, Gam, J + 1, J, Val)
        else
            Result = find_best_pivot(H, N, NH, Gam, J + 1,
                BestIdx, BestVal)
        )
    ).

:- func swap_vec(vec, int, int) = vec.

swap_vec(V, I, J) = vec_set(vec_set(V, I, vec_get(V, J)), J, vec_get(V, I)).

:- func swap_rows(matrix, int, int, int) = matrix.

swap_rows(M, R1, R2, NCols) = swap_rows_col(M, R1, R2, NCols, 0).

:- func swap_rows_col(matrix, int, int, int, int) = matrix.

swap_rows_col(M, R1, R2, NCols, J) = Result :-
    ( if J >= NCols then Result = M
    else
        V1 = mat_get(M, R1, J),
        V2 = mat_get(M, R2, J),
        Result = swap_rows_col(
            mat_set(mat_set(M, R1, J, V2), R2, J, V1),
            R1, R2, NCols, J + 1)
    ).

:- func swap_cols(matrix, int, int, int) = matrix.

swap_cols(M, C1, C2, NRows) = swap_cols_row(M, C1, C2, NRows, 0).

:- func swap_cols_row(matrix, int, int, int, int) = matrix.

swap_cols_row(M, C1, C2, NRows, I) = Result :-
    ( if I >= NRows then Result = M
    else
        V1 = mat_get(M, I, C1),
        V2 = mat_get(M, I, C2),
        Result = swap_cols_row(
            mat_set(mat_set(M, I, C1, V2), I, C2, V1),
            C1, C2, NRows, I + 1)
    ).

:- func safe_h_get(matrix, int, int, int) = float.

safe_h_get(H, I, J, NCols) =
    ( if J < NCols then mat_get(H, I, J) else 0.0 ).

:- func apply_givens_cols(matrix, int, int, int, float, float) = matrix.

apply_givens_cols(H, N, NH, PivotM, Co, Si) =
    apply_givens_row(H, N, NH, PivotM, Co, Si, 0).

:- func apply_givens_row(matrix, int, int, int, float, float, int) = matrix.

apply_givens_row(H, N, NH, PM, Co, Si, I) = Result :-
    ( if I >= N then Result = H
    else
        C1 = mat_get(H, I, PM),
        C2 = safe_h_get(H, I, PM + 1, NH),
        New1 = Co * C1 + Si * C2,
        New2 = -(Si) * C1 + Co * C2,
        H1 = mat_set(H, I, PM, New1),
        H2 = ( if PM + 1 < NH then mat_set(H1, I, PM + 1, New2) else H1 ),
        Result = apply_givens_row(H2, N, NH, PM, Co, Si, I + 1)
    ).

%---------------------------------------------------------------------------%
% Hermite reduction
%---------------------------------------------------------------------------%

:- func hermite_reduce(int, int, matrix, matrix, matrix, vec)
    = {matrix, matrix, matrix, vec}.

hermite_reduce(N, NH, H, A, B, Y) =
    hermite_pairs(N, NH, H, A, B, Y, hermite_pair_list(N)).

:- func hermite_pair_list(int) = list({int, int}).

hermite_pair_list(N) = Pairs :-
    Pairs = list.condense(
        list.map(( func(I) =
            list.map(( func(J) = {I, J} ),
                int_range_down(I - 1, 0))
        ), int_range(1, N - 1))).

:- func int_range(int, int) = list(int).

int_range(Lo, Hi) = Result :-
    ( if Lo > Hi then Result = []
    else Result = [Lo | int_range(Lo + 1, Hi)]
    ).

:- func int_range_down(int, int) = list(int).

int_range_down(Hi, Lo) = Result :-
    ( if Hi < Lo then Result = []
    else Result = [Hi | int_range_down(Hi - 1, Lo)]
    ).

:- func hermite_pairs(int, int, matrix, matrix, matrix, vec,
    list({int, int})) = {matrix, matrix, matrix, vec}.

hermite_pairs(N, NH, H, A, B, Y, Pairs) = Result :-
    ( if Pairs = [] then
        Result = {H, A, B, Y}
    else if Pairs = [{I, J} | Rest] then
        HJJ = mat_get(H, J, J),
        ( if float.abs(HJJ) > 1.0e-20 then
            T0 = mat_get(H, I, J) / HJJ,
            T = float.float(float.round_to_int(T0))
        else
            T = 0.0
        ),
        ( if float.abs(T) < 0.5 then
            Result = hermite_pairs(N, NH, H, A, B, Y, Rest)
        else
            H1 = update_row(H, I, J, T, NH),
            A1 = update_row(A, I, J, T, N),
            B1 = update_col(B, J, I, T, N),
            Y1 = vec_set(Y, J, vec_get(Y, J) - T * vec_get(Y, I)),
            Result = hermite_pairs(N, NH, H1, A1, B1, Y1, Rest)
        )
    else
        Result = {H, A, B, Y}
    ).

:- func update_row(matrix, int, int, float, int) = matrix.

update_row(M, I, J, T, NCols) = update_row_col(M, I, J, T, NCols, 0).

:- func update_row_col(matrix, int, int, float, int, int) = matrix.

update_row_col(M, I, J, T, NCols, C) = Result :-
    ( if C >= NCols then Result = M
    else
        New = mat_get(M, I, C) - T * mat_get(M, J, C),
        Result = update_row_col(mat_set(M, I, C, New), I, J, T, NCols, C + 1)
    ).

:- func update_col(matrix, int, int, float, int) = matrix.

update_col(M, J, I, T, NRows) = update_col_row(M, J, I, T, NRows, 0).

:- func update_col_row(matrix, int, int, float, int, int) = matrix.

update_col_row(M, J, I, T, NRows, R) = Result :-
    ( if R >= NRows then Result = M
    else
        New = mat_get(M, R, J) + T * mat_get(M, R, I),
        Result = update_col_row(mat_set(M, R, J, New), J, I, T, NRows, R + 1)
    ).

%---------------------------------------------------------------------------%
% Termination check
%---------------------------------------------------------------------------%

:- func check_columns(list(float), matrix, int, int) = maybe(list(int)).

check_columns(OrigXs, B, N, J) = Result :-
    ( if J >= N then
        Result = no
    else
        Col = extract_col_rounded(B, N, J),
        ( if has_nonzero(Col) then
            Dot = dot_int_float(Col, OrigXs),
            ( if float.abs(Dot) < 1.0e-10 then
                Result = yes(Col)
            else
                Result = check_columns(OrigXs, B, N, J + 1)
            )
        else
            Result = check_columns(OrigXs, B, N, J + 1)
        )
    ).

:- pred has_nonzero(list(int)::in) is semidet.
has_nonzero([C | Cs]) :-
    ( if C \= 0 then true else has_nonzero(Cs) ).

:- func extract_col_rounded(matrix, int, int) = list(int).

extract_col_rounded(B, N, J) =
    list.map(( func(I) = float.round_to_int(mat_get(B, I, J)) ),
        int_range(0, N - 1)).

:- func dot_int_float(list(int), list(float)) = float.

dot_int_float(Cs, Xs) = Result :-
    ( if Cs = [C | CRest], Xs = [X | XRest] then
        Result = float.float(C) * X + dot_int_float(CRest, XRest)
    else
        Result = 0.0
    ).

%---------------------------------------------------------------------------%
% findMinPoly
%---------------------------------------------------------------------------%

find_min_poly(Alpha, MaxDeg) = try_degree(Alpha, MaxDeg, 1).

:- func try_degree(float, int, int) = maybe(list(int)).

try_degree(Alpha, MaxDeg, D) = Result :-
    ( if D > MaxDeg then
        Result = no
    else
        Powers = list.map(( func(I) = math.pow(Alpha, float.float(I)) ),
            int_range(0, D)),
        MaybePSLQ = pslq(Powers, 2000),
        ( if MaybePSLQ = yes(Rel),
             has_nonzero(Rel),
             is_min_poly(Alpha, Rel)
        then
            Result = yes(Rel)
        else
            Result = try_degree(Alpha, MaxDeg, D + 1)
        )
    ).

:- pred is_min_poly(float::in, list(int)::in) is semidet.

is_min_poly(Alpha, Rel) :-
    D = list.length(Rel) - 1,
    D >= 1,
    % Leading coefficient nonzero
    list.det_last(Rel) \= 0,
    % Coefficients reasonably small
    MaxCoeff = list.foldl(
        ( func(C, Acc) = int.max(int.abs(C), Acc) ), Rel, 0),
    MaxCoeff =< 10000,
    % Residual small
    Residual = float.abs(eval_int_poly(Rel, Alpha)),
    RelResidual = Residual / float.max(1.0, float.float(MaxCoeff)),
    RelResidual < 1.0e-8,
    % Irreducible over Q
    QPoly = mk_poly(list.map(
        ( func(C) = rational.rational(C, 1) ), Rel)),
    Factors = factor_square_free(monic(QPoly)),
    list.length(Factors) = 1.

:- func eval_int_poly(list(int), float) = float.

eval_int_poly(Coeffs, X) = eval_int_poly_acc(Coeffs, X, 0.0, 1.0).

:- func eval_int_poly_acc(list(int), float, float, float) = float.

eval_int_poly_acc([], _, Acc, _) = Acc.
eval_int_poly_acc([C | Cs], X, Acc, XPow) =
    eval_int_poly_acc(Cs, X, Acc + float.float(C) * XPow, XPow * X).

%---------------------------------------------------------------------------%
:- end_module pslq.
%---------------------------------------------------------------------------%
