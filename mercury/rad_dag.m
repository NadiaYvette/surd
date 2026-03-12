%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: rad_dag.m
% Main author: nyc
%
% Explicit DAG (directed acyclic graph) representation for radical expressions.
%
% RadExpr is a tree ADT. In Haskell, lazy evaluation creates DAG-shaped
% expressions through thunk sharing. In Mercury there is no thunk sharing,
% so we instead use explicit structural hash-consing during toDAG
% conversion. Algorithms on RadDAG process each unique node exactly once.
%
%---------------------------------------------------------------------------%

:- module rad_dag.
:- interface.

:- import_module interval.
:- import_module map.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

:- type node_id == int.

:- type rad_node_op
    --->    n_lit(rational)
    ;       n_neg(node_id)
    ;       n_add(node_id, node_id)
    ;       n_mul(node_id, node_id)
    ;       n_inv(node_id)
    ;       n_root(int, node_id)
    ;       n_pow(node_id, int).

:- type rad_dag
    --->    rad_dag(
                dag_nodes :: map(int, rad_node_op),
                dag_root  :: node_id
            ).

    % Convert a RadExpr to an explicit DAG.
    % Uses structural hash-consing: identical sub-expressions
    % map to the same NodeId.
    %
:- func to_dag(rad_expr(rational)) = rad_dag.

    % Convert a DAG back to RadExpr.
    %
:- func from_dag(rad_dag) = rad_expr(rational).

    % Number of unique nodes in the DAG.
    %
:- func dag_size(rad_dag) = int.

    % Depth of the DAG (longest path from root to leaf).
    %
:- func dag_depth(rad_dag) = int.

    % DAG simplification pass: constant folding, power simplification,
    % perfect power extraction. Each node processed once, O(n).
    %
:- func dag_fold_constants(rad_dag) = rad_dag.

    % Evaluate DAG to complex interval.
    %
:- func dag_eval_complex_interval(rad_dag) = complex_interval.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module float.
:- use_module int.
:- import_module integer.
:- import_module list.
:- import_module pair.
:- import_module positive.
:- import_module prime_factors.

%---------------------------------------------------------------------------%
% toDAG: structural hash-consing
%---------------------------------------------------------------------------%

:- type dag_state
    --->    dag_state(
                ds_nodes   :: map(int, rad_node_op),
                ds_next    :: int,
                % Hash-cons map: RadNodeOp → NodeId
                ds_cache   :: map(rad_node_op, int)
            ).

:- func init_state = dag_state.

init_state = dag_state(map.init, 0, map.init).

:- func alloc_node(rad_node_op, dag_state) = {int, dag_state}.

alloc_node(Op, dag_state(Nodes, Next, Cache)) = {Nid, State} :-
    ( if map.search(Cache, Op, Existing) then
        Nid = Existing,
        State = dag_state(Nodes, Next, Cache)
    else
        Nid = Next,
        State = dag_state(
            map.set(Nodes, Next, Op),
            int.'+'(Next, 1),
            map.set(Cache, Op, Next))
    ).

:- func go_dag(rad_expr(rational), dag_state) = {int, dag_state}.

go_dag(Expr, !.S) = {Nid, !:S} :-
    ( if Expr = re_lit(R) then
        {Nid, !:S} = alloc_node(n_lit(R), !.S)
    else if Expr = re_neg(A) then
        {NA, S1} = go_dag(A, !.S),
        {Nid, !:S} = alloc_node(n_neg(NA), S1)
    else if Expr = re_add(A, B) then
        {NA, S1} = go_dag(A, !.S),
        {NB, S2} = go_dag(B, S1),
        {Nid, !:S} = alloc_node(n_add(NA, NB), S2)
    else if Expr = re_mul(A, B) then
        {NA, S1} = go_dag(A, !.S),
        {NB, S2} = go_dag(B, S1),
        {Nid, !:S} = alloc_node(n_mul(NA, NB), S2)
    else if Expr = re_inv(A) then
        {NA, S1} = go_dag(A, !.S),
        {Nid, !:S} = alloc_node(n_inv(NA), S1)
    else if Expr = re_root(N, A) then
        {NA, S1} = go_dag(A, !.S),
        {Nid, !:S} = alloc_node(n_root(N, NA), S1)
    else if Expr = re_pow(A, N) then
        {NA, S1} = go_dag(A, !.S),
        {Nid, !:S} = alloc_node(n_pow(NA, N), S1)
    else
        {Nid, !:S} = alloc_node(n_lit(rational.zero), !.S)
    ).

to_dag(Expr) = rad_dag(Nodes, RootId) :-
    {RootId, dag_state(Nodes, _, _)} = go_dag(Expr, init_state).

%---------------------------------------------------------------------------%
% fromDAG
%---------------------------------------------------------------------------%

from_dag(rad_dag(Nodes, RootId)) = Result :-
    % Process nodes in key order (topological: children have lower IDs)
    Memo = map.foldl(
        ( func(Nid, Op, Acc) =
            map.set(Acc, Nid, build_expr(Acc, Op))
        ),
        Nodes, map.init),
    map.lookup(Memo, RootId, Result).

:- func build_expr(map(int, rad_expr(rational)), rad_node_op)
    = rad_expr(rational).

build_expr(_, n_lit(R)) = re_lit(R).
build_expr(M, n_neg(A)) = re_neg(map.lookup(M, A)).
build_expr(M, n_add(A, B)) = re_add(map.lookup(M, A), map.lookup(M, B)).
build_expr(M, n_mul(A, B)) = re_mul(map.lookup(M, A), map.lookup(M, B)).
build_expr(M, n_inv(A)) = re_inv(map.lookup(M, A)).
build_expr(M, n_root(N, A)) = re_root(N, map.lookup(M, A)).
build_expr(M, n_pow(A, N)) = re_pow(map.lookup(M, A), N).

%---------------------------------------------------------------------------%
% Metrics
%---------------------------------------------------------------------------%

dag_size(rad_dag(Nodes, _)) = map.count(Nodes).

dag_depth(rad_dag(Nodes, RootId)) = Result :-
    Depths = map.foldl(
        ( func(Nid, Op, Acc) =
            map.set(Acc, Nid, compute_depth(Acc, Op))
        ),
        Nodes, map.init),
    map.lookup(Depths, RootId, Result).

:- func compute_depth(map(int, int), rad_node_op) = int.

compute_depth(_, n_lit(_)) = 0.
compute_depth(D, n_neg(A)) = int.'+'(1, map.lookup(D, A)).
compute_depth(D, n_add(A, B)) =
    int.'+'(1, int.max(map.lookup(D, A), map.lookup(D, B))).
compute_depth(D, n_mul(A, B)) =
    int.'+'(1, int.max(map.lookup(D, A), map.lookup(D, B))).
compute_depth(D, n_inv(A)) = int.'+'(1, map.lookup(D, A)).
compute_depth(D, n_root(_, A)) = int.'+'(1, map.lookup(D, A)).
compute_depth(D, n_pow(A, _)) = int.'+'(1, map.lookup(D, A)).

%---------------------------------------------------------------------------%
% dagFoldConstants
%---------------------------------------------------------------------------%

:- type fold_state
    --->    fold_state(
                fs_remap  :: map(int, int),
                fs_nodes  :: map(int, rad_node_op),
                fs_next   :: int
            ).

dag_fold_constants(rad_dag(Nodes, RootId)) = rad_dag(FinalNodes, NewRoot) :-
    FS0 = fold_state(map.init, map.init, 0),
    FinalFS = map.foldl(fold_step, Nodes, FS0),
    FinalNodes = fs_nodes(FinalFS),
    map.lookup(fs_remap(FinalFS), RootId, NewRoot).

:- func fold_step(int, rad_node_op, fold_state) = fold_state.

fold_step(OldId, Op, FS0) = FS :-
    RM = fs_remap(FS0),
    Op1 = remap_op(RM, Op),
    NS = fs_nodes(FS0),
    Nxt = fs_next(FS0),
    SimplResult = simplify(NS, Nxt, Op1),
    ( if SimplResult = left(ExistId) then
        FS = fold_state(map.set(RM, OldId, ExistId), NS, Nxt)
    else if SimplResult = right(NewOps) then
        {FinalId, NS1, Nxt1} = alloc_chain(NS, Nxt, NewOps),
        FS = fold_state(map.set(RM, OldId, FinalId), NS1, Nxt1)
    else
        FS = FS0
    ).

:- type simpl_result
    --->    left(int)
    ;       right(list(rad_node_op)).

:- func remap_op(map(int, int), rad_node_op) = rad_node_op.

remap_op(_, n_lit(K)) = n_lit(K).
remap_op(RM, n_neg(A)) = n_neg(r(RM, A)).
remap_op(RM, n_add(A, B)) = n_add(r(RM, A), r(RM, B)).
remap_op(RM, n_mul(A, B)) = n_mul(r(RM, A), r(RM, B)).
remap_op(RM, n_inv(A)) = n_inv(r(RM, A)).
remap_op(RM, n_root(N, A)) = n_root(N, r(RM, A)).
remap_op(RM, n_pow(A, N)) = n_pow(r(RM, A), N).

:- func r(map(int, int), int) = int.

r(RM, X) = map.lookup(RM, X).

:- func alloc_chain(map(int, rad_node_op), int, list(rad_node_op))
    = {int, map(int, rad_node_op), int}.

alloc_chain(NS, Nxt, []) = {int.'-'(Nxt, 1), NS, Nxt}.
alloc_chain(NS, Nxt, [Op | Ops]) =
    alloc_chain(map.set(NS, Nxt, Op), int.'+'(Nxt, 1), Ops).

:- func simplify(map(int, rad_node_op), int, rad_node_op) = simpl_result.

simplify(NS, _, n_neg(A)) = Result :-
    map.lookup(NS, A, OpA),
    ( if OpA = n_lit(R1) then
        Result = right([n_lit(rational.'-'(rational.zero, R1))])
    else if OpA = n_neg(AP) then
        Result = left(AP)
    else
        Result = right([n_neg(A)])
    ).
simplify(NS, _, n_add(A, B)) = Result :-
    map.lookup(NS, A, OpA),
    map.lookup(NS, B, OpB),
    ( if OpA = n_lit(RA), OpB = n_lit(RB) then
        Result = right([n_lit(rational.'+'(RA, RB))])
    else if OpA = n_lit(R1), R1 = rational.zero then
        Result = left(B)
    else if OpB = n_lit(R2), R2 = rational.zero then
        Result = left(A)
    else
        Result = right([n_add(A, B)])
    ).
simplify(NS, _, n_mul(A, B)) = Result :-
    map.lookup(NS, A, OpA),
    map.lookup(NS, B, OpB),
    ( if OpA = n_lit(RA), OpB = n_lit(RB) then
        Result = right([n_lit(rational.'*'(RA, RB))])
    else if OpA = n_lit(R1), R1 = rational.zero then
        Result = right([n_lit(rational.zero)])
    else if OpB = n_lit(R2), R2 = rational.zero then
        Result = right([n_lit(rational.zero)])
    else if OpA = n_lit(R3), R3 = rational.one then
        Result = left(B)
    else if OpB = n_lit(R4), R4 = rational.one then
        Result = left(A)
    else if OpA = n_lit(R5),
            R5 = rational.'-'(rational.zero, rational.one)
    then
        Result = right([n_neg(B)])
    else if OpB = n_lit(R6),
            R6 = rational.'-'(rational.zero, rational.one)
    then
        Result = right([n_neg(A)])
    else if OpA = n_root(2, RA2), OpB = n_root(2, RB2), RA2 = RB2 then
        Result = left(RA2)
    else
        Result = right([n_mul(A, B)])
    ).
simplify(NS, _, n_inv(A)) = Result :-
    map.lookup(NS, A, OpA),
    ( if OpA = n_lit(R1), R1 \= rational.zero then
        Result = right([n_lit(rational.'/'(rational.one, R1))])
    else if OpA = n_inv(AP) then
        Result = left(AP)
    else
        Result = right([n_inv(A)])
    ).
simplify(NS, Nxt, n_root(N, A)) = Result :-
    map.lookup(NS, A, OpA),
    ( if OpA = n_lit(R1), R1 = rational.zero then
        Result = right([n_lit(rational.zero)])
    else if OpA = n_lit(R2), R2 = rational.one then
        Result = right([n_lit(rational.one)])
    else if OpA = n_lit(R3), rational.'>'(R3, rational.zero) then
        Result = extract_perfect_power_dag(Nxt, N, R3)
    else if OpA = n_root(M, AP) then
        Result = right([n_root(int.'*'(M, N), AP)])
    else if OpA = n_pow(AP, M), M = N then
        Result = left(AP)
    else
        Result = right([n_root(N, A)])
    ).
simplify(NS, _, n_pow(A, N)) = Result :-
    map.lookup(NS, A, OpA),
    ( if N = 0 then
        Result = right([n_lit(rational.one)])
    else if N = 1 then
        Result = left(A)
    else if OpA = n_lit(R1) then
        Result = right([n_lit(rat_int_pow2(R1, N))])
    else if OpA = n_pow(AP, M) then
        Result = right([n_pow(AP, int.'*'(M, N))])
    else if OpA = n_root(NR, AP), NR = N then
        Result = left(AP)
    else
        Result = right([n_pow(A, N)])
    ).
simplify(_, _, n_lit(K)) = right([n_lit(K)]).

:- func rat_int_pow2(rational, int) = rational.

rat_int_pow2(R, N) = Result :-
    ( if int.'<'(N, 0) then
        Result = rational.'/'(rational.one,
            rat_int_pow2(R, int.'-'(0, N)))
    else if N = 0 then
        Result = rational.one
    else if int.even(N) then
        Half = rat_int_pow2(R, int.'//'(N, 2)),
        Result = rational.'*'(Half, Half)
    else
        Result = rational.'*'(R, rat_int_pow2(R, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Perfect power extraction
%---------------------------------------------------------------------------%

:- func extract_perfect_power_dag(int, int, rational) = simpl_result.

extract_perfect_power_dag(Nxt, N, R) = Result :-
    Num = numer(R),
    Den = denom(R),
    {NumOut, NumIn} = extract_nth_power(N, integer.abs(Num)),
    {DenOut, DenIn} = extract_nth_power(N, Den),
    ( if DenIn = integer.one then
        OuterCoeff = rational.'/'(
            rational.from_integer(NumOut),
            rational.from_integer(DenOut)),
        InnerRat = rational.from_integer(NumIn)
    else
        NewInner = integer.'*'(NumIn,
            integer.pow(DenIn, integer.integer(int.'-'(N, 1)))),
        NewOuter = rational.'/'(
            rational.from_integer(NumOut),
            rational.from_integer(integer.'*'(DenOut, DenIn))),
        {NumOut2, NumIn2} = extract_nth_power(N, NewInner),
        OuterCoeff = rational.'*'(NewOuter,
            rational.from_integer(NumOut2)),
        InnerRat = rational.from_integer(NumIn2)
    ),
    ( if OuterCoeff = rational.one, InnerRat = rational.one then
        Result = right([n_lit(rational.one)])
    else if OuterCoeff = rational.one then
        Result = right([n_lit(InnerRat), n_root(N, Nxt)])
    else if InnerRat = rational.one then
        Result = right([n_lit(OuterCoeff)])
    else
        Result = right([
            n_lit(InnerRat),
            n_root(N, Nxt),
            n_lit(OuterCoeff),
            n_mul(int.'+'(Nxt, 2), int.'+'(Nxt, 1))
        ])
    ).

:- func extract_nth_power(int, integer) = {integer, integer}.

extract_nth_power(N, M) = {Extracted, Remainder} :-
    ( if M = integer.zero then
        Extracted = integer.zero,
        Remainder = integer.zero
    else if M = integer.one then
        Extracted = integer.one,
        Remainder = integer.one
    else
        P = det_positive(integer.abs(M)),
        Fs = factorise(P),
        Extracted = list.foldl(
            ( func(PP, Acc) =
                integer.'*'(Acc,
                    integer.pow(pp_prime(PP),
                        integer.integer(int.'//'(pp_exponent(PP), N))))
            ),
            Fs, integer.one),
        Remainder = list.foldl(
            ( func(PP, Acc) =
                integer.'*'(Acc,
                    integer.pow(pp_prime(PP),
                        integer.integer(int.mod(pp_exponent(PP), N))))
            ),
            Fs, integer.one)
    ).

%---------------------------------------------------------------------------%
% dagEvalComplexInterval
%---------------------------------------------------------------------------%

dag_eval_complex_interval(rad_dag(Nodes, RootId)) = Result :-
    Vals = map.foldl(
        ( func(Nid, Op, Acc) =
            map.set(Acc, Nid, eval_ci(Acc, Op))
        ),
        Nodes, map.init),
    map.lookup(Vals, RootId, Result).

:- func eval_ci(map(int, complex_interval), rad_node_op) = complex_interval.

eval_ci(_, n_lit(R)) = ci_from_rational(R).
eval_ci(V, n_neg(A)) = cineg(map.lookup(V, A)).
eval_ci(V, n_add(A, B)) = ciadd(map.lookup(V, A), map.lookup(V, B)).
eval_ci(V, n_mul(A, B)) = cimul(map.lookup(V, A), map.lookup(V, B)).
eval_ci(V, n_inv(A)) = ciinv(map.lookup(V, A)).
eval_ci(V, n_pow(A, N)) = cipow(map.lookup(V, A), N).
eval_ci(V, n_root(N, A)) = Result :-
    CI = map.lookup(V, A),
    RePart = ci_real_part(CI),
    ImPart = ci_imag_part(CI),
    ImLo = iv_lo(ImPart),
    ImHi = iv_hi(ImPart),
    ReLo = iv_lo(RePart),
    ReHi = iv_hi(RePart),
    ( if rational.'>='(ImLo, rational.zero),
         rational.'=<'(ImHi, rational.zero),
         rational.'>='(ReLo, rational.zero)
    then
        % Non-negative real: use real nth root
        Result = ci_from_real(inth(N, RePart))
    else if rational.'>='(ImLo, rational.zero),
            rational.'=<'(ImHi, rational.zero),
            rational.'=<'(ReHi, rational.zero), int.odd(N)
    then
        % Negative real, odd root
        Pos = inth(N, interval(
            rational.'-'(rational.zero, ReHi),
            rational.'-'(rational.zero, ReLo))),
        Result = complex_interval(
            interval(
                rational.'-'(rational.zero, iv_hi(Pos)),
                rational.'-'(rational.zero, iv_lo(Pos))),
            from_rational(rational.zero))
    else if rational.'>='(ImLo, rational.zero),
            rational.'=<'(ImHi, rational.zero),
            rational.'=<'(ReHi, rational.zero), N = 2
    then
        % √(negative) = i·√(|x|)
        Pos = isqrt(interval(
            rational.'-'(rational.zero, ReHi),
            rational.'-'(rational.zero, ReLo))),
        Result = complex_interval(
            from_rational(rational.zero), Pos)
    else
        % General complex root
        Result = cinthroot(N, CI)
    ).

%---------------------------------------------------------------------------%
:- end_module rad_dag.
%---------------------------------------------------------------------------%
