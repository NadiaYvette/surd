%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: permutation.m
% Main author: nyc
%
% Permutation groups on finite sets with Schreier-Sims machinery.
%
% Permutations acting on {0, 1, ..., n-1}, stored as image lists.
% The main group-theoretic functionality is the Schreier-Sims algorithm,
% which computes a base and strong generating set (BSGS) for a
% permutation group G ≤ Sₙ given by generators.
%
%---------------------------------------------------------------------------%

:- module permutation.
:- interface.

:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%
% Permutation type
%---------------------------------------------------------------------------%

:- type perm
    --->    perm(
                perm_n   :: int,
                perm_img :: list(int)
            ).

    % Apply σ to a point: σ(i).
    %
:- func perm_apply(perm, int) = int.

    % Compose two permutations (right-to-left): (σ·τ)(i) = σ(τ(i)).
    %
:- func perm_compose(perm, perm) = perm.

    % Inverse permutation σ⁻¹.
    %
:- func perm_inverse(perm) = perm.

    % Identity permutation on {0, ..., n-1}.
    %
:- func perm_id(int) = perm.

    % Test whether a permutation is the identity.
    %
:- pred perm_is_id(perm::in) is semidet.

    % The order of a permutation (smallest k with σᵏ = id).
    %
:- func perm_order(perm) = int.

    % Cycle decomposition, excluding fixed points.
    %
:- func perm_cycles(perm) = list(list(int)).

    % Sign of a permutation: 1 for even, -1 for odd.
    %
:- func perm_sign(perm) = int.

    % Build a permutation from cycle notation.
    % from_cycles(N, [[0,1,2],[3,4]]) constructs (0 1 2)(3 4) ∈ Sₙ.
    %
:- func from_cycles(int, list(list(int))) = perm.

    % Build a permutation from a full image list.
    %
:- func from_mapping(list(int)) = perm.

    % Orbit of a point under a group given by generators (BFS).
    %
:- func orbit(int, list(perm)) = set(int).

%---------------------------------------------------------------------------%
% BSGS (Base and Strong Generating Set)
%---------------------------------------------------------------------------%

:- type bsgs
    --->    bsgs(
                bsgs_base         :: list(int),
                bsgs_transversals :: list(map(int, perm)),
                bsgs_n            :: int
            ).

    % Compute a BSGS from generators via Schreier-Sims.
    %
:- func schreier_sims(int, list(perm)) = bsgs.

    % Group order: |G| = Π |Δᵢ|.
    %
:- func group_order(bsgs) = integer.

    % Test whether a permutation belongs to the group.
    %
:- pred group_contains(bsgs::in, perm::in) is semidet.

    % Enumerate all elements of the group.
    %
:- func group_elements(bsgs) = list(perm).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module pair.

%---------------------------------------------------------------------------%
% Basic permutation operations
%---------------------------------------------------------------------------%

perm_apply(perm(_, Img), I) = list.det_index0(Img, I).

perm_compose(perm(N, Img1), perm(_, Img2)) =
    perm(N, list.map(
        ( func(I) = list.det_index0(Img1, list.det_index0(Img2, I)) ),
        0 `..` (N - 1))).

perm_inverse(perm(N, Img)) = perm(N, InvImg) :-
    InvMap = list.foldl(
        ( func(I, M) =
            map.set(M, list.det_index0(Img, I), I) ),
        0 `..` (N - 1), map.init),
    InvImg = list.map(
        ( func(I) = map.lookup(InvMap, I) ),
        0 `..` (N - 1)).

perm_id(N) = perm(N, 0 `..` (N - 1)).

perm_is_id(perm(N, Img)) :-
    Img = 0 `..` (N - 1).

perm_order(P) = Result :-
    ( if perm_is_id(P) then
        Result = 1
    else
        Cycles = perm_cycles(P),
        Lengths = list.map(list.length, Cycles),
        Result = list.foldl(lcm, Lengths, 1)
    ).

:- func lcm(int, int) = int.

lcm(A, B) = int.abs(A * B) // gcd_int(A, B).

:- func gcd_int(int, int) = int.

gcd_int(A, B) = Result :-
    AA = int.abs(A), BB = int.abs(B),
    ( if BB = 0 then Result = AA
    else Result = gcd_int(BB, AA mod BB)
    ).

perm_cycles(perm(N, Img)) = cycles_go(Img, N, set.init, 0).

:- func cycles_go(list(int), int, set(int), int) = list(list(int)).

cycles_go(Img, N, Visited, I) = Result :-
    ( if I >= N then
        Result = []
    else if set.member(I, Visited) then
        Result = cycles_go(Img, N, Visited, I + 1)
    else if list.det_index0(Img, I) = I then
        Result = cycles_go(Img, N, set.insert(Visited, I), I + 1)
    else
        Cyc = trace_cycle(Img, I, list.det_index0(Img, I), [I]),
        NewVisited = list.foldl(
            ( func(X, S) = set.insert(S, X) ), Cyc, Visited),
        Result = [Cyc | cycles_go(Img, N, NewVisited, I + 1)]
    ).

:- func trace_cycle(list(int), int, int, list(int)) = list(int).

trace_cycle(Img, Start, Cur, Acc) = Result :-
    ( if Cur = Start then
        Result = list.reverse(Acc)
    else
        Next = list.det_index0(Img, Cur),
        Result = trace_cycle(Img, Start, Next, [Cur | Acc])
    ).

perm_sign(P) = Result :-
    K = list.foldl(
        ( func(Cyc, Acc) = Acc + list.length(Cyc) - 1 ),
        perm_cycles(P), 0),
    ( if int.even(K) then Result = 1 else Result = -1 ).

from_cycles(N, Cycles) = perm(N, FinalImg) :-
    BaseMap = list.foldl(
        ( func(I, M) = map.set(M, I, I) ),
        0 `..` (N - 1), map.init),
    FinalMap = list.foldl(apply_cycle, Cycles, BaseMap),
    FinalImg = list.map(
        ( func(I) = map.lookup(FinalMap, I) ),
        0 `..` (N - 1)).

:- func apply_cycle(list(int), map(int, int)) = map(int, int).

apply_cycle(Cyc, M) = Result :-
    ( if Cyc = [C0 | _], list.length(Cyc) > 1 then
        Pairs = cycle_pairs(Cyc, C0),
        Result = list.foldl(
            ( func({A, B}, Acc) = map.set(Acc, A, B) ),
            Pairs, M)
    else
        Result = M
    ).

:- func cycle_pairs(list(int), int) = list({int, int}).

cycle_pairs(Cyc, C0) = Result :-
    ( if Cyc = [A, B | Rest] then
        Result = [{A, B} | cycle_pairs([B | Rest], C0)]
    else if Cyc = [A] then
        Result = [{A, C0}]
    else
        Result = []
    ).

from_mapping(Img) = perm(list.length(Img), Img).

orbit(Pt, Gens) = bfs_orbit(Gens, set.make_singleton_set(Pt), [Pt]).

:- func bfs_orbit(list(perm), set(int), list(int)) = set(int).

bfs_orbit(_, Visited, []) = Visited.
bfs_orbit(Gens, Visited, [X | Queue]) = Result :-
    {NewVisited, NewPts} = list.foldl(
        ( func(G, {V, Ns}) = {V1, Ns1} :-
            Y = perm_apply(G, X),
            ( if set.member(Y, V) then
                V1 = V, Ns1 = Ns
            else
                V1 = set.insert(V, Y), Ns1 = [Y | Ns]
            )
        ),
        Gens, {Visited, []}),
    Result = bfs_orbit(Gens, NewVisited, Queue ++ NewPts).

%---------------------------------------------------------------------------%
% Schreier-Sims algorithm
%---------------------------------------------------------------------------%

schreier_sims(N, Gens) = Result :-
    NonTrivial = list.filter(
        ( pred(G::in) is semidet :- not perm_is_id(G) ), Gens),
    ( if NonTrivial = [] then
        Result = bsgs([], [], N)
    else
        Initial = list.foldl(sift_insert(N, Gens), NonTrivial,
            bsgs([], [], N)),
        Result = saturate(N, Gens, Initial)
    ).

:- func sift_insert(int, list(perm), perm, bsgs) = bsgs.

sift_insert(N, Gens, G, BSGS) = Result :-
    R = sift(BSGS, G),
    ( if perm_is_id(R) then
        Result = BSGS
    else
        Result = extend_bsgs(N, Gens, BSGS, R)
    ).

:- func sift(bsgs, perm) = perm.

sift(bsgs(Base, Trans, _), G) = sift_go(Base, Trans, G, 0).

:- func sift_go(list(int), list(map(int, perm)), perm, int) = perm.

sift_go(Base, Trans, H, I) = Result :-
    ( if I >= list.length(Base) then
        Result = H
    else
        B = list.det_index0(Base, I),
        Img = perm_apply(H, B),
        T = list.det_index0(Trans, I),
        ( if map.search(T, Img, U) then
            Result = sift_go(Base, Trans, perm_compose(perm_inverse(U), H),
                I + 1)
        else
            Result = H
        )
    ).

:- func extend_bsgs(int, list(perm), bsgs, perm) = bsgs.

extend_bsgs(N, _Gens, bsgs(Base, Trans, _), G) = Result :-
    FixedLevels = count_fixed(G, Base, 0),
    ( if FixedLevels >= list.length(Base) then
        % g fixes all base points; add new level
        Pt = find_moved_point(G, N, 0),
        NewTrans = build_transversal(N, Pt, [G]),
        Result = bsgs(Base ++ [Pt], Trans ++ [NewTrans], N)
    else
        % Rebuild transversal at FixedLevels with g added
        OldTrans = list.det_index0(Trans, FixedLevels),
        ExistingReps = map.values(OldTrans),
        BasePt = list.det_index0(Base, FixedLevels),
        NewTrans = build_transversal(N, BasePt, [G | ExistingReps]),
        Result = bsgs(Base,
            list.det_replace_nth(Trans, FixedLevels + 1, NewTrans), N)
    ).

:- func count_fixed(perm, list(int), int) = int.

count_fixed(G, Base, I) = Result :-
    ( if I >= list.length(Base) then
        Result = I
    else
        B = list.det_index0(Base, I),
        ( if perm_apply(G, B) = B then
            Result = count_fixed(G, Base, I + 1)
        else
            Result = I
        )
    ).

:- func find_moved_point(perm, int, int) = int.

find_moved_point(G, N, I) = Result :-
    ( if I >= N then
        Result = 0
    else if perm_apply(G, I) \= I then
        Result = I
    else
        Result = find_moved_point(G, N, I + 1)
    ).

:- func build_transversal(int, int, list(perm)) = map(int, perm).

build_transversal(N, Pt, Gs) =
    bfs_trans(Gs, N, map.from_assoc_list([Pt - perm_id(N)]), [Pt]).

:- func bfs_trans(list(perm), int, map(int, perm), list(int))
    = map(int, perm).

bfs_trans(_, _, Visited, []) = Visited.
bfs_trans(Gs, N, Visited, [X | Queue]) = Result :-
    UX = map.lookup(Visited, X),
    {NewVisited, NewPts} = list.foldl(
        ( func(G, {V, Ns}) = {V1, Ns1} :-
            Y = perm_apply(G, X),
            ( if map.contains(V, Y) then
                V1 = V, Ns1 = Ns
            else
                V1 = map.set(V, Y, perm_compose(G, UX)),
                Ns1 = [Y | Ns]
            )
        ),
        Gs, {Visited, []}),
    Result = bfs_trans(Gs, N, NewVisited, Queue ++ NewPts).

:- func saturate(int, list(perm), bsgs) = bsgs.

saturate(N, Gens, BSGS) = Result :-
    SGens = schreier_gens(N, Gens, BSGS),
    BSGS1 = list.foldl(sift_insert(N, Gens), SGens, BSGS),
    ( if bsgs_transversals(BSGS1) = bsgs_transversals(BSGS) then
        Result = BSGS1
    else
        Result = saturate(N, Gens, BSGS1)
    ).

:- func schreier_gens(int, list(perm), bsgs) = list(perm).

schreier_gens(N, Gens, bsgs(Base, Trans, _)) = SGens :-
    SGens = list.condense(list.map(
        ( func(I) = LevelGens :-
            Transversal = list.det_index0(Trans, I),
            B = list.det_index0(Base, I),
            Reps = map.values(Transversal),
            LevelGens = list.filter_map(
                ( func(US) = SG is semidet :-
                    US = {U, S},
                    Beta = perm_apply(U, B),
                    SBeta = perm_apply(S, Beta),
                    USBeta = map_lookup_default(Transversal, SBeta,
                        perm_id(N)),
                    SG = perm_compose(perm_inverse(USBeta),
                        perm_compose(S, U)),
                    not perm_is_id(SG)
                ),
                cross_product(Reps, Gens))
        ),
        0 `..` (list.length(Trans) - 1))).

:- func cross_product(list(A), list(B)) = list({A, B}).

cross_product(As, Bs) = list.condense(
    list.map(( func(A) = list.map(( func(B) = {A, B} ), Bs) ), As)).

:- func map_lookup_default(map(K, V), K, V) = V.

map_lookup_default(M, K, Default) = Result :-
    ( if map.search(M, K, V) then Result = V
    else Result = Default ).

%---------------------------------------------------------------------------%
% Group operations
%---------------------------------------------------------------------------%

group_order(bsgs(_, Trans, _)) =
    list.foldl(
        ( func(T, Acc) = integer.'*'(Acc, integer(map.count(T))) ),
        Trans, integer.one).

group_contains(bsgs(Base, Trans, _), G) :-
    group_contains_go(Base, Trans, G, 0).

:- pred group_contains_go(list(int)::in, list(map(int, perm))::in,
    perm::in, int::in) is semidet.

group_contains_go(Base, Trans, H, I) :-
    ( if perm_is_id(H) then
        true
    else
        I < list.length(Base),
        B = list.det_index0(Base, I),
        Img = perm_apply(H, B),
        T = list.det_index0(Trans, I),
        map.search(T, Img, U),
        group_contains_go(Base, Trans,
            perm_compose(perm_inverse(U), H), I + 1)
    ).

group_elements(bsgs(_, Trans, N)) = Result :-
    ( if Trans = [_ | _] then
        RepLists = list.map(map.values, Trans),
        Combos = cartesian(RepLists),
        Result = list.map(
            ( func(Combo) = list.foldl(perm_compose, Combo, perm_id(N)) ),
            Combos)
    else
        Result = [perm_id(N)]
    ).

:- func cartesian(list(list(A))) = list(list(A)).

cartesian([]) = [[]].
cartesian([Xs | Rest]) = Result :-
    Tails = cartesian(Rest),
    Result = list.condense(
        list.map(( func(X) =
            list.map(( func(T) = [X | T] ), Tails)
        ), Xs)).

%---------------------------------------------------------------------------%
:- end_module permutation.
%---------------------------------------------------------------------------%
