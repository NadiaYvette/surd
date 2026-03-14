%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: transitive_group.m
% Main author: nyc
%
% Database and runtime computation of transitive subgroups of Sₙ.
%
% Transitive subgroups of Sₙ (up to conjugacy) classify the possible
% Galois groups of irreducible degree-n polynomials over Q.
%
% For prime p, the solvable transitive subgroups of Sₚ are exactly
% Z/p ⋊ H where H ≤ (Z/pZ)*, one per divisor d of p-1. These are
% computed at runtime from the structure of AGL(1,p).
%
% Groups are numbered following the Butler-McKay convention (1983),
% which is also the standard in GAP, Magma, and the LMFDB.
%
%---------------------------------------------------------------------------%

:- module transitive_group.
:- interface.

:- import_module bool.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module permutation.

%---------------------------------------------------------------------------%

:- type transitive_group
    --->    transitive_group(
                tg_name                :: string,
                tg_degree              :: int,
                tg_order               :: integer,
                tg_generators          :: list(perm),
                tg_solvable            :: bool,
                tg_maximal_supergroups :: list(int),
                tg_composition_factors :: list(int)
            ).

    % All transitive subgroups of Sₙ for the given degree.
    % Covers degree 5 (hard-coded) and all prime degrees >= 3
    % (computed at runtime from AGL(1,p) structure).
    %
:- func trans_groups_of_degree(int) = list(transitive_group).

    % Find transitive group(s) of given degree and order.
    %
:- func trans_group_by_order(int, integer) = list(transitive_group).

    % Test whether a transitive group is solvable.
    %
:- func is_solvable(transitive_group) = bool.

    % For a solvable group, return the composition series as a list
    % of generating sets, descending from G to {1}.
    % Returns no for non-solvable groups.
    %
:- func composition_series(transitive_group)
    = maybe(list(list(perm))).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module positive.
:- import_module prime_factors.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

trans_groups_of_degree(N) = Result :-
    ( if N = 5 then
        Result = degree5_groups
    else if int.'>='(N, 3), is_prime(integer(N)) then
        Result = trans_groups_of_prime_rt(N)
    else
        Result = []
    ).

trans_group_by_order(Deg, Ord) =
    list.filter(
        ( pred(G::in) is semidet :- tg_order(G) = Ord ),
        trans_groups_of_degree(Deg)).

is_solvable(G) = tg_solvable(G).

composition_series(TG) = Result :-
    ( if tg_solvable(TG) = no then
        Result = no
    else
        Name = tg_name(TG),
        ( if Name = "C5" then
            Result = yes([tg_generators(TG), []])
        else if Name = "D5" then
            Result = yes([
                tg_generators(TG),
                [from_cycles(5, [[0, 1, 2, 3, 4]])],
                []])
        else if Name = "F20" then
            Result = yes([
                tg_generators(TG),
                [from_cycles(5, [[0, 1, 2, 3, 4]]),
                 from_cycles(5, [[1, 4], [2, 3]])],
                [from_cycles(5, [[0, 1, 2, 3, 4]])],
                []])
        else if is_prime(integer(tg_degree(TG))) then
            Result = composition_series_prime(TG)
        else
            Result = no
        )
    ).

%---------------------------------------------------------------------------%
% Degree 5 transitive groups (hard-coded fast path)
%---------------------------------------------------------------------------%

:- func degree5_groups = list(transitive_group).

degree5_groups = [
    % T1: C5 = ⟨(0 1 2 3 4)⟩
    transitive_group(
        "C5", 5, integer(5),
        [from_cycles(5, [[0, 1, 2, 3, 4]])],
        yes,
        [1],
        [5]),

    % T2: D5 = ⟨(0 1 2 3 4), (1 4)(2 3)⟩
    transitive_group(
        "D5", 5, integer(10),
        [from_cycles(5, [[0, 1, 2, 3, 4]]),
         from_cycles(5, [[1, 4], [2, 3]])],
        yes,
        [2],
        [5, 2]),

    % T3: F20 = ⟨(0 1 2 3 4), (1 2 4 3)⟩
    transitive_group(
        "F20", 5, integer(20),
        [from_cycles(5, [[0, 1, 2, 3, 4]]),
         from_cycles(5, [[1, 2, 4, 3]])],
        yes,
        [3, 4],
        [5, 2, 2]),

    % T4: A5 = ⟨(0 1 2 3 4), (0 1 2)⟩
    transitive_group(
        "A5", 5, integer(60),
        [from_cycles(5, [[0, 1, 2, 3, 4]]),
         from_cycles(5, [[0, 1, 2]])],
        no,
        [4],
        []),

    % T5: S5 = ⟨(0 1 2 3 4), (0 1)⟩
    transitive_group(
        "S5", 5, integer(120),
        [from_cycles(5, [[0, 1, 2, 3, 4]]),
         from_cycles(5, [[0, 1]])],
        no,
        [],
        [])
].

%---------------------------------------------------------------------------%
% Runtime AGL(1,p) computation for prime degrees
%---------------------------------------------------------------------------%

    % Modular exponentiation: mod_exp(B, E, M) = B^E mod M.
    %
:- func mod_exp_rt(integer, integer, integer) = integer.

mod_exp_rt(B, E, M) = Result :-
    ( if integer.is_zero(E) then
        Result = integer.one
    else if integer.'mod'(E, integer(2)) = integer.zero then
        Half = mod_exp_rt(B, E // integer(2), M),
        Result = integer.'mod'(Half * Half, M)
    else
        Result = integer.'mod'(B * mod_exp_rt(B, E - integer.one, M), M)
    ).

    % Find a primitive root modulo a prime p via trial.
    %
:- func primitive_root_rt(integer) = integer.

primitive_root_rt(P) = Result :-
    Phi = P - integer.one,
    Factors = prime_factors(det_positive(Phi)),
    ( if find_primitive_root(P, Phi, Factors, integer(2), G) then
        Result = G
    else
        Result = integer(2)  % should not happen for primes
    ).

:- pred find_primitive_root(integer::in, integer::in, list(integer)::in,
    integer::in, integer::out) is semidet.

find_primitive_root(P, Phi, Factors, G, Result) :-
    integer.'<'(G, P),
    ( if is_prim_root(G, P, Phi, Factors) then
        Result = G
    else
        find_primitive_root(P, Phi, Factors, G + integer.one, Result)
    ).

:- pred is_prim_root(integer::in, integer::in, integer::in,
    list(integer)::in) is semidet.

is_prim_root(_, _, _, []).
is_prim_root(G, P, Phi, [Q | Qs]) :-
    mod_exp_rt(G, Phi // Q, P) \= integer.one,
    is_prim_root(G, P, Phi, Qs).

    % Sorted positive divisors of n.
    %
:- func divisors_rt(integer) = list(integer).

divisors_rt(N) = Result :-
    integer_sqrt_rt(N, Sq),
    collect_divisors(N, integer.one, Sq, [], Divs),
    list.sort_and_remove_dups(Divs, Result).

:- pred collect_divisors(integer::in, integer::in, integer::in,
    list(integer)::in, list(integer)::out) is det.

collect_divisors(N, D, Sq, Acc, Result) :-
    ( if integer.'>'(D, Sq) then
        Result = Acc
    else if integer.'mod'(N, D) = integer.zero then
        Q = N // D,
        ( if D = Q then
            collect_divisors(N, D + integer.one, Sq, [D | Acc], Result)
        else
            collect_divisors(N, D + integer.one, Sq, [D, Q | Acc], Result)
        )
    else
        collect_divisors(N, D + integer.one, Sq, Acc, Result)
    ).

:- pred integer_sqrt_rt(integer::in, integer::out) is det.

integer_sqrt_rt(N, Result) :-
    ( if integer.'=<'(N, integer.zero) then
        Result = integer.zero
    else
        % Newton's method from a rough starting point
        NF = integer.det_to_int(N),
        Approx0 = integer(float.truncate_to_int(
            math.sqrt(float.float(NF)))),
        Start = ( if integer.'<'(Approx0, integer.one) then
                      integer.one
                  else
                      Approx0 + integer.one ),
        isqrt_newton_rt(N, Start, Result)
    ).

:- pred isqrt_newton_rt(integer::in, integer::in, integer::out) is det.

isqrt_newton_rt(N, X, Result) :-
    XP = (X + N // X) // integer(2),
    ( if integer.'>='(XP, X) then
        Result = X
    else
        isqrt_newton_rt(N, XP, Result)
    ).

:- use_module float.
:- use_module math.

    % Prime factorisation as a flat list with multiplicity.
    % E.g. prime_factors_with_mult(12) = [2, 2, 3].
    %
:- func prime_factors_with_mult_rt(integer) = list(integer).

prime_factors_with_mult_rt(N) = Result :-
    ( if N = integer.one then
        Result = []
    else
        Fs = factorise(det_positive(N)),
        Result = list.condense(list.map(
            ( func(PP) = list.duplicate(pp_exponent(PP), pp_prime(PP)) ),
            Fs))
    ).

    % Build all transitive subgroups of Sₚ for a prime p.
    %
    % Solvable subgroups: Z/p ⋊ H for each divisor d of p-1.
    % Non-solvable: Aₚ and Sₚ.
    % Maximal supergroups are computed from divisibility.
    %
:- func trans_groups_of_prime_rt(int) = list(transitive_group).

trans_groups_of_prime_rt(N) = Result :-
    P = integer(N),
    G = primitive_root_rt(P),
    Ds = divisors_rt(P - integer.one),
    SolvableGroups = list.map(mk_affine_group(N, P, G), Ds),
    % Aₚ
    Factorial = factorial_integer(P),
    Ap = transitive_group(
        "A" ++ string.int_to_string(N),
        N,
        Factorial // integer(2),
        [from_cycles(N, [0 `..` int.'-'(N, 1)]),
         from_cycles(N, [[0, 1, 2]])],
        ( if int.'<'(N, 5) then yes else no ),
        [],
        []),
    % Sₚ
    Sp = transitive_group(
        "S" ++ string.int_to_string(N),
        N,
        Factorial,
        [from_cycles(N, [0 `..` int.'-'(N, 1)]),
         from_cycles(N, [[0, 1]])],
        ( if int.'<'(N, 4) then yes else no ),
        [],
        []),
    AllGroups0 = SolvableGroups ++ [Ap, Sp],
    list.sort(order_by_order, AllGroups0, AllGroupsSorted),
    % Assign maximal supergroups
    indexed_list(AllGroupsSorted, 0, Indexed),
    Result = list.map(assign_supergroups(Indexed), Indexed).

:- func factorial_integer(integer) = integer.

factorial_integer(N) = Result :-
    ( if integer.'=<'(N, integer.one) then
        Result = integer.one
    else
        Result = N * factorial_integer(N - integer.one)
    ).

:- pred order_by_order(transitive_group::in, transitive_group::in,
    comparison_result::out) is det.

order_by_order(A, B, Result) :-
    compare(Result, tg_order(A), tg_order(B)).

:- func mk_affine_group(int, integer, integer, integer) = transitive_group.

mk_affine_group(N, P, G, D) = Result :-
    Trans = from_mapping(list.map(
        ( func(I) = integer.det_to_int(
            integer.'mod'(integer(I) + integer.one, P)) ),
        0 `..` int.'-'(N, 1))),
    ScaleFactor = mod_exp_rt(G, (P - integer.one) // D, P),
    Scale = from_mapping(list.map(
        ( func(I) = integer.det_to_int(
            integer.'mod'(ScaleFactor * integer(I), P)) ),
        0 `..` int.'-'(N, 1))),
    Gens = ( if D = integer.one then [Trans] else [Trans, Scale] ),
    Name = affine_group_name(P, D),
    DFactors = prime_factors_with_mult_rt(D),
    CompFactors = list.map(integer.det_to_int, DFactors ++ [P]),
    Result = transitive_group(
        Name, N, P * D, Gens, yes, [], CompFactors).

:- func affine_group_name(integer, integer) = string.

affine_group_name(P, D) = Name :-
    PS = integer.to_string(P),
    DS = integer.to_string(D),
    ( if D = integer.one then
        Name = "Z" ++ PS
    else if D = integer(2) then
        Name = "D" ++ PS
    else if D = P - integer.one then
        Name = "AGL(1," ++ PS ++ ")"
    else
        Name = "Z" ++ PS ++ ":Z" ++ DS
    ).

:- pred indexed_list(list(T)::in, int::in,
    list({int, T})::out) is det.

indexed_list([], _, []).
indexed_list([X | Xs], I, [{I, X} | Rest]) :-
    indexed_list(Xs, int.'+'(I, 1), Rest).

:- func assign_supergroups(list({int, transitive_group}),
    {int, transitive_group}) = transitive_group.

assign_supergroups(AllIndexed, {MyIdx, TG}) = Result :-
    MyOrder = tg_order(TG),
    % Groups that could contain us (larger order, our order divides theirs)
    Candidates = list.filter(
        ( pred({I, CG}::in) is semidet :-
            I \= MyIdx,
            integer.'>'(tg_order(CG), MyOrder),
            integer.'mod'(tg_order(CG), MyOrder) = integer.zero ),
        AllIndexed),
    % A supergroup is maximal if no candidate has order strictly between
    MaxSupers = list.filter_map(
        ( func({I, SuperG}) = I is semidet :-
            SuperOrder = tg_order(SuperG),
            not has_intermediate(Candidates, MyOrder, SuperOrder)
        ),
        Candidates),
    Result = TG ^ tg_maximal_supergroups := MaxSupers.

    % Check whether any group in the candidate list has order strictly
    % between MyOrder and SuperOrder, dividing both.
    %
:- pred has_intermediate(list({int, transitive_group})::in,
    integer::in, integer::in) is semidet.

has_intermediate([{_, MidG} | Rest], MyOrder, SuperOrder) :-
    MidOrder = tg_order(MidG),
    ( if
        integer.'>'(MidOrder, MyOrder),
        integer.'<'(MidOrder, SuperOrder),
        integer.'mod'(SuperOrder, MidOrder) = integer.zero,
        integer.'mod'(MidOrder, MyOrder) = integer.zero
    then
        true
    else
        has_intermediate(Rest, MyOrder, SuperOrder)
    ).

%---------------------------------------------------------------------------%
% Composition series for prime-degree affine subgroups
%---------------------------------------------------------------------------%

    % Composition series for Z/p ⋊ H where H has order d.
    % Descends through subgroups of H by removing one prime factor at a time.
    %
:- func composition_series_prime(transitive_group)
    = maybe(list(list(perm))).

composition_series_prime(TG) = Result :-
    P = integer(tg_degree(TG)),
    N = tg_degree(TG),
    G = primitive_root_rt(P),
    D = tg_order(TG) // P,
    DFactors = prime_factors_with_mult_rt(D),
    % Chain of divisors: d, d/q1, d/(q1*q2), ..., 1
    DChain = scan_div(D, DFactors),
    Trans = from_cycles(N, [0 `..` int.'-'(N, 1)]),
    GensList = list.map(mk_gens_for_d(N, P, G, Trans), DChain),
    Result = yes(GensList ++ [[]]).

:- func scan_div(integer, list(integer)) = list(integer).

scan_div(Acc, []) = [Acc].
scan_div(Acc, [Q | Qs]) = [Acc | scan_div(Acc // Q, Qs)].

:- func mk_gens_for_d(int, integer, integer, perm, integer) = list(perm).

mk_gens_for_d(N, P, G, Trans, D) = Gens :-
    ( if integer.'=<'(D, integer.one) then
        Gens = [Trans]
    else
        SF = mod_exp_rt(G, (P - integer.one) // D, P),
        Scale = from_mapping(list.map(
            ( func(I) = integer.det_to_int(
                integer.'mod'(SF * integer(I), P)) ),
            0 `..` int.'-'(N, 1))),
        Gens = [Trans, Scale]
    ).

%---------------------------------------------------------------------------%
:- end_module transitive_group.
%---------------------------------------------------------------------------%
