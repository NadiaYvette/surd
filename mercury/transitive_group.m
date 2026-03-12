%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: transitive_group.m
% Main author: nyc
%
% Database of transitive subgroups of Sₙ for small n.
%
% Transitive subgroups of Sₙ (up to conjugacy) classify the possible
% Galois groups of irreducible degree-n polynomials over Q.
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
    % Currently covers degree 5 only.
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

%---------------------------------------------------------------------------%

trans_groups_of_degree(N) = Result :-
    ( if N = 5 then
        Result = degree5_groups
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
        else
            Result = no
        )
    ).

%---------------------------------------------------------------------------%
% Degree 5 transitive groups
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
:- end_module transitive_group.
%---------------------------------------------------------------------------%
