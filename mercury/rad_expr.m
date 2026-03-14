%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: rad_expr.m
% Main author: nyc
%
% Core AST for radical expressions, parameterised by the coefficient type.
% Includes construction helpers and structural queries.
%
% Normalization is explicit: these constructors are "dumb".
% Call functions in rad_normalize for simplification.
%
%---------------------------------------------------------------------------%

:- module rad_expr.
:- interface.

:- import_module list.
:- import_module rational.

%---------------------------------------------------------------------------%
% Core type
%---------------------------------------------------------------------------%

    % A radical expression over a coefficient field K.
    %
:- type rad_expr(K)
    --->    re_lit(K)
    ;       re_neg(rad_expr(K))
    ;       re_add(rad_expr(K), rad_expr(K))
    ;       re_mul(rad_expr(K), rad_expr(K))
    ;       re_inv(rad_expr(K))
    ;       re_root(int, rad_expr(K))   % Root n x: principal nth root
    ;       re_pow(rad_expr(K), int).   % Pow x n: integer power

%---------------------------------------------------------------------------%
% Construction helpers
%---------------------------------------------------------------------------%

    % Subtraction: re_sub(A, B) = re_add(A, re_neg(B)).
    %
:- func re_sub(rad_expr(K), rad_expr(K)) = rad_expr(K).

    % Division: re_div(A, B) = re_mul(A, re_inv(B)).
    %
:- func re_div(rad_expr(K), rad_expr(K)) = rad_expr(K).

    % Square root shorthand: re_sqrt(X) = re_root(2, X).
    %
:- func re_sqrt(rad_expr(K)) = rad_expr(K).

    % Lift a rational number into a radical expression.
    %
:- func rat_e(rational) = rad_expr(rational).

    % Lift a machine integer into a radical expression over rationals.
    %
:- func int_e(int) = rad_expr(rational).

%---------------------------------------------------------------------------%
% Structural queries
%---------------------------------------------------------------------------%

    % Nesting depth of the expression tree.
    %
:- func re_depth(rad_expr(K)) = int.

    % Number of nodes in the expression tree.
    %
:- func re_size(rad_expr(K)) = int.

    % Check if all coefficients satisfy a predicate.
    %
:- pred re_free_of(pred(K), rad_expr(K)).
:- mode re_free_of(in(pred(in) is semidet), in) is semidet.

    % Map a function over the coefficients.
    %
:- func re_map_coeffs((func(K1) = K2), rad_expr(K1)) = rad_expr(K2).

%---------------------------------------------------------------------------%
% Radical collection and dependency ordering
%---------------------------------------------------------------------------%

    % Collect distinct (rootIndex, radicand) pairs from an expression.
    %
:- func collect_radicals(rad_expr(K)) = list({int, rad_expr(K)}).

    % Topologically sort radicals so that radicals with rational radicands
    % come first, followed by radicals whose radicands depend only on
    % earlier radicals.
    %
:- func topo_sort_radicals(list({int, rad_expr(K)}))
    = list({int, rad_expr(K)}).

    % Check whether all Root subexpressions in a radicand are present
    % in the resolved set.
    %
:- pred all_roots_resolved(list({int, rad_expr(K)})::in,
    rad_expr(K)::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.

%---------------------------------------------------------------------------%
% Construction helpers
%---------------------------------------------------------------------------%

re_sub(A, B) = re_add(A, re_neg(B)).
re_div(A, B) = re_mul(A, re_inv(B)).
re_sqrt(A) = re_root(2, A).
rat_e(R) = re_lit(R).
int_e(N) = re_lit(rational(N, 1)).

%---------------------------------------------------------------------------%
% Structural queries
%---------------------------------------------------------------------------%

re_depth(re_lit(_)) = 0.
re_depth(re_neg(A)) = re_depth(A).
re_depth(re_add(A, B)) = int.'+'(1, int.max(re_depth(A), re_depth(B))).
re_depth(re_mul(A, B)) = int.'+'(1, int.max(re_depth(A), re_depth(B))).
re_depth(re_inv(A)) = int.'+'(1, re_depth(A)).
re_depth(re_root(_, A)) = int.'+'(1, re_depth(A)).
re_depth(re_pow(A, _)) = int.'+'(1, re_depth(A)).

re_size(re_lit(_)) = 1.
re_size(re_neg(A)) = int.'+'(1, re_size(A)).
re_size(re_add(A, B)) = int.'+'(1, int.'+'(re_size(A), re_size(B))).
re_size(re_mul(A, B)) = int.'+'(1, int.'+'(re_size(A), re_size(B))).
re_size(re_inv(A)) = int.'+'(1, re_size(A)).
re_size(re_root(_, A)) = int.'+'(1, re_size(A)).
re_size(re_pow(A, _)) = int.'+'(1, re_size(A)).

re_free_of(P, re_lit(K)) :- P(K).
re_free_of(P, re_neg(A)) :- re_free_of(P, A).
re_free_of(P, re_add(A, B)) :- re_free_of(P, A), re_free_of(P, B).
re_free_of(P, re_mul(A, B)) :- re_free_of(P, A), re_free_of(P, B).
re_free_of(P, re_inv(A)) :- re_free_of(P, A).
re_free_of(P, re_root(_, A)) :- re_free_of(P, A).
re_free_of(P, re_pow(A, _)) :- re_free_of(P, A).

re_map_coeffs(F, Expr) = Result :-
    (
        Expr = re_lit(K),
        Result = re_lit(F(K))
    ;
        Expr = re_neg(A),
        Result = re_neg(re_map_coeffs(F, A))
    ;
        Expr = re_add(A, B),
        Result = re_add(re_map_coeffs(F, A), re_map_coeffs(F, B))
    ;
        Expr = re_mul(A, B),
        Result = re_mul(re_map_coeffs(F, A), re_map_coeffs(F, B))
    ;
        Expr = re_inv(A),
        Result = re_inv(re_map_coeffs(F, A))
    ;
        Expr = re_root(N, A),
        Result = re_root(N, re_map_coeffs(F, A))
    ;
        Expr = re_pow(A, N),
        Result = re_pow(re_map_coeffs(F, A), N)
    ).

%---------------------------------------------------------------------------%
% Radical collection and dependency ordering
%---------------------------------------------------------------------------%

collect_radicals(Expr) = nub_pairs(go(Expr)) :-
    true.

:- func go(rad_expr(K)) = list({int, rad_expr(K)}).

go(re_lit(_)) = [].
go(re_neg(A)) = go(A).
go(re_add(A, B)) = go(A) ++ go(B).
go(re_mul(A, B)) = go(A) ++ go(B).
go(re_inv(A)) = go(A).
go(re_pow(A, _)) = go(A).
go(re_root(N, A)) = go(A) ++ [{N, A}].

:- func nub_pairs(list({int, rad_expr(K)})) = list({int, rad_expr(K)}).

nub_pairs([]) = [].
nub_pairs([X | Xs]) = [X | nub_pairs(list.filter(
    ( pred(Y::in) is semidet :- not X = Y ), Xs))].

topo_sort_radicals(Radicals) = topo_loop([], Radicals).

:- func topo_loop(list({int, rad_expr(K)}),
    list({int, rad_expr(K)})) = list({int, rad_expr(K)}).

topo_loop(Sorted, []) = Sorted.
topo_loop(Sorted, Remaining) = Result :-
    Remaining = [_ | _],
    list.filter(( pred(R::in) is semidet :-
        {_, Rad} = R,
        all_roots_resolved(Sorted, Rad)
    ), Remaining, Ready, NotReady),
    ( if Ready = [] then
        Result = Sorted ++ Remaining
    else
        Result = topo_loop(Sorted ++ Ready, NotReady)
    ).

all_roots_resolved(_, re_lit(_)).
all_roots_resolved(Resolved, re_neg(A)) :-
    all_roots_resolved(Resolved, A).
all_roots_resolved(Resolved, re_add(A, B)) :-
    all_roots_resolved(Resolved, A),
    all_roots_resolved(Resolved, B).
all_roots_resolved(Resolved, re_mul(A, B)) :-
    all_roots_resolved(Resolved, A),
    all_roots_resolved(Resolved, B).
all_roots_resolved(Resolved, re_inv(A)) :-
    all_roots_resolved(Resolved, A).
all_roots_resolved(Resolved, re_pow(A, _)) :-
    all_roots_resolved(Resolved, A).
all_roots_resolved(Resolved, re_root(N, A)) :-
    list.member({N, A}, Resolved).

%---------------------------------------------------------------------------%
:- end_module rad_expr.
%---------------------------------------------------------------------------%
