%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: galois_solve.m
% Main author: nyc
%
% Top-level interface for solving polynomials via Galois group computation.
%
% Composes:
%   - galois_identify: identifies the Galois group of an irreducible
%     polynomial (currently degree 5 only)
%   - radical_tower: constructs an explicit radical tower and solves
%     via Lagrange resolvents
%   - rad_alg_convert: handles degrees 1–4 via quadratic/Cardano/Ferrari
%
% For degree ≤ 4, solve_alg_num returns no so that the caller can use
% direct formulas. For degree 5, the Galois pipeline is invoked.
%
%---------------------------------------------------------------------------%

:- module galois_solve.
:- interface.

:- import_module alg_num.
:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Try to express an algebraic number as a radical expression.
    %
    % Returns no for degree ≤ 4 (caller should use Cardano/Ferrari),
    % for degree > 5 (not yet supported), or for non-solvable groups.
    %
:- func solve_alg_num(alg_num) = maybe(rad_expr(rational)).

    % Solve an irreducible polynomial, returning radical expressions
    % for all roots.
    %
:- func solve_poly(poly(rational)) = maybe(list(rad_expr(rational))).

    % Identify the Galois group and solve if solvable.
    % Returns {group_name, roots} or no.
    %
:- func identify_and_solve(poly(rational)) =
    maybe({string, list(rad_expr(rational))}).

    % Pick the radical expression whose numerical value is closest to
    % the target real value.
    %
:- func pick_closest_real(list(rad_expr(rational)), float) =
    maybe(rad_expr(rational)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- use_module float.
:- use_module int.
:- use_module math.
:- import_module galois_identify.
:- import_module integer.
:- import_module rad_eval.
:- import_module radical_tower.
:- import_module string.
:- import_module transitive_group.

%---------------------------------------------------------------------------%

solve_alg_num(A) = Result :-
    P = alg_min_poly(A),
    D = poly.degree(P),
    ( if int.'=<'(D, 4) then
        Result = no
    else
        ( if solve_poly(P) = yes(AllRoots) then
            ApproxVal = rational_to_float(
                alg_approx(from_integers(integer.one,
                                         integer(1000000000000000)), A)),
            Result = pick_closest_real(AllRoots, ApproxVal)
        else
            Result = no
        )
    ).

solve_poly(F) = Result :-
    ( if identify_and_solve(F) = yes({_, Roots}) then
        Result = yes(Roots)
    else
        Result = no
    ).

identify_and_solve(F) = Result :-
    ( if poly.degree(F) \= 5 then
        Result = no
    else
        ( if identify_galois_group_5(F) = yes(GR) then
            TG = gr_group(GR),
            ( if tg_solvable(TG) = bool.yes then
                ( if solve_via_tower(GR, F) = yes(Roots) then
                    Result = yes({tg_name(TG), Roots})
                else
                    Result = no
                )
            else
                Result = no
            )
        else
            Result = no
        )
    ).

%---------------------------------------------------------------------------%
% Pick closest real root
%---------------------------------------------------------------------------%

pick_closest_real(Exprs, Target) = Result :-
    Scored = list.map(
        func(E) = {E, eval_dist(E, Target)},
        Exprs),
    Valid = list.filter(
        (pred({_, D}::in) is semidet :- float.'<'(D, 1.0e-4)),
        Scored),
    ( if Valid = [_ | _] then
        Result = yes(fst(find_min_pair(Valid)))
    else
        Best = find_min_pair(Scored),
        ( if float.'<'(snd(Best), 1.0) then
            Result = yes(fst(Best))
        else
            Result = no
        )
    ).

:- func eval_dist(rad_expr(rational), float) = float.

eval_dist(E, Target) = Result :-
    V = eval_complex(E),
    ( if float.'<'(float.abs(im(V)), 1.0e-6) then
        Result = float.abs(float.'-'(re(V), Target))
    else
        Dre = float.'-'(re(V), Target),
        Dim = im(V),
        Result = math.sqrt(float.'+'(float.'*'(Dre, Dre),
                                      float.'*'(Dim, Dim)))
    ).

:- func find_min_pair(list({rad_expr(rational), float})) =
    {rad_expr(rational), float}.

find_min_pair(Xs) = Result :-
    ( if Xs = [H | T] then
        Result = list.foldl(
            func({E, D}, {BE, BD}) = R :-
                ( if float.'<'(D, BD) then R = {E, D}
                else R = {BE, BD} ),
            T, H)
    else
        % Should not happen — caller ensures non-empty
        Result = {re_lit(rational.zero), 1.0e10}
    ).

:- func fst({A, B}) = A.
fst({X, _}) = X.

:- func snd({A, B}) = B.
snd({_, Y}) = Y.

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
:- end_module galois_solve.
%---------------------------------------------------------------------------%
