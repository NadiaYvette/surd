%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: root_isolation.m
% Main author: nyc
%
% Real root isolation for polynomials with rational coefficients.
%
% Uses Sturm's theorem with bisection to separate and refine roots
% of square-free polynomials.
%
%---------------------------------------------------------------------------%

:- module root_isolation.
:- interface.

:- import_module interval.
:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module rational.

%---------------------------------------------------------------------------%

    % An isolating interval for a real root of a polynomial.
    % The polynomial has exactly one root in the open interval (lo, hi),
    % unless lo = hi in which case it's an exact rational root.
    %
:- type isolating_interval
    --->    isolating_interval(
                ii_poly     :: poly(rational),
                ii_interval :: interval
            ).

    % Isolate all real roots of a polynomial.
    % Returns isolating intervals ordered by increasing root value.
    %
:- func isolate_real_roots(poly(rational)) = list(isolating_interval).

    % Refine an isolating interval until its width is less than epsilon.
    %
:- func refine_root(rational, isolating_interval) = isolating_interval.

    % Check if an exact rational root exists within the interval.
    %
:- func root_in_interval(isolating_interval) = maybe(rational).

    % Count real roots of p in the half-open interval (a, b]
    % using Sturm's theorem.
    %
:- func sturm_count(poly(rational), rational, rational) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module integer.
:- import_module root_bound.

%---------------------------------------------------------------------------%
% Root isolation
%---------------------------------------------------------------------------%

isolate_real_roots(P) = Result :-
    D = degree(P),
    ( if int.'=<'(D, 0) then
        Result = []
    else if D = 1 then
        Cs = coeffs(P),
        ( if Cs = [A, B] then
            R = rational.'-'(rational.zero,
                rational.'/'(A, B)),
            Result = [isolating_interval(monic_poly(P),
                interval.interval(R, R))]
        else
            Result = []
        )
    else
        MP = monic_poly(P),
        Bound = root_bound.root_bound(MP),
        IV = interval.interval(rational.'-'(rational.zero, Bound), Bound),
        Result = list.map(
            ( func(I) = isolating_interval(MP, I) ),
            isolate_in(MP, IV))
    ).

%---------------------------------------------------------------------------%
% Refinement
%---------------------------------------------------------------------------%

refine_root(Eps, isolating_interval(P, IV)) =
    isolating_interval(P, refine_interval(P, Eps, IV)).

root_in_interval(isolating_interval(P, interval(L, H))) = Result :-
    ( if L = H, poly.eval(P, L) = rational.zero then
        Result = yes(L)
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Internal: isolate within an interval
%---------------------------------------------------------------------------%

:- func isolate_in(poly(rational), interval) = list(interval).

isolate_in(P, IV) = Result :-
    IV = interval(L, H),
    SC = sturm_count(P, L, H),
    ( if SC = 0 then
        Result = []
    else if SC = 1 then
        Result = [IV]
    else
        Two = rational(2, 1),
        M = rational.'/'(rational.'+'(L, H), Two),
        FM = poly.eval(P, M),
        ( if FM = rational.zero then
            Result = [interval(M, M) |
                list.append(isolate_in(P, interval(L, M)),
                    isolate_in(P, interval(M, H)))]
        else
            Result = list.append(
                isolate_in(P, interval(L, M)),
                isolate_in(P, interval(M, H)))
        )
    ).

%---------------------------------------------------------------------------%
% Sturm chain and counting
%---------------------------------------------------------------------------%

sturm_count(P, A, B) = Result :-
    Chain = sturm_chain(P),
    VA = sign_changes_at(Chain, A),
    VB = sign_changes_at(Chain, B),
    Result = int.'-'(VA, VB).

:- func sturm_chain(poly(rational)) = list(poly(rational)).

sturm_chain(P) = [P, DP | build_chain(P, DP)] :-
    DP = poly.diff(P).

:- func build_chain(poly(rational), poly(rational)) = list(poly(rational)).

build_chain(A, B) = Result :-
    ( if int.'<'(degree(B), 0) then
        Result = []
    else
        poly.div_mod(A, B, _, R),
        NR = poly.scale(rational.'-'(rational.zero, rational.one), R),
        ( if int.'<'(degree(NR), 0) then
            Result = []
        else
            Result = [NR | build_chain(B, NR)]
        )
    ).

:- func sign_changes_at(list(poly(rational)), rational) = int.

sign_changes_at(Chain, X) = Result :-
    Vals0 = list.map(( func(P) = poly.eval(P, X) ), Chain),
    Vals = list.filter(( pred(V::in) is semidet :- V \= rational.zero ), Vals0),
    Result = count_sign_changes(Vals).

:- func count_sign_changes(list(rational)) = int.

count_sign_changes(Vals) = Result :-
    ( if Vals = [] then
        Result = 0
    else if Vals = [_] then
        Result = 0
    else if Vals = [A, B | Rest] then
        SA = rat_signum(A),
        SB = rat_signum(B),
        ( if SA \= SB then
            Result = int.'+'(1, count_sign_changes([B | Rest]))
        else
            Result = count_sign_changes([B | Rest])
        )
    else
        Result = 0
    ).

:- func rat_signum(rational) = int.

rat_signum(R) = Result :-
    ( if rational.'>'(R, rational.zero) then
        Result = 1
    else if rational.'<'(R, rational.zero) then
        Result = -1
    else
        Result = 0
    ).

%---------------------------------------------------------------------------%
% Make monic
%---------------------------------------------------------------------------%

:- func monic_poly(poly(rational)) = poly(rational).

monic_poly(P) = Result :-
    Cs = coeffs(P),
    ( if Cs = [] then
        Result = P
    else
        LC = list.det_last(Cs),
        ( if LC = rational.zero then
            Result = P
        else
            LCInv = rational.'/'(rational.one, LC),
            Result = poly.scale(LCInv, P)
        )
    ).

%---------------------------------------------------------------------------%
% Interval refinement
%---------------------------------------------------------------------------%

:- func refine_interval(poly(rational), rational, interval) = interval.

refine_interval(P, Eps, IV) = Result :-
    IV = interval(L, H),
    ( if L = H then
        Result = IV
    else if rational.'<'(rational.'-'(H, L), Eps) then
        Result = IV
    else
        Two = rational(2, 1),
        M = rational.'/'(rational.'+'(L, H), Two),
        FM = poly.eval(P, M),
        ( if FM = rational.zero then
            Result = interval(M, M)
        else
            FL = poly.eval(P, L),
            ( if rat_signum(FL) \= rat_signum(FM) then
                Result = refine_interval(P, Eps, interval(L, M))
            else
                Result = refine_interval(P, Eps, interval(M, H))
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module root_isolation.
%---------------------------------------------------------------------------%
