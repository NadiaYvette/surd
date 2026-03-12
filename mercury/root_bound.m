%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: root_bound.m
% Main author: nyc
%
% Root bounding, approximate root finding, and factor selection
% for rational polynomials.
%
%---------------------------------------------------------------------------%

:- module root_bound.
:- interface.

:- import_module list.
:- import_module one_or_more.
:- import_module poly.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Cauchy's root bound: all roots r satisfy |r| <= root_bound(p).
    %
:- func root_bound(poly(rational)) = rational.

    % Find approximate real roots of a polynomial by scanning and bisection.
    %
:- func approx_roots(poly(rational)) = list(rational).

    % Bisection root finding on [Lo, Hi] with N iterations.
    %
:- func bisect_root(poly(rational), rational, rational, int) = rational.

    % Pick the factor whose numerical root is closest to a target value.
    %
:- func pick_closest(float, one_or_more(poly(rational))) = poly(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module float.
:- use_module int.
:- use_module integer.
:- import_module require.

%---------------------------------------------------------------------------%

root_bound(P) = Result :-
    Cs = coeffs(P) : list(rational),
    ( if Cs = [] then
        Result = rational.zero
    else
        list.det_last(Cs, LC),
        list.det_split_last(Cs, Init, _),
        Ratios = list.map(
            ( func(C) = rational.abs(rational.'/'(C, LC)) ), Init),
        MaxRatio = list.foldl(
            ( func(R, Acc) = ( if rational.'>'(R, Acc) then R else Acc ) ),
            Ratios, rational.zero),
        Result = rational.'+'(rational.one, MaxRatio)
    ).

%---------------------------------------------------------------------------%

approx_roots(P) = Result :-
    Deg = degree(P),
    ( if int.'=<'(Deg, 0) then
        Result = []
    else if Deg = 1 then
        Cs = coeffs(P),
        ( if Cs = [A, B] then
            Result = [rational.'/'(rational.'-'(A), B)]
        else
            Result = []
        )
    else
        Bound = root_bound(P),
        BoundF = rat_to_float(Bound),
        Lo = float.truncate_to_int(float.'-'(0.0, float.'*'(BoundF, 10.0))),
        Hi = float.ceiling_to_int(float.'*'(BoundF, 10.0)),
        make_scan_points(Lo, Hi, Pts),
        Signs = list.map(
            ( func(X) = {X, rat_signum(eval(P, X))} ),
            Pts),
        sign_changes(Signs, Changes),
        Roots = list.map(
            ( func({X1, X2}) = bisect_root(P, X1, X2, 50) ),
            Changes),
        ( if Roots = [] then
            Result = [rational.zero]
        else
            Result = Roots
        )
    ).

:- pred make_scan_points(int::in, int::in, list(rational)::out) is det.

make_scan_points(Lo, Hi, Pts) :-
    ( if int.'>'(Lo, Hi) then
        Pts = []
    else
        make_scan_points(int.'+'(Lo, 1), Hi, Rest),
        Pts = [rational.rational(Lo, 10) | Rest]
    ).

:- pred sign_changes(list({rational, int})::in,
    list({rational, rational})::out) is det.

sign_changes([], []).
sign_changes([_], []).
sign_changes([{X1, S1}, {X2, S2} | Rest], Changes) :-
    ( if S1 \= S2, S1 \= 0, S2 \= 0 then
        sign_changes([{X2, S2} | Rest], RestChanges),
        Changes = [{X1, X2} | RestChanges]
    else
        sign_changes([{X2, S2} | Rest], Changes)
    ).

:- func rat_signum(rational) = int.

rat_signum(R) =
    ( if rational.'>'(R, rational.zero) then 1
    else if rational.'<'(R, rational.zero) then -1
    else 0
    ).

%---------------------------------------------------------------------------%

bisect_root(P, Lo, Hi, N) = Result :-
    ( if int.'=<'(N, 0) then
        Result = Lo
    else
        Two = rational.rational(2),
        Mid = rational.'/'(rational.'+'(Lo, Hi), Two),
        FMid = eval(P, Mid),
        FLo = eval(P, Lo),
        ( if FMid = rational.zero then
            Result = Mid
        else if rat_signum(FMid) = rat_signum(FLo) then
            Result = bisect_root(P, Mid, Hi, int.'-'(N, 1))
        else
            Result = bisect_root(P, Lo, Mid, int.'-'(N, 1))
        )
    ).

%---------------------------------------------------------------------------%

pick_closest(Target, one_or_more(F, Fs)) = Best :-
    Score = ( func(P) = MinDist :-
        Rs = approx_roots(P),
        Dists = list.map(
            ( func(R) = float.abs(float.'-'(rat_to_float(R), Target)) ),
            Rs),
        ( if Dists = [] then
            MinDist = float.max
        else
            MinDist = list.foldl(float.min, Dists, float.max)
        )
    ),
    pick_min(F, Score(F), Fs, Score, Best).

:- pred pick_min(poly(rational)::in, float::in, list(poly(rational))::in,
    (func(poly(rational)) = float)::in, poly(rational)::out) is det.

pick_min(BestSoFar, _, [], _, BestSoFar).
pick_min(BestSoFar, BestScore, [F | Fs], Score, Best) :-
    S = Score(F),
    ( if float.'<'(S, BestScore) then
        pick_min(F, S, Fs, Score, Best)
    else
        pick_min(BestSoFar, BestScore, Fs, Score, Best)
    ).

%---------------------------------------------------------------------------%
% Utility
%---------------------------------------------------------------------------%

:- func rat_to_float(rational) = float.

rat_to_float(R) =
    float.'/'(integer.float(numer(R)), integer.float(denom(R))).

%---------------------------------------------------------------------------%
:- end_module root_bound.
%---------------------------------------------------------------------------%
