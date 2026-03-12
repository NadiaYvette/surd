%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: positive.m
% Main author: nyc
%
% A type for strictly positive integers (> 0).
%
% This eliminates runtime errors for functions that require positive input
% (factorise, cyclotomic, Euler's totient, etc.) by encoding the constraint
% in the type system.
%
%---------------------------------------------------------------------------%

:- module positive.
:- interface.

:- import_module integer.

%---------------------------------------------------------------------------%

    % A strictly positive integer (> 0).
    % The constructor is not exported; use positive/1 or det_positive/1.
    %
:- type positive.

    % Unwrap a positive integer to its underlying value.
    %
:- func to_integer(positive) = integer.

    % Checked construction: fails if the integer is not positive.
    %
:- pred positive(integer::in, positive::out) is semidet.

    % Deterministic construction: throws an exception if not positive.
    %
:- func det_positive(integer) = positive.

    % Arithmetic on positives: results are guaranteed positive.
    %
:- func positive + positive = positive.
:- func positive * positive = positive.

    % Comparison.
    %
:- pred positive < positive.
:- mode (in < in) is semidet.

:- pred positive > positive.
:- mode (in > in) is semidet.

:- pred positive =< positive.
:- mode (in =< in) is semidet.

:- pred positive >= positive.
:- mode (in >= in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

:- type positive
    --->    positive(integer).

to_integer(positive(N)) = N.

positive(N, positive(N)) :-
    N > integer.zero.

det_positive(N) =
    ( if N > integer.zero then
        positive(N)
    else
        func_error($pred, "non-positive integer")
    ).

positive(A) + positive(B) = positive(A + B).
positive(A) * positive(B) = positive(A * B).

positive(A) < positive(B) :- A < B.
positive(A) > positive(B) :- A > B.
positive(A) =< positive(B) :- A =< B.
positive(A) >= positive(B) :- A >= B.

%---------------------------------------------------------------------------%
:- end_module positive.
%---------------------------------------------------------------------------%
