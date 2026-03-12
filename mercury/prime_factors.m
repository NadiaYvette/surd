%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: prime_factors.m
% Main author: nyc
%
% Small prime factorisation and related utilities.
%
%---------------------------------------------------------------------------%

:- module prime_factors.
:- interface.

:- import_module integer.
:- import_module list.
:- import_module positive.

%---------------------------------------------------------------------------%

    % factorise(N) = Factors:
    %
    % Factorise a positive integer into (prime, exponent) pairs,
    % with primes in ascending order.
    %
    % factorise(det_positive(integer(360))) =
    %   [{integer(2), 3}, {integer(3), 2}, {integer(5), 1}]
    %
:- func factorise(positive) = list(prime_power).

:- type prime_power
    --->    prime_power(
                pp_prime    :: integer,
                pp_exponent :: int
            ).

    % prime_factors(N) = Primes:
    %
    % Distinct prime factors of a positive integer, in ascending order.
    %
:- func prime_factors(positive) = list(integer).

    % is_prime(N):
    %
    % Trial-division primality test (adequate for the sizes we encounter).
    %
:- pred is_prime(integer::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

factorise(Pos) = Factors :-
    N = to_integer(Pos),
    ( if N = integer.one then
        Factors = []
    else
        trial_divide(N, integer(2), [], RevFactors),
        list.reverse(RevFactors, Factors)
    ).

    % trial_divide(N, P, !Acc):
    %
    % Divide out all powers of primes starting from P.
    %
:- pred trial_divide(integer::in, integer::in,
    list(prime_power)::in, list(prime_power)::out) is det.

trial_divide(N, P, !Acc) :-
    ( if N = integer.one then
        true
    else if P * P > N then
        % N is prime; record it.
        !:Acc = [prime_power(N, 1) | !.Acc]
    else
        count_factor(N, P, 0, Exp, Remainder),
        ( if Exp > 0 then
            !:Acc = [prime_power(P, Exp) | !.Acc],
            trial_divide(Remainder, next_candidate(P), !Acc)
        else
            trial_divide(N, next_candidate(P), !Acc)
        )
    ).

    % count_factor(N, P, !Exp, Remainder):
    %
    % Count how many times P divides N, returning the exponent
    % and the remainder after dividing out all copies of P.
    %
:- pred count_factor(integer::in, integer::in,
    int::in, int::out, integer::out) is det.

count_factor(N, P, !Exp, Remainder) :-
    integer.divide_with_rem(N, P, Q, R),
    ( if R = integer.zero then
        count_factor(Q, P, !.Exp + 1, !:Exp, Remainder)
    else
        Remainder = N,
        !:Exp = !.Exp
    ).

    % next_candidate(P) = NextP:
    %
    % After 2, try only odd numbers.
    %
:- func next_candidate(integer) = integer.

next_candidate(P) =
    ( if P = integer(2) then
        integer(3)
    else
        P + integer(2)
    ).

prime_factors(Pos) =
    list.map(func(prime_power(P, _)) = P, factorise(Pos)).

is_prime(N) :-
    N > integer.one,
    not has_small_factor(N, integer(2)).

:- pred has_small_factor(integer::in, integer::in) is semidet.

has_small_factor(N, P) :-
    P * P =< N,
    ( if integer.rem(N, P) = integer.zero then
        true
    else
        has_small_factor(N, next_candidate(P))
    ).

%---------------------------------------------------------------------------%
:- end_module prime_factors.
%---------------------------------------------------------------------------%
