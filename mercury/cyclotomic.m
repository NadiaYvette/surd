%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: cyclotomic.m
% Main author: nyc
%
% Cyclotomic polynomials and related number-theoretic utilities.
%
% The nth cyclotomic polynomial Phi_n(x) is the minimal polynomial of
% primitive nth roots of unity over Q. It has degree phi(n) (Euler's totient).
%
%---------------------------------------------------------------------------%

:- module cyclotomic.
:- interface.

:- import_module list.
:- import_module poly.
:- import_module positive.
:- import_module rational.

%---------------------------------------------------------------------------%

    % cyclotomic(N) = Phi_N(x):
    %
    % Compute the Nth cyclotomic polynomial over Q.
    % Uses the identity: x^n - 1 = prod_{d|n} Phi_d(x).
    %
:- func cyclotomic(int) = poly(rational).

    % Euler's totient function phi(n).
    %
:- func eulers_totient(positive) = int.

    % Moebius function mu(n).
    %
:- func moebius_mu(positive) = int.

    % Compute all cyclotomic polynomials Phi_1 through Phi_N.
    %
:- func all_cyclotomic(int) = list(poly(rational)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module prime_factors.
:- import_module require.

%---------------------------------------------------------------------------%

cyclotomic(N) = Result :-
    ( if N =< 0 then
        unexpected($pred, "non-positive argument")
    else if N = 1 then
        Result = mk_poly([rational(-1), rational.one])
    else
        XNM1 = x_to_the_n_minus_1(N),
        Divs = proper_divisors(N),
        Denom = list.foldl(
            ( func(D, Acc) = mul(Acc, cyclotomic(D)) ),
            Divs, const_poly(rational.one)),
        div_mod(XNM1, Denom, Result, _)
    ).

    % x^n - 1.
    %
:- func x_to_the_n_minus_1(int) = poly(rational).

x_to_the_n_minus_1(N) =
    mk_poly([rational(-1)] ++ list.duplicate(N - 1, rational.zero) ++
        [rational.one]).

    % Proper divisors of n (all divisors except n itself).
    %
:- func proper_divisors(int) = list(int).

proper_divisors(N) =
    list.filter(( pred(D::in) is semidet :- D < N ),
        all_divisors(N)).

    % All positive divisors of n.
    %
:- func all_divisors(int) = list(int).

all_divisors(N) =
    list.filter(( pred(D::in) is semidet :- N mod D = 0 ),
        1 `..` N).

%---------------------------------------------------------------------------%

eulers_totient(Pos) = Result :-
    Fs = factorise(Pos),
    Result = list.foldl(
        ( func(prime_power(P, E), Acc) = Acc * ET :-
            PInt = integer.det_to_int(P),
            ET = (PInt - 1) * int.pow(PInt, E - 1)
        ),
        Fs, 1).

moebius_mu(Pos) = (Result : int) :-
    Fs = factorise(Pos),
    ( if has_squared_factor(Fs) then
        Result = (0 : int)
    else
        Len = list.length(Fs) : int,
        ( if is_even(Len) then
            Result = (1 : int)
        else
            Result = (-1 : int)
        )
    ).

:- pred has_squared_factor(list(prime_power)::in) is semidet.

has_squared_factor([PP | PPs]) :-
    PP = prime_power(_, E),
    ( if (E : int) > (1 : int) then
        true
    else
        has_squared_factor(PPs)
    ).

:- pred is_even(int::in) is semidet.

is_even(N) :- N mod 2 = 0.

%---------------------------------------------------------------------------%

all_cyclotomic(N) =
    ( if N =< 0 then
        []
    else
        list.map(cyclotomic, 1 `..` N)
    ).

%---------------------------------------------------------------------------%
:- end_module cyclotomic.
%---------------------------------------------------------------------------%
