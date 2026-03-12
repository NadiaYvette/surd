%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: factoring.m
% Main author: nyc
%
% Polynomial factoring over Q.
%
% Implements Kronecker's method for small-degree polynomials and rational
% root testing. Sufficient for the radical denesting and trig evaluation
% use cases where we typically factor cyclotomic polynomials and
% low-degree minimal polynomials.
%
%---------------------------------------------------------------------------%

:- module factoring.
:- interface.

:- import_module list.
:- import_module poly.
:- import_module rational.

%---------------------------------------------------------------------------%

:- type factor_pair
    --->    factor_pair(
                fp_factor       :: poly(rational),
                fp_multiplicity :: int
            ).

    % Factor a polynomial over Q into irreducible factors.
    % Returns a list of (irreducible factor, multiplicity) pairs.
    %
:- func factor(poly(rational)) = list(factor_pair).

    % Factor a square-free polynomial over Q into irreducible factors.
    %
:- func factor_square_free(poly(rational)) = list(poly(rational)).

    % Find all rational roots of a polynomial using the rational root theorem.
    %
:- func rational_roots(poly(rational)) = list(rational).

    % Check if a polynomial is irreducible over Q.
    %
:- pred is_irreducible(poly(rational)::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module positive.
:- import_module prime_factors.
:- import_module require.

%---------------------------------------------------------------------------%

factor(P) = Result :-
    ( if degree(P) =< 0 then
        Result = []
    else
        SFFactors = square_free(P),
        Result = list.condense(list.map(
            ( func(sq_free_factor(F, M)) =
                list.map(
                    ( func(G) = factor_pair(G, M) ),
                    factor_square_free(F))
            ), SFFactors))
    ).

factor_square_free(P) = Result :-
    ( if degree(P) =< 0 then
        Result = []
    else if degree(P) = 1 then
        Result = [monic(P)]
    else
        Result = factor_sf_loop(monic(P))
    ).

:- func factor_sf_loop(poly(rational)) = list(poly(rational)).

factor_sf_loop(F) = Result :-
    ( if degree(F) =< 1 then
        Result = [F]
    else
        ( if find_linear_factor(F, Root, Quotient) then
            LinFactor = mk_poly([-Root, rational.one]),
            Result = [LinFactor | factor_sf_loop(Quotient)]
        else if find_quadratic_factor(F, Fac, Quotient2) then
            Result = [Fac | factor_sf_loop(Quotient2)]
        else
            % Cannot factor further; assume irreducible
            Result = [F]
        )
    ).

%---------------------------------------------------------------------------%
% Rational roots
%---------------------------------------------------------------------------%

rational_roots(P) = Result :-
    rational_roots_2(P, Result).

:- pred rational_roots_2(poly(rational)::in, list(rational)::out) is det.

rational_roots_2(P, Result) :-
    Deg = degree(P) : int,
    ( if Deg =< 0 then
        Result = []
    else
        rational_roots_3(P, Result)
    ).

:- pred rational_roots_3(poly(rational)::in, list(rational)::out) is det.

rational_roots_3(P, Result) :-
    get_candidates(P, Candidates),
    filter_roots(P, Candidates, Result).

:- pred get_candidates(poly(rational)::in, list(rational)::out) is det.

get_candidates(P, Candidates) :-
    Cs = coeffs(P) : list(rational),
    lcm_of_denoms(Cs, LcmDenom),
    to_int_coeffs(Cs, LcmDenom, IntCoeffs),
    candidates_from_int_coeffs(IntCoeffs, Candidates).

:- pred candidates_from_int_coeffs(list(integer)::in,
    list(rational)::out) is det.

candidates_from_int_coeffs(IntCoeffs, Candidates) :-
    ( if IntCoeffs = [A0 | _] then
        ( if list.last(IntCoeffs, AN0) then
            AN = AN0
        else
            unexpected($pred, "empty list")
        ),
        candidates_from_endpoints(A0, AN, Candidates)
    else
        Candidates = ([] : list(rational))
    ).

:- pred candidates_from_endpoints(integer::in, integer::in,
    list(rational)::out) is det.

candidates_from_endpoints(A0, AN, Candidates) :-
    ( if integer.is_zero(A0) then
        NumDivs = [integer.zero]
    else
        NumDivs = int_divisors(integer.abs(A0))
    ),
    DenDivs = int_divisors(integer.abs(AN)),
    Candidates = make_candidates(NumDivs, DenDivs).

:- pred lcm_of_denoms(list(rational)::in, integer::out) is det.

lcm_of_denoms(Cs, LcmDenom) :-
    list.foldl(
        ( pred(C::in, Acc0::in, Acc1::out) is det :-
            Acc1 = lcm_integer(Acc0, integer.abs(denom(C)))
        ), Cs, integer.one, LcmDenom).

:- pred to_int_coeffs(list(rational)::in, integer::in,
    list(integer)::out) is det.

to_int_coeffs(Cs, LcmDenom, IntCoeffs) :-
    Scale = from_integers(LcmDenom, integer.one),
    IntCoeffs = list.map(
        ( func(C) = numer(C * Scale) ),
        Cs).

:- pred is_root(poly(rational)::in, rational::in) is semidet.

is_root(P, R) :-
    V = eval(P, R) : rational,
    V = rational.zero.

:- pred filter_roots(poly(rational)::in, list(rational)::in,
    list(rational)::out) is det.

filter_roots(_, [], []).
filter_roots(P, [R | Rs], Result) :-
    filter_roots(P, Rs, Rest),
    ( if is_root(P, R) then
        Result = [R | Rest]
    else
        Result = Rest
    ).

:- func make_candidates(list(integer), list(integer)) = list(rational).

make_candidates(NumDivs, DenDivs) = Result :-
    PosNeg = list.condense(list.map(
        ( func(D) = [D, integer.'-'(D)] ), NumDivs)),
    Result = list.condense(list.map(
        ( func(N) = list.filter_map(
            ( func(D) = R is semidet :-
                not integer.is_zero(D),
                R = from_integers(N, D)
            ), DenDivs)
        ), PosNeg)).

%---------------------------------------------------------------------------%

is_irreducible(P) :-
    degree(P) >= 1,
    ( if degree(P) = 1 then
        true
    else
        list.length(factor_square_free(P)) = 1
    ).

%---------------------------------------------------------------------------%
% Internal helpers
%---------------------------------------------------------------------------%

:- pred find_linear_factor(poly(rational)::in,
    rational::out, poly(rational)::out) is semidet.

find_linear_factor(P, Root, Quotient) :-
    Roots = rational_roots(P),
    Roots = [Root | _],
    LinPoly = mk_poly([-Root, rational.one]),
    div_mod(P, LinPoly, Quotient, _).

:- pred find_quadratic_factor(poly(rational)::in,
    poly(rational)::out, poly(rational)::out) is semidet.

find_quadratic_factor(P, MonicFac, Quotient) :-
    degree(P) >= 4,
    V0 = eval(P, rational.zero),
    V1 = eval(P, rational.one),
    VM1 = eval(P, rational(-1)),
    D0 = ( if V0 = rational.zero then [rational.zero]
        else rational_divisors(V0) ),
    D1 = ( if V1 = rational.zero then [rational.zero]
        else rational_divisors(V1) ),
    DM1 = ( if VM1 = rational.zero then [rational.zero]
        else rational_divisors(VM1) ),
    Two = rational(2),
    Candidates = list.filter_map(
        ( func({A, F1, FM1}) = Cand is semidet :-
            B = (F1 - FM1) / Two,
            C = (F1 + FM1) / Two - A,
            C \= rational.zero,
            Cand = mk_poly([A, B, C])
        ),
        triples(D0, D1, DM1)),
    find_dividing(P, Candidates, MonicFac, Quotient).

:- func triples(list(A), list(B), list(C)) = list({A, B, C}).

triples(As, Bs, Cs) = list.condense(list.map(
    ( func(A) = list.condense(list.map(
        ( func(B) = list.map(
            ( func(C) = {A, B, C} ), Cs)
        ), Bs))
    ), As)).

:- pred find_dividing(poly(rational)::in, list(poly(rational))::in,
    poly(rational)::out, poly(rational)::out) is semidet.

find_dividing(P, [F | Fs], MonicFac, Quotient) :-
    ( if try_divide(P, F, Q) then
        MonicFac = monic(F),
        Quotient = Q
    else
        find_dividing(P, Fs, MonicFac, Quotient)
    ).

:- pred try_divide(poly(rational)::in, poly(rational)::in,
    poly(rational)::out) is semidet.

try_divide(P, F, Q) :-
    degree(F) >= 1,
    degree(F) =< degree(P),
    div_mod(P, F, Q, R),
    coeffs(R) = [].

%---------------------------------------------------------------------------%
% Integer divisors
%---------------------------------------------------------------------------%

:- func int_divisors(integer) = list(integer).

int_divisors(N) = Result :-
    ( if integer.is_zero(N) then
        Result = [integer.zero]
    else
        ( if positive.positive(N, Pos) then
            Fs = factorise(Pos),
            Result = divisors_from_factors(Fs)
        else
            Result = [integer.one]
        )
    ).

:- func divisors_from_factors(list(prime_power)) = list(integer).

divisors_from_factors(Fs) = Result :-
    ( if Fs = [] then
        Result = [integer.one]
    else if Fs = [prime_power(P, E) | Rest] then
        Ds = divisors_from_factors(Rest),
        Result = list.condense(list.map(
            ( func(K) = list.map(
                ( func(D) = integer.pow(P, integer(K)) * D ),
                Ds)
            ), 0 `..` E))
    else
        unexpected($pred, "impossible")
    ).

:- func rational_divisors(rational) = list(rational).

rational_divisors(R) = Result :-
    N = integer.abs(numer(R)),
    D = integer.abs(denom(R)),
    NDivs = int_divisors(N),
    DDivs = int_divisors(D),
    PosNeg = list.condense(list.map(
        ( func(X) = [X, integer.'-'(X)] ), NDivs)),
    Result = list.condense(list.map(
        ( func(Num) = list.filter_map(
            ( func(Den) = Rat is semidet :-
                not integer.is_zero(Den),
                Rat = from_integers(Num, Den)
            ), DDivs)
        ), PosNeg)).

%---------------------------------------------------------------------------%
% Utility
%---------------------------------------------------------------------------%

:- func lcm_integer(integer, integer) = integer.

lcm_integer(A, B) =
    ( if integer.is_zero(A) then
        B
    else if integer.is_zero(B) then
        A
    else
        integer.abs(A * B) // gcd_integer(A, B)
    ).

:- func gcd_integer(integer, integer) = integer.

gcd_integer(A, B) = Result :-
    AA = integer.abs(A),
    BB = integer.abs(B),
    ( if integer.is_zero(BB) then
        Result = AA
    else
        Result = gcd_integer(BB, AA `rem` BB)
    ).

%---------------------------------------------------------------------------%
:- end_module factoring.
%---------------------------------------------------------------------------%
