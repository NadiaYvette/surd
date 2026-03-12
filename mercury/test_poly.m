%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: test_poly.m
%
% Tests for the polynomial arithmetic library and related modules.
%
%---------------------------------------------------------------------------%

:- module test_poly.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cyclotomic.
:- import_module factoring.
:- import_module integer.
:- import_module int.
:- import_module list.
:- import_module positive.
:- import_module prime_factors.
:- import_module poly.
:- import_module rational.
:- import_module resultant.
:- import_module root_bound.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("=== Positive tests ===\n", !IO),
    test_positive(!IO),

    io.write_string("\n=== Prime factorisation tests ===\n", !IO),
    test_prime_factors(!IO),

    io.write_string("\n=== Polynomial tests ===\n", !IO),
    test_poly_basics(!IO),

    io.write_string("\n=== Resultant tests ===\n", !IO),
    test_resultant(!IO),

    io.write_string("\n=== Cyclotomic tests ===\n", !IO),
    test_cyclotomic(!IO),

    io.write_string("\n=== Factoring tests ===\n", !IO),
    test_factoring(!IO),

    io.write_string("\n=== Root bound tests ===\n", !IO),
    test_root_bound(!IO),

    io.write_string("\nAll tests passed.\n", !IO).

%---------------------------------------------------------------------------%
% Positive tests
%---------------------------------------------------------------------------%

:- pred test_positive(io::di, io::uo) is det.

test_positive(!IO) :-
    P5 = det_positive(integer(5)),
    io.format("det_positive(5) = %s\n",
        [s(integer.to_string(to_integer(P5)))], !IO),

    ( if positive.positive(integer(3), P3) then
        io.format("positive(3) succeeded: %s\n",
            [s(integer.to_string(to_integer(P3)))], !IO)
    else
        io.write_string("positive(3) FAILED (unexpected)\n", !IO)
    ),

    ( if positive.positive(integer(0), _) then
        io.write_string("positive(0) succeeded (unexpected)\n", !IO)
    else
        io.write_string("positive(0) correctly failed\n", !IO)
    ),

    P7 = det_positive(integer(7)),
    Sum = P5 + P7,
    Prod = P5 * P7,
    io.format("5 + 7 = %s\n",
        [s(integer.to_string(to_integer(Sum)))], !IO),
    io.format("5 * 7 = %s\n",
        [s(integer.to_string(to_integer(Prod)))], !IO).

%---------------------------------------------------------------------------%
% Prime factorisation tests
%---------------------------------------------------------------------------%

:- pred test_prime_factors(io::di, io::uo) is det.

test_prime_factors(!IO) :-
    F360 = factorise(det_positive(integer(360))),
    io.format("factorise(360) = %s\n",
        [s(show_factors(F360))], !IO),

    F1 = factorise(det_positive(integer(1))),
    io.format("factorise(1) = %s\n",
        [s(show_factors(F1))], !IO),

    F97 = factorise(det_positive(integer(97))),
    io.format("factorise(97) = %s\n",
        [s(show_factors(F97))], !IO),

    ( if is_prime(integer(97)) then
        io.write_string("97 is prime: yes\n", !IO)
    else
        io.write_string("97 is prime: no (WRONG)\n", !IO)
    ),
    ( if is_prime(integer(100)) then
        io.write_string("100 is prime: yes (WRONG)\n", !IO)
    else
        io.write_string("100 is prime: no\n", !IO)
    ).

:- func show_factors(list(prime_power)) = string.

show_factors(Fs) =
    "[" ++ string.join_list(", ", list.map(show_factor, Fs)) ++ "]".

:- func show_factor(prime_power) = string.

show_factor(prime_power(P, E)) =
    "(" ++ integer.to_string(P) ++ ", " ++ int_to_string(E) ++ ")".

%---------------------------------------------------------------------------%
% Polynomial tests
%---------------------------------------------------------------------------%

:- pred test_poly_basics(io::di, io::uo) is det.

test_poly_basics(!IO) :-
    R = ( func(N) = rational.rational(N) ),

    Z = zero_poly : poly(rational),
    io.format("degree(0) = %d\n", [i(degree(Z))], !IO),

    C3 = const_poly(R(3)),
    io.format("degree(3) = %d\n", [i(degree(C3))], !IO),

    Xp = mono_x : poly(rational),
    io.format("degree(x) = %d\n", [i(degree(Xp))], !IO),

    XP3 = add(Xp, C3),
    io.format("coeffs(x + 3) = %s\n",
        [s(show_rat_poly(XP3))], !IO),

    XM1 = add(Xp, const_poly(R(-1))),
    Product = mul(XP3, XM1),
    io.format("(x+3)*(x-1) = %s\n",
        [s(show_rat_poly(Product))], !IO),

    V = eval(Product, R(2)),
    io.format("eval(x^2+2x-3, 2) = %s\n",
        [s(show_rat(V))], !IO),

    D = diff(Product),
    io.format("diff(x^2+2x-3) = %s\n",
        [s(show_rat_poly(D))], !IO),

    div_mod(Product, XP3, Q, Rem),
    io.format("(x^2+2x-3) / (x+3) = %s, rem %s\n",
        [s(show_rat_poly(Q)), s(show_rat_poly(Rem))], !IO),

    X2M1 = add(mul(Xp, Xp), const_poly(R(-1))),
    X2P2XP1 = add(add(mul(Xp, Xp), scale(R(2), Xp)), const_poly(R(1))),
    G = gcd(X2M1, X2P2XP1),
    io.format("gcd(x^2-1, x^2+2x+1) = %s\n",
        [s(show_rat_poly(G))], !IO),

    Cubic = mul(X2P2XP1, XM1),
    SqF = square_free(Cubic),
    io.format("square_free((x+1)^2*(x-1)) =\n", [], !IO),
    list.foldl(
        ( pred(sq_free_factor(F, M)::in, !.S::di, !:S::uo) is det :-
            io.format("  factor: %s, multiplicity: %d\n",
                [s(show_rat_poly(F)), i(M)], !S)
        ), SqF, !IO).

%---------------------------------------------------------------------------%
% Resultant tests
%---------------------------------------------------------------------------%

:- pred test_resultant(io::di, io::uo) is det.

test_resultant(!IO) :-
    R = ( func(N) = rational.rational(N) ),
    Xp = mono_x : poly(rational),

    % Resultant of (x - 1) and (x - 2) = (1 - 2) = -1
    % (for monic linear polys, Res(x-a, x-b) = a - b)
    P1 = add(Xp, const_poly(R(-1))),  % x - 1
    P2 = add(Xp, const_poly(R(-2))),  % x - 2
    Res12 = poly_resultant(P1, P2),
    io.format("Res(x-1, x-2) = %s\n", [s(show_rat(Res12))], !IO),

    % Resultant of x^2-1 and x-1 should be 0 (common root at 1)
    X2M1 = add(mul(Xp, Xp), const_poly(R(-1))),
    Res0 = poly_resultant(X2M1, P1),
    io.format("Res(x^2-1, x-1) = %s (should be 0)\n",
        [s(show_rat(Res0))], !IO),

    % negate_var(x + 1) = -x + 1 = [1, -1]
    NV = negate_var(add(Xp, const_poly(R(1)))),
    io.format("negate_var(x+1) = %s\n", [s(show_rat_poly(NV))], !IO),

    % reciprocal_poly([1, 2, 3]) = [3, 2, 1]
    RP = reciprocal_poly(mk_poly([R(1), R(2), R(3)])),
    io.format("reciprocal([1,2,3]) = %s\n", [s(show_rat_poly(RP))], !IO).

%---------------------------------------------------------------------------%
% Cyclotomic tests
%---------------------------------------------------------------------------%

:- pred test_cyclotomic(io::di, io::uo) is det.

test_cyclotomic(!IO) :-
    % Phi_1(x) = x - 1
    C1 = cyclotomic(1),
    io.format("Phi_1 = %s\n", [s(show_rat_poly(C1))], !IO),

    % Phi_2(x) = x + 1
    C2 = cyclotomic(2),
    io.format("Phi_2 = %s\n", [s(show_rat_poly(C2))], !IO),

    % Phi_6(x) = x^2 - x + 1
    C6 = cyclotomic(6),
    io.format("Phi_6 = %s\n", [s(show_rat_poly(C6))], !IO),

    % Phi_12(x) = x^4 - x^2 + 1
    C12 = cyclotomic(12),
    io.format("Phi_12 = %s\n", [s(show_rat_poly(C12))], !IO),

    % phi(12) = 4
    Phi12 = eulers_totient(det_positive(integer(12))),
    io.format("phi(12) = %d\n", [i(Phi12)], !IO),

    % mu(30) = -1 (30 = 2*3*5, three distinct primes)
    Mu30 = moebius_mu(det_positive(integer(30))),
    io.format("mu(30) = %d\n", [i(Mu30)], !IO),

    % mu(12) = 0 (12 = 2^2 * 3, has squared factor)
    Mu12 = moebius_mu(det_positive(integer(12))),
    io.format("mu(12) = %d\n", [i(Mu12)], !IO).

%---------------------------------------------------------------------------%
% Factoring tests
%---------------------------------------------------------------------------%

:- pred test_factoring(io::di, io::uo) is det.

test_factoring(!IO) :-
    R = ( func(N) = rational.rational(N) ),
    Xp = mono_x : poly(rational),

    % Rational roots of x^2 - 1 = {-1, 1}
    X2M1 = add(mul(Xp, Xp), const_poly(R(-1))),
    Roots = rational_roots(X2M1),
    io.format("rational_roots(x^2-1) = %s\n",
        [s(show_rat_list(Roots))], !IO),

    % factor_square_free(x^2-1) = [x-1, x+1]
    Factors = factor_square_free(X2M1),
    io.format("factor_sf(x^2-1) =\n", [], !IO),
    list.foldl(
        ( pred(F::in, !.S::di, !:S::uo) is det :-
            io.format("  %s\n", [s(show_rat_poly(F))], !S)
        ), Factors, !IO),

    % x^2 + 1 is irreducible over Q
    X2P1 = add(mul(Xp, Xp), const_poly(R(1))),
    ( if is_irreducible(X2P1) then
        io.write_string("x^2+1 is irreducible: yes\n", !IO)
    else
        io.write_string("x^2+1 is irreducible: no (WRONG)\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Root bound tests
%---------------------------------------------------------------------------%

:- pred test_root_bound(io::di, io::uo) is det.

test_root_bound(!IO) :-
    R = ( func(N) = rational.rational(N) ),
    Xp = mono_x : poly(rational),

    % root_bound(x^2 - 1) = 1 + max(|-1|/1, |0|/1) = 2
    X2M1 = add(mul(Xp, Xp), const_poly(R(-1))),
    Bound = root_bound(X2M1),
    io.format("root_bound(x^2-1) = %s\n", [s(show_rat(Bound))], !IO),

    % approx_roots should find two roots near -1 and 1
    Roots = approx_roots(X2M1),
    io.format("approx_roots(x^2-1) = %s\n",
        [s(show_rat_list(Roots))], !IO).

%---------------------------------------------------------------------------%
% Display helpers
%---------------------------------------------------------------------------%

:- func show_rat(rational) = string.

show_rat(R) = S :-
    N = numer(R),
    D = denom(R),
    ( if D = integer.one then
        S = integer.to_string(N)
    else
        S = integer.to_string(N) ++ "/" ++ integer.to_string(D)
    ).

:- func show_rat_poly(poly(rational)) = string.

show_rat_poly(poly(Cs)) =
    "[" ++ string.join_list(", ", list.map(show_rat, Cs)) ++ "]".

:- func show_rat_list(list(rational)) = string.

show_rat_list(Rs) =
    "[" ++ string.join_list(", ", list.map(show_rat, Rs)) ++ "]".

%---------------------------------------------------------------------------%
:- end_module test_poly.
%---------------------------------------------------------------------------%
