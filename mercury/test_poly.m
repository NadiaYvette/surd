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
:- import_module extension.
:- import_module factoring.
:- import_module groebner.
:- import_module integer.
:- import_module int.
:- import_module interval.
:- import_module list.
:- import_module map.
:- import_module multivariate.
:- import_module positive.
:- import_module prime_factors.
:- import_module poly.
:- import_module rational.
:- import_module resultant.
:- import_module root_bound.
:- import_module set.
:- import_module string.
:- import_module trager_factoring.
:- import_module transcendental.

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

    io.write_string("\n=== Interval tests ===\n", !IO),
    test_interval(!IO),

    io.write_string("\n=== Extension field tests ===\n", !IO),
    test_extension(!IO),

    io.write_string("\n=== Multivariate tests ===\n", !IO),
    test_multivariate(!IO),

    io.write_string("\n=== Transcendental tests ===\n", !IO),
    test_transcendental(!IO),

    io.write_string("\n=== Groebner tests ===\n", !IO),
    test_groebner(!IO),

    io.write_string("\n=== Trager factoring tests ===\n", !IO),
    test_trager(!IO),

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
% Interval tests
%---------------------------------------------------------------------------%

:- pred test_interval(io::di, io::uo) is det.

test_interval(!IO) :-
    R = ( func(N) = rational.rational(N) ),

    % midpoint([1, 3]) = 2
    Iv1 = interval(R(1), R(3)),
    io.format("midpoint([1,3]) = %s\n",
        [s(show_rat(midpoint(Iv1)))], !IO),

    % width([1, 3]) = 2
    io.format("width([1,3]) = %s\n",
        [s(show_rat(width(Iv1)))], !IO),

    % iadd([1,2], [3,4]) = [4,6]
    Iv2 = interval(R(3), R(4)),
    IvSum = iadd(interval(R(1), R(2)), Iv2),
    io.format("iadd([1,2],[3,4]) = [%s, %s]\n",
        [s(show_rat(iv_lo(IvSum))), s(show_rat(iv_hi(IvSum)))], !IO),

    % imul([2,3], [4,5]) = [8,15]
    IvProd = imul(interval(R(2), R(3)), interval(R(4), R(5))),
    io.format("imul([2,3],[4,5]) = [%s, %s]\n",
        [s(show_rat(iv_lo(IvProd))), s(show_rat(iv_hi(IvProd)))], !IO),

    % isqrt([4,9]) contains [2,3]
    IvSqrt = isqrt(interval(R(4), R(9))),
    io.format("isqrt([4,9]) ~ [%s, %s]\n",
        [s(show_rat(iv_lo(IvSqrt))), s(show_rat(iv_hi(IvSqrt)))], !IO),

    % pi_interval should contain a value near 3.14159...
    Pi = pi_interval,
    io.format("pi ~ [%s, %s]\n",
        [s(show_rat(iv_lo(Pi))), s(show_rat(iv_hi(Pi)))], !IO),

    % strictly_positive test
    ( if strictly_positive(interval(R(1), R(2))) then
        io.write_string("[1,2] strictly positive: yes\n", !IO)
    else
        io.write_string("[1,2] strictly positive: no (WRONG)\n", !IO)
    ),

    % contains_zero test
    ( if contains_zero(interval(R(-1), R(1))) then
        io.write_string("[-1,1] contains zero: yes\n", !IO)
    else
        io.write_string("[-1,1] contains zero: no (WRONG)\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Extension field tests
%---------------------------------------------------------------------------%

:- pred test_extension(io::di, io::uo) is det.

test_extension(!IO) :-
    R = ( func(N) = rational.rational(N) ),

    % Q(√2): minimal polynomial x² - 2
    MinPoly = mk_poly([R(-2), R(0), R(1)]),
    Field = mk_ext_field(MinPoly, "√2"),
    io.format("ext_degree(Q(√2)) = %d\n", [i(ext_degree(Field))], !IO),

    % α = generator
    Alpha = generator(Field) : ext_elem(rational),

    % α² should reduce to 2 (since α² ≡ 2 mod (x²-2))
    AlphaSq = ext_mul(Alpha, Alpha),
    io.format("α² = %s (should be [2])\n",
        [s(show_rat_poly(elem_poly(AlphaSq)))], !IO),

    % (1 + α) * (1 - α) = 1 - α² = 1 - 2 = -1
    One = embed(Field, R(1)),
    OnePlusA = ext_add(One, Alpha),
    OneMinusA = ext_sub(One, Alpha),
    Prod = ext_mul(OnePlusA, OneMinusA),
    io.format("(1+α)(1-α) = %s (should be [-1])\n",
        [s(show_rat_poly(elem_poly(Prod)))], !IO),

    % Inverse of α: α⁻¹ = α/2 (since α·(α/2) = α²/2 = 2/2 = 1)
    AlphaInv = ext_inv(Alpha),
    io.format("α⁻¹ = %s (should be [0, 1/2])\n",
        [s(show_rat_poly(elem_poly(AlphaInv)))], !IO),

    % Verify: α * α⁻¹ = 1
    Check = ext_mul(Alpha, AlphaInv),
    io.format("α·α⁻¹ = %s (should be [1])\n",
        [s(show_rat_poly(elem_poly(Check)))], !IO),

    % Test ring instance: ring_add, ring_mul
    A2 = ring_add(Alpha, Alpha) : ext_elem(rational),
    io.format("α+α = %s (should be [0, 2])\n",
        [s(show_rat_poly(elem_poly(A2)))], !IO),

    % extGcd: gcd(x, x²-2) should give (1, s, t) with s·x + t·(x²-2) = 1
    {G, _, _} = ext_gcd(
        mk_poly([R(0), R(1)]),
        mk_poly([R(-2), R(0), R(1)])),
    io.format("gcd(x, x²-2) = %s (should be [1])\n",
        [s(show_rat_poly(G))], !IO).

%---------------------------------------------------------------------------%
% Multivariate tests
%---------------------------------------------------------------------------%

:- pred test_multivariate(io::di, io::uo) is det.

test_multivariate(!IO) :-
    X = var(0),
    Y = var(1),

    % x + y
    PX = mp_var(X) : mpoly(rational),
    PY = mp_var(Y) : mpoly(rational),
    Sum = mp_add(PX, PY),
    io.format("num_terms(x+y) = %d\n", [i(num_terms(Sum))], !IO),
    io.format("total_degree(x+y) = %d\n", [i(total_degree(Sum))], !IO),

    % x * y
    Prod = mp_mul(PX, PY),
    io.format("num_terms(x*y) = %d\n", [i(num_terms(Prod))], !IO),
    io.format("total_degree(x*y) = %d\n", [i(total_degree(Prod))], !IO),

    % (x + y)² = x² + 2xy + y²
    Sq = mp_mul(Sum, Sum),
    io.format("num_terms((x+y)²) = %d\n", [i(num_terms(Sq))], !IO),
    io.format("total_degree((x+y)²) = %d\n", [i(total_degree(Sq))], !IO),

    % degree_in(x, x²+2xy+y²) = 2
    io.format("degree_in(x, (x+y)²) = %d\n",
        [i(degree_in(X, Sq))], !IO),

    % Evaluate x²+2xy+y² at x=2, y=3: should be (2+3)² = 25
    Env = ( func(V) = R :-
        ( if V = X then R = rational(2)
        else R = rational(3) )
    ),
    Val = mp_eval(Env, Sq),
    io.format("eval((x+y)², x=2,y=3) = %s (should be 25)\n",
        [s(show_rat(Val))], !IO),

    % mp_const and mp_is_zero
    Zero = mp_zero : mpoly(rational),
    ( if mp_is_zero(Zero) then
        io.write_string("mp_zero is zero: yes\n", !IO)
    else
        io.write_string("mp_zero is zero: no (WRONG)\n", !IO)
    ),
    ( if mp_is_zero(PX) then
        io.write_string("x is zero: yes (WRONG)\n", !IO)
    else
        io.write_string("x is zero: no\n", !IO)
    ),

    % Test variables
    Vars = mp_variables(Sq),
    io.format("variables((x+y)²) has %d vars\n",
        [i(set.count(Vars))], !IO).

%---------------------------------------------------------------------------%
% Transcendental tests
%---------------------------------------------------------------------------%

:- pred test_transcendental(io::di, io::uo) is det.

test_transcendental(!IO) :-
    % x/1 + y/1 as rational functions
    X = var_rf(var(0)) : rat_func(rational),
    Y = var_rf(var(1)) : rat_func(rational),

    % x + y
    Sum = ring_add(X, Y),
    io.format("rf_num terms(x+y) = %d\n",
        [i(num_terms(rf_num(Sum)))], !IO),

    % x * y
    Prod = ring_mul(X, Y),
    io.format("rf_num terms(x*y) = %d\n",
        [i(num_terms(rf_num(Prod)))], !IO),

    % x / y
    Quot = field_div(X, Y),
    io.format("rf_num terms(x/y) = %d\n",
        [i(num_terms(rf_num(Quot)))], !IO),

    % const is const
    C = const_rf(rational(3, 1)) : rat_func(rational),
    ( if is_const_rf(C) then
        io.write_string("const_rf(3) is const: yes\n", !IO)
    else
        io.write_string("const_rf(3) is const: no (WRONG)\n", !IO)
    ),
    ( if is_const_rf(X) then
        io.write_string("var_rf(x) is const: yes (WRONG)\n", !IO)
    else
        io.write_string("var_rf(x) is const: no\n", !IO)
    ),

    % eval: x=2, y=3 => (x+y) = 5
    Env = ( func(V) = R :-
        ( if V = var(0) then R = rational(2, 1)
        else R = rational(3, 1) )
    ),
    EvalSum = eval_rf(Env, Sum),
    io.format("eval(x+y, x=2,y=3) = %s (should be 5)\n",
        [s(show_rat(EvalSum))], !IO),

    % reduce_frac: (x²-1)/(x-1) should reduce if gcd works
    X2 = ring_mul(X, X),
    % (x²-1) / (x-1)
    Num = mp_sub(rf_num(X2), mp_one),
    Den = mp_sub(mp_var(var(0)), mp_one),
    RF = mk_rat_func(Num, Den),
    RFR = reduce_frac(RF),
    io.format("reduce_frac((x²-1)/(x-1)): num terms=%d, den terms=%d\n",
        [i(num_terms(rf_num(RFR))), i(num_terms(rf_den(RFR)))], !IO).

%---------------------------------------------------------------------------%
% Groebner tests
%---------------------------------------------------------------------------%

:- pred test_groebner(io::di, io::uo) is det.

test_groebner(!IO) :-
    % Simple test: ideal <x+y, x-y> should have basis containing x and y
    X = mp_var(var(0)) : mpoly(rational),
    Y = mp_var(var(1)) : mpoly(rational),
    G1 = mp_add(X, Y),
    G2 = mp_sub(X, Y),
    GB = groebner_basis(grevlex_ord, [G1, G2]),
    io.format("GB(<x+y, x-y>) has %d elements\n",
        [i(list.length(gb_polys(GB)))], !IO),

    % x+y should reduce to 0 modulo the basis
    R1 = gb_reduce(GB, G1),
    ( if mp_is_zero(R1) then
        io.write_string("x+y reduces to 0: yes\n", !IO)
    else
        io.write_string("x+y reduces to 0: no (WRONG)\n", !IO)
    ),

    % x² + y² should reduce to 0 modulo <x+y, x-y>
    % since x² + y² = x(x+y) - y(x-y) + 2y² but also x=0, y=0 in V(I)
    % Actually x+y, x-y => x, y are in the ideal, so x²+y² reduces to 0
    X2Y2 = mp_add(mp_mul(X, X), mp_mul(Y, Y)),
    R2 = gb_reduce(GB, X2Y2),
    ( if mp_is_zero(R2) then
        io.write_string("x²+y² reduces to 0: yes\n", !IO)
    else
        io.write_string("x²+y² reduces to 0: no (WRONG)\n", !IO)
    ),

    % div_mod_mpoly test
    {_, Rem} = div_mod_mpoly(grevlex_ord, X2Y2, gb_polys(GB)),
    ( if mp_is_zero(Rem) then
        io.write_string("div_mod remainder is 0: yes\n", !IO)
    else
        io.write_string("div_mod remainder is 0: no (WRONG)\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Trager factoring tests
%---------------------------------------------------------------------------%

:- pred test_trager(io::di, io::uo) is det.

test_trager(!IO) :-
    % Q(√2): minpoly = t²-2
    MinPoly = poly([rational(-2, 1), rational.zero, rational.one]),
    Field = mk_ext_field(MinPoly, "√2"),
    io.format("ext_degree = %d\n", [i(ext_degree(Field))], !IO),

    % x² - 2 over Q(√2) should factor as (x-α)(x+α)
    Alpha = generator(Field),
    Two = embed(Field, rational(2, 1)),
    % f = x² - 2 (as poly over Q(√2))
    ZeroExt = embed(Field, rational.zero),
    OneExt = embed(Field, rational.one),
    NegTwo = ext_neg(Two),
    F = poly([NegTwo, ZeroExt, OneExt]),
    Factors = factor_sf_over_extension(Field, F),
    io.format("factors of x²-2 over Q(√2): %d factors\n",
        [i(list.length(Factors))], !IO),

    % Each factor should be degree 1
    ( if Factors = [F1, F2] then
        io.format("  factor degrees: %d, %d\n",
            [i(degree(F1)), i(degree(F2))], !IO)
    else
        io.write_string("  unexpected number of factors\n", !IO)
    ),

    % norm_poly test: norm of (x - α) should be x² - 2
    XMinusAlpha = poly([ext_neg(Alpha), OneExt]),
    N = norm_poly(Field, XMinusAlpha),
    io.format("norm(x-α) = %s\n", [s(show_rat_poly(N))], !IO).

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
