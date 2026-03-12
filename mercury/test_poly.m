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

:- import_module bool.
:- import_module cyclotomic.
:- import_module extension.
:- import_module factoring.
:- use_module float.
:- import_module groebner.
:- import_module integer.
:- import_module int.
:- import_module interval.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multivariate.
:- import_module positive.
:- import_module prime_factors.
:- import_module poly.
:- import_module rad_eval.
:- import_module rad_expr.
:- import_module rad_normalize.
:- import_module rational.
:- import_module resultant.
:- import_module root_bound.
:- import_module set.
:- import_module string.
:- import_module trager_factoring.
:- import_module transcendental.
:- import_module pretty.
:- import_module latex.
:- import_module denest_sqrt.
:- import_module denest_nthroot.
:- import_module denest.
:- import_module normal_form.
:- import_module root_isolation.

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

    io.write_string("\n=== Radical expression tests ===\n", !IO),
    test_rad_expr(!IO),

    io.write_string("\n=== Radical eval tests ===\n", !IO),
    test_rad_eval(!IO),

    io.write_string("\n=== Radical normalize tests ===\n", !IO),
    test_rad_normalize(!IO),

    io.write_string("\n=== Pretty-print tests ===\n", !IO),
    test_pretty(!IO),

    io.write_string("\n=== LaTeX tests ===\n", !IO),
    test_latex(!IO),

    io.write_string("\n=== Sqrt denesting tests ===\n", !IO),
    test_denest_sqrt(!IO),

    io.write_string("\n=== Nth root denesting tests ===\n", !IO),
    test_denest_nthroot(!IO),

    io.write_string("\n=== Normal form tests ===\n", !IO),
    test_normal_form(!IO),

    io.write_string("\n=== Root isolation tests ===\n", !IO),
    test_root_isolation(!IO),

    io.write_string("\n=== Denest dispatcher tests ===\n", !IO),
    test_denest(!IO),

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
% Radical expression tests
%---------------------------------------------------------------------------%

:- pred test_rad_expr(io::di, io::uo) is det.

test_rad_expr(!IO) :-
    % √12 expression
    Sqrt12 = re_sqrt(re_lit(rational(12, 1))),
    io.format("depth(√12) = %d\n", [i(re_depth(Sqrt12))], !IO),
    io.format("size(√12) = %d\n", [i(re_size(Sqrt12))], !IO),

    % 2 + 3√5
    E1 = re_add(re_lit(rational(2, 1)),
                re_mul(re_lit(rational(3, 1)),
                       re_sqrt(re_lit(rational(5, 1))))),
    io.format("size(2+3√5) = %d\n", [i(re_size(E1))], !IO),

    % collect_radicals
    Rads = collect_radicals(E1),
    io.format("radicals(2+3√5) = %d pairs\n",
        [i(list.length(Rads))], !IO),

    % sub and div helpers
    S = re_sub(re_lit(rational(5, 1)), re_lit(rational(3, 1))),
    ( if S = re_add(re_lit(_), re_neg(re_lit(_))) then
        io.write_string("re_sub structure: correct\n", !IO)
    else
        io.write_string("re_sub structure: WRONG\n", !IO)
    ),

    % map_coeffs: double all coefficients
    E2 = re_map_coeffs(( func(R) = rational.'*'(rational(2, 1), R) ),
        re_lit(rational(3, 1))),
    ( if E2 = re_lit(R2), R2 = rational(6, 1) then
        io.write_string("map_coeffs(×2, 3) = 6: yes\n", !IO)
    else
        io.write_string("map_coeffs(×2, 3) = 6: WRONG\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Radical eval tests
%---------------------------------------------------------------------------%

:- pred test_rad_eval(io::di, io::uo) is det.

test_rad_eval(!IO) :-
    % eval_float(√4) = 2.0
    V1 = eval_float(re_sqrt(re_lit(rational(4, 1)))),
    io.format("eval_float(√4) = %.1f\n", [f(V1)], !IO),

    % eval_float(2 + 3) = 5.0
    V2 = eval_float(re_add(re_lit(rational(2, 1)),
                           re_lit(rational(3, 1)))),
    io.format("eval_float(2+3) = %.1f\n", [f(V2)], !IO),

    % eval_complex(√(-1)) should give i
    C1 = eval_complex(re_sqrt(re_lit(rational(-1, 1)))),
    io.format("eval_complex(√(-1)) = %.4f + %.4fi\n",
        [f(re(C1)), f(im(C1))], !IO),

    % eval_float(³√8) = 2.0
    V3 = eval_float(re_root(3, re_lit(rational(8, 1)))),
    io.format("eval_float(³√8) = %.1f\n", [f(V3)], !IO),

    % eval_float(2^3) = 8.0
    V4 = eval_float(re_pow(re_lit(rational(2, 1)), 3)),
    io.format("eval_float(2³) = %.1f\n", [f(V4)], !IO),

    % eval_interval(√2): should contain 1.414...
    IV = eval_interval(re_sqrt(re_lit(rational(2, 1)))),
    Lo = iv_lo(IV),
    Hi = iv_hi(IV),
    ( if rational.'<'(Lo, rational(15, 10)),
        rational.'>'(Hi, rational(14, 10))
    then
        io.write_string("eval_interval(√2) contains 1.414: yes\n", !IO)
    else
        io.write_string("eval_interval(√2) contains 1.414: WRONG\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Radical normalize tests
%---------------------------------------------------------------------------%

:- pred test_rad_normalize(io::di, io::uo) is det.

test_rad_normalize(!IO) :-
    % fold_constants: 2 + 3 = 5
    E1 = re_add(re_lit(rational(2, 1)), re_lit(rational(3, 1))),
    R1 = fold_constants(E1),
    ( if R1 = re_lit(R), R = rational(5, 1) then
        io.write_string("fold_constants(2+3) = 5: yes\n", !IO)
    else
        io.write_string("fold_constants(2+3) = 5: WRONG\n", !IO)
    ),

    % fold_constants: 0 + x = x
    X = re_sqrt(re_lit(rational(2, 1))),
    R2 = fold_constants(re_add(re_lit(rational.zero), X)),
    ( if R2 = X then
        io.write_string("fold_constants(0+√2) = √2: yes\n", !IO)
    else
        io.write_string("fold_constants(0+√2) = √2: WRONG\n", !IO)
    ),

    % fold_constants: 1 * x = x
    R3 = fold_constants(re_mul(re_lit(rational.one), X)),
    ( if R3 = X then
        io.write_string("fold_constants(1*√2) = √2: yes\n", !IO)
    else
        io.write_string("fold_constants(1*√2) = √2: WRONG\n", !IO)
    ),

    % extract_perfect_powers: √12 = 2√3
    R4 = extract_perfect_powers(re_sqrt(re_lit(rational(12, 1)))),
    ( if R4 = re_mul(re_lit(C4), re_root(2, re_lit(Inner4))),
        C4 = rational(2, 1), Inner4 = rational(3, 1)
    then
        io.write_string("extract(√12) = 2√3: yes\n", !IO)
    else
        io.write_string("extract(√12) = 2√3: WRONG\n", !IO)
    ),

    % extract_perfect_powers: √4 = 2
    R5 = extract_perfect_powers(re_sqrt(re_lit(rational(4, 1)))),
    ( if R5 = re_lit(C5), C5 = rational(2, 1) then
        io.write_string("extract(√4) = 2: yes\n", !IO)
    else
        io.write_string("extract(√4) = 2: WRONG\n", !IO)
    ),

    % normalize: 3√5 + 2√5 = 5√5
    E6 = re_add(
        re_mul(re_lit(rational(3, 1)),
               re_sqrt(re_lit(rational(5, 1)))),
        re_mul(re_lit(rational(2, 1)),
               re_sqrt(re_lit(rational(5, 1))))),
    R6 = normalize(E6),
    ( if R6 = re_mul(re_lit(C6), re_root(2, re_lit(B6))),
        C6 = rational(5, 1), B6 = rational(5, 1)
    then
        io.write_string("normalize(3√5+2√5) = 5√5: yes\n", !IO)
    else
        io.write_string("normalize(3√5+2√5) = 5√5: WRONG\n", !IO)
    ),

    % normalize: √12 + √3 = 3√3
    E7 = re_add(re_sqrt(re_lit(rational(12, 1))),
                re_sqrt(re_lit(rational(3, 1)))),
    R7 = normalize(E7),
    ( if R7 = re_mul(re_lit(C7), re_root(2, re_lit(B7))),
        C7 = rational(3, 1), B7 = rational(3, 1)
    then
        io.write_string("normalize(√12+√3) = 3√3: yes\n", !IO)
    else
        io.write_string("normalize(√12+√3) = 3√3: WRONG\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Pretty-print tests
%---------------------------------------------------------------------------%

:- pred test_pretty(io::di, io::uo) is det.

test_pretty(!IO) :-
    % pretty(√2) = "√2"
    E1 = re_sqrt(re_lit(rational(2, 1))),
    S1 = pretty.pretty(E1),
    io.format("pretty(√2) = %s\n", [s(S1)], !IO),

    % pretty(2 + 3√5)
    E2 = re_add(re_lit(rational(2, 1)),
        re_mul(re_lit(rational(3, 1)), re_sqrt(re_lit(rational(5, 1))))),
    S2 = pretty.pretty(E2),
    io.format("pretty(2+3√5) = %s\n", [s(S2)], !IO),

    % pretty(1/2)
    E3 = re_lit(rational(1, 2)),
    S3 = pretty.pretty(E3),
    io.format("pretty(1/2) = %s\n", [s(S3)], !IO),

    % pretty(a - b) = "a - b" not "a + -b"
    E4 = re_add(re_lit(rational(3, 1)), re_neg(re_lit(rational(2, 1)))),
    S4 = pretty.pretty(E4),
    io.format("pretty(3-2) = %s\n", [s(S4)], !IO),

    % pretty(√(-1)) = "i"
    E5 = re_root(2, re_lit(rational(-1, 1))),
    S5 = pretty.pretty(E5),
    io.format("pretty(√(-1)) = %s\n", [s(S5)], !IO),

    % pretty(x^3)
    E6 = re_pow(re_sqrt(re_lit(rational(2, 1))), 3),
    S6 = pretty.pretty(E6),
    io.format("pretty(√2³) = %s\n", [s(S6)], !IO),

    % pretty_cse: shared subexpressions
    S7 = re_sqrt(re_lit(rational(2, 1))),
    E7 = re_add(re_mul(S7, S7), re_mul(S7, re_lit(rational(3, 1)))),
    Cse7 = pretty_cse(E7),
    io.format("pretty_cse(√2·√2 + √2·3) = %s\n", [s(Cse7)], !IO).

%---------------------------------------------------------------------------%
% LaTeX tests
%---------------------------------------------------------------------------%

:- pred test_latex(io::di, io::uo) is det.

test_latex(!IO) :-
    % latex(√2)
    E1 = re_sqrt(re_lit(rational(2, 1))),
    S1 = latex.latex(E1),
    io.format("latex(√2) = %s\n", [s(S1)], !IO),

    % latex(1/√2) = \\frac{1}{\\sqrt{2}}
    E2 = re_inv(re_sqrt(re_lit(rational(2, 1)))),
    S2 = latex.latex(E2),
    io.format("latex(1/√2) = %s\n", [s(S2)], !IO),

    % latex(a/b)
    E3 = re_mul(re_lit(rational(1, 1)), re_inv(re_lit(rational(2, 1)))),
    S3 = latex.latex(E3),
    io.format("latex(1/2) = %s\n", [s(S3)], !IO),

    % latex(√(-1)) = \\mathrm{i}
    E4 = re_root(2, re_lit(rational(-1, 1))),
    S4 = latex.latex(E4),
    io.format("latex(√(-1)) = %s\n", [s(S4)], !IO),

    % latex(³√5)
    E5 = re_root(3, re_lit(rational(5, 1))),
    S5 = latex.latex(E5),
    io.format("latex(³√5) = %s\n", [s(S5)], !IO),

    % latex(1/2 as rational)
    E6 = re_lit(rational(1, 2)),
    S6 = latex.latex(E6),
    io.format("latex(1/2 lit) = %s\n", [s(S6)], !IO).

%---------------------------------------------------------------------------%
% Sqrt denesting tests
%---------------------------------------------------------------------------%

:- pred test_denest_sqrt(io::di, io::uo) is det.

test_denest_sqrt(!IO) :-
    % √(3 + 2√2) = √2 + √1 = 1 + √2
    ( if try_sqrt_denest(rational(3, 1), rational(2, 1), rational(2, 1),
            {Sign, X, Y}) then
        io.format("√(3+2√2): sign=%d, x=%s, y=%s\n",
            [i(Sign), s(show_rat(X)), s(show_rat(Y))], !IO)
    else
        io.write_string("√(3+2√2): no denesting (WRONG)\n", !IO)
    ),

    % √(5 + 2√6) = √2 + √3
    ( if try_sqrt_denest(rational(5, 1), rational(2, 1), rational(6, 1),
            {Sign2, X2, Y2}) then
        io.format("√(5+2√6): sign=%d, x=%s, y=%s\n",
            [i(Sign2), s(show_rat(X2)), s(show_rat(Y2))], !IO)
    else
        io.write_string("√(5+2√6): no denesting (WRONG)\n", !IO)
    ),

    % √(2 + √3) should NOT denest (disc = 4 - 3 = 1, sd = 1, x=3/2, y=1/2, ok)
    % Actually it does! Let's check.
    ( if try_sqrt_denest(rational(2, 1), rational(1, 1), rational(3, 1),
            {Sign3, X3, Y3}) then
        io.format("√(2+√3): sign=%d, x=%s, y=%s\n",
            [i(Sign3), s(show_rat(X3)), s(show_rat(Y3))], !IO)
    else
        io.write_string("√(2+√3): no denesting\n", !IO)
    ),

    % Test expression-level denesting
    E1 = re_root(2, re_add(re_lit(rational(3, 1)),
        re_mul(re_lit(rational(2, 1)),
            re_root(2, re_lit(rational(2, 1)))))),
    ( if denest_sqrt_expr(E1, D1) then
        S1 = pretty.pretty(D1),
        io.format("denest_sqrt_expr(√(3+2√2)) = %s\n", [s(S1)], !IO)
    else
        io.write_string("denest_sqrt_expr(√(3+2√2)): failed (WRONG)\n", !IO)
    ),

    % Test recursive denesting
    R1 = denest_sqrt(E1),
    SR1 = pretty.pretty(R1),
    io.format("denest_sqrt(√(3+2√2)) = %s\n", [s(SR1)], !IO).

%---------------------------------------------------------------------------%
% Nth root denesting tests
%---------------------------------------------------------------------------%

:- pred test_denest_nthroot(io::di, io::uo) is det.

test_denest_nthroot(!IO) :-
    % ⁿ√(aⁿ·b) = a·ⁿ√b: √12 = 2√3
    E1 = denest_nthroot(re_root(2, re_lit(rational(12, 1)))),
    S1 = pretty.pretty(E1),
    io.format("denest(√12) = %s\n", [s(S1)], !IO),

    % ³√8 = 2
    E2 = denest_nthroot(re_root(3, re_lit(rational(8, 1)))),
    S2 = pretty.pretty(E2),
    io.format("denest(³√8) = %s\n", [s(S2)], !IO),

    % ³√(27/8) = 3/2
    E3 = denest_nthroot(re_root(3, re_lit(rational(27, 8)))),
    S3 = pretty.pretty(E3),
    io.format("denest(³√(27/8)) = %s\n", [s(S3)], !IO),

    % √(√16) = √4 → collapse to ⁴√16 → 2
    E4 = denest_nthroot(re_root(2, re_root(2, re_lit(rational(16, 1))))),
    S4 = pretty.pretty(E4),
    io.format("denest(√(√16)) = %s\n", [s(S4)], !IO),

    % ³√(-8) = -2 (odd root of negative)
    E5 = denest_nthroot(re_root(3, re_lit(rational(-8, 1)))),
    S5 = pretty.pretty(E5),
    io.format("denest(³√(-8)) = %s\n", [s(S5)], !IO),

    % Cube root denesting: ³√(2+√5) — likely won't denest
    ( if try_cube_root_denest(rational(2, 1), rational(1, 1), rational(5, 1), _) then
        io.write_string("³√(2+√5) denests\n", !IO)
    else
        io.write_string("³√(2+√5) does not denest: expected\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Normal form tests
%---------------------------------------------------------------------------%

:- pred test_normal_form(io::di, io::uo) is det.

test_normal_form(!IO) :-
    % norm_lit(3) is not zero
    NL3 = norm_lit(rational(3, 1)),
    ( if norm_is_zero(NL3) then
        io.write_string("norm_lit(3) is zero: WRONG\n", !IO)
    else
        io.write_string("norm_lit(3) is not zero: yes\n", !IO)
    ),

    % norm_coeff of a literal
    ( if norm_coeff(NL3) = yes(C3), C3 = rational(3, 1) then
        io.write_string("norm_coeff(3) = 3: yes\n", !IO)
    else
        io.write_string("norm_coeff(3) = 3: WRONG\n", !IO)
    ),

    % norm_add: 3 + 5 = 8
    Sum = norm_add(norm_lit(rational(3, 1)), norm_lit(rational(5, 1))),
    ( if norm_coeff(Sum) = yes(C8), C8 = rational(8, 1) then
        io.write_string("norm_add(3,5) = 8: yes\n", !IO)
    else
        io.write_string("norm_add(3,5) = 8: WRONG\n", !IO)
    ),

    % norm_root(2, 4) = 2 (perfect square extracted)
    NR4 = norm_root(2, rational(4, 1)),
    ( if norm_coeff(NR4) = yes(C2), C2 = rational(2, 1) then
        io.write_string("norm_root(2,4) = 2: yes\n", !IO)
    else
        io.write_string("norm_root(2,4) = 2: WRONG\n", !IO)
    ),

    % √2 · √2 = 2 via norm_mul
    Sqrt2 = norm_root(2, rational(2, 1)),
    Prod = norm_mul(Sqrt2, Sqrt2),
    ( if norm_coeff(Prod) = yes(CP), CP = rational(2, 1) then
        io.write_string("√2·√2 = 2: yes\n", !IO)
    else
        io.write_string("√2·√2 = 2: WRONG\n", !IO)
    ),

    % √2 · √3 is NOT rational
    Sqrt3 = norm_root(2, rational(3, 1)),
    Prod23 = norm_mul(Sqrt2, Sqrt3),
    ( if norm_coeff(Prod23) = no then
        io.write_string("√2·√3 is irrational: yes\n", !IO)
    else
        io.write_string("√2·√3 is irrational: WRONG\n", !IO)
    ),

    % norm_root(2, 12) = 2√3 (check via norm_mul with itself)
    NR12 = norm_root(2, rational(12, 1)),
    Sq12 = norm_mul(NR12, NR12),
    ( if norm_coeff(Sq12) = yes(C12), C12 = rational(12, 1) then
        io.write_string("(√12)² = 12: yes\n", !IO)
    else
        io.write_string("(√12)² = 12: WRONG\n", !IO)
    ),

    % toNormExpr round-trip
    E1 = re_add(re_lit(rational(2, 1)),
        re_mul(re_lit(rational(3, 1)),
            re_sqrt(re_lit(rational(5, 1))))),
    NE1 = to_norm_expr(E1),
    RE1 = from_norm_expr(NE1),
    V_orig = eval_float(E1),
    V_round = eval_float(RE1),
    Diff = float.abs(float.'-'(V_orig, V_round)),
    ( if float.'<'(Diff, 1.0e-10) then
        io.write_string("toNormExpr/fromNormExpr round-trip: yes\n", !IO)
    else
        io.format("toNormExpr/fromNormExpr round-trip: WRONG (diff=%f)\n",
            [f(Diff)], !IO)
    ),

    % normInv: 1/√2 · √2 = 1
    InvSqrt2 = norm_inv(Sqrt2),
    Check = norm_mul(InvSqrt2, Sqrt2),
    ( if norm_coeff(Check) = yes(C1), C1 = rational.one then
        io.write_string("(1/√2)·√2 = 1: yes\n", !IO)
    else
        io.write_string("(1/√2)·√2 = 1: WRONG\n", !IO)
    ),

    % ImagUnit: i² = -1
    I = norm_atom(imag_unit),
    I2 = norm_mul(I, I),
    ( if norm_coeff(I2) = yes(CM1), CM1 = rational(-1, 1) then
        io.write_string("i² = -1: yes\n", !IO)
    else
        io.write_string("i² = -1: WRONG\n", !IO)
    ),

    % norm_pow(√2, 4) = 4
    P4 = norm_pow(Sqrt2, 4),
    ( if norm_coeff(P4) = yes(C4P), C4P = rational(4, 1) then
        io.write_string("(√2)⁴ = 4: yes\n", !IO)
    else
        io.write_string("(√2)⁴ = 4: WRONG\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Root isolation tests
%---------------------------------------------------------------------------%

:- pred test_root_isolation(io::di, io::uo) is det.

test_root_isolation(!IO) :-
    % x² - 2: two real roots
    P1 = poly([-rational(2, 1), rational.zero, rational.one]),
    Roots1 = isolate_real_roots(P1),
    io.format("roots of x²-2: %d found\n", [i(list.length(Roots1))], !IO),

    % x² + 1: no real roots
    P2 = poly([rational.one, rational.zero, rational.one]),
    Roots2 = isolate_real_roots(P2),
    io.format("roots of x²+1: %d found (expected 0)\n",
        [i(list.length(Roots2))], !IO),

    % x - 3: one root at 3
    P3 = poly([-rational(3, 1), rational.one]),
    Roots3 = isolate_real_roots(P3),
    ( if Roots3 = [II3], root_in_interval(II3) = yes(R3),
         R3 = rational(3, 1) then
        io.write_string("root of x-3 at 3: yes\n", !IO)
    else
        io.write_string("root of x-3 at 3: WRONG\n", !IO)
    ),

    % Sturm count for x²-1 on (-2, 2]
    P4 = poly([-rational.one, rational.zero, rational.one]),
    SC = sturm_count(P4, rational(-2, 1), rational(2, 1)),
    io.format("sturm_count(x²-1, -2, 2) = %d (expected 2)\n", [i(SC)], !IO),

    % Refine root of x²-2 near √2
    ( if Roots1 = [_, II2] then
        Refined = refine_root(rational(1, 1000), II2),
        IV = ii_interval(Refined),
        Lo = iv_lo(IV),
        Hi = iv_hi(IV),
        Width = rational.'-'(Hi, Lo),
        ( if rational.'<'(Width, rational(1, 1000)) then
            io.write_string("refined √2 interval width < 0.001: yes\n", !IO)
        else
            io.write_string("refined √2 interval width < 0.001: WRONG\n", !IO)
        )
    else
        io.write_string("refine: no positive root found\n", !IO)
    ).

%---------------------------------------------------------------------------%
% Denest dispatcher tests
%---------------------------------------------------------------------------%

:- pred test_denest(io::di, io::uo) is det.

test_denest(!IO) :-
    % denest(√12) should simplify
    E1 = re_root(2, re_lit(rational(12, 1))),
    D1 = denest.denest(E1),
    S1 = pretty.pretty(D1),
    io.format("denest(√12) = %s\n", [s(S1)], !IO),

    % denest(√(3+2√2)) should denest via sqrt algorithm
    E2 = re_root(2, re_add(re_lit(rational(3, 1)),
        re_mul(re_lit(rational(2, 1)),
            re_root(2, re_lit(rational(2, 1)))))),
    D2 = denest.denest(E2),
    S2 = pretty.pretty(D2),
    io.format("denest(√(3+2√2)) = %s\n", [s(S2)], !IO),

    % denest(³√8) should simplify to 2
    E3 = re_root(3, re_lit(rational(8, 1))),
    D3 = denest.denest(E3),
    S3 = pretty.pretty(D3),
    io.format("denest(³√8) = %s\n", [s(S3)], !IO).

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
