%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: test_surd.m
% Main author: nyc
%
% Comprehensive test suite for the surd Mercury port.
% Uses PASS/FAIL style output for automated verification.
%
%---------------------------------------------------------------------------%

:- module test_surd.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module denest.
:- import_module denest_nthroot.
:- import_module denest_sqrt.
:- import_module extension.
:- import_module factoring.
:- use_module float.
:- import_module integer.
:- import_module int.
:- import_module interval.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module normal_form.
:- import_module poly.
:- import_module pretty.
:- import_module rad_eval.
:- import_module rad_expr.
:- import_module rad_normalize.
:- import_module rational.
:- import_module string.

%---------------------------------------------------------------------------%
% Test harness
%---------------------------------------------------------------------------%

:- pred check(string::in, bool::in, io::di, io::uo) is det.

check(Name, yes, !IO) :-
    io.format("  PASS: %s\n", [s(Name)], !IO).
check(Name, no, !IO) :-
    io.format("  FAIL: %s\n", [s(Name)], !IO).

:- mutable(fail_count, int, 0, ground, [untrailed, attach_to_io_state]).

:- pred check_count(string::in, bool::in, io::di, io::uo) is det.

check_count(Name, Result, !IO) :-
    check(Name, Result, !IO),
    ( if Result = no then
        get_fail_count(N, !IO),
        set_fail_count(int.'+'(N, 1), !IO)
    else
        true
    ).

    % Helper: check near-equality for floats.
    %
:- pred near(float::in, float::in) is semidet.

near(A, B) :-
    float.'<'(float.abs(float.'-'(A, B)), 1.0e-10).

    % Helper: check near-equality for complex.
    %
:- pred cnear(complex::in, complex::in) is semidet.

cnear(complex(R1, I1), complex(R2, I2)) :-
    near(R1, R2),
    near(I1, I2).

%---------------------------------------------------------------------------%
% Main
%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("=== Surd Mercury Test Suite ===\n\n", !IO),

    io.write_string("--- Rational arithmetic ---\n", !IO),
    test_rational(!IO),

    io.write_string("\n--- Polynomial arithmetic ---\n", !IO),
    test_poly_arith(!IO),

    io.write_string("\n--- Polynomial GCD and division ---\n", !IO),
    test_poly_gcd(!IO),

    io.write_string("\n--- Normalize ---\n", !IO),
    test_normalize(!IO),

    io.write_string("\n--- Eval ---\n", !IO),
    test_eval(!IO),

    io.write_string("\n--- Pretty ---\n", !IO),
    test_pretty_print(!IO),

    io.write_string("\n--- Extension field ---\n", !IO),
    test_extension_field(!IO),

    io.write_string("\n--- Factoring ---\n", !IO),
    test_factoring(!IO),

    io.write_string("\n--- Normal form ---\n", !IO),
    test_normal_form_nf(!IO),

    io.write_string("\n--- Denesting ---\n", !IO),
    test_denesting(!IO),

    io.write_string("\n--- Interval ---\n", !IO),
    test_interval_arith(!IO),

    io.write_string("\n--- Rad expr structure ---\n", !IO),
    test_rad_expr_struct(!IO),

    io.write_string("\n", !IO),
    get_fail_count(Fails, !IO),
    ( if Fails = 0 then
        io.write_string("All tests passed.\n", !IO)
    else
        io.format("%d test(s) FAILED.\n", [i(Fails)], !IO)
    ).

%---------------------------------------------------------------------------%
% Rational arithmetic tests (6 tests)
%---------------------------------------------------------------------------%

:- pred test_rational(io::di, io::uo) is det.

test_rational(!IO) :-
    % Addition: 1/2 + 1/3 = 5/6
    R1 = rational(1, 2) + rational(1, 3),
    check_count("rat_add: 1/2 + 1/3 = 5/6",
        ( if R1 = rational(5, 6) then yes else no ), !IO),

    % Multiplication: 2/3 * 3/4 = 1/2
    R2 = rational(2, 3) * rational(3, 4),
    check_count("rat_mul: 2/3 * 3/4 = 1/2",
        ( if R2 = rational(1, 2) then yes else no ), !IO),

    % Subtraction: 3/4 - 1/4 = 1/2
    R3 = rational(3, 4) - rational(1, 4),
    check_count("rat_sub: 3/4 - 1/4 = 1/2",
        ( if R3 = rational(1, 2) then yes else no ), !IO),

    % Division: (2/3) / (4/3) = 1/2
    R4 = rational(2, 3) / rational(4, 3),
    check_count("rat_div: (2/3) / (4/3) = 1/2",
        ( if R4 = rational(1, 2) then yes else no ), !IO),

    % Negation: -(1/3) = -1/3
    R5 = rational.'-'(rational(1, 3)),
    check_count("rat_neg: -(1/3)",
        ( if R5 = rational(-1, 3) then yes else no ), !IO),

    % Comparisons: 1/3 < 1/2
    check_count("rat_cmp: 1/3 < 1/2",
        ( if rational.'<'(rational(1, 3), rational(1, 2)) then yes else no ),
        !IO).

%---------------------------------------------------------------------------%
% Polynomial arithmetic tests (6 tests)
%---------------------------------------------------------------------------%

:- pred test_poly_arith(io::di, io::uo) is det.

test_poly_arith(!IO) :-
    % (x+1)^2 = x^2 + 2x + 1
    P_xp1 = mk_poly([rational(1, 1), rational(1, 1)]),
    P_sq = mk_poly([rational(1, 1), rational(2, 1), rational(1, 1)]),
    check_count("poly_mul: (x+1)^2 = x^2+2x+1",
        ( if poly.mul(P_xp1, P_xp1) = P_sq then yes else no ), !IO),

    % Degree
    check_count("poly_degree: deg(x^2+2x+1) = 2",
        ( if poly.degree(P_sq) = 2 then yes else no ), !IO),

    % Addition: (x+1) + (x+1) = 2x+2
    P_2xp2 = mk_poly([rational(2, 1), rational(2, 1)]),
    check_count("poly_add: (x+1)+(x+1) = 2x+2",
        ( if poly.add(P_xp1, P_xp1) = P_2xp2 then yes else no ), !IO),

    % Subtraction: (x+1) - (x+1) = 0
    check_count("poly_sub: (x+1)-(x+1) = 0",
        ( if poly.sub(P_xp1, P_xp1) = poly.zero_poly then yes else no ),
        !IO),

    % Eval: (x^2+2x+1) at x=3 = 16
    V = poly.eval(P_sq, rational(3, 1)),
    check_count("poly_eval: (x^2+2x+1)(3) = 16",
        ( if V = rational(16, 1) then yes else no ), !IO),

    % Scale: 3*(x+1) = 3x+3
    P_3xp3 = mk_poly([rational(3, 1), rational(3, 1)]),
    check_count("poly_scale: 3*(x+1) = 3x+3",
        ( if poly.scale(rational(3, 1), P_xp1) = P_3xp3 then yes
          else no ), !IO).

%---------------------------------------------------------------------------%
% Polynomial GCD and division tests (6 tests)
%---------------------------------------------------------------------------%

:- pred test_poly_gcd(io::di, io::uo) is det.

test_poly_gcd(!IO) :-
    P_xp1 = mk_poly([rational(1, 1), rational(1, 1)]),
    P_sq = mk_poly([rational(1, 1), rational(2, 1), rational(1, 1)]),

    % GCD: gcd(x^2+2x+1, x+1) = x+1
    G = poly.gcd(P_sq, P_xp1),
    check_count("poly_gcd: gcd(x^2+2x+1, x+1) = x+1",
        ( if G = P_xp1 then yes else no ), !IO),

    % Division: (x^2+2x+1) / (x+1) = (x+1), remainder 0
    poly.div_mod(P_sq, P_xp1, Q, R),
    check_count("poly_div: (x^2+2x+1)/(x+1) quotient = x+1",
        ( if Q = P_xp1 then yes else no ), !IO),
    check_count("poly_div: (x^2+2x+1)/(x+1) remainder = 0",
        ( if R = poly.zero_poly then yes else no ), !IO),

    % Monic: 2x+2 made monic = x+1
    P_2xp2 = mk_poly([rational(2, 1), rational(2, 1)]),
    check_count("poly_monic: monic(2x+2) = x+1",
        ( if poly.monic(P_2xp2) = P_xp1 then yes else no ), !IO),

    % Derivative: d/dx(x^2+2x+1) = 2x+2
    check_count("poly_diff: d/dx(x^2+2x+1) = 2x+2",
        ( if poly.diff(P_sq) = P_2xp2 then yes else no ), !IO),

    % GCD of coprime polys: gcd(x+1, x-1) = 1
    P_xm1 = mk_poly([rational(-1, 1), rational(1, 1)]),
    G2 = poly.gcd(P_xp1, P_xm1),
    check_count("poly_gcd_coprime: gcd(x+1, x-1) = 1",
        ( if poly.degree(G2) = 0 then yes else no ), !IO).

%---------------------------------------------------------------------------%
% Normalize tests (4 tests)
%---------------------------------------------------------------------------%

:- pred test_normalize(io::di, io::uo) is det.

test_normalize(!IO) :-
    % Idempotence: normalize(normalize(E)) = normalize(E)
    E = re_add(re_lit(rational(1, 1)), re_root(2, re_lit(rational(2, 1)))),
    N1 = normalize(E),
    N2 = normalize(N1),
    check_count("normalize_idempotent",
        ( if N1 = N2 then yes else no ), !IO),

    % Constant folding: 2 + 3 = 5
    E2 = re_add(re_lit(rational(2, 1)), re_lit(rational(3, 1))),
    check_count("normalize_fold_constants: 2+3=5",
        ( if normalize(E2) = re_lit(rational(5, 1)) then yes else no ),
        !IO),

    % Double negation: --x = x
    E3 = re_neg(re_neg(re_lit(rational(7, 1)))),
    check_count("normalize_double_neg: --7=7",
        ( if normalize(E3) = re_lit(rational(7, 1)) then yes else no ),
        !IO),

    % Zero addition: 0 + x = x
    E4 = re_add(re_lit(rational.zero), re_root(2, re_lit(rational(3, 1)))),
    N4 = normalize(E4),
    check_count("normalize_add_zero: 0+sqrt(3)=sqrt(3)",
        ( if N4 = re_root(2, re_lit(rational(3, 1))) then yes else no ),
        !IO).

%---------------------------------------------------------------------------%
% Eval tests (4 tests)
%---------------------------------------------------------------------------%

:- pred test_eval(io::di, io::uo) is det.

test_eval(!IO) :-
    % eval_float: 1 + sqrt(2) ~ 2.41421356
    E = re_add(re_lit(rational(1, 1)), re_root(2, re_lit(rational(2, 1)))),
    V = eval_float(E),
    check_count("eval_float: 1+sqrt(2)",
        ( if near(V, 2.41421356237) then yes else no ), !IO),

    % eval_complex: same expression
    C = eval_complex(E),
    check_count("eval_complex: 1+sqrt(2) real part",
        ( if near(re(C), 2.41421356237) then yes else no ), !IO),

    % eval_complex: sqrt(-1) = i
    Ci = eval_complex(re_root(2, re_lit(rational(-1, 1)))),
    check_count("eval_complex: sqrt(-1) = i",
        ( if cnear(Ci, complex(0.0, 1.0)) then yes else no ), !IO),

    % eval consistency: eval(E) = eval(normalize(E))
    N = normalize(E),
    V2 = eval_float(N),
    check_count("eval_consistency: eval(E) ~ eval(normalize(E))",
        ( if near(V, V2) then yes else no ), !IO).

%---------------------------------------------------------------------------%
% Pretty-printing tests (2 tests)
%---------------------------------------------------------------------------%

:- pred test_pretty_print(io::di, io::uo) is det.

test_pretty_print(!IO) :-
    % Basic expression renders to non-empty string
    E = re_add(re_lit(rational(1, 1)), re_root(2, re_lit(rational(2, 1)))),
    S = pretty(E),
    check_count("pretty_nonempty",
        ( if S \= "" then yes else no ), !IO),

    % sqrt(-1) renders as "i"
    Si = pretty(re_root(2, re_lit(rational(-1, 1)))),
    check_count("pretty_sqrt_neg1_is_i",
        ( if Si = "i" then yes else no ), !IO).

%---------------------------------------------------------------------------%
% Extension field tests (4 tests)
%---------------------------------------------------------------------------%

:- pred test_extension_field(io::di, io::uo) is det.

test_extension_field(!IO) :-
    % Q(sqrt(2)): minimal poly x^2 - 2
    MP = mk_poly([rational(-2, 1), rational(0, 1), rational(1, 1)]),
    F = mk_ext_field(MP, "α"),
    Alpha = generator(F),
    One = embed(F, rational(1, 1)),

    % α * α = 2 (i.e. ext_mul(α, α) = embed(2))
    A2 = ext_mul(Alpha, Alpha),
    Two = embed(F, rational(2, 1)),
    check_count("ext_mul: α*α = 2 in Q(√2)",
        ( if ext_eq(A2, Two) then yes else no ), !IO),

    % α + α = 2α
    TwoAlpha = ext_add(Alpha, Alpha),
    TwoAlphaExpected = ext_mul(embed(F, rational(2, 1)), Alpha),
    check_count("ext_add: α+α = 2α in Q(√2)",
        ( if ext_eq(TwoAlpha, TwoAlphaExpected) then yes else no ), !IO),

    % (1 + α) * (1 - α) = 1 - 2 = -1
    OnePlusA = ext_add(One, Alpha),
    OneMinusA = ext_sub(One, Alpha),
    Prod = ext_mul(OnePlusA, OneMinusA),
    NegOne = embed(F, rational(-1, 1)),
    check_count("ext_mul: (1+α)(1-α) = -1 in Q(√2)",
        ( if ext_eq(Prod, NegOne) then yes else no ), !IO),

    % Inverse: α^{-1} * α = 1
    AlphaInv = ext_inv(Alpha),
    ProdInv = ext_mul(AlphaInv, Alpha),
    check_count("ext_inv: α^{-1}*α = 1 in Q(√2)",
        ( if ext_eq(ProdInv, One) then yes else no ), !IO).

%---------------------------------------------------------------------------%
% Factoring tests (4 tests)
%---------------------------------------------------------------------------%

:- pred test_factoring(io::di, io::uo) is det.

test_factoring(!IO) :-
    % Factor x^2 - 1 = (x-1)(x+1): two irreducible factors
    XSqM1 = mk_poly([rational(-1, 1), rational(0, 1), rational(1, 1)]),
    Fs = factor_square_free(XSqM1),
    check_count("factor_x2m1: x^2-1 has 2 factors",
        ( if list.length(Fs) = 2 then yes else no ), !IO),

    % Rational roots of x^2 - 1 are +1 and -1
    Roots = rational_roots(XSqM1),
    check_count("rational_roots_x2m1: roots are 1 and -1",
        ( if list.length(Roots) = 2,
             list.member(rational(1, 1), Roots),
             list.member(rational(-1, 1), Roots)
        then yes else no ), !IO),

    % x^2 + 1 is irreducible over Q
    XSqP1 = mk_poly([rational(1, 1), rational(0, 1), rational(1, 1)]),
    check_count("irreducible_x2p1: x^2+1 is irreducible",
        ( if is_irreducible(XSqP1) then yes else no ), !IO),

    % Factor x^2 - 4 = (x-2)(x+2): roots are +2 and -2
    XSqM4 = mk_poly([rational(-4, 1), rational(0, 1), rational(1, 1)]),
    Roots2 = rational_roots(XSqM4),
    check_count("rational_roots_x2m4: roots are 2 and -2",
        ( if list.length(Roots2) = 2,
             list.member(rational(2, 1), Roots2),
             list.member(rational(-2, 1), Roots2)
        then yes else no ), !IO).

%---------------------------------------------------------------------------%
% Normal form tests (4 tests)
%---------------------------------------------------------------------------%

:- pred test_normal_form_nf(io::di, io::uo) is det.

test_normal_form_nf(!IO) :-
    % Round-trip: from_norm_expr(to_norm_expr(E)) normalises correctly
    E1 = re_add(re_root(2, re_lit(rational(2, 1))),
                re_root(2, re_lit(rational(2, 1)))),
    NF1 = to_norm_expr(E1),
    RT1 = from_norm_expr(NF1),
    V_orig = eval_float(E1),
    V_rt = eval_float(RT1),
    check_count("nf_roundtrip: 2*sqrt(2) value preserved",
        ( if near(V_orig, V_rt) then yes else no ), !IO),

    % Normal form recognises sqrt(2)*sqrt(2) = 2
    E2 = re_mul(re_root(2, re_lit(rational(2, 1))),
                re_root(2, re_lit(rational(2, 1)))),
    NF2 = to_norm_expr(E2),
    RT2 = from_norm_expr(NF2),
    check_count("nf_sqrt2_sq: sqrt(2)*sqrt(2) = 2",
        ( if near(eval_float(RT2), 2.0) then yes else no ), !IO),

    % to_norm_expr(0) = norm_zero
    check_count("nf_zero: to_norm_expr(0) is zero",
        ( if norm_is_zero(to_norm_expr(re_lit(rational.zero)))
          then yes else no ), !IO),

    % NF arithmetic: norm_add(1, 2) = 3
    NF_1 = norm_lit(rational(1, 1)),
    NF_2 = norm_lit(rational(2, 1)),
    NF_3 = norm_add(NF_1, NF_2),
    RT3 = from_norm_expr(NF_3),
    check_count("nf_add: 1+2=3",
        ( if near(eval_float(RT3), 3.0) then yes else no ), !IO).

%---------------------------------------------------------------------------%
% Denesting tests (4 tests)
%---------------------------------------------------------------------------%

:- pred test_denesting(io::di, io::uo) is det.

test_denesting(!IO) :-
    % sqrt(3 + 2*sqrt(2)) = 1 + sqrt(2)
    Inner = re_add(re_lit(rational(3, 1)),
                   re_mul(re_lit(rational(2, 1)),
                          re_root(2, re_lit(rational(2, 1))))),
    Expr = re_root(2, Inner),
    ( if denest_sqrt_expr(Expr, Denested) then
        V_orig = eval_float(Expr),
        V_den = eval_float(Denested),
        check_count("denest_sqrt_3p2sqrt2: value match",
            ( if near(V_orig, V_den) then yes else no ), !IO)
    else
        check_count("denest_sqrt_3p2sqrt2: denesting succeeded", no, !IO)
    ),

    % try_sqrt_denest: a=3, b=2, r=2
    ( if try_sqrt_denest(rational(3, 1), rational(2, 1), rational(2, 1),
            {_, X, Y}) then
        % X + Y = 3
        Sum = rational.'+'(X, Y),
        check_count("try_sqrt_denest: X+Y=3",
            ( if Sum = rational(3, 1) then yes else no ), !IO)
    else
        check_count("try_sqrt_denest: succeeded", no, !IO)
    ),

    % denest_sqrt function (recursive denesting)
    Denested2 = denest_sqrt.denest_sqrt(Expr),
    V2_orig = eval_float(Expr),
    V2_den = eval_float(Denested2),
    check_count("denest_sqrt_recursive: value match",
        ( if near(V2_orig, V2_den) then yes else no ), !IO),

    % Top-level denest preserves value
    E3 = re_root(2, re_add(re_lit(rational(5, 1)),
                            re_mul(re_lit(rational(2, 1)),
                                   re_root(2, re_lit(rational(6, 1)))))),
    D3 = denest(E3),
    check_count("denest_5p2sqrt6: value match",
        ( if near(eval_float(E3), eval_float(D3)) then yes else no ), !IO).

%---------------------------------------------------------------------------%
% Interval arithmetic tests (4 tests)
%---------------------------------------------------------------------------%

:- pred test_interval_arith(io::di, io::uo) is det.

test_interval_arith(!IO) :-
    I1 = interval(rational(1, 1), rational(2, 1)),
    I2 = interval(rational(3, 1), rational(4, 1)),

    % Addition: [1,2] + [3,4] = [4,6]
    IS = iadd(I1, I2),
    check_count("interval_add: [1,2]+[3,4]=[4,6]",
        ( if iv_lo(IS) = rational(4, 1), iv_hi(IS) = rational(6, 1)
          then yes else no ), !IO),

    % Multiplication: [1,2] * [3,4] = [3,8]
    IP = imul(I1, I2),
    check_count("interval_mul: [1,2]*[3,4]=[3,8]",
        ( if iv_lo(IP) = rational(3, 1), iv_hi(IP) = rational(8, 1)
          then yes else no ), !IO),

    % Contains: 3/2 in [1,2]
    check_count("interval_contains: 3/2 in [1,2]",
        ( if contains(I1, rational(3, 2)) then yes else no ), !IO),

    % Eval interval width is positive (interval is valid)
    E = re_add(re_lit(rational(1, 1)), re_root(2, re_lit(rational(2, 1)))),
    IV = eval_interval(E),
    check_count("eval_interval_valid: width >= 0 for 1+sqrt(2)",
        ( if rational.'>='(width(IV), rational.zero) then yes else no ),
        !IO).

%---------------------------------------------------------------------------%
% Radical expression structure tests (4 tests)
%---------------------------------------------------------------------------%

:- pred test_rad_expr_struct(io::di, io::uo) is det.

test_rad_expr_struct(!IO) :-
    E = re_add(re_lit(rational(1, 1)),
               re_mul(re_lit(rational(2, 1)),
                      re_root(2, re_lit(rational(3, 1))))),

    % re_depth
    check_count("re_depth: 1 + 2*sqrt(3)",
        ( if int.'>='(re_depth(E), 2) then yes else no ), !IO),

    % re_size
    check_count("re_size: positive",
        ( if int.'>'(re_size(E), 0) then yes else no ), !IO),

    % collect_radicals finds sqrt(3)
    Rads = collect_radicals(E),
    check_count("collect_radicals: finds sqrt(3)",
        ( if list.member({2, re_lit(rational(3, 1))}, Rads)
          then yes else no ), !IO),

    % re_map_coeffs: multiply all coefficients by 2
    E2 = re_map_coeffs(( func(R) = rational.'*'(R, rational(2, 1)) ),
                       re_lit(rational(3, 1))),
    check_count("re_map_coeffs: 3 -> 6",
        ( if E2 = re_lit(rational(6, 1)) then yes else no ), !IO).

%---------------------------------------------------------------------------%
:- end_module test_surd.
%---------------------------------------------------------------------------%
