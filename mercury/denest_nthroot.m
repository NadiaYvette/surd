%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: denest_nthroot.m
% Main author: nyc
%
% General nth-root denesting.
%
% 1. Simple nth-root simplification: ⁿ√(aⁿ·b) = a·ⁿ√b
% 2. Root-index reduction: ⁿ√(x^k) simplification
% 3. Nested root collapse: ᵐ√(ⁿ√x) = ᵐⁿ√x
% 4. Cube root denesting for the form ³√(a + b√c)
%
% Reference: Landau (1992), Zippel (1985)
%
%---------------------------------------------------------------------------%

:- module denest_nthroot.
:- interface.

:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Attempt to denest an nth root expression.
    %
:- func denest_nthroot(rad_expr(rational)) = rad_expr(rational).

    % Try to denest ³√(a + b√c) into p + q√c.
    % Returns the denested form, or fails.
    %
:- pred try_cube_root_denest(rational::in, rational::in, rational::in,
    rad_expr(rational)::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module integer.
:- import_module list.
:- import_module prime_factors.
:- import_module positive.
:- import_module rad_eval.
:- use_module float.
:- use_module string.

%---------------------------------------------------------------------------%
% Main denesting dispatch
%---------------------------------------------------------------------------%

denest_nthroot(Expr) = Result :-
    ( if Expr = re_root(N, re_root(M, A)) then
        % ᵐ√(ⁿ√a) = ᵐⁿ√a
        Result = denest_nthroot(re_root(int.'*'(M, N), denest_nthroot(re_root(1, A))))
    else if Expr = re_root(N, re_lit(R)) then
        ( if R = rational.zero then
            Result = re_lit(rational.zero)
        else if R = rational.one then
            Result = re_lit(rational.one)
        else if rational.'>'(R, rational.zero) then
            Result = simplify_root_of_rational(N, R)
        else if int.odd(N) then
            Result = re_neg(denest_nthroot(
                re_root(N, re_lit(rational.'-'(rational.zero, R)))))
        else
            Result = re_root(N, re_lit(R))
        )
    else if Expr = re_root(3, Inner) then
        ( if match_sqrt_nested3(Inner, A, B, C) then
            ( if try_cube_root_denest(A, B, C, Denested) then
                Result = Denested
            else
                Result = re_root(3, denest_sub(Inner))
            )
        else
            Result = re_root(3, denest_sub(Inner))
        )
    else if Expr = re_root(N, A) then
        Result = re_root(N, denest_sub(A))
    else
        Result = Expr
    ).

:- func denest_sub(rad_expr(rational)) = rad_expr(rational).

denest_sub(E) = Result :-
    ( if E = re_root(M, A) then
        Result = denest_nthroot(re_root(M, A))
    else if E = re_neg(A) then
        Result = re_neg(denest_sub(A))
    else if E = re_add(A, B) then
        Result = re_add(denest_sub(A), denest_sub(B))
    else if E = re_mul(A, B) then
        Result = re_mul(denest_sub(A), denest_sub(B))
    else if E = re_inv(A) then
        Result = re_inv(denest_sub(A))
    else if E = re_pow(A, N) then
        Result = re_pow(denest_sub(A), N)
    else
        Result = E
    ).

%---------------------------------------------------------------------------%
% Simplify ⁿ√r for rational r by extracting perfect nth powers
%---------------------------------------------------------------------------%

:- func simplify_root_of_rational(int, rational) = rad_expr(rational).

simplify_root_of_rational(N, R) = Result :-
    Num = numer(R),
    Den = denom(R),
    extract_power(N, Num, NumOut, NumRem),
    extract_power(N, Den, DenOut, DenRem),
    ( if NumOut = integer.one, DenOut = integer.one then
        Result = re_root(N, re_lit(R))
    else
        Coeff = rational.from_integers(NumOut, DenOut),
        Rem = rational.from_integers(NumRem, DenRem),
        ( if Rem = rational.one then
            Result = re_lit(Coeff)
        else
            Result = re_mul(re_lit(Coeff), re_root(N, re_lit(Rem)))
        )
    ).

%---------------------------------------------------------------------------%
% Extract the largest perfect nth power factor
%---------------------------------------------------------------------------%

:- pred extract_power(int::in, integer::in, integer::out, integer::out) is det.

extract_power(N, M, Extracted, Remainder) :-
    AbsM = integer.abs(M),
    ( if AbsM = integer.zero then
        Extracted = integer.zero,
        Remainder = integer.zero
    else if AbsM = integer.one then
        Extracted = integer.one,
        ( if integer.'<'(M, integer.zero) then
            Remainder = integer.'-'(integer.zero, integer.one)
        else
            Remainder = integer.one
        )
    else
        ( if to_positive(AbsM, Pos) then
            Factors = factorise(Pos),
            compute_power_parts(N, Factors, integer.one, integer.one,
                ExtAbs, RemAbs),
            Extracted = ExtAbs,
            ( if integer.'<'(M, integer.zero) then
                Remainder = integer.'-'(integer.zero, RemAbs)
            else
                Remainder = RemAbs
            )
        else
            Extracted = integer.one,
            Remainder = M
        )
    ).

:- pred to_positive(integer::in, positive::out) is semidet.

to_positive(I, Pos) :-
    integer.'>'(I, integer.zero),
    positive(I, Pos).

:- pred compute_power_parts(int::in, list(prime_power)::in,
    integer::in, integer::in, integer::out, integer::out) is det.

compute_power_parts(_, [], ExtAcc, RemAcc, ExtAcc, RemAcc).
compute_power_parts(N, [PP | PPs], ExtAcc, RemAcc, Ext, Rem) :-
    P = pp_prime(PP),
    E = pp_exponent(PP),
    DivE = int.'//'(E, N),
    ModE = int.mod(E, N),
    NewExt = integer.'*'(ExtAcc, integer.pow(P, integer.integer(DivE))),
    NewRem = integer.'*'(RemAcc, integer.pow(P, integer.integer(ModE))),
    compute_power_parts(N, PPs, NewExt, NewRem, Ext, Rem).

%---------------------------------------------------------------------------%
% Pattern matching: a + b√c (for cube root denesting)
%---------------------------------------------------------------------------%

:- pred match_sqrt_nested3(rad_expr(rational)::in,
    rational::out, rational::out, rational::out) is semidet.

match_sqrt_nested3(Expr, A, B, C) :-
    ( if Expr = re_add(re_lit(A0), re_mul(re_lit(B0), re_root(2, re_lit(C0)))) then
        A = A0, B = B0, C = C0
    else if Expr = re_add(re_mul(re_lit(B0), re_root(2, re_lit(C0))), re_lit(A0)) then
        A = A0, B = B0, C = C0
    else if Expr = re_add(re_lit(A0), re_root(2, re_lit(C0))) then
        A = A0, B = rational.one, C = C0
    else if Expr = re_add(re_root(2, re_lit(C0)), re_lit(A0)) then
        A = A0, B = rational.one, C = C0
    else if Expr = re_add(re_lit(A0), re_neg(re_mul(re_lit(B0), re_root(2, re_lit(C0))))) then
        A = A0, B = rational.'-'(rational.zero, B0), C = C0
    else if Expr = re_add(re_lit(A0), re_neg(re_root(2, re_lit(C0)))) then
        A = A0, B = rational.'-'(rational.zero, rational.one), C = C0
    else
        fail
    ).

%---------------------------------------------------------------------------%
% Cube root denesting: ³√(a + b√c) = p + q√c
%---------------------------------------------------------------------------%

try_cube_root_denest(A, B, C, Result) :-
    % Norm = a² - b²c; needs to be a perfect cube
    Norm = rational.'-'(rational.'*'(A, A), rational.'*'(rational.'*'(B, B), C)),
    is_rational_cube_root(Norm, CbrtNorm),
    % p is a rational root of 4x³ - 3·CbrtNorm·x - a = 0
    Three = rational(3, 1),
    Four = rational(4, 1),
    CoeffB = rational.'-'(rational.zero, rational.'*'(Three, CbrtNorm)),
    CoeffC = rational.'-'(rational.zero, A),
    rational_cube_eq_roots(Four, CoeffB, CoeffC, Candidates),
    Candidates = [P | _],
    % q² = (p² - CbrtNorm) / c
    Q2 = rational.'/'(rational.'-'(rational.'*'(P, P), CbrtNorm), C),
    is_rational_sqrt(Q2, QAbs),
    % Pick sign of q based on sign of b
    ThreePP = rational.'*'(Three, rational.'*'(P, P)),
    QQC = rational.'*'(rational.'*'(QAbs, QAbs), C),
    BTest = rational.'*'(QAbs, rational.'+'(ThreePP, QQC)),
    ( if ( rational.'>'(B, rational.zero), rational.'>='(BTest, rational.zero)
         ; rational.'<'(B, rational.zero), rational.'=<'(BTest, rational.zero) ) then
        Q = QAbs
    else
        Q = rational.'-'(rational.zero, QAbs)
    ),
    % Verify via numerical evaluation
    Original = re_root(3, re_add(re_lit(A), re_mul(re_lit(B), re_root(2, re_lit(C))))),
    Candidate = re_add(re_lit(P), re_mul(re_lit(Q), re_root(2, re_lit(C)))),
    Expected = eval_float(Original),
    Got = eval_float(Candidate),
    Diff = float.abs(float.'-'(Expected, Got)),
    float.'<'(Diff, 1.0e-10),
    Result = Candidate.

%---------------------------------------------------------------------------%
% Perfect cube root test for rationals
%---------------------------------------------------------------------------%

:- pred is_rational_cube_root(rational::in, rational::out) is semidet.

is_rational_cube_root(Q, Result) :-
    ( if Q = rational.zero then
        Result = rational.zero
    else
        N = numer(Q),
        D = denom(Q),
        AbsN = integer.abs(N),
        exact_integer_cube_root(AbsN, SN),
        exact_integer_cube_root(D, SD),
        ( if integer.'<'(N, integer.zero) then
            Result = rational.from_integers(integer.'-'(integer.zero, SN), SD)
        else
            Result = rational.from_integers(SN, SD)
        )
    ).

%---------------------------------------------------------------------------%
% Perfect square root test for rationals
%---------------------------------------------------------------------------%

:- pred is_rational_sqrt(rational::in, rational::out) is semidet.

is_rational_sqrt(Q, Result) :-
    ( if Q = rational.zero then
        Result = rational.zero
    else
        rational.'>='(Q, rational.zero),
        N = numer(Q),
        D = denom(Q),
        exact_integer_sqrt(N, SN),
        exact_integer_sqrt(D, SD),
        Result = rational.from_integers(SN, SD)
    ).

:- pred exact_integer_sqrt(integer::in, integer::out) is semidet.

exact_integer_sqrt(N, Root) :-
    integer.'>='(N, integer.zero),
    ( if N = integer.zero then
        Root = integer.zero
    else if N = integer.one then
        Root = integer.one
    else
        isqrt_newton(N, N, Root0),
        integer.'*'(Root0, Root0) = N,
        Root = Root0
    ).

:- pred isqrt_newton(integer::in, integer::in, integer::out) is det.

isqrt_newton(N, Guess, Root) :-
    Two = integer.det_from_string("2"),
    Next = integer.div(integer.'+'(Guess, integer.div(N, Guess)), Two),
    ( if integer.'>='(Next, Guess) then
        Root = Guess
    else
        isqrt_newton(N, Next, Root)
    ).

%---------------------------------------------------------------------------%
% Exact integer cube root
%---------------------------------------------------------------------------%

:- pred exact_integer_cube_root(integer::in, integer::out) is semidet.

exact_integer_cube_root(N, Root) :-
    integer.'>='(N, integer.zero),
    ( if N = integer.zero then
        Root = integer.zero
    else if N = integer.one then
        Root = integer.one
    else
        icbrt_newton(N, N, Root0),
        integer.'*'(integer.'*'(Root0, Root0), Root0) = N,
        Root = Root0
    ).

:- pred icbrt_newton(integer::in, integer::in, integer::out) is det.

icbrt_newton(N, Guess, Root) :-
    Three = integer.det_from_string("3"),
    % Next = (2*Guess + N/(Guess*Guess)) / 3
    GG = integer.'*'(Guess, Guess),
    Next = integer.div(integer.'+'(integer.'*'(integer.det_from_string("2"), Guess),
        integer.div(N, GG)), Three),
    ( if integer.'>='(Next, Guess) then
        Root = Guess
    else
        icbrt_newton(N, Next, Root)
    ).

%---------------------------------------------------------------------------%
% Rational roots of ax³ + bx + c = 0 (depressed cubic)
%---------------------------------------------------------------------------%

:- pred rational_cube_eq_roots(rational::in, rational::in, rational::in,
    list(rational)::out) is det.

rational_cube_eq_roots(A, B, C, Roots) :-
    % Generate candidates via rational root theorem
    NumC = integer.abs(numer(C)),
    DenC = denom(C),
    NumA = integer.abs(numer(A)),
    DenA = denom(A),
    CDivisors = int_divisors(integer.'*'(NumC, DenA)),
    ADivisors = int_divisors(integer.'*'(NumA, DenC)),
    Candidates = make_candidates(CDivisors, ADivisors),
    F = ( func(X) = rational.'+'(rational.'+'(
            rational.'*'(A, rational.'*'(X, rational.'*'(X, X))),
            rational.'*'(B, X)), C) ),
    Roots = list.filter(( pred(X::in) is semidet :-
        F(X) = rational.zero
    ), Candidates).

:- func make_candidates(list(integer), list(integer)) = list(rational).

make_candidates(PDs, QDs) = Result :-
    Result = list.condense(list.map(
        ( func(P) = list.condense(list.map(
            ( func(Q) = Rs :-
                ( if integer.'>'(Q, integer.zero) then
                    R1 = rational.from_integers(P, Q),
                    R2 = rational.from_integers(integer.'-'(integer.zero, P), Q),
                    Rs = [R1, R2]
                else
                    Rs = []
                )
            ), QDs))
        ), PDs)).

:- func int_divisors(integer) = list(integer).

int_divisors(N) = Result :-
    AbsN = integer.abs(N),
    ( if AbsN = integer.zero then
        Result = [integer.one]
    else
        Result = divisors_loop(integer.one, AbsN)
    ).

:- func divisors_loop(integer, integer) = list(integer).

divisors_loop(D, N) = Result :-
    ( if integer.'>'(D, N) then
        Result = []
    else if integer.mod(N, D) = integer.zero then
        Result = [D | divisors_loop(integer.'+'(D, integer.one), N)]
    else
        Result = divisors_loop(integer.'+'(D, integer.one), N)
    ).

%---------------------------------------------------------------------------%
:- end_module denest_nthroot.
%---------------------------------------------------------------------------%
