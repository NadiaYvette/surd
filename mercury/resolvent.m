%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: resolvent.m
% Main author: nyc
%
% Resolvent polynomials for Galois group computation.
%
% Roots of f are approximated in C via the Aberth-Ehrlich simultaneous
% iteration, the invariant is evaluated on permuted roots, and the
% resolvent polynomial is reconstructed with rational coefficients via
% bounded-denominator rational approximation.
%
%---------------------------------------------------------------------------%

:- module resolvent.
:- interface.

:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module rad_eval.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Approximate all complex roots of a polynomial over Q using
    % the Aberth-Ehrlich simultaneous iteration method.
    %
:- func complex_roots_of(poly(rational)) = list(complex).

    % Compute a resolvent polynomial from numerical roots and an invariant.
    %
    % Given:
    %   - Numerical roots of f (from complex_roots_of)
    %   - An invariant function theta : C^n -> C
    %   - Permutations (as index lists) representing coset representatives
    %
    % Returns the resolvent polynomial with rational coefficients, or
    % no if coefficient rounding fails.
    %
:- func resolvent_from_roots(list(complex),
    (func(list(complex)) = complex),
    list(list(int))) = maybe(poly(rational)).

    % Discriminant of a polynomial over Q.
    %
    % disc(f) = (-1)^(n(n-1)/2) * Res(f,f') / lc(f)
    %
:- func discriminant_of(poly(rational)) = rational.

    % Test whether a rational number is a perfect square in Q.
    %
:- pred is_square_rational(rational::in) is semidet.

    % Test whether a polynomial over Q has at least one rational root.
    %
:- pred has_rational_root(poly(rational)::in) is semidet.

    % Bounded-denominator rational approximation.
    % Finds p/q with 1 <= q <= 10000 minimising |p/q - x|.
    %
:- func best_rational(float) = rational.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module float.
:- use_module int.
:- use_module math.
:- import_module factoring.
:- import_module integer.
:- import_module resultant.

%---------------------------------------------------------------------------%
% Numerical root finding via Aberth-Ehrlich
%---------------------------------------------------------------------------%

complex_roots_of(P) = Result :-
    D = degree(P),
    ( if int.'<'(D, 1) then
        Result = []
    else if D = 1 then
        Cs = coeffs(P),
        ( if Cs = [C0, C1] then
            X = rational.'-'(rational.zero, C0 / C1),
            Result = [complex(rational_to_float(X), 0.0)]
        else
            Result = []
        )
    else
        Result = aberth_ehrlich(P)
    ).

:- func aberth_ehrlich(poly(rational)) = list(complex).

aberth_ehrlich(P) = Result :-
    N = degree(P),
    R = cauchy_bound(P),
    Pi = math.pi,
    Z0 = list.map(
        ( func(K) = complex(
            float.'*'(R, math.cos(Ang)),
            float.'*'(R, math.sin(Ang))) :-
            Ang = float.'+'(
                float.'/'(
                    float.'*'(float.'*'(2.0, Pi), float.float(K)),
                    float.float(N)),
                0.37)
        ), 0 `..` int.'-'(N, 1)),
    DP = poly.diff(P),
    Result = ae_iterate(P, DP, Z0, N, 500).

:- func ae_iterate(poly(rational), poly(rational),
    list(complex), int, int) = list(complex).

ae_iterate(P, DP, Zs, N, Iter) = Result :-
    ( if int.'=<'(Iter, 0) then
        Result = Zs
    else
        ae_step(P, DP, Zs, N, 0, Zs, [], ZsNew0),
        list.reverse(ZsNew0, ZsNew),
        max_shift(Zs, ZsNew, 0.0, MaxShift),
        ( if float.'<'(MaxShift, 1.0e-15) then
            Result = ZsNew
        else
            Result = ae_iterate(P, DP, ZsNew, N, int.'-'(Iter, 1))
        )
    ).

:- pred ae_step(poly(rational)::in, poly(rational)::in,
    list(complex)::in, int::in, int::in,
    list(complex)::in, list(complex)::in, list(complex)::out) is det.

ae_step(_P, _DP, _AllZs, _N, _I, [], Acc, Acc).
ae_step(P, DP, AllZs, N, I, [Z | Rest], Acc0, Acc) :-
    Fz = eval_poly_complex(P, Z),
    Fpz = eval_poly_complex(DP, Z),
    ( if float.'<'(cmag(Fpz), 1.0e-300) then
        ZNew = Z
    else
        W = cdiv(Fz, Fpz),
        repulsion_sum(AllZs, Z, I, 0, complex(0.0, 0.0), S),
        Denom = csub(complex(1.0, 0.0), cmul(W, S)),
        ( if float.'<'(cmag(Denom), 1.0e-300) then
            ZNew = csub(Z, W)
        else
            ZNew = csub(Z, cdiv(W, Denom))
        )
    ),
    ae_step(P, DP, AllZs, N, int.'+'(I, 1), Rest, [ZNew | Acc0], Acc).

:- pred repulsion_sum(list(complex)::in, complex::in, int::in, int::in,
    complex::in, complex::out) is det.

repulsion_sum([], _Z, _I, _J, Acc, Acc).
repulsion_sum([Zj | Rest], Z, I, J, Acc0, Acc) :-
    ( if I = J then
        Acc1 = Acc0
    else
        Diff = csub(Z, Zj),
        ( if float.'<'(cmag(Diff), 1.0e-300) then
            Acc1 = Acc0
        else
            Acc1 = cadd(Acc0, cinv(Diff))
        )
    ),
    repulsion_sum(Rest, Z, I, int.'+'(J, 1), Acc1, Acc).

:- pred max_shift(list(complex)::in, list(complex)::in,
    float::in, float::out) is det.

max_shift([], _, M, M).
max_shift([_ | _], [], M, M).
max_shift([A | As], [B | Bs], M0, M) :-
    D = cmag(csub(A, B)),
    M1 = float.max(M0, D),
    max_shift(As, Bs, M1, M).

:- func eval_poly_complex(poly(rational), complex) = complex.

eval_poly_complex(P, Z) = Result :-
    Cs = coeffs(P),
    list.reverse(Cs, RevCs),
    ( if RevCs = [] then
        Result = complex(0.0, 0.0)
    else
        list.foldl(
            ( pred(C::in, Acc0::in, Acc1::out) is det :-
                Acc1 = cadd(complex(rational_to_float(C), 0.0),
                            cmul(Acc0, Z))
            ), RevCs, complex(0.0, 0.0), Result)
    ).

:- func cauchy_bound(poly(rational)) = float.

cauchy_bound(P) = Result :-
    Cs = coeffs(P),
    ( if list.last(Cs, AN) then
        An = float.abs(rational_to_float(AN)),
        list.det_split_last(Cs, AllButLast, _),
        max_coeff_ratio(AllButLast, An, 0.0, MaxCoeff),
        Result = float.'+'(1.0, MaxCoeff)
    else
        Result = 1.0
    ).

:- pred max_coeff_ratio(list(rational)::in, float::in,
    float::in, float::out) is det.

max_coeff_ratio([], _An, M, M).
max_coeff_ratio([C | Cs], An, M0, M) :-
    R = float.'/'(float.abs(rational_to_float(C)), An),
    M1 = float.max(M0, R),
    max_coeff_ratio(Cs, An, M1, M).

%---------------------------------------------------------------------------%
% Resolvent construction from numerical roots
%---------------------------------------------------------------------------%

resolvent_from_roots(Roots, Theta, Perms) = Result :-
    Values = list.map(
        ( func(Sigma) = Theta(list.map(
            ( func(J) = list.det_index0(Roots, J) ), Sigma))
        ), Perms),
    PolyC = poly_from_roots_c(Values),
    Result = round_poly_c(PolyC).

:- func poly_from_roots_c(list(complex)) = list(complex).

poly_from_roots_c(Roots) =
    list.foldl(mul_linear, Roots, [complex(1.0, 0.0)]).

:- func mul_linear(complex, list(complex)) = list(complex).

mul_linear(R, Cs) = Result :-
    Shifted = [complex(0.0, 0.0) | Cs],
    NR = cneg(R),
    Scaled = list.append(list.map(( func(C) = cmul(NR, C) ), Cs),
                         [complex(0.0, 0.0)]),
    zipwith_add(Shifted, Scaled, Result).

:- pred zipwith_add(list(complex)::in, list(complex)::in,
    list(complex)::out) is det.

zipwith_add([], Bs, Bs).
zipwith_add(As, [], As) :- As = [_ | _].
zipwith_add([A | As], [B | Bs], [cadd(A, B) | Cs]) :-
    zipwith_add(As, Bs, Cs).

:- func round_poly_c(list(complex)) = maybe(poly(rational)).

round_poly_c(Cs) = Result :-
    round_coeffs(Cs, [], MaybeRs),
    ( if MaybeRs = yes(Rs) then
        Result = yes(mk_poly(Rs))
    else
        Result = no
    ).

:- pred round_coeffs(list(complex)::in, list(rational)::in,
    maybe(list(rational))::out) is det.

round_coeffs([], Acc, yes(Rs)) :-
    list.reverse(Acc, Rs).
round_coeffs([complex(R, I) | Rest], Acc, Result) :-
    ( if float.'>'(float.abs(I),
            float.'*'(1.0e-4, float.max(1.0, float.abs(R)))) then
        Result = no
    else
        round_coeffs(Rest, [best_rational(R) | Acc], Result)
    ).

%---------------------------------------------------------------------------%
% Complex arithmetic helpers
%---------------------------------------------------------------------------%

:- func cdiv(complex, complex) = complex.

cdiv(complex(A, B), complex(C, D)) = complex(Re, Im) :-
    Denom = float.'+'(float.'*'(C, C), float.'*'(D, D)),
    Re = float.'/'(float.'+'(float.'*'(A, C), float.'*'(B, D)), Denom),
    Im = float.'/'(float.'-'(float.'*'(B, C), float.'*'(A, D)), Denom).

:- func cmag(complex) = float.

cmag(complex(A, B)) = math.sqrt(float.'+'(float.'*'(A, A), float.'*'(B, B))).

%---------------------------------------------------------------------------%
% Discriminant and rational tests
%---------------------------------------------------------------------------%

discriminant_of(F) = Result :-
    N = degree(F),
    Cs = coeffs(F),
    ( if list.last(Cs, LC) then
        FPrime = poly.diff(F),
        Res = poly_resultant(F, FPrime),
        E = int.'/'(int.'*'(N, int.'-'(N, 1)), 2),
        Sign = ( if int.'mod'(E, 2) = 0 then rational.one
                 else rational.'-'(rational.zero, rational.one) ),
        Result = rational.'/'(rational.'*'(Sign, Res), LC)
    else
        Result = rational.zero
    ).

is_square_rational(R) :-
    rational.'>='(R, rational.zero),
    ( if R = rational.zero then
        true
    else
        N = integer.abs(numer(R)),
        D = denom(R),
        is_square_integer(N),
        is_square_integer(D)
    ).

:- pred is_square_integer(integer::in) is semidet.

is_square_integer(N) :-
    not integer.'<'(N, integer.zero),
    ( if integer.is_zero(N) then
        true
    else
        S = integer_sqrt(N),
        S * S = N
    ).

:- func integer_sqrt(integer) = integer.

integer_sqrt(N) = Result :-
    % Use Double sqrt for initial estimate, clamped to avoid overflow.
    % For very large N, use N itself as start (Newton will converge down).
    ( if integer.'>'(N, integer(1000000000000000)) then
        Start = N
    else
        Approx0 = integer(float.truncate_to_int(
            math.sqrt(float.float(integer.det_to_int(N))))),
        % Add 1 to ensure we start >= the true sqrt (Newton goes down)
        Start = ( if integer.'<'(Approx0, integer.one) then
                      integer.one
                  else
                      Approx0 + integer.one )
    ),
    Result = isqrt_newton(N, Start).

:- func isqrt_newton(integer, integer) = integer.

isqrt_newton(N, X) = Result :-
    XPrime = (X + N // X) // integer(2),
    ( if integer.'>='(XPrime, X) then
        Result = X
    else
        Result = isqrt_newton(N, XPrime)
    ).

has_rational_root(P) :-
    rational_roots(P) = [_ | _].

best_rational(X) = Result :-
    best_rational_loop(X, 1, 10000, rational(round_to_int(X)),
        float.abs(float.'-'(X, float.float(round_to_int(X)))), Result).

:- func round_to_int(float) = int.

round_to_int(X) =
    ( if float.'>='(X, 0.0) then
        float.truncate_to_int(float.'+'(X, 0.5))
    else
        int.'-'(0, float.truncate_to_int(float.'+'(float.'-'(0.0, X), 0.5)))
    ).

:- pred best_rational_loop(float::in, int::in, int::in,
    rational::in, float::in, rational::out) is det.

best_rational_loop(X, D, MaxD, BestR, BestErr, Result) :-
    ( if int.'>'(D, MaxD) then
        Result = BestR
    else
        N = round_to_int(float.'*'(X, float.float(D))),
        Err = float.abs(float.'-'(
            float.'/'(float.float(N), float.float(D)), X)),
        ( if float.'<'(Err, BestErr) then
            NewR = from_integers(integer(N), integer(D)),
            best_rational_loop(X, int.'+'(D, 1), MaxD, NewR, Err, Result)
        else
            best_rational_loop(X, int.'+'(D, 1), MaxD, BestR, BestErr, Result)
        )
    ).

:- func rational_to_float(rational) = float.

rational_to_float(R) =
    float.'/'(
        float.float(integer.det_to_int(numer(R))),
        float.float(integer.det_to_int(denom(R)))).

%---------------------------------------------------------------------------%
:- end_module resolvent.
%---------------------------------------------------------------------------%
