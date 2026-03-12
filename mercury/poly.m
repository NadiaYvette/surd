%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: poly.m
% Main author: nyc
%
% Dense univariate polynomial arithmetic over an arbitrary field.
%
% Polynomials are represented as coefficient lists, low-degree first:
% [a0, a1, ..., an] means a0 + a1*x + ... + an*x^n.
%
% Invariant: trailing zeros are stripped (the leading coefficient is nonzero),
% except for the zero polynomial which is [].
%
% The coefficient type K must support the field typeclass (below) for
% division-based operations (div_mod, gcd, monic, square_free).
% Addition-only operations (add, sub, mul, eval, diff) require only ring.
%
%---------------------------------------------------------------------------%

:- module poly.
:- interface.

:- import_module list.
:- import_module rational.

%---------------------------------------------------------------------------%
% Typeclasses for algebraic structures
%
% Method names are prefixed with ring_/field_ to avoid overloading
% collisions with int, integer, rational, and float modules.
%---------------------------------------------------------------------------%

    % A commutative ring with identity.
    %
:- typeclass ring(K) where [
    func ring_zero = K,
    func ring_one = K,
    func ring_add(K, K) = K,
    func ring_sub(K, K) = K,
    func ring_mul(K, K) = K,
    func ring_negate(K) = K,
    func ring_from_int(int) = K,
    pred ring_is_zero(K::in) is semidet
].

    % A field: a ring with division.
    %
:- typeclass field(K) <= ring(K) where [
    func field_div(K, K) = K
].

%---------------------------------------------------------------------------%
% Polynomial type
%---------------------------------------------------------------------------%

    % A univariate polynomial with coefficients in K.
    % Stored as a list of coefficients, low-degree first.
    %
:- type poly(K)
    --->    poly(list(K)).

    % Unwrap to coefficient list.
    %
:- func coeffs(poly(K)) = list(K).

    % Smart constructor: strip trailing zeros.
    %
:- func mk_poly(list(K)) = poly(K) <= ring(K).

    % The zero polynomial.
    %
:- func zero_poly = poly(K) <= ring(K).

    % A constant polynomial.
    %
:- func const_poly(K) = poly(K) <= ring(K).

    % The polynomial x.
    %
:- func mono_x = poly(K) <= ring(K).

%---------------------------------------------------------------------------%
% Basic queries
%---------------------------------------------------------------------------%

    % Degree of the polynomial. Returns -1 for the zero polynomial.
    %
:- func degree(poly(K)) = int.

    % Leading coefficient. Fails for the zero polynomial.
    %
:- pred lead_coeff(poly(K)::in, K::out) is semidet.

    % Deterministic leading coefficient; throws for zero polynomial.
    %
:- func det_lead_coeff(poly(K)) = K.

%---------------------------------------------------------------------------%
% Arithmetic
%---------------------------------------------------------------------------%

    % Evaluate a polynomial at a point using Horner's method.
    %
:- func eval(poly(K), K) = K <= ring(K).

    % Multiply by a scalar.
    %
:- func scale(K, poly(K)) = poly(K) <= ring(K).

    % Polynomial addition.
    %
:- func add(poly(K), poly(K)) = poly(K) <= ring(K).

    % Polynomial subtraction.
    %
:- func sub(poly(K), poly(K)) = poly(K) <= ring(K).

    % Polynomial multiplication (schoolbook).
    %
:- func mul(poly(K), poly(K)) = poly(K) <= ring(K).

    % Polynomial negation.
    %
:- func neg(poly(K)) = poly(K) <= ring(K).

%---------------------------------------------------------------------------%
% Division (requires field)
%---------------------------------------------------------------------------%

    % div_mod(F, G, Q, R):
    %
    % Polynomial division with remainder: F = G*Q + R
    % and degree(R) < degree(G).
    %
:- pred div_mod(poly(K)::in, poly(K)::in, poly(K)::out, poly(K)::out)
    is det <= field(K).

    % GCD of two polynomials via the Euclidean algorithm, made monic.
    %
:- func gcd(poly(K), poly(K)) = poly(K) <= field(K).

    % Make a polynomial monic (leading coefficient 1).
    %
:- func monic(poly(K)) = poly(K) <= field(K).

%---------------------------------------------------------------------------%
% Calculus and composition
%---------------------------------------------------------------------------%

    % Formal derivative.
    %
:- func diff(poly(K)) = poly(K) <= ring(K).

    % Compose two polynomials: compose(F, G) = F(G(x)).
    %
:- func compose(poly(K), poly(K)) = poly(K) <= ring(K).

%---------------------------------------------------------------------------%
% Square-free factorisation
%---------------------------------------------------------------------------%

:- type sq_free_factor(K)
    --->    sq_free_factor(
                sff_factor       :: poly(K),
                sff_multiplicity :: int
            ).

    % Square-free factorisation via Yun's algorithm.
    % Requires characteristic 0.
    %
:- func square_free(poly(K)) = list(sq_free_factor(K)) <= field(K).

%---------------------------------------------------------------------------%
% Rational polynomial instance
%---------------------------------------------------------------------------%

:- instance ring(rational).
:- instance field(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
% Typeclass instances for rational
%---------------------------------------------------------------------------%

:- instance ring(rational) where [
    (ring_zero = rational.zero),
    (ring_one = rational.one),
    (ring_add(X, Y) = rational.'+'(X, Y)),
    (ring_sub(X, Y) = rational.'-'(X, Y)),
    (ring_mul(X, Y) = rational.'*'(X, Y)),
    (ring_negate(X) = rational.'-'(X)),
    (ring_from_int(N) = rational.rational(N)),
    (ring_is_zero(X) :- X = rational.zero)
].

:- instance field(rational) where [
    (field_div(X, Y) = rational.'/'(X, Y))
].

%---------------------------------------------------------------------------%
% Polynomial constructors
%---------------------------------------------------------------------------%

coeffs(poly(Cs)) = Cs.

mk_poly(Cs) = poly(strip_zeros(Cs)).

:- func strip_zeros(list(K)) = list(K) <= ring(K).

strip_zeros(Cs) = Stripped :-
    list.reverse(Cs, Rev),
    drop_leading_zeros(Rev, RevStripped),
    list.reverse(RevStripped, Stripped).

:- pred drop_leading_zeros(list(K)::in, list(K)::out) is det <= ring(K).

drop_leading_zeros([], []).
drop_leading_zeros([H | T], Result) :-
    ( if ring_is_zero(H) then
        drop_leading_zeros(T, Result)
    else
        Result = [H | T]
    ).

zero_poly = poly([]).

const_poly(C) =
    ( if ring_is_zero(C) then
        poly([])
    else
        poly([C])
    ).

mono_x = poly([ring_zero, ring_one]).

%---------------------------------------------------------------------------%
% Basic queries
%---------------------------------------------------------------------------%

degree(poly(Cs)) =
    ( if Cs = [] then
        -1
    else
        list.length(Cs) - 1
    ).

lead_coeff(poly(Cs), LC) :-
    Cs = [_ | _],
    list.last(Cs, LC).

det_lead_coeff(P) = LC :-
    ( if lead_coeff(P, LC0) then
        LC = LC0
    else
        unexpected($pred, "zero polynomial has no leading coefficient")
    ).

%---------------------------------------------------------------------------%
% Evaluation
%---------------------------------------------------------------------------%

eval(poly(Cs), X) = Result :-
    ( if Cs = [] then
        Result = ring_zero
    else
        list.reverse(Cs, RevCs),
        horner(RevCs, X, ring_zero, Result)
    ).

:- pred horner(list(K)::in, K::in, K::in, K::out) is det <= ring(K).

horner([], _, Acc, Acc).
horner([C | Rest], X, Acc, Result) :-
    horner(Rest, X, ring_add(C, ring_mul(X, Acc)), Result).

%---------------------------------------------------------------------------%
% Scalar multiplication
%---------------------------------------------------------------------------%

scale(S, P) =
    ( if ring_is_zero(S) then
        zero_poly
    else
        mk_poly(list.map(func(C) = ring_mul(S, C), coeffs(P)))
    ).

%---------------------------------------------------------------------------%
% Addition and subtraction
%---------------------------------------------------------------------------%

add(poly(As), poly(Bs)) = mk_poly(zip_with_add(As, Bs)).

:- func zip_with_add(list(K), list(K)) = list(K) <= ring(K).

zip_with_add([], []) = [].
zip_with_add([], [B | Bs]) = [B | zip_with_add([], Bs)].
zip_with_add([A | As], []) = [A | zip_with_add(As, [])].
zip_with_add([A | As], [B | Bs]) =
    [ring_add(A, B) | zip_with_add(As, Bs)].

sub(poly(As), poly(Bs)) = mk_poly(zip_with_sub(As, Bs)).

:- func zip_with_sub(list(K), list(K)) = list(K) <= ring(K).

zip_with_sub([], []) = [].
zip_with_sub([], [B | Bs]) =
    [ring_negate(B) | zip_with_sub([], Bs)].
zip_with_sub([A | As], []) =
    [A | zip_with_sub(As, [])].
zip_with_sub([A | As], [B | Bs]) =
    [ring_sub(A, B) | zip_with_sub(As, Bs)].

neg(poly(Cs)) = poly(list.map(ring_negate, Cs)).

%---------------------------------------------------------------------------%
% Multiplication
%---------------------------------------------------------------------------%

mul(poly(As), poly(Bs)) = Result :-
    ( if As = [] then
        Result = zero_poly
    else if Bs = [] then
        Result = zero_poly
    else
        RLen = list.length(As) + list.length(Bs) - 1,
        Zeros = list.duplicate(RLen, ring_zero),
        mul_accumulate(As, 0, Bs, Zeros, Cs),
        Result = mk_poly(Cs)
    ).

:- pred mul_accumulate(list(K)::in, int::in, list(K)::in,
    list(K)::in, list(K)::out) is det <= ring(K).

mul_accumulate([], _, _, !Acc).
mul_accumulate([A | As], I, Bs, !Acc) :-
    add_scaled_at(Bs, A, I, !Acc),
    mul_accumulate(As, I + 1, Bs, !Acc).

:- pred add_scaled_at(list(K)::in, K::in, int::in,
    list(K)::in, list(K)::out) is det <= ring(K).

add_scaled_at([], _, _, !Acc).
add_scaled_at([B | Bs], A, Pos, !Acc) :-
    add_at_index(Pos, ring_mul(A, B), !Acc),
    add_scaled_at(Bs, A, Pos + 1, !Acc).

:- pred add_at_index(int::in, K::in, list(K)::in, list(K)::out)
    is det <= ring(K).

add_at_index(Idx, Val, !Acc) :-
    list.det_replace_nth(!.Acc, Idx + 1,
        ring_add(list.det_index0(!.Acc, Idx), Val), !:Acc).

%---------------------------------------------------------------------------%
% Division
%---------------------------------------------------------------------------%

div_mod(F, G, Q, R) :-
    ( if G = poly([]) then
        unexpected($pred, "division by zero polynomial")
    else if degree(F) < degree(G) then
        Q = zero_poly,
        R = F
    else
        LC_G = det_lead_coeff(G),
        div_mod_loop(F, G, degree(G), LC_G, zero_poly, Q, R)
    ).

:- pred div_mod_loop(poly(K)::in, poly(K)::in, int::in, K::in,
    poly(K)::in, poly(K)::out, poly(K)::out) is det <= field(K).

div_mod_loop(R0, G, DG, LC_G, Q0, Q, R) :-
    ( if degree(R0) < DG then
        Q = Q0,
        R = R0
    else
        DR = degree(R0),
        LC_R = det_lead_coeff(R0),
        C = field_div(LC_R, LC_G),
        D = DR - DG,
        % Term = C * x^D
        TermCoeffs = list.duplicate(D, ring_zero) ++ [C],
        Term = poly(TermCoeffs),
        R1 = sub(R0, mul(Term, G)),
        Q1 = add(Q0, Term),
        div_mod_loop(R1, G, DG, LC_G, Q1, Q, R)
    ).

gcd(A, B) = GCD :-
    ( if B = poly([]) then
        GCD = monic(A)
    else
        div_mod(A, B, _, R),
        GCD = gcd(B, R)
    ).

monic(P) = Result :-
    ( if P = poly([]) then
        Result = zero_poly
    else
        LC = det_lead_coeff(P),
        Result = poly(list.map(func(C) = field_div(C, LC), coeffs(P)))
    ).

%---------------------------------------------------------------------------%
% Calculus and composition
%---------------------------------------------------------------------------%

diff(poly(Cs)) =
    ( if Cs = [_ | Tail] then
        mk_poly(diff_coeffs(Tail, 1))
    else
        zero_poly
    ).

:- func diff_coeffs(list(K), int) = list(K) <= ring(K).

diff_coeffs([], _) = [].
diff_coeffs([C | Cs], I) =
    [ring_mul(ring_from_int(I), C) | diff_coeffs(Cs, I + 1)].

compose(poly(Cs), G) = Result :-
    ( if Cs = [] then
        Result = zero_poly
    else
        list.reverse(Cs, RevCs),
        horner_poly(RevCs, G, zero_poly, Result)
    ).

:- pred horner_poly(list(K)::in, poly(K)::in, poly(K)::in, poly(K)::out)
    is det <= ring(K).

horner_poly([], _, Acc, Acc).
horner_poly([C | Rest], G, Acc, Result) :-
    horner_poly(Rest, G, add(const_poly(C), mul(G, Acc)), Result).

%---------------------------------------------------------------------------%
% Square-free factorisation (Yun's algorithm)
%---------------------------------------------------------------------------%

square_free(F) = Factors :-
    ( if F = poly([]) then
        Factors = []
    else
        F1 = diff(F),
        C = gcd(F, F1),
        div_mod(F, C, W, _),
        yun_loop(W, C, 1, [], RevFactors),
        list.reverse(RevFactors, Factors)
    ).

:- pred yun_loop(poly(K)::in, poly(K)::in, int::in,
    list(sq_free_factor(K))::in, list(sq_free_factor(K))::out)
    is det <= field(K).

yun_loop(W, C, I, !Acc) :-
    ( if degree(W) = 0 then
        ( if degree(C) > 0 then
            !:Acc = [sq_free_factor(C, I) | !.Acc]
        else
            true
        )
    else
        Y = gcd(W, C),
        div_mod(W, Y, Z, _),
        div_mod(C, Y, C1, _),
        ( if degree(Z) > 0 then
            !:Acc = [sq_free_factor(Z, I) | !.Acc]
        else
            true
        ),
        yun_loop(Y, C1, I + 1, !Acc)
    ).

%---------------------------------------------------------------------------%
:- end_module poly.
%---------------------------------------------------------------------------%
