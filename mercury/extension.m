%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: extension.m
% Main author: nyc
%
% Arithmetic in simple algebraic extension fields K(α),
% where α is a root of an irreducible polynomial over K.
%
% Elements of K(α) are represented as polynomials in α of degree
% less than the degree of the minimal polynomial.
%
%---------------------------------------------------------------------------%

:- module extension.
:- interface.

:- import_module poly.

%---------------------------------------------------------------------------%

    % A simple algebraic extension K(α)/K defined by the minimal
    % polynomial of α over K.
    %
:- type ext_field(K)
    --->    ext_field(
                gen_min_poly :: poly(K),
                ext_degree   :: int,
                ext_name     :: string
            ).

    % An element of an extension field, represented as a polynomial
    % in the generator of degree < ext_degree.
    %
:- type ext_elem(K)
    --->    ext_elem(
                elem_poly  :: poly(K),
                elem_field :: ext_field(K)
            ).

    % Construct an extension field from an irreducible polynomial.
    %
:- func mk_ext_field(poly(K), string) = ext_field(K) <= field(K).

    % Embed a base field element into the extension.
    %
:- func embed(ext_field(K), K) = ext_elem(K) <= ring(K).

    % The generator α of the extension.
    %
:- func generator(ext_field(K)) = ext_elem(K) <= ring(K).

    % Extension field arithmetic.
    %
:- func ext_add(ext_elem(K), ext_elem(K)) = ext_elem(K) <= field(K).
:- func ext_sub(ext_elem(K), ext_elem(K)) = ext_elem(K) <= field(K).
:- func ext_neg(ext_elem(K)) = ext_elem(K) <= ring(K).
:- func ext_mul(ext_elem(K), ext_elem(K)) = ext_elem(K) <= field(K).
:- func ext_inv(ext_elem(K)) = ext_elem(K) <= field(K).
:- func ext_div(ext_elem(K), ext_elem(K)) = ext_elem(K) <= field(K).
:- func ext_pow(ext_elem(K), int) = ext_elem(K) <= field(K).

    % Equality test for extension elements.
    %
:- pred ext_eq(ext_elem(K)::in, ext_elem(K)::in) is semidet <= ring(K).

    % Check if a field is the sentinel (placeholder for literals).
    %
:- pred is_sentinel_field(ext_field(K)::in) is semidet.

    % Extended Euclidean algorithm for polynomials.
    % Returns {G, S, T} such that G = S*A + T*B, with G monic.
    %
:- func ext_gcd(poly(K), poly(K)) = {poly(K), poly(K), poly(K)}
    <= field(K).

    % ring/field instances for ext_elem(K), enabling field tower nesting.
    %
:- instance ring(ext_elem(K)) <= field(K).
:- instance field(ext_elem(K)) <= field(K).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module list.
:- import_module require.

%---------------------------------------------------------------------------%

mk_ext_field(P, Name) = ext_field(monic(P), degree(monic(P)), Name).

embed(Field, C) = ext_elem(const_poly(C), Field).

generator(Field) = ext_elem(mono_x, Field).

%---------------------------------------------------------------------------%
% Reduction modulo minimal polynomial
%---------------------------------------------------------------------------%

:- func reduce(ext_field(K), poly(K)) = poly(K) <= field(K).

reduce(Field, P) = Result :-
    ( if is_sentinel_field(Field) then
        Result = P
    else
        div_mod(P, gen_min_poly(Field), _, Result)
    ).

%---------------------------------------------------------------------------%
% Arithmetic
%---------------------------------------------------------------------------%

ext_add(ext_elem(A, F), ext_elem(B, _)) =
    ext_elem(reduce(F, add(A, B)), F).

ext_sub(ext_elem(A, F), ext_elem(B, _)) =
    ext_elem(reduce(F, sub(A, B)), F).

ext_neg(ext_elem(A, F)) =
    ext_elem(neg(A), F).

ext_mul(ext_elem(A, F), ext_elem(B, _)) =
    ext_elem(reduce(F, mul(A, B)), F).

ext_inv(ext_elem(A, F)) = Result :-
    ( if coeffs(A) = [] then
        unexpected($pred, "division by zero in extension field")
    else
        {_, S, _} = ext_gcd(A, gen_min_poly(F)),
        Result = ext_elem(reduce(F, S), F)
    ).

ext_div(A, B) = ext_mul(A, ext_inv(B)).

ext_pow(E, N) = Result :-
    ( if N = 0 then
        Result = embed(elem_field(E), ring_one)
    else if int.'<'(N, 0) then
        Result = ext_pow(ext_inv(E), int.'-'(0, N))
    else if int.even(N) then
        Half = ext_pow(E, int.'//'(N, 2)),
        Result = ext_mul(Half, Half)
    else
        Result = ext_mul(E, ext_pow(E, int.'-'(N, 1)))
    ).

ext_eq(ext_elem(A, _), ext_elem(B, _)) :-
    coeffs(A) = coeffs(B).

%---------------------------------------------------------------------------%
% Sentinel field
%---------------------------------------------------------------------------%

:- func sentinel_field = ext_field(K) <= ring(K).

sentinel_field = ext_field(zero_poly, 0, "<literal>").

is_sentinel_field(F) :- ext_degree(F) = 0.

:- func pick_field(ext_field(K), ext_field(K)) = ext_field(K).

pick_field(F, G) =
    ( if is_sentinel_field(F) then G else F ).

%---------------------------------------------------------------------------%
% Extended GCD
%---------------------------------------------------------------------------%

ext_gcd(A, B) = Result :-
    ext_gcd_loop(A, B, const_poly(ring_one), zero_poly,
        zero_poly, const_poly(ring_one), Result).

:- pred ext_gcd_loop(poly(K)::in, poly(K)::in,
    poly(K)::in, poly(K)::in, poly(K)::in, poly(K)::in,
    {poly(K), poly(K), poly(K)}::out) is det <= field(K).

ext_gcd_loop(R0, R1, S0, S1, T0, T1, Result) :-
    ( if coeffs(R1) = [] then
        ( if lead_coeff(R0, LC) then
            InvLC = field_div(ring_one, LC),
            G = scale(InvLC, R0),
            S = scale(InvLC, S0),
            T = scale(InvLC, T0)
        else
            G = zero_poly,
            S = S0,
            T = T0
        ),
        Result = {G, S, T}
    else
        div_mod(R0, R1, Q, R),
        S2 = sub(S0, mul(Q, S1)),
        T2 = sub(T0, mul(Q, T1)),
        ext_gcd_loop(R1, R, S1, S2, T1, T2, Result)
    ).

%---------------------------------------------------------------------------%
% Typeclass instances
%---------------------------------------------------------------------------%

:- instance ring(ext_elem(K)) <= field(K) where [
    (ring_zero = ext_elem(zero_poly, sentinel_field)),
    (ring_one = ext_elem(const_poly(ring_one), sentinel_field)),
    (ring_add(A, B) = R :-
        ext_elem(PA, FA) = A,
        ext_elem(PB, FB) = B,
        F = pick_field(FA, FB),
        R = ext_elem(reduce(F, add(PA, PB)), F)),
    (ring_sub(A, B) = R :-
        ext_elem(PA, FA) = A,
        ext_elem(PB, FB) = B,
        F = pick_field(FA, FB),
        R = ext_elem(reduce(F, sub(PA, PB)), F)),
    (ring_mul(A, B) = R :-
        ext_elem(PA, FA) = A,
        ext_elem(PB, FB) = B,
        F = pick_field(FA, FB),
        R = ext_elem(reduce(F, mul(PA, PB)), F)),
    (ring_negate(ext_elem(P, F)) = ext_elem(neg(P), F)),
    (ring_from_int(N) =
        ext_elem(const_poly(ring_from_int(N)), sentinel_field)),
    (ring_is_zero(ext_elem(P, _)) :- coeffs(P) = [])
].

:- instance field(ext_elem(K)) <= field(K) where [
    (field_div(A, B) = ext_div(A, B))
].

%---------------------------------------------------------------------------%
:- end_module extension.
%---------------------------------------------------------------------------%
