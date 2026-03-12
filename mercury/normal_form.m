%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: normal_form.m
% Main author: nyc
%
% Normal form for radical expressions: Q-linear combinations of
% products of radical atoms.
%
% Every expression in Q[√2, √3, ∛5, ...] has a unique representation
% as a sum of (rational coefficient × monomial), where each monomial
% is a product of radical atoms raised to bounded powers.
%
% This gives automatic like-term collection, canonical ordering, and
% reliable structural equality.
%
%---------------------------------------------------------------------------%

:- module normal_form.
:- interface.

:- import_module map.
:- import_module maybe.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%
% Types
%---------------------------------------------------------------------------%

    % A radical atom: an irreducible nth root of a positive rational.
    % - rat_root(N, R): principal nth root of R (R > 0, nth-power-free)
    % - imag_unit: √(-1) = i, kept separate since i² = -1
    % - nested_root(N, E): ⁿ√E where E is a non-rational expression
    %
:- type atom
    --->    rat_root(int, rational)
    ;       imag_unit
    ;       nested_root(int, rad_expr(rational)).

    % A monomial: product of atoms raised to positive powers.
    % Empty map = multiplicative identity (1).
    %
:- type monomial
    --->    monomial(map(atom, int)).

    % A normalized expression: Q-linear combination of monomials.
    % Invariant: no zero coefficients in the map.
    %
:- type norm_expr
    --->    norm_expr(map(monomial, rational)).

%---------------------------------------------------------------------------%
% Construction
%---------------------------------------------------------------------------%

:- func norm_zero = norm_expr.
:- func norm_lit(rational) = norm_expr.
:- func norm_atom(atom) = norm_expr.
:- func norm_root(int, rational) = norm_expr.

%---------------------------------------------------------------------------%
% Arithmetic
%---------------------------------------------------------------------------%

:- func norm_add(norm_expr, norm_expr) = norm_expr.
:- func norm_sub(norm_expr, norm_expr) = norm_expr.
:- func norm_neg(norm_expr) = norm_expr.
:- func norm_mul(norm_expr, norm_expr) = norm_expr.
:- func norm_scale(rational, norm_expr) = norm_expr.
:- func norm_pow(norm_expr, int) = norm_expr.
:- func norm_inv(norm_expr) = norm_expr.

%---------------------------------------------------------------------------%
% Conversion
%---------------------------------------------------------------------------%

:- func to_norm_expr(rad_expr(rational)) = norm_expr.
:- func from_norm_expr(norm_expr) = rad_expr(rational).

%---------------------------------------------------------------------------%
% Queries
%---------------------------------------------------------------------------%

:- pred norm_is_zero(norm_expr::in) is semidet.
:- func norm_coeff(norm_expr) = maybe(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- use_module int.
:- import_module integer.
:- import_module list.
:- import_module pair.
:- import_module positive.
:- import_module prime_factors.
:- import_module rad_eval.
:- use_module float.
:- use_module math.
:- use_module require.
:- use_module string.

%---------------------------------------------------------------------------%
% Unit monomial
%---------------------------------------------------------------------------%

:- func unit_mono = monomial.
unit_mono = monomial(map.init).

%---------------------------------------------------------------------------%
% Construction
%---------------------------------------------------------------------------%

norm_zero = norm_expr(map.init).

norm_lit(R) = Result :-
    ( if R = rational.zero then
        Result = norm_zero
    else
        Result = norm_expr(map.singleton(unit_mono, R))
    ).

norm_atom(A) = norm_expr(map.singleton(
    monomial(map.singleton(A, 1)), rational.one)).

norm_root(N, R) = Result :-
    ( if R = rational.zero then
        Result = norm_zero
    else if R = rational.one then
        Result = norm_lit(rational.one)
    else if rational.'<'(R, rational.zero), N = 2 then
        % √(-r) = i · √r
        Result = norm_mul(norm_atom(imag_unit),
            norm_root(2, rational.'-'(rational.zero, R)))
    else if rational.'<'(R, rational.zero), int.even(N) then
        Result = norm_mul(norm_atom(imag_unit),
            norm_root(N, rational.'-'(rational.zero, R)))
    else if rational.'<'(R, rational.zero), int.odd(N) then
        Result = norm_neg(norm_root(N, rational.'-'(rational.zero, R)))
    else
        Num = numer(R),
        Den = denom(R),
        extract_nth_power(N, integer.abs(Num), NumOut, NumIn),
        extract_nth_power(N, Den, DenOut, DenIn),
        % Rationalize: ⁿ√(a/b) = (1/b)·ⁿ√(a·b^(n-1))
        Coeff = rational.'/'(NumOut,
            rational.'*'(DenOut, DenIn)),
        Radicand = rational.'*'(NumIn,
            rat_int_pow(DenIn, int.'-'(N, 1))),
        ( if Radicand = rational.one then
            Result = norm_lit(Coeff)
        else
            Result = norm_scale(Coeff,
                norm_atom(rat_root(N, Radicand)))
        )
    ).

:- func rat_int_pow(rational, int) = rational.

rat_int_pow(R, N) = Result :-
    ( if N = 0 then
        Result = rational.one
    else if int.'<'(N, 0) then
        Result = rational.'/'(rational.one, rat_int_pow(R, int.'-'(0, N)))
    else if int.even(N) then
        Half = rat_int_pow(R, int.'//'(N, 2)),
        Result = rational.'*'(Half, Half)
    else
        Result = rational.'*'(R, rat_int_pow(R, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Extract largest nth power factor from an integer
%---------------------------------------------------------------------------%

:- pred extract_nth_power(int::in, integer::in,
    rational::out, rational::out) is det.

extract_nth_power(N, M, Extracted, Remainder) :-
    ( if M = integer.zero then
        Extracted = rational.zero,
        Remainder = rational.zero
    else if M = integer.one then
        Extracted = rational.one,
        Remainder = rational.one
    else
        ( if to_positive_int(M, Pos) then
            Factors = factorise(Pos),
            compute_power_parts(N, Factors, integer.one, integer.one,
                ExtI, RemI),
            Extracted = rational.from_integers(ExtI, integer.one),
            Remainder = rational.from_integers(RemI, integer.one)
        else
            Extracted = rational.one,
            Remainder = rational.from_integers(M, integer.one)
        )
    ).

:- pred to_positive_int(integer::in, positive::out) is semidet.

to_positive_int(I, Pos) :-
    integer.'>'(I, integer.zero),
    positive(I, Pos).

:- pred compute_power_parts(int::in, list(prime_power)::in,
    integer::in, integer::in, integer::out, integer::out) is det.

compute_power_parts(N, Factors, ExtAcc, RemAcc, Ext, Rem) :-
    ( if Factors = [] then
        Ext = ExtAcc, Rem = RemAcc
    else if Factors = [PP | PPs] then
        P = pp_prime(PP),
        E = pp_exponent(PP),
        DivE = int.'//'(E, N),
        ModE = int.mod(E, N),
        NewExt = integer.'*'(ExtAcc, integer.pow(P, integer.integer(DivE))),
        NewRem = integer.'*'(RemAcc, integer.pow(P, integer.integer(ModE))),
        compute_power_parts(N, PPs, NewExt, NewRem, Ext, Rem)
    else
        Ext = ExtAcc, Rem = RemAcc
    ).

%---------------------------------------------------------------------------%
% Arithmetic
%---------------------------------------------------------------------------%

norm_add(norm_expr(A), norm_expr(B)) =
    norm_expr(map_filter_nonzero(
        map_union_with(rat_add, A, B))).

norm_sub(A, B) = norm_add(A, norm_neg(B)).

norm_neg(norm_expr(M)) =
    norm_expr(map.map_values(( func(_, V) = rational.'-'(rational.zero, V) ), M)).

norm_scale(C, norm_expr(M)) = Result :-
    ( if C = rational.zero then
        Result = norm_zero
    else
        Result = norm_expr(map_filter_nonzero(
            map.map_values(( func(_, V) = rational.'*'(C, V) ), M)))
    ).

norm_mul(norm_expr(A), norm_expr(B)) = Result :-
    % For each pair (m1,c1) from A and (m2,c2) from B,
    % multiply and reduce, then sum all results.
    Result = map.foldl(
        ( func(M1, C1, Acc1) =
            map.foldl(
                ( func(M2, C2, Acc2) =
                    norm_add(Acc2, mul_mono_coeff(M1, C1, M2, C2))
                ), B, Acc1)
        ), A, norm_zero).

:- func mul_mono_coeff(monomial, rational, monomial, rational) = norm_expr.

mul_mono_coeff(monomial(M1), C1, monomial(M2), C2) = Result :-
    Merged = map_union_with(int_add, M1, M2),
    Coeff = rational.'*'(C1, C2),
    Result = reduce_monomial(monomial(Merged), Coeff).

:- func int_add(int, int) = int.
int_add(A, B) = int.'+'(A, B).

:- func rat_add(rational, rational) = rational.
rat_add(A, B) = rational.'+'(A, B).

%---------------------------------------------------------------------------%
% Monomial reduction
%---------------------------------------------------------------------------%

:- func reduce_monomial(monomial, rational) = norm_expr.

reduce_monomial(monomial(Atoms), Coeff) = Result :-
    map.foldl2(reduce_atom, Atoms,
        Coeff, Coeff1, map.init, Atoms1),
    ( if Coeff1 = rational.zero then
        Result = norm_zero
    else
        Result = reduce_nested_roots(Coeff1, Atoms1)
    ).

:- pred reduce_atom(atom::in, int::in,
    rational::in, rational::out,
    map(atom, int)::in, map(atom, int)::out) is det.

reduce_atom(Atom, E, !C, !M) :-
    ( if Atom = imag_unit then
        E1 = int.mod(E, 4),
        ( if E1 = 0 then
            true
        else if E1 = 1 then
            map.det_insert(imag_unit, 1, !M)
        else if E1 = 2 then
            !:C = rational.'-'(rational.zero, !.C)
        else
            % E1 = 3: i³ = -i
            !:C = rational.'-'(rational.zero, !.C),
            map.det_insert(imag_unit, 1, !M)
        )
    else if Atom = rat_root(N, R) then
        Full = int.div(E, N),
        Rem = int.mod(E, N),
        !:C = rational.'*'(!.C, rat_int_pow(R, Full)),
        ( if Rem \= 0 then
            map.det_insert(Atom, Rem, !M)
        else
            true
        )
    else if Atom = nested_root(_, _) then
        ( if E = 0 then
            true
        else
            map.det_insert(Atom, E, !M)
        )
    else
        true
    ).

:- func reduce_nested_roots(rational, map(atom, int)) = norm_expr.

reduce_nested_roots(Coeff, Atoms) = Result :-
    % Find any NestedRoot with exponent >= its root index
    ( if find_reducible_nested(Atoms, nested_root(N, Inner), E) then
        Full = int.'//'(E, N),
        Rem = int.mod(E, N),
        ( if Rem = 0 then
            Atoms1 = map.delete(Atoms, nested_root(N, Inner))
        else
            Atoms1 = map.set(Atoms, nested_root(N, Inner), Rem)
        ),
        Base = norm_expr(map.singleton(monomial(Atoms1), Coeff)),
        InnerNorm = norm_pow(to_norm_expr(Inner), Full),
        Result = norm_mul(Base, InnerNorm)
    else
        Result = norm_expr(map.singleton(monomial(Atoms), Coeff))
    ).

:- pred find_reducible_nested(map(atom, int)::in,
    atom::out, int::out) is semidet.

find_reducible_nested(Atoms, Atom, E) :-
    map.foldl(
        ( pred(A::in, AE::in, Found0::in, Found::out) is det :-
            ( if Found0 = yes({_, _}) then
                Found = Found0
            else if A = nested_root(N, _), int.'>='(AE, N) then
                Found = yes({A, AE})
            else
                Found = Found0
            )
        ), Atoms, no, MaybeFound),
    MaybeFound = yes({Atom, E}).

%---------------------------------------------------------------------------%
% Integer power
%---------------------------------------------------------------------------%

norm_pow(E, N) = Result :-
    ( if N = 0 then
        Result = norm_lit(rational.one)
    else if N = 1 then
        Result = E
    else if int.'<'(N, 0) then
        require.error("norm_pow: negative exponent")
    else if int.even(N) then
        Half = norm_pow(E, int.'//'(N, 2)),
        Result = norm_mul(Half, Half)
    else
        Result = norm_mul(E, norm_pow(E, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Multiplicative inverse
%---------------------------------------------------------------------------%

norm_inv(NE) = Result :-
    NE = norm_expr(M),
    Terms = map.to_assoc_list(M),
    ( if Terms = [] then
        require.error("norm_inv: division by zero")
    else if Terms = [Mono - C] then
        % Single monomial: invert directly
        Result = norm_scale(rational.'/'(rational.one, C),
            invert_monomial(Mono))
    else
        % Multi-term: rationalize iteratively
        Result = rationalize_inv(NE)
    ).

:- func invert_monomial(monomial) = norm_expr.

invert_monomial(monomial(Atoms)) =
    reduce_monomial(monomial(
        map.map_values(( func(_, E) = int.'-'(0, E) ), Atoms)),
        rational.one).

%---------------------------------------------------------------------------%
% Rationalization via extended GCD
%---------------------------------------------------------------------------%

:- func rationalize_inv(norm_expr) = norm_expr.

rationalize_inv(NE) = Result :-
    ( if find_radical_atom(NE, Atom) then
        N = atom_degree(Atom),
        PolyCoeffs = to_atom_poly(Atom, NE, N),
        MinPoly = min_poly_coeffs(N, Atom),
        norm_poly_ext_gcd(PolyCoeffs, MinPoly, _, Inv, _),
        Result = from_atom_poly(Atom, Inv)
    else
        MaybeC = norm_coeff(NE),
        ( if MaybeC = yes(R), R \= rational.zero then
            Result = norm_lit(rational.'/'(rational.one, R))
        else
            require.error("rationalize_inv: cannot invert")
        )
    ).

:- pred find_radical_atom(norm_expr::in, atom::out) is semidet.

find_radical_atom(norm_expr(M), Atom) :-
    map.foldl(
        ( pred(monomial(As)::in, _V::in, Found0::in, Found::out) is det :-
            ( if Found0 = yes(_) then
                Found = Found0
            else
                Keys = map.keys(As),
                ( if Keys = [A | _] then
                    Found = yes(A)
                else
                    Found = Found0
                )
            )
        ), M, no, MaybeAtom),
    MaybeAtom = yes(Atom).

:- func atom_degree(atom) = int.

atom_degree(imag_unit) = 2.
atom_degree(rat_root(N, _)) = N.
atom_degree(nested_root(N, _)) = N.

:- func to_atom_poly(atom, norm_expr, int) = list(norm_expr).

to_atom_poly(Atom, norm_expr(M), N) = Result :-
    % Group by power of Atom
    Grouped = map.foldl(
        ( func(monomial(As), C, Acc) = Acc1 :-
            ( if map.search(As, Atom, Power) then
                Idx = int.mod(Power, N),
                RestAs = map.delete(As, Atom)
            else
                Idx = 0,
                RestAs = As
            ),
            RestTerm = norm_expr(map.singleton(monomial(RestAs), C)),
            ( if map.search(Acc, Idx, Old) then
                Acc1 = map.set(Acc, Idx, norm_add(Old, RestTerm))
            else
                Acc1 = map.set(Acc, Idx, RestTerm)
            )
        ), M, map.init : map(int, norm_expr)),
    Result = list.map(
        ( func(I) = ( if map.search(Grouped, I, V) then V else norm_zero ) ),
        0 `..` int.'-'(N, 1)).

:- func min_poly_coeffs(int, atom) = list(norm_expr).

min_poly_coeffs(N, Atom) = Result :-
    ( if Atom = imag_unit then
        Result = [norm_lit(rational.one), norm_zero, norm_lit(rational.one)]
    else if Atom = rat_root(_, R) then
        Result = [norm_lit(rational.'-'(rational.zero, R)) |
            list.append(list.duplicate(int.'-'(N, 1), norm_zero),
                [norm_lit(rational.one)])]
    else if Atom = nested_root(_, Inner) then
        Result = [norm_neg(to_norm_expr(Inner)) |
            list.append(list.duplicate(int.'-'(N, 1), norm_zero),
                [norm_lit(rational.one)])]
    else
        Result = [norm_lit(rational.one)]
    ).

:- func from_atom_poly(atom, list(norm_expr)) = norm_expr.

from_atom_poly(Atom, Coeffs) = Result :-
    ( if Coeffs = [] then
        Result = norm_zero
    else
        Result = from_atom_poly_loop(Atom, 0, Coeffs, norm_zero)
    ).

:- func from_atom_poly_loop(atom, int, list(norm_expr), norm_expr) = norm_expr.

from_atom_poly_loop(Atom, I, Coeffs, Acc) = Result :-
    ( if Coeffs = [] then
        Result = Acc
    else if Coeffs = [C | Cs] then
        Result = from_atom_poly_loop(Atom, int.'+'(I, 1), Cs,
            norm_add(Acc, norm_mul(C, atom_pow(Atom, I))))
    else
        Result = Acc
    ).

:- func atom_pow(atom, int) = norm_expr.

atom_pow(A, K) = Result :-
    ( if K = 0 then
        Result = norm_lit(rational.one)
    else
        Result = norm_expr(map.singleton(
            monomial(map.singleton(A, K)), rational.one))
    ).

%---------------------------------------------------------------------------%
% Polynomial arithmetic over NormExpr coefficients
%---------------------------------------------------------------------------%

:- pred norm_poly_ext_gcd(list(norm_expr)::in, list(norm_expr)::in,
    list(norm_expr)::out, list(norm_expr)::out, list(norm_expr)::out) is det.

norm_poly_ext_gcd(A, B, Gcd, S, T) :-
    ext_gcd_loop(A, B,
        [norm_lit(rational.one)], [norm_zero],
        [norm_zero], [norm_lit(rational.one)],
        Gcd, S, T).

:- pred ext_gcd_loop(
    list(norm_expr)::in, list(norm_expr)::in,
    list(norm_expr)::in, list(norm_expr)::in,
    list(norm_expr)::in, list(norm_expr)::in,
    list(norm_expr)::out, list(norm_expr)::out,
    list(norm_expr)::out) is det.

ext_gcd_loop(R0, R1, S0, S1, T0, T1, Gcd, S, T) :-
    ( if norm_poly_is_zero(R1) then
        MaybeLC = norm_poly_lead_coeff(R0),
        ( if MaybeLC = yes(LC) then
            LCInv = norm_inv(LC),
            Gcd = norm_poly_scale(LCInv, R0),
            S = norm_poly_scale(LCInv, S0),
            T = norm_poly_scale(LCInv, T0)
        else
            Gcd = [norm_lit(rational.one)],
            S = S0,
            T = T0
        )
    else
        norm_poly_div_mod(R0, R1, Q, R),
        S2 = norm_poly_sub(S0, norm_poly_mul(Q, S1)),
        T2 = norm_poly_sub(T0, norm_poly_mul(Q, T1)),
        ext_gcd_loop(R1, R, S1, S2, T1, T2, Gcd, S, T)
    ).

:- func norm_poly_add(list(norm_expr), list(norm_expr)) = list(norm_expr).

norm_poly_add(As, Bs) = Result :-
    ( if As = [] then Result = Bs
    else if Bs = [] then Result = As
    else if As = [A | AT], Bs = [B | BT] then
        Result = [norm_add(A, B) | norm_poly_add(AT, BT)]
    else Result = As
    ).

:- func norm_poly_sub(list(norm_expr), list(norm_expr)) = list(norm_expr).

norm_poly_sub(A, B) = norm_poly_add(A, list.map(norm_neg, B)).

:- func norm_poly_mul(list(norm_expr), list(norm_expr)) = list(norm_expr).

norm_poly_mul(As, Bs) = Result :-
    ( if As = [] then
        Result = []
    else if Bs = [] then
        Result = []
    else
        Result = poly_mul_loop(0, As, Bs, [])
    ).

:- func poly_mul_loop(int, list(norm_expr), list(norm_expr),
    list(norm_expr)) = list(norm_expr).

poly_mul_loop(I, Xs, Bs, Acc) = Result :-
    ( if Xs = [] then
        Result = Acc
    else if Xs = [A | As] then
        Result = poly_mul_loop(int.'+'(I, 1), As, Bs,
            norm_poly_add(Acc,
                list.append(list.duplicate(I, norm_zero),
                    list.map(( func(B) = norm_mul(A, B) ), Bs))))
    else
        Result = Acc
    ).

:- func norm_poly_scale(norm_expr, list(norm_expr)) = list(norm_expr).

norm_poly_scale(C, Xs) = list.map(( func(X) = norm_mul(C, X) ), Xs).

:- pred norm_poly_div_mod(list(norm_expr)::in, list(norm_expr)::in,
    list(norm_expr)::out, list(norm_expr)::out) is det.

norm_poly_div_mod(Num, Den, Q, R) :-
    DegNum = norm_poly_deg(Num),
    DegDen = norm_poly_deg(Den),
    ( if int.'<'(DegNum, DegDen) then
        Q = [], R = Num
    else
        MaybeLC = norm_poly_lead_coeff(Den),
        ( if MaybeLC = yes(LCDen) then
            LCDenInv = norm_inv(LCDen),
            QLen = int.'-'(int.'+'(DegNum, 1), DegDen),
            Q0 = list.duplicate(QLen, norm_zero),
            poly_div_loop(DegDen, LCDenInv, Den, Q0, Num, Q, R)
        else
            Q = [], R = Num
        )
    ).

:- pred poly_div_loop(int::in, norm_expr::in, list(norm_expr)::in,
    list(norm_expr)::in, list(norm_expr)::in,
    list(norm_expr)::out, list(norm_expr)::out) is det.

poly_div_loop(DegDen, LCDenInv, Den, Q0, R0, Q, R) :-
    ( if norm_poly_is_zero(R0) then
        Q = norm_poly_trim(Q0),
        R = norm_poly_trim(R0)
    else if int.'<'(norm_poly_deg(R0), DegDen) then
        Q = norm_poly_trim(Q0),
        R = norm_poly_trim(R0)
    else
        DR = norm_poly_deg(R0),
        MaybeLC = norm_poly_lead_coeff(R0),
        ( if MaybeLC = yes(LCR) then
            CoeffQ = norm_mul(LCR, LCDenInv),
            Shift = int.'-'(DR, DegDen),
            Q1 = norm_poly_set_coeff(Shift, CoeffQ, Q0),
            ShiftPoly = list.append(
                list.duplicate(Shift, norm_zero),
                list.map(( func(D) = norm_mul(CoeffQ, D) ), Den)),
            R1 = norm_poly_trim(norm_poly_sub(R0, ShiftPoly)),
            poly_div_loop(DegDen, LCDenInv, Den, Q1, R1, Q, R)
        else
            Q = norm_poly_trim(Q0),
            R = norm_poly_trim(R0)
        )
    ).

:- func norm_poly_set_coeff(int, norm_expr, list(norm_expr)) = list(norm_expr).

norm_poly_set_coeff(I, C, Xs) = Result :-
    Len = list.length(Xs),
    ( if int.'>='(I, Len) then
        Padded = list.append(Xs, list.duplicate(int.'-'(int.'+'(I, 1), Len), norm_zero))
    else
        Padded = Xs
    ),
    Prefix = list.take_upto(I, Padded),
    ( if list.drop(int.'+'(I, 1), Padded, Suffix) then
        Result = list.append(Prefix, [C | Suffix])
    else
        Result = list.append(Prefix, [C])
    ).

:- func norm_poly_deg(list(norm_expr)) = int.

norm_poly_deg(Xs) = poly_deg_loop(-1, 0, Xs).

:- func poly_deg_loop(int, int, list(norm_expr)) = int.

poly_deg_loop(D, _, []) = D.
poly_deg_loop(D, I, [X | Xs]) =
    poly_deg_loop(( if norm_is_zero(X) then D else I ),
        int.'+'(I, 1), Xs).

:- func norm_poly_lead_coeff(list(norm_expr)) = maybe(norm_expr).

norm_poly_lead_coeff(Xs) = Result :-
    D = norm_poly_deg(Xs),
    ( if int.'<'(D, 0) then
        Result = no
    else
        Result = yes(list.det_index0(Xs, D))
    ).

:- pred norm_poly_is_zero(list(norm_expr)::in) is semidet.

norm_poly_is_zero(Xs) :-
    list.all_true(norm_is_zero, Xs).

:- func norm_poly_trim(list(norm_expr)) = list(norm_expr).

norm_poly_trim(Xs) =
    list.reverse(drop_while_zero(list.reverse(Xs))).

:- func drop_while_zero(list(norm_expr)) = list(norm_expr).

drop_while_zero(Xs) = Result :-
    ( if Xs = [] then
        Result = []
    else if Xs = [X | Rest] then
        ( if norm_is_zero(X) then
            Result = drop_while_zero(Rest)
        else
            Result = Xs
        )
    else
        Result = []
    ).

%---------------------------------------------------------------------------%
% Queries
%---------------------------------------------------------------------------%

norm_is_zero(norm_expr(M)) :- map.is_empty(M).

norm_coeff(norm_expr(M)) = Result :-
    Terms = map.to_assoc_list(M),
    ( if Terms = [] then
        Result = yes(rational.zero)
    else if Terms = [monomial(A) - C], map.is_empty(A) then
        Result = yes(C)
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Conversion from RadExpr
%---------------------------------------------------------------------------%

to_norm_expr(Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = norm_lit(R)
    else if Expr = re_neg(A) then
        Result = norm_neg(to_norm_expr(A))
    else if Expr = re_add(A, B) then
        Result = norm_add(to_norm_expr(A), to_norm_expr(B))
    else if Expr = re_mul(A, B) then
        Result = norm_mul(to_norm_expr(A), to_norm_expr(B))
    else if Expr = re_root(N, re_lit(R)) then
        Result = norm_root(N, R)
    else if Expr = re_root(N, A) then
        Inner = to_norm_expr(A),
        MaybeR = norm_coeff(Inner),
        ( if MaybeR = yes(R) then
            Result = norm_root(N, R)
        else
            InnerTerms = map.to_assoc_list(un_norm_expr(Inner)),
            ( if InnerTerms = [monomial(Atoms) - C] then
                % Single monomial: distribute root
                RootC = norm_root(N, C),
                RootAtoms = map.foldl(
                    ( func(Atom, E, Acc) =
                        norm_mul(Acc, root_of_atom_pow(N, Atom, E))
                    ), Atoms, norm_lit(rational.one)),
                Result = norm_mul(RootC, RootAtoms)
            else
                % Multi-term: wrap as NestedRoot with canonicalization
                Result = canonicalize_nested_root(N, Inner, InnerTerms)
            )
        )
    else if Expr = re_inv(A) then
        Result = norm_inv(to_norm_expr(A))
    else if Expr = re_pow(A, N) then
        ( if int.'>='(N, 0) then
            Result = norm_pow(to_norm_expr(A), N)
        else
            Result = norm_inv(norm_pow(to_norm_expr(A), int.'-'(0, N)))
        )
    else
        Result = norm_zero
    ).

:- func un_norm_expr(norm_expr) = map(monomial, rational).

un_norm_expr(norm_expr(M)) = M.

:- func root_of_atom_pow(int, atom, int) = norm_expr.

root_of_atom_pow(N, Atom, E) = Result :-
    ( if Atom = rat_root(M, R) then
        Result = reduce_monomial(
            monomial(map.singleton(rat_root(int.'*'(M, N), R), E)),
            rational.one)
    else if Atom = nested_root(M, Inner) then
        Result = reduce_monomial(
            monomial(map.singleton(nested_root(int.'*'(M, N), Inner), E)),
            rational.one)
    else
        % imag_unit case
        E1 = int.mod(E, 4),
        ( if E1 = 0 then
            Result = norm_lit(rational.one)
        else if E1 = 2 then
            Result = norm_root(N, rational.'-'(rational.zero, rational.one))
        else
            Base = norm_root(int.'*'(2, N),
                rational.'-'(rational.zero, rational.one)),
            ( if E1 = 1 then
                Result = Base
            else
                Result = norm_neg(Base)
            )
        )
    ).

%---------------------------------------------------------------------------%
% Canonicalize multi-term radicand under NestedRoot
%---------------------------------------------------------------------------%

:- func canonicalize_nested_root(int, norm_expr,
    list(pair(monomial, rational))) = norm_expr.

canonicalize_nested_root(N, _Inner, Terms) = Result :-
    % Extract LCD and GCD for integral canonicalization
    Coeffs = list.map(( func(_ - C) = C ), Terms),
    LCD = compute_lcd(Coeffs),
    IntCoeffs = list.map(
        ( func(_ - C) = numer(rational.'*'(C, rational.from_integers(LCD, integer.one))) ),
        Terms),
    G = compute_gcd_list(list.map(integer.abs, IntCoeffs)),
    TotalContent = integer.'*'(G, integer.pow(LCD, integer.integer(int.'-'(N, 1)))),
    extract_nth_power(N, TotalContent, NthOut, NthRem),
    OuterCoeff0 = rational.'/'(NthOut, rational.from_integers(LCD, integer.one)),
    CleanedTerms = list.map(
        ( func(Mono - C) = Mono -
            rational.'*'(C, rational.from_integers(LCD, integer.one))
                `rational_div` rational.from_integers(G, integer.one)
        ), Terms),
    CleanedRadicand0 = norm_expr(map.from_assoc_list(CleanedTerms)),
    % For odd roots, flip sign if radicand is negative
    ( if int.odd(N) then
        V = eval_float(from_norm_expr(CleanedRadicand0)),
        ( if float.is_nan(V) then
            OuterCoeff = OuterCoeff0,
            CleanedRadicand = CleanedRadicand0,
            EvenRootNeg = no
        else if float.'<'(V, 0.0) then
            OuterCoeff = rational.'-'(rational.zero, OuterCoeff0),
            CleanedRadicand = norm_neg(CleanedRadicand0),
            EvenRootNeg = no
        else
            OuterCoeff = OuterCoeff0,
            CleanedRadicand = CleanedRadicand0,
            EvenRootNeg = no
        )
    else
        % Even root: check if negative, factor out i
        V = eval_float(from_norm_expr(CleanedRadicand0)),
        ( if float.is_nan(V) then
            OuterCoeff = OuterCoeff0,
            CleanedRadicand = CleanedRadicand0,
            EvenRootNeg = no
        else if float.'<'(V, 0.0) then
            OuterCoeff = OuterCoeff0,
            CleanedRadicand = norm_neg(CleanedRadicand0),
            EvenRootNeg = yes
        else
            OuterCoeff = OuterCoeff0,
            CleanedRadicand = CleanedRadicand0,
            EvenRootNeg = no
        )
    ),
    CanSplitRem = ( if int.odd(N) then yes
                   else if NthRem = rational.one then yes
                   else no ),
    WrapI = ( func(E) =
        ( if EvenRootNeg = yes then norm_mul(norm_atom(imag_unit), E)
          else E ) ),
    ( if norm_is_zero(CleanedRadicand) then
        Result = norm_zero
    else if CanSplitRem = no then
        FullRadicand = norm_scale(NthRem, CleanedRadicand),
        Result = norm_scale(OuterCoeff,
            WrapI(norm_atom(nested_root(N, from_norm_expr(FullRadicand)))))
    else
        MaybeR = norm_coeff(CleanedRadicand),
        ( if MaybeR = yes(R) then
            Result = norm_scale(OuterCoeff,
                WrapI(norm_mul(norm_root(N, NthRem), norm_root(N, R))))
        else
            NestedPart = norm_atom(nested_root(N, from_norm_expr(CleanedRadicand))),
            ( if NthRem = rational.one then
                Result = norm_scale(OuterCoeff, WrapI(NestedPart))
            else
                Result = norm_scale(OuterCoeff,
                    WrapI(norm_mul(norm_root(N, NthRem), NestedPart)))
            )
        )
    ).

:- func rational_div(rational, rational) = rational.

rational_div(A, B) = rational.'/'(A, B).

:- func compute_lcd(list(rational)) = integer.

compute_lcd([]) = integer.one.
compute_lcd([R | Rs]) =
    list.foldl(( func(X, Acc) = lcm_integer(Acc, denom(X)) ),
        Rs, denom(R)).

:- func lcm_integer(integer, integer) = integer.

lcm_integer(A, B) = Result :-
    ( if A = integer.zero then
        Result = B
    else if B = integer.zero then
        Result = A
    else
        G = gcd_integer(A, B),
        Result = integer.'*'(integer.div(integer.abs(A), G), integer.abs(B))
    ).

:- func gcd_integer(integer, integer) = integer.

gcd_integer(A, B) = Result :-
    AbsA = integer.abs(A),
    AbsB = integer.abs(B),
    ( if AbsB = integer.zero then
        Result = AbsA
    else
        Result = gcd_integer(AbsB, integer.mod(AbsA, AbsB))
    ).

:- func compute_gcd_list(list(integer)) = integer.

compute_gcd_list([]) = integer.zero.
compute_gcd_list([X | Xs]) =
    list.foldl(gcd_integer, Xs, X).

%---------------------------------------------------------------------------%
% Conversion to RadExpr
%---------------------------------------------------------------------------%

from_norm_expr(norm_expr(M)) = Result :-
    Terms = map.to_assoc_list(M),
    ( if Terms = [] then
        Result = re_lit(rational.zero)
    else
        Exprs = list.map(mono_term_to_expr, Terms),
        Result = build_sum(Exprs)
    ).

:- func mono_term_to_expr(pair(monomial, rational)) = rad_expr(rational).

mono_term_to_expr(monomial(Atoms) - Coeff) = Result :-
    AtomList = map.to_assoc_list(Atoms),
    AtomExprs = list.map(atom_pair_to_expr, AtomList),
    Body = build_prod(AtomExprs),
    Result = apply_coeff(Coeff, Body).

:- func atom_pair_to_expr(pair(atom, int)) = rad_expr(rational).

atom_pair_to_expr(Atom - E) = Result :-
    ( if E = 0 then
        Result = re_lit(rational.one)
    else if Atom = imag_unit then
        ( if E = 1 then
            Result = re_root(2, re_lit(rational.'-'(rational.zero, rational.one)))
        else
            Result = re_pow(re_root(2, re_lit(rational.'-'(rational.zero, rational.one))), E)
        )
    else if Atom = rat_root(N, R) then
        ( if E = 1 then
            Result = re_root(N, re_lit(R))
        else
            Result = re_pow(re_root(N, re_lit(R)), E)
        )
    else if Atom = nested_root(N, Inner) then
        ( if E = 1 then
            Result = re_root(N, Inner)
        else
            Result = re_pow(re_root(N, Inner), E)
        )
    else
        Result = re_lit(rational.one)
    ).

:- func apply_coeff(rational, rad_expr(rational)) = rad_expr(rational).

apply_coeff(C, Body) = Result :-
    ( if C = rational.one then
        Result = Body
    else if C = rational.'-'(rational.zero, rational.one) then
        Result = re_neg(Body)
    else if Body = re_lit(R), R = rational.one then
        Result = re_lit(C)
    else
        Result = re_mul(re_lit(C), Body)
    ).

:- func build_prod(list(rad_expr(rational))) = rad_expr(rational).

build_prod(Fs) = Result :-
    ( if Fs = [] then Result = re_lit(rational.one)
    else if Fs = [X] then Result = X
    else if Fs = [X | Xs], Xs = [_ | _] then
        Result = list.foldl(( func(B, Acc) = re_mul(Acc, B) ), Xs, X)
    else Result = re_lit(rational.one)
    ).

:- func build_sum(list(rad_expr(rational))) = rad_expr(rational).

build_sum(Fs) = Result :-
    ( if Fs = [] then Result = re_lit(rational.zero)
    else if Fs = [X] then Result = X
    else if Fs = [X | Xs], Xs = [_ | _] then
        Result = list.foldl(( func(B, Acc) = re_add(Acc, B) ), Xs, X)
    else Result = re_lit(rational.zero)
    ).

%---------------------------------------------------------------------------%
% Map utilities
%---------------------------------------------------------------------------%

:- func map_union_with((func(V, V) = V), map(K, V), map(K, V)) = map(K, V).

map_union_with(F, A, B) =
    map.foldl(
        ( func(K, V, Acc) =
            ( if map.search(Acc, K, Old) then
                map.set(Acc, K, F(Old, V))
            else
                map.set(Acc, K, V)
            )
        ), B, A).

:- func map_filter_nonzero(map(monomial, rational)) = map(monomial, rational).

map_filter_nonzero(M) =
    map.foldl(
        ( func(K, V, Acc) =
            ( if V = rational.zero then Acc
              else map.set(Acc, K, V) )
        ), M, map.init).

%---------------------------------------------------------------------------%
:- end_module normal_form.
%---------------------------------------------------------------------------%
