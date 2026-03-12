%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: multivariate.m
% Main author: nyc
%
% Sparse multivariate polynomials over an arbitrary coefficient ring.
%
% Representation: map(mono, K) where each monomial maps to its
% non-zero coefficient. Monomials are products of variables
% raised to positive integer powers.
%
%---------------------------------------------------------------------------%

:- module multivariate.
:- interface.

:- import_module map.
:- import_module poly.
:- import_module rational.
:- import_module set.

%---------------------------------------------------------------------------%
% Types
%---------------------------------------------------------------------------%

    % Variable identifier.
    %
:- type var
    --->    var(int).

    % Monomial: product of variables with positive exponents.
    % Invariant: no zero exponents in the map.
    %
:- type mono
    --->    mono(map(var, int)).

    % Sparse multivariate polynomial.
    % Invariant: no zero coefficients.
    %
:- type mpoly(K)
    --->    mpoly(map(mono, K)).

%---------------------------------------------------------------------------%
% Monomial operations
%---------------------------------------------------------------------------%

:- func mono_one = mono.
:- func mono_var(var, int) = mono.
:- func mono_mul(mono, mono) = mono.
:- func mono_degree(mono) = int.
:- func mono_gcd(mono, mono) = mono.
:- func mono_lcm(mono, mono) = mono.
:- pred mono_divides(mono::in, mono::in) is semidet.
:- func mono_div(mono, mono) = mono.
:- func compare_grevlex(mono, mono) = comparison_result.

%---------------------------------------------------------------------------%
% Construction
%---------------------------------------------------------------------------%

:- func mp_const(K) = mpoly(K) <= ring(K).
:- func mp_var(var) = mpoly(K) <= ring(K).
:- func mp_zero = mpoly(K).
:- func mp_one = mpoly(K) <= ring(K).
:- pred mp_is_zero(mpoly(K)::in) is semidet.

%---------------------------------------------------------------------------%
% Arithmetic
%---------------------------------------------------------------------------%

:- func mp_add(mpoly(K), mpoly(K)) = mpoly(K) <= ring(K).
:- func mp_sub(mpoly(K), mpoly(K)) = mpoly(K) <= ring(K).
:- func mp_mul(mpoly(K), mpoly(K)) = mpoly(K) <= ring(K).
:- func mp_neg(mpoly(K)) = mpoly(K) <= ring(K).
:- func mp_scale(K, mpoly(K)) = mpoly(K) <= ring(K).
:- func mul_by_mono(mono, mpoly(K)) = mpoly(K).

%---------------------------------------------------------------------------%
% Queries
%---------------------------------------------------------------------------%

:- func total_degree(mpoly(K)) = int.
:- func degree_in(var, mpoly(K)) = int.
:- func mp_variables(mpoly(K)) = set(var).
:- func num_terms(mpoly(K)) = int.

%---------------------------------------------------------------------------%
% Leading term operations
%---------------------------------------------------------------------------%

:- pred lead_term_grlex(mpoly(K)::in, mono::out, K::out) is semidet.
:- pred lead_term_grevlex(mpoly(K)::in, mono::out, K::out) is semidet.

%---------------------------------------------------------------------------%
% Evaluation and substitution
%---------------------------------------------------------------------------%

:- func mp_eval((func(var) = K), mpoly(K)) = K <= ring(K).
:- func subst_var(var, mpoly(K), mpoly(K)) = mpoly(K) <= ring(K).

%---------------------------------------------------------------------------%
% Conversion to/from univariate
%---------------------------------------------------------------------------%

:- func to_univariate(var, mpoly(K)) = poly(mpoly(K)) <= ring(K).
:- func from_univariate(var, poly(K)) = mpoly(K) <= ring(K).

%---------------------------------------------------------------------------%
% GCD (over rational)
%---------------------------------------------------------------------------%

:- func gcd_mpoly(mpoly(rational), mpoly(rational)) = mpoly(rational).
:- func content_mpoly(var, mpoly(rational)) = mpoly(rational).
:- func prim_part_mpoly(var, mpoly(rational)) = mpoly(rational).
:- func exact_div_mpoly(mpoly(rational), mpoly(rational)) = mpoly(rational).

%---------------------------------------------------------------------------%
% ring instance for mpoly(K)
%---------------------------------------------------------------------------%

:- instance ring(mpoly(K)) <= ring(K).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module list.
:- import_module require.

%---------------------------------------------------------------------------%
% Monomial operations
%---------------------------------------------------------------------------%

mono_one = mono(map.init).

mono_var(V, N) = Result :-
    ( if N = 0 then
        Result = mono_one
    else
        Result = mono(map.singleton(V, N))
    ).

mono_mul(mono(A), mono(B)) = mono(Filtered) :-
    map.union(merge_add_int, A, B, Merged),
    map.foldl(keep_nonzero, Merged, map.init, Filtered).

mono_degree(mono(M)) = Result :-
    map.foldl(( pred(_::in, E::in, Acc0::in, Acc1::out) is det :-
        Acc1 = int.'+'(Acc0, E)
    ), M, 0, Result).

mono_gcd(mono(A), mono(B)) = mono(Filtered) :-
    map.intersect(merge_min_int, A, B, Merged),
    map.foldl(keep_nonzero, Merged, map.init, Filtered).

mono_lcm(mono(A), mono(B)) = mono(Result) :-
    map.union(merge_max_int, A, B, Result).

mono_divides(mono(A), mono(B)) :-
    Keys = map.keys(A),
    list.all_true(( pred(V::in) is semidet :-
        map.lookup(A, V, EA),
        map.search(B, V, EB),
        int.'=<'(EA, EB)
    ), Keys).

mono_div(mono(A), mono(B)) = mono(Filtered) :-
    map.foldl(( pred(V::in, EA::in, Acc0::in, Acc1::out) is det :-
        ( if map.search(B, V, EB) then
            Diff = int.'-'(EA, EB),
            ( if Diff = 0 then
                Acc1 = Acc0
            else
                map.det_insert(V, Diff, Acc0, Acc1)
            )
        else
            map.det_insert(V, EA, Acc0, Acc1)
        )
    ), A, map.init, Filtered).

compare_grevlex(A, B) = Result :-
    DA = mono_degree(A),
    DB = mono_degree(B),
    ( if int.'<'(DA, DB) then
        Result = (<)
    else if int.'>'(DA, DB) then
        Result = (>)
    else
        Result = compare_revlex(A, B)
    ).

:- func compare_revlex(mono, mono) = comparison_result.

compare_revlex(mono(A), mono(B)) = Result :-
    map.union(merge_keep_left, A, B, AllVars),
    map.keys(AllVars, VarList),
    list.reverse(VarList, RevVars),
    compare_revlex_vars(RevVars, A, B, Result).

:- pred compare_revlex_vars(list(var)::in, map(var, int)::in,
    map(var, int)::in, comparison_result::out) is det.

compare_revlex_vars([], _, _, (=)).
compare_revlex_vars([V | Vs], A, B, Result) :-
    EA = ( if map.search(A, V, EA0) then EA0 else 0 ),
    EB = ( if map.search(B, V, EB0) then EB0 else 0 ),
    ( if EA = EB then
        compare_revlex_vars(Vs, A, B, Result)
    else if int.'<'(EA, EB) then
        Result = (>)
    else
        Result = (<)
    ).

%---------------------------------------------------------------------------%
% Construction
%---------------------------------------------------------------------------%

mp_const(C) = Result :-
    ( if ring_is_zero(C) then
        Result = mp_zero
    else
        Result = mpoly(map.singleton(mono_one, C))
    ).

mp_var(V) = mpoly(map.singleton(mono_var(V, 1), ring_one)).

mp_zero = mpoly(map.init).

mp_one = mp_const(ring_one).

mp_is_zero(mpoly(M)) :- map.is_empty(M).

%---------------------------------------------------------------------------%
% Arithmetic
%---------------------------------------------------------------------------%

mp_add(mpoly(A), mpoly(B)) = mp_clean(Merged) :-
    map.union(merge_ring_add, A, B, Merged).

mp_sub(A, B) = mp_add(A, mp_neg(B)).

mp_neg(mpoly(M)) = mpoly(map.map_values_only(ring_negate, M)).

mp_mul(mpoly(A), mpoly(B)) = Result :-
    map.foldl(( pred(MA::in, CA::in, Acc0::in, Acc1::out) is det :-
        map.foldl(( pred(MB::in, CB::in, IAcc0::in, IAcc1::out) is det :-
            MNew = mono_mul(MA, MB),
            CNew = ring_mul(CA, CB),
            ( if map.search(IAcc0, MNew, Old) then
                map.det_update(MNew, ring_add(Old, CNew), IAcc0, IAcc1)
            else
                map.det_insert(MNew, CNew, IAcc0, IAcc1)
            )
        ), B, Acc0, Acc1)
    ), A, map.init, Merged),
    Result = mp_clean(Merged).

mp_scale(C, P) = Result :-
    ( if ring_is_zero(C) then
        Result = mp_zero
    else
        mpoly(M) = P,
        Scaled = map.map_values_only(( func(V) = ring_mul(C, V) ), M),
        Result = mp_clean(Scaled)
    ).

mul_by_mono(M, mpoly(P)) =
    mpoly(map.foldl(( func(MK, V, Acc) =
        map.det_insert(Acc, mono_mul(M, MK), V)
    ), P, map.init)).

%---------------------------------------------------------------------------%
% Queries
%---------------------------------------------------------------------------%

total_degree(mpoly(M)) = Result :-
    ( if map.is_empty(M) then
        Result = 0
    else
        map.foldl(( pred(Mono::in, _::in, Acc0::in, Acc1::out) is det :-
            D = mono_degree(Mono),
            Acc1 = ( if int.'>'(D, Acc0) then D else Acc0 )
        ), M, 0, Result)
    ).

degree_in(V, mpoly(M)) = Result :-
    map.foldl(( pred(mono(Vars)::in, _::in, Acc0::in, Acc1::out) is det :-
        D = ( if map.search(Vars, V, D0) then D0 else 0 ),
        Acc1 = ( if int.'>'(D, Acc0) then D else Acc0 )
    ), M, 0, Result).

mp_variables(mpoly(M)) = Result :-
    map.foldl(( pred(mono(Vars)::in, _::in, Acc0::in, Acc1::out) is det :-
        Acc1 = set.union(Acc0, set.from_sorted_list(map.keys(Vars)))
    ), M, set.init, Result).

num_terms(mpoly(M)) = map.count(M).

%---------------------------------------------------------------------------%
% Leading term operations
%---------------------------------------------------------------------------%

lead_term_grlex(mpoly(M), BestMono, BestCoeff) :-
    not map.is_empty(M),
    map.keys(M, [FirstMono | _]),
    map.lookup(M, FirstMono, FirstCoeff0),
    map.foldl2(
        ( pred(Mono::in, Coeff::in, BM0::in, BM1::out,
               BC0::in, BC1::out) is det :-
            DM = mono_degree(Mono),
            DB = mono_degree(BM0),
            ( if int.'>'(DM, DB) then
                BM1 = Mono, BC1 = Coeff
            else if DM = DB then
                compare(CmpResult, Mono, BM0),
                ( if CmpResult = (>) then
                    BM1 = Mono, BC1 = Coeff
                else
                    BM1 = BM0, BC1 = BC0
                )
            else
                BM1 = BM0, BC1 = BC0
            )
        ), M, FirstMono, BestMono, FirstCoeff0, BestCoeff).

lead_term_grevlex(mpoly(M), BestMono, BestCoeff) :-
    not map.is_empty(M),
    map.keys(M, [FirstMono | _]),
    map.lookup(M, FirstMono, FirstCoeff0),
    map.foldl2(
        ( pred(Mono::in, Coeff::in, BM0::in, BM1::out,
               BC0::in, BC1::out) is det :-
            ( if compare_grevlex(Mono, BM0) = (>) then
                BM1 = Mono, BC1 = Coeff
            else
                BM1 = BM0, BC1 = BC0
            )
        ), M, FirstMono, BestMono, FirstCoeff0, BestCoeff).

%---------------------------------------------------------------------------%
% Evaluation and substitution
%---------------------------------------------------------------------------%

mp_eval(Env, mpoly(M)) = Result :-
    map.foldl(( pred(Mono::in, Coeff::in, Acc0::in, Acc1::out) is det :-
        MonoVal = eval_mono(Env, Mono),
        Acc1 = ring_add(Acc0, ring_mul(Coeff, MonoVal))
    ), M, ring_zero, Result).

:- func eval_mono((func(var) = K), mono) = K <= ring(K).

eval_mono(Env, mono(Vars)) = Result :-
    map.foldl(( pred(V::in, N::in, Acc0::in, Acc1::out) is det :-
        Acc1 = ring_mul(Acc0, pow_ring(Env(V), N))
    ), Vars, ring_one, Result).

:- func pow_ring(K, int) = K <= ring(K).

pow_ring(X, N) = Result :-
    ( if N = 0 then
        Result = ring_one
    else
        Result = ring_mul(X, pow_ring(X, int.'-'(N, 1)))
    ).

subst_var(V, Replacement, mpoly(M)) = Result :-
    map.foldl(( pred(mono(Vars)::in, C::in, Acc0::in, Acc1::out) is det :-
        ( if map.search(Vars, V, VarExp) then
            RestMono = mono(map.delete(Vars, V)),
            RestPoly = mpoly(map.singleton(RestMono, C)),
            SubstPart = mp_pow(Replacement, VarExp),
            Acc1 = mp_add(Acc0, mp_mul(RestPoly, SubstPart))
        else
            TermPoly = mpoly(map.singleton(mono(Vars), C)),
            Acc1 = mp_add(Acc0, TermPoly)
        )
    ), M, mp_zero, Result).

:- func mp_pow(mpoly(K), int) = mpoly(K) <= ring(K).

mp_pow(P, N) = Result :-
    ( if N = 0 then
        Result = mp_one
    else
        Result = mp_mul(P, mp_pow(P, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Conversion
%---------------------------------------------------------------------------%

to_univariate(V, mpoly(M)) = Result :-
    map.foldl(
        ( pred(mono(Vars)::in, C::in,
               Acc0::in, Acc1::out) is det :-
            Deg = ( if map.search(Vars, V, D0) then D0 else 0 ),
            RestMono = mono(map.delete(Vars, V)),
            CoeffPoly = mpoly(map.singleton(RestMono, C)),
            ( if map.search(Acc0, Deg, OldCoeff) then
                map.det_update(Deg, mp_add(OldCoeff, CoeffPoly), Acc0, Acc1)
            else
                map.det_insert(Deg, CoeffPoly, Acc0, Acc1)
            )
        ), M, map.init : map(int, mpoly(K)), Groups),
    ( if map.is_empty(Groups) then
        Result = zero_poly
    else
        map.foldl(( pred(D::in, _::in, Max0::in, Max1::out) is det :-
            Max1 = ( if int.'>'(D, Max0) then D else Max0 )
        ), Groups, 0, MaxDeg),
        build_coeff_list(0, MaxDeg, Groups, CoeffList),
        Result = mk_poly(CoeffList)
    ).

:- pred build_coeff_list(int::in, int::in, map(int, mpoly(K))::in,
    list(mpoly(K))::out) is det <= ring(K).

build_coeff_list(I, MaxDeg, Groups, Result) :-
    ( if int.'>'(I, MaxDeg) then
        Result = []
    else
        C = ( if map.search(Groups, I, C0) then C0
              else mp_zero : mpoly(K) ),
        build_coeff_list(int.'+'(I, 1), MaxDeg, Groups, Rest),
        Result = [C | Rest]
    ).

from_univariate(V, Poly) = Result :-
    Cs = coeffs(Poly),
    from_uni_loop(V, Cs, 0, mp_zero, Result).

:- pred from_uni_loop(var::in, list(K)::in, int::in,
    mpoly(K)::in, mpoly(K)::out) is det <= ring(K).

from_uni_loop(_, [], _, Acc, Acc).
from_uni_loop(V, [C | Cs], I, Acc0, Result) :-
    ( if ring_is_zero(C) then
        from_uni_loop(V, Cs, int.'+'(I, 1), Acc0, Result)
    else
        Term = mpoly(map.singleton(mono_var(V, I), C)),
        from_uni_loop(V, Cs, int.'+'(I, 1), mp_add(Acc0, Term), Result)
    ).

%---------------------------------------------------------------------------%
% GCD over rational
%---------------------------------------------------------------------------%

gcd_mpoly(A, B) = Result :-
    ( if mp_is_zero(A) then
        Result = monic_mpoly(B)
    else if mp_is_zero(B) then
        Result = monic_mpoly(A)
    else
        ( if pick_var(A, B, V) then
            CA = content_mpoly(V, A),
            CB = content_mpoly(V, B),
            CG = gcd_mpoly(CA, CB),
            PA = prim_part_mpoly(V, A),
            PB = prim_part_mpoly(V, B),
            UA = to_univariate(V, PA),
            UB = to_univariate(V, PB),
            UG = pseudo_gcd_poly(UA, UB),
            G0 = from_univariate_m(V, UG),
            G1 = prim_part_mpoly(V, G0),
            G2 = mp_mul(CG, G1),
            Result = monic_mpoly(G2)
        else
            Result = mp_const(rational.one)
        )
    ).

content_mpoly(V, P) = Result :-
    poly(Cs) = to_univariate(V, P),
    NonZero = list.filter(
        ( pred(C::in) is semidet :- not mp_is_zero(C) ), Cs),
    ( if NonZero = [] then
        Result = mp_zero
    else if NonZero = [C0] then
        Result = monic_mpoly(C0)
    else if NonZero = [C0 | Rest] then
        Result = list.foldl(gcd_mpoly, Rest, C0)
    else
        Result = mp_zero
    ).

prim_part_mpoly(V, P) = Result :-
    C = content_mpoly(V, P),
    ( if mp_is_zero(C) then
        Result = mp_zero
    else
        Result = exact_div_mpoly(P, C)
    ).

exact_div_mpoly(A, B) = Result :-
    ( if mp_is_zero(B) then
        unexpected($pred, "division by zero")
    else if is_const_mpoly(B) then
        C = const_coeff_q(B),
        InvC = rational.'/'(rational.one, C),
        Result = mp_scale(InvC, A)
    else
        ( if pick_var(A, B, V) then
            UA = to_univariate(V, A),
            UB = to_univariate(V, B),
            Result = from_univariate_m(V, exact_div_upoly(UA, UB))
        else
            CA = const_coeff_q(A),
            CB = const_coeff_q(B),
            Result = mp_const(rational.'/'(CA, CB))
        )
    ).

%---------------------------------------------------------------------------%
% Internal helpers for GCD
%---------------------------------------------------------------------------%

:- pred pick_var(mpoly(K)::in, mpoly(K)::in, var::out) is semidet.

pick_var(A, B, V) :-
    Vs = set.union(mp_variables(A), mp_variables(B)),
    not set.is_empty(Vs),
    VList = set.to_sorted_list(Vs),
    list.det_last(VList, V).

:- func monic_mpoly(mpoly(rational)) = mpoly(rational).

monic_mpoly(mpoly(M)) = Result :-
    ( if map.is_empty(M) then
        Result = mp_zero
    else
        map.foldl(( pred(Mono::in, _::in, Best0::in, Best1::out) is det :-
            compare(CmpResult, Mono, Best0),
            ( if CmpResult = (>) then
                Best1 = Mono
            else
                Best1 = Best0
            )
        ), M, mono_one, MaxMono),
        ( if map.search(M, MaxMono, LC) then
            ( if LC = rational.zero then
                Result = mp_zero
            else
                InvLC = rational.'/'(rational.one, LC),
                Result = mp_scale(InvLC, mpoly(M))
            )
        else
            Result = mpoly(M)
        )
    ).

:- pred is_const_mpoly(mpoly(K)::in) is semidet.

is_const_mpoly(mpoly(M)) :-
    ( map.is_empty(M)
    ; map.count(M) = 1,
      map.keys(M, [mono(Vars)]),
      map.is_empty(Vars)
    ).

:- func const_coeff_q(mpoly(rational)) = rational.

const_coeff_q(mpoly(M)) =
    ( if map.search(M, mono_one, C) then C else rational.zero ).

:- func from_univariate_m(var, poly(mpoly(rational))) = mpoly(rational).

from_univariate_m(V, poly(Cs)) = Result :-
    from_uni_m_loop(V, Cs, 0, mp_zero, Result).

:- pred from_uni_m_loop(var::in, list(mpoly(rational))::in, int::in,
    mpoly(rational)::in, mpoly(rational)::out) is det.

from_uni_m_loop(_, [], _, Acc, Acc).
from_uni_m_loop(V, [C | Cs], I, Acc0, Result) :-
    ( if mp_is_zero(C) then
        from_uni_m_loop(V, Cs, int.'+'(I, 1), Acc0, Result)
    else
        VarPow = ( if I = 0 then mp_one
                   else mpoly(map.singleton(mono_var(V, I), rational.one)) ),
        Term = mp_mul(C, VarPow),
        from_uni_m_loop(V, Cs, int.'+'(I, 1), mp_add(Acc0, Term), Result)
    ).

%---------------------------------------------------------------------------%
% Pseudo-division for poly(mpoly(rational))
%---------------------------------------------------------------------------%

:- func pseudo_rem_poly(poly(mpoly(rational)), poly(mpoly(rational)))
    = poly(mpoly(rational)).

pseudo_rem_poly(F, G) = Result :-
    ( if int.'<'(degree(F), degree(G)) then
        Result = F
    else if int.'<'(degree(G), 0) then
        unexpected($pred, "division by zero")
    else
        LCG = det_lead_coeff(G),
        DG = degree(G),
        E = int.'-'(int.'+'(degree(F), 1), DG),
        pseudo_rem_loop(F, G, LCG, DG, E, Result)
    ).

:- pred pseudo_rem_loop(poly(mpoly(rational))::in,
    poly(mpoly(rational))::in, mpoly(rational)::in,
    int::in, int::in, poly(mpoly(rational))::out) is det.

pseudo_rem_loop(R, G, LCG, DG, E, Result) :-
    ( if int.'=<'(E, 0) then
        Result = R
    else if int.'<'(degree(R), DG) then
        Result = R
    else
        LCR = det_lead_coeff(R),
        D = int.'-'(degree(R), DG),
        ShiftCoeffs = list.duplicate(D, mp_zero : mpoly(rational))
            ++ [LCR],
        ShiftPoly = mk_poly(ShiftCoeffs),
        R1 = sub(scale(LCG, R), mul(ShiftPoly, G)),
        pseudo_rem_loop(R1, G, LCG, DG, int.'-'(E, 1), Result)
    ).

:- func pseudo_gcd_poly(poly(mpoly(rational)), poly(mpoly(rational)))
    = poly(mpoly(rational)).

pseudo_gcd_poly(A, B) = Result :-
    ( if int.'<'(degree(A), 0) then
        Result = B
    else if int.'<'(degree(B), 0) then
        Result = A
    else if int.'<'(degree(A), degree(B)) then
        Result = pseudo_gcd_poly(B, A)
    else
        pseudo_gcd_loop(A, B, Result)
    ).

:- pred pseudo_gcd_loop(poly(mpoly(rational))::in,
    poly(mpoly(rational))::in, poly(mpoly(rational))::out) is det.

pseudo_gcd_loop(F, G, Result) :-
    ( if int.'<'(degree(G), 0) then
        Result = F
    else
        R = pseudo_rem_poly(F, G),
        ( if int.'<'(degree(R), 0) then
            Result = G
        else
            pseudo_gcd_loop(G, prim_part_upoly(R), Result)
        )
    ).

:- func prim_part_upoly(poly(mpoly(rational))) = poly(mpoly(rational)).

prim_part_upoly(poly(Cs)) = Result :-
    NonZero = list.filter(
        ( pred(C::in) is semidet :- not mp_is_zero(C) ), Cs),
    ( if NonZero = [] then
        Result = zero_poly
    else if NonZero = [C0 | Rest] then
        G = list.foldl(gcd_mpoly, Rest, C0),
        ( if mp_is_zero(G) then
            Result = poly(Cs)
        else
            Result = mk_poly(list.map(
                ( func(C) = exact_div_mpoly(C, G) ), Cs))
        )
    else
        Result = zero_poly
    ).

:- func exact_div_upoly(poly(mpoly(rational)), poly(mpoly(rational)))
    = poly(mpoly(rational)).

exact_div_upoly(F, G) = Result :-
    ( if int.'<'(degree(G), 0) then
        unexpected($pred, "division by zero")
    else if int.'<'(degree(F), degree(G)) then
        Result = zero_poly
    else if int.'<'(degree(F), 0) then
        Result = zero_poly
    else
        LCG = det_lead_coeff(G),
        DG = degree(G),
        exact_div_upoly_loop(F, G, LCG, DG, zero_poly, Result)
    ).

:- pred exact_div_upoly_loop(poly(mpoly(rational))::in,
    poly(mpoly(rational))::in, mpoly(rational)::in,
    int::in, poly(mpoly(rational))::in,
    poly(mpoly(rational))::out) is det.

exact_div_upoly_loop(R, G, LCG, DG, QAcc, Result) :-
    ( if int.'<'(degree(R), 0) then
        Result = QAcc
    else if int.'<'(degree(R), DG) then
        Result = QAcc
    else
        DR = degree(R),
        LCR = det_lead_coeff(R),
        D = int.'-'(DR, DG),
        C = exact_div_mpoly(LCR, LCG),
        TermCoeffs = list.duplicate(D, mp_zero : mpoly(rational))
            ++ [C],
        Term = mk_poly(TermCoeffs),
        R1 = sub(R, mul(Term, G)),
        exact_div_upoly_loop(R1, G, LCG, DG, add(QAcc, Term), Result)
    ).

%---------------------------------------------------------------------------%
% Utility: map merge predicates
%---------------------------------------------------------------------------%

:- func mp_clean(map(mono, K)) = mpoly(K) <= ring(K).

mp_clean(M) = mpoly(Filtered) :-
    map.foldl(( pred(K::in, V::in, Acc0::in, Acc1::out) is det :-
        ( if ring_is_zero(V) then
            Acc1 = Acc0
        else
            map.det_insert(K, V, Acc0, Acc1)
        )
    ), M, map.init, Filtered).

:- pred keep_nonzero(var::in, int::in, map(var, int)::in,
    map(var, int)::out) is det.

keep_nonzero(K, V, !Acc) :-
    ( if V = 0 then
        true
    else
        map.det_insert(K, V, !Acc)
    ).

:- pred merge_add_int(int::in, int::in, int::out) is det.
merge_add_int(A, B, int.'+'(A, B)).

:- pred merge_min_int(int::in, int::in, int::out) is det.
merge_min_int(A, B, ( if int.'=<'(A, B) then A else B )).

:- pred merge_max_int(int::in, int::in, int::out) is det.
merge_max_int(A, B, ( if int.'>='(A, B) then A else B )).

:- pred merge_keep_left(int::in, int::in, int::out) is det.
merge_keep_left(A, _, A).

:- pred merge_ring_add(K::in, K::in, K::out) is det <= ring(K).
merge_ring_add(A, B, ring_add(A, B)).

%---------------------------------------------------------------------------%
% ring instance
%---------------------------------------------------------------------------%

:- instance ring(mpoly(K)) <= ring(K) where [
    (ring_zero = mp_zero),
    (ring_one = mp_one),
    (ring_add(A, B) = mp_add(A, B)),
    (ring_sub(A, B) = mp_sub(A, B)),
    (ring_mul(A, B) = mp_mul(A, B)),
    (ring_negate(A) = mp_neg(A)),
    (ring_from_int(N) = mp_const(ring_from_int(N))),
    (ring_is_zero(A) :- mp_is_zero(A))
].

%---------------------------------------------------------------------------%
:- end_module multivariate.
%---------------------------------------------------------------------------%
