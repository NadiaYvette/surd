%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: rad_normalize.m
% Main author: nyc
%
% Normalization passes for radical expressions.
%
% These are explicit transformations the user applies as needed.
% They do NOT denest.
%
%---------------------------------------------------------------------------%

:- module rad_normalize.
:- interface.

:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Apply all normalization passes, iterated to a fixed point.
    %
:- func normalize(rad_expr(rational)) = rad_expr(rational).

    % A single normalization pass (all sub-passes composed once).
    %
:- func normalize_once(rad_expr(rational)) = rad_expr(rational).

    % Individual passes (exported for selective use).
    %
:- func flatten_arith(rad_expr(K)) = rad_expr(K).
:- func fold_constants(rad_expr(rational)) = rad_expr(rational).
:- func simplify_powers(rad_expr(rational)) = rad_expr(rational).
:- func extract_perfect_powers(rad_expr(rational)) = rad_expr(rational).
:- func collect_coefficients(rad_expr(rational)) = rad_expr(rational).
:- func collect_terms(rad_expr(rational)) = rad_expr(rational).
:- func distribute(rad_expr(rational)) = rad_expr(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module positive.
:- import_module prime_factors.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
% Top-level
%---------------------------------------------------------------------------%

normalize(E) = fix_n(10, normalize_once, E).

normalize_once(E) =
    collect_terms(
    collect_coefficients(
    distribute(
    sort_commutative(
    extract_perfect_powers(
    simplify_powers(
    fold_constants(
    flatten_arith(E)))))))).

:- func fix_n(int, (func(rad_expr(rational)) = rad_expr(rational)),
    rad_expr(rational)) = rad_expr(rational).

fix_n(N, F, X) = Result :-
    ( if N = 0 then
        Result = X
    else
        X1 = F(X),
        ( if X1 = X then
            Result = X
        else
            Result = fix_n(int.'-'(N, 1), F, X1)
        )
    ).

%---------------------------------------------------------------------------%
% Flatten arithmetic (associativity, double negation/inverse)
%---------------------------------------------------------------------------%

flatten_arith(Expr) = Result :-
    ( if Expr = re_neg(re_neg(A)) then
        Result = flatten_arith(A)
    else if Expr = re_neg(A) then
        Result = re_neg(flatten_arith(A))
    else if Expr = re_add(A, B) then
        Result = re_add(flatten_arith(A), flatten_arith(B))
    else if Expr = re_mul(A, B) then
        Result = re_mul(flatten_arith(A), flatten_arith(B))
    else if Expr = re_inv(re_inv(A)) then
        Result = flatten_arith(A)
    else if Expr = re_inv(A) then
        Result = re_inv(flatten_arith(A))
    else if Expr = re_root(N, A) then
        Result = re_root(N, flatten_arith(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(flatten_arith(A), N)
    else
        Result = Expr
    ).

%---------------------------------------------------------------------------%
% Fold constants
%---------------------------------------------------------------------------%

fold_constants(Expr) = Result :-
    ( if Expr = re_lit(_) then
        Result = Expr
    else if Expr = re_neg(A) then
        A1 = fold_constants(A),
        ( if A1 = re_lit(R) then
            Result = re_lit(rational.'-'(rational.zero, R))
        else if A1 = re_neg(A2) then
            Result = A2
        else
            Result = re_neg(A1)
        )
    else if Expr = re_add(A, B) then
        A1 = fold_constants(A),
        B1 = fold_constants(B),
        ( if A1 = re_lit(R), B1 = re_lit(S) then
            Result = re_lit(rational.'+'(R, S))
        else if A1 = re_lit(R), R = rational.zero then
            Result = B1
        else if B1 = re_lit(S), S = rational.zero then
            Result = A1
        else if B1 = re_neg(B2), A1 = B2 then
            Result = re_lit(rational.zero)
        else
            Result = re_add(A1, B1)
        )
    else if Expr = re_mul(A, B) then
        A1 = fold_constants(A),
        B1 = fold_constants(B),
        ( if A1 = re_lit(R), B1 = re_lit(S) then
            Result = re_lit(rational.'*'(R, S))
        else if A1 = re_lit(R), R = rational.zero then
            Result = re_lit(rational.zero)
        else if B1 = re_lit(S), S = rational.zero then
            Result = re_lit(rational.zero)
        else if A1 = re_lit(R), R = rational.one then
            Result = B1
        else if B1 = re_lit(S), S = rational.one then
            Result = A1
        else if A1 = re_lit(R), R = neg_one then
            Result = re_neg(B1)
        else if B1 = re_lit(S), S = neg_one then
            Result = re_neg(A1)
        else
            Result = re_mul(A1, B1)
        )
    else if Expr = re_inv(A) then
        A1 = fold_constants(A),
        ( if A1 = re_lit(R), not R = rational.zero then
            Result = re_lit(rational.'/'(rational.one, R))
        else if A1 = re_inv(A2) then
            Result = A2
        else
            Result = re_inv(A1)
        )
    else if Expr = re_root(N, A) then
        A1 = fold_constants(A),
        ( if A1 = re_lit(R), R = rational.zero then
            Result = re_lit(rational.zero)
        else if A1 = re_lit(R), R = rational.one then
            Result = re_lit(rational.one)
        else
            Result = re_root(N, A1)
        )
    else if Expr = re_pow(A, N) then
        A1 = fold_constants(A),
        ( if A1 = re_lit(R) then
            Result = re_lit(rat_pow(R, N))
        else if N = 0 then
            Result = re_lit(rational.one)
        else if N = 1 then
            Result = A1
        else
            Result = re_pow(A1, N)
        )
    else
        Result = Expr
    ).

:- func neg_one = rational.
neg_one = rational.'-'(rational.zero, rational.one).

:- func rat_pow(rational, int) = rational.
rat_pow(X, N) = Result :-
    ( if N = 0 then
        Result = rational.one
    else if int.'<'(N, 0) then
        Result = rational.'/'(rational.one, rat_pow(X, int.'-'(0, N)))
    else if int.even(N) then
        Half = rat_pow(X, int.'//'(N, 2)),
        Result = rational.'*'(Half, Half)
    else
        Result = rational.'*'(X, rat_pow(X, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Simplify powers
%---------------------------------------------------------------------------%

simplify_powers(Expr) = Result :-
    ( if Expr = re_mul(re_root(2, A), re_root(2, B)), A = B then
        Result = simplify_powers(A)
    else if Expr = re_pow(re_pow(A, M), N) then
        Result = simplify_powers(re_pow(A, int.'*'(M, N)))
    else if Expr = re_pow(re_root(N, A), M), M = N then
        Result = simplify_powers(A)
    else if Expr = re_root(M, re_root(N, A)) then
        Result = re_root(int.'*'(M, N), simplify_powers(A))
    else if Expr = re_root(N, re_pow(A, M)), M = N then
        Result = simplify_powers(A)
    else if Expr = re_neg(A) then
        Result = re_neg(simplify_powers(A))
    else if Expr = re_add(A, B) then
        Result = re_add(simplify_powers(A), simplify_powers(B))
    else if Expr = re_mul(A, B) then
        Result = re_mul(simplify_powers(A), simplify_powers(B))
    else if Expr = re_inv(A) then
        Result = re_inv(simplify_powers(A))
    else if Expr = re_root(N, A) then
        Result = re_root(N, simplify_powers(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(simplify_powers(A), N)
    else
        Result = Expr
    ).

%---------------------------------------------------------------------------%
% Extract perfect powers
%---------------------------------------------------------------------------%

extract_perfect_powers(Expr) = Result :-
    ( if Expr = re_root(N, re_lit(R)) then
        Result = extract_root_lit(N, R)
    else if Expr = re_root(N, A) then
        Result = re_root(N, extract_perfect_powers(A))
    else if Expr = re_neg(A) then
        Result = re_neg(extract_perfect_powers(A))
    else if Expr = re_add(A, B) then
        Result = re_add(extract_perfect_powers(A),
                        extract_perfect_powers(B))
    else if Expr = re_mul(A, B) then
        Result = re_mul(extract_perfect_powers(A),
                        extract_perfect_powers(B))
    else if Expr = re_inv(A) then
        Result = re_inv(extract_perfect_powers(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(extract_perfect_powers(A), N)
    else
        Result = Expr
    ).

:- func extract_root_lit(int, rational) = rad_expr(rational).

extract_root_lit(N, R) = Result :-
    ( if rational.'>'(R, rational.zero) then
        Num = numer(R),
        Den = denom(R),
        extract_nth_power(N, integer.abs(Num), NumOut, NumIn),
        extract_nth_power(N, integer.abs(Den), DenOut, DenIn),
        ( if DenIn = rational.one then
            OuterCoeff = rational.'/'(NumOut, DenOut),
            InnerRat = NumIn
        else
            DenInPow = rat_pow(DenIn, int.'-'(N, 1)),
            NewInner = rational.'*'(NumIn, DenInPow),
            NewInnerInt = numer(NewInner),
            extract_nth_power(N, NewInnerInt, NumOut2, NumIn2),
            OuterCoeff = rational.'/'(
                rational.'*'(NumOut, NumOut2),
                rational.'*'(DenOut, DenIn)),
            InnerRat = NumIn2
        ),
        ( if OuterCoeff = rational.one, InnerRat = rational.one then
            Result = re_lit(rational.one)
        else if OuterCoeff = rational.one then
            Result = re_root(N, re_lit(InnerRat))
        else if InnerRat = rational.one then
            Result = re_lit(OuterCoeff)
        else
            Result = re_mul(re_lit(OuterCoeff), re_root(N, re_lit(InnerRat)))
        )
    else if rational.'<'(R, rational.zero), int.odd(N) then
        Result = re_neg(extract_root_lit(N,
            rational.'-'(rational.zero, R)))
    else
        Result = re_root(N, re_lit(R))
    ).

:- pred extract_nth_power(int::in, integer::in,
    rational::out, rational::out) is det.

extract_nth_power(N, M, Extracted, Remainder) :-
    ( if positive(M, PM) then
        Fs = factorise(PM),
        list.foldl2(( pred(PP::in, ExtAcc0::in, ExtAcc1::out,
                RemAcc0::in, RemAcc1::out) is det :-
            P = pp_prime(PP),
            E = pp_exponent(PP),
            Q = int.'//'(E, N),
            Rem = int.rem(E, N),
            ExtAcc1 = rational.'*'(ExtAcc0,
                rat_from_integer(integer_pow(P, Q))),
            RemAcc1 = rational.'*'(RemAcc0,
                rat_from_integer(integer_pow(P, Rem)))
        ), Fs, rational.one, Extracted, rational.one, Remainder)
    else
        Extracted = rational.one,
        Remainder = rational.one
    ).

:- func rat_from_integer(integer) = rational.

rat_from_integer(I) = R :-
    S = integer.to_string(I),
    ( if string.to_int(S, N) then
        R = rational(N, 1)
    else
        % Large integer fallback: split into chunks
        R = build_rat_from_big_int(I)
    ).

:- func build_rat_from_big_int(integer) = rational.

build_rat_from_big_int(I) = R :-
    ( if integer.'<'(I, integer.zero) then
        R = rational.'-'(rational.zero,
            build_rat_from_big_int(integer.'-'(integer.zero, I)))
    else if I = integer.zero then
        R = rational.zero
    else
        Billion = integer.det_from_string("1000000000"),
        Q = integer.'div'(I, Billion),
        Rem = integer.'-'(I, integer.'*'(Q, Billion)),
        QR = build_rat_from_big_int(Q),
        RemS = integer.to_string(Rem),
        ( if string.to_int(RemS, RemInt) then
            RR = rational(RemInt, 1)
        else
            RR = rational.zero
        ),
        R = rational.'+'(rational.'*'(QR, rational(1000000000, 1)), RR)
    ).

:- func integer_pow(integer, int) = integer.

integer_pow(X, N) = Result :-
    ( if N = 0 then
        Result = integer.one
    else if int.even(N) then
        Half = integer_pow(X, int.'//'(N, 2)),
        Result = integer.'*'(Half, Half)
    else
        Result = integer.'*'(X, integer_pow(X, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Collect coefficients in products
%---------------------------------------------------------------------------%

collect_coefficients(Expr) = Result :-
    ( if Expr = re_mul(_, _) then
        Factors = flatten_mul(Expr),
        ProcessedFactors = list.map(collect_coefficients, Factors),
        partition_lits(ProcessedFactors, Lits, Rest),
        Coeff = list.foldl(rat_mul, Lits, rational.one),
        Body = build_mul(Rest),
        Result = apply_coeff(Coeff, Body)
    else if Expr = re_neg(A) then
        A1 = collect_coefficients(A),
        ( if A1 = re_lit(R) then
            Result = re_lit(rational.'-'(rational.zero, R))
        else
            Result = re_neg(A1)
        )
    else if Expr = re_add(A, B) then
        Result = re_add(collect_coefficients(A),
                        collect_coefficients(B))
    else if Expr = re_inv(A) then
        Result = re_inv(collect_coefficients(A))
    else if Expr = re_root(N, A) then
        Result = re_root(N, collect_coefficients(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(collect_coefficients(A), N)
    else
        Result = Expr
    ).

:- func rat_mul(rational, rational) = rational.
rat_mul(A, B) = rational.'*'(A, B).

:- func flatten_mul(rad_expr(rational)) = list(rad_expr(rational)).

flatten_mul(Expr) = Result :-
    ( if Expr = re_mul(A, B) then
        Result = flatten_mul(A) ++ flatten_mul(B)
    else
        Result = [Expr]
    ).

:- pred partition_lits(list(rad_expr(rational))::in,
    list(rational)::out, list(rad_expr(rational))::out) is det.

partition_lits([], [], []).
partition_lits([X | Xs], Lits, Rest) :-
    partition_lits(Xs, Lits0, Rest0),
    ( if X = re_lit(R) then
        Lits = [R | Lits0],
        Rest = Rest0
    else if X = re_inv(re_lit(R)), not R = rational.zero then
        Lits = [rational.'/'(rational.one, R) | Lits0],
        Rest = Rest0
    else
        Lits = Lits0,
        Rest = [X | Rest0]
    ).

:- func build_mul(list(rad_expr(rational))) = rad_expr(rational).

build_mul(Xs) = Result :-
    ( if Xs = [] then
        Result = re_lit(rational.one)
    else if Xs = [X] then
        Result = X
    else if Xs = [X | Rest], Rest = [_ | _] then
        Result = re_mul(X, build_mul(Rest))
    else
        unexpected($pred, "unreachable")
    ).

:- func apply_coeff(rational, rad_expr(rational)) = rad_expr(rational).

apply_coeff(C, Body) = Result :-
    ( if C = rational.zero then
        Result = re_lit(rational.zero)
    else if C = rational.one then
        Result = Body
    else if C = neg_one then
        Result = re_neg(Body)
    else if Body = re_lit(R) then
        Result = re_lit(rational.'*'(C, R))
    else
        Result = re_mul(re_lit(C), Body)
    ).

%---------------------------------------------------------------------------%
% Collect like terms in sums
%---------------------------------------------------------------------------%

collect_terms(Expr) = Result :-
    ( if Expr = re_add(_, _) then
        Terms = flatten_add(Expr),
        Processed = list.map(collect_terms, Terms),
        Grouped = group_terms(Processed),
        map.foldl(( pred(Base::in, C::in, Acc0::in, Acc1::out) is det :-
            ( if C = rational.zero then
                Acc1 = Acc0
            else
                Acc1 = [apply_coeff_term(C, Base) | Acc0]
            )
        ), Grouped, [], RebuiltRev),
        Rebuilt = list.sort(RebuiltRev),
        Result = build_add(Rebuilt)
    else if Expr = re_neg(A) then
        Result = re_neg(collect_terms(A))
    else if Expr = re_mul(A, B) then
        Result = re_mul(collect_terms(A), collect_terms(B))
    else if Expr = re_inv(A) then
        Result = re_inv(collect_terms(A))
    else if Expr = re_root(N, A) then
        Result = re_root(N, collect_terms(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(collect_terms(A), N)
    else
        Result = Expr
    ).

:- func flatten_add(rad_expr(rational)) = list(rad_expr(rational)).

flatten_add(Expr) = Result :-
    ( if Expr = re_add(A, B) then
        Result = flatten_add(A) ++ flatten_add(B)
    else
        Result = [Expr]
    ).

:- func split_coeff(rad_expr(rational))
    = {rational, rad_expr(rational)}.

split_coeff(Expr) = Result :-
    ( if Expr = re_mul(re_lit(C), Body) then
        Result = {C, Body}
    else if Expr = re_neg(E) then
        {C, B} = split_coeff(E),
        Result = {rational.'-'(rational.zero, C), B}
    else if Expr = re_lit(R) then
        Result = {R, re_lit(rational.one)}
    else
        Result = {rational.one, Expr}
    ).

:- func group_terms(list(rad_expr(rational)))
    = map(rad_expr(rational), rational).

group_terms(Terms) =
    list.foldl(( func(Term, Acc) = Res :-
        {C, Base} = split_coeff(Term),
        ( if map.search(Acc, Base, Old) then
            map.det_update(Base, rational.'+'(Old, C), Acc, Res)
        else
            map.det_insert(Base, C, Acc, Res)
        )
    ), Terms, map.init).

:- func apply_coeff_term(rational, rad_expr(rational)) = rad_expr(rational).

apply_coeff_term(C, Base) = Result :-
    ( if C = rational.one, Base = re_lit(R), R = rational.one then
        Result = re_lit(rational.one)
    else if Base = re_lit(R), R = rational.one then
        Result = re_lit(C)
    else if C = rational.one then
        Result = Base
    else if C = neg_one then
        Result = re_neg(Base)
    else
        Result = re_mul(re_lit(C), Base)
    ).

:- func build_add(list(rad_expr(rational))) = rad_expr(rational).

build_add(Xs) = Result :-
    ( if Xs = [] then
        Result = re_lit(rational.zero)
    else if Xs = [X] then
        Result = X
    else if Xs = [X | Rest], Rest = [_ | _] then
        Result = list.foldl(
            ( func(T, Acc) = re_add(Acc, T) ), Rest, X)
    else
        unexpected($pred, "unreachable")
    ).

%---------------------------------------------------------------------------%
% Sort commutative operators
%---------------------------------------------------------------------------%

:- func sort_commutative(rad_expr(rational)) = rad_expr(rational).

sort_commutative(Expr) = Result :-
    ( if Expr = re_add(_, _) then
        Terms = flatten_add(Expr),
        Sorted = list.sort(list.map(sort_commutative, Terms)),
        Result = build_add(Sorted)
    else if Expr = re_mul(_, _) then
        Factors = flatten_mul(Expr),
        Sorted = list.sort(list.map(sort_commutative, Factors)),
        Result = build_mul(Sorted)
    else if Expr = re_neg(A) then
        Result = re_neg(sort_commutative(A))
    else if Expr = re_inv(A) then
        Result = re_inv(sort_commutative(A))
    else if Expr = re_root(N, A) then
        Result = re_root(N, sort_commutative(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(sort_commutative(A), N)
    else
        Result = Expr
    ).

%---------------------------------------------------------------------------%
% Distribute
%---------------------------------------------------------------------------%

distribute(Expr) = Result :-
    ( if Expr = re_mul(re_lit(C), R), is_sum(R) then
        Terms = flatten_add_s(R),
        Result = rebuild_add_s(list.map(
            ( func(T) = re_mul(re_lit(C), T) ), Terms))
    else if Expr = re_mul(L, re_lit(C)), is_sum(L) then
        Terms = flatten_add_s(L),
        Result = rebuild_add_s(list.map(
            ( func(T) = re_mul(T, re_lit(C)) ), Terms))
    else if Expr = re_mul(re_lit(C), re_neg(A)) then
        Result = re_neg(distribute(re_mul(re_lit(C), A)))
    else if Expr = re_mul(re_neg(A), re_lit(C)) then
        Result = re_neg(distribute(re_mul(A, re_lit(C))))
    else if Expr = re_mul(L, R),
        not L = re_lit(_), not R = re_lit(_),
        is_tiny_sum(L), not is_sum(R)
    then
        Terms = flatten_add_s(L),
        Result = rebuild_add_s(list.map(
            ( func(T) = distribute(re_mul(T, R)) ), Terms))
    else if Expr = re_mul(L, R),
        not L = re_lit(_), not R = re_lit(_),
        not is_sum(L), is_tiny_sum(R)
    then
        Terms = flatten_add_s(R),
        Result = rebuild_add_s(list.map(
            ( func(T) = distribute(re_mul(L, T)) ), Terms))
    else if Expr = re_mul(A, B) then
        Result = re_mul(distribute(A), distribute(B))
    else if Expr = re_neg(A) then
        Result = re_neg(distribute(A))
    else if Expr = re_add(A, B) then
        Result = re_add(distribute(A), distribute(B))
    else if Expr = re_inv(A) then
        Result = re_inv(distribute(A))
    else if Expr = re_root(N, A) then
        Result = re_root(N, distribute(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(distribute(A), N)
    else
        Result = Expr
    ).

:- pred is_sum(rad_expr(K)::in) is semidet.

is_sum(re_add(_, _)).
is_sum(re_neg(A)) :- is_sum(A).

:- pred is_tiny_sum(rad_expr(K)::in) is semidet.

is_tiny_sum(E) :-
    is_sum(E),
    Terms = flatten_add_s(E),
    int.'=<'(list.length(Terms), 2).

:- func flatten_add_s(rad_expr(K)) = list(rad_expr(K)).

flatten_add_s(Expr) = Result :-
    ( if Expr = re_add(A, B) then
        Result = flatten_add_s(A) ++ flatten_add_s(B)
    else if Expr = re_neg(A) then
        Result = list.map(( func(T) = re_neg(T) ), flatten_add_s(A))
    else
        Result = [Expr]
    ).

:- func rebuild_add_s(list(rad_expr(K))) = rad_expr(K).

rebuild_add_s(Xs) = Result :-
    ( if Xs = [X] then
        Result = X
    else if Xs = [X | Rest], Rest = [_ | _] then
        Result = list.foldl(
            ( func(T, Acc) = re_add(Acc, T) ), Rest, X)
    else
        unexpected($pred, "empty list in rebuild_add_s")
    ).

%---------------------------------------------------------------------------%
:- end_module rad_normalize.
%---------------------------------------------------------------------------%
