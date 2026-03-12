%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: dyn_tower.m
% Main author: nyc
%
% Dynamically-nested algebraic extension tower.
%
% Unlike ext_elem which uses static type nesting, tower_elem uses a
% recursive data type supporting towers of arbitrary runtime-determined
% depth.  Elements support full field arithmetic and can be converted
% to radical expressions.
%
%---------------------------------------------------------------------------%

:- module dyn_tower.
:- interface.

:- import_module list.
:- import_module integer.
:- import_module maybe.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

:- type tower_elem
    --->    t_rat(rational)
    ;       t_ext(
                te_coeffs :: list(tower_elem),
                te_level  :: tower_level
            ).

:- type tower_level
    --->    tower_level(
                tl_id       :: int,
                tl_degree   :: int,
                tl_root_deg :: int,
                tl_radicand :: tower_elem
            ).

    % Check if a tower element is zero.
    %
:- pred t_is_zero(tower_elem::in) is semidet.

    % Get the outermost tower level, or no for t_rat.
    %
:- func t_level(tower_elem) = maybe(tower_level).

    % Outermost level ID, or -1 for t_rat.
    %
:- func level_id(tower_elem) = int.

    % Promote to a given level by embedding as constant.
    %
:- func promote_to(tower_level, tower_elem) = tower_elem.

    % Adjoin an nth root: create a new level where α^n = r.
    % Returns (level, generator).
    %
:- func adjoin_tower_root(int, int, tower_elem) = {tower_level, tower_elem}.

    % Convert a tower element to a radical expression.
    %
:- func tower_to_rad_expr(tower_elem) = rad_expr(rational).

    % Tower element arithmetic.
    %
:- func t_add(tower_elem, tower_elem) = tower_elem.
:- func t_sub(tower_elem, tower_elem) = tower_elem.
:- func t_neg(tower_elem) = tower_elem.
:- func t_mul(tower_elem, tower_elem) = tower_elem.
:- func t_inv(tower_elem) = tower_elem.
:- func t_div(tower_elem, tower_elem) = tower_elem.
:- func t_from_int(int) = tower_elem.

    % Equality test.
    %
:- pred t_eq(tower_elem::in, tower_elem::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module require.
:- use_module string.

%---------------------------------------------------------------------------%
% Zero checking, level inspection
%---------------------------------------------------------------------------%

t_is_zero(t_rat(R)) :- integer.is_zero(numer(R)).
t_is_zero(t_ext(Cs, _)) :- list.all_true(t_is_zero, Cs).

t_level(t_rat(_)) = no.
t_level(t_ext(_, L)) = yes(L).

level_id(t_rat(_)) = -1.
level_id(t_ext(_, L)) = tl_id(L).

%---------------------------------------------------------------------------%
% Equality
%---------------------------------------------------------------------------%

:- pred levels_eq(tower_level::in, tower_level::in) is semidet.

levels_eq(A, B) :- tl_id(A) = tl_id(B).

t_eq(t_rat(A), t_rat(B)) :- A = B.
t_eq(t_ext(Cs1, L1), t_ext(Cs2, L2)) :-
    levels_eq(L1, L2),
    eq_coeffs(Cs1, Cs2).
t_eq(t_rat(A), t_ext(Cs, _)) :-
    eq_coeffs([t_rat(A)], Cs).
t_eq(t_ext(Cs, _), t_rat(A)) :-
    eq_coeffs(Cs, [t_rat(A)]).

:- pred eq_coeffs(list(tower_elem)::in, list(tower_elem)::in) is semidet.

eq_coeffs([], []).
eq_coeffs([], Bs) :- list.all_true(t_is_zero, Bs).
eq_coeffs(As, []) :- As = [_ | _], list.all_true(t_is_zero, As).
eq_coeffs([A | As], [B | Bs]) :- t_eq(A, B), eq_coeffs(As, Bs).

%---------------------------------------------------------------------------%
% Trimming and smart constructor
%---------------------------------------------------------------------------%

:- func trim_te(list(tower_elem)) = list(tower_elem).

trim_te(Cs) = list.reverse(drop_while_zero(list.reverse(Cs))).

:- func drop_while_zero(list(tower_elem)) = list(tower_elem).

drop_while_zero(Xs) = Result :-
    ( if Xs = [H | T], t_is_zero(H) then
        Result = drop_while_zero(T)
    else
        Result = Xs
    ).

:- func mk_t_ext(list(tower_elem), tower_level) = tower_elem.

mk_t_ext(Cs, L) = Result :-
    Trimmed = trim_te(Cs),
    ( if Trimmed = [] then
        Result = t_rat(rational.zero)
    else if Trimmed = [C], C = t_rat(_) then
        Result = C
    else
        Result = t_ext(pad_to(tl_degree(L), Trimmed), L)
    ).

:- func pad_to(int, list(tower_elem)) = list(tower_elem).

pad_to(N, Cs) = Result :-
    Len = list.length(Cs),
    ( if int.'>='(Len, N) then
        Result = list.take_upto(N, Cs)
    else
        Result = Cs ++ list.duplicate(int.'-'(N, Len), t_rat(rational.zero))
    ).

%---------------------------------------------------------------------------%
% Promotion
%---------------------------------------------------------------------------%

promote_to(Lvl, E) = Result :-
    ( if E = t_rat(_) then
        Result = t_ext(
            [E | list.duplicate(int.'-'(tl_degree(Lvl), 1),
                                t_rat(rational.zero))],
            Lvl)
    else if E = t_ext(_, L) then
        ( if levels_eq(L, Lvl) then
            Result = E
        else if int.'<'(tl_id(L), tl_id(Lvl)) then
            Result = t_ext(
                [E | list.duplicate(int.'-'(tl_degree(Lvl), 1),
                                    t_rat(rational.zero))],
                Lvl)
        else
            require.error("promote_to: cannot demote")
        )
    else
        Result = E
    ).

%---------------------------------------------------------------------------%
% Arithmetic
%---------------------------------------------------------------------------%

t_add(A, B) = Result :-
    ( if A = t_rat(RA), B = t_rat(RB) then
        Result = t_rat(rational.'+'(RA, RB))
    else if A = t_rat(_), B = t_ext(Bs, L) then
        Result = t_ext(add_coeffs([A], Bs), L)
    else if A = t_ext(As, L), B = t_rat(_) then
        Result = t_ext(add_coeffs(As, [B]), L)
    else if A = t_ext(As, L1), B = t_ext(Bs, L2) then
        ( if levels_eq(L1, L2) then
            Result = mk_t_ext(add_coeffs(As, Bs), L1)
        else if int.'<'(tl_id(L1), tl_id(L2)) then
            Result = t_add(promote_to(L2, A), B)
        else
            Result = t_add(A, promote_to(L1, B))
        )
    else
        Result = t_rat(rational.zero)
    ).

t_sub(A, B) = t_add(A, t_neg(B)).

t_neg(E) = Result :-
    ( if E = t_rat(R) then
        Result = t_rat(rational.'-'(rational.zero, R))
    else if E = t_ext(Cs, L) then
        Result = t_ext(list.map(t_neg, Cs), L)
    else
        Result = E
    ).

t_mul(A, B) = Result :-
    ( if A = t_rat(RA), B = t_rat(RB) then
        Result = t_rat(rational.'*'(RA, RB))
    else if A = t_rat(_), B = t_ext(Bs, L) then
        Result = mk_t_ext(list.map(( func(X) = t_mul(A, X) ), Bs), L)
    else if A = t_ext(As, L), B = t_rat(_) then
        Result = mk_t_ext(list.map(( func(X) = t_mul(X, B) ), As), L)
    else if A = t_ext(As, L1), B = t_ext(Bs, L2) then
        ( if levels_eq(L1, L2) then
            Result = mk_t_ext(
                reduce_coeffs(L1, poly_mul_te(As, Bs)), L1)
        else if int.'<'(tl_id(L1), tl_id(L2)) then
            Result = t_mul(promote_to(L2, A), B)
        else
            Result = t_mul(A, promote_to(L1, B))
        )
    else
        Result = t_rat(rational.zero)
    ).

t_inv(E) = Result :-
    ( if t_is_zero(E) then
        require.error("t_inv: division by zero")
    else if E = t_rat(R) then
        Result = t_rat(rational.'/'(rational.one, R))
    else if E = t_ext(Cs, L) then
        Result = t_inv_ext(Cs, L)
    else
        require.error("t_inv: impossible")
    ).

t_div(A, B) = t_mul(A, t_inv(B)).

t_from_int(N) = t_rat(rational(N, 1)).

%---------------------------------------------------------------------------%
% Coefficient-list arithmetic
%---------------------------------------------------------------------------%

:- func add_coeffs(list(tower_elem), list(tower_elem)) = list(tower_elem).

add_coeffs([], Bs) = Bs.
add_coeffs(As, []) = As :-
    As = [_ | _].
add_coeffs([A | As], [B | Bs]) = [t_add(A, B) | add_coeffs(As, Bs)].

:- func poly_mul_te(list(tower_elem), list(tower_elem))
    = list(tower_elem).

poly_mul_te(As, Bs) = Result :-
    ( if As = [] then
        Result = []
    else if Bs = [] then
        Result = []
    else
        RLen = int.'-'(int.'+'(list.length(As), list.length(Bs)), 1),
        Zeros = list.duplicate(RLen, t_rat(rational.zero)),
        Terms = list.condense(
            list.map(
                ( func({I, A}) =
                    list.map(
                        ( func({J, B}) = {int.'+'(I, J), t_mul(A, B)} ),
                        indexed(Bs))
                ),
                indexed(As))),
        Result = list.foldl(
            ( func({Idx, C}, Acc) = add_at(Idx, C, Acc) ),
            Terms, Zeros)
    ).

:- func indexed(list(tower_elem)) = list({int, tower_elem}).

indexed(Xs) = indexed_from(0, Xs).

:- func indexed_from(int, list(tower_elem)) = list({int, tower_elem}).

indexed_from(_, []) = [].
indexed_from(I, [X | Xs]) = [{I, X} | indexed_from(int.'+'(I, 1), Xs)].

:- func add_at(int, tower_elem, list(tower_elem)) = list(tower_elem).

add_at(_, _, []) = [].
add_at(I, C, [X | Xs]) = Result :-
    ( if I = 0 then
        Result = [t_add(X, C) | Xs]
    else
        Result = [X | add_at(int.'-'(I, 1), C, Xs)]
    ).

:- func reduce_coeffs(tower_level, list(tower_elem)) = list(tower_elem).

reduce_coeffs(L, Cs) = Result :-
    N = tl_degree(L),
    ( if int.'=<'(list.length(Cs), N) then
        Result = Cs
    else
        Lo = list.take_upto(N, Cs),
        Hi = drop_n(N, Cs),
        Shifted = list.map(
            ( func(X) = t_mul(X, tl_radicand(L)) ), Hi),
        Combined = add_coeffs(Lo, Shifted),
        Result = reduce_coeffs(L, Combined)
    ).

:- func drop_n(int, list(T)) = list(T).

drop_n(N, Xs) = Result :-
    ( if int.'=<'(N, 0) then
        Result = Xs
    else if Xs = [_ | Tail] then
        Result = drop_n(int.'-'(N, 1), Tail)
    else
        Result = []
    ).

%---------------------------------------------------------------------------%
% Inversion via extended GCD
%---------------------------------------------------------------------------%

:- func t_inv_ext(list(tower_elem), tower_level) = tower_elem.

t_inv_ext(Cs, L) = Result :-
    ( if list.all_true(t_is_zero, Cs) then
        require.error("t_inv_ext: division by zero")
    else
        MinP = min_poly_coeffs(L),
        {_G, S, _T} = poly_ext_gcd(trim_te(Cs), MinP),
        Reduced = reduce_coeffs(L, S),
        Result = mk_t_ext(Reduced, L)
    ).

:- func min_poly_coeffs(tower_level) = list(tower_elem).

min_poly_coeffs(L) = Result :-
    NegR = t_neg(tl_radicand(L)),
    Zeros = list.duplicate(int.'-'(tl_degree(L), 1), t_rat(rational.zero)),
    Result = [NegR | Zeros] ++ [t_rat(rational.one)].

:- func poly_ext_gcd(list(tower_elem), list(tower_elem))
    = {list(tower_elem), list(tower_elem), list(tower_elem)}.

poly_ext_gcd(A, B) =
    poly_ext_gcd_loop(A, B,
        [t_rat(rational.one)], [],
        [], [t_rat(rational.one)]).

:- func poly_ext_gcd_loop(
    list(tower_elem), list(tower_elem),
    list(tower_elem), list(tower_elem),
    list(tower_elem), list(tower_elem))
    = {list(tower_elem), list(tower_elem), list(tower_elem)}.

poly_ext_gcd_loop(R0, R1, S0, S1, T0, T1) = Result :-
    ( if list.all_true(t_is_zero, R1) then
        LC = last_nonzero(R0),
        LCInv = t_inv(LC),
        R0P = list.map(( func(X) = t_mul(X, LCInv) ), R0),
        S0P = list.map(( func(X) = t_mul(X, LCInv) ), S0),
        T0P = list.map(( func(X) = t_mul(X, LCInv) ), T0),
        Result = {trim_te(R0P), trim_te(S0P), trim_te(T0P)}
    else
        {Q, R} = poly_div_mod(R0, R1),
        S2 = poly_sub(S0, poly_mul_te(Q, S1)),
        T2 = poly_sub(T0, poly_mul_te(Q, T1)),
        Result = poly_ext_gcd_loop(R1, R, S1, S2, T1, T2)
    ).

:- func last_nonzero(list(tower_elem)) = tower_elem.

last_nonzero(Xs) = Result :-
    Trimmed = trim_te(Xs),
    ( if Trimmed = [] then
        Result = t_rat(rational.one)
    else
        Result = list.det_last(Trimmed)
    ).

:- func poly_div_mod(list(tower_elem), list(tower_elem))
    = {list(tower_elem), list(tower_elem)}.

poly_div_mod(F, G) = Result :-
    F1 = trim_te(F),
    G1 = trim_te(G),
    DegF = int.'-'(list.length(F1), 1),
    DegG = int.'-'(list.length(G1), 1),
    ( if int.'<'(DegF, DegG) then
        Result = {[], F1}
    else
        LCG = list.det_last(G1),
        Result = poly_div_loop([], F1, G1, DegG, LCG)
    ).

:- func poly_div_loop(list(tower_elem), list(tower_elem),
    list(tower_elem), int, tower_elem)
    = {list(tower_elem), list(tower_elem)}.

poly_div_loop(Q, R, G, DegG, LCG) = Result :-
    R1 = trim_te(R),
    DegR = int.'-'(list.length(R1), 1),
    ( if int.'<'(DegR, DegG) then
        Result = {trim_te(Q), R1}
    else if list.all_true(t_is_zero, R1) then
        Result = {trim_te(Q), R1}
    else
        LCR = list.det_last(R1),
        C = t_div(LCR, LCG),
        D = int.'-'(DegR, DegG),
        Term = list.duplicate(D, t_rat(rational.zero)) ++ [C],
        R2 = poly_sub(R1, poly_mul_te(Term, G)),
        Result = poly_div_loop(add_coeffs(Q, Term), R2, G, DegG, LCG)
    ).

:- func poly_sub(list(tower_elem), list(tower_elem)) = list(tower_elem).

poly_sub([], Bs) = list.map(t_neg, Bs).
poly_sub(As, []) = As :- As = [_ | _].
poly_sub([A | As], [B | Bs]) = [t_sub(A, B) | poly_sub(As, Bs)].

%---------------------------------------------------------------------------%
% Tower construction
%---------------------------------------------------------------------------%

adjoin_tower_root(LvlId, N, R) = {Level, Gen} :-
    Level = tower_level(LvlId, N, N, R),
    Gen = t_ext(
        [t_rat(rational.zero), t_rat(rational.one) |
         list.duplicate(int.'-'(N, 2), t_rat(rational.zero))],
        Level).

%---------------------------------------------------------------------------%
% Conversion to RadExpr
%---------------------------------------------------------------------------%

tower_to_rad_expr(t_rat(R)) = re_lit(R).
tower_to_rad_expr(t_ext(Cs, Level)) = Result :-
    Gen = re_root(tl_root_deg(Level),
                  tower_to_rad_expr(tl_radicand(Level))),
    Terms = make_terms(Cs, Gen, 0),
    ( if Terms = [] then
        Result = re_lit(rational.zero)
    else if Terms = [T] then
        Result = T
    else
        Result = list.foldl(
            ( func(X, Acc) = re_add(Acc, X) ),
            list.det_tail(Terms),
            list.det_head(Terms))
    ).

:- func make_terms(list(tower_elem), rad_expr(rational), int)
    = list(rad_expr(rational)).

make_terms([], _, _) = [].
make_terms([C | Cs], Gen, I) = Result :-
    ( if t_is_zero(C) then
        Result = make_terms(Cs, Gen, int.'+'(I, 1))
    else
        CE = tower_to_rad_expr(C),
        ( if I = 0 then
            Term = CE
        else if I = 1 then
            Term = re_mul(CE, Gen)
        else
            Term = re_mul(CE, re_pow(Gen, I))
        ),
        Result = [Term | make_terms(Cs, Gen, int.'+'(I, 1))]
    ).

%---------------------------------------------------------------------------%
:- end_module dyn_tower.
%---------------------------------------------------------------------------%
