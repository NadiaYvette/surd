%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: pretty.m
% Main author: nyc
%
% Pretty-printing radical expressions in human-readable mathematical notation.
%
% Includes common subexpression elimination (CSE): repeated subexpressions
% are named and shown as let-bindings.
%
%---------------------------------------------------------------------------%

:- module pretty.
:- interface.

:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Render a radical expression as a human-readable string.
    %
:- func pretty(rad_expr(rational)) = string.

    % Render with precedence context.
    %
:- func pretty_prec(int, rad_expr(rational)) = string.

    % Render with CSE: repeated subexpressions are shown as named
    % intermediates with let-bindings.
    %
:- func pretty_cse(rad_expr(rational)) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- use_module char.
:- use_module int.
:- import_module integer.
:- import_module list.
:- import_module map.
:- use_module string.

%---------------------------------------------------------------------------%
% Precedence levels
%---------------------------------------------------------------------------%

:- func prec_add = int.
prec_add = 1.

:- func prec_mul = int.
prec_mul = 2.

:- func prec_neg = int.
prec_neg = 3.

:- func prec_pow = int.
prec_pow = 4.

:- func prec_atom = int.
prec_atom = 5.

%---------------------------------------------------------------------------%
% Basic pretty-printing
%---------------------------------------------------------------------------%

pretty(Expr) = pretty_prec(0, Expr).

pretty_prec(P, Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = pretty_rat(R)
    else if Expr = re_neg(Inner) then
        ( if Inner = re_add(_, _) then
            Terms = flatten_add_basic(Inner),
            NegTerms = list.map(neg_term, Terms),
            Result = parens_if(gt(P, prec_add),
                render_terms_basic(NegTerms))
        else if Inner = re_mul(re_lit(C), Rest) then
            Result = pretty_prec(P, re_mul(re_lit(rational.'-'(rational.zero, C)), Rest))
        else if Inner = re_lit(R) then
            Result = pretty_prec(P, re_lit(rational.'-'(rational.zero, R)))
        else
            Result = parens_if(gt(P, prec_neg),
                string.append("-", pretty_prec(prec_neg, Inner)))
        )
    else if Expr = re_add(_, _) then
        Terms = flatten_add_basic(Expr),
        Result = parens_if(gt(P, prec_add),
            render_terms_basic(Terms))
    else if Expr = re_mul(A, re_inv(B)) then
        Result = parens_if(gt(P, prec_mul),
            string.append_list([pretty_prec(prec_mul, A), "/",
                pretty_prec(prec_pow, B)]))
    else if Expr = re_mul(_, _) then
        Factors = flatten_mul_basic(Expr),
        Result = parens_if(gt(P, prec_mul),
            render_factors_basic(Factors))
    else if Expr = re_inv(Inner2) then
        Result = parens_if(gt(P, prec_mul),
            string.append("1/", pretty_prec(prec_pow, Inner2)))
    else if Expr = re_root(2, re_lit(R)),
            rational.'<'(R, rational.zero),
            rational.'+'(R, rational.one) = rational.zero then
        % √(-1) = i
        Result = "i"
    else if Expr = re_root(2, Inner3) then
        Result = string.append("√", pretty_radicand_basic(Inner3))
    else if Expr = re_root(3, Inner4) then
        Result = string.append("∛", pretty_radicand_basic(Inner4))
    else if Expr = re_root(N, Inner5) then
        Result = string.append_list([string.int_to_string(N), "√",
            pretty_radicand_basic(Inner5)])
    else if Expr = re_pow(_, 0) then
        Result = "1"
    else if Expr = re_pow(Base, Exp) then
        ( if int.'<'(Exp, 0) then
            Result = pretty_prec(P, re_inv(re_pow(Base, int.'-'(0, Exp))))
        else if Exp = 1 then
            Result = pretty_prec(P, Base)
        else
            Result = parens_if(gt(P, prec_pow),
                string.append_list([pretty_prec(prec_pow, Base), "^",
                    string.int_to_string(Exp)]))
        )
    else
        Result = "?"
    ).

%---------------------------------------------------------------------------%
% Radicand rendering (basic)
%---------------------------------------------------------------------------%

:- func pretty_radicand_basic(rad_expr(rational)) = string.

pretty_radicand_basic(E) = Result :-
    ( if E = re_lit(R) then
        ( if rational.'>='(R, rational.zero),
             denom(R) = integer.one then
            Result = integer.to_string(numer(R))
        else
            Result = string.append_list(["(", pretty(E), ")"])
        )
    else if is_simple_basic(E) then
        Result = pretty_prec(prec_pow, E)
    else
        Result = string.append_list(["(", pretty(E), ")"])
    ).

:- pred is_simple_basic(rad_expr(rational)::in) is semidet.

is_simple_basic(re_lit(_)).
is_simple_basic(re_root(_, _)).

%---------------------------------------------------------------------------%
% Flatten and render add chains (basic)
%---------------------------------------------------------------------------%

:- func flatten_add_basic(rad_expr(rational)) = list({bool, rad_expr(rational)}).

flatten_add_basic(Expr) = Result :-
    ( if Expr = re_add(A, B) then
        Result = list.append(flatten_add_basic(A), flatten_add_basic(B))
    else if Expr = re_neg(Inner) then
        Result = list.map(neg_term, flatten_add_basic(Inner))
    else if Expr = re_lit(R), rational.'<'(R, rational.zero) then
        Result = [{no, re_lit(rational.'-'(rational.zero, R))}]
    else if Expr = re_mul(re_neg(A2), B2) then
        Result = [{no, re_mul(A2, B2)}]
    else if Expr = re_mul(_, _) then
        Factors = flatten_mul_basic(Expr),
        ( if Factors = [re_lit(R2) | Rest], rational.'<'(R2, rational.zero) then
            Result = [{no, rebuild_mul([re_lit(rational.'-'(rational.zero, R2)) | Rest])}]
        else
            Result = [{yes, Expr}]
        )
    else
        Result = [{yes, Expr}]
    ).

:- func neg_term({bool, rad_expr(rational)}) = {bool, rad_expr(rational)}.

neg_term({S, T}) = {flip_bool(S), T}.

:- func flip_bool(bool) = bool.

flip_bool(yes) = no.
flip_bool(no) = yes.

:- func render_terms_basic(list({bool, rad_expr(rational)})) = string.

render_terms_basic(Terms) = Result :-
    ( if Terms = [] then
        Result = "0"
    else if Terms = [{S, T} | Rest] then
        Hd = ( if S = yes then pretty_prec(prec_add, T)
               else string.append("-", pretty_prec(prec_mul, T)) ),
        Result = string.append(Hd, render_rest_basic(Rest))
    else
        Result = "0"
    ).

:- func render_rest_basic(list({bool, rad_expr(rational)})) = string.

render_rest_basic([]) = "".
render_rest_basic([{S, E} | Rest]) = Result :-
    This = ( if S = yes then string.append(" + ", pretty_prec(prec_add, E))
             else string.append(" - ", pretty_prec(prec_mul, E)) ),
    Result = string.append(This, render_rest_basic(Rest)).

%---------------------------------------------------------------------------%
% Flatten and render mul chains (basic)
%---------------------------------------------------------------------------%

:- func flatten_mul_basic(rad_expr(rational)) = list(rad_expr(rational)).

flatten_mul_basic(Expr) = Result :-
    ( if Expr = re_mul(A, B) then
        Result = list.append(flatten_mul_basic(A), flatten_mul_basic(B))
    else
        Result = [Expr]
    ).

:- func render_factors_basic(list(rad_expr(rational))) = string.

render_factors_basic(Factors) = Result :-
    ( if Factors = [] then
        Result = "1"
    else if Factors = [X] then
        Result = pretty_prec(prec_mul, X)
    else if Factors = [re_lit(C) | Rest] then
        ( if C = rational.one then
            Result = join_with("·", list.map(pretty_prec_pow, Rest))
        else if C = rational.'-'(rational.zero, rational.one) then
            Result = string.append("-",
                join_with("·", list.map(pretty_prec_pow, Rest)))
        else
            Result = string.append_list([pretty_rat(C), "·",
                join_with("·", list.map(pretty_prec_pow, Rest))])
        )
    else
        Result = join_with("·", list.map(pretty_prec_pow, Factors))
    ).

:- func pretty_prec_pow(rad_expr(rational)) = string.
pretty_prec_pow(E) = pretty_prec(prec_pow, E).

%---------------------------------------------------------------------------%
% Rebuild mul from list
%---------------------------------------------------------------------------%

:- func rebuild_mul(list(rad_expr(rational))) = rad_expr(rational).

rebuild_mul(Fs) = Result :-
    ( if Fs = [] then
        Result = re_lit(rational.one)
    else if Fs = [X] then
        Result = X
    else if Fs = [X | Xs] then
        Result = list.foldl(func(B, Acc) = re_mul(Acc, B), Xs, X)
    else
        Result = re_lit(rational.one)
    ).

%---------------------------------------------------------------------------%
% Rational number rendering
%---------------------------------------------------------------------------%

:- func pretty_rat(rational) = string.

pretty_rat(R) = Result :-
    N = numer(R),
    D = denom(R),
    ( if D = integer.one then
        Result = integer.to_string(N)
    else if integer.'<'(N, integer.zero) then
        Result = string.append_list(["(-", integer.to_string(integer.abs(N)),
            "/", integer.to_string(D), ")"])
    else
        Result = string.append_list(["(", integer.to_string(N),
            "/", integer.to_string(D), ")"])
    ).

%---------------------------------------------------------------------------%
% Utility
%---------------------------------------------------------------------------%

:- func gt(int, int) = bool.

gt(A, B) = ( if int.'>'(A, B) then yes else no ).

:- func parens_if(bool, string) = string.

parens_if(yes, S) = string.append_list(["(", S, ")"]).
parens_if(no, S) = S.

:- func join_with(string, list(string)) = string.

join_with(_, []) = "".
join_with(_, [X]) = X.
join_with(Sep, [X | Xs]) = Result :-
    Xs = [_ | _],
    Result = string.append(X, join_rest(Sep, Xs)).

:- func join_rest(string, list(string)) = string.

join_rest(_, []) = "".
join_rest(Sep, [X | Xs]) =
    string.append(string.append(Sep, X), join_rest(Sep, Xs)).

%---------------------------------------------------------------------------%
% CSE pretty-printing
%---------------------------------------------------------------------------%

pretty_cse(Expr) = Result :-
    Counts = count_subs(Expr, map.init),
    SharedKeys = map.foldl(func(K, V, Acc) =
        ( if int.'>='(V, 2), worth_naming(K) then [K | Acc] else Acc ),
        Counts, []),
    Sorted = sort_by_size(SharedKeys),
    NameList = make_names(0, Sorted),
    NameMap = build_name_map(Sorted, NameList, map.init),
    Bindings = make_bindings(Sorted, NameList, NameMap),
    Body = render_with(NameMap, 0, Expr),
    ( if Bindings = [] then
        Result = Body
    else
        BindingStrs = list.map(func(B) = string.append_list(["  ", B, "\n"]),
            Bindings),
        Result = string.append_list(["let\n" |
            list.append(BindingStrs, ["in ", Body])])
    ).

:- func count_subs(rad_expr(rational), map.map(rad_expr(rational), int))
    = map.map(rad_expr(rational), int).

count_subs(E, M0) = Result :-
    ( if map.search(M0, E, Old) then
        M1 = map.det_update(M0, E, int.'+'(Old, 1))
    else
        M1 = map.det_insert(M0, E, 1)
    ),
    ( if E = re_lit(_) then
        Result = M1
    else if E = re_neg(A) then
        Result = count_subs(A, M1)
    else if E = re_add(A, B) then
        Result = count_subs(B, count_subs(A, M1))
    else if E = re_mul(A, B) then
        Result = count_subs(B, count_subs(A, M1))
    else if E = re_inv(A) then
        Result = count_subs(A, M1)
    else if E = re_root(_, A) then
        Result = count_subs(A, M1)
    else if E = re_pow(A, _) then
        Result = count_subs(A, M1)
    else
        Result = M1
    ).

:- pred worth_naming(rad_expr(rational)::in) is semidet.

worth_naming(E) :-
    not E = re_lit(_),
    not ( E = re_neg(re_lit(_)) ),
    not ( E = re_root(_, re_lit(_)) ).

:- func sort_by_size(list(rad_expr(rational))) = list(rad_expr(rational)).

sort_by_size(Es) = list.sort(
    ( func(A, B) = R :-
        SA = re_size(A), SB = re_size(B),
        compare(R, SA, SB)
    ), Es).

:- func make_names(int, list(rad_expr(rational))) = list(string).

make_names(_, []) = [].
make_names(I, [_ | Rest]) = [var_name(I) | make_names(int.'+'(I, 1), Rest)].

:- func var_name(int) = string.

var_name(I) = Result :-
    ( if int.'<'(I, 26) then
        C = char.det_from_int(int.'+'(char.to_int('a'), I)),
        Result = string.from_char(C)
    else
        C = char.det_from_int(int.'+'(char.to_int('a'), int.mod(I, 26))),
        Result = string.append(string.from_char(C),
            string.int_to_string(int.'//'(I, 26)))
    ).

:- func build_name_map(list(rad_expr(rational)), list(string),
    map.map(rad_expr(rational), string)) = map.map(rad_expr(rational), string).

build_name_map([], _, M) = M.
build_name_map([_ | _], [], M) = M.
build_name_map([E | Es], [N | Ns], M) =
    build_name_map(Es, Ns, map.det_insert(M, E, N)).

:- func make_bindings(list(rad_expr(rational)), list(string),
    map.map(rad_expr(rational), string)) = list(string).

make_bindings([], _, _) = [].
make_bindings([_ | _], [], _) = [].
make_bindings([Sub | Subs], [Name | Names], NameMap) = Result :-
    AvailNames = map.delete(NameMap, Sub),
    Body = render_with(AvailNames, 0, Sub),
    Binding = string.append_list([Name, " = ", Body]),
    Result = [Binding | make_bindings(Subs, Names, NameMap)].

:- func render_with(map.map(rad_expr(rational), string), int,
    rad_expr(rational)) = string.

render_with(Names, P, E) = Result :-
    ( if map.search(Names, E, Name) then
        Result = parens_if(gt(P, prec_atom), Name)
    else
        Result = pretty_prec(P, E)
    ).

%---------------------------------------------------------------------------%
:- end_module pretty.
%---------------------------------------------------------------------------%
