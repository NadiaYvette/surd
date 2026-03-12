%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: latex.m
% Main author: nyc
%
% LaTeX rendering of radical expressions.
%
%---------------------------------------------------------------------------%

:- module latex.
:- interface.

:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Render a radical expression as a LaTeX math-mode string.
    %
:- func latex(rad_expr(rational)) = string.

    % Render with precedence context.
    %
:- func latex_prec(int, rad_expr(rational)) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- use_module int.
:- import_module integer.
:- import_module list.
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

%---------------------------------------------------------------------------%
% Main rendering
%---------------------------------------------------------------------------%

latex(Expr) = latex_prec(0, Expr).

latex_prec(P, Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = latex_rat(R)
    else if Expr = re_neg(Inner) then
        ( if Inner = re_add(_, _) then
            Terms = flatten_add(Inner),
            NegTerms = list.map(neg_term, Terms),
            Result = parens_if(gt(P, prec_add),
                render_terms(NegTerms))
        else if Inner = re_mul(re_lit(C), Rest) then
            Result = latex_prec(P,
                re_mul(re_lit(rational.'-'(rational.zero, C)), Rest))
        else if Inner = re_lit(R) then
            Result = latex_prec(P, re_lit(rational.'-'(rational.zero, R)))
        else
            Result = parens_if(gt(P, prec_neg),
                string.append("-", latex_prec(prec_neg, Inner)))
        )
    else if Expr = re_add(_, _) then
        Terms = flatten_add(Expr),
        Result = parens_if(gt(P, prec_add), render_terms(Terms))
    else if Expr = re_mul(A, re_inv(B)) then
        Result = parens_if(gt(P, prec_mul),
            string.append_list(["\\frac{", latex_prec(0, A), "}{",
                latex_prec(0, B), "}"]))
    else if Expr = re_mul(re_inv(A), B) then
        Result = parens_if(gt(P, prec_mul),
            string.append_list(["\\frac{", latex_prec(0, B), "}{",
                latex_prec(0, A), "}"]))
    else if Expr = re_inv(Inner2) then
        Result = parens_if(gt(P, prec_mul),
            string.append_list(["\\frac{1}{", latex_prec(0, Inner2), "}"]))
    else if Expr = re_root(2, re_lit(R)),
            rational.'<'(R, rational.zero),
            rational.'+'(R, rational.one) = rational.zero then
        Result = "\\mathrm{i}"
    else if Expr = re_root(2, Inner3) then
        Result = string.append_list(["\\sqrt{",
            latex_radicand(Inner3), "}"])
    else if Expr = re_root(N, Inner4) then
        Result = string.append_list(["\\sqrt[",
            string.int_to_string(N), "]{", latex_radicand(Inner4), "}"])
    else if Expr = re_pow(_, 0) then
        Result = "1"
    else if Expr = re_pow(Base, Exp) then
        ( if int.'<'(Exp, 0) then
            Result = latex_prec(P,
                re_inv(re_pow(Base, int.'-'(0, Exp))))
        else if Exp = 1 then
            Result = latex_prec(P, Base)
        else
            Result = parens_if(gt(P, prec_pow),
                string.append_list([latex_base(Base), "^{",
                    string.int_to_string(Exp), "}"]))
        )
    else if Expr = re_mul(_, _) then
        Factors = flatten_mul(Expr),
        Result = parens_if(gt(P, prec_mul),
            render_factors(Factors))
    else
        Result = "?"
    ).

%---------------------------------------------------------------------------%
% Base rendering for powers
%---------------------------------------------------------------------------%

:- func latex_base(rad_expr(rational)) = string.

latex_base(E) = Result :-
    ( if E = re_root(_, _) then
        Result = string.append_list(["\\left(", latex_prec(0, E), "\\right)"])
    else if E = re_add(_, _) then
        Result = string.append_list(["\\left(", latex_prec(0, E), "\\right)"])
    else if E = re_mul(_, _) then
        Result = string.append_list(["\\left(", latex_prec(0, E), "\\right)"])
    else if E = re_neg(_) then
        Result = string.append_list(["\\left(", latex_prec(0, E), "\\right)"])
    else if E = re_inv(_) then
        Result = string.append_list(["\\left(", latex_prec(0, E), "\\right)"])
    else
        Result = latex_prec(prec_pow, E)
    ).

%---------------------------------------------------------------------------%
% Radicand rendering
%---------------------------------------------------------------------------%

:- func latex_radicand(rad_expr(rational)) = string.

latex_radicand(E) = Result :-
    ( if E = re_lit(R) then
        N = numer(R),
        D = denom(R),
        ( if D = integer.one then
            Result = integer.to_string(N)
        else if integer.'<'(N, integer.zero) then
            Result = string.append_list(["-\\frac{",
                integer.to_string(integer.abs(N)), "}{",
                integer.to_string(D), "}"])
        else
            Result = string.append_list(["\\frac{",
                integer.to_string(N), "}{",
                integer.to_string(D), "}"])
        )
    else
        Result = latex_prec(0, E)
    ).

%---------------------------------------------------------------------------%
% Rational rendering
%---------------------------------------------------------------------------%

:- func latex_rat(rational) = string.

latex_rat(R) = Result :-
    N = numer(R),
    D = denom(R),
    ( if D = integer.one then
        Result = integer.to_string(N)
    else if integer.'<'(N, integer.zero) then
        Result = string.append_list(["-\\frac{",
            integer.to_string(integer.abs(N)), "}{",
            integer.to_string(D), "}"])
    else
        Result = string.append_list(["\\frac{",
            integer.to_string(N), "}{",
            integer.to_string(D), "}"])
    ).

%---------------------------------------------------------------------------%
% Helpers
%---------------------------------------------------------------------------%

:- func gt(int, int) = bool.

gt(A, B) = ( if int.'>'(A, B) then yes else no ).

:- func parens_if(bool, string) = string.

parens_if(yes, S) = string.append_list(["\\left(", S, "\\right)"]).
parens_if(no, S) = S.

:- func neg_term({bool, rad_expr(rational)}) = {bool, rad_expr(rational)}.

neg_term({S, T}) = {flip_bool(S), T}.

:- func flip_bool(bool) = bool.

flip_bool(yes) = no.
flip_bool(no) = yes.

%---------------------------------------------------------------------------%
% Flatten and render add chains
%---------------------------------------------------------------------------%

:- func flatten_add(rad_expr(rational)) = list({bool, rad_expr(rational)}).

flatten_add(Expr) = Result :-
    ( if Expr = re_add(A, B) then
        Result = list.append(flatten_add(A), flatten_add(B))
    else if Expr = re_neg(Inner) then
        Result = list.map(neg_term, flatten_add(Inner))
    else if Expr = re_lit(R), rational.'<'(R, rational.zero) then
        Result = [{no, re_lit(rational.'-'(rational.zero, R))}]
    else if Expr = re_mul(re_neg(A2), B2) then
        Result = [{no, re_mul(A2, B2)}]
    else if Expr = re_mul(_, _) then
        Factors = flatten_mul(Expr),
        ( if Factors = [re_lit(R2) | Rest], rational.'<'(R2, rational.zero) then
            Result = [{no, rebuild_mul([re_lit(rational.'-'(rational.zero, R2)) | Rest])}]
        else
            Result = [{yes, Expr}]
        )
    else
        Result = [{yes, Expr}]
    ).

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

:- func render_terms(list({bool, rad_expr(rational)})) = string.

render_terms(Terms) = Result :-
    ( if Terms = [] then
        Result = "0"
    else if Terms = [{S, T} | Rest] then
        Hd = ( if S = yes then latex_prec(prec_add, T)
               else string.append("-", latex_prec(prec_mul, T)) ),
        Result = string.append(Hd, render_rest(Rest))
    else
        Result = "0"
    ).

:- func render_rest(list({bool, rad_expr(rational)})) = string.

render_rest([]) = "".
render_rest([{S, E} | Rest]) = Result :-
    This = ( if S = yes then string.append(" + ", latex_prec(prec_add, E))
             else string.append(" - ", latex_prec(prec_mul, E)) ),
    Result = string.append(This, render_rest(Rest)).

%---------------------------------------------------------------------------%
% Flatten and render mul chains
%---------------------------------------------------------------------------%

:- func flatten_mul(rad_expr(rational)) = list(rad_expr(rational)).

flatten_mul(Expr) = Result :-
    ( if Expr = re_mul(A, B) then
        Result = list.append(flatten_mul(A), flatten_mul(B))
    else
        Result = [Expr]
    ).

:- func render_factors(list(rad_expr(rational))) = string.

render_factors(Factors) = Result :-
    ( if Factors = [] then
        Result = "1"
    else if Factors = [X] then
        Result = latex_prec(prec_mul, X)
    else if Factors = [re_lit(C) | Rest] then
        ( if C = rational.one then
            Result = join_mul(list.map(latex_prec_pow, Rest))
        else if C = rational.'-'(rational.zero, rational.one) then
            Result = string.append("-",
                join_mul(list.map(latex_prec_pow, Rest)))
        else
            Result = string.append_list([latex_rat(C), " \\cdot ",
                join_mul(list.map(latex_prec_pow, Rest))])
        )
    else
        Result = join_mul(list.map(latex_prec_pow, Factors))
    ).

:- func latex_prec_pow(rad_expr(rational)) = string.
latex_prec_pow(E) = latex_prec(prec_pow, E).

:- func join_mul(list(string)) = string.

join_mul([]) = "".
join_mul([X]) = X.
join_mul([X | Xs]) = Result :-
    Xs = [_ | _],
    Result = string.append(X, join_mul_rest(Xs)).

:- func join_mul_rest(list(string)) = string.

join_mul_rest([]) = "".
join_mul_rest([X | Xs]) =
    string.append(string.append(" \\cdot ", X), join_mul_rest(Xs)).

%---------------------------------------------------------------------------%
:- end_module latex.
%---------------------------------------------------------------------------%
