%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: denest_sqrt.m
% Main author: nyc
%
% Square root denesting.
%
% Implements the Borodin-Fagin-Hopcroft-Tompa (1985) algorithm for
% denesting expressions of the form √(a + b√r) where a, b, r ∈ Q.
%
% If denesting is possible, √(a + b√r) = √x ± √y
% where x + y = a and x*y = b²r/4.
%
%---------------------------------------------------------------------------%

:- module denest_sqrt.
:- interface.

:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Try to denest √(a + b√r) into √x ± √y.
    % Returns {Sign, X, Y} such that √(a + b√r) = √x + Sign*√y,
    % or fails if denesting is not possible over Q.
    %
:- pred try_sqrt_denest(rational::in, rational::in, rational::in,
    {int, rational, rational}::out) is semidet.

    % Try to denest a radical expression that is a square root.
    % Looks for the pattern √(a + b·√r) and attempts denesting.
    %
:- pred denest_sqrt_expr(rad_expr(rational)::in,
    rad_expr(rational)::out) is semidet.

    % Recursively try to denest all square roots in an expression.
    %
:- func denest_sqrt(rad_expr(rational)) = rad_expr(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module integer.

%---------------------------------------------------------------------------%
% Core denesting algorithm
%---------------------------------------------------------------------------%

try_sqrt_denest(A, B, R, {Sign, X, Y}) :-
    Disc = rational.'-'(rational.'*'(A, A), rational.'*'(rational.'*'(B, B), R)),
    rational.'>='(Disc, rational.zero),
    is_rational_sqrt(Disc, SD),
    X = rational.'/'(rational.'+'(A, SD), rational(2, 1)),
    Y = rational.'/'(rational.'-'(A, SD), rational(2, 1)),
    rational.'>='(X, rational.zero),
    rational.'>='(Y, rational.zero),
    Sign = ( if rational.'>'(B, rational.zero) then 1 else -1 ).

%---------------------------------------------------------------------------%
% Perfect square test for rationals
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

%---------------------------------------------------------------------------%
% Exact integer square root
%---------------------------------------------------------------------------%

:- pred exact_integer_sqrt(integer::in, integer::out) is semidet.

exact_integer_sqrt(N, Root) :-
    integer.'>='(N, integer.zero),
    ( if N = integer.zero then
        Root = integer.zero
    else if N = integer.one then
        Root = integer.one
    else
        % Newton's method for integer square root
        isqrt_newton(N, N, Root),
        integer.'*'(Root, Root) = N
    ).

:- pred isqrt_newton(integer::in, integer::in, integer::out) is det.

isqrt_newton(N, Guess, Root) :-
    Next = integer.div(integer.'+'(Guess, integer.div(N, Guess)),
        integer.det_from_string("2")),
    ( if integer.'>='(Next, Guess) then
        Root = Guess
    else
        isqrt_newton(N, Next, Root)
    ).

%---------------------------------------------------------------------------%
% Pattern matching: a + b√r
%---------------------------------------------------------------------------%

:- pred match_sqrt_nested(rad_expr(rational)::in,
    rational::out, rational::out, rational::out) is semidet.

match_sqrt_nested(Expr, A, B, R) :-
    ( if Expr = re_add(re_lit(A0), re_mul(re_lit(B0), re_root(2, re_lit(R0)))) then
        A = A0, B = B0, R = R0
    else if Expr = re_add(re_mul(re_lit(B0), re_root(2, re_lit(R0))), re_lit(A0)) then
        A = A0, B = B0, R = R0
    else if Expr = re_add(re_lit(A0), re_root(2, re_lit(R0))) then
        A = A0, B = rational.one, R = R0
    else if Expr = re_add(re_root(2, re_lit(R0)), re_lit(A0)) then
        A = A0, B = rational.one, R = R0
    else if Expr = re_add(re_lit(A0), re_neg(re_mul(re_lit(B0), re_root(2, re_lit(R0))))) then
        A = A0, B = rational.'-'(rational.zero, B0), R = R0
    else if Expr = re_add(re_lit(A0), re_neg(re_root(2, re_lit(R0)))) then
        A = A0, B = rational.'-'(rational.zero, rational.one), R = R0
    else
        fail
    ).

%---------------------------------------------------------------------------%
% Expression-level denesting
%---------------------------------------------------------------------------%

denest_sqrt_expr(Expr, Result) :-
    Expr = re_root(2, Inner),
    match_sqrt_nested(Inner, A, B, R),
    try_sqrt_denest(A, B, R, {Sign, X, Y}),
    SX = re_root(2, re_lit(X)),
    SY = re_root(2, re_lit(Y)),
    ( if int.'>'(Sign, 0) then
        Result = re_add(SX, SY)
    else
        Result = re_add(SX, re_neg(SY))
    ).

%---------------------------------------------------------------------------%
% Recursive denesting
%---------------------------------------------------------------------------%

denest_sqrt(Expr) = Result :-
    ( if Expr = re_root(2, _) then
        ( if denest_sqrt_expr(Expr, Denested) then
            Result = denest_sqrt(Denested)
        else if Expr = re_root(2, A) then
            Result = re_root(2, denest_sqrt(A))
        else
            Result = Expr
        )
    else if Expr = re_neg(A) then
        Result = re_neg(denest_sqrt(A))
    else if Expr = re_add(A, B) then
        Result = re_add(denest_sqrt(A), denest_sqrt(B))
    else if Expr = re_mul(A, B) then
        Result = re_mul(denest_sqrt(A), denest_sqrt(B))
    else if Expr = re_inv(A) then
        Result = re_inv(denest_sqrt(A))
    else if Expr = re_root(N, A) then
        Result = re_root(N, denest_sqrt(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(denest_sqrt(A), N)
    else
        Result = Expr
    ).

%---------------------------------------------------------------------------%
:- end_module denest_sqrt.
%---------------------------------------------------------------------------%
