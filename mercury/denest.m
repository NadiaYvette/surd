%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: denest.m
% Main author: nyc
%
% Top-level radical denesting dispatcher.
%
% Applies the appropriate denesting algorithm based on the root index
% and expression structure:
% - Square root denesting (Borodin-FHKT)
% - General nth-root simplification and cube root denesting
% - (Landau denesting: planned, not yet implemented)
%
%---------------------------------------------------------------------------%

:- module denest.
:- interface.

:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Denest a radical expression: normalize first, then apply all
    % available denesting algorithms recursively.
    %
:- func denest(rad_expr(rational)) = rad_expr(rational).

    % Full denesting pass without pre-normalization.
    %
:- func denest_full(rad_expr(rational)) = rad_expr(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module denest_sqrt.
:- import_module denest_nthroot.
:- import_module rad_normalize.
:- use_module string.

%---------------------------------------------------------------------------%

denest(Expr) = denest_full(normalize(Expr)).

denest_full(Expr) = Result :-
    ( if Expr = re_root(2, Inner) then
        Specialized = denest_sqrt.denest_sqrt(Expr),
        ( if changed(Specialized, Expr) then
            Result = Specialized
        else
            Result = re_root(2, denest_full(Inner))
        )
    else if Expr = re_root(N, Inner) then
        Specialized = denest_nthroot.denest_nthroot(Expr),
        ( if changed(Specialized, Expr) then
            Result = Specialized
        else
            Result = re_root(N, denest_full(Inner))
        )
    else if Expr = re_neg(A) then
        Result = re_neg(denest_full(A))
    else if Expr = re_add(A, B) then
        Result = re_add(denest_full(A), denest_full(B))
    else if Expr = re_mul(A, B) then
        Result = re_mul(denest_full(A), denest_full(B))
    else if Expr = re_inv(A) then
        Result = re_inv(denest_full(A))
    else if Expr = re_pow(A, N) then
        Result = re_pow(denest_full(A), N)
    else
        Result = Expr
    ).

:- pred changed(rad_expr(rational)::in, rad_expr(rational)::in) is semidet.

changed(New, Old) :-
    New \= Old.

%---------------------------------------------------------------------------%
:- end_module denest.
%---------------------------------------------------------------------------%
