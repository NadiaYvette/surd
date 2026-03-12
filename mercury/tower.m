%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: tower.m
% Main author: nyc
%
% Tower of algebraic extensions.
%
% A field tower Q ⊂ Q(α₁) ⊂ Q(α₁)(α₂) ⊂ ... is built by iterated
% simple extensions.  This module provides utilities for constructing
% towers from radical expressions: each radical introduces a new
% extension layer.
%
%---------------------------------------------------------------------------%

:- module tower.
:- interface.

:- import_module extension.
:- import_module poly.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Adjoin an nth root to a field: given radicand r and degree n,
    % construct the extension K(α) where α = ⁿ√r.
    % Returns (field, generator).
    %
:- func adjoin_root(int, rational) = {ext_field(rational), ext_elem(rational)}.

    % Convenience: adjoin a square root.
    %
:- func adjoin_sqrt(rational) = {ext_field(rational), ext_elem(rational)}.

    % Evaluate a radical expression in a field, given:
    %   - a function to embed rationals
    %   - a function to resolve Root(n, evaluated_radicand)
    %
:- func eval_in_field(
    (func(rational) = ext_elem(rational)),
    (func(int, ext_elem(rational)) = ext_elem(rational)),
    rad_expr(rational)) = ext_elem(rational).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- use_module int.
:- use_module string.

%---------------------------------------------------------------------------%

adjoin_root(N, R) = {Field, Gen} :-
    % Minimal polynomial: x^n - r
    NegR = rational.'-'(rational.zero, R),
    Zeros = list.duplicate(int.'-'(N, 1), rational.zero),
    MinPoly = mk_poly([NegR | Zeros] ++ [rational.one]),
    Field = mk_ext_field(MinPoly,
        string.append("α", string.int_to_string(N))),
    Gen = generator(Field).

adjoin_sqrt(R) = adjoin_root(2, R).

eval_in_field(EmbedR, ResolveRoot, Expr) = Result :-
    ( if Expr = re_lit(R) then
        Result = EmbedR(R)
    else if Expr = re_neg(A) then
        Result = ext_neg(eval_in_field(EmbedR, ResolveRoot, A))
    else if Expr = re_add(A, B) then
        Result = ext_add(
            eval_in_field(EmbedR, ResolveRoot, A),
            eval_in_field(EmbedR, ResolveRoot, B))
    else if Expr = re_mul(A, B) then
        Result = ext_mul(
            eval_in_field(EmbedR, ResolveRoot, A),
            eval_in_field(EmbedR, ResolveRoot, B))
    else if Expr = re_inv(A) then
        Result = ext_inv(eval_in_field(EmbedR, ResolveRoot, A))
    else if Expr = re_root(N, A) then
        Result = ResolveRoot(N,
            eval_in_field(EmbedR, ResolveRoot, A))
    else if Expr = re_pow(A, N) then
        V = eval_in_field(EmbedR, ResolveRoot, A),
        ( if int.'>='(N, 0) then
            Result = ext_pow(V, N)
        else
            Result = ext_inv(ext_pow(V, int.'-'(0, N)))
        )
    else
        Result = EmbedR(rational.zero)
    ).

%---------------------------------------------------------------------------%
:- end_module tower.
%---------------------------------------------------------------------------%
