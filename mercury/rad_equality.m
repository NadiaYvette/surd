%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: rad_equality.m
% Main author: nyc
%
% Equality testing for radical expressions.
%
% Two radical expressions are equal iff they represent the same
% algebraic number: same minimal polynomial and same root
% (verified by isolating interval overlap).
%
%---------------------------------------------------------------------------%

:- module rad_equality.
:- interface.

:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Test equality of two radical expressions over Q.
    %
    % Converts both to algebraic numbers (minimal polynomial + isolating
    % interval) and checks equality using Sturm-based interval refinement.
    % This is rigorous: no floating-point tolerance is involved in the
    % final decision.
    %
:- pred radical_eq(rad_expr(rational)::in, rad_expr(rational)::in)
    is semidet.

    % Inequality test.
    %
:- pred radical_neq(rad_expr(rational)::in, rad_expr(rational)::in)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module alg_num.
:- import_module rad_alg_convert.

%---------------------------------------------------------------------------%

radical_eq(A, B) :-
    alg_eq(rad_expr_to_alg_num(A), rad_expr_to_alg_num(B)).

radical_neq(A, B) :-
    not radical_eq(A, B).

%---------------------------------------------------------------------------%
:- end_module rad_equality.
%---------------------------------------------------------------------------%
