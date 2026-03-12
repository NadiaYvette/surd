%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: rad_order.m
% Main author: nyc
%
% Ordering of real radical expressions.
%
% Uses algebraic number comparison with Sturm-based interval
% refinement for rigorous results.
%
%---------------------------------------------------------------------------%

:- module rad_order.
:- interface.

:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

    % Compare two radical expressions.
    %
    % Converts both to algebraic numbers and compares using
    % Sturm-based interval refinement.  This is rigorous and
    % correct for all real radical expressions.
    %
:- func radical_compare(rad_expr(rational), rad_expr(rational))
    = comparison_result.

    % Convenience predicates.
    %
:- pred radical_lt(rad_expr(rational)::in, rad_expr(rational)::in)
    is semidet.

:- pred radical_gt(rad_expr(rational)::in, rad_expr(rational)::in)
    is semidet.

:- pred radical_leq(rad_expr(rational)::in, rad_expr(rational)::in)
    is semidet.

:- pred radical_geq(rad_expr(rational)::in, rad_expr(rational)::in)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module alg_num.
:- import_module rad_alg_convert.

%---------------------------------------------------------------------------%

radical_compare(A, B) =
    alg_compare(rad_expr_to_alg_num(A), rad_expr_to_alg_num(B)).

radical_lt(A, B) :- radical_compare(A, B) = (<).

radical_gt(A, B) :- radical_compare(A, B) = (>).

radical_leq(A, B) :- not radical_gt(A, B).

radical_geq(A, B) :- not radical_lt(A, B).

%---------------------------------------------------------------------------%
:- end_module rad_order.
%---------------------------------------------------------------------------%
