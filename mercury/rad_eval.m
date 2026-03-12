%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: rad_eval.m
% Main author: nyc
%
% Numerical evaluation of radical expressions.
%
% - eval_float: fast Double evaluation
% - eval_complex: Complex Double evaluation (handles complex intermediates)
% - eval_interval: rigorous interval enclosure
% - eval_complex_interval: rigorous complex interval enclosure
%
%---------------------------------------------------------------------------%

:- module rad_eval.
:- interface.

:- import_module interval.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%
% Complex number type (simple pair)
%---------------------------------------------------------------------------%

:- type complex
    --->    complex(
                re :: float,
                im :: float
            ).

%---------------------------------------------------------------------------%
% Evaluation functions
%---------------------------------------------------------------------------%

    % Evaluate to a Double.
    % Even roots of negative numbers produce NaN — use eval_complex instead.
    %
:- func eval_float(rad_expr(rational)) = float.

    % Evaluate to a Complex Double.
    % Handles expressions with complex intermediates.
    %
:- func eval_complex(rad_expr(rational)) = complex.

    % Evaluate to a rigorous interval enclosure.
    %
:- func eval_interval(rad_expr(rational)) = interval.

    % Evaluate to a rigorous complex interval enclosure.
    %
:- func eval_complex_interval(rad_expr(rational)) = complex_interval.

%---------------------------------------------------------------------------%
% Complex arithmetic helpers (exported for use by other modules)
%---------------------------------------------------------------------------%

:- func cadd(complex, complex) = complex.
:- func csub(complex, complex) = complex.
:- func cmul(complex, complex) = complex.
:- func cinv(complex) = complex.
:- func cneg(complex) = complex.
:- func complex_nth_root(int, complex) = complex.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- use_module float.
:- use_module math.

%---------------------------------------------------------------------------%
% Float evaluation
%---------------------------------------------------------------------------%

eval_float(re_lit(R)) = rational_to_float(R).
eval_float(re_neg(A)) = float.'-'(0.0, eval_float(A)).
eval_float(re_add(A, B)) = float.'+'(eval_float(A), eval_float(B)).
eval_float(re_mul(A, B)) = float.'*'(eval_float(A), eval_float(B)).
eval_float(re_inv(A)) = float.'/'(1.0, eval_float(A)).
eval_float(re_root(N, A)) = Result :-
    V = eval_float(A),
    ( if float.'<'(V, 0.0) then
        Result = float.'-'(0.0, math.exp(float.'*'(
            float.'/'(1.0, float.float(N)),
            math.ln(float.'-'(0.0, V)))))
    else if float.'<'(V, 1.0e-300) then
        Result = 0.0
    else
        Result = math.exp(float.'*'(
            float.'/'(1.0, float.float(N)),
            math.ln(V)))
    ).
eval_float(re_pow(A, N)) = Result :-
    V = eval_float(A),
    Result = float_pow(V, N).

%---------------------------------------------------------------------------%
% Complex evaluation
%---------------------------------------------------------------------------%

eval_complex(re_lit(R)) = complex(rational_to_float(R), 0.0).
eval_complex(re_neg(A)) = cneg(eval_complex(A)).
eval_complex(re_add(A, B)) = cadd(eval_complex(A), eval_complex(B)).
eval_complex(re_mul(A, B)) = cmul(eval_complex(A), eval_complex(B)).
eval_complex(re_inv(A)) = cinv(eval_complex(A)).
eval_complex(re_root(N, A)) = complex_nth_root(N, eval_complex(A)).
eval_complex(re_pow(A, N)) = complex_pow(eval_complex(A), N).

%---------------------------------------------------------------------------%
% Interval evaluation
%---------------------------------------------------------------------------%

eval_interval(re_lit(R)) = from_rational(R).
eval_interval(re_neg(A)) = Result :-
    interval(Lo, Hi) = eval_interval(A),
    Result = interval(rational.'-'(rational.zero, Hi),
                      rational.'-'(rational.zero, Lo)).
eval_interval(re_add(A, B)) = iadd(eval_interval(A), eval_interval(B)).
eval_interval(re_mul(A, B)) = imul(eval_interval(A), eval_interval(B)).
eval_interval(re_inv(A)) = iinv(eval_interval(A)).
eval_interval(re_root(N, A)) = inth(N, eval_interval(A)).
eval_interval(re_pow(A, N)) = ipow(eval_interval(A), N).

%---------------------------------------------------------------------------%
% Complex interval evaluation
%---------------------------------------------------------------------------%

eval_complex_interval(re_lit(R)) = ci_from_rational(R).
eval_complex_interval(re_neg(A)) = cineg(eval_complex_interval(A)).
eval_complex_interval(re_add(A, B)) =
    ciadd(eval_complex_interval(A), eval_complex_interval(B)).
eval_complex_interval(re_mul(A, B)) =
    cimul(eval_complex_interval(A), eval_complex_interval(B)).
eval_complex_interval(re_inv(A)) = ciinv(eval_complex_interval(A)).
eval_complex_interval(re_pow(A, N)) =
    cipow(eval_complex_interval(A), N).
eval_complex_interval(re_root(N, A)) = Result :-
    CI = eval_complex_interval(A),
    RePart = ci_real(CI),
    ImPart = ci_imag(CI),
    LoIm = iv_lo(ImPart),
    HiIm = iv_hi(ImPart),
    LoRe = iv_lo(RePart),
    HiRe = iv_hi(RePart),
    Zero = rational.zero,
    ( if
        rational.'>='(LoIm, Zero),
        rational.'=<'(HiIm, Zero),
        rational.'>='(LoRe, Zero)
    then
        % Non-negative real
        Result = ci_from_real(inth(N, RePart))
    else if
        rational.'>='(LoIm, Zero),
        rational.'=<'(HiIm, Zero),
        rational.'=<'(HiRe, Zero),
        int.odd(N)
    then
        % Odd root of negative real
        Pos = inth(N, interval(rational.'-'(Zero, HiRe),
                                   rational.'-'(Zero, LoRe))),
        Result = complex_interval(
            interval(rational.'-'(Zero, iv_hi(Pos)),
                        rational.'-'(Zero, iv_lo(Pos))),
            from_rational(Zero))
    else if
        rational.'>='(LoIm, Zero),
        rational.'=<'(HiIm, Zero),
        rational.'=<'(HiRe, Zero),
        N = 2
    then
        % sqrt(negative) = i * sqrt(|x|)
        Pos = isqrt(interval(rational.'-'(Zero, HiRe),
                                 rational.'-'(Zero, LoRe))),
        Result = complex_interval(from_rational(Zero), Pos)
    else
        % General complex root
        Result = cinthroot(N, CI)
    ).

%---------------------------------------------------------------------------%
% Complex arithmetic helpers
%---------------------------------------------------------------------------%

cadd(complex(R1, I1), complex(R2, I2)) =
    complex(float.'+'(R1, R2), float.'+'(I1, I2)).

csub(complex(R1, I1), complex(R2, I2)) =
    complex(float.'-'(R1, R2), float.'-'(I1, I2)).

cmul(complex(R1, I1), complex(R2, I2)) =
    complex(float.'-'(float.'*'(R1, R2), float.'*'(I1, I2)),
            float.'+'(float.'*'(R1, I2), float.'*'(I1, R2))).

cneg(complex(R, I)) = complex(float.'-'(0.0, R), float.'-'(0.0, I)).

cinv(complex(R, I)) = complex(float.'/'(R, D), float.'/'(float.'-'(0.0, I), D)) :-
    D = float.'+'(float.'*'(R, R), float.'*'(I, I)).

complex_nth_root(N, complex(R, I)) = Result :-
    Mag = math.sqrt(float.'+'(float.'*'(R, R), float.'*'(I, I))),
    ( if float.'<'(Mag, 1.0e-300) then
        Result = complex(0.0, 0.0)
    else
        Theta = math.atan2(I, R),
        FN = float.float(N),
        RN = math.exp(float.'*'(float.'/'(1.0, FN), math.ln(Mag))),
        AN = float.'/'(Theta, FN),
        Result = complex(float.'*'(RN, math.cos(AN)),
                         float.'*'(RN, math.sin(AN)))
    ).

:- func complex_pow(complex, int) = complex.

complex_pow(Z, N) = Result :-
    ( if N = 0 then
        Result = complex(1.0, 0.0)
    else if int.'<'(N, 0) then
        Result = cinv(complex_pow(Z, int.'-'(0, N)))
    else if int.even(N) then
        Half = complex_pow(Z, int.'//'(N, 2)),
        Result = cmul(Half, Half)
    else
        Result = cmul(Z, complex_pow(Z, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Utility
%---------------------------------------------------------------------------%

:- import_module integer.
:- import_module string.

:- func rational_to_float(rational) = float.

rational_to_float(R) = Result :-
    N = numer(R),
    D = denom(R),
    NF = integer_to_float(N),
    DF = integer_to_float(D),
    Result = float.'/'(NF, DF).

:- func integer_to_float(integer) = float.

integer_to_float(I) = F :-
    S = integer.to_string(I),
    ( if string.to_float(S, F0) then
        F = F0
    else
        % Fallback: should not happen for valid integers
        F = 0.0
    ).

:- func float_pow(float, int) = float.

float_pow(X, N) = Result :-
    ( if N = 0 then
        Result = 1.0
    else if int.'<'(N, 0) then
        Result = float.'/'(1.0, float_pow(X, int.'-'(0, N)))
    else if int.even(N) then
        Half = float_pow(X, int.'//'(N, 2)),
        Result = float.'*'(Half, Half)
    else
        Result = float.'*'(X, float_pow(X, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
:- end_module rad_eval.
%---------------------------------------------------------------------------%
