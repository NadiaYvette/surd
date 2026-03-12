%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: euler_integrate.m
% Main author: nyc
%
% Euler substitution for integrals of the form
% ∫ P(x)/Q(x) · (√(ax²+bx+c))^n dx.
%
% Three substitutions reduce these to rational function integrals:
%   Euler 1 (a > 0): √(ax²+bx+c) = t − x√a
%   Euler 2 (c > 0): √(ax²+bx+c) = xt + √c
%   Euler 3 (Δ > 0): √(a(x−r₁)(x−r₂)) = t(x−r₁)
%
%---------------------------------------------------------------------------%

:- module euler_integrate.
:- interface.

:- import_module maybe.
:- import_module poly.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%
% Types
%---------------------------------------------------------------------------%

    % Symbolic expression for an antiderivative (function of x).
    %
:- type sym_expr
    --->    s_rat(rational)
    ;       s_rad(rad_expr(rational))
    ;       s_var
    ;       s_surd(rational, rational, rational)    % √(ax²+bx+c)
    ;       s_neg(sym_expr)
    ;       s_add(sym_expr, sym_expr)
    ;       s_mul(sym_expr, sym_expr)
    ;       s_div(sym_expr, sym_expr)
    ;       s_pow(sym_expr, int)
    ;       s_ln(sym_expr)
    ;       s_arctan(sym_expr)
    ;       s_arcsin(sym_expr)
    ;       s_arsinh(sym_expr)
    ;       s_arcosh(sym_expr).

    % Result of Euler substitution integration.
    %
:- type integral_result
    --->    integral_result(
                ir_expr :: sym_expr,
                ir_a    :: rational,
                ir_b    :: rational,
                ir_c    :: rational
            ).

    % An integrand P(x)/Q(x) · (√(ax²+bx+c))^n.
    %
:- type euler_integrand
    --->    euler_integrand(
                ei_p         :: poly(rational),
                ei_q         :: poly(rational),
                ei_sqrt_pow  :: int,
                ei_a         :: rational,
                ei_b         :: rational,
                ei_c         :: rational
            ).

    % Integrate P(x)/Q(x) · (√(ax²+bx+c))^n dx via Euler substitution.
    %
:- func euler_integrate(euler_integrand) = maybe(integral_result).

    % Integrate a rational function N(t)/D(t) dt (standalone).
    %
:- func integrate_rational(poly(rational), poly(rational)) = sym_expr.

    % Pretty-print a symbolic expression.
    %
:- func pretty_sym_expr(sym_expr) = string.

    % LaTeX render a symbolic expression.
    %
:- func latex_sym_expr(sym_expr) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module int.
:- import_module extension.
:- import_module factoring.
:- import_module integer.
:- import_module list.
:- import_module pretty.
:- import_module latex.
:- import_module rad_normalize.
:- import_module string.

%---------------------------------------------------------------------------%
% Euler substitution type
%---------------------------------------------------------------------------%

:- type euler_sub
    --->    euler1(rational)
    ;       euler2(rational)
    ;       euler3(rational, rational).

:- type sub_result
    --->    sub_result(
                sr_num  :: poly(rational),
                sr_den  :: poly(rational),
                sr_back :: sym_expr
            ).

%---------------------------------------------------------------------------%
% Smart constructors
%---------------------------------------------------------------------------%

:- func s_add_smart(sym_expr, sym_expr) = sym_expr.

s_add_smart(A, B) = Result :-
    ( if A = s_rat(RA), RA = rational.zero then
        Result = B
    else if B = s_rat(RB), RB = rational.zero then
        Result = A
    else if A = s_rat(RA), B = s_rat(RB) then
        Result = s_rat(rational.'+'(RA, RB))
    else
        Result = s_add(A, B)
    ).

:- func s_mul_smart(sym_expr, sym_expr) = sym_expr.

s_mul_smart(A, B) = Result :-
    ( if A = s_rat(RA), RA = rational.zero then
        Result = s_rat(rational.zero)
    else if B = s_rat(RB), RB = rational.zero then
        Result = s_rat(rational.zero)
    else if A = s_rat(RA), RA = rational.one then
        Result = B
    else if B = s_rat(RB), RB = rational.one then
        Result = A
    else if
        A = s_rat(RA),
        RA = rational.'-'(rational.zero, rational.one)
    then
        Result = s_neg_smart(B)
    else if A = s_rat(RA), B = s_rat(RB) then
        Result = s_rat(rational.'*'(RA, RB))
    else if A = s_rat(RA), B = s_rad(E) then
        Result = s_rad(normalize(re_mul(re_lit(RA), E)))
    else if A = s_rad(E), B = s_rat(RB) then
        Result = s_rad(normalize(re_mul(re_lit(RB), E)))
    else
        Result = s_mul(A, B)
    ).

:- func s_div_smart(sym_expr, sym_expr) = sym_expr.

s_div_smart(A, B) = Result :-
    ( if A = s_rat(RA), RA = rational.zero then
        Result = s_rat(rational.zero)
    else if B = s_rat(RB), RB = rational.one then
        Result = A
    else if A = s_rat(RA), B = s_rat(RB) then
        Result = s_rat(rational.'/'(RA, RB))
    else if A = s_rat(RA), B = s_rad(E) then
        C = rad_coeff(E),
        EP = rad_coeff_rest(E),
        Result = s_rad(normalize(re_mul(re_lit(rational.'/'(RA, C)),
                                        re_inv(EP))))
    else if A = s_mul(s_rat(RA), X), B = s_rat(RB) then
        Result = s_mul_smart(s_rat(rational.'/'(RA, RB)), X)
    else
        Result = s_div(A, B)
    ).

:- func rad_coeff(rad_expr(rational)) = rational.

rad_coeff(E) = R :-
    ( if E = re_mul(re_lit(C), _) then R = C
    else if E = re_mul(_, re_lit(C)) then R = C
    else R = rational.one
    ).

:- func rad_coeff_rest(rad_expr(rational)) = rad_expr(rational).

rad_coeff_rest(E) = R :-
    ( if E = re_mul(re_lit(_), Rest) then R = Rest
    else if E = re_mul(Rest, re_lit(_)) then R = Rest
    else R = E
    ).

:- func s_neg_smart(sym_expr) = sym_expr.

s_neg_smart(E) = Result :-
    ( if E = s_rat(R) then
        Result = s_rat(rational.'-'(rational.zero, R))
    else if E = s_neg(X) then
        Result = X
    else
        Result = s_neg(E)
    ).

:- func s_sum(list(sym_expr)) = sym_expr.

s_sum([]) = s_rat(rational.zero).
s_sum([H | T]) = list.foldl(func(X, Acc) = s_add_smart(Acc, X), T, H).

%---------------------------------------------------------------------------%
% Choosing the substitution
%---------------------------------------------------------------------------%

:- func choose_euler(rational, rational, rational) = maybe(euler_sub).

choose_euler(A, B, C) = Result :-
    ( if rational.'>'(A, rational.zero), rat_sqrt(A) = yes(S) then
        Result = yes(euler1(S))
    else if rational.'>'(C, rational.zero), rat_sqrt(C) = yes(S) then
        Result = yes(euler2(S))
    else
        Disc = rational.'-'(rational.'*'(B, B),
                            rational.'*'(rational(4),
                                         rational.'*'(A, C))),
        ( if rational.'>'(Disc, rational.zero), rat_sqrt(Disc) = yes(SD) then
            R1 = rational.'/'(rational.'+'(rational.'-'(rational.zero, B), SD),
                              rational.'*'(rational(2), A)),
            R2 = rational.'/'(rational.'-'(rational.'-'(rational.zero, B), SD),
                              rational.'*'(rational(2), A)),
            Result = yes(euler3(R1, R2))
        else
            Result = no
        )
    ).

:- func rat_sqrt(rational) = maybe(rational).

rat_sqrt(R) = Result :-
    ( if rational.'<'(R, rational.zero) then
        Result = no
    else if R = rational.zero then
        Result = yes(rational.zero)
    else
        N = numer(R),
        D = denom(R),
        ( if int_sqrt(N) = yes(SN), int_sqrt(D) = yes(SD) then
            Result = yes(from_integers(SN, SD))
        else
            Result = no
        )
    ).

:- func int_sqrt(integer) = maybe(integer).

int_sqrt(X) = Result :-
    ( if integer.'<'(X, integer.zero) then
        Result = no
    else if X = integer.zero then
        Result = yes(integer.zero)
    else
        XStr = integer.to_string(X),
        ( if string.to_float(XStr, XF0) then
            XF = XF0
        else
            XF = 0.0
        ),
        S = integer.det_from_string(string.from_int(
            float.truncate_to_int(math.sqrt(XF)))),
        ( if integer.'*'(S, S) = X then
            Result = yes(S)
        else
            S1 = integer.'+'(S, integer.one),
            ( if integer.'*'(S1, S1) = X then
                Result = yes(S1)
            else
                Result = no
            )
        )
    ).

:- use_module float.
:- use_module math.

%---------------------------------------------------------------------------%
% Apply Euler substitution
%---------------------------------------------------------------------------%

:- func apply_euler(euler_sub, poly(rational), poly(rational), int,
    rational, rational, rational) = sub_result.

% Euler 1: y = t - sx, so t = y + sx.
% x = (t² - c) / (b + 2st)
% y = (sc + bt + st²) / (b + 2st)
% dx/dt = 2·y / (b + 2st)²
apply_euler(euler1(S), PPoly, QPoly, N, _A, B, C) = Result :-
    Qx = mk_poly([B, rational.'*'(rational(2), S)]),
    Px = mk_poly([rational.'-'(rational.zero, C), rational.zero, rational.one]),
    Py = mk_poly([rational.'*'(S, C), B, S]),

    DP = poly.degree(PPoly),
    DQ = poly.degree(QPoly),
    PNum = subst_rat_func(PPoly, Px, Qx),
    QNum = subst_rat_func(QPoly, Px, Qx),

    NumParts = poly.mul(poly.scale(rational(2), PNum),
                        poly_pow(Py, int.max(0, int.'+'(N, 1)))),
    DenParts = poly.mul(poly.mul(QNum,
                        poly_pow(Qx, int.'+'(int.'-'(DP, DQ),
                                             int.'+'(N, 2)))),
                        poly_pow(Py, int.max(0, int.'-'(0, int.'+'(N, 1))))),

    Back = s_add_smart(s_surd(rational.'*'(S, S), B, C),
                       s_mul_smart(s_rat(S), s_var)),
    Result = sub_result(NumParts, DenParts, Back).

% Euler 2: y = xt + √c, so t = (y - √c) / x.
apply_euler(euler2(SC), PPoly, QPoly, N, A, B, _C) = Result :-
    Px = mk_poly([rational.'-'(rational.zero, B),
                  rational.'*'(rational(2), SC)]),
    Qx = mk_poly([A, rational.zero, rational.'-'(rational.zero, rational.one)]),
    Py = mk_poly([rational.'*'(SC, A),
                  rational.'-'(rational.zero, B), SC]),

    DP = poly.degree(PPoly),
    DQ = poly.degree(QPoly),
    PNum = subst_rat_func(PPoly, Px, Qx),
    QNum = subst_rat_func(QPoly, Px, Qx),

    NumParts = poly.mul(poly.scale(rational(2), PNum),
                        poly_pow(Py, int.max(0, int.'+'(N, 1)))),
    DenParts = poly.mul(poly.mul(QNum,
                        poly_pow(Qx, int.'+'(int.'-'(DP, DQ),
                                             int.'+'(N, 2)))),
                        poly_pow(Py, int.max(0, int.'-'(0, int.'+'(N, 1))))),

    Back = s_div_smart(
        s_add_smart(s_surd(A, B, rational.'*'(SC, SC)),
                    s_neg_smart(s_rat(SC))),
        s_var),
    Result = sub_result(NumParts, DenParts, Back).

% Euler 3: y = t(x - r₁), roots r₁, r₂.
apply_euler(euler3(R1, R2), PPoly, QPoly, N, A, B, C) = Result :-
    Px = mk_poly([rational.'*'(A, R2), rational.zero,
                  rational.'-'(rational.zero, R1)]),
    Qx = mk_poly([A, rational.zero, rational.'-'(rational.zero, rational.one)]),
    Py = mk_poly([rational.zero,
                  rational.'*'(A, rational.'-'(R2, R1))]),

    Px1 = poly.diff(Px),
    Qx1 = poly.diff(Qx),
    DxNum = poly.sub(poly.mul(Px1, Qx), poly.mul(Px, Qx1)),

    DP = poly.degree(PPoly),
    DQ = poly.degree(QPoly),
    PNum = subst_rat_func(PPoly, Px, Qx),
    QNum = subst_rat_func(QPoly, Px, Qx),

    NumParts = poly.mul(poly.mul(PNum, DxNum),
                        poly_pow(Py, int.max(0, N))),
    DenParts = poly.mul(poly.mul(QNum,
                        poly_pow(Qx, int.'+'(int.'-'(DP, DQ), 2))),
                        poly_pow(Py, int.max(0, int.'-'(0, N)))),

    Back = s_div_smart(s_surd(A, B, C),
                       s_add_smart(s_var,
                                   s_neg_smart(s_rat(R1)))),
    Result = sub_result(NumParts, DenParts, Back).

%---------------------------------------------------------------------------%
% Polynomial substitution
%---------------------------------------------------------------------------%

    % Substitute x = p/q into polynomial f.
    % Returns numerator of f(p/q) · q^(deg f).
    %
:- func subst_rat_func(poly(rational), poly(rational), poly(rational)) =
    poly(rational).

subst_rat_func(F, P, Q) = Result :-
    Cs = coeffs(F),
    D = poly.degree(F),
    ( if Cs = [] then
        Result = zero_poly
    else
        Terms = subst_terms(Cs, P, Q, D, 0),
        Result = list.foldl(poly.add, Terms, zero_poly)
    ).

:- func subst_terms(list(rational), poly(rational), poly(rational),
    int, int) = list(poly(rational)).

subst_terms([], _, _, _, _) = [].
subst_terms([C | Rest], P, Q, D, I) =
    [poly.scale(C, poly.mul(poly_pow(P, I), poly_pow(Q, int.'-'(D, I)))) |
     subst_terms(Rest, P, Q, D, int.'+'(I, 1))].

    % Integer power of a polynomial.
    %
:- func poly_pow(poly(rational), int) = poly(rational).

poly_pow(P, N) = Result :-
    ( if int.'=<'(N, 0) then
        Result = const_poly(rational.one)
    else if N = 1 then
        Result = P
    else
        ( if int.even(N) then
            H = poly_pow(P, int.'//'(N, 2)),
            Result = poly.mul(H, H)
        else
            Result = poly.mul(P, poly_pow(P, int.'-'(N, 1)))
        )
    ).

%---------------------------------------------------------------------------%
% Rational function integration
%---------------------------------------------------------------------------%

integrate_rational(Num, Den) = Result :-
    ( if coeffs(Den) = [] then
        Result = s_rat(rational.zero)
    else
        ( if lead_coeff(Den, LC) then
            Num1 = poly.scale(rational.'/'(rational.one, LC), Num),
            Den1 = poly.monic(Den)
        else
            Num1 = Num,
            Den1 = Den
        ),
        poly.div_mod(Num1, Den1, QP, RP),
        PolyPart = integrate_poly(QP),
        Facs = factor(Den1),
        PFTerms = partial_fractions(RP, Facs),
        PFParts = list.map(integrate_pf_term, PFTerms),
        Result = s_sum([PolyPart | PFParts])
    ).

:- func integrate_poly(poly(rational)) = sym_expr.

integrate_poly(P) = Result :-
    Cs = coeffs(P),
    ( if Cs = [] then
        Result = s_rat(rational.zero)
    else
        Terms = integrate_poly_terms(Cs, 0),
        Result = s_sum(Terms)
    ).

:- func integrate_poly_terms(list(rational), int) = list(sym_expr).

integrate_poly_terms([], _) = [].
integrate_poly_terms([C | Rest], I) = Result :-
    ( if C = rational.zero then
        Result = integrate_poly_terms(Rest, int.'+'(I, 1))
    else
        Coeff = rational.'/'(C, rational(int.'+'(I, 1))),
        Term = s_mul_smart(s_rat(Coeff),
                           s_pow(s_var, int.'+'(I, 1))),
        Result = [Term | integrate_poly_terms(Rest, int.'+'(I, 1))]
    ).

%---------------------------------------------------------------------------%
% Partial fraction decomposition
%---------------------------------------------------------------------------%

:- type pf_term
    --->    pf_term(
                pf_num    :: poly(rational),
                pf_factor :: poly(rational),
                pf_power  :: int
            ).

:- func partial_fractions(poly(rational), list(factor_pair)) = list(pf_term).

partial_fractions(_, []) = [].
partial_fractions(R, [factor_pair(F, E)]) = expand_power(R, F, E).
partial_fractions(R, [factor_pair(F, E) | Rest]) = Result :-
    Rest = [_ | _],
    Others = list.foldl(
        func(factor_pair(G, M), Acc) = poly.mul(Acc, poly_pow(G, M)),
        Rest, const_poly(rational.one)),
    FE = poly_pow(F, E),
    {_, U, V} = ext_gcd(FE, Others),
    poly.div_mod(poly.mul(R, V), FE, _, RV),
    poly.div_mod(poly.mul(R, U), Others, _, RU),
    Result = expand_power(RV, F, E) ++
             partial_fractions(RU, Rest).

:- func expand_power(poly(rational), poly(rational), int) = list(pf_term).

expand_power(R, F, E) = expand_power_loop(R, F, E).

:- func expand_power_loop(poly(rational), poly(rational), int) =
    list(pf_term).

expand_power_loop(Rem, F, K) = Result :-
    ( if int.'=<'(K, 0) then
        Result = []
    else
        poly.div_mod(Rem, F, Q, RK),
        Result = [pf_term(RK, F, K) |
                  expand_power_loop(Q, F, int.'-'(K, 1))]
    ).

%---------------------------------------------------------------------------%
% Integrate partial fraction terms
%---------------------------------------------------------------------------%

:- func integrate_pf_term(pf_term) = sym_expr.

integrate_pf_term(pf_term(Num, Fac, K)) = Result :-
    Cs = coeffs(Num),
    ( if Cs = [] then
        Result = s_rat(rational.zero)
    else if Cs = [C0], C0 = rational.zero then
        Result = s_rat(rational.zero)
    else if
        Cs = [A], A \= rational.zero,
        coeffs(Fac) = [NegAlpha, One0],
        One0 = rational.one
    then
        % Linear factor: f = t + α
        ( if K = 1 then
            Result = s_mul_smart(s_rat(A),
                                 s_ln(s_add_smart(s_var, s_rat(NegAlpha))))
        else
            K1 = rational(int.'-'(1, K)),
            Result = s_mul_smart(s_rat(rational.'/'(A, K1)),
                                 s_pow(s_add_smart(s_var, s_rat(NegAlpha)),
                                       int.'-'(1, K)))
        )
    else if
        Cs = [C1], C1 \= rational.zero,
        poly.degree(Fac) = 2,
        coeffs(Fac) = [_, _, One1],
        One1 = rational.one,
        K = 1
    then
        Result = integrate_quadratic(C1, rational.zero, Fac)
    else if
        Cs = [C2, B2],
        poly.degree(Fac) = 2,
        coeffs(Fac) = [_, _, One2],
        One2 = rational.one,
        K = 1
    then
        Result = integrate_quadratic(C2, B2, Fac)
    else
        Result = s_div(poly_to_sym(Num), s_pow(poly_to_sym(Fac), K))
    ).

%---------------------------------------------------------------------------%
% Quadratic integration
%---------------------------------------------------------------------------%

:- func integrate_quadratic(rational, rational, poly(rational)) = sym_expr.

integrate_quadratic(C_, B_, Fac) = Result :-
    ( if coeffs(Fac) = [Q_, P_, _] then
        Disc = rational.'-'(rational.'*'(rational(4), Q_),
                            rational.'*'(P_, P_)),
        QuadExpr = s_add_smart(
            s_add_smart(s_pow(s_var, 2),
                        s_mul_smart(s_rat(P_), s_var)),
            s_rat(Q_)),
        LogPart = s_mul_smart(s_rat(rational.'/'(B_, rational(2))),
                              s_ln(QuadExpr)),
        RemCoeff = rational.'-'(C_,
                                rational.'/'(rational.'*'(B_, P_),
                                             rational(2))),
        ( if rational.'>'(Disc, rational.zero) then
            % Complex roots: arctan formula
            SqrtDisc = ( if rat_sqrt(Disc) = yes(S) then
                            s_rat(S)
                         else
                            s_rad(normalize(re_root(2,
                                re_lit(Disc)))) ),
            ArctanArg = s_div_smart(
                s_add_smart(s_mul_smart(s_rat(rational(2)), s_var),
                            s_rat(P_)),
                SqrtDisc),
            ArctanPart = s_mul_smart(
                s_div_smart(s_rat(rational.'*'(RemCoeff, rational(2))),
                            SqrtDisc),
                s_arctan(ArctanArg)),
            Result = s_add_smart(LogPart, ArctanPart)
        else
            % Real roots: log formula
            NegDisc = rational.'-'(rational.zero, Disc),
            SqrtNegDisc = ( if rat_sqrt(NegDisc) = yes(S) then
                              s_rat(S)
                           else
                              s_rad(normalize(re_root(2,
                                  re_lit(NegDisc)))) ),
            TwoTPlusP = s_add_smart(s_mul_smart(s_rat(rational(2)), s_var),
                                    s_rat(P_)),
            LogArg = s_div_smart(
                s_add_smart(TwoTPlusP, s_neg_smart(SqrtNegDisc)),
                s_add_smart(TwoTPlusP, SqrtNegDisc)),
            LogRatPart = s_mul_smart(
                s_div_smart(s_rat(RemCoeff), SqrtNegDisc),
                s_ln(LogArg)),
            Result = s_add_smart(LogPart, LogRatPart)
        )
    else
        Result = s_rat(rational.zero)
    ).

%---------------------------------------------------------------------------%
% Polynomial to SymExpr
%---------------------------------------------------------------------------%

:- func poly_to_sym(poly(rational)) = sym_expr.

poly_to_sym(P) = Result :-
    Cs = coeffs(P),
    ( if Cs = [] then
        Result = s_rat(rational.zero)
    else
        Terms = poly_to_sym_terms(Cs, 0),
        ( if Terms = [] then
            Result = s_rat(rational.zero)
        else
            Result = s_sum(Terms)
        )
    ).

:- func poly_to_sym_terms(list(rational), int) = list(sym_expr).

poly_to_sym_terms([], _) = [].
poly_to_sym_terms([C | Rest], I) = Result :-
    ( if C = rational.zero then
        Result = poly_to_sym_terms(Rest, int.'+'(I, 1))
    else
        Term = s_mul_smart(s_rat(C), s_pow(s_var, I)),
        Result = [Term | poly_to_sym_terms(Rest, int.'+'(I, 1))]
    ).

%---------------------------------------------------------------------------%
% Main integration entry point
%---------------------------------------------------------------------------%

euler_integrate(EI) = Result :-
    MaybeSub = choose_euler(ei_a(EI), ei_b(EI), ei_c(EI)),
    ( if MaybeSub = yes(Sub) then
        SR = apply_euler(Sub, ei_p(EI), ei_q(EI), ei_sqrt_pow(EI),
                         ei_a(EI), ei_b(EI), ei_c(EI)),
        FT = integrate_rational(sr_num(SR), sr_den(SR)),
        BackSubbed = subst_back(FT, sr_back(SR)),
        Detected = detect_inverse_trig(BackSubbed),
        Result = yes(integral_result(Detected, ei_a(EI), ei_b(EI), ei_c(EI)))
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Back-substitution
%---------------------------------------------------------------------------%

:- func subst_back(sym_expr, sym_expr) = sym_expr.

subst_back(Expr, TExpr) = subst_back_inner(Expr, TExpr).

:- func subst_back_inner(sym_expr, sym_expr) = sym_expr.

subst_back_inner(E, TExpr) = R :-
    ( if E = s_var then
        R = TExpr
    else if E = s_rat(_) then
        R = E
    else if E = s_rad(_) then
        R = E
    else if E = s_surd(_, _, _) then
        R = E
    else if E = s_neg(A) then
        R = s_neg_smart(subst_back_inner(A, TExpr))
    else if E = s_add(A, B) then
        R = s_add_smart(subst_back_inner(A, TExpr),
                        subst_back_inner(B, TExpr))
    else if E = s_mul(A, B) then
        R = s_mul_smart(subst_back_inner(A, TExpr),
                        subst_back_inner(B, TExpr))
    else if E = s_div(A, B) then
        R = s_div_smart(subst_back_inner(A, TExpr),
                        subst_back_inner(B, TExpr))
    else if E = s_pow(A, N) then
        R = s_pow(subst_back_inner(A, TExpr), N)
    else if E = s_ln(A) then
        R = s_ln(subst_back_inner(A, TExpr))
    else if E = s_arctan(A) then
        R = s_arctan(subst_back_inner(A, TExpr))
    else if E = s_arcsin(A) then
        R = s_arcsin(subst_back_inner(A, TExpr))
    else if E = s_arsinh(A) then
        R = s_arsinh(subst_back_inner(A, TExpr))
    else if E = s_arcosh(A) then
        R = s_arcosh(subst_back_inner(A, TExpr))
    else
        R = E
    ).

%---------------------------------------------------------------------------%
% Inverse trig/hyperbolic detection
%---------------------------------------------------------------------------%

:- func detect_inverse_trig(sym_expr) = sym_expr.

detect_inverse_trig(E) = Result :-
    ( if E = s_add(A, B) then
        Result = s_add(detect_inverse_trig(A), detect_inverse_trig(B))
    else if E = s_mul(C, Inner) then
        Result = simplify_mul_trig(C, detect_inverse_trig(Inner))
    else if E = s_neg(A) then
        Result = s_neg(detect_inverse_trig(A))
    else if E = s_ln(Arg) then
        ( if match_arsinh(Arg) = yes(X) then
            Result = s_arsinh(X)
        else if match_arcosh(Arg) = yes(X) then
            Result = s_arcosh(X)
        else
            Result = s_ln(Arg)
        )
    else
        Result = E
    ).

:- func simplify_mul_trig(sym_expr, sym_expr) = sym_expr.

simplify_mul_trig(C, Inner) = Result :-
    ( if
        C = s_rat(R),
        R = rational.'-'(rational.zero, rational(2)),
        Inner = s_arctan(Arg),
        match_arcsin_from_arctan(Arg) = yes(X)
    then
        Result = s_arcsin(X)
    else if Inner = s_ln(Arg) then
        ( if match_arsinh(Arg) = yes(X) then
            Result = s_mul_smart(C, s_arsinh(X))
        else if match_arcosh(Arg) = yes(X) then
            Result = s_mul_smart(C, s_arcosh(X))
        else
            Result = s_mul(C, s_ln(Arg))
        )
    else
        Result = s_mul(C, Inner)
    ).

:- func match_arsinh(sym_expr) = maybe(sym_expr).

match_arsinh(Expr) = Result :-
    ( if find_s_surd(Expr) = yes({A, B, C}) then
        Disc = rational.'-'(rational.'*'(rational(4),
                                         rational.'*'(A, C)),
                            rational.'*'(B, B)),
        ( if rational.'>'(A, rational.zero), rational.'>'(Disc, rational.zero) then
            Result = yes(inv_hyp_arg(rational.'*'(rational(2), A), B, Disc))
        else
            Result = no
        )
    else
        Result = no
    ).

:- func match_arcosh(sym_expr) = maybe(sym_expr).

match_arcosh(Expr) = Result :-
    ( if find_s_surd(Expr) = yes({A, B, C}) then
        Disc = rational.'-'(rational.'*'(rational(4),
                                         rational.'*'(A, C)),
                            rational.'*'(B, B)),
        ( if rational.'>'(A, rational.zero), rational.'<'(Disc, rational.zero) then
            Result = yes(inv_hyp_arg(rational.'*'(rational(2), A), B,
                                     rational.'-'(rational.zero, Disc)))
        else
            Result = no
        )
    else
        Result = no
    ).

:- func inv_hyp_arg(rational, rational, rational) = sym_expr.

inv_hyp_arg(Coeff, Cnst, D) = Result :-
    ( if rat_sqrt(D) = yes(SD) then
        Result = s_add_smart(
            s_mul_smart(s_rat(rational.'/'(Coeff, SD)), s_var),
            s_rat(rational.'/'(Cnst, SD)))
    else
        Result = s_div_smart(
            s_add_smart(s_mul_smart(s_rat(Coeff), s_var),
                        s_rat(Cnst)),
            s_rad(normalize(re_root(2, re_lit(D)))))
    ).

:- func find_s_surd(sym_expr) = maybe({rational, rational, rational}).

find_s_surd(E) = Result :-
    ( if E = s_surd(A, B, C) then
        Result = yes({A, B, C})
    else if E = s_add(X, Y) then
        ( if find_s_surd(X) = yes(R) then
            Result = yes(R)
        else
            Result = find_s_surd(Y)
        )
    else
        Result = no
    ).

:- func match_arcsin_from_arctan(sym_expr) = maybe(sym_expr).

match_arcsin_from_arctan(E) = Result :-
    ( if E = s_div(s_add(s_surd(A_, B_, C_), RestE), Denom) then
        {_, F} = extract_linear(RestE),
        {SD, _} = extract_linear(Denom),
        ( if
            rational.'<'(A_, rational.zero),
            B_ = rational.zero,
            rational.'<'(F, rational.zero),
            SD \= rational.zero,
            rational.'>'(C_, rational.zero)
        then
            NegA = rational.'-'(rational.zero, A_),
            ( if rat_sqrt(NegA) = yes(SA), rat_sqrt(C_) = yes(SC) then
                Result = yes(s_mul_smart(
                    s_rat(rational.'/'(SA, SC)), s_var))
            else
                Result = yes(s_div_smart(
                    s_mul_smart(
                        s_rad(normalize(re_root(2, re_lit(NegA)))),
                        s_var),
                    s_rad(normalize(re_root(2, re_lit(C_))))))
            )
        else
            Result = no
        )
    else
        Result = no
    ).

:- func extract_linear(sym_expr) = {rational, rational}.

extract_linear(E) = Result :-
    ( if E = s_var then
        Result = {rational.one, rational.zero}
    else if E = s_rat(R) then
        Result = {rational.zero, R}
    else if E = s_mul(s_rat(S), s_var) then
        Result = {S, rational.zero}
    else if E = s_add(A, B) then
        {S1, F1} = extract_linear(A),
        {S2, F2} = extract_linear(B),
        Result = {rational.'+'(S1, S2), rational.'+'(F1, F2)}
    else if E = s_neg(A) then
        {S, F} = extract_linear(A),
        Result = {rational.'-'(rational.zero, S),
                  rational.'-'(rational.zero, F)}
    else
        Result = {rational.zero, rational.zero}
    ).

%---------------------------------------------------------------------------%
% Pretty-print rendering
%---------------------------------------------------------------------------%

pretty_sym_expr(E) = pretty_prec(0, E).

:- func pretty_prec(int, sym_expr) = string.

pretty_prec(P, E) = Result :-
    ( if E = s_rat(R) then
        Result = pretty_rational(R)
    else if E = s_rad(RE) then
        Result = pretty.pretty(normalize(RE))
    else if E = s_var then
        Result = "x"
    else if E = s_surd(A, B, C) then
        Result = "√(" ++ pretty_quad(A, B, C) ++ ")"
    else if E = s_neg(A) then
        Result = "-" ++ pretty_prec(4, A)
    else if E = s_add(A, s_neg(B)) then
        Result = parens_if(gt_bool(P, 1),
            pretty_prec(1, A) ++ " - " ++ pretty_prec(2, B))
    else if E = s_add(A, s_rat(R2)) then
        ( if rational.'<'(R2, rational.zero) then
            Result = parens_if(gt_bool(P, 1),
                pretty_prec(1, A) ++ " - " ++
                pretty_prec(2, s_rat(rational.'-'(rational.zero, R2))))
        else
            Result = parens_if(gt_bool(P, 1),
                pretty_prec(1, A) ++ " + " ++ pretty_prec(1, s_rat(R2)))
        )
    else if E = s_add(A, s_mul(s_rat(R3), B)) then
        ( if rational.'<'(R3, rational.zero) then
            Result = parens_if(gt_bool(P, 1),
                pretty_prec(1, A) ++ " - " ++
                pretty_prec(2, s_mul_smart(s_rat(rational.'-'(rational.zero, R3)), B)))
        else
            Result = parens_if(gt_bool(P, 1),
                pretty_prec(1, A) ++ " + " ++ pretty_prec(1, s_mul(s_rat(R3), B)))
        )
    else if E = s_add(A, B) then
        Result = parens_if(gt_bool(P, 1),
            pretty_prec(1, A) ++ " + " ++ pretty_prec(1, B))
    else if E = s_mul(A, B) then
        Result = parens_if(gt_bool(P, 2),
            pretty_prec(2, A) ++ "·" ++ pretty_prec(3, B))
    else if E = s_div(A, B) then
        Result = parens_if(gt_bool(P, 2),
            pretty_prec(2, A) ++ "/" ++ pretty_prec(3, B))
    else if E = s_pow(A, N) then
        ( if N = 0 then Result = "1"
        else if N = 1 then Result = pretty_prec(5, A)
        else Result = pretty_prec(5, A) ++ "^" ++ string.from_int(N)
        )
    else if E = s_ln(A) then
        Result = "ln|" ++ pretty_prec(0, A) ++ "|"
    else if E = s_arctan(A) then
        Result = "arctan(" ++ pretty_prec(0, A) ++ ")"
    else if E = s_arcsin(A) then
        Result = "arcsin(" ++ pretty_prec(0, A) ++ ")"
    else if E = s_arsinh(A) then
        Result = "arsinh(" ++ pretty_prec(0, A) ++ ")"
    else if E = s_arcosh(A) then
        Result = "arcosh(" ++ pretty_prec(0, A) ++ ")"
    else
        Result = "?"
    ).

:- func gt_bool(int, int) = bool.
gt_bool(A, B) = ( if int.'>'(A, B) then yes else no ).

:- func parens_if(bool, string) = string.

:- import_module bool.

parens_if(yes, S) = "(" ++ S ++ ")".
parens_if(no, S) = S.

:- func pretty_rational(rational) = string.

pretty_rational(R) = Result :-
    N = numer(R),
    D = denom(R),
    ( if D = integer.one then
        Result = integer.to_string(N)
    else
        Result = integer.to_string(N) ++ "/" ++ integer.to_string(D)
    ).

:- func pretty_quad(rational, rational, rational) = string.

pretty_quad(A, B, C) = Result :-
    AStr = ( if A = rational.one then "x²"
             else if A = rational.'-'(rational.zero, rational.one) then "-x²"
             else pretty_rational(A) ++ "x²" ),
    BStr = ( if B = rational.zero then ""
             else if B = rational.one then "x"
             else if B = rational.'-'(rational.zero, rational.one) then "-x"
             else pretty_rational(B) ++ "x" ),
    CStr = ( if C = rational.zero then ""
             else pretty_rational(C) ),
    Terms = list.filter(
        (pred(S::in) is semidet :- S \= ""),
        [AStr, BStr, CStr]),
    ( if Terms = [] then Result = "0"
    else Result = string.join_list(" + ", Terms)
    ).

%---------------------------------------------------------------------------%
% LaTeX rendering
%---------------------------------------------------------------------------%

latex_sym_expr(E) = latex_prec(0, E).

:- func latex_prec(int, sym_expr) = string.

latex_prec(P, E) = Result :-
    ( if E = s_rat(R) then
        N = numer(R),
        D = denom(R),
        ( if D = integer.one then
            Result = integer.to_string(N)
        else
            Result = "\\frac{" ++ integer.to_string(N) ++ "}{" ++
                     integer.to_string(D) ++ "}"
        )
    else if E = s_rad(RE) then
        Result = latex.latex(normalize(RE))
    else if E = s_var then
        Result = "x"
    else if E = s_surd(A, B, C) then
        Result = "\\sqrt{" ++ latex_quad(A, B, C) ++ "}"
    else if E = s_neg(A) then
        Result = "-" ++ latex_prec(4, A)
    else if E = s_add(A, s_neg(B)) then
        Result = latex_parens_if(gt_bool(P, 1),
            latex_prec(1, A) ++ " - " ++ latex_prec(2, B))
    else if E = s_add(A, B) then
        Result = latex_parens_if(gt_bool(P, 1),
            latex_prec(1, A) ++ " + " ++ latex_prec(1, B))
    else if E = s_mul(s_rat(R2), B) then
        ( if R2 = rational.'-'(rational.zero, rational.one) then
            Result = latex_parens_if(gt_bool(P, 2),
                "-" ++ latex_prec(3, B))
        else
            Result = latex_parens_if(gt_bool(P, 2),
                latex_prec(2, s_rat(R2)) ++ " \\cdot " ++ latex_prec(3, B))
        )
    else if E = s_mul(A, B) then
        Result = latex_parens_if(gt_bool(P, 2),
            latex_prec(2, A) ++ " \\cdot " ++ latex_prec(3, B))
    else if E = s_div(A, B) then
        Result = "\\frac{" ++ latex_prec(0, A) ++ "}{" ++
                 latex_prec(0, B) ++ "}"
    else if E = s_pow(A, N) then
        ( if N = 0 then Result = "1"
        else if N = 1 then Result = latex_prec(5, A)
        else Result = latex_prec(5, A) ++ "^{" ++ string.from_int(N) ++ "}"
        )
    else if E = s_ln(A) then
        Result = "\\ln\\left|" ++ latex_prec(0, A) ++ "\\right|"
    else if E = s_arctan(A) then
        Result = "\\arctan\\left(" ++ latex_prec(0, A) ++ "\\right)"
    else if E = s_arcsin(A) then
        Result = "\\arcsin\\left(" ++ latex_prec(0, A) ++ "\\right)"
    else if E = s_arsinh(A) then
        Result = "\\operatorname{arsinh}\\left(" ++ latex_prec(0, A) ++ "\\right)"
    else if E = s_arcosh(A) then
        Result = "\\operatorname{arcosh}\\left(" ++ latex_prec(0, A) ++ "\\right)"
    else
        Result = "?"
    ).

:- func latex_parens_if(bool, string) = string.

latex_parens_if(yes, S) = "\\left(" ++ S ++ "\\right)".
latex_parens_if(no, S) = S.

:- func latex_quad(rational, rational, rational) = string.

latex_quad(A, B, C) = Result :-
    AStr = ( if A = rational.one then "x^{2}"
             else latex_prec(0, s_rat(A)) ++ "x^{2}" ),
    BStr = ( if B = rational.zero then ""
             else if B = rational.one then "x"
             else latex_prec(0, s_rat(B)) ++ "x" ),
    CStr = ( if C = rational.zero then ""
             else latex_prec(0, s_rat(C)) ),
    Terms = list.filter(
        (pred(S::in) is semidet :- S \= ""),
        [AStr, BStr, CStr]),
    ( if Terms = [] then Result = "0"
    else Result = string.join_list(" + ", Terms)
    ).

%---------------------------------------------------------------------------%
:- end_module euler_integrate.
%---------------------------------------------------------------------------%
