%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: elliptic_integrate.m
% Main author: nyc
%
% Reduction of elliptic integrals to Legendre normal forms.
%
% Given ∫ R(x) dx / √P(x) where P(x) has degree 3 or 4 with all real
% roots, expresses the result in terms of:
%   F(φ, k) — incomplete elliptic integral of the first kind
%   E(φ, k) — incomplete elliptic integral of the second kind
%   Π(φ, n, k) — incomplete elliptic integral of the third kind
%
% The modulus k is computed as an exact radical expression via cross-ratio
% of the roots and alg_num_to_rad_expr.
%
%---------------------------------------------------------------------------%

:- module elliptic_integrate.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%
% Types
%---------------------------------------------------------------------------%

    % An elliptic integrand: R(x) dx / √P(x), where R(x) = num/den.
    %
:- type elliptic_integrand
    --->    elliptic_integrand(
                ei_num      :: poly(rational),
                ei_den      :: poly(rational),
                ei_radicand :: poly(rational)
            ).

    % Which kind of Legendre elliptic integral.
    %
:- type legendre_kind
    --->    first_kind
    ;       second_kind
    ;       third_kind.

    % A single Legendre form term: coeff · F/E/Π(φ, [n,] k).
    %
:- type legendre_form
    --->    legendre_form(
                lf_kind      :: legendre_kind,
                lf_coeff     :: rad_expr(rational),
                lf_amplitude :: amplitude_expr,
                lf_parameter :: maybe(rad_expr(rational)),
                lf_modulus   :: rad_expr(rational)
            ).

    % Expression for the amplitude φ = arcsin(√((x - shift) · scale)).
    %
:- type amplitude_expr
    --->    amplitude_expr(
                ae_shift :: rad_expr(rational),
                ae_scale :: rad_expr(rational)
            ).

    % Result of elliptic reduction.
    %
:- type elliptic_result
    --->    elliptic_result(
                er_terms      :: list(legendre_form),
                er_modulus    :: rad_expr(rational),
                er_modulus_sq :: rad_expr(rational),
                er_roots      :: list(rad_expr(rational)),
                er_lead_coeff :: rational,
                er_degree     :: int,
                er_jacobi     :: bool
            ).

    % Reduce an elliptic integral to Legendre forms.
    %
:- func reduce_elliptic(bool, elliptic_integrand) = maybe(elliptic_result).

    % Pretty-print an elliptic result.
    %
:- func pretty_elliptic_result(elliptic_result) = string.

    % LaTeX render an elliptic result.
    %
:- func latex_elliptic_result(elliptic_result) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module alg_num.
:- import_module factoring.
:- use_module float.
:- use_module int.
:- import_module integer.
:- import_module interval.
:- import_module normal_form.
:- import_module pretty.
:- import_module latex.
:- import_module rad_alg_convert.
:- import_module rad_normalize.
:- import_module root_isolation.
:- import_module string.

%---------------------------------------------------------------------------%
% Root finding
%---------------------------------------------------------------------------%

:- func find_real_roots(poly(rational)) =
    maybe(list({alg_num, rad_expr(rational)})).

find_real_roots(P) = Result :-
    D = poly.degree(P),
    ( if D \= 3, D \= 4 then
        Result = no
    else
        Facs = factor(P),
        MaybeRoots = roots_from_factors(Facs),
        ( if MaybeRoots = yes(Roots), list.length(Roots) = D then
            list.sort(
                (pred({A1, _}::in, {A2X, _}::in, R::out) is det :-
                    V1 = alg_to_double(A1),
                    V2 = alg_to_double(A2X),
                    ( if float.'>'(V1, V2) then R = (<)
                    else if float.'<'(V1, V2) then R = (>)
                    else R = (=)
                    )
                ), Roots, Sorted),
            Result = yes(Sorted)
        else
            Result = no
        )
    ).

:- func roots_from_factors(list(factor_pair)) =
    maybe(list({alg_num, rad_expr(rational)})).

roots_from_factors(Facs) = Result :-
    roots_from_factors_loop(Facs, [], Result).

:- pred roots_from_factors_loop(list(factor_pair)::in,
    list({alg_num, rad_expr(rational)})::in,
    maybe(list({alg_num, rad_expr(rational)}))::out) is det.

roots_from_factors_loop([], Acc, yes(Acc)).
roots_from_factors_loop([factor_pair(F, _Mult) | Rest], Acc, Result) :-
    ( if roots_of_factor(F) = yes(Roots) then
        roots_from_factors_loop(Rest, Acc ++ Roots, Result)
    else
        Result = no
    ).

:- func roots_of_factor(poly(rational)) =
    maybe(list({alg_num, rad_expr(rational)})).

roots_of_factor(F) = Result :-
    D = poly.degree(F),
    ( if D = 0 then
        Result = yes([])
    else if D = 1 then
        Cs = coeffs(F),
        ( if Cs = [C, _] then
            R = rational.'-'(rational.zero, C),
            Result = yes([{alg_from_rational(R), re_lit(R)}])
        else
            Result = no
        )
    else
        IIs = isolate_real_roots(F),
        iso_to_alg_rad_list(IIs, [], Result)
    ).

:- pred iso_to_alg_rad_list(list(isolating_interval)::in,
    list({alg_num, rad_expr(rational)})::in,
    maybe(list({alg_num, rad_expr(rational)}))::out) is det.

iso_to_alg_rad_list([], Acc, yes(Acc)).
iso_to_alg_rad_list([II | Rest], Acc, Result) :-
    ( if iso_to_alg_rad(II) = yes(Pair) then
        iso_to_alg_rad_list(Rest, Acc ++ [Pair], Result)
    else
        Result = no
    ).

:- func iso_to_alg_rad(isolating_interval) =
    maybe({alg_num, rad_expr(rational)}).

iso_to_alg_rad(II) = Result :-
    ( if root_in_interval(II) = yes(R) then
        Result = yes({alg_from_rational(R), re_lit(R)})
    else
        P = ii_poly(II),
        interval(Lo, Hi) = ii_interval(II),
        Mid = rational.'/'(rational.'+'(Lo, Hi), rational(2)),
        Approx = rational_to_float(Mid),
        Alg = alg_from_poly(P, Approx),
        ( if alg_num_to_rad_expr(Alg) = yes(Rad) then
            Result = yes({Alg, normalize(Rad)})
        else
            Result = no
        )
    ).

:- func alg_to_double(alg_num) = float.

alg_to_double(A) = rational_to_float(
    alg_approx(from_integers(integer.one, integer(1000000000000000)), A)).

:- func rational_to_float(rational) = float.

rational_to_float(R) = Result :-
    N = integer.to_string(numer(R)),
    D = integer.to_string(denom(R)),
    ( if string.to_float(N, NF), string.to_float(D, DF) then
        Result = float.'/'(NF, DF)
    else
        Result = 0.0
    ).

%---------------------------------------------------------------------------%
% Modulus computation
%---------------------------------------------------------------------------%

:- func compute_modulus_sq(list({alg_num, rad_expr(rational)})) =
    maybe({alg_num, rad_expr(rational)}).

compute_modulus_sq(Roots) = Result :-
    ( if Roots = [{E1A, _}, {E2A, _}, {E3A, _}] then
        % Cubic: k² = (e₂ - e₃) / (e₁ - e₃)
        K2Alg = alg_div(alg_sub(E2A, E3A), alg_sub(E1A, E3A)),
        ( if alg_num_to_rad_expr(K2Alg) = yes(K2Rad) then
            Result = yes({K2Alg, normalize(K2Rad)})
        else
            Result = no
        )
    else if Roots = [{E1A, _}, {E2A, _}, {E3A, _}, {E4A, _}] then
        % Quartic: k² = (e₂-e₃)(e₁-e₄) / ((e₁-e₃)(e₂-e₄))
        Num = alg_mul(alg_sub(E2A, E3A), alg_sub(E1A, E4A)),
        Den = alg_mul(alg_sub(E1A, E3A), alg_sub(E2A, E4A)),
        K2Alg = alg_div(Num, Den),
        ( if alg_num_to_rad_expr(K2Alg) = yes(K2Rad) then
            Result = yes({K2Alg, normalize(K2Rad)})
        else
            Result = no
        )
    else
        Result = no
    ).

:- func simplify_rad(rad_expr(rational)) = rad_expr(rational).

simplify_rad(E) =
    simplify_via_canonical(normalize(from_norm_expr(to_norm_expr(E)))).

%---------------------------------------------------------------------------%
% Reduction to Legendre forms
%---------------------------------------------------------------------------%

reduce_elliptic(Jacobi, EI) = Result :-
    P = ei_radicand(EI),
    D = poly.degree(P),
    ( if lead_coeff(P, LC0) then LC = LC0 else LC = rational.one ),
    ( if find_real_roots(P) = yes(Roots) then
        ( if compute_modulus_sq(Roots) = yes({_K2Alg, K2Raw}) then
            K2Rad = simplify_rad(K2Raw),
            KRad = simplify_rad(re_root(2, K2Rad)),
            ( if reduce_integrand(EI, Roots, LC, D, K2Rad, KRad) =
                yes(Terms)
            then
                Result = yes(elliptic_result(
                    Terms, KRad, K2Rad,
                    list.map(func({_, R}) = R, Roots),
                    LC, D, Jacobi))
            else
                Result = no
            )
        else
            Result = no
        )
    else
        Result = no
    ).

:- func reduce_integrand(elliptic_integrand,
    list({alg_num, rad_expr(rational)}),
    rational, int,
    rad_expr(rational), rad_expr(rational)) = maybe(list(legendre_form)).

reduce_integrand(EI, Roots, LC, D, K2Rad, KRad) = Result :-
    Num = ei_num(EI),
    Den = ei_den(EI),
    DegNum = poly.degree(Num),
    DegDen = poly.degree(Den),
    ( if DegNum = 0, DegDen = 0 then
        % ∫ dx / √P(x) → F form
        ( if lead_coeff(Num, N0), lead_coeff(Den, D0) then
            C = rational.'/'(N0, D0)
        else
            C = rational.one
        ),
        Result = reduce_dx_over_sqrt_p(C, Roots, LC, D, KRad)
    else if Num = ei_radicand(EI), DegDen = 0 then
        % ∫ √P(x) dx → E and F forms
        ( if lead_coeff(Den, D0) then
            C = rational.'/'(rational.one, D0)
        else
            C = rational.one
        ),
        Result = reduce_sqrt_p(C, Roots, LC, D, K2Rad, KRad)
    else if DegNum = 0, DegDen = 1 then
        % ∫ dx / ((x - a) · √P(x)) → Π form
        Cs = coeffs(Den),
        ( if Cs = [NegA, S] then
            Pole = rational.'/'(rational.'-'(rational.zero, NegA), S),
            ( if lead_coeff(Num, N0) then
                C = rational.'/'(N0, S)
            else
                C = rational.'/'(rational.one, S)
            ),
            Result = reduce_pi_term(C, Pole, Roots, LC, D, KRad)
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% ∫ c · dx / √P(x) → F form
%---------------------------------------------------------------------------%

:- func reduce_dx_over_sqrt_p(rational,
    list({alg_num, rad_expr(rational)}),
    rational, int, rad_expr(rational)) = maybe(list(legendre_form)).

reduce_dx_over_sqrt_p(C, Roots, LC, D, KRad) = Result :-
    ( if D = 3,
         Roots = [{E1A, _}, {_, E2R}, {E3A, E3R}]
    then
        Diff13 = alg_sub(E1A, E3A),
        ( if alg_num_to_rad_expr(Diff13) = yes(Diff13Rad) then
            ScaleSq = re_mul(re_lit(LC), normalize(Diff13Rad)),
            Scale = normalize(re_root(2, ScaleSq)),
            Coeff = simplify_rad(re_mul(re_lit(rational.'*'(rational(2), C)),
                                        re_inv(Scale))),
            Diff23 = normalize(re_add(re_neg(E3R), E2R)),
            Amp = amplitude_expr(E3R, normalize(re_inv(Diff23))),
            Result = yes([legendre_form(first_kind, Coeff, Amp, no, KRad)])
        else
            Result = no
        )
    else if D = 4,
            Roots = [{E1A, _}, {E2A, E2R}, {E3A, E3R}, {_, _}]
    then
        Diff13 = alg_sub(E1A, E3A),
        Diff24 = alg_sub(E2A, E3A),
        ( if
            alg_num_to_rad_expr(Diff13) = yes(Diff13Rad),
            alg_num_to_rad_expr(Diff24) = yes(Diff24Rad)
        then
            ScaleSq = re_mul(re_lit(LC),
                        re_mul(normalize(Diff13Rad), normalize(Diff24Rad))),
            Scale = normalize(re_root(2, ScaleSq)),
            Coeff = simplify_rad(re_mul(re_lit(rational.'*'(rational(2), C)),
                                        re_inv(Scale))),
            Diff23 = normalize(re_add(re_neg(E3R), E2R)),
            Amp = amplitude_expr(E3R, normalize(re_inv(Diff23))),
            Result = yes([legendre_form(first_kind, Coeff, Amp, no, KRad)])
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% ∫ c · √P(x) dx → E and F forms
%---------------------------------------------------------------------------%

:- func reduce_sqrt_p(rational,
    list({alg_num, rad_expr(rational)}),
    rational, int,
    rad_expr(rational), rad_expr(rational)) = maybe(list(legendre_form)).

reduce_sqrt_p(C, Roots, LC, D, K2Rad, KRad) = Result :-
    ( if D = 3,
         Roots = [{E1A, _}, {E2A, E2R}, {E3A, E3R}]
    then
        Diff13 = alg_sub(E1A, E3A),
        Diff23 = alg_sub(E2A, E3A),
        ( if
            alg_num_to_rad_expr(Diff13) = yes(Diff13Rad),
            alg_num_to_rad_expr(Diff23) = yes(Diff23Rad)
        then
            D13 = normalize(Diff13Rad),
            D23 = normalize(Diff23Rad),
            SqrtScale = normalize(re_root(2, re_mul(re_lit(LC), D13))),
            TwoC3 = rational.'/'(rational.'*'(rational(2), C), rational(3)),
            CoeffE = simplify_rad(re_mul(re_lit(TwoC3),
                                         re_mul(SqrtScale, D23))),
            OneMinusK2 = simplify_rad(re_add(re_lit(rational.one),
                                              re_neg(K2Rad))),
            NegTwoC3 = rational.'-'(rational.zero, TwoC3),
            CoeffF = simplify_rad(re_mul(re_lit(NegTwoC3),
                        re_mul(SqrtScale, re_mul(D23, OneMinusK2)))),
            Diff23Expr = normalize(re_add(re_neg(E3R), E2R)),
            Amp = amplitude_expr(E3R, normalize(re_inv(Diff23Expr))),
            Result = yes([
                legendre_form(second_kind, CoeffE, Amp, no, KRad),
                legendre_form(first_kind, CoeffF, Amp, no, KRad)
            ])
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% ∫ c · dx / ((x - pole) · √P(x)) → Π form
%---------------------------------------------------------------------------%

:- func reduce_pi_term(rational, rational,
    list({alg_num, rad_expr(rational)}),
    rational, int, rad_expr(rational)) = maybe(list(legendre_form)).

reduce_pi_term(C, Pole, Roots, LC, D, KRad) = Result :-
    ( if D = 3,
         Roots = [{E1A, _}, {E2A, E2R}, {E3A, E3R}]
    then
        PoleAlg = alg_from_rational(Pole),
        Diff13 = alg_sub(E1A, E3A),
        Diff23 = alg_sub(E2A, E3A),
        DiffPE3 = alg_sub(PoleAlg, E3A),
        ( if
            alg_num_to_rad_expr(Diff13) = yes(Diff13Rad),
            alg_num_to_rad_expr(DiffPE3) = yes(DiffPE3Rad)
        then
            NAlg = alg_div(Diff23, DiffPE3),
            ( if alg_num_to_rad_expr(NAlg) = yes(NRad) then
                ScaleSq = re_mul(re_lit(LC), normalize(Diff13Rad)),
                Scale = normalize(re_root(2, ScaleSq)),
                CoeffDen = normalize(re_mul(Scale, normalize(DiffPE3Rad))),
                Coeff = simplify_rad(
                    re_mul(re_lit(rational.'*'(rational(2), C)),
                           re_inv(CoeffDen))),
                Diff23Expr = normalize(re_add(re_neg(E3R), E2R)),
                Amp = amplitude_expr(E3R, normalize(re_inv(Diff23Expr))),
                Result = yes([legendre_form(third_kind, Coeff, Amp,
                                            yes(normalize(NRad)), KRad)])
            else
                Result = no
            )
        else
            Result = no
        )
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Pretty-print rendering
%---------------------------------------------------------------------------%

pretty_elliptic_result(ER) = Result :-
    RootStrs = list.map(pretty.pretty, er_roots(ER)),
    Lines = [
        "Roots of P(x) (descending): " ++ string.join_list(", ", RootStrs),
        "Modulus: k = " ++ pretty.pretty(er_modulus(ER)),
        "        k² = " ++ pretty.pretty(er_modulus_sq(ER)),
        "",
        "Reduction:"
    ] ++ list.map(
        func(LF) = "  " ++ pretty_legendre_form(er_jacobi(ER), LF),
        er_terms(ER)),
    Result = string.join_list("\n", Lines).

:- func pretty_legendre_form(bool, legendre_form) = string.

pretty_legendre_form(Jacobi, LF) = Result :-
    C = pretty_coeff(lf_coeff(LF)),
    K = pretty.pretty(lf_modulus(LF)),
    Phi = pretty_amplitude(lf_amplitude(LF)),
    ( if lf_kind(LF) = first_kind then
        ( if Jacobi = yes then
            Result = C ++ " · sn⁻¹(" ++
                     pretty_amplitude_sin(lf_amplitude(LF)) ++ ", " ++ K ++ ")"
        else
            Result = C ++ " · F(" ++ Phi ++ ", " ++ K ++ ")"
        )
    else if lf_kind(LF) = second_kind then
        Result = C ++ " · E(" ++ Phi ++ ", " ++ K ++ ")"
    else
        ParamStr = ( if lf_parameter(LF) = yes(NR) then
                        pretty.pretty(NR)
                     else "?" ),
        Result = C ++ " · Π(" ++ Phi ++ ", " ++ ParamStr ++ ", " ++ K ++ ")"
    ).

:- func pretty_coeff(rad_expr(rational)) = string.

pretty_coeff(E) = Result :-
    ( if E = re_add(_, _) then
        Result = "(" ++ pretty.pretty(E) ++ ")"
    else
        Result = pretty.pretty(E)
    ).

:- func pretty_amplitude(amplitude_expr) = string.

pretty_amplitude(AE) =
    "arcsin(√(" ++ pretty_amp_inner(AE) ++ "))".

:- func pretty_amplitude_sin(amplitude_expr) = string.

pretty_amplitude_sin(AE) =
    "√(" ++ pretty_amp_inner(AE) ++ ")".

:- func pretty_amp_inner(amplitude_expr) = string.

pretty_amp_inner(AE) = Result :-
    HasScale = has_nontrivial_scale(ae_scale(AE)),
    ShiftStr = pretty_shift(ae_shift(AE), HasScale),
    ScaleStr = ( if HasScale = yes then
                    ( if ae_scale(AE) = re_inv(E) then
                        "/" ++ pretty.pretty(E)
                    else
                        " · " ++ pretty.pretty(ae_scale(AE))
                    )
                 else "" ),
    Result = ShiftStr ++ ScaleStr.

:- func pretty_shift(rad_expr(rational), bool) = string.

pretty_shift(Shift, HasScale) = Result :-
    ( if Shift = re_lit(R), R = rational.zero then
        Result = "x"
    else if Shift = re_lit(R), rational.'<'(R, rational.zero) then
        NegR = rational.'-'(rational.zero, R),
        Inner = "x + " ++ pretty.pretty(re_lit(NegR)),
        Result = ( if HasScale = yes then "(" ++ Inner ++ ")"
                   else Inner )
    else if Shift = re_lit(R), rational.'>'(R, rational.zero) then
        Inner = "x - " ++ pretty.pretty(re_lit(R)),
        Result = ( if HasScale = yes then "(" ++ Inner ++ ")"
                   else Inner )
    else
        Result = "(x - " ++ pretty.pretty(Shift) ++ ")"
    ).

:- func has_nontrivial_scale(rad_expr(rational)) = bool.

has_nontrivial_scale(E) = Result :-
    ( if E = re_inv(re_lit(R)), R = rational.one then
        Result = no
    else if E = re_lit(R), R = rational.one then
        Result = no
    else
        Result = yes
    ).

%---------------------------------------------------------------------------%
% LaTeX rendering
%---------------------------------------------------------------------------%

latex_elliptic_result(ER) = Result :-
    RootStrs = list.map(latex.latex, er_roots(ER)),
    Lines = [
        "\\text{Roots of } P(x) \\text{ (descending): } " ++
            string.join_list(", ", RootStrs),
        "k = " ++ latex.latex(er_modulus(ER)) ++
            ", \\quad k^2 = " ++ latex.latex(er_modulus_sq(ER)),
        "",
        "\\text{Reduction:}"
    ] ++ list.map(
        func(LF) = "  " ++ latex_legendre_form(er_jacobi(ER), LF),
        er_terms(ER)),
    Result = string.join_list("\n", Lines).

:- func latex_legendre_form(bool, legendre_form) = string.

latex_legendre_form(Jacobi, LF) = Result :-
    C = latex_coeff(lf_coeff(LF)),
    K = latex.latex(lf_modulus(LF)),
    Phi = latex_amplitude(lf_amplitude(LF)),
    ( if lf_kind(LF) = first_kind then
        ( if Jacobi = yes then
            Result = C ++ " \\operatorname{sn}^{-1}\\!\\left(" ++
                     latex_amplitude_sin(lf_amplitude(LF)) ++
                     ", " ++ K ++ "\\right)"
        else
            Result = C ++ " F\\!\\left(" ++ Phi ++ ", " ++ K ++ "\\right)"
        )
    else if lf_kind(LF) = second_kind then
        Result = C ++ " E\\!\\left(" ++ Phi ++ ", " ++ K ++ "\\right)"
    else
        ParamStr = ( if lf_parameter(LF) = yes(NR) then
                        latex.latex(NR)
                     else "?" ),
        Result = C ++ " \\Pi\\!\\left(" ++ Phi ++ ", " ++
                 ParamStr ++ ", " ++ K ++ "\\right)"
    ).

:- func latex_coeff(rad_expr(rational)) = string.

latex_coeff(E) = Result :-
    ( if E = re_add(_, _) then
        Result = "\\left(" ++ latex.latex(E) ++ "\\right)"
    else
        Result = latex.latex(E)
    ).

:- func latex_amplitude(amplitude_expr) = string.

latex_amplitude(AE) =
    "\\arcsin\\sqrt{" ++ latex_amp_inner(AE) ++ "}".

:- func latex_amplitude_sin(amplitude_expr) = string.

latex_amplitude_sin(AE) =
    "\\sqrt{" ++ latex_amp_inner(AE) ++ "}".

:- func latex_amp_inner(amplitude_expr) = string.

latex_amp_inner(AE) = Result :-
    NumStr = latex_amp_num(ae_shift(AE)),
    DenStr = latex_amp_den(ae_scale(AE)),
    ( if DenStr = no then
        Result = NumStr
    else if DenStr = yes(DStr) then
        Result = "\\frac{" ++ NumStr ++ "}{" ++ DStr ++ "}"
    else
        Result = NumStr
    ).

:- func latex_amp_num(rad_expr(rational)) = string.

latex_amp_num(Shift) = Result :-
    ( if Shift = re_lit(R0), R0 = rational.zero then
        Result = "x"
    else if Shift = re_lit(R1), rational.'<'(R1, rational.zero) then
        Result = "x + " ++
            latex.latex(re_lit(rational.'-'(rational.zero, R1)))
    else
        Result = "x - " ++ latex.latex(Shift)
    ).

:- func latex_amp_den(rad_expr(rational)) = maybe(string).

latex_amp_den(Scale) = Result :-
    ( if Scale = re_inv(re_lit(R0)), R0 = rational.one then
        Result = no
    else if Scale = re_inv(E) then
        Result = yes(latex.latex(E))
    else if Scale = re_lit(R1), R1 = rational.one then
        Result = no
    else
        Result = yes(latex.latex(re_inv(Scale)))
    ).

%---------------------------------------------------------------------------%
:- end_module elliptic_integrate.
%---------------------------------------------------------------------------%
