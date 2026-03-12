%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: tower_descent.m
% Main author: nyc
%
% Tower-based Gauss period descent.
%
% Mirrors the algorithm in trig_galois.m but represents periods as
% elements of a dynamically-built field tower (tower_elem from dyn_tower.m)
% instead of flat rad_expr. This preserves algebraic structure:
%
%   * Each descent level is an explicit field extension
%   * Arithmetic uses polynomial reduction (exact, no expression growth)
%   * Galois action σₐ: ζ→ζᵃ is a substitution in the tower
%   * Conversion to rad_expr happens only at display time
%
% The tower approach eliminates expression swell: cos(2π/11) stays as
% a degree-10 tower element with small rational coefficients, rather
% than a tree of nested radicals.
%
%---------------------------------------------------------------------------%

:- module tower_descent.
:- interface.

:- import_module dyn_tower.
:- import_module integer.
:- import_module map.
:- import_module maybe.
:- import_module rad_eval.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

:- type tower_result
    --->    tower_result(
                tr_cos      :: tower_elem,
                tr_sin      :: tower_elem,
                tr_periods  :: map(int, tower_elem),
                tr_rad_expr :: rad_expr(rational)
            ).

    % Compute cos(2π/n) via tower-based Gauss period descent.
    %
:- func cos_via_tower(int) = maybe(rad_expr(rational)).

    % Tower-based descent producing all periods.
    %
:- func all_periods_via_tower(int) = maybe(tower_result).

    % Approximate numerical evaluation of a tower element.
    % Used only for branch selection / assignment matching.
    %
:- func eval_tower_approx(tower_elem, integer) = complex.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module float.
:- use_module int.
:- use_module math.
:- import_module list.
:- import_module pair.
:- import_module positive.
:- import_module prime_factors.
:- import_module string.
:- import_module trig_galois.

%---------------------------------------------------------------------------%

cos_via_tower(N) = Result :-
    ( if all_periods_via_tower(N) = yes(TR) then
        Result = yes(tr_rad_expr(TR))
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% Main entry point
%---------------------------------------------------------------------------%

all_periods_via_tower(N) = Result :-
    ( if int.'=<'(N, 2) then
        Result = no
    else
        NI = integer(N),
        ( if primitive_root_mod(NI) = yes(G) then
            Phi = euler_totient(NI),
            Fs = factorise(det_positive(Phi)),
            Steps = reorder_factors(NI, Fs),
            StepList = expand_steps(Steps),

            % Coprime elements: g^k mod n for k = 0..phi-1
            PhiInt = integer.det_to_int(Phi),
            CoprimeElems = list.map(
                ( func(K) = mod_exp(G, integer(K), NI) ),
                0 `..` int.'-'(PhiInt, 1)),

            % Initial sum of all cosines (numerical, then round)
            InitSumFloat = list.foldl(
                ( func(K, Acc) =
                    float.'+'(Acc,
                        math.cos(float.'/'(
                            float.'*'(
                                float.'*'(2.0, math.pi),
                                float.float(integer.det_to_int(K))),
                            float.float(N))))
                ), CoprimeElems, 0.0),
            InitInt = float.round_to_int(InitSumFloat),
            InitExpr = t_rat(rational(InitInt)),

            InitPeriod = tp_state(InitExpr, CoprimeElems, NI),

            % Descend through the step chain
            {FinalPeriods, _} = list.foldl(
                ( func(Q, {Periods, NextId}) =
                    tower_descent_step(Periods, Q, NI, NextId)
                ), StepList, {[InitPeriod], 1}),

            % Build period map: for each period state, each exponent
            % maps to that period's tower element
            PeriodMap = list.foldl(
                ( func(PS, M) =
                    list.foldl(
                        ( func(K, M0) =
                            map.det_insert(M0,
                                integer.det_to_int(K),
                                tp_elem(PS))
                        ), tp_elems(PS), M)
                ), FinalPeriods, map.init),

            % cos(2π/n) = (ζ + ζ⁻¹)/2
            Target = 1,
            ConjElem = int.'-'(N, 1),
            ( if map.search(PeriodMap, Target, Zeta),
                 map.search(PeriodMap, ConjElem, ZetaConj)
            then
                Two = t_rat(rational(2)),
                CosVal = t_div(t_add(Zeta, ZetaConj), Two),
                CosRad = tower_to_rad_expr(CosVal),
                % sin(2π/n) = (ζ - ζ⁻¹)/(2i)
                IElem = tower_sqrt_neg1,
                SinVal = t_div(t_sub(Zeta, ZetaConj),
                               t_mul(Two, IElem)),
                Result = yes(tower_result(CosVal, SinVal,
                                          PeriodMap, CosRad))
            else
                Result = no
            )
        else
            Result = no
        )
    ).

%---------------------------------------------------------------------------%
% Period state
%---------------------------------------------------------------------------%

:- type tp_state
    --->    tp_state(
                tp_elem     :: tower_elem,
                tp_elems    :: list(integer),
                tp_p        :: integer
            ).

%---------------------------------------------------------------------------%
% √(-1) as a tower element
%---------------------------------------------------------------------------%

:- func tower_sqrt_neg1 = tower_elem.

tower_sqrt_neg1 = Alpha :-
    {_, Alpha} = adjoin_tower_root(9999, 2, t_rat(rational.'-'(rational.zero, rational.one))).

%---------------------------------------------------------------------------%
% Descent step
%---------------------------------------------------------------------------%

:- func tower_descent_step(list(tp_state), int, integer, int) =
    {list(tp_state), int}.

tower_descent_step(Periods, Q, P, NextId) = Result :-
    tower_descent_step_loop(Periods, Periods, Q, P, NextId, [], Result).

:- pred tower_descent_step_loop(list(tp_state)::in, list(tp_state)::in,
    int::in, integer::in, int::in, list(tp_state)::in,
    {list(tp_state), int}::out) is det.

tower_descent_step_loop([], _, _, _, NId, Acc, {list.reverse(Acc), NId}).
tower_descent_step_loop([Per | Rest], AllPeriods, Q, P, NId, Acc, Result) :-
    {Subs, NId1} = tower_split_period(AllPeriods, Q, P, NId, Per),
    tower_descent_step_loop(Rest, AllPeriods, Q, P, NId1,
        list.reverse(Subs) ++ Acc, Result).

%---------------------------------------------------------------------------%
% Split one period into q sub-periods
%---------------------------------------------------------------------------%

:- func tower_split_period(list(tp_state), int, integer, int,
    tp_state) = {list(tp_state), int}.

tower_split_period(AllPeriods, Q, P, NextId, Parent) = Result :-
    Elems = tp_elems(Parent),
    F = list.length(Elems),
    SubF = int.'/'(F, Q),
    % Build sub-period element lists
    SubPeriodElems = list.map(
        ( func(K) = list.map(
            ( func(J) = list.det_index0(Elems, int.'+'(K, int.'*'(Q, J))) ),
            0 `..` int.'-'(SubF, 1))
        ), 0 `..` int.'-'(Q, 1)),

    % Numerical values of sub-periods
    SubPeriodValues = list.map(sum_roots_of_unity(P), SubPeriodElems),

    % Numerical values of current-level periods
    PeriodValues = list.map(
        ( func(PS) = sum_roots_of_unity(P, tp_elems(PS)) ),
        AllPeriods),

    ( if Q = 2 then
        Result = tower_solve_quadratic(AllPeriods, NextId, Parent,
            SubPeriodElems, SubPeriodValues, PeriodValues, P)
    else if Q = 3 then
        Result = tower_solve_cubic(AllPeriods, NextId, Parent,
            SubPeriodElems, SubPeriodValues, PeriodValues, P)
    else if Q = 5 then
        Result = tower_solve_q5(AllPeriods, NextId, Parent,
            SubPeriodElems, SubPeriodValues, PeriodValues, P)
    else
        % Fallback for q > 5: match sub-periods numerically
        SubPeriodTower = list.map(
            match_to_tower_elem(AllPeriods,
                list.map(
                    ( func(PS) = sum_roots_of_unity(P, tp_elems(PS)) ),
                    AllPeriods),
                P),
            SubPeriodValues),
        Result = {build_tp_states(SubPeriodTower, SubPeriodElems, P),
                  NextId}
    ).

%---------------------------------------------------------------------------%
% Sum of ζ^k for a list of exponents
%---------------------------------------------------------------------------%

:- func sum_roots_of_unity(integer, list(integer)) = complex.

sum_roots_of_unity(N, Ks) = list.foldl(
    ( func(K, Acc) = cadd(Acc, zeta_k(N, K)) ),
    Ks, complex(0.0, 0.0)).

:- func zeta_k(integer, integer) = complex.

zeta_k(N, K) = complex(math.cos(Theta), math.sin(Theta)) :-
    Theta = float.'/'(
        float.'*'(float.'*'(2.0, math.pi),
                  float.float(integer.det_to_int(K))),
        float.float(integer.det_to_int(N))).

%---------------------------------------------------------------------------%
% Quadratic solver (tower-native)
%---------------------------------------------------------------------------%

:- func tower_solve_quadratic(list(tp_state), int, tp_state,
    list(list(integer)), list(complex), list(complex), integer) =
    {list(tp_state), int}.

tower_solve_quadratic(AllPeriods, NextId, Parent,
        SubPeriodElems, SubPeriodValues, PeriodValues, P) = Result :-
    % e₁ = sum of sub-periods = parent
    E1 = tp_elem(Parent),
    % e₂ = product of two sub-periods (numerical, then match)
    E2Num = cmul(list.det_index0(SubPeriodValues, 0),
                 list.det_index0(SubPeriodValues, 1)),
    E2 = match_to_tower_elem(AllPeriods, PeriodValues, P, E2Num),

    % disc = e₁² - 4·e₂
    Disc = t_sub(t_mul(E1, E1), t_mul(t_from_int(4), E2)),

    % Adjoin √disc
    {Lvl, SqrtDisc} = adjoin_tower_root(NextId, 2, Disc),

    % Embed e1 into new level
    E1P = promote_to(Lvl, E1),
    Two = t_from_int(2),

    % roots = (e₁ ± √disc) / 2
    Root1 = t_div(t_add(E1P, SqrtDisc), Two),
    Root2 = t_div(t_sub(E1P, SqrtDisc), Two),

    % Match to numerical values for correct assignment
    Roots = assign_tower_by_value([Root1, Root2], SubPeriodValues, P),
    Result = {build_tp_states(Roots, SubPeriodElems, P),
              int.'+'(NextId, 1)}.

%---------------------------------------------------------------------------%
% Cubic solver (tower-native)
%---------------------------------------------------------------------------%

:- func tower_solve_cubic(list(tp_state), int, tp_state,
    list(list(integer)), list(complex), list(complex), integer) =
    {list(tp_state), int}.

tower_solve_cubic(AllPeriods, NextId, Parent,
        SubPeriodElems, SubPeriodValues, PeriodValues, P) = Result :-
    E1 = tp_elem(Parent),

    % e₂ = Σᵢ<ⱼ ηᵢηⱼ
    V0 = list.det_index0(SubPeriodValues, 0),
    V1 = list.det_index0(SubPeriodValues, 1),
    V2 = list.det_index0(SubPeriodValues, 2),
    E2Num = cadd(cadd(cmul(V0, V1), cmul(V0, V2)), cmul(V1, V2)),
    E2 = match_to_tower_elem(AllPeriods, PeriodValues, P, E2Num),

    % e₃ = η₀η₁η₂
    E3Num = cmul(cmul(V0, V1), V2),
    E3 = match_to_tower_elem(AllPeriods, PeriodValues, P, E3Num),

    % Depressed cubic: p = e₂ - e₁²/3
    Three = t_from_int(3),
    PCoeff = t_sub(E2, t_div(t_mul(E1, E1), Three)),

    % q = -e₃ + e₁·e₂/3 - 2·e₁³/27
    QCoeff = t_add(
        t_add(t_neg(E3),
              t_div(t_mul(E1, E2), Three)),
        t_neg(t_div(t_mul(t_from_int(2),
                          t_mul(E1, t_mul(E1, E1))),
                    t_from_int(27)))),

    % Δ = q²/4 + p³/27
    Delta = t_add(
        t_div(t_mul(QCoeff, QCoeff), t_from_int(4)),
        t_div(t_mul(PCoeff, t_mul(PCoeff, PCoeff)), t_from_int(27))),

    % Adjoin √Δ
    {Lvl1, SqrtDelta} = adjoin_tower_root(NextId, 2, Delta),

    % Embed into new level
    QCoeffP = promote_to(Lvl1, QCoeff),
    PCoeffP = promote_to(Lvl1, PCoeff),
    E1P = promote_to(Lvl1, E1),

    % u₁_arg = -q/2 + √Δ,  u₂_arg = -q/2 - √Δ
    NegQHalf = t_div(t_neg(QCoeffP), t_from_int(2)),
    U1Arg = t_add(NegQHalf, SqrtDelta),
    U2Arg = t_sub(NegQHalf, SqrtDelta),

    % When u₁_arg = 0, adjoin ∛u₂_arg instead of using -p/(3·u₁).
    % Check U1Arg for zero BEFORE adjoining to avoid denormalized zeros.
    ( if t_is_zero(U1Arg) then
        % Degenerate Cardano: U1Arg = 0, adjoin ∛U2Arg instead
        {Lvl2, U2B} = adjoin_tower_root(int.'+'(NextId, 1), 3, U2Arg),
        PCoeffPP = promote_to(Lvl2, PCoeffP),
        ( if t_is_zero(PCoeffPP) then
            U1 = t_from_int(0),
            U2 = U2B
        else
            U2 = U2B,
            U1 = t_div(t_neg(PCoeffPP), t_mul(t_from_int(3), U2))
        )
    else
        {Lvl2, U1} = adjoin_tower_root(int.'+'(NextId, 1), 3, U1Arg),
        PCoeffPP = promote_to(Lvl2, PCoeffP),
        _ = U2Arg,
        ( if t_is_zero(PCoeffPP) then
            U2 = t_from_int(0)
        else
            U2 = t_div(t_neg(PCoeffPP), t_mul(t_from_int(3), U1))
        )
    ),

    % ω = (-1 + √(-3))/2
    Neg3 = promote_to(Lvl2, promote_to(Lvl1, t_from_int(-3))),
    {Lvl3, SqrtNeg3} = adjoin_tower_root(int.'+'(NextId, 2), 2, Neg3),

    Omega = t_div(t_add(t_from_int(-1), SqrtNeg3), t_from_int(2)),
    OmegaBar = t_div(t_sub(t_from_int(-1), SqrtNeg3), t_from_int(2)),

    U1PP = promote_to(Lvl3, U1),
    U2PP = promote_to(Lvl3, U2),
    Shift = t_div(promote_to(Lvl3, promote_to(Lvl2, E1P)), t_from_int(3)),

    Root0 = t_add(t_add(U1PP, U2PP), Shift),
    Root1 = t_add(t_add(t_mul(Omega, U1PP), t_mul(OmegaBar, U2PP)), Shift),
    Root2 = t_add(t_add(t_mul(OmegaBar, U1PP), t_mul(Omega, U2PP)), Shift),
    Roots = assign_tower_by_value([Root0, Root1, Root2], SubPeriodValues, P),
    Result = {build_tp_states(Roots, SubPeriodElems, P),
              int.'+'(NextId, 3)}.

%---------------------------------------------------------------------------%
% Quintic resolvent solver (q = 5)
%---------------------------------------------------------------------------%

:- func tower_solve_q5(list(tp_state), int, tp_state,
    list(list(integer)), list(complex), list(complex), integer) =
    {list(tp_state), int}.

tower_solve_q5(AllPeriods, NextId, Parent,
        SubPeriodElems, SubPeriodValues, PeriodValues, P) = Result :-
    Q = 5,

    % Numerical DFT to get d_s coefficients
    OmegaC = complex(math.cos(float.'/'(float.'*'(2.0, math.pi), 5.0)),
                     math.sin(float.'/'(float.'*'(2.0, math.pi), 5.0))),
    ResolventVals = list.map(
        ( func(J) = list.foldl(
            ( func(K, Acc) =
                cadd(Acc,
                    cmul(cpow(OmegaC, int.'mod'(int.'*'(J, K), Q)),
                         list.det_index0(SubPeriodValues, K)))
            ), 0 `..` int.'-'(Q, 1), complex(0.0, 0.0))
        ), 0 `..` int.'-'(Q, 1)),
    ResolventPowers = list.map(
        ( func(RV) = cpow(RV, Q) ), ResolventVals),
    DCoeffsNum = list.map(
        ( func(S) =
            cscale(0.2,
                list.foldl(
                    ( func(J, Acc) =
                        cadd(Acc,
                            cmul(cpow(OmegaC,
                                    int.'mod'(int.'-'(0, int.'*'(J, S)), Q)),
                                 list.det_index0(ResolventPowers, J)))
                    ), 0 `..` int.'-'(Q, 1), complex(0.0, 0.0)))
        ), 0 `..` int.'-'(Q, 1)),
    DCoeffs = list.map(
        match_to_tower_elem(AllPeriods, PeriodValues, P),
        DCoeffsNum),

    % Build ω₅ as a tower element:
    %   √5, √(10 + 2√5), √(-1)
    {_LvlS5, Sqrt5} = adjoin_tower_root(NextId, 2, t_from_int(5)),
    Cos2pi5 = t_div(t_sub(Sqrt5, t_from_int(1)), t_from_int(4)),
    SinRadicand = t_add(t_from_int(10), t_mul(t_from_int(2), Sqrt5)),
    {_LvlSR, SqrtSinRad} = adjoin_tower_root(int.'+'(NextId, 1), 2, SinRadicand),
    Sin2pi5 = t_div(SqrtSinRad, t_from_int(4)),
    {_LvlI, IUnit} = adjoin_tower_root(int.'+'(NextId, 2), 2,
        t_rat(rational.'-'(rational.zero, rational.one))),
    OmegaTE = t_add(Cos2pi5, t_mul(IUnit, Sin2pi5)),

    % Powers ω₅^k, k = 0..4
    OmPows = omega_powers(OmegaTE, Q),

    % R_j^5 = Σ_s d_s · ω₅^{js} as tower elements
    Rjq = list.map(
        ( func(J) = list.foldl(
            ( func(S, Acc) =
                t_add(Acc,
                    t_mul(list.det_index0(DCoeffs, S),
                          list.det_index0(OmPows,
                              int.'mod'(int.'*'(J, S), Q))))
            ), 0 `..` int.'-'(Q, 1), t_from_int(0))
        ), 0 `..` int.'-'(Q, 1)),

    % R₀ = parent (known); adjoin ⁵√(R_j^5) for j = 1..4
    R0 = tp_elem(Parent),

    % Adjoin and select branches for j = 1..4
    {R1, NId1} = adjoin_and_select(int.'+'(NextId, 3), 1, Q, Rjq,
        OmPows, ResolventVals, P),
    {R2, NId2} = adjoin_and_select(NId1, 2, Q, Rjq,
        OmPows, ResolventVals, P),
    {R3, NId3} = adjoin_and_select(NId2, 3, Q, Rjq,
        OmPows, ResolventVals, P),
    {R4, NId4} = adjoin_and_select(NId3, 4, Q, Rjq,
        OmPows, ResolventVals, P),

    Rjs = [R0, R1, R2, R3, R4],

    % Inverse DFT: η_k = (1/5) Σ_j ω₅^{-jk} · R_j
    Fifth = t_rat(rational(1, 5)),
    SubPeriodTower = list.map(
        ( func(K) =
            t_mul(Fifth,
                list.foldl(
                    ( func(J, Acc) =
                        t_add(Acc,
                            t_mul(list.det_index0(OmPows,
                                      int.'mod'(int.'-'(0, int.'*'(J, K)), Q)),
                                  list.det_index0(Rjs, J)))
                    ), 0 `..` int.'-'(Q, 1), t_from_int(0)))
        ), 0 `..` int.'-'(Q, 1)),

    Assigned = assign_tower_by_value(SubPeriodTower, SubPeriodValues, P),
    Result = {build_tp_states(Assigned, SubPeriodElems, P), NId4}.

%---------------------------------------------------------------------------%
% Adjoin ⁿ√(R_j^n) with branch selection
%---------------------------------------------------------------------------%

:- func adjoin_and_select(int, int, int, list(tower_elem),
    list(tower_elem), list(complex), integer) = {tower_elem, int}.

adjoin_and_select(NId, J, Q, Rjq, OmPows, ResolventVals, P) = {Best, int.'+'(NId, 1)} :-
    Radicand = list.det_index0(Rjq, J),
    {_, Alpha} = adjoin_tower_root(NId, Q, Radicand),
    % Five candidate roots: ω₅^m · α for m = 0..4
    Candidates = list.map(
        ( func(M) = t_mul(list.det_index0(OmPows, M), Alpha) ),
        0 `..` int.'-'(Q, 1)),
    Target = list.det_index0(ResolventVals, J),
    CandVals = list.map(
        ( func(C) = eval_tower_approx(C, P) ), Candidates),
    Best = pick_closest_tower(Candidates, CandVals, Target).

%---------------------------------------------------------------------------%
% Omega powers
%---------------------------------------------------------------------------%

:- func omega_powers(tower_elem, int) = list(tower_elem).

omega_powers(Omega, Q) = omega_powers_loop(Omega, Q, 0, t_from_int(1), []).

:- func omega_powers_loop(tower_elem, int, int, tower_elem,
    list(tower_elem)) = list(tower_elem).

omega_powers_loop(Omega, Q, K, Cur, Acc) = Result :-
    ( if int.'>='(K, Q) then
        Result = list.reverse(Acc)
    else
        Result = omega_powers_loop(Omega, Q, int.'+'(K, 1),
            t_mul(Cur, Omega), [Cur | Acc])
    ).

%---------------------------------------------------------------------------%
% Matching and assignment
%---------------------------------------------------------------------------%

    % Match a numerical Complex Double value to an integer linear
    % combination of period tower elements.
    %
:- func match_to_tower_elem(list(tp_state), list(complex),
    integer, complex) = tower_elem.

match_to_tower_elem(Periods, PeriodValues, _P, Target) = Result :-
    ReT = re(Target) `with_type` float,
    NearestInt = float.round_to_int(ReT),
    NearIntC = complex(float.float(NearestInt), 0.0),
    Dist = cmag(csub(Target, NearIntC)) `with_type` float,
    ( if float.'<'(Dist, 1.0e-8) then
        Result = t_rat(rational(NearestInt))
    else
        % Try single-period matching first
        ( if try_single_period_match(Periods, PeriodValues, Target, 0,
              Match)
        then
            Result = Match
        else
            % Fall back to multi-period matching
            TRat = {float_to_rational(re(Target)),
                    float_to_rational(im(Target))}
                `with_type` {rational, rational},
            VRat = list.map(
                ( func(V) =
                    {float_to_rational(re(V)),
                     float_to_rational(im(V))}
                    `with_type` {rational, rational}
                ),
                PeriodValues),
            ( if solve_linear_integer_c(TRat, VRat) = yes({C, Coeffs}) then
                Result = build_linear_combo(Periods, C, Coeffs, 0)
            else
                % Last resort: rational approximation
                Result = t_rat(float_to_rational(re(Target)))
            )
        )
    ).

:- pred try_single_period_match(list(tp_state)::in, list(complex)::in,
    complex::in, int::in, tower_elem::out) is semidet.

try_single_period_match(Periods, PeriodValues, Target, I, Match) :-
    int.'<'(I, list.length(PeriodValues)),
    PV = list.det_index0(PeriodValues, I),
    ( if match_single_period(Target, PV) = yes({C, A}) then
        Err = cmag(csub(Target,
            cadd(complex(float.float(C), 0.0),
                 cscale(float.float(A), PV)))) `with_type` float,
        TgtMag = cmag(Target) `with_type` float,
        RelTol = float.'/'(Err, float.max(1.0, TgtMag)),
        float.'<'(RelTol, 1.0e-6),
        Match = t_add(t_rat(rational(C)),
                       t_mul(t_rat(rational(A)),
                             tp_elem(list.det_index0(Periods, I))))
    else
        try_single_period_match(Periods, PeriodValues, Target,
            int.'+'(I, 1), Match)
    ).

:- func match_single_period(complex, complex) = maybe({int, int}).

match_single_period(Target, V) = Result :-
    ImV = im(V) `with_type` float,
    ReV = re(V) `with_type` float,
    ImT = im(Target) `with_type` float,
    ReT = re(Target) `with_type` float,
    ( if float.'>'(float.abs(ImV), 1.0e-10) then
        A = float.round_to_int(float.'/'(ImT, ImV))
    else if float.'>'(float.abs(ReV), 1.0e-10) then
        A = float.round_to_int(float.'/'(ReT, ReV))
    else
        A = 0
    ),
    AF = float.float(A) `with_type` float,
    Remainder = csub(Target, cscale(AF, V)),
    C = float.round_to_int(re(Remainder)),
    CF = float.float(C) `with_type` float,
    Recon = cadd(complex(CF, 0.0), cscale(AF, V)),
    Err = cmag(csub(Recon, Target)) `with_type` float,
    TgtMag = cmag(Target) `with_type` float,
    RelErr = float.'/'(Err, float.max(1.0, TgtMag)),
    ( if float.'<'(RelErr, 1.0e-6) then
        Result = yes({C, A})
    else
        Result = no
    ).

:- func build_linear_combo(list(tp_state), int, list(int), int) =
    tower_elem.

build_linear_combo(Periods, C, Coeffs, I) = Result :-
    ( if Coeffs = [] then
        Result = t_rat(rational(C))
    else if Coeffs = [A | Rest] then
        RestResult = build_linear_combo(Periods, C, Rest, int.'+'(I, 1)),
        ( if A = 0 then
            Result = RestResult
        else
            Result = t_add(t_mul(t_rat(rational(A)),
                                 tp_elem(list.det_index0(Periods, I))),
                           RestResult)
        )
    else
        Result = t_rat(rational(C))
    ).

    % Assign tower expressions to numerical values by matching.
    %
:- func assign_tower_by_value(list(tower_elem), list(complex),
    integer) = list(tower_elem).

assign_tower_by_value(Exprs, Vals, P) = list.map(
    pick_closest_tower_expr(ExprVals), Vals) :-
    ExprVals = list.map(
        ( func(E) = {E, eval_tower_approx(E, P)} ),
        Exprs).

:- func pick_closest_tower_expr(list({tower_elem, complex})) =
    ( func(complex) = tower_elem ).

pick_closest_tower_expr(EVs) =
    ( func(Target) = Best :-
        ( if EVs = [H | T] then
            InitD = cmag(csub(snd(H), Target)) `with_type` float,
            pick_closest_loop(T, Target, fst(H), InitD, Best)
        else
            Best = t_from_int(0)
        )
    ).

:- func pick_closest_tower(list(tower_elem), list(complex), complex)
    = tower_elem.

pick_closest_tower(Candidates, CandVals, Target) = Best :-
    Pairs = zip_lists(Candidates, CandVals),
    ( if Pairs = [{H, HV} | T] then
        InitD = cmag(csub(HV, Target)) `with_type` float,
        pick_closest_loop(T, Target, H, InitD, Best)
    else
        Best = t_from_int(0)
    ).

:- pred pick_closest_loop(list({tower_elem, complex})::in, complex::in,
    tower_elem::in, float::in, tower_elem::out) is det.

pick_closest_loop([], _, BestE, _, BestE).
pick_closest_loop([{E, V} | Rest], Target, BestE, BestD, Result) :-
    D = cmag(csub(V, Target)) `with_type` float,
    ( if float.'<'(D, BestD) then
        pick_closest_loop(Rest, Target, E, D, Result)
    else
        pick_closest_loop(Rest, Target, BestE, BestD, Result)
    ).

%---------------------------------------------------------------------------%
% Numerical evaluation of tower elements
%---------------------------------------------------------------------------%

eval_tower_approx(E, P) = Result :-
    ( if E = t_rat(R) then
        Result = complex(rational_to_float(R), 0.0)
    else if E = t_ext(Cs, Level) then
        GenVal = eval_gen_approx(Level, P),
        Result = eval_horner(Cs, GenVal, P)
    else
        Result = complex(0.0, 0.0)
    ).

:- func eval_gen_approx(tower_level, integer) = complex.

eval_gen_approx(Level, P) = Result :-
    RVal = eval_tower_approx(tl_radicand(Level), P),
    N = tl_root_deg(Level),
    Mag = math.pow(cmag(RVal), float.'/'(1.0, float.float(N))),
    Phase = float.'/'(cphase(RVal), float.float(N)),
    Result = complex(float.'*'(Mag, math.cos(Phase)),
                     float.'*'(Mag, math.sin(Phase))).

:- func eval_horner(list(tower_elem), complex, integer) = complex.

eval_horner(Cs, GenVal, P) = Result :-
    % Evaluate polynomial in generator using Horner's method
    % Cs = [c₀, c₁, ..., c_{n-1}], value = c₀ + c₁·α + ... + c_{n-1}·α^{n-1}
    eval_horner_loop(Cs, GenVal, P, 0, complex(0.0, 0.0),
        complex(1.0, 0.0), Result).

:- pred eval_horner_loop(list(tower_elem)::in, complex::in, integer::in,
    int::in, complex::in, complex::in, complex::out) is det.

eval_horner_loop([], _, _, _, Acc, _, Acc).
eval_horner_loop([C | Rest], GenVal, P, I, Acc, GenPow, Result) :-
    CV = eval_tower_approx(C, P),
    NewAcc = cadd(Acc, cmul(CV, GenPow)),
    NewPow = cmul(GenPow, GenVal),
    eval_horner_loop(Rest, GenVal, P, int.'+'(I, 1),
        NewAcc, NewPow, Result).

%---------------------------------------------------------------------------%
% Helpers
%---------------------------------------------------------------------------%

:- func build_tp_states(list(tower_elem), list(list(integer)), integer)
    = list(tp_state).

build_tp_states(Towers, ElemLists, P) =
    list.map_corresponding(
        ( func(T, Es) = tp_state(T, Es, P) ),
        Towers, ElemLists).

:- func expand_steps(list({integer, int})) = list(int).

expand_steps([]) = [].
expand_steps([{Q, E} | Rest]) =
    list.append(
        list.duplicate(E, integer.det_to_int(Q)),
        expand_steps(Rest)).

:- func zip_lists(list(A), list(B)) = list({A, B}).

zip_lists(Xs, Ys) = Result :-
    ( if Xs = [A | As], Ys = [B | Bs] then
        Result = [{A, B} | zip_lists(As, Bs)]
    else
        Result = []
    ).

:- func fst({A, B}) = A.
fst({X, _}) = X.

:- func snd({A, B}) = B.
snd({_, Y}) = Y.

    % Complex number helpers.
    %
:- func cpow(complex, int) = complex.

cpow(Z, N) = Result :-
    ( if int.'=<'(N, 0) then
        Result = complex(1.0, 0.0)
    else if N = 1 then
        Result = Z
    else
        H = cpow(Z, int.'//'(N, 2)),
        H2 = cmul(H, H),
        ( if int.'mod'(N, 2) = 0 then
            Result = H2
        else
            Result = cmul(H2, Z)
        )
    ).

:- func cmag(complex) = float.
cmag(complex(A, B)) = math.sqrt(float.'+'(float.'*'(A, A),
                                           float.'*'(B, B))).

:- func cscale(float, complex) = complex.
cscale(S, complex(A, B)) = complex(float.'*'(S, A), float.'*'(S, B)).

:- func cphase(complex) = float.
cphase(complex(A, B)) = math.atan2(B, A).

:- func float_to_rational(float) = rational.
float_to_rational(F) = Result :-
    % Approximate: multiply by 10^9, round, reduce
    Scale = 1000000000,
    Num = float.round_to_int(float.'*'(F, float.float(Scale))),
    Result = rational(Num, Scale).

:- func rational_to_float(rational) = float.
rational_to_float(R) = Result :-
    NStr = integer.to_string(numer(R)),
    DStr = integer.to_string(denom(R)),
    ( if string.to_float(NStr, NF), string.to_float(DStr, DF) then
        Result = float.'/'(NF, DF)
    else
        Result = 0.0
    ).

%---------------------------------------------------------------------------%
:- end_module tower_descent.
%---------------------------------------------------------------------------%
