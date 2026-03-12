%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: trig_galois.m
% Main author: nyc
%
% Gauss period computation for expressing cos(2π/n) in radicals.
%
% Every root of unity can be expressed in radicals, since cyclotomic
% extensions have abelian (hence solvable) Galois groups.
%
% The algorithm descends through the subgroup chain of (Z/nZ)*,
% solving a period equation at each step. Each step introduces
% radicals of degree equal to the prime index of that step.
%
%---------------------------------------------------------------------------%

:- module trig_galois.
:- interface.

:- import_module integer.
:- import_module interval.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module prime_factors.
:- import_module rad_eval.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

:- type period_state
    --->    period_state(
                ps_expr     :: rad_expr(rational),
                ps_elems    :: list(integer),
                ps_p        :: integer
            ).

    % Compute cos(2π/n) as a radical expression via Gauss period descent.
    %
:- func cos_of_unity_via_gauss(int) = maybe(rad_expr(rational)).

    % Compute all primitive nth roots of unity as radical expressions.
    %
:- func all_periods_via_gauss(int) = maybe(map(int, rad_expr(rational))).

    % Compute Gauss periods for a prime p with e periods.
    %
:- func gauss_periods(integer, int) = list(list(integer)).

    % Find a primitive root modulo a prime p.
    %
:- func primitive_root(integer) = maybe(integer).

    % Compute the subgroup chain of (Z/pZ)*.
    %
:- func subgroup_chain(integer) = list(list(integer)).

    % Modular exponentiation: base^exp mod m.
    %
:- func mod_exp(integer, integer, integer) = integer.

    % Solve target = c + Σ aᵢ·xᵢ for integer c, aᵢ.
    %
:- func solve_linear_integer_c({rational, rational},
    list({rational, rational})) = maybe({int, list(int)}).

    % Chebyshev polynomial T_k(x).
    %
:- func chebyshev(int, rad_expr(rational)) = rad_expr(rational).

    % Euler's totient function.
    %
:- func euler_totient(integer) = integer.

    % Primitive root modulo n (supports primes, odd prime powers,
    % and 2 × odd prime power).
    %
:- func primitive_root_mod(integer) = maybe(integer).

    % Reorder prime factorization steps for prime power moduli.
    %
:- func reorder_factors(integer, list(prime_power)) = list({integer, int}).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- use_module float.
:- use_module int.
:- use_module math.
:- import_module pair.
:- import_module positive.
:- import_module rad_dag.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
% DAG-aware helpers
%---------------------------------------------------------------------------%

:- func dag_fold(rad_expr(rational)) = rad_expr(rational).

dag_fold(E) = from_dag(dag_fold_constants(to_dag(E))).

:- func dag_eval(rad_expr(rational)) = complex.

dag_eval(E) = eval_complex(E).

%---------------------------------------------------------------------------%
% Chebyshev polynomial
%---------------------------------------------------------------------------%

chebyshev(K, X) = Result :-
    ( if int.'=<'(K, 0) then
        Result = re_lit(rational.one)
    else if K = 1 then
        Result = X
    else
        Result = chebyshev_loop(2, K, X, re_lit(rational.one), X)
    ).

:- func chebyshev_loop(int, int, rad_expr(rational),
    rad_expr(rational), rad_expr(rational)) = rad_expr(rational).

chebyshev_loop(N, K, X, T0, T1) = Result :-
    ( if int.'>'(N, K) then
        Result = T1
    else
        T2 = re_add(re_mul(re_mul(re_lit(rational(2)), X), T1),
                     re_neg(T0)),
        Result = chebyshev_loop(int.'+'(N, 1), K, X, T1, T2)
    ).

%---------------------------------------------------------------------------%
% cos(2π/n) via Gauss period descent
%---------------------------------------------------------------------------%

cos_of_unity_via_gauss(N) = Result :-
    ( if MaybePeriods = all_periods_via_gauss(N),
         MaybePeriods = yes(Periods),
         map.search(Periods, 1, Target),
         ConjElem = int.'-'(N, 1),
         map.search(Periods, ConjElem, Conjugate) then
        Result = yes(re_mul(re_inv(re_lit(rational(2))),
                            re_add(Target, Conjugate)))
    else
        Result = no
    ).

%---------------------------------------------------------------------------%
% All periods via Gauss period descent
%---------------------------------------------------------------------------%

all_periods_via_gauss(N) = Result :-
    ( if int.'=<'(N, 2) then
        Result = no
    else
        NI = integer(N),
        ( if primitive_root_mod(NI) = yes(G) then
            Phi = euler_totient(NI),
            Fs = factorise(det_positive(Phi)),
            Steps = reorder_factors(NI, Fs),
            StepList = expand_steps(Steps),
            CoprimeElems = coprime_elems_list(G, Phi, NI),
            InitSum = sum_roots_of_unity_real(NI, CoprimeElems),
            InitSumR = round_to_integer(InitSum),
            InitExpr = re_lit(from_integers(InitSumR, integer.one)),
            InitPeriod = period_state(InitExpr, CoprimeElems, NI),
            FinalPeriods = list.foldl(descend_step, StepList, [InitPeriod]),
            PeriodMap = build_period_map(FinalPeriods),
            Result = yes(PeriodMap)
        else
            Result = no
        )
    ).

:- func coprime_elems_list(integer, integer, integer) = list(integer).

coprime_elems_list(G, Phi, N) =
    list.map(
        ( func(K) = mod_exp(G, integer(K), N) ),
        0 `..` int.'-'(integer.det_to_int(Phi), 1)).

:- func sum_roots_of_unity_real(integer, list(integer)) = float.

sum_roots_of_unity_real(N, Ks) =
    list.foldl(
        ( func(K, Acc) = float.'+'(Acc,
            math.cos(float.'/'(float.'*'(float.'*'(2.0, math.pi),
                               float.float(integer.det_to_int(K))),
                     float.float(integer.det_to_int(N))))) ),
        Ks, 0.0).

:- func round_to_integer(float) = integer.

round_to_integer(X) = integer(round_to_int(X)).

:- func round_to_int(float) = int.

round_to_int(X) =
    ( if float.'>='(X, 0.0) then
        float.truncate_to_int(float.'+'(X, 0.5))
    else
        int.'-'(0, float.truncate_to_int(float.'+'(float.'-'(0.0, X), 0.5)))
    ).

:- func expand_steps(list({integer, int})) = list(int).

expand_steps([]) = [].
expand_steps([{Q, E} | Rest]) =
    list.append(
        list.duplicate(E, integer.det_to_int(Q)),
        expand_steps(Rest)).

:- func build_period_map(list(period_state)) = map(int, rad_expr(rational)).

build_period_map(Periods) =
    list.foldl(
        ( func(PS, M0) =
            list.foldl(
                ( func(K, M1) =
                    map.set(M1, integer.det_to_int(K), ps_expr(PS)) ),
                ps_elems(PS), M0)
        ), Periods, map.init).

%---------------------------------------------------------------------------%
% Descent step: split each period into q sub-periods
%---------------------------------------------------------------------------%

:- func descend_step(int, list(period_state)) = list(period_state).

descend_step(Q, Periods) =
    list.condense(
        list.map(
            ( func(P) = split_period(Periods, Q, P) ),
            Periods)).

:- func split_period(list(period_state), int, period_state)
    = list(period_state).

split_period(AllPeriods, Q, Parent) = Result :-
    P = ps_p(Parent),
    Elems = ps_elems(Parent),
    F = list.length(Elems),
    SubF = int.'/'(F, Q),
    SubPeriodElems = list.map(
        ( func(K) = list.map(
            ( func(J) = list.det_index0(Elems,
                int.'+'(K, int.'*'(Q, J))) ),
            0 `..` int.'-'(SubF, 1)) ),
        0 `..` int.'-'(Q, 1)),
    SubPeriodValues = list.map(sum_roots_of_unity_c(P), SubPeriodElems),
    ( if int.'=<'(Q, 3) then
        SymFuncs = elementary_symmetric_c(SubPeriodValues),
        CoeffExprs = list.map(
            match_to_period_expr(AllPeriods, P), SymFuncs),
        SubPeriodExprs = solve_period_equation(Q, ps_expr(Parent),
                                               CoeffExprs, SubPeriodValues)
    else
        SubPeriodExprs = solve_period_via_resolvent(Q, AllPeriods, P,
            ps_expr(Parent), SubPeriodElems, SubPeriodValues)
    ),
    Result = build_sub_periods(SubPeriodExprs, SubPeriodElems, P).

:- func build_sub_periods(list(rad_expr(rational)),
    list(list(integer)), integer) = list(period_state).

build_sub_periods(Exprs, ElemsList, P) = Result :-
    ( if Exprs = [E | Es], ElemsList = [Elms | Rest] then
        Result = [period_state(E, Elms, P) |
                  build_sub_periods(Es, Rest, P)]
    else
        Result = []
    ).

%---------------------------------------------------------------------------%
% Sum of roots of unity (complex)
%---------------------------------------------------------------------------%

:- func sum_roots_of_unity_c(integer, list(integer)) = complex.

sum_roots_of_unity_c(N, Ks) =
    list.foldl(
        ( func(K, Acc) =
            cadd(Acc, complex(
                math.cos(Ang), math.sin(Ang))) :-
                Ang = float.'/'(
                    float.'*'(float.'*'(2.0, math.pi),
                              float.float(integer.det_to_int(K))),
                    float.float(integer.det_to_int(N)))
        ), Ks, complex(0.0, 0.0)).

%---------------------------------------------------------------------------%
% Elementary symmetric functions (complex)
%---------------------------------------------------------------------------%

:- func elementary_symmetric_c(list(complex)) = list(complex).

elementary_symmetric_c(Xs) =
    list.map(
        ( func(K) = elem_sym(K, Xs) ),
        1 `..` list.length(Xs)).

:- func elem_sym(int, list(complex)) = complex.

elem_sym(K, Xs) = Result :-
    ( if K = 1 then
        Result = list.foldl(cadd, Xs, complex(0.0, 0.0))
    else
        Result = list.foldl(
            ( func(Combo, Acc) = cadd(Acc, combo_product(Combo)) ),
            choose(K, Xs), complex(0.0, 0.0))
    ).

:- func combo_product(list(complex)) = complex.

combo_product(Xs) = list.foldl(cmul, Xs, complex(1.0, 0.0)).

:- func choose(int, list(complex)) = list(list(complex)).

choose(K, Ys) = Result :-
    ( if int.'=<'(K, 0) then
        Result = [[]]
    else
        ( if Ys = [] then
            Result = []
        else if Ys = [Y | Ys1] then
            Result = list.append(
                list.map(( func(C) = [Y | C] ),
                         choose(int.'-'(K, 1), Ys1)),
                choose(K, Ys1))
        else
            Result = []
        )
    ).

%---------------------------------------------------------------------------%
% Period expression matching
%---------------------------------------------------------------------------%

:- func try_match_to_period_expr(list(period_state), integer, complex)
    = maybe(rad_expr(rational)).

try_match_to_period_expr(Periods, P, Target) = Result :-
    NearestInt = round_to_int(re(Target)),
    NearestF = float.float(NearestInt),
    ( if float.'<'(cmag(csub(Target, complex(NearestF, 0.0))), 1.0e-8) then
        Result = yes(re_lit(rational(NearestInt)))
    else
        PeriodVals = list.map(
            ( func(PS) = sum_roots_of_unity_c(P, ps_elems(PS)) ),
            Periods),
        SingleMatches = find_single_matches(Target, PeriodVals, Periods, 0),
        ( if SingleMatches = [Best | _] then
            Result = yes(Best)
        else
            TargetR = {float_to_rat(re(Target)),
                       float_to_rat(im(Target))},
            ValsR = list.map(
                ( func(V) = {float_to_rat(re(V)), float_to_rat(im(V))} ),
                PeriodVals),
            ( if solve_linear_integer_c(TargetR, ValsR)
                   = yes({C, As}) then
                Expr = build_combo_expr(C, As, Periods),
                ErrC = recon_error_c(C, As, PeriodVals, Target),
                RelErr = float.'/'(ErrC,
                    float.max(1.0, cmag(Target))),
                ( if float.'<'(RelErr, 1.0e-6) then
                    Result = yes(Expr)
                else
                    Result = no
                )
            else
                Result = no
            )
        )
    ).

:- func find_single_matches(complex, list(complex),
    list(period_state), int) = list(rad_expr(rational)).

find_single_matches(_, [], _, _) = [].
find_single_matches(Target, [PV | PVs], Periods, I) = Result :-
    Rest = find_single_matches(Target, PVs, Periods, int.'+'(I, 1)),
    ( if match_single_period(Target, PV) = yes({C, A}),
         Expr = build_single_expr(C, A, I, Periods),
         ErrC = recon_error_c(C, [A], [PV], Target),
         RelErr = float.'/'(ErrC, float.max(1.0, cmag(Target))),
         float.'<'(RelErr, 1.0e-6) then
        Result = [Expr | Rest]
    else
        Result = Rest
    ).

:- func match_single_period(complex, complex) = maybe({int, int}).

match_single_period(Target, V) = Result :-
    ( if float.'>'(float.abs(im(V)), 1.0e-10) then
        A = round_to_int(float.'/'(im(Target), im(V)))
    else if float.'>'(float.abs(re(V)), 1.0e-10) then
        A = round_to_int(float.'/'(re(Target), re(V)))
    else
        A = 0
    ),
    AF = float.float(A),
    Remainder = csub(Target, cmul(complex(AF, 0.0), V)),
    C = round_to_int(re(Remainder)),
    CF = float.float(C),
    Recon = cadd(complex(CF, 0.0), cmul(complex(AF, 0.0), V)),
    Err = cmag(csub(Recon, Target)),
    RelErr = float.'/'(Err, float.max(1.0, cmag(Target))),
    ( if float.'<'(RelErr, 1.0e-6) then
        Result = yes({C, A})
    else
        Result = no
    ).

:- func build_single_expr(int, int, int, list(period_state))
    = rad_expr(rational).

build_single_expr(C, A, I, Periods) = Result :-
    NP = list.length(Periods),
    Coeffs = list.map(
        ( func(J) = ( if J = I then A else 0 ) ),
        0 `..` int.'-'(NP, 1)),
    Result = build_combo_expr(C, Coeffs, Periods).

:- func build_combo_expr(int, list(int), list(period_state))
    = rad_expr(rational).

build_combo_expr(C, Coeffs, Periods) = Result :-
    build_combo_loop(Coeffs, Periods, re_lit(rational(C)), Result).

:- pred build_combo_loop(list(int)::in, list(period_state)::in,
    rad_expr(rational)::in, rad_expr(rational)::out) is det.

build_combo_loop([], _, Acc, Acc).
build_combo_loop([_ | _], [], Acc, Acc).
build_combo_loop([A | As], [PS | PSs], Acc0, Result) :-
    ( if A = 0 then
        Acc1 = Acc0
    else if A = 1 then
        Acc1 = re_add(Acc0, ps_expr(PS))
    else
        Acc1 = re_add(Acc0, re_mul(re_lit(rational(A)), ps_expr(PS)))
    ),
    build_combo_loop(As, PSs, Acc1, Result).

:- func recon_error_c(int, list(int), list(complex), complex) = float.

recon_error_c(C, As, PVs, Target) = Result :-
    recon_error_loop(As, PVs, complex(float.float(C), 0.0), Sum),
    Result = cmag(csub(Target, Sum)).

:- pred recon_error_loop(list(int)::in, list(complex)::in,
    complex::in, complex::out) is det.

recon_error_loop([], _, Acc, Acc).
recon_error_loop([_ | _], [], Acc, Acc).
recon_error_loop([A | As], [PV | PVs], Acc0, Result) :-
    Acc1 = cadd(Acc0, cmul(complex(float.float(A), 0.0), PV)),
    recon_error_loop(As, PVs, Acc1, Result).

:- func cmag(complex) = float.

cmag(complex(A, B)) =
    math.sqrt(float.'+'(float.'*'(A, A), float.'*'(B, B))).

:- func match_to_period_expr(list(period_state), integer, complex)
    = rad_expr(rational).

match_to_period_expr(Periods, P, Target) = Result :-
    ( if try_match_to_period_expr(Periods, P, Target) = yes(Expr) then
        Result = Expr
    else
        Re = float_to_rat(re(Target)),
        Im = float_to_rat(im(Target)),
        ( if float.'<'(float.abs(im(Target)), 1.0e-12) then
            Result = re_lit(Re)
        else
            Result = re_add(re_lit(Re),
                re_mul(re_lit(Im), re_root(2, re_lit(rational.'-'(
                    rational.zero, rational.one)))))
        )
    ).

%---------------------------------------------------------------------------%
% Integer linear combination solver (Rational arithmetic)
%---------------------------------------------------------------------------%

solve_linear_integer_c(Target, Vals) = Result :-
    Target = {TRe, TIm},
    ( if Vals = [] then
        C = round_rational(TRe),
        ErrReF = float.'-'(rational_to_float(TRe), float.float(C)),
        ErrImF = rational_to_float(TIm),
        ErrSqF = float.'+'(float.'*'(ErrReF, ErrReF),
                            float.'*'(ErrImF, ErrImF)),
        TReF0 = rational_to_float(TRe),
        TImF0 = rational_to_float(TIm),
        MagSqF = float.max(1.0, float.'+'(float.'*'(TReF0, TReF0),
                                            float.'*'(TImF0, TImF0))),
        ( if float.'<'(ErrSqF, float.'*'(MagSqF, 1.0e-12)) then
            Result = yes({C, []})
        else
            Result = no
        )
    else
        N = list.length(Vals),
        Indexed = index_list(Vals, 0),
        separate_im_heavy(Indexed, [], [], ImHeavy0, RealOnly0),
        list.reverse(ImHeavy0, ImHeavy1),
        list.reverse(RealOnly0, RealOnly),
        SortedIm = list.sort(
            ( func({_, {_, Im1}}, {_, {_, Im2}}) = R :-
                A1 = rational_abs(Im1),
                A2 = rational_abs(Im2),
                ( if rat_lt(A2, A1) then R = (<)
                else if rat_lt(A1, A2) then R = (>)
                else R = (=) )
            ), ImHeavy1),
        TReF = rational_to_float(TRe),
        TImF = rational_to_float(TIm),
        greedy_assign(SortedIm, TReF, TImF, [], ResRe1, ResIm1, Coeffs1),
        greedy_assign_real(RealOnly, ResRe1, ResIm1, Coeffs1,
                           ResRe2, _ResIm2, Coeffs2),
        C = float.round_to_int(ResRe2),
        AllCoeffs = list.sort(
            ( func({I1, _}, {I2, _}) = ordering(I1, I2) ),
            Coeffs2),
        FinalCoeffs = list.map(snd, AllCoeffs),
        verify_combo(C, FinalCoeffs, Vals, TRe, TIm, N, Verified),
        ( if Verified = yes then
            Result = yes({C, FinalCoeffs})
        else
            Result = try_perturbation(TRe, TIm, Vals, N, C, FinalCoeffs)
        )
    ).

:- func very_small_rational = rational.
very_small_rational = from_integers(integer.one, integer(1000000000000)).

:- pred separate_im_heavy(list({int, {rational, rational}})::in,
    list({int, {rational, rational}})::in,
    list({int, {rational, rational}})::in,
    list({int, {rational, rational}})::out,
    list({int, {rational, rational}})::out) is det.

separate_im_heavy([], IH, RO, IH, RO).
separate_im_heavy([IV | Rest], IH0, RO0, IH, RO) :-
    IV = {_, {_, Im}},
    ( if rat_lt(very_small_rational, rational_abs(Im)) then
        separate_im_heavy(Rest, [IV | IH0], RO0, IH, RO)
    else
        separate_im_heavy(Rest, IH0, [IV | RO0], IH, RO)
    ).

:- pred greedy_assign(list({int, {rational, rational}})::in,
    float::in, float::in,
    list({int, int})::in,
    float::out, float::out,
    list({int, int})::out) is det.

greedy_assign([], RRe, RIm, Cs, RRe, RIm, Cs).
greedy_assign([{I, {VRe, VIm}} | Rest], RRe0, RIm0, Cs0,
              RRe, RIm, Cs) :-
    VImF = rational_to_float(VIm),
    VReF = rational_to_float(VRe),
    A = float.round_to_int(float.'/'(RIm0, VImF)),
    AF = float.float(A),
    RRe1 = float.'-'(RRe0, float.'*'(AF, VReF)),
    RIm1 = float.'-'(RIm0, float.'*'(AF, VImF)),
    greedy_assign(Rest, RRe1, RIm1, [{I, A} | Cs0], RRe, RIm, Cs).

:- pred greedy_assign_real(list({int, {rational, rational}})::in,
    float::in, float::in,
    list({int, int})::in,
    float::out, float::out,
    list({int, int})::out) is det.

greedy_assign_real([], RRe, RIm, Cs, RRe, RIm, Cs).
greedy_assign_real([{I, {VRe, _VIm}} | Rest], RRe0, RIm0, Cs0,
                   RRe, RIm, Cs) :-
    VReF = rational_to_float(VRe),
    ( if float.'>'(float.abs(VReF), 1.0e-12) then
        A = float.round_to_int(float.'/'(RRe0, VReF)),
        AF = float.float(A),
        RRe1 = float.'-'(RRe0, float.'*'(AF, VReF))
    else
        A = 0,
        RRe1 = RRe0
    ),
    greedy_assign_real(Rest, RRe1, RIm0, [{I, A} | Cs0],
                       RRe, RIm, Cs).

:- pred verify_combo(int::in, list(int)::in,
    list({rational, rational})::in,
    rational::in, rational::in, int::in, bool::out) is det.

verify_combo(C, Coeffs, Vals, TRe, TIm, N, Result) :-
    recon_float(C, Coeffs, Vals, ReconReF, ReconImF),
    DRe = float.'-'(ReconReF, rational_to_float(TRe)),
    DIm = float.'-'(ReconImF, rational_to_float(TIm)),
    ErrSq = float.'+'(float.'*'(DRe, DRe), float.'*'(DIm, DIm)),
    TReF = rational_to_float(TRe),
    TImF = rational_to_float(TIm),
    MagSq = float.max(1.0, float.'+'(float.'*'(TReF, TReF),
                                       float.'*'(TImF, TImF))),
    Tol = 1.0e-12,
    CoeffBound = int.'*'(10000, int.max(1, N)),
    ( if float.'<'(ErrSq, float.'*'(MagSq, Tol)),
         coeffs_ok(C, Coeffs, CoeffBound) then
        Result = yes
    else
        Result = no
    ).

:- pred recon_float(int::in, list(int)::in,
    list({rational, rational})::in,
    float::out, float::out) is det.

recon_float(C, Coeffs, Vals, ReconRe, ReconIm) :-
    zip_sum_float(Coeffs, Vals, 0.0, 0.0, SumRe, SumIm),
    ReconRe = float.'+'(float.float(C), SumRe),
    ReconIm = SumIm.

:- pred zip_sum_float(list(int)::in, list({rational, rational})::in,
    float::in, float::in, float::out, float::out) is det.

zip_sum_float([], _, Re, Im, Re, Im).
zip_sum_float([_ | _], [], Re, Im, Re, Im).
zip_sum_float([A | As], [{VRe, VIm} | Vs], Re0, Im0, Re, Im) :-
    AF = float.float(A),
    Re1 = float.'+'(Re0, float.'*'(AF, rational_to_float(VRe))),
    Im1 = float.'+'(Im0, float.'*'(AF, rational_to_float(VIm))),
    zip_sum_float(As, Vs, Re1, Im1, Re, Im).

:- pred coeffs_ok(int::in, list(int)::in, int::in) is semidet.

coeffs_ok(C, Coeffs, Bound) :-
    int.'=<'(int.abs(C), int.'*'(Bound, Bound)),
    list.all_true(
        ( pred(A::in) is semidet :- int.'=<'(int.abs(A), Bound) ),
        Coeffs).

:- func try_perturbation(rational, rational,
    list({rational, rational}), int, int, list(int))
    = maybe({int, list(int)}).

try_perturbation(TRe, TIm, Vals, N, _C0, Coeffs0) = Result :-
    CoeffBound = int.'*'(10000, int.max(1, N)),
    try_perturb_loop(0, N, Coeffs0, Vals, TRe, TIm, CoeffBound, Result).

:- pred try_perturb_loop(int::in, int::in, list(int)::in,
    list({rational, rational})::in,
    rational::in, rational::in, int::in,
    maybe({int, list(int)})::out) is det.

try_perturb_loop(I, N, Coeffs0, Vals, TRe, TIm, CoeffBound, Result) :-
    ( if int.'>='(I, N) then
        Result = no
    else
        ( if try_one_perturb(I, -1, Coeffs0, Vals, TRe, TIm,
                             N, CoeffBound) = yes(R) then
            Result = yes(R)
        else if try_one_perturb(I, 1, Coeffs0, Vals, TRe, TIm,
                                N, CoeffBound) = yes(R) then
            Result = yes(R)
        else
            try_perturb_loop(int.'+'(I, 1), N, Coeffs0, Vals,
                             TRe, TIm, CoeffBound, Result)
        )
    ).

:- func try_one_perturb(int, int, list(int),
    list({rational, rational}),
    rational, rational, int, int)
    = maybe({int, list(int)}).

try_one_perturb(I, Delta, Coeffs0, Vals, TRe, TIm, N, _CoeffBound)
        = Result :-
    NewCoeffs = perturb_coeffs(Coeffs0, I, Delta, 0),
    zip_sum_float(NewCoeffs, Vals, 0.0, 0.0, SumRe, _SumIm),
    ResReF = float.'-'(rational_to_float(TRe), SumRe),
    C = float.round_to_int(ResReF),
    verify_combo(C, NewCoeffs, Vals, TRe, TIm, N, OK),
    ( if OK = yes then
        Result = yes({C, NewCoeffs})
    else
        Result = no
    ).

:- func perturb_coeffs(list(int), int, int, int) = list(int).

perturb_coeffs([], _, _, _) = [].
perturb_coeffs([A | As], I, Delta, J) =
    [( if J = I then int.'+'(A, Delta) else A ) |
     perturb_coeffs(As, I, Delta, int.'+'(J, 1))].

%---------------------------------------------------------------------------%
% Rational helper functions
%---------------------------------------------------------------------------%

    % Cross-multiplication comparison: a/b < c/d iff a*d < c*b
    % (assuming positive denominators). Avoids rational subtraction
    % which triggers an integer normalization bug for large denominators.
    %
:- pred rat_lt(rational::in, rational::in) is semidet.

rat_lt(A, B) :-
    NA = numer(A),
    DA = denom(A),
    NB = numer(B),
    DB = denom(B),
    integer.'<'(integer.'*'(NA, DB), integer.'*'(NB, DA)).

:- func from_int(int) = rational.
from_int(N) = rational(N).

:- func float_to_rat(float) = rational.
float_to_rat(X) = from_integers(
    integer(float.truncate_to_int(float.'*'(X, 1.0e15))),
    integer(1000000000000000)).

:- func round_rational(rational) = int.

round_rational(R) =
    round_to_int(rational_to_float(R)).

:- func rational_to_float(rational) = float.

rational_to_float(R) =
    float.'/'(
        float.float(integer.det_to_int(numer(R))),
        float.float(integer.det_to_int(denom(R)))).

:- func rational_abs(rational) = rational.

rational_abs(R) =
    ( if integer.'<'(numer(R), integer.zero) then
        from_integers(integer.'-'(integer.zero, numer(R)), denom(R))
    else
        R
    ).

:- func rational_max(rational, rational) = rational.

rational_max(A, B) =
    ( if rat_lt(B, A) then A
    else if rat_lt(A, B) then B
    else A ).

:- func index_list(list(T), int) = list({int, T}).

index_list([], _) = [].
index_list([X | Xs], I) = [{I, X} | index_list(Xs, int.'+'(I, 1))].

:- func snd({A, B}) = B.
snd({_, B}) = B.

%---------------------------------------------------------------------------%
% Period equation solving (q = 2, 3)
%---------------------------------------------------------------------------%

:- func solve_period_equation(int, rad_expr(rational),
    list(rad_expr(rational)), list(complex))
    = list(rad_expr(rational)).

solve_period_equation(Q, _E1, CoeffExprs, NumVals) = Result :-
    ( if Q = 2 then
        ( if CoeffExprs = [E1, E2] then
            Disc = re_add(re_mul(E1, E1),
                          re_neg(re_mul(re_lit(rational(4)), E2))),
            SqrtDisc = re_root(2, Disc),
            Root1 = re_mul(re_inv(re_lit(rational(2))),
                           re_add(E1, SqrtDisc)),
            Root2 = re_mul(re_inv(re_lit(rational(2))),
                           re_add(E1, re_neg(SqrtDisc))),
            Result = assign_by_value(
                list.map(dag_fold, [Root1, Root2]), NumVals)
        else
            unexpected($module, $pred,
                "quadratic needs exactly 2 coefficients")
        )
    else if Q = 3 then
        ( if CoeffExprs = [E1, E2, E3] then
            PExpr = re_add(E2, re_neg(re_mul(re_inv(re_lit(rational(3))),
                                              re_mul(E1, E1)))),
            QExpr = re_add(
                re_add(re_neg(E3),
                       re_mul(re_inv(re_lit(rational(3))),
                              re_mul(E1, E2))),
                re_neg(re_mul(
                    re_lit(from_integers(integer(2), integer(27))),
                    re_mul(E1, re_mul(E1, E1))))),
            Delta = re_add(
                re_mul(re_inv(re_lit(rational(4))),
                       re_mul(QExpr, QExpr)),
                re_mul(re_inv(re_lit(rational(27))),
                       re_mul(PExpr, re_mul(PExpr, PExpr)))),
            SqrtDelta = re_root(2, Delta),
            NegQHalf = re_mul(re_inv(re_lit(rational(-2))), QExpr),
            U1Arg = re_add(NegQHalf, SqrtDelta),
            U1 = re_root(3, U1Arg),
            U2 = re_mul(re_neg(PExpr),
                        re_inv(re_mul(re_lit(rational(3)), U1))),
            Shift = re_mul(re_inv(re_lit(rational(3))), E1),
            Omega = re_mul(re_inv(re_lit(rational(2))),
                re_add(re_lit(rational(-1)),
                       re_root(2, re_lit(rational(-3))))),
            OmegaBar = re_mul(re_inv(re_lit(rational(2))),
                re_add(re_lit(rational(-1)),
                       re_neg(re_root(2, re_lit(rational(-3)))))),
            R0 = re_add(re_add(U1, U2), Shift),
            R1 = re_add(re_add(re_mul(Omega, U1),
                                re_mul(OmegaBar, U2)), Shift),
            R2 = re_add(re_add(re_mul(OmegaBar, U1),
                                re_mul(Omega, U2)), Shift),
            Result = assign_by_value(
                list.map(dag_fold, [R0, R1, R2]), NumVals)
        else
            unexpected($module, $pred,
                "cubic needs exactly 3 coefficients")
        )
    else
        Result = list.map(
            ( func(V) = re_lit(float_to_rat(re(V))) ), NumVals)
    ).

%---------------------------------------------------------------------------%
% Assign expressions to values by closest match
%---------------------------------------------------------------------------%

:- func assign_by_value(list(rad_expr(rational)), list(complex))
    = list(rad_expr(rational)).

assign_by_value(Exprs, Vals) = Result :-
    ExprVals = list.map(
        ( func(E) = {E, dag_eval(E)} ), Exprs),
    Result = list.map(pick_closest(ExprVals), Vals).

:- func pick_closest(list({rad_expr(rational), complex}), complex)
    = rad_expr(rational).

pick_closest(EVs, Target) = Result :-
    ( if EVs = [{E0, V0} | Rest] then
        D0 = cmag(csub(V0, Target)),
        pick_closest_loop(Rest, Target, E0, D0, Result)
    else
        unexpected($module, $pred, "empty expression list")
    ).

:- pred pick_closest_loop(list({rad_expr(rational), complex})::in,
    complex::in, rad_expr(rational)::in, float::in,
    rad_expr(rational)::out) is det.

pick_closest_loop([], _, Best, _, Best).
pick_closest_loop([{E, V} | Rest], Target, Best0, BestD0, Best) :-
    D = cmag(csub(V, Target)),
    ( if float.'<'(D, BestD0) then
        pick_closest_loop(Rest, Target, E, D, Best)
    else
        pick_closest_loop(Rest, Target, Best0, BestD0, Best)
    ).

%---------------------------------------------------------------------------%
% Resolvent approach for q ≥ 5
%---------------------------------------------------------------------------%

:- func solve_period_via_resolvent(int, list(period_state), integer,
    rad_expr(rational), list(list(integer)), list(complex))
    = list(rad_expr(rational)).

solve_period_via_resolvent(Q, AllPeriods, P, ParentExpr,
                           SubPeriodElems, _NumVals) = Result :-
    OmegaQ = complex(
        math.cos(float.'/'(float.'*'(2.0, math.pi), float.float(Q))),
        math.sin(float.'/'(float.'*'(2.0, math.pi), float.float(Q)))),
    SubPeriodVals = list.map(sum_roots_of_unity_c(P), SubPeriodElems),
    % Compute R_j = Σ ω^{jk} η_k for j = 0..q-1
    ResolventVals = list.map(
        ( func(J) =
            compute_resolvent_val(OmegaQ, SubPeriodVals, J, Q)
        ), 0 `..` int.'-'(Q, 1)),
    % R_j^q values
    ResolventPowers = list.map(
        ( func(RV) = complex_pow(RV, Q) ), ResolventVals),
    % DFT of R_j^q: d_s = (1/q) Σ_j ω^{-js} R_j^q
    QF = float.float(Q),
    OmegaQInv = complex(re(OmegaQ), float.'-'(0.0, im(OmegaQ))),
    DCoeffs = list.map(
        ( func(S) = compute_dft_coeff(OmegaQInv, ResolventPowers, S, Q, QF)
        ), 0 `..` int.'-'(Q, 1)),
    % Period values as rational pairs (for matching)
    PeriodValsD = list.map(
        ( func(PS) = {float_to_rat(re(PV)), float_to_rat(im(PV))} :-
            PV = sum_roots_of_unity_c(P, ps_elems(PS))
        ), AllPeriods),
    % Match d_s to linear combinations of periods
    DExprs = list.map(
        match_d_coeff(DCoeffs, PeriodValsD, AllPeriods),
        0 `..` int.'-'(Q, 1)),
    % cos(2π/q) as RadExpr
    ( if cos_of_unity_via_gauss(Q) = yes(CosBase0) then
        CosBaseExpr = CosBase0
    else
        unexpected($module, $pred, "can't compute cos(2π/q) for resolvent")
    ),
    % ω^m as RadExpr for all m = 0..q-1
    OmegaPowers = list.map(omega_power_expr(Q, CosBaseExpr),
                           0 `..` int.'-'(Q, 1)),
    % R_j^q = Σ_s d_s · ω^{js} as RadExpr, for j = 1..q-1
    ResolventPowerExprs = list.map(
        ( func(J) =
            build_resolvent_power_expr(DExprs, OmegaPowers, J, Q)
        ), 1 `..` int.'-'(Q, 1)),
    % Select correct branch of q-th root
    ResolventExprs = list.map(
        ( func(J) =
            select_resolvent_branch(Q, OmegaPowers,
                list.det_index0(ResolventPowerExprs, int.'-'(J, 1)),
                list.det_index0(ResolventVals, J))
        ), 1 `..` int.'-'(Q, 1)),
    AllResolvents = [ParentExpr | ResolventExprs],
    % Recover sub-periods: η_k = (1/q) Σ_{j=0}^{q-1} ω^{-jk} · R_j
    Result = list.map(
        ( func(K) =
            build_sub_period_expr(OmegaPowers, AllResolvents, K, Q)
        ), 0 `..` int.'-'(Q, 1)).

:- func compute_resolvent_val(complex, list(complex), int, int) = complex.

compute_resolvent_val(OmegaQ, SubPeriodVals, J, Q) =
    compute_resolvent_val_loop(SubPeriodVals, OmegaQ, J, Q, 0,
                               complex(0.0, 0.0)).

:- func compute_resolvent_val_loop(list(complex), complex, int, int, int,
    complex) = complex.

compute_resolvent_val_loop([], _, _, _, _, Acc) = Acc.
compute_resolvent_val_loop([SV | SVs], OmegaQ, J, Q, K, Acc) =
    compute_resolvent_val_loop(SVs, OmegaQ, J, Q, int.'+'(K, 1),
        cadd(Acc, cmul(complex_pow(OmegaQ, int.'*'(J, K)), SV))).

:- func compute_dft_coeff(complex, list(complex), int, int, float)
    = {rational, rational}.

compute_dft_coeff(OmegaQInv, ResolventPowers, S, Q, QF) = Result :-
    DS = compute_dft_sum(ResolventPowers, OmegaQInv, S, Q, 0,
                         complex(0.0, 0.0)),
    Result = {float_to_rat(float.'/'(re(DS), QF)),
              float_to_rat(float.'/'(im(DS), QF))}.

:- func compute_dft_sum(list(complex), complex, int, int, int, complex)
    = complex.

compute_dft_sum([], _, _, _, _, Acc) = Acc.
compute_dft_sum([RP | RPs], OmegaQInv, S, Q, J, Acc) =
    compute_dft_sum(RPs, OmegaQInv, S, Q, int.'+'(J, 1),
        cadd(Acc, cmul(complex_pow(OmegaQInv, int.'*'(J, S)), RP))).

:- func match_d_coeff(list({rational, rational}),
    list({rational, rational}), list(period_state), int)
    = rad_expr(rational).

match_d_coeff(DCoeffs, PeriodValsD, AllPeriods, S) = Result :-
    DS = list.det_index0(DCoeffs, S),
    ( if solve_linear_integer_c(DS, PeriodValsD) = yes({C, Coeffs}) then
        Result = build_combo_expr(C, Coeffs, AllPeriods)
    else
        DS = {DSRe, DSIm},
        ( if rat_lt(rational_abs(DSIm), very_small_rational) then
            Result = re_lit(DSRe)
        else
            Result = re_add(re_lit(DSRe),
                re_mul(re_lit(DSIm),
                    re_root(2, re_lit(rational.'-'(
                        rational.zero, rational.one)))))
        )
    ).

:- func build_resolvent_power_expr(list(rad_expr(rational)),
    list(rad_expr(rational)), int, int) = rad_expr(rational).

build_resolvent_power_expr(DExprs, OmegaPowers, J, Q) =
    list.foldl(
        ( func(S, Acc) =
            re_add(Acc,
                re_mul(list.det_index0(DExprs, S),
                       list.det_index0(OmegaPowers,
                           int.'mod'(int.'*'(J, S), Q))))
        ), 0 `..` int.'-'(Q, 1),
        re_lit(rational.zero)).

:- func build_sub_period_expr(list(rad_expr(rational)),
    list(rad_expr(rational)), int, int) = rad_expr(rational).

build_sub_period_expr(OmegaPowers, AllResolvents, K, Q) =
    re_mul(re_inv(re_lit(rational(Q))),
        list.foldl(
            ( func(J, Acc) =
                re_add(Acc,
                    re_mul(
                        list.det_index0(OmegaPowers,
                            int.'mod'(int.'-'(Q,
                                int.'mod'(int.'*'(J, K), Q)), Q)),
                        list.det_index0(AllResolvents, J)))
            ), 0 `..` int.'-'(Q, 1),
            re_lit(rational.zero))).

%---------------------------------------------------------------------------%
% omega_power_expr: ω^m = cos(2πm/q) + i·sin(2πm/q)
%---------------------------------------------------------------------------%

:- func omega_power_expr(int, rad_expr(rational), int) = rad_expr(rational).

omega_power_expr(Q, CosBase, M) = Result :-
    M1 = int.'mod'(M, Q),
    ( if M1 = 0 then
        Result = re_lit(rational.one)
    else
        CosM = chebyshev(M1, CosBase),
        Sin2M = re_add(re_lit(rational.one),
                        re_neg(re_mul(CosM, CosM))),
        SinM = re_root(2, Sin2M),
        SinMSigned = ( if int.'<'(int.'*'(2, M1), Q) then SinM
                        else re_neg(SinM) ),
        I = re_root(2, re_lit(rational.'-'(rational.zero, rational.one))),
        Result = re_add(CosM, re_mul(I, SinMSigned))
    ).

%---------------------------------------------------------------------------%
% Branch selection for q-th root
%---------------------------------------------------------------------------%

:- func select_resolvent_branch(int, list(rad_expr(rational)),
    rad_expr(rational), complex) = rad_expr(rational).

select_resolvent_branch(Q, OmegaPowers, RjqExpr, TargetVal) = Result :-
    PrincipalRoot = re_root(Q, RjqExpr),
    RjqVal = dag_eval(RjqExpr),
    select_branch(Q, RjqVal, TargetVal, BestK, Confidence),
    ( if float.'>'(Confidence, 0.5) then
        RjqCI = dag_eval_complex_interval(to_dag(RjqExpr)),
        RjqHP = ci_to_complex(RjqCI),
        select_branch(Q, RjqHP, TargetVal, BestKHP, _),
        ( if BestKHP = 0 then
            Result = PrincipalRoot
        else
            Result = re_mul(list.det_index0(OmegaPowers, BestKHP),
                            PrincipalRoot)
        )
    else
        ( if BestK = 0 then
            Result = PrincipalRoot
        else
            Result = re_mul(list.det_index0(OmegaPowers, BestK),
                            PrincipalRoot)
        )
    ).

:- pred select_branch(int::in, complex::in, complex::in,
    int::out, float::out) is det.

select_branch(Q, RjqVal, TargetVal, BestK, Confidence) :-
    OmegaC = complex(
        math.cos(float.'/'(float.'*'(2.0, math.pi), float.float(Q))),
        math.sin(float.'/'(float.'*'(2.0, math.pi), float.float(Q)))),
    Mag = cmag(RjqVal),
    Phase = math.atan2(im(RjqVal), re(RjqVal)),
    ( if float.'<'(Mag, 1.0e-300) then
        PrinMag = 0.0
    else
        PrinMag = math.exp(float.'*'(float.'/'(1.0, float.float(Q)),
                                      math.ln(Mag)))
    ),
    PrinPhase = float.'/'(Phase, float.float(Q)),
    PrincipalVal = complex(
        float.'*'(PrinMag, math.cos(PrinPhase)),
        float.'*'(PrinMag, math.sin(PrinPhase))),
    Scored = list.map(
        ( func(K) = {K,
            cmag(csub(cmul(complex_pow(OmegaC, K), PrincipalVal),
                      TargetVal))}
        ), 0 `..` int.'-'(Q, 1)),
    SortedScored = list.sort(
        ( func({_, D1}, {_, D2}) = ordering(D1, D2) ),
        Scored),
    ( if SortedScored = [{BK, Best}, {_, Second} | _] then
        BestK = BK,
        Confidence = float.'/'(Best, float.max(1.0e-20, Second))
    else
        BestK = 0,
        Confidence = 0.0
    ).

:- func ci_to_complex(complex_interval) = complex.

ci_to_complex(CI) =
    complex(
        rational_to_float(midpoint(ci_real(CI))),
        rational_to_float(midpoint(ci_imag(CI)))).

:- func complex_pow(complex, int) = complex.

complex_pow(Z, N) = Result :-
    ( if int.'=<'(N, 0) then
        Result = complex(1.0, 0.0)
    else if int.'mod'(N, 2) = 0 then
        Half = complex_pow(Z, int.'/'(N, 2)),
        Result = cmul(Half, Half)
    else
        Result = cmul(Z, complex_pow(Z, int.'-'(N, 1)))
    ).

%---------------------------------------------------------------------------%
% Number theory
%---------------------------------------------------------------------------%

mod_exp(B, E, M) = Result :-
    ( if integer.is_zero(E) then
        Result = integer.one
    else if integer.'mod'(E, integer(2)) = integer.zero then
        Half = mod_exp(B, integer.'//'(E, integer(2)), M),
        Result = integer.'mod'(integer.'*'(Half, Half), M)
    else
        Result = integer.'mod'(
            integer.'*'(B, mod_exp(B, integer.'-'(E, integer.one), M)),
            M)
    ).

euler_totient(N) = Result :-
    ( if integer.'=<'(N, integer.one) then
        Result = integer.one
    else
        Fs = factorise(det_positive(N)),
        Result = list.foldl(
            ( func(PP, Acc) =
                integer.'*'(Acc,
                    integer.'*'(
                        integer.'-'(pp_prime(PP), integer.one),
                        integer_pow(pp_prime(PP),
                            int.'-'(pp_exponent(PP), 1))))
            ), Fs, integer.one)
    ).

:- func integer_pow(integer, int) = integer.

integer_pow(B, E) = Result :-
    ( if int.'=<'(E, 0) then
        Result = integer.one
    else
        Result = integer.'*'(B, integer_pow(B, int.'-'(E, 1)))
    ).

primitive_root(P) = Result :-
    ( if integer.'=<'(P, integer.one) then
        Result = no
    else if P = integer(2) then
        Result = yes(integer.one)
    else
        Phi = integer.'-'(P, integer.one),
        Factors = prime_factors(det_positive(Phi)),
        find_prim_root(2, P, Phi, Factors, Result)
    ).

:- pred find_prim_root(int::in, integer::in, integer::in,
    list(integer)::in, maybe(integer)::out) is det.

find_prim_root(G, P, Phi, Factors, Result) :-
    GI = integer(G),
    ( if integer.'>='(GI, P) then
        Result = no
    else if is_prim_root(GI, P, Phi, Factors) then
        Result = yes(GI)
    else
        find_prim_root(int.'+'(G, 1), P, Phi, Factors, Result)
    ).

:- pred is_prim_root(integer::in, integer::in, integer::in,
    list(integer)::in) is semidet.

is_prim_root(G, P, Phi, Factors) :-
    list.all_true(
        ( pred(Q::in) is semidet :-
            mod_exp(G, integer.'//'(Phi, Q), P) \= integer.one
        ), Factors).

primitive_root_mod(N) = Result :-
    ( if integer.'=<'(N, integer.zero) then
        Result = no
    else if integer.'=<'(N, integer(2)) then
        Result = yes(integer.one)
    else if N = integer(4) then
        Result = yes(integer(3))
    else
        Fs = factorise(det_positive(N)),
        ( if Fs = [prime_power(P, _)], integer.'>'(P, integer(2)) then
            find_prim_root_mod(N, Result)
        else if Fs = [prime_power(integer(2), 1), prime_power(P, _)],
                integer.'>'(P, integer(2)) then
            find_prim_root_mod(N, Result)
        else
            Result = no
        )
    ).

:- pred find_prim_root_mod(integer::in, maybe(integer)::out) is det.

find_prim_root_mod(N, Result) :-
    Phi = euler_totient(N),
    Factors = prime_factors(det_positive(Phi)),
    find_prim_root_mod_loop(2, N, Phi, Factors, Result).

:- pred find_prim_root_mod_loop(int::in, integer::in, integer::in,
    list(integer)::in, maybe(integer)::out) is det.

find_prim_root_mod_loop(G, N, Phi, Factors, Result) :-
    GI = integer(G),
    ( if integer.'>='(GI, N) then
        Result = no
    else if integer_gcd(GI, N) = integer.one,
            is_prim_root(GI, N, Phi, Factors) then
        Result = yes(GI)
    else
        find_prim_root_mod_loop(int.'+'(G, 1), N, Phi, Factors, Result)
    ).

:- func integer_gcd(integer, integer) = integer.

integer_gcd(A, B) = Result :-
    AA = integer.abs(A),
    BB = integer.abs(B),
    ( if integer.is_zero(BB) then
        Result = AA
    else
        Result = integer_gcd(BB, integer.'mod'(AA, BB))
    ).

%---------------------------------------------------------------------------%
% Subgroup chain and Gauss periods
%---------------------------------------------------------------------------%

subgroup_chain(P) = Result :-
    Phi = integer.'-'(P, integer.one),
    Fs = factorise(det_positive(Phi)),
    ( if primitive_root(P) = yes(G) then
        PrimeFactorList = expand_steps(
            list.map(( func(PP) = {pp_prime(PP), pp_exponent(PP)} ), Fs)),
        Result = build_chain(G, P, Phi,
            list.map(integer, PrimeFactorList))
    else
        Result = [[integer.one]]
    ).

:- func build_chain(integer, integer, integer, list(integer))
    = list(list(integer)).

build_chain(G, P, Phi, Qs) = Result :-
    ( if Qs = [] then
        Result = [[integer.one]]
    else if Qs = [Q | QRest] then
        SubOrder = integer.'//'(Phi, Q),
        SubGen = mod_exp(G, Q, P),
        SubGroup = list.sort_and_remove_dups(
            list.map(
                ( func(K) = mod_exp(SubGen, integer(K), P) ),
                0 `..` int.'-'(integer.det_to_int(SubOrder), 1))),
        Rest = build_chain(mod_exp(G, Q, P), P, SubOrder, QRest),
        Result = [SubGroup | Rest]
    else
        Result = [[integer.one]]
    ).

gauss_periods(P, E) = Result :-
    Phi = integer.'-'(P, integer.one),
    F = int.'/'(integer.det_to_int(Phi), E),
    ( if primitive_root(P) = yes(G) then
        Result = list.map(
            ( func(K) = list.map(
                ( func(J) = mod_exp(G,
                    integer(int.'+'(int.'*'(E, J), K)), P) ),
                0 `..` int.'-'(F, 1))
            ), 0 `..` int.'-'(E, 1))
    else
        Result = []
    ).

%---------------------------------------------------------------------------%
% Reorder factors for prime powers
%---------------------------------------------------------------------------%

reorder_factors(N, Fs) = Result :-
    NFs = factorise(det_positive(N)),
    ( if NFs = [prime_power(P, K)], int.'>'(K, 1) then
        PFactor = list.filter(
            ( pred(PP::in) is semidet :- pp_prime(PP) = P ), Fs),
        RestFs = list.filter(
            ( pred(PP::in) is semidet :- pp_prime(PP) \= P ), Fs),
        Result = list.map(
            ( func(PP) = {pp_prime(PP), pp_exponent(PP)} ),
            list.append(PFactor, RestFs))
    else
        Result = list.map(
            ( func(PP) = {pp_prime(PP), pp_exponent(PP)} ), Fs)
    ).

%---------------------------------------------------------------------------%
:- end_module trig_galois.
%---------------------------------------------------------------------------%
