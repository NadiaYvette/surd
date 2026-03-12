%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: galois_identify.m
% Main author: nyc
%
% Galois group identification for irreducible degree-5 polynomials over Q.
%
% Uses the Stauduhar descent approach: start from S5, then apply
% resolvent polynomials to descend through the transitive subgroup
% lattice of S5.
%
% Decision tree:
%   disc square?  sextic root?  result
%   no            no            S5
%   yes           no            A5
%   no            yes           F20
%   yes           yes           D5 or C5 (Frobenius test)
%
%---------------------------------------------------------------------------%

:- module galois_identify.
:- interface.

:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module rad_eval.
:- import_module rational.
:- import_module transitive_group.

%---------------------------------------------------------------------------%

:- type galois_result
    --->    galois_result(
                gr_group :: transitive_group,
                gr_roots :: list(complex)
            ).

    % Identify the Galois group of a degree-5 polynomial over Q.
    %
    % Returns no if the polynomial does not have degree 5 or if the
    % sextic resolvent cannot be constructed.
    %
    % Precondition: the input polynomial should be irreducible over Q.
    %
:- func identify_galois_group_5(poly(rational)) = maybe(galois_result).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module float.
:- use_module int.
:- use_module math.
:- import_module integer.
:- import_module require.
:- import_module resolvent.
:- import_module string.

%---------------------------------------------------------------------------%
% Main identification
%---------------------------------------------------------------------------%

identify_galois_group_5(F) = Result :-
    ( if degree(F) \= 5 then
        Result = no
    else
        Disc = discriminant_of(F),
        DiscSq = ( if is_square_rational(Disc) then yes else no ),
        Roots = complex_roots_of(F),
        MaybeSextic = sextic_resolvent_5(Roots),
        ( if MaybeSextic = yes(Sextic) then
            HasSexticRoot = ( if has_rational_root(Sextic) then yes else no ),
            Name = identify_name(DiscSq, HasSexticRoot, F),
            Groups = trans_groups_of_degree(5),
            ( if find_group_by_name(Groups, Name, G) then
                Result = yes(galois_result(G, Roots))
            else
                Result = no
            )
        else
            Result = no
        )
    ).

:- func identify_name(bool, bool, poly(rational)) = string.
:- import_module bool.

identify_name(DiscSq, HasSexticRoot, F) = Name :-
    ( if HasSexticRoot = no, DiscSq = no then
        Name = "S5"
    else if HasSexticRoot = no, DiscSq = yes then
        Name = "A5"
    else if HasSexticRoot = yes, DiscSq = no then
        Name = "F20"
    else
        % Both yes: D5 or C5
        ( if is_cyclic_by_frobenius(F) then
            Name = "C5"
        else
            Name = "D5"
        )
    ).

:- pred find_group_by_name(list(transitive_group)::in,
    string::in, transitive_group::out) is semidet.

find_group_by_name([G | Gs], Name, Result) :-
    ( if tg_name(G) = Name then
        Result = G
    else
        find_group_by_name(Gs, Name, Result)
    ).

%---------------------------------------------------------------------------%
% Sextic resolvent
%---------------------------------------------------------------------------%

    % Construct the sextic resolvent from approximate complex roots.
    %
    % Evaluates the F20-invariant theta at all 120 permutations of the
    % five roots, clusters the values into 6 orbits, and builds
    % R_theta(x) = prod(x - v_j).
    %
:- func sextic_resolvent_5(list(complex)) = maybe(poly(rational)).

sextic_resolvent_5(Roots) = Result :-
    AllPerms = perms_of([0, 1, 2, 3, 4]),
    Vals = list.map(
        ( func(Perm) = theta_f20(
            list.map(( func(J) = list.det_index0(Roots, J) ), Perm))
        ), AllPerms),
    Clusters = cluster_by_distance(Vals, 1.0e-4),
    ( if list.length(Clusters) = 6 then
        Centers = list.map(cluster_center, Clusters),
        Result = round_to_rat_poly(Centers)
    else
        Result = no
    ).

    % F20-invariant:
    % theta(x0,...,x4) = sum_{i=0}^{4} x_i^2 * (x_{i+1}*x_{i+4} + x_{i+2}*x_{i+3})
    % (indices mod 5)
    %
:- func theta_f20(list(complex)) = complex.

theta_f20(Xs) = Result :-
    theta_f20_sum(Xs, 0, complex(0.0, 0.0), Result).

:- pred theta_f20_sum(list(complex)::in, int::in,
    complex::in, complex::out) is det.

theta_f20_sum(Xs, I, Acc, Result) :-
    ( if int.'>='(I, 5) then
        Result = Acc
    else
        Xi = list.det_index0(Xs, I),
        X1 = list.det_index0(Xs, int.'mod'(int.'+'(I, 1), 5)),
        X2 = list.det_index0(Xs, int.'mod'(int.'+'(I, 2), 5)),
        X3 = list.det_index0(Xs, int.'mod'(int.'+'(I, 3), 5)),
        X4 = list.det_index0(Xs, int.'mod'(int.'+'(I, 4), 5)),
        Term = cmul(cmul(Xi, Xi),
                    cadd(cmul(X1, X4), cmul(X2, X3))),
        theta_f20_sum(Xs, int.'+'(I, 1), cadd(Acc, Term), Result)
    ).

%---------------------------------------------------------------------------%
% Permutation generation
%---------------------------------------------------------------------------%

:- func perms_of(list(int)) = list(list(int)).

perms_of([]) = [[]].
perms_of(Xs) = Result :-
    Xs = [_ | _],
    Result = list.condense(list.map(
        ( func(X) = list.map(
            ( func(Rest) = [X | Rest] ),
            perms_of(list.delete_all(Xs, X)))
        ), Xs)).

%---------------------------------------------------------------------------%
% Clustering
%---------------------------------------------------------------------------%

:- func cluster_by_distance(list(complex), float) = list(list(complex)).

cluster_by_distance([], _Tol) = [].
cluster_by_distance([V | Vs], Tol) = Result :-
    cluster_loop(Vs, Tol, [[V]], Result).

:- pred cluster_loop(list(complex)::in, float::in,
    list(list(complex))::in, list(list(complex))::out) is det.

cluster_loop([], _Tol, Clusters, Clusters).
cluster_loop([X | Xs], Tol, Clusters0, Clusters) :-
    find_nearest_cluster(X, Clusters0, 0, -1, 1.0e300, BestIdx, BestDist),
    ( if float.'<'(BestDist, Tol), int.'>='(BestIdx, 0) then
        add_to_cluster(X, BestIdx, 0, Clusters0, Clusters1)
    else
        Clusters1 = [[X] | Clusters0]
    ),
    cluster_loop(Xs, Tol, Clusters1, Clusters).

:- pred find_nearest_cluster(complex::in, list(list(complex))::in,
    int::in, int::in, float::in, int::out, float::out) is det.

find_nearest_cluster(_X, [], _I, BestI, BestD, BestI, BestD).
find_nearest_cluster(X, [Cl | Cls], I, BestI0, BestD0, BestI, BestD) :-
    min_dist_to_cluster(X, Cl, 1.0e300, D),
    ( if float.'<'(D, BestD0) then
        find_nearest_cluster(X, Cls, int.'+'(I, 1), I, D, BestI, BestD)
    else
        find_nearest_cluster(X, Cls, int.'+'(I, 1), BestI0, BestD0, BestI, BestD)
    ).

:- pred min_dist_to_cluster(complex::in, list(complex)::in,
    float::in, float::out) is det.

min_dist_to_cluster(_X, [], D, D).
min_dist_to_cluster(X, [C | Cs], D0, D) :-
    Dist = cmag(csub(X, C)),
    D1 = float.min(D0, Dist),
    min_dist_to_cluster(X, Cs, D1, D).

:- pred add_to_cluster(complex::in, int::in, int::in,
    list(list(complex))::in, list(list(complex))::out) is det.

add_to_cluster(_X, _Target, _I, [], []).
add_to_cluster(X, Target, I, [Cl | Cls], [NewCl | NewCls]) :-
    ( if I = Target then
        NewCl = [X | Cl],
        NewCls = Cls
    else
        NewCl = Cl,
        add_to_cluster(X, Target, int.'+'(I, 1), Cls, NewCls)
    ).

:- func cluster_center(list(complex)) = complex.

cluster_center(Cs) = Result :-
    N = list.length(Cs),
    Sum = list.foldl(cadd, Cs, complex(0.0, 0.0)),
    FN = float.float(N),
    Result = complex(float.'/'(re(Sum), FN), float.'/'(im(Sum), FN)).

:- func round_to_rat_poly(list(complex)) = maybe(poly(rational)).

round_to_rat_poly(Roots) = Result :-
    PolyC = list.foldl(mul_linear_c, Roots, [complex(1.0, 0.0)]),
    round_coeffs_c(PolyC, [], MaybeRs),
    ( if MaybeRs = yes(Rs) then
        Result = yes(mk_poly(Rs))
    else
        Result = no
    ).

:- func mul_linear_c(complex, list(complex)) = list(complex).

mul_linear_c(R, Cs) = Result :-
    Shifted = [complex(0.0, 0.0) | Cs],
    NR = cneg(R),
    Scaled = list.append(list.map(( func(C) = cmul(NR, C) ), Cs),
                         [complex(0.0, 0.0)]),
    zipwith_add_c(Shifted, Scaled, Result).

:- pred zipwith_add_c(list(complex)::in, list(complex)::in,
    list(complex)::out) is det.

zipwith_add_c([], Bs, Bs).
zipwith_add_c(As, [], As) :- As = [_ | _].
zipwith_add_c([A | As], [B | Bs], [cadd(A, B) | Cs]) :-
    zipwith_add_c(As, Bs, Cs).

:- pred round_coeffs_c(list(complex)::in, list(rational)::in,
    maybe(list(rational))::out) is det.

round_coeffs_c([], Acc, yes(Rs)) :-
    list.reverse(Acc, Rs).
round_coeffs_c([complex(R, I) | Rest], Acc, Result) :-
    ( if float.'>'(float.abs(I),
            float.'*'(1.0e-4, float.max(1.0, float.abs(R)))) then
        Result = no
    else
        round_coeffs_c(Rest, [best_rational(R) | Acc], Result)
    ).

% Note: uses best_rational/1 from resolvent module for coefficient rounding.

%---------------------------------------------------------------------------%
% Complex arithmetic helpers (local)
%---------------------------------------------------------------------------%

:- func cmag(complex) = float.

cmag(complex(A, B)) = math.sqrt(float.'+'(float.'*'(A, A), float.'*'(B, B))).

%---------------------------------------------------------------------------%
% Frobenius test for C5 vs D5
%---------------------------------------------------------------------------%

    % Frobenius/Chebotarev test to distinguish C5 from D5.
    %
    % For C5, only patterns {5} and {1,1,1,1,1} are possible.
    % D5 additionally admits {1,2,2}.
    % Tests 20 good primes.
    %
:- pred is_cyclic_by_frobenius(poly(rational)::in) is semidet.

is_cyclic_by_frobenius(F) :-
    Cs = coeffs(F),
    lcm_of_denoms(Cs, LcmDen),
    Scale = from_integers(LcmDen, integer.one),
    IntCs = list.map(( func(C) = numer(C * Scale) ), Cs),
    ( if list.last(IntCs, LC0) then
        LC = LC0
    else
        LC = integer.one
    ),
    Disc = discriminant_of(F),
    DiscN = integer.abs(numer(Disc)),
    DiscD = denom(Disc),
    good_primes(LC, DiscN, DiscD, small_primes, 0, 20, GoodPs),
    not has_non_cyclic(IntCs, GoodPs).

:- pred lcm_of_denoms(list(rational)::in, integer::out) is det.

lcm_of_denoms([], integer.one).
lcm_of_denoms([C | Cs], Result) :-
    lcm_of_denoms(Cs, Rest),
    D = integer.abs(denom(C)),
    Result = lcm_integer(Rest, D).

:- func lcm_integer(integer, integer) = integer.

lcm_integer(A, B) = Result :-
    ( if integer.is_zero(A) then
        Result = B
    else if integer.is_zero(B) then
        Result = A
    else
        Result = integer.abs(A * B) // gcd_integer(A, B)
    ).

:- func gcd_integer(integer, integer) = integer.

gcd_integer(A, B) = Result :-
    AA = integer.abs(A),
    BB = integer.abs(B),
    ( if integer.is_zero(BB) then
        Result = AA
    else
        Result = gcd_integer(BB, AA `rem` BB)
    ).

:- pred good_primes(integer::in, integer::in, integer::in,
    list(int)::in, int::in, int::in, list(int)::out) is det.

good_primes(LC, DiscN, DiscD, Primes, Count, Max, Result) :-
    ( if int.'>='(Count, Max) then
        Result = []
    else
        (
            Primes = [],
            Result = []
        ;
            Primes = [P | Ps],
            PI = integer(P),
            ( if integer.'mod'(LC, PI) \= integer.zero,
                 integer.'mod'(DiscN, PI) \= integer.zero,
                 integer.'mod'(DiscD, PI) \= integer.zero then
                good_primes(LC, DiscN, DiscD, Ps,
                    int.'+'(Count, 1), Max, Rest),
                Result = [P | Rest]
            else
                good_primes(LC, DiscN, DiscD, Ps, Count, Max, Result)
            )
        )
    ).

:- pred has_non_cyclic(list(integer)::in, list(int)::in) is semidet.

has_non_cyclic(IntCs, [P | Ps]) :-
    ( if has_non_cyclic_pattern(IntCs, P) then
        true
    else
        has_non_cyclic(IntCs, Ps)
    ).

:- pred has_non_cyclic_pattern(list(integer)::in, int::in) is semidet.

has_non_cyclic_pattern(IntCs, P) :-
    PI = integer(P),
    FpCs = list.map(
        ( func(C) = integer.'mod'(integer.'+'(integer.'mod'(C, PI), PI), PI) ),
        IntCs),
    Pat = factor_pattern(FpCs, PI),
    Pat \= [5],
    Pat \= [1, 1, 1, 1, 1].

%---------------------------------------------------------------------------%
% Fp polynomial arithmetic (ascending coefficient lists)
%---------------------------------------------------------------------------%

:- func factor_pattern(list(integer), integer) = list(int).

factor_pattern(Fcs, P) = Result :-
    F = fp_trim(Fcs),
    fp_factor_loop([], 1, F, [integer.zero, integer.one], P, Degs),
    list.sort(Degs, Result).

:- pred fp_factor_loop(list(int)::in, int::in, list(integer)::in,
    list(integer)::in, integer::in, list(int)::out) is det.

fp_factor_loop(Degs, K, F, H, P, Result) :-
    ( if int.'=<'(fp_deg(F), 0) then
        Result = Degs
    else
        % h <- h^p mod f
        HPrime = fp_pow_mod(H, P, F, P),
        % g = gcd(h' - x, f) mod p
        HX = fp_sub(HPrime, [integer.zero, integer.one], P),
        G = fp_gcd(HX, F, P),
        GD = fp_deg(G),
        ( if GD = 0 then
            fp_factor_loop(Degs, int.'+'(K, 1), F, HPrime, P, Result)
        else
            NF = int.'/'(GD, K),
            FPrime = fp_div(F, G, P),
            NewDegs = list.append(Degs, list.duplicate(NF, K)),
            fp_factor_loop(NewDegs, int.'+'(K, 1), FPrime, HPrime, P, Result)
        )
    ).

:- func fp_trim(list(integer)) = list(integer).

fp_trim(Cs) = Result :-
    list.reverse(Cs, Rev),
    drop_zeros(Rev, Trimmed),
    list.reverse(Trimmed, Result).

:- pred drop_zeros(list(integer)::in, list(integer)::out) is det.

drop_zeros([], []).
drop_zeros([C | Cs], Result) :-
    ( if integer.is_zero(C) then
        drop_zeros(Cs, Result)
    else
        Result = [C | Cs]
    ).

:- func fp_deg(list(integer)) = int.

fp_deg(Cs) = int.'-'(list.length(fp_trim(Cs)), 1).

:- func fp_add(list(integer), list(integer), integer) = list(integer).

fp_add(A, B, P) = fp_trim(Result) :-
    NA = list.length(A),
    NB = list.length(B),
    N = int.max(NA, NB),
    AP = pad(A, N),
    BP = pad(B, N),
    zipwith_mod_add(AP, BP, P, Result).

:- func fp_sub(list(integer), list(integer), integer) = list(integer).

fp_sub(A, B, P) = fp_trim(Result) :-
    NA = list.length(A),
    NB = list.length(B),
    N = int.max(NA, NB),
    AP = pad(A, N),
    BP = pad(B, N),
    zipwith_mod_sub(AP, BP, P, Result).

:- func fp_mul(list(integer), list(integer), integer) = list(integer).

fp_mul(A, B, P) = Result :-
    ( if A = []; B = [] then
        Result = []
    else
        NA = list.length(A),
        NB = list.length(B),
        NR = int.'-'(int.'+'(NA, NB), 1),
        Result = fp_trim(list.map(
            ( func(I) = integer.'mod'(
                fp_mul_coeff(A, B, I, NA, NB), P)
            ), 0 `..` int.'-'(NR, 1)))
    ).

:- func fp_mul_coeff(list(integer), list(integer), int, int, int) = integer.

fp_mul_coeff(A, B, I, NA, NB) = Result :-
    list.foldl(
        ( pred(J::in, Acc0::in, Acc1::out) is det :-
            IJ = int.'-'(I, J),
            ( if int.'>='(J, 0), int.'<'(J, NA),
                 int.'>='(IJ, 0), int.'<'(IJ, NB) then
                AJ = list.det_index0(A, J),
                BIJ = list.det_index0(B, IJ),
                Acc1 = Acc0 + AJ * BIJ
            else
                Acc1 = Acc0
            )
        ), 0 `..` I, integer.zero, Result).

:- func fp_mod(list(integer), list(integer), integer) = list(integer).

fp_mod(A, B, P) = Result :-
    TA = fp_trim(A),
    TB = fp_trim(B),
    ( if int.'<'(fp_deg(TA), fp_deg(TB)) then
        Result = fp_trim(list.map(
            ( func(X) = integer.'mod'(integer.'+'(integer.'mod'(X, P), P), P) ),
            TA))
    else if TB = [] then
        unexpected($pred, "fp_mod: division by zero")
    else
        ( if list.last(TB, LcB0) then
            LcBInv = fp_inv(LcB0, P),
            ( if list.last(TA, LcA0) then
                DA = fp_deg(TA),
                DB = fp_deg(TB),
                Shift = int.'-'(DA, DB),
                Fac = integer.'mod'(LcA0 * LcBInv, P),
                subtract_shifted(TA, TB, Fac, Shift, P, Sub),
                Result = fp_mod(fp_trim(Sub), TB, P)
            else
                Result = []
            )
        else
            Result = []
        )
    ).

:- pred subtract_shifted(list(integer)::in, list(integer)::in,
    integer::in, int::in, integer::in, list(integer)::out) is det.

subtract_shifted(A, B, Fac, Shift, P, Result) :-
    N = list.length(A),
    Result = list.map(
        ( func(I) = integer.'mod'(integer.'+'(
            integer.'mod'(integer.'-'(AI, FacBI), P), P), P) :-
            AI = ( if int.'<'(I, list.length(A)) then
                       list.det_index0(A, I)
                   else
                       integer.zero ),
            IShift = int.'-'(I, Shift),
            FacBI = ( if int.'>='(IShift, 0),
                         int.'<'(IShift, list.length(B)) then
                         Fac * list.det_index0(B, IShift)
                     else
                         integer.zero )
        ), 0 `..` int.'-'(N, 1)).

:- func fp_div(list(integer), list(integer), integer) = list(integer).

fp_div(A, B, P) = Result :-
    TB = fp_trim(B),
    ( if TB = [] then
        unexpected($pred, "fp_div: division by zero")
    else if list.last(TB, LcB0) then
        LcBInv = fp_inv(LcB0, P),
        fp_div_loop([], fp_trim(A), TB, LcBInv, P, Result)
    else
        Result = []
    ).

:- pred fp_div_loop(list(integer)::in, list(integer)::in,
    list(integer)::in, integer::in, integer::in,
    list(integer)::out) is det.

fp_div_loop(Q, R, B, LcBInv, P, Result) :-
    DB = fp_deg(B),
    ( if int.'<'(fp_deg(R), DB) then
        Result = fp_trim(Q)
    else
        ( if list.last(fp_trim(R), LcR0) then
            DR = fp_deg(R),
            Shift = int.'-'(DR, DB),
            Fac = integer.'mod'(LcR0 * LcBInv, P),
            QPrime = fp_add(Q, pad_front(Shift, [Fac]), P),
            subtract_shifted(fp_trim(R), B, Fac, Shift, P, Sub),
            fp_div_loop(QPrime, fp_trim(Sub), B, LcBInv, P, Result)
        else
            Result = fp_trim(Q)
        )
    ).

:- func fp_gcd(list(integer), list(integer), integer) = list(integer).

fp_gcd(A, B, P) = Result :-
    TB = fp_trim(B),
    ( if TB = []; int.'<'(fp_deg(TB), 0) then
        Result = fp_make_monic(fp_trim(A), P)
    else
        Result = fp_gcd(B, fp_mod(A, B, P), P)
    ).

:- func fp_make_monic(list(integer), integer) = list(integer).

fp_make_monic([], _P) = [].
fp_make_monic(Cs, P) = Result :-
    Cs = [_ | _],
    ( if list.last(Cs, LC0) then
        LCInv = fp_inv(LC0, P),
        Result = list.map(
            ( func(C) = integer.'mod'(C * LCInv, P) ), Cs)
    else
        Result = Cs
    ).

:- func fp_pow_mod(list(integer), integer, list(integer), integer)
    = list(integer).

fp_pow_mod(Base, Expo, Modulus, P) = Result :-
    fp_pow_mod_loop([integer.one], Base, Expo, Modulus, P, Result).

:- pred fp_pow_mod_loop(list(integer)::in, list(integer)::in,
    integer::in, list(integer)::in, integer::in,
    list(integer)::out) is det.

fp_pow_mod_loop(Res, B, Expo, Modulus, P, Result) :-
    ( if integer.is_zero(Expo) then
        Result = Res
    else
        ( if integer.'mod'(Expo, integer(2)) = integer.one then
            ResPrime = fp_mod(fp_mul(Res, B, P), Modulus, P)
        else
            ResPrime = Res
        ),
        BPrime = fp_mod(fp_mul(B, B, P), Modulus, P),
        fp_pow_mod_loop(ResPrime, BPrime, Expo // integer(2), Modulus, P, Result)
    ).

:- func fp_inv(integer, integer) = integer.

fp_inv(A, M) = integer.'mod'(integer.'+'(integer.'mod'(X, M), M), M) :-
    egcd(A, M, _G, X, _Y).

:- pred egcd(integer::in, integer::in,
    integer::out, integer::out, integer::out) is det.

egcd(A, B, G, X, Y) :-
    ( if integer.is_zero(A) then
        G = B, X = integer.zero, Y = integer.one
    else
        egcd(integer.'mod'(B, A), A, G1, X1, Y1),
        G = G1,
        X = Y1 - (B // A) * X1,
        Y = X1
    ).

%---------------------------------------------------------------------------%
% Helper functions
%---------------------------------------------------------------------------%

:- func pad(list(integer), int) = list(integer).

pad(Cs, N) = Result :-
    L = list.length(Cs),
    ( if int.'>='(L, N) then
        Result = Cs
    else
        Result = list.append(Cs, list.duplicate(int.'-'(N, L), integer.zero))
    ).

:- func pad_front(int, list(integer)) = list(integer).

pad_front(N, Cs) =
    list.append(list.duplicate(N, integer.zero), Cs).

:- pred zipwith_mod_add(list(integer)::in, list(integer)::in,
    integer::in, list(integer)::out) is det.

zipwith_mod_add([], _, _, []).
zipwith_mod_add([_ | _], [], _, []).
zipwith_mod_add([A | As], [B | Bs], P, [R | Rs]) :-
    R = integer.'mod'(A + B, P),
    zipwith_mod_add(As, Bs, P, Rs).

:- pred zipwith_mod_sub(list(integer)::in, list(integer)::in,
    integer::in, list(integer)::out) is det.

zipwith_mod_sub([], _, _, []).
zipwith_mod_sub([_ | _], [], _, []).
zipwith_mod_sub([A | As], [B | Bs], P, [R | Rs]) :-
    R = integer.'mod'(integer.'+'(integer.'mod'(A - B, P), P), P),
    zipwith_mod_sub(As, Bs, P, Rs).

:- func small_primes = list(int).

small_primes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31,
    37, 41, 43, 47, 53, 59, 61, 67, 71, 73,
    79, 83, 89, 97, 101, 103, 107, 109, 113].

%---------------------------------------------------------------------------%
:- end_module galois_identify.
%---------------------------------------------------------------------------%
