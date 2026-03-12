%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: rad_groebner.m
% Main author: nyc
%
% Gröbner basis reduction for radical expressions.
%
% Converts a NormExpr (polynomial in radical atoms over Q) into an
% MPoly Rational, computes a Gröbner basis of the ideal of defining
% relations between the atoms, and reduces the expression modulo that ideal.
%
% Three reduction strategies:
%   1. ClearDenominators: multiply through by atom powers
%   2. InverseVariables: introduce a_inv with a·a_inv = 1
%   3. EliminateNested: use elimination ordering for NestedRoot atoms
%
%---------------------------------------------------------------------------%

:- module rad_groebner.
:- interface.

:- import_module list.
:- import_module normal_form.
:- import_module rad_expr.
:- import_module rational.

%---------------------------------------------------------------------------%

:- type strategy
    --->    clear_denominators
    ;       inverse_variables
    ;       eliminate_nested.

:- type groebner_context.

    % Create an empty context.
    %
:- func empty_context = groebner_context.

    % Create a context from a list of atoms using the given strategy.
    %
:- func context_from_atoms(strategy, list(atom)) = groebner_context.

    % Reduce a NormExpr modulo the Gröbner basis.
    %
:- func reduce_norm_expr(strategy, groebner_context, norm_expr)
    = norm_expr.

    % Reduce a RadExpr using inverse_variables strategy.
    %
:- func reduce_rad_expr(rad_expr(rational)) = rad_expr(rational).

    % Run all three strategies, return results sorted by term count.
    %
:- func reduce_rad_expr_all(rad_expr(rational))
    = list({strategy, int, rad_expr(rational)}).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module groebner.
:- use_module int.
:- import_module map.
:- import_module multivariate.
:- import_module pair.
:- import_module set.

%---------------------------------------------------------------------------%
% InverseAtom encoding: nested_root(0, encode(atom))
% Root index 0 is never used by real atoms (always >= 2).
%---------------------------------------------------------------------------%

:- func inverse_atom_encode(atom) = rad_expr(rational).

inverse_atom_encode(rat_root(N, R)) = re_root(N, re_lit(R)).
inverse_atom_encode(imag_unit) = re_root(2, re_lit(
    rational.'-'(rational.zero, rational.one))).
inverse_atom_encode(nested_root(N, E)) = re_root(N, E).

:- func inverse_atom_decode(rad_expr(rational)) = atom.

inverse_atom_decode(E) = Result :-
    ( if E = re_root(N, re_lit(R)) then
        ( if N = 2, R = rational.'-'(rational.zero, rational.one) then
            Result = imag_unit
        else
            Result = rat_root(N, R)
        )
    else if E = re_root(N, Inner) then
        Result = nested_root(N, Inner)
    else
        Result = rat_root(1, rational.one)
    ).

:- func mk_inverse_atom(atom) = atom.

mk_inverse_atom(A) = nested_root(0, inverse_atom_encode(A)).

:- pred is_inverse_atom(atom::in, atom::out) is semidet.

is_inverse_atom(nested_root(0, E), inverse_atom_decode(E)).

%---------------------------------------------------------------------------%
% Context
%---------------------------------------------------------------------------%

:- type groebner_context
    --->    groebner_context(
                ctx_atom_to_var :: map(atom, var),
                ctx_var_to_atom :: map(var, atom),
                ctx_next_var    :: int,
                ctx_basis       :: gb(rational),
                ctx_ord         :: mono_ord,
                ctx_inverse_of  :: map(atom, atom)
            ).

empty_context = groebner_context(
    map.init, map.init, 0,
    groebner_basis(grevlex_ord, []),
    grevlex_ord,
    map.init).

context_from_atoms(Strat, Atoms) = Ctx3 :-
    {Ctx1, _} = list.foldl(add_atom, Atoms, {empty_context, []}),
    Ctx2 = ( if Strat = clear_denominators then Ctx1
             else add_inverse_vars(Atoms, Ctx1) ),
    Ctx2B = ( if Strat = eliminate_nested then
        set_elim_ordering(Ctx2)
    else
        Ctx2
    ),
    Rels = list.condense(list.map(
        ( func(A) = atom_relations(Ctx2B, A) ),
        map.keys(ctx_atom_to_var(Ctx2B)))),
    Basis = groebner_basis(ctx_ord(Ctx2B), Rels),
    Ctx3 = Ctx2B ^ ctx_basis := Basis.

:- func set_elim_ordering(groebner_context) = groebner_context.

set_elim_ordering(Ctx) = Ctx ^ ctx_ord := elim_ord(ElimVars) :-
    NestedVars = set.from_list(list.filter_map(
        nested_var_extract,
        map.to_assoc_list(ctx_atom_to_var(Ctx)))),
    IVars = set.from_list(list.filter_map(
        imag_var_extract,
        map.to_assoc_list(ctx_atom_to_var(Ctx)))),
    ElimVars = set.union(NestedVars, IVars).

:- func nested_var_extract(pair(atom, var)) = var is semidet.

nested_var_extract(nested_root(_, _) - V) = V.

:- func imag_var_extract(pair(atom, var)) = var is semidet.

imag_var_extract(imag_unit - V) = V.

:- func add_atom(atom, {groebner_context, list(var)})
    = {groebner_context, list(var)}.

add_atom(Atom, {Ctx, Vars}) = {Ctx1, Vars1} :-
    ( if map.search(ctx_atom_to_var(Ctx), Atom, V) then
        Ctx1 = Ctx,
        Vars1 = Vars ++ [V]
    else
        V = var(ctx_next_var(Ctx)),
        Ctx1 = (((Ctx
            ^ ctx_atom_to_var := map.set(ctx_atom_to_var(Ctx), Atom, V))
            ^ ctx_var_to_atom := map.set(ctx_var_to_atom(Ctx), V, Atom))
            ^ ctx_next_var := int.'+'(ctx_next_var(Ctx), 1)),
        Vars1 = Vars ++ [V]
    ).

:- func add_inverse_vars(list(atom), groebner_context) = groebner_context.

add_inverse_vars(Atoms, Ctx) = list.foldl(add_inv, Atoms, Ctx).

:- func add_inv(atom, groebner_context) = groebner_context.

add_inv(Atom, C) = Result :-
    InvAtom = mk_inverse_atom(Atom),
    ( if map.contains(ctx_atom_to_var(C), InvAtom) then
        Result = C
    else
        V = var(ctx_next_var(C)),
        Result = ((((C
            ^ ctx_atom_to_var := map.set(ctx_atom_to_var(C), InvAtom, V))
            ^ ctx_var_to_atom := map.set(ctx_var_to_atom(C), V, InvAtom))
            ^ ctx_next_var := int.'+'(ctx_next_var(C), 1))
            ^ ctx_inverse_of := map.set(ctx_inverse_of(C), Atom, InvAtom))
    ).

%---------------------------------------------------------------------------%
% Atom relations
%---------------------------------------------------------------------------%

:- func atom_relations(groebner_context, atom) = list(mpoly(rational)).

atom_relations(Ctx, Atom) = Result :-
    ( if map.search(ctx_atom_to_var(Ctx), Atom, V) then
        XV = mp_var(V),
        ( if is_inverse_atom(Atom, OrigAtom) then
            ( if map.search(ctx_atom_to_var(Ctx), OrigAtom, OrigV) then
                Result = [mp_sub(mp_mul(mp_var(OrigV), XV),
                    mp_const(rational.one))]
            else
                Result = []
            )
        else if Atom = rat_root(N, R) then
            Result = [mp_sub(pow_mpoly(XV, N),
                mp_const(rational.from_integers(numer(R), denom(R))))]
        else if Atom = imag_unit then
            Result = [mp_add(mp_mul(XV, XV), mp_const(rational.one))]
        else if Atom = nested_root(N, Inner) then
            RadicandNE = to_norm_expr(Inner),
            RadicandPoly = norm_expr_to_mpoly_direct(Ctx, RadicandNE),
            Result = [mp_sub(pow_mpoly(XV, N), RadicandPoly)]
        else
            Result = []
        )
    else
        Result = []
    ).

%---------------------------------------------------------------------------%
% Conversion: NormExpr ↔ MPoly
%---------------------------------------------------------------------------%

:- func norm_expr_to_mpoly_cleared(groebner_context, norm_expr)
    = mpoly(rational).

norm_expr_to_mpoly_cleared(Ctx, norm_expr(Terms)) = Result :-
    NegExps = map.foldl(collect_negs, Terms, map.init),
    ( if map.is_empty(NegExps) then
        Result = norm_expr_to_mpoly_direct(Ctx, norm_expr(Terms))
    else
        Result = map.foldl(
            ( func(Mono, Coeff, Acc) =
                mp_add(Acc, mp_scale(Coeff,
                    monomial_to_mpoly_shifted(Ctx, NegExps, Mono)))
            ),
            Terms, mp_zero)
    ).

:- func collect_negs(monomial, rational, map(atom, int))
    = map(atom, int).

collect_negs(monomial(Atoms), _, Acc) =
    map.foldl(
        ( func(Atom, E, A) = Result :-
            ( if int.'<'(E, 0) then
                ( if map.search(A, Atom, Prev) then
                    Result = map.set(A, Atom, int.min(Prev, E))
                else
                    Result = map.set(A, Atom, E)
                )
            else
                Result = A
            )
        ),
        Atoms, Acc).

:- func norm_expr_to_mpoly_inverse(groebner_context, norm_expr)
    = mpoly(rational).

norm_expr_to_mpoly_inverse(Ctx, norm_expr(Terms)) =
    map.foldl(
        ( func(Mono, Coeff, Acc) =
            mp_add(Acc, mp_scale(Coeff,
                monomial_to_mpoly_inverse(Ctx, Mono)))
        ),
        Terms, mp_zero).

:- func norm_expr_to_mpoly_direct(groebner_context, norm_expr)
    = mpoly(rational).

norm_expr_to_mpoly_direct(Ctx, norm_expr(Terms)) =
    map.foldl(
        ( func(Mono, Coeff, Acc) =
            mp_add(Acc, mp_scale(Coeff,
                monomial_to_mpoly_direct(Ctx, Mono)))
        ),
        Terms, mp_zero).

:- func monomial_to_mpoly_direct(groebner_context, monomial) = mpoly(rational).

monomial_to_mpoly_direct(Ctx, monomial(Atoms)) =
    map.foldl(
        ( func(Atom, E, Acc) = Result :-
            ( if map.search(ctx_atom_to_var(Ctx), Atom, V),
                 int.'>'(E, 0)
            then
                Result = mp_mul(Acc, pow_mpoly(mp_var(V), E))
            else
                Result = Acc
            )
        ),
        Atoms, mp_const(rational.one)).

:- func monomial_to_mpoly_shifted(groebner_context, map(atom, int),
    monomial) = mpoly(rational).

monomial_to_mpoly_shifted(Ctx, NegExps, monomial(Atoms)) = Result :-
    AllAtoms = map.foldl(
        ( func(A, NE, Acc) =
            ( if map.search(Acc, A, Cur) then
                map.set(Acc, A, int.'+'(Cur, int.abs(NE)))
            else
                map.set(Acc, A, int.abs(NE))
            )
        ),
        NegExps, Atoms),
    Result = map.foldl(
        ( func(Atom, E, Acc) = R :-
            ( if map.search(ctx_atom_to_var(Ctx), Atom, V),
                 int.'>'(E, 0)
            then
                R = mp_mul(Acc, pow_mpoly(mp_var(V), E))
            else
                R = Acc
            )
        ),
        AllAtoms, mp_const(rational.one)).

:- func monomial_to_mpoly_inverse(groebner_context, monomial)
    = mpoly(rational).

monomial_to_mpoly_inverse(Ctx, monomial(Atoms)) =
    map.foldl(
        ( func(Atom, E, Acc) = Result :-
            ( if int.'>'(E, 0) then
                ( if map.search(ctx_atom_to_var(Ctx), Atom, V) then
                    Result = mp_mul(Acc, pow_mpoly(mp_var(V), E))
                else
                    Result = Acc
                )
            else
                ( if map.search(ctx_inverse_of(Ctx), Atom, InvAtom),
                     map.search(ctx_atom_to_var(Ctx), InvAtom, IV)
                then
                    Result = mp_mul(Acc, pow_mpoly(mp_var(IV), int.abs(E)))
                else
                    Result = Acc
                )
            )
        ),
        Atoms, mp_const(rational.one)).

:- func mpoly_to_norm_expr(groebner_context, mpoly(rational)) = norm_expr.

mpoly_to_norm_expr(Ctx, mpoly(Terms)) = norm_expr(ResultMap) :-
    ResultMap = map.foldl(
        ( func(M, Coeff, Acc) = Result :-
            ( if Coeff = rational.zero then
                Result = Acc
            else
                NfMono = mono_to_monomial(Ctx, M),
                ( if map.search(Acc, NfMono, Prev) then
                    NewC = rational.'+'(Prev, Coeff),
                    ( if NewC = rational.zero then
                        Result = map.delete(Acc, NfMono)
                    else
                        Result = map.set(Acc, NfMono, NewC)
                    )
                else
                    Result = map.set(Acc, NfMono, Coeff)
                )
            )
        ),
        Terms, map.init).

:- func mono_to_monomial(groebner_context, mono) = monomial.

mono_to_monomial(Ctx, mono(Vars)) = monomial(ResultAtoms) :-
    ResultAtoms = map.foldl(
        ( func(V, E, Acc) = Result :-
            ( if E = 0 then
                Result = Acc
            else if map.search(ctx_var_to_atom(Ctx), V, AtomVal) then
                ( if is_inverse_atom(AtomVal, Orig) then
                    NegE = int.'-'(0, E),
                    ( if map.search(Acc, Orig, Prev) then
                        NewE = int.'+'(Prev, NegE),
                        ( if NewE = 0 then
                            Result = map.delete(Acc, Orig)
                        else
                            Result = map.set(Acc, Orig, NewE)
                        )
                    else
                        Result = map.set(Acc, Orig, NegE)
                    )
                else
                    ( if map.search(Acc, AtomVal, Prev) then
                        NewE = int.'+'(Prev, E),
                        ( if NewE = 0 then
                            Result = map.delete(Acc, AtomVal)
                        else
                            Result = map.set(Acc, AtomVal, NewE)
                        )
                    else
                        Result = map.set(Acc, AtomVal, E)
                    )
                )
            else
                Result = Acc
            )
        ),
        Vars, map.init).

%---------------------------------------------------------------------------%
% Reduction
%---------------------------------------------------------------------------%

reduce_norm_expr(Strat, Ctx, NE) = Result :-
    Poly = ( if Strat = clear_denominators then
        norm_expr_to_mpoly_cleared(Ctx, NE)
    else
        norm_expr_to_mpoly_inverse(Ctx, NE)
    ),
    Reduced = gb_reduce(ctx_basis(Ctx), Poly),
    ( if mp_is_zero(Reduced) then
        Result = norm_lit(rational.zero)
    else
        Result = mpoly_to_norm_expr(Ctx, Reduced)
    ).

reduce_rad_expr(Expr) = from_norm_expr(Reduced) :-
    NE = to_norm_expr(Expr),
    Atoms = collect_atoms_ne(NE),
    AtomList = set.to_sorted_list(Atoms),
    Ctx = context_from_atoms(inverse_variables, AtomList),
    Reduced = reduce_norm_expr(inverse_variables, Ctx, NE).

reduce_rad_expr_all(Expr) = Sorted :-
    NE = to_norm_expr(Expr),
    Atoms = set.to_sorted_list(collect_atoms_ne(NE)),
    Results = list.map(
        ( func(Strat) = {Strat, NTerms, from_norm_expr(Reduced)} :-
            Ctx = context_from_atoms(Strat, Atoms),
            Reduced = reduce_norm_expr(Strat, Ctx, NE),
            Reduced = norm_expr(RM),
            NTerms = map.count(RM)
        ),
        [clear_denominators, inverse_variables, eliminate_nested]),
    list.sort(
        ( pred({_, N1, _}::in, {_, N2, _}::in, Cmp::out) is det :-
            compare(Cmp, N1, N2)
        ),
        Results, Sorted).

%---------------------------------------------------------------------------%
% Collect atoms from NormExpr
%---------------------------------------------------------------------------%

:- func collect_atoms_ne(norm_expr) = set(atom).

collect_atoms_ne(norm_expr(Terms)) =
    map.foldl(
        ( func(Mono, _, Acc) = collect_atoms_mono(Mono, Acc) ),
        Terms, set.init).

:- func collect_atoms_mono(monomial, set(atom)) = set(atom).

collect_atoms_mono(monomial(Atoms), Acc) =
    map.foldl(
        ( func(Atom, _, A) = collect_from_atom(Atom, A) ),
        Atoms, Acc).

:- func collect_from_atom(atom, set(atom)) = set(atom).

collect_from_atom(Atom, Acc) = Result :-
    ( if Atom = nested_root(_, Inner) then
        Result = set.insert(
            collect_atoms_ne(to_norm_expr(Inner)), Atom)
            `set.union` Acc
    else
        Result = set.insert(Acc, Atom)
    ).

%---------------------------------------------------------------------------%
% Helpers
%---------------------------------------------------------------------------%

:- func pow_mpoly(mpoly(rational), int) = mpoly(rational).

pow_mpoly(P, N) = Result :-
    ( if N = 0 then
        Result = mp_const(rational.one)
    else if N = 1 then
        Result = P
    else
        ( if int.even(N) then
            Half = pow_mpoly(P, int.'//'(N, 2)),
            Result = mp_mul(Half, Half)
        else
            Result = mp_mul(P, pow_mpoly(P, int.'-'(N, 1)))
        )
    ).

%---------------------------------------------------------------------------%
:- end_module rad_groebner.
%---------------------------------------------------------------------------%
