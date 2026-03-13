%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: demo_solvable_quintic.m
%
% Solvable quintic solver demo.
%
% Demonstrates Galois-theoretic identification and radical tower construction
% for degree-5 polynomials over Q. Classifies each polynomial's Galois group
% among the five transitive subgroups of S₅, then — when the group is solvable
% — constructs explicit radical expressions for the roots.
%
%---------------------------------------------------------------------------%

:- module demo_solvable_quintic.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module float.
:- import_module bool.
:- import_module galois_identify.
:- import_module galois_solve.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module pretty.
:- import_module rad_eval.
:- import_module radical_tower.
:- import_module rational.
:- import_module resolvent.
:- import_module string.
:- import_module transitive_group.

%---------------------------------------------------------------------------%

:- type example
    --->    example(
                ex_name     :: string,
                ex_poly     :: poly(rational),
                ex_notes    :: string
            ).

:- func examples = list(example).

examples = [
    example(
        "x⁵ + x⁴ - 4x³ - 3x² + 3x + 1",
        mk_poly([rational(1), rational(3), rational(-3),
                  rational(-4), rational(1), rational(1)]),
        "C₅ — minimal polynomial of 2cos(2π/11)"
    ),
    example(
        "x⁵ - 2",
        mk_poly([rational(-2), rational.zero, rational.zero,
                  rational.zero, rational.zero, rational(1)]),
        "F₂₀ — Gal(Q(⁵√2,ζ₅)/Q) = Z/5 ⋊ Z/4"
    ),
    example(
        "x⁵ + 20x + 32",
        mk_poly([rational(32), rational(20), rational.zero,
                  rational.zero, rational.zero, rational(1)]),
        "D₅ — dihedral group of order 10"
    ),
    example(
        "x⁵ - 4x + 2",
        mk_poly([rational(2), rational(-4), rational.zero,
                  rational.zero, rational.zero, rational(1)]),
        "S₅ — irreducible by Eisenstein (not solvable)"
    ),
    example(
        "x⁵ - x - 1",
        mk_poly([rational(-1), rational(-1), rational.zero,
                  rational.zero, rational.zero, rational(1)]),
        "S₅ (not solvable)"
    )
].

%---------------------------------------------------------------------------%

:- func rat_to_string(rational) = string.

rat_to_string(R) = Result :-
    N = numer(R),
    D = denom(R),
    ( if D = integer.one then
        Result = integer.to_string(N)
    else
        Result = integer.to_string(N) ++ "/" ++ integer.to_string(D)
    ).

%---------------------------------------------------------------------------%

:- pred run_example(example::in, io::di, io::uo) is det.

run_example(Ex, !IO) :-
    io.format("─── %s ───\n", [s(ex_name(Ex))], !IO),
    io.format("  (%s)\n", [s(ex_notes(Ex))], !IO),

    F = ex_poly(Ex),
    Disc = discriminant_of(F),
    DiscSq = ( if is_square_rational(Disc) then yes else no ),

    io.format("  disc(f) = %s\n", [s(rat_to_string(Disc))], !IO),
    ( DiscSq = yes,
        io.write_string("  disc is square: yes\n", !IO)
    ; DiscSq = no,
        io.write_string("  disc is square: no\n", !IO)
    ),

    MaybeGR = identify_galois_group_5(F),
    (
        MaybeGR = no,
        io.write_string("  (Galois group identification failed)\n\n", !IO)
    ;
        MaybeGR = yes(GR),
        TG = gr_group(GR),
        io.format("  Galois group: %s (order %s)\n",
            [s(tg_name(TG)), s(integer.to_string(tg_order(TG)))], !IO),
        Solv = is_solvable(TG),
        ( Solv = yes,
            io.write_string("  Solvable: yes\n", !IO)
        ; Solv = no,
            io.write_string("  Solvable: no\n", !IO)
        ),

        % Show numerical roots
        Roots = gr_roots(GR),
        RealRoots = list.filter(
            (pred(R::in) is semidet :- float.(float.abs(im(R)) < 1e-6)),
            Roots),
        io.format("  Real roots: %d\n",
            [i(list.length(RealRoots))], !IO),
        list.foldl(
            (pred(R::in, !.IO0::di, !:IO0::uo) is det :-
                io.format("    %.10f\n", [f(re(R))], !IO0)
            ), RealRoots, !IO),

        ( Solv = yes ->
            MaybeSol = solve_via_tower(GR, F),
            (
                MaybeSol = no,
                io.write_string(
                    "  (radical tower construction failed)\n\n", !IO)
            ;
                MaybeSol = yes(RadExprs),
                io.write_string("  Radical expressions:\n", !IO),
                RealExprs = list.filter(
                    (pred(E::in) is semidet :-
                        V = eval_complex(E),
                        float.(float.abs(im(V)) < 1e-6)),
                    RadExprs),
                ShowExprs = ( if RealExprs = [] then
                    list.take_upto(1, RadExprs)
                else
                    RealExprs
                ),
                list.foldl(
                    (pred(E::in, !.IO1::di, !:IO1::uo) is det :-
                        V = eval_complex(E),
                        io.format("    %s\n", [s(pretty(E))], !IO1),
                        io.format("    ≈ %.10f\n", [f(re(V))], !IO1)
                    ), ShowExprs, !IO),
                io.write_string("\n", !IO)
            )
        ;
            io.write_string(
                "  (not solvable — no radical expression exists)\n\n", !IO)
        )
    ).

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("══════ Solvable Quintic Solver ══════\n\n", !IO),
    list.foldl(run_example, examples, !IO).

%---------------------------------------------------------------------------%
:- end_module demo_solvable_quintic.
%---------------------------------------------------------------------------%
