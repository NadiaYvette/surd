%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: demo_elliptic_integral.m
%
% Demo: elliptic integral reduction to Legendre normal forms.
%
% Shows how ∫ R(x) dx / √P(x) with P degree 3 or 4 reduces to
% F(φ,k), E(φ,k), and Π(φ,n,k) with exact radical modulus k.
%
%---------------------------------------------------------------------------%

:- module demo_elliptic_integral.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module elliptic_integrate.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module rational.
:- import_module string.

%---------------------------------------------------------------------------%

:- type example
    --->    example(
                ex_name     :: string,
                ex_intgd    :: elliptic_integrand,
                ex_notes    :: string
            ).

:- func examples = list(example).

examples = [
    example(
        "∫ dx / √(x³ - x)",
        elliptic_integrand(
            const_poly(one), const_poly(one),
            mk_poly([zero, -one, zero, one])),
        "roots 1, 0, -1; k² = 1/2"
    ),
    example(
        "∫ dx / √(x³ - 1)",
        elliptic_integrand(
            const_poly(one), const_poly(one),
            mk_poly([-one, zero, zero, one])),
        "one rational root at 1, two complex — should fail (not all real)"
    ),
    example(
        "∫ dx / √((1-x²)(1-½x²))",
        elliptic_integrand(
            const_poly(one), const_poly(one),
            mk_poly([one, zero,
                from_integers(integer(-3), integer(2)), zero,
                from_integers(integer.one, integer(2))])),
        "already Legendre-like; roots ±1, ±√2; k² = 1/2"
    ),
    example(
        "∫ dx / √(4x³ - 4x)",
        elliptic_integrand(
            const_poly(one), const_poly(one),
            mk_poly([zero, rational(-4), zero, rational(4)])),
        "same roots as #1, leading coeff 4; k² = 1/2"
    ),
    example(
        "∫ dx / √((x²-1)(x²-4))",
        elliptic_integrand(
            const_poly(one), const_poly(one),
            mk_poly([rational(4), zero, rational(-5), zero, one])),
        "roots 2, 1, -1, -2; quartic"
    ),
    example(
        "∫ dx / ((x-3)·√(x³ - x))",
        elliptic_integrand(
            const_poly(one),
            mk_poly([rational(-3), one]),
            mk_poly([zero, -one, zero, one])),
        "Π form: pole at x=3"
    ),
    example(
        "∫ dx / √(x³ - 7x + 6)",
        elliptic_integrand(
            const_poly(one), const_poly(one),
            mk_poly([rational(6), rational(-7), zero, one])),
        "roots 2, 1, -3; factors (x-1)(x-2)(x+3)"
    )
].

%---------------------------------------------------------------------------%

:- pred render_example(string::in, bool::in, example::in,
    io::di, io::uo) is det.

render_example(Fmt, Jacobi, Ex, !IO) :-
    io.format("─── %s ───\n", [s(ex_name(Ex))], !IO),
    io.format("  (%s)\n", [s(ex_notes(Ex))], !IO),
    Result = reduce_elliptic(Jacobi, ex_intgd(Ex)),
    (
        Result = no,
        io.write_string(
            "  (cannot reduce: not all roots are real, " ++
            "or degree unsupported)\n\n", !IO)
    ;
        Result = yes(ER),
        ( if Fmt = "latex" then
            io.write_string(latex_elliptic_result(ER), !IO)
        else
            io.write_string(pretty_elliptic_result(ER), !IO)
        ),
        io.write_string("\n", !IO)
    ).

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("══════ Legendre form ══════\n\n", !IO),
    list.foldl(render_example("text", no), examples, !IO),
    io.write_string("\n══════ Jacobi inverse form ══════\n\n", !IO),
    ( if list.det_index0(examples, 0) = Ex0 then
        render_example("text", yes, Ex0, !IO)
    else
        true
    ),
    ( if list.det_index0(examples, 4) = Ex4 then
        render_example("text", yes, Ex4, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module demo_elliptic_integral.
%---------------------------------------------------------------------------%
