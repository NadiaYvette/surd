%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: demo_euler_integral.m
%
% Demo: Euler substitution integrals.
%
% Computes antiderivatives of ∫ P(x)/Q(x) · (√(ax²+bx+c))^n dx
% using Euler's three substitutions, with radical coefficients
% simplified by surd's normalization machinery.
%
%---------------------------------------------------------------------------%

:- module demo_euler_integral.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module euler_integrate.
:- import_module list.
:- import_module maybe.
:- import_module poly.
:- import_module rational.
:- import_module string.

%---------------------------------------------------------------------------%

:- type example
    --->    example(
                ex_name     :: string,
                ex_intgd    :: euler_integrand,
                ex_known    :: string
            ).

:- func examples = list(example).

examples = [
    example(
        "∫ dx / √(x²+1)",
        euler_integrand(const_poly(one), const_poly(one), -1, one, zero, one),
        "ln|x + √(x²+1)|"
    ),
    example(
        "∫ dx / √(x²-1)",
        euler_integrand(const_poly(one), const_poly(one), -1, one, zero, -one),
        "ln|x + √(x²-1)|"
    ),
    example(
        "∫ dx / √(x²+2x+2)",
        euler_integrand(const_poly(one), const_poly(one), -1,
            one, rational(2), rational(2)),
        "ln|x + 1 + √(x²+2x+2)|"
    ),
    example(
        "∫ x dx / √(x²+1)",
        euler_integrand(mk_poly([zero, one]), const_poly(one), -1,
            one, zero, one),
        "√(x²+1)"
    ),
    example(
        "∫ dx / √(4x²+1)",
        euler_integrand(const_poly(one), const_poly(one), -1,
            rational(4), zero, one),
        "(1/2)·ln|2x + √(4x²+1)|"
    ),
    example(
        "∫ dx / ((x+1)·√(x²+1))",
        euler_integrand(const_poly(one), mk_poly([one, one]), -1,
            one, zero, one),
        "..."
    ),
    example(
        "∫ √(x²+1) dx",
        euler_integrand(const_poly(one), const_poly(one), 1,
            one, zero, one),
        "(x·√(x²+1) + ln|x + √(x²+1)|) / 2"
    ),
    example(
        "∫ dx / √(1-x²)  [Euler 2]",
        euler_integrand(const_poly(one), const_poly(one), -1,
            -one, zero, one),
        "arcsin(x)"
    ),
    example(
        "∫ dx / √(x²-3x+2)  [Euler 3]",
        euler_integrand(const_poly(one), const_poly(one), -1,
            one, rational(-3), rational(2)),
        "ln|x - 3/2 + √(x²-3x+2)|"
    )
].

%---------------------------------------------------------------------------%

:- pred render_example(string::in, example::in, io::di, io::uo) is det.

render_example(Fmt, Ex, !IO) :-
    io.format("─── %s ───\n", [s(ex_name(Ex))], !IO),
    Result = euler_integrate(ex_intgd(Ex)),
    (
        Result = no,
        io.write_string("  (no rational Euler substitution found)\n", !IO)
    ;
        Result = yes(IR),
        ( if Fmt = "latex" then
            io.format("  = %s\n", [s(latex_sym_expr(ir_expr(IR)))], !IO)
        else
            io.format("  = %s\n", [s(pretty_sym_expr(ir_expr(IR)))], !IO)
        ),
        io.format("  Known: %s\n", [s(ex_known(Ex))], !IO)
    ),
    io.write_string("\n", !IO).

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(render_example("text"), examples, !IO).

%---------------------------------------------------------------------------%
:- end_module demo_euler_integral.
%---------------------------------------------------------------------------%
