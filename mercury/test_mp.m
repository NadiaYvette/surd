%---------------------------------------------------------------------------%
:- module test_mp.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module eval_mp.
:- import_module float.
:- import_module interval.
:- import_module integer.
:- import_module rad_dag.
:- import_module rad_eval.
:- import_module rad_expr.
:- import_module rational.
:- import_module list.
:- import_module string.

main(!IO) :-
    % Test basic MPFR from_rational
    V1 = mpfr_from_rational(200, rational(1, 2)),
    F1 = mp_to_float(V1),
    io.format("mpfr(1/2) = %.10f (expect 0.5)\n", [f(F1)], !IO),

    % Test MPFR sqrt(2)
    TwoMPFR = mpfr_from_rational(200, rational(2, 1)),
    V2 = mp_sqrt(TwoMPFR),
    F2 = mp_to_float(V2),
    io.format("mpfr_sqrt(2) = %.10f (expect 1.4142...)\n", [f(F2)], !IO),

    % Test mp_to_interval
    IV = mp_to_interval(V2),
    IVLo = mp_to_float(mpfr_from_rational(200, iv_lo(IV))),
    IVHi = mp_to_float(mpfr_from_rational(200, iv_hi(IV))),
    io.format("mp_to_interval(√2) = [%.15f, %.15f]\n",
        [f(IVLo), f(IVHi)], !IO),

    % Test DAG evaluation of √2
    Sqrt2Expr = re_root(2, re_lit(rational(2, 1))),
    Dag2 = to_dag(Sqrt2Expr),
    CI2 = dag_eval_complex_mp(200, Dag2),
    ReLo2 = iv_lo(ci_real(CI2)),
    ReHi2 = iv_hi(ci_real(CI2)),
    ( if rational.'<'(ReLo2, rational(15, 10)),
         rational.'>'(ReHi2, rational(14, 10))
    then io.write_string("dag √2 interval contains 1.414: yes\n", !IO)
    else io.write_string("dag √2 interval contains 1.414: no\n", !IO)
    ),

    io.write_string("Done.\n", !IO).

:- end_module test_mp.
