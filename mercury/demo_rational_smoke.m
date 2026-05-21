%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: demo_rational_smoke.m
%
% Smoke test for the Frankenstein Mercury bridge against surd's rational
% module — the smallest probe that exercises:
%   * surd/rational module compilation (rational/2, numer, denom)
%   * operator-overloaded '+' on user types
%   * Mercury stdlib `integer` to_string conversion
%   * io.format with %s placeholder + arg list
%   * cross-module HLDS aggregation (rational imports integer; main
%     imports rational, integer, string, io)
%
% Native expected output (`mmc --make demo_rational_smoke && ./...`):
%   1/2 + 1/3 = 5/6
%   2/3 * 3/4 = 1/2
%   1 - 1/3 = 2/3
%   done
%
%---------------------------------------------------------------------------%

:- module demo_rational_smoke.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module integer.
:- import_module list.
:- import_module rational.
:- import_module string.

:- pred show_rat(string::in, rational::in, io::di, io::uo) is det.
show_rat(Label, R, !IO) :-
    N = numer(R),
    D = denom(R),
    io.format("%s = %s/%s\n",
        [s(Label),
         s(integer.to_string(N)),
         s(integer.to_string(D))],
        !IO).

main(!IO) :-
    show_rat("1/2 + 1/3", rational(1, 2) + rational(1, 3), !IO),
    show_rat("2/3 * 3/4", rational(2, 3) * rational(3, 4), !IO),
    show_rat("1 - 1/3",   rational(1, 1) - rational(1, 3), !IO),
    io.write_string("done\n", !IO).

%---------------------------------------------------------------------------%
:- end_module demo_rational_smoke.
%---------------------------------------------------------------------------%
