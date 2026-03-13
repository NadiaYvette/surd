%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: demo_trig_table.m
%
% Generate tables of exact trigonometric values.
%
% Usage:
%   demo_trig_table [OPTIONS] SPEC [SPEC ...]
%
% Each SPEC is one of:
%   N          — tabulate cos and sin at kπ/N for k = 0, 1, ..., N
%   p/q        — single angle pπ/q
%   N..M       — tabulate at kπ/q for each q in N..M, k = 0..q
%
% Options:
%   --format=latex     LaTeX longtable (default)
%   --format=text      plain text (Unicode)
%   --format=tower     field extension tower display
%   --standalone       emit a complete LaTeX document (default)
%   --no-standalone    table fragment only
%   --cos-only         omit sine column
%   --sin-only         omit cosine column
%   --tan              include tangent column
%
%---------------------------------------------------------------------------%

:- module demo_trig_table.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module dyn_tower.
:- import_module dyn_tower_display.
:- import_module int.
:- import_module integer.
:- import_module latex.
:- import_module list.
:- import_module math.
:- import_module maybe.
:- import_module poly.
:- import_module pretty.
:- import_module rad_eval.
:- import_module rad_expr.
:- import_module rad_normalize.
:- import_module rational.
:- import_module string.
:- import_module tower_descent.
:- import_module trig.

%---------------------------------------------------------------------------%
% Int arithmetic helpers (avoid overload ambiguity with integer/rational)
%---------------------------------------------------------------------------%

:- func isub(int, int) = int.
isub(A, B) = A - B.

:- func iadd(int, int) = int.
iadd(A, B) = A + B.

:- func imul(int, int) = int.
imul(A, B) = A * B.

:- func idiv(int, int) = int.
idiv(A, B) = A / B.

%---------------------------------------------------------------------------%
% Configuration
%---------------------------------------------------------------------------%

:- type format
    --->    fmt_latex
    ;       fmt_text
    ;       fmt_tower.

:- type cols
    --->    cos_and_sin
    ;       cos_only
    ;       sin_only.

:- type config
    --->    config(
                cfg_format      :: format,
                cfg_standalone  :: bool,
                cfg_cols        :: cols,
                cfg_tan         :: bool
            ).

:- func default_config = config.

default_config = config(fmt_latex, yes, cos_and_sin, no).

%---------------------------------------------------------------------------%
% Angle specification
%---------------------------------------------------------------------------%

:- type spec
    --->    single(integer, integer)    % p/q
    ;       denom(integer)             % all k/n for k = 0..n
    ;       range(integer, integer).   % all k/q for q in lo..hi

:- type angle == {integer, integer}.

:- func spec_angles(spec) = list(angle).

spec_angles(single(P, Q)) = [{P, Q}].
spec_angles(denom(N)) = Angles :-
    NI = integer.det_to_int(N),
    Angles = list.map(
        (func(K) = {integer(K), N}),
        0 .. NI).
spec_angles(range(Lo, Hi)) = Angles :-
    LoI = integer.det_to_int(Lo),
    HiI = integer.det_to_int(Hi),
    Qs = LoI .. HiI,
    Angles = list.condense(list.map(
        (func(QI) = list.map(
            (func(K) = {integer(K), integer(QI)}),
            0 .. QI)),
        Qs)).

%---------------------------------------------------------------------------%
% Parsing
%---------------------------------------------------------------------------%

:- func parse_spec(string) = maybe(spec).

parse_spec(S) = Result :-
    Len = string.length(S),
    ( if string.sub_string_search(S, "..", DotPos) then
        A = string.left(S, DotPos),
        B = string.right(S, isub(isub(Len, DotPos), 2)),
        ( if
            string.to_int(A, AV),
            string.to_int(B, BV)
        then
            Result = yes(range(integer(AV), integer(BV)))
        else
            Result = no
        )
    else if string.sub_string_search(S, "/", SlashPos) then
        P = string.left(S, SlashPos),
        Q = string.right(S, isub(isub(Len, SlashPos), 1)),
        ( if
            string.to_int(P, PV),
            string.to_int(Q, QV)
        then
            Result = yes(single(integer(PV), integer(QV)))
        else
            Result = no
        )
    else if string.to_int(S, NV) then
        Result = yes(denom(integer(NV)))
    else
        Result = no
    ).

:- func apply_opt(string, config) = maybe(config).

apply_opt(Opt, Cfg) = Result :-
    ( if Opt = "--format=latex" then
        Result = yes(Cfg ^ cfg_format := fmt_latex)
    else if Opt = "--format=text" then
        Result = yes(Cfg ^ cfg_format := fmt_text)
    else if Opt = "--format=tower" then
        Result = yes(Cfg ^ cfg_format := fmt_tower)
    else if Opt = "--standalone" then
        Result = yes(Cfg ^ cfg_standalone := yes)
    else if Opt = "--no-standalone" then
        Result = yes(Cfg ^ cfg_standalone := no)
    else if Opt = "--cos-only" then
        Result = yes(Cfg ^ cfg_cols := cos_only)
    else if Opt = "--sin-only" then
        Result = yes(Cfg ^ cfg_cols := sin_only)
    else if Opt = "--tan" then
        Result = yes(Cfg ^ cfg_tan := yes)
    else
        Result = no
    ).

:- pred parse_args(list(string)::in, config::out, list(spec)::out)
    is semidet.

parse_args(Args, Cfg, Specs) :-
    list.filter(
        (pred(A::in) is semidet :- string.prefix(A, "--")),
        Args, Opts, Rest),
    list.foldl(
        (pred(O::in, C0::in, C1::out) is semidet :-
            apply_opt(O, C0) = yes(C1)),
        Opts, default_config, Cfg),
    SpecMaybes = list.map(parse_spec, Rest),
    Specs0 = list.filter_map(
        (func(yes(Sp)) = Sp is semidet),
        SpecMaybes),
    Specs0 = [_ | _],
    Specs = Specs0.

%---------------------------------------------------------------------------%
% Rendering helpers
%---------------------------------------------------------------------------%

:- func latex_frac(integer, integer) = string.

latex_frac(P, Q) = Result :-
    ( if P = integer.zero then
        Result = "0"
    else if Q = integer.one then
        Result = show_coeff_pi(P)
    else
        ( if P < integer.zero, integer.abs(P) = integer.one then
            Sn = "-", Np = ""
        else if P < integer.zero then
            Sn = "-", Np = integer.to_string(integer.abs(P))
        else if P = integer.one then
            Sn = "", Np = ""
        else
            Sn = "", Np = integer.to_string(P)
        ),
        Result = Sn ++ "\\frac{" ++ Np ++ "\\pi}{" ++
            integer.to_string(Q) ++ "}"
    ).

:- func show_coeff_pi(integer) = string.

show_coeff_pi(N) =
    ( if N = integer.one then "\\pi"
    else if N = -integer.one then "-\\pi"
    else integer.to_string(N) ++ "\\pi"
    ).

:- func text_frac(integer, integer) = string.

text_frac(P, Q) = Result :-
    ( if P = integer.zero then
        Result = "0"
    else if Q = integer.one then
        ( if P = integer.one then
            Result = "π"
        else if P = -integer.one then
            Result = "-π"
        else
            Result = integer.to_string(P) ++ "π"
        )
    else if P = integer.one then
        Result = "π/" ++ integer.to_string(Q)
    else if P = -integer.one then
        Result = "-π/" ++ integer.to_string(Q)
    else
        Result = integer.to_string(P) ++ "π/" ++ integer.to_string(Q)
    ).

:- func render_angle(format, angle) = string.

render_angle(Fmt, {P, Q}) =
    ( if Fmt = fmt_latex then latex_frac(P, Q)
    else text_frac(P, Q)
    ).

%---------------------------------------------------------------------------%
% Result rendering
%---------------------------------------------------------------------------%

:- type rendered_result
    --->    rendered_result(
                rr_defs :: list({string, string}),
                rr_expr :: string
            ).

:- func render_result(config, trig_result) = rendered_result.

render_result(Cfg, radical(E)) = Result :-
    ( if cfg_format(Cfg) = fmt_latex then
        % Use plain latex (no DAG extraction in Mercury latex module)
        Result = rendered_result([], latex(E))
    else
        Result = rendered_result([], pretty(E))
    ).
render_result(Cfg, min_poly(P)) = Result :-
    ( if cfg_format(Cfg) = fmt_latex then
        Result = rendered_result([], "\\text{minpoly: }" ++ poly_to_string(P))
    else
        Result = rendered_result([], "minpoly: " ++ poly_to_string(P))
    ).

:- func poly_to_string(poly(rational)) = string.

poly_to_string(P) = S :-
    Cs = coeffs(P),
    CStrs = list.map(rat_to_string, Cs),
    S = "[" ++ string.join_list(", ", CStrs) ++ "]".

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
% Row computation
%---------------------------------------------------------------------------%

:- type row
    --->    row(
                row_angle :: string,
                row_cos   :: maybe(rendered_result),
                row_sin   :: maybe(rendered_result),
                row_tan   :: maybe(rendered_result)
            ).

:- func is_zero(rad_expr(rational)) = bool.

is_zero(E) = ( if E = re_lit(R), R = rational.zero then yes else no ).

:- func compute_tan(config, trig_result, trig_result) = rendered_result.

compute_tan(Cfg, CosR, SinR) = Result :-
    ( if SinR = radical(SE), CosR = radical(CE) then
        ( if is_zero(CE) = yes then
            ( if cfg_format(Cfg) = fmt_latex then
                Result = rendered_result([], "\\text{undefined}")
            else
                Result = rendered_result([], "undefined")
            )
        else if is_zero(SE) = yes then
            Result = render_result(Cfg, radical(re_lit(rational.zero)))
        else
            TanExpr = normalize(re_mul(SE, re_inv(CE))),
            Result = render_result(Cfg,
                simplify_trig_result(radical(TanExpr)))
        )
    else
        ( if cfg_format(Cfg) = fmt_latex then
            Result = rendered_result([], "\\text{undefined}")
        else
            Result = rendered_result([], "undefined")
        )
    ).

:- func compute_row(config, angle) = row.

compute_row(Cfg, {P, Q}) = Row :-
    SimpCos = simplify_trig_result(cos_exact(P, Q)),
    SimpSin = simplify_trig_result(sin_exact(P, Q)),
    ( if cfg_cols(Cfg) \= sin_only then
        MCos = yes(render_result(Cfg, SimpCos))
    else
        MCos = no
    ),
    ( if cfg_cols(Cfg) \= cos_only then
        MSin = yes(render_result(Cfg, SimpSin))
    else
        MSin = no
    ),
    ( if cfg_tan(Cfg) = yes then
        MTan = yes(compute_tan(Cfg, SimpCos, SimpSin))
    else
        MTan = no
    ),
    Row = row(render_angle(cfg_format(Cfg), {P, Q}), MCos, MSin, MTan).

%---------------------------------------------------------------------------%
% LaTeX table
%---------------------------------------------------------------------------%

:- func latex_preamble = string.

latex_preamble =
    "\\documentclass[11pt]{article}\n" ++
    "\\usepackage[a4paper,margin=1in]{geometry}\n" ++
    "\\usepackage{amsmath,amssymb}\n" ++
    "\\usepackage{longtable}\n" ++
    "\\usepackage{booktabs}\n" ++
    "\\title{Exact Trigonometric Values}\n" ++
    "\\author{Generated by \\texttt{surd}}\n" ++
    "\\date{}\n" ++
    "\\begin{document}\n" ++
    "\\maketitle\n\n".

:- func num_cols(config) = int.

num_cols(Cfg) = iadd(iadd(A, B), C) :-
    A = ( if cfg_cols(Cfg) \= sin_only then 1 else 0 ),
    B = ( if cfg_cols(Cfg) \= cos_only then 1 else 0 ),
    C = ( if cfg_tan(Cfg) = yes then 1 else 0 ).

:- func col_spec(config) = string.

col_spec(Cfg) = "c" ++ string.duplicate_char('l', num_cols(Cfg)).

:- func latex_header(config) = string.

latex_header(Cfg) = Result :-
    Cols = ["$\\theta$"] ++
        ( if cfg_cols(Cfg) \= sin_only then ["$\\cos\\theta$"] else [] ) ++
        ( if cfg_cols(Cfg) \= cos_only then ["$\\sin\\theta$"] else [] ) ++
        ( if cfg_tan(Cfg) = yes then ["$\\tan\\theta$"] else [] ),
    Result = string.join_list(" & ", Cols).

:- func latex_row(row) = string.

latex_row(R) = Result :-
    WrapMath = (func(S) = "$" ++ S ++ "$"),
    Fields = [row_angle(R)] ++
        maybe_expr(row_cos(R)) ++ maybe_expr(row_sin(R)) ++
        maybe_expr(row_tan(R)),
    Result = string.join_list(" & ", list.map(WrapMath, Fields)) ++ " \\\\".

:- func maybe_expr(maybe(rendered_result)) = list(string).

maybe_expr(no) = [].
maybe_expr(yes(RR)) = [rr_expr(RR)].

:- func latex_table(config, list(row)) = string.

latex_table(Cfg, Rows) = Result :-
    Body = string.join_list("\n", list.map(latex_row, Rows)),
    Table =
        "\\begin{longtable}{" ++ col_spec(Cfg) ++ "}\n" ++
        "\\toprule\n" ++
        latex_header(Cfg) ++ " \\\\\n" ++
        "\\midrule\n" ++
        "\\endhead\n" ++
        Body ++ "\n" ++
        "\\bottomrule\n" ++
        "\\end{longtable}",
    ( if cfg_standalone(Cfg) = yes then
        Result = latex_preamble ++ Table ++ "\n\\end{document}\n"
    else
        Result = Table
    ).

%---------------------------------------------------------------------------%
% Text table
%---------------------------------------------------------------------------%

:- func text_header(config) = string.

text_header(Cfg) = Result :-
    Cols = ["θ"] ++
        ( if cfg_cols(Cfg) \= sin_only then ["cos θ"] else [] ) ++
        ( if cfg_cols(Cfg) \= cos_only then ["sin θ"] else [] ) ++
        ( if cfg_tan(Cfg) = yes then ["tan θ"] else [] ),
    Result = string.join_list("  │  ", Cols).

:- func text_row(row) = string.

text_row(R) = Result :-
    Fields = [row_angle(R)] ++
        maybe_expr(row_cos(R)) ++ maybe_expr(row_sin(R)) ++
        maybe_expr(row_tan(R)),
    Result = string.join_list("  │  ", Fields).

:- func text_table(config, list(row)) = string.

text_table(Cfg, Rows) = Result :-
    Hdr = text_header(Cfg),
    Sep = string.duplicate_char('-', string.length(Hdr)),
    Body = string.join_list("\n", list.map(text_row, Rows)),
    Result = Hdr ++ "\n" ++ Sep ++ "\n" ++ Body ++ "\n".

:- func gcd_int(int, int) = int.

gcd_int(A, B) = ( if B = 0 then int.abs(A) else gcd_int(B, A rem B) ).

%---------------------------------------------------------------------------%
% Tower output
%---------------------------------------------------------------------------%

:- func tower_angle(config, angle) = string.

tower_angle(Cfg, {P, Q}) = Result :-
    PI = integer.det_to_int(P),
    QI = integer.det_to_int(Q),
    G = gcd_int(int.abs(PI), imul(2, QI)),
    N = idiv(imul(2, QI), G),
    CosLabel = "\\cos " ++ latex_frac(P, Q),
    SinLabel = "\\sin " ++ latex_frac(P, Q),
    ( if all_periods_via_tower(N) = yes(TR) then
        CosDisp = extract_tower(CosLabel, tr_cos(TR)),
        SinDisp = extract_tower(SinLabel, tr_sin(TR)),
        CosOut = latex_tower(CosDisp),
        SinOut = latex_tower(SinDisp),
        ( if cfg_cols(Cfg) = cos_only then
            Sections = [CosOut]
        else if cfg_cols(Cfg) = sin_only then
            Sections = [SinOut]
        else
            Sections = [CosOut, "", SinOut]
        ),
        Result = string.join_list("\n", Sections)
    else
        TR0 = simplify_trig_result(cos_exact(P, Q)),
        ( if TR0 = radical(E) then
            Result = "\\begin{align*}\n& " ++ CosLabel ++ " = " ++
                latex(E) ++ "\n\\end{align*}\n"
        else
            Result = "\\begin{align*}\n& " ++ CosLabel ++
                " = \\text{(minpoly)}\n\\end{align*}\n"
        )
    ).

:- func tower_output(config, list(angle)) = string.

tower_output(Cfg, Angles) = Result :-
    Preamble = ( if cfg_standalone(Cfg) = yes then latex_preamble else "" ),
    Postamble = ( if cfg_standalone(Cfg) = yes then
        "\\end{document}\n" else "" ),
    Body = string.join_list("\n",
        list.map(tower_angle(Cfg), Angles)),
    Result = Preamble ++ Body ++ Postamble.

%---------------------------------------------------------------------------%
% Main
%---------------------------------------------------------------------------%

:- func usage = string.

usage =
    "Usage: demo_trig_table [OPTIONS] SPEC [SPEC ...]\n\n" ++
    "SPEC is one of:\n" ++
    "  N          tabulate cos/sin at kπ/N for k = 0..N\n" ++
    "  p/q        single angle pπ/q\n" ++
    "  N..M       tabulate for each denominator q in N..M\n\n" ++
    "Options:\n" ++
    "  --format=latex     LaTeX longtable (default)\n" ++
    "  --format=text      plain text (Unicode)\n" ++
    "  --format=tower     field extension tower display\n" ++
    "  --standalone       complete LaTeX document (default)\n" ++
    "  --no-standalone    table fragment only\n" ++
    "  --cos-only         omit sine column\n" ++
    "  --sin-only         omit cosine column\n" ++
    "  --tan              include tangent column\n".

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if parse_args(Args, Cfg, Specs) then
        Angles = list.condense(list.map(spec_angles, Specs)),
        ( if cfg_format(Cfg) = fmt_tower then
            io.write_string(tower_output(Cfg, Angles), !IO)
        else
            Rows = list.map(compute_row(Cfg), Angles),
            Output = ( if cfg_format(Cfg) = fmt_latex then
                latex_table(Cfg, Rows)
            else
                text_table(Cfg, Rows)
            ),
            io.write_string(Output, !IO)
        )
    else
        io.write_string("Error: no valid angle specifications given\n\n",
            !IO),
        io.write_string(usage, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module demo_trig_table.
%---------------------------------------------------------------------------%
