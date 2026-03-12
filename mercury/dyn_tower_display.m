%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% File: dyn_tower_display.m
% Main author: nyc
%
% Display tower elements as structured field extension towers.
%
% Instead of expanding tower elements into radical trees (which explode
% exponentially for solvable quintics etc.), this module renders the
% tower structure directly: each level is shown as a field extension
% with its minimal polynomial, and elements are polynomials in the
% generator of each level.
%
% This follows Gauss's own presentation style from the Disquisitiones
% Arithmeticae.
%
%---------------------------------------------------------------------------%

:- module dyn_tower_display.
:- interface.

:- import_module dyn_tower.
:- import_module list.

%---------------------------------------------------------------------------%

:- type extension_step
    --->    extension_step(
                es_level      :: tower_level,
                es_name       :: string,
                es_latex_name :: string,
                es_degree     :: int,
                es_radicand   :: tower_elem
            ).

:- type tower_display
    --->    tower_display(
                td_steps   :: list(extension_step),
                td_element :: tower_elem,
                td_label   :: string
            ).

    % Extract the tower structure from a TowerElem.
    % Collects all distinct tower levels, merges equivalent levels,
    % and assigns Greek letter names.
    %
:- func extract_tower(string, tower_elem) = tower_display.

    % Render a tower element as a complete LaTeX display.
    %
:- func latex_tower(tower_display) = string.

    % Render a single tower element as LaTeX using the tower's name map.
    %
:- func latex_tower_elem(tower_display, tower_elem) = string.

    % Render a tower element as a complete text display.
    %
:- func pretty_tower(tower_display) = string.

    % Render a single tower element as text using the tower's name map.
    %
:- func pretty_tower_elem(tower_display, tower_elem) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module integer.
:- use_module int.
:- import_module map.
:- import_module rational.
:- use_module string.

%---------------------------------------------------------------------------%
% Canonical form for radicand equivalence
%---------------------------------------------------------------------------%

:- type canon_elem
    --->    c_rat(rational)
    ;       c_ext(list(canon_elem), int, canon_elem).

:- func canonicalize(tower_elem) = canon_elem.

canonicalize(E) = Result :-
    ( if E = t_rat(R) then
        Result = c_rat(R)
    else if E = t_ext(Cs, Lvl) then
        ( if Cs = [C | Rest], list.all_true(t_is_zero, Rest) then
            Result = canonicalize(C)
        else
            Result = c_ext(
                list.map(canonicalize, Cs),
                tl_root_deg(Lvl),
                canonicalize(tl_radicand(Lvl)))
        )
    else
        Result = c_rat(rational.zero)
    ).

:- pred radicand_equiv(tower_elem::in, tower_elem::in) is semidet.

radicand_equiv(A, B) :-
    canonicalize(A) = canonicalize(B).

%---------------------------------------------------------------------------%
% Collect levels
%---------------------------------------------------------------------------%

:- func collect_levels(tower_elem) = map(int, tower_level).

collect_levels(E) = Result :-
    ( if E = t_rat(_) then
        Result = map.init
    else if E = t_ext(Cs, Lvl) then
        FromCoeffs = list.foldl(
            ( func(C, Acc) = map.overlay(collect_levels(C), Acc) ),
            Cs, map.init),
        FromRadicand = collect_levels(tl_radicand(Lvl)),
        Result = map.set(
            map.overlay(FromCoeffs, FromRadicand),
            tl_id(Lvl), Lvl)
    else
        Result = map.init
    ).

%---------------------------------------------------------------------------%
% Merge equivalent levels and assign names
%---------------------------------------------------------------------------%

:- func merge_equiv_levels(list(tower_level))
    = {list(extension_step), map(int, extension_step)}.

merge_equiv_levels(Lvls) = merge_loop(Lvls, 1).

:- func merge_loop(list(tower_level), int)
    = {list(extension_step), map(int, extension_step)}.

merge_loop([], _) = {[], map.init}.
merge_loop([L | Ls], NextName) = {[Step | RestSteps], Map} :-
    classify_levels(L, Ls, Equivs, NonEquivs),
    Step = extension_step(
        L,
        greek_name(NextName),
        greek_latex_name(NextName),
        tl_degree(L),
        tl_radicand(L)),
    IdEntries = [{tl_id(L), Step} |
        list.map(( func(Eq) = {tl_id(Eq), Step} ), Equivs)],
    {RestSteps, RestMap} = merge_loop(NonEquivs, int.'+'(NextName, 1)),
    Map = list.foldl(
        ( func({Id, S}, Acc) = map.set(Acc, Id, S) ),
        IdEntries, RestMap).

:- pred classify_levels(tower_level::in, list(tower_level)::in,
    list(tower_level)::out, list(tower_level)::out) is det.

classify_levels(_, [], [], []).
classify_levels(L, [L2 | Rest], Equivs, NonEquivs) :-
    classify_levels(L, Rest, RestEq, RestNE),
    ( if tl_degree(L) = tl_degree(L2),
         radicand_equiv(tl_radicand(L), tl_radicand(L2))
    then
        Equivs = [L2 | RestEq],
        NonEquivs = RestNE
    else
        Equivs = RestEq,
        NonEquivs = [L2 | RestNE]
    ).

%---------------------------------------------------------------------------%
% Greek letter naming
%---------------------------------------------------------------------------%

:- func greek_name(int) = string.

greek_name(I) = Result :-
    Names = ["α", "β", "γ", "δ", "ε", "ζ_t", "η_t", "θ_t"],
    ( if int.'>='(I, 1), int.'=<'(I, 8) then
        Result = list.det_index0(Names, int.'-'(I, 1))
    else
        Result = string.append("α", string.int_to_string(int.'-'(I, 7)))
    ).

:- func greek_latex_name(int) = string.

greek_latex_name(I) = Result :-
    Names = ["\\alpha", "\\beta", "\\gamma", "\\delta",
             "\\varepsilon", "\\zeta_t", "\\eta_t", "\\theta_t"],
    ( if int.'>='(I, 1), int.'=<'(I, 8) then
        Result = list.det_index0(Names, int.'-'(I, 1))
    else
        Result = string.append_list(["\\alpha_{",
            string.int_to_string(int.'-'(I, 7)), "}"])
    ).

%---------------------------------------------------------------------------%
% Extract tower
%---------------------------------------------------------------------------%

extract_tower(Label, E) = tower_display(Steps, E, Label) :-
    Levels = collect_levels(E),
    list.sort(
        ( pred(A::in, B::in, Cmp::out) is det :-
            compare(Cmp, tl_id(A), tl_id(B))
        ),
        map.values(Levels), Sorted),
    {Steps, _} = merge_equiv_levels(Sorted).

%---------------------------------------------------------------------------%
% Name map
%---------------------------------------------------------------------------%

:- type name_map == map(int, extension_step).

:- func build_name_map(tower_display) = name_map.

build_name_map(TD) = Result :-
    Levels = collect_levels(td_element(TD)),
    Steps = td_steps(TD),
    Result = map.foldl(
        ( func(Lid, Lvl, Acc) = map.set(Acc, Lid, find_step(Steps, Lvl)) ),
        Levels, map.init).

:- func find_step(list(extension_step), tower_level) = extension_step.

find_step(Steps, Lvl) = Result :-
    Matches = list.filter(
        ( pred(S::in) is semidet :-
            es_degree(S) = tl_degree(Lvl),
            radicand_equiv(es_radicand(S), tl_radicand(Lvl))
        ),
        Steps),
    ( if Matches = [M | _] then
        Result = M
    else
        % Fallback: find by exact level ID match
        IDMatches = list.filter(
            ( pred(S::in) is semidet :-
                tl_id(es_level(S)) = tl_id(Lvl)
            ),
            Steps),
        ( if IDMatches = [M2 | _] then
            Result = M2
        else if Steps = [S1 | _] then
            Result = S1
        else
            Result = extension_step(Lvl, "?", "?", tl_degree(Lvl),
                tl_radicand(Lvl))
        )
    ).

%---------------------------------------------------------------------------%
% LaTeX rendering
%---------------------------------------------------------------------------%

latex_tower(TD) = Result :-
    NM = build_name_map(TD),
    StepLines = list.condense(
        list.map(( func(S) = latex_step(NM, S) ), td_steps(TD))),
    ElemStr = latex_te(NM, td_element(TD)),
    ElemLines = latex_elem_lines(td_label(TD), ElemStr),
    Header = ( if td_steps(TD) = [] then [] else
        ["& & \\textbf{Tower of extensions:} \\\\"] ),
    AllLines = ["\\begin{alignat*}{2}"] ++ Header ++ StepLines ++
        ElemLines ++ ["\\end{alignat*}"],
    Result = string.join_list("\n", AllLines).

:- func latex_step(name_map, extension_step) = list(string).

latex_step(NM, Step) = Result :-
    Name = es_latex_name(Step),
    N = es_degree(Step),
    Rad = latex_te(NM, es_radicand(Step)),
    LHS = string.append_list([Name, "^{",
        string.int_to_string(N), "}"]),
    RootE = root_expr(N, Rad),
    BracketInline = string.append_list(["\\bigl[", Name, " = ",
        RootE, "\\bigr]"]),
    ( if int.'=<'(string.length(Rad), 80) then
        Result = [string.append_list([LHS, " &= &", Rad, " \\qquad ",
            BracketInline, " \\\\"])]
    else
        BracketRow = string.append_list(["\\bigl[", Name, " &= &",
            RootE, "\\bigr]"]),
        Result = [string.append_list([LHS, " &= &", Rad, " \\\\"]),
                  string.append(BracketRow, " \\\\")]
    ).

:- func root_expr(int, string) = string.

root_expr(N, R) = Result :-
    ( if N = 2 then
        Result = string.append_list(["\\sqrt{", R, "}"])
    else if N = 3 then
        Result = string.append_list(["\\sqrt[3]{", R, "}"])
    else
        Result = string.append_list(["\\sqrt[",
            string.int_to_string(N), "]{", R, "}"])
    ).

:- func latex_elem_lines(string, string) = list(string).

latex_elem_lines(Label, Expr) = Result :-
    ( if int.'=<'(string.length(Expr), 120) then
        Result = [string.append_list([Label, " &= &", Expr])]
    else
        Result = [string.append_list([Label, " &= &", Expr])]
    ).

latex_tower_elem(TD, E) = latex_te(build_name_map(TD), E).

:- func latex_te(name_map, tower_elem) = string.

latex_te(_, t_rat(R)) = latex_rat(R).
latex_te(NM, t_ext(Cs, Lvl)) = Result :-
    ( if map.search(NM, tl_id(Lvl), Step) then
        Name = es_latex_name(Step),
        Terms = collect_terms(Name, NM, Cs),
        ( if Terms = [] then
            Result = "0"
        else
            Result = render_latex_terms(Terms)
        )
    else
        Result = string.append_list(["?_{",
            string.int_to_string(tl_id(Lvl)), "}"])
    ).

:- func collect_terms(string, name_map, list(tower_elem))
    = list({bool, string}).

collect_terms(Name, NM, Cs) = Result :-
    Result = collect_terms_loop(Name, NM, Cs, 0).

:- func collect_terms_loop(string, name_map, list(tower_elem), int)
    = list({bool, string}).

collect_terms_loop(_, _, [], _) = [].
collect_terms_loop(Name, NM, [C | Cs], I) = Result :-
    Rest = collect_terms_loop(Name, NM, Cs, int.'+'(I, 1)),
    ( if t_is_zero(C) then
        Result = Rest
    else
        {Pos, Rendered} = render_coeff_term(Name, NM, C, I),
        Result = [{Pos, Rendered} | Rest]
    ).

:- func render_coeff_term(string, name_map, tower_elem, int)
    = {bool, string}.

render_coeff_term(Name, NM, C, I) = Result :-
    ( if I = 0 then
        S0 = latex_te(NM, C),
        ( if string.index(S0, 0, '-') then
            Result = {no, string.right(S0, int.'-'(string.length(S0), 1))}
        else
            Result = {yes, S0}
        )
    else
        GenPart = ( if I = 1 then Name
                    else string.append_list([Name, "^{",
                        string.int_to_string(I), "}"]) ),
        ( if C = t_rat(R) then
            ( if R = rational.one then
                Result = {yes, GenPart}
            else if R = rational.'-'(rational.zero, rational.one) then
                Result = {no, GenPart}
            else if rational.'>'(R, rational.zero) then
                Result = {yes, string.append_list([latex_rat(R), " ",
                    GenPart])}
            else
                NegR = rational.'-'(rational.zero, R),
                Result = {no, string.append_list([latex_rat(NegR), " ",
                    GenPart])}
            )
        else
            S1 = latex_te(NM, C),
            ( if string.index(S1, 0, '-') then
                Rest = string.right(S1, int.'-'(string.length(S1), 1)),
                Result = {no, string.append_list([
                    wrap_if_compound(Rest), " ", GenPart])}
            else
                Result = {yes, string.append_list([
                    wrap_if_compound(S1), " ", GenPart])}
            )
        )
    ).

:- func wrap_if_compound(string) = string.

wrap_if_compound(S) = Result :-
    ( if int.'>'(string.length(S), 1),
         Rest = string.right(S, int.'-'(string.length(S), 1)),
         ( string.contains_char(Rest, '+')
         ; string.contains_char(Rest, '-') )
    then
        Result = string.append_list([
            "\\left(", S, "\\right)", " \\cdot"])
    else
        Result = S
    ).

:- func render_latex_terms(list({bool, string})) = string.

render_latex_terms([]) = "0".
render_latex_terms([{Sign, T} | Rest]) = Result :-
    Hd = ( if Sign = yes then T else string.append("-", T) ),
    Result = string.append(Hd,
        string.append_list(list.map(render_rest_latex, Rest))).

:- func render_rest_latex({bool, string}) = string.

render_rest_latex({yes, E}) = string.append_list([" + ", E]).
render_rest_latex({no, E}) = string.append_list([" - ", E]).

:- func latex_rat(rational) = string.

latex_rat(R) = Result :-
    N = numer(R),
    D = denom(R),
    ( if D = integer.one then
        Result = integer.to_string(N)
    else if integer.'<'(N, integer.zero) then
        Result = string.append_list(["-\\frac{",
            integer.to_string(integer.abs(N)), "}{",
            integer.to_string(D), "}"])
    else
        Result = string.append_list(["\\frac{",
            integer.to_string(N), "}{",
            integer.to_string(D), "}"])
    ).

%---------------------------------------------------------------------------%
% Text rendering
%---------------------------------------------------------------------------%

pretty_tower(TD) = Result :-
    NM = build_name_map(TD),
    StepLines = list.map(
        ( func(S) = pretty_step(NM, S) ), td_steps(TD)),
    ElemStr = pretty_te(NM, td_element(TD)),
    Header = ( if td_steps(TD) = [] then [] else
        ["Tower of extensions:"] ),
    AllLines = Header ++ StepLines ++
        ["", string.append_list([td_label(TD), " = ", ElemStr])],
    Result = string.join_list("\n", AllLines).

:- func pretty_step(name_map, extension_step) = string.

pretty_step(NM, Step) = Result :-
    Name = es_name(Step),
    N = es_degree(Step),
    Rad = pretty_te(NM, es_radicand(Step)),
    RootStr = ( if N = 2 then "√"
                else if N = 3 then "∛"
                else string.append(string.int_to_string(N), "√") ),
    Result = string.append_list(["  ", Name, "^",
        string.int_to_string(N), " = ", Rad,
        "    [", Name, " = ", RootStr, "(", Rad, ")]"]).

pretty_tower_elem(TD, E) = pretty_te(build_name_map(TD), E).

:- func pretty_te(name_map, tower_elem) = string.

pretty_te(_, t_rat(R)) = pretty_rat(R).
pretty_te(NM, t_ext(Cs, Lvl)) = Result :-
    ( if map.search(NM, tl_id(Lvl), Step) then
        Name = es_name(Step),
        Terms = collect_text_terms(Name, NM, Cs),
        ( if Terms = [] then
            Result = "0"
        else
            Result = render_text_terms(Terms)
        )
    else
        Result = string.append("?_", string.int_to_string(tl_id(Lvl)))
    ).

:- func collect_text_terms(string, name_map, list(tower_elem))
    = list({bool, string}).

collect_text_terms(Name, NM, Cs) =
    collect_text_terms_loop(Name, NM, Cs, 0).

:- func collect_text_terms_loop(string, name_map, list(tower_elem), int)
    = list({bool, string}).

collect_text_terms_loop(_, _, [], _) = [].
collect_text_terms_loop(Name, NM, [C | Cs], I) = Result :-
    Rest = collect_text_terms_loop(Name, NM, Cs, int.'+'(I, 1)),
    ( if t_is_zero(C) then
        Result = Rest
    else
        {Pos, Rendered} = render_text_coeff_term(Name, NM, C, I),
        Result = [{Pos, Rendered} | Rest]
    ).

:- func render_text_coeff_term(string, name_map, tower_elem, int)
    = {bool, string}.

render_text_coeff_term(Name, NM, C, I) = Result :-
    ( if I = 0 then
        S0 = pretty_te(NM, C),
        ( if string.index(S0, 0, '-') then
            Result = {no, string.right(S0, int.'-'(string.length(S0), 1))}
        else
            Result = {yes, S0}
        )
    else
        GenPart = ( if I = 1 then Name
                    else string.append_list([Name, "^",
                        string.int_to_string(I)]) ),
        ( if C = t_rat(R) then
            ( if R = rational.one then
                Result = {yes, GenPart}
            else if R = rational.'-'(rational.zero, rational.one) then
                Result = {no, GenPart}
            else if rational.'>'(R, rational.zero) then
                Result = {yes, string.append_list([pretty_rat(R), "·",
                    GenPart])}
            else
                NegR = rational.'-'(rational.zero, R),
                Result = {no, string.append_list([pretty_rat(NegR), "·",
                    GenPart])}
            )
        else
            S1 = pretty_te(NM, C),
            ( if string.index(S1, 0, '-') then
                Rest = string.right(S1, int.'-'(string.length(S1), 1)),
                Result = {no, string.append_list([
                    "(", Rest, ")·", GenPart])}
            else if int.'>'(string.length(S1), 1),
                    InnerS = string.right(S1,
                        int.'-'(string.length(S1), 1)),
                    ( string.contains_char(InnerS, '+')
                    ; string.contains_char(InnerS, '-') )
            then
                Result = {yes, string.append_list([
                    "(", S1, ")·", GenPart])}
            else
                Result = {yes, string.append_list([
                    S1, "·", GenPart])}
            )
        )
    ).

:- func render_text_terms(list({bool, string})) = string.

render_text_terms([]) = "0".
render_text_terms([{Sign, T} | Rest]) = Result :-
    Hd = ( if Sign = yes then T else string.append("-", T) ),
    Result = string.append(Hd,
        string.append_list(list.map(render_rest_text, Rest))).

:- func render_rest_text({bool, string}) = string.

render_rest_text({yes, E}) = string.append_list([" + ", E]).
render_rest_text({no, E}) = string.append_list([" - ", E]).

:- func pretty_rat(rational) = string.

pretty_rat(R) = Result :-
    N = numer(R),
    D = denom(R),
    ( if D = integer.one then
        Result = integer.to_string(N)
    else if integer.'<'(N, integer.zero) then
        Result = string.append_list(["(-",
            integer.to_string(integer.abs(N)), "/",
            integer.to_string(D), ")"])
    else
        Result = string.append_list(["(",
            integer.to_string(N), "/",
            integer.to_string(D), ")"])
    ).

%---------------------------------------------------------------------------%
:- end_module dyn_tower_display.
%---------------------------------------------------------------------------%
