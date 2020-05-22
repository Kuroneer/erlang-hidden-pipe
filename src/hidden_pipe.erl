%%%-------------------------------------------------------------------
%%% Part of hidden_pipe parse transform erlang app
%%% MIT License
%%% Copyright (c) 2020 Jose Maria Perez Ramos <josem.perez.ramos+git gmail com>
%%%-------------------------------------------------------------------
%%% TL;DR: This parse transform changes all _ in expressions to the result of
%%% the previous expression in the same expressions block.
%%%
%%% This parse transforms searchs for a specific variable name in expressions
%%% (_ by default, because it's 'unbound' in expressions). If it finds it
%%% anywhere but in the 1st expression in an expression block, it's switched with
%%% a new, automatic variable that matches the previous expression.
%%%
%%% Thus, the following (incorrect pure erlang) code
%%%
%%%   foo() ->
%%%       bar(),
%%%       case _ of
%%%         baz ->
%%%           a(),
%%%           b(_)
%%%       end,
%%%       baz(_).
%%%
%%% gets transformed internally into (the automatic names have stranger names,
%%% though)
%%%
%%%   foo() ->
%%%       _Auto1 = bar(),
%%%       _AUto3 = case _Auto1 of
%%%         baz ->
%%%           _Auto2 = a(),
%%%           b(_Auto2)
%%%       end,
%%%       baz(_Auto3).
%%%
%%% And it allows methods to be 'magically' chained with the use of '_':
%%%
%%%   pipe_example() ->
%%%       "Erlang parse_transforms are really flexible", string:uppercase(_), string:split(_, " ", all),
%%%       ["ERLANG", "PARSE_TRANSFORMS" | _] = _.
%%%
%%%   pipe_block_example() ->
%%%       ["Erlang", "parse_transforms"] = begin "Erlang parse_transforms", string:split(_, " ") end.
%%%
%%% Configuration of the module is performed through the attributes:
%%% -hidden_pipe({to_substitute|verbose, atom()}).
%%%
-module(hidden_pipe).
-export([parse_transform/2]).

%% Keys for process dictionary.
%% This module uses the process dictionary a lot, I know it's not the best
%% option, but it makes the AST traversal code much cleaner.
%% The process dictionary is used instead of a #state{} that would be passed
%% along.
-define(STACK, stack).
-define(MODULE_NAME, module_name).
-define(FUN_NAME, fun_name).
-define(VAR_NAMES, var_names).
-define(VAR_INDEX, var_index).
-define(VAR_SUBSTITUTION, var_substitution).
-define(TO_SUBSTITUTE, to_substitute).
-define(VERBOSE, verbose).


%%====================================================================
%% Parse transform
%%====================================================================

parse_transform(Forms, _Options) ->
    % Save old dict in case it has anything
    OldDict = erase(),

    % Set up
    put(?VAR_INDEX, 1),
    put(?TO_SUBSTITUTE, '_'),
    put(?VERBOSE, false),
    [put(?MODULE_NAME, ModuleName) || {attribute, _, module, ModuleName} <- Forms],
    % Allow to override by using -hidden_pipe attribute
    [put(Key, Value) || {attribute, _, ?MODULE, {Key, Value}} <- Forms],

    % GO!
    NewForms = [form(Form) || Form <- Forms],

    % Restore old dict
    erase(),
    [put(K, V) || {K, V} <- OldDict],
    NewForms.


form({function, Line, Name, Arity, Clauses}) ->
    SubstituteVarNameBin = atom_to_binary(get(?TO_SUBSTITUTE), utf8),
    VarNamesList = lists:flatten(var_names(Clauses)),
    case maps:from_list([{atom_to_binary(VarName, utf8), VarName} || VarName <- VarNamesList]) of
        #{SubstituteVarNameBin := _} = VarNames ->
            put(?STACK, []),
            put(?FUN_NAME, Name),
            put(?VAR_NAMES, VarNames),
            {function, Line, Name, Arity, [clause(Clause) || Clause <- Clauses]};
        _ ->
            {function, Line, Name, Arity, Clauses}
    end;
form(F) ->  F.


clause({clause,Line,Head,Guard,Body}) ->
    {clause, Line, Head, Guard, exprs(Body)}.


exprs([]) -> [];
exprs([H | T]) ->
    % Save current substitution to the stack
    put(?STACK, [erase(?VAR_SUBSTITUTION) | get(?STACK)]),
    exprs_tail(expr(H), T).
exprs_tail(Previous, [Current| RestExpressions]) ->
    Line = element(2, Previous),
    put(?VAR_SUBSTITUTION, {line, Line}),

    % If the new var was used in Current, add a match to Previous
    ParsedCurrent = expr(Current),
    [case erase(?VAR_SUBSTITUTION) of
         {atom, NewVarNameAtom} ->
             case get(?VERBOSE) of
                 true ->
                    PrintArgs = [?MODULE, get(?MODULE_NAME), get(?FUN_NAME), Line],
                    io:format("~p: ~p:~p:~p Used in next expression~n", PrintArgs);
                 _ -> ok
             end,
             {match, Line, {var, Line, NewVarNameAtom}, Previous};
         _ -> Previous
     end | exprs_tail(ParsedCurrent, RestExpressions)];
exprs_tail(PreviousExpression, []) ->
    % Recover previous substitution
    [OldStackVarSubstitution | OldStack] = get(?STACK),
    put(?STACK, OldStack),
    put(?VAR_SUBSTITUTION, OldStackVarSubstitution),
    [PreviousExpression].


% If the name for the variable to be substituted is found, substitute it
% (creating a new var if required)
expr({var, Line, OriginalVarName}) ->
    {var, Line, case {get(?TO_SUBSTITUTE), get(?VAR_SUBSTITUTION)} of
                    {OriginalVarName, {line, ExpressionLine}} ->
                        NewVarNameAtom = binary_to_atom(new_var_name(ExpressionLine), utf8),
                        put(?VAR_SUBSTITUTION, {atom, NewVarNameAtom}),
                        NewVarNameAtom;
                    {OriginalVarName, {atom, VarNameAtom}} ->  VarNameAtom;
                    _ -> OriginalVarName
                end};

% Push to expression block stack
% These create new blocks of expressions,  so they need to call exprs
expr({block, Line, Es}) -> {block, Line, exprs(Es)};
expr({'case', Line, E, Cs}) -> {'case', Line,  expr(E),  [clause(C) || C <- Cs]};
expr({'if', Line, Cs}) -> {'if', Line, [clause(C) || C <- Cs]};
expr({'receive', Line, Cs}) -> {'receive', Line, [clause(C) || C <- Cs]};
expr({'receive', Line, Cs, To, ToEs}) -> {'receive', Line, [clause(C) || C <- Cs], expr(To), exprs(ToEs)};
expr({'try', Line, Es, Scs, Ccs, As}) -> {'try', Line, exprs(Es), [clause(Sc) || Sc <- Scs], [clause(Cc) || Cc <- Ccs], exprs(As)};
expr({named_fun, Line, Name, Cs}) -> {named_fun, Line, Name, [clause(C) || C <- Cs]};
expr({'fun', Line, {clauses, Cs}}) -> {'fun', Line, {clauses, [clause(C) || C <- Cs]}};

% Patterns here (list/binary comprehension + match)
% Since these contain Patterns, they need to be iterated 'manually' to avoid
% modifying the patterns
expr({lc, Line, E, Qs}) -> {lc, Line, expr(E), [lc_bc_qual(Q) || Q <- Qs]};
expr({bc, Line, E, Qs}) -> {bc, Line, expr(E), [lc_bc_qual(Q) || Q <- Qs]};
expr({match, Line, P, E}) -> {match, Line, P, expr(E)};

% Default tree search
expr(Es) when is_list(Es) -> [expr(E) || E <- Es];
expr(E) when is_tuple(E), tuple_size(E) > 2 ->
    [Key, Line | Rest] = tuple_to_list(E),
    list_to_tuple([Key, Line | expr(Rest)]);
expr(E) -> E.


lc_bc_qual({generate,Line,P,E}) -> {generate,Line,P,expr(E)};
lc_bc_qual({b_generate,Line,P,E}) -> {b_generate,Line,P,expr(E)};
lc_bc_qual(E0) -> expr(E0).


%%====================================================================
%% Aux functions
%%====================================================================

var_names({var, _Line, VarName}) -> [VarName];
var_names([]) -> [];
var_names([H | T]) -> [var_names(H) | var_names(T)];
var_names(T) when is_tuple(T), tuple_size(T) > 2 -> var_names(tl(tl(tuple_to_list(T))));
var_names(_T) -> [].


new_var_name(Line) ->
    new_var_name(Line, get(?VAR_NAMES)).
new_var_name(Line, VarNames) ->
    Index = put(?VAR_INDEX, get(?VAR_INDEX) + 1),
    Candidate = <<"_", (integer_to_binary(Line))/binary, "_", (integer_to_binary(Index))/binary>>,
    case VarNames of
        #{Candidate := _} ->
            new_var_name(Line, VarNames);
        _ ->
            Candidate
    end.

