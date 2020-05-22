-module(example_success_1_basic).
-compile({parse_transform, hidden_pipe}).
-export([
         pipe_example/0,
         pipe_block_example/0,

         simple/0,
         double/0,
         chain/0,
         chain2/0,
         chain3/0,
         no_clash/0
        ]).

pipe_example() ->
    "Erlang parse_transforms are really flexible", string:uppercase(_), string:split(_, " ", all),
    ["ERLANG", "PARSE_TRANSFORMS" | _] = _,
    ok.

pipe_block_example() ->
    ["Erlang", "parse_transforms"] = begin "Erlang parse_transforms", string:split(_, " ") end,
    ok.

simple() ->
    A = ok,
    ok = _,
    A = _.

double() ->
    A = a,
    {{A, {a, a}, a}, a} = {{_, {_, _}, _}, _},
    ok.

chain() ->
    A = a,
    tuple(_), tuple(_), tuple(_),
    {{{A}}} = _,
    ok.

chain2() ->
    ok, id(_), id(_), id(_), id(_).

chain3() ->
    ok, id(_), id(_), id(_),
    A = _,
    error, id(_), id(_),
    error = _,
    A.

no_clash() ->
    _51_25 = ok,
    _52_26 = _51_25,
    _53_27 = _52_26.

%%====================================================================
%% Private functions
%%====================================================================

tuple(Term) -> {Term}.
id(Term) -> Term.

