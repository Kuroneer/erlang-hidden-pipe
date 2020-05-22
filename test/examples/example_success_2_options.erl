-module(example_success_2_options).
-compile({parse_transform, hidden_pipe}).
-hidden_pipe({to_substitute, 'Automatic'}).
-export([
         simple/0,
         chain/0,
         chain2/0,
         chain3/0
        ]).

simple() ->
    A = ok,
    ok = Automatic,
    A = Automatic.

chain() ->
    A = a,
    tuple(Automatic), tuple(Automatic), tuple(Automatic),
    {{{A}}} = Automatic,
    ok.

chain2() ->
    ok, id(Automatic), id(Automatic), id(Automatic), id(Automatic).

chain3() ->
    ok, id(Automatic), id(Automatic), id(Automatic),
    A = Automatic,
    error, id(Automatic), id(Automatic),
    error = Automatic,
    A.

%%====================================================================
%% Private functions
%%====================================================================

tuple(Term) -> {Term}.
id(Term) -> Term.

