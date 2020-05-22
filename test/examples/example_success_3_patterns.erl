-module(example_success_3_patterns).
-compile({parse_transform, hidden_pipe}).
-export([
         simple/0,
         map/0,
         record/0,
         list_c/0,
         binary_c/0
        ]).

-record(record, {key, value}).

simple() ->
    {_, B} = {a, b},
    {{a, b}, b} = {_, B},
    ok.

map() ->
    #{a => 1},
    #{a := _} = _,
    ok.

record() ->
    #record{key = a, value = 1},
    #record{key = _, value = _} = _,
    ok.

list_c() ->
    [0,0,0,0,0,0] = [0 || _ <- lists:seq(1,3), _ <- lists:seq(1,2)],
    ok.

binary_c() ->
    [0, 2, 4] = [A || << A, _>> <= <<0,1,2,3,4,5>>],
    ok.

