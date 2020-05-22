%%%-------------------------------------------------------------------
%%% Part of hidden_pipe parse transform erlang app
%%% MIT License
%%% Copyright (c) 2020 Jose Maria Perez Ramos <josem.perez.ramos+git gmail com>
%%%-------------------------------------------------------------------
-module(hidden_pipe_SUITE).
-export([all/0, suite/0, success_compile_dir/1]).

all() -> [success_compile_dir].

suite() ->
    [{timetrap, {seconds, 30}}].

%%====================================================================
%% Test cases
%%====================================================================

success_compile_dir(_Config) ->
    MyDir = filename:dirname(code:which(?MODULE)),
    [ begin
            Rootname = filename:absname_join(MyDir, filename:rootname(SuccessfulFile)),
            CompileResult = compile:file(Rootname),
            ct:pal("Compile result for ~p is ~p", [SuccessfulFile, CompileResult]),
            {ok, ModuleName} = CompileResult,
            [ begin
                  OkResult = (catch ModuleName:Export()),
                  ct:pal("~p:~p() is ~p (it should be 'ok')", [ModuleName, Export, OkResult]),
                  ok = OkResult
              end || {Export, 0} <- ModuleName:module_info(exports), Export /= module_info]
      end || SuccessfulFile <- filelib:wildcard("*/example_success_*.erl", MyDir)].

