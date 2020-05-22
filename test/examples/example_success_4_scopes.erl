-module(example_success_4_scopes).
-compile({parse_transform, hidden_pipe}).
-export([
         op_case1/0,
         op_case2/0,
         op_case3/0,
         op_block/0,
         op_if1/0,
         op_if2/0,
         op_receive_timeout/0,
         op_receive/0,
         op_try1/0,
         op_try2/0,
         op_try3/0,
         op_named_fun/0,
         op_fun/0
        ]).

op_case1() ->
    op_case(a),
    {{a}} = _,
    ok.

op_case2() ->
    op_case(b),
    {{b}} = _,
    ok.

op_case3() ->
    op_case(c),
    {undefined} = _,
    ok.

op_block() ->
    tuple(a),
    begin
        tuple(c),
        tuple(_),
        _
    end,
    {{c}} = _,
    ok.

op_if1() ->
    op_if(false),
    {{b}} = _,
    ok.

op_if2() ->
    op_if(true),
    {{a}} = _,
    ok.

op_receive_timeout() ->
    Ref = make_ref(),
    0,
    receive Ref -> ok
    after _ -> ok
    end.

op_receive() ->
    a,
    self() ! _,
    receive b -> undefined;
            a -> c,
                 tuple(_)
    end,
    {c} = _,
    ok.

op_try1() ->
    try a, tuple(_) of
        {a} -> ok;
        _ -> error
    catch
        _:_:_ -> error
    end,
    _.

op_try2() ->
    try tuple(a) of
        {a} ->
            b,
            tuple(_);
        _ -> error
    catch
        _:_:_ -> error
    end,
    {b} = _,
    ok.

op_try3() ->
    try throw(c) of
        {a} ->
            b,
            tuple(_);
        _ -> error
    catch
        _:c:_ -> error,
                 tuple(_)
    end,
    {error} = _,
    ok.

op_named_fun() ->
    a,
    F = fun A(In) when is_function(In) -> A, tuple(_);
            A(In) -> In, _, _, _, _
        end,
    {_} = _(_),
    F(ok).

op_fun() ->
    a,
    F = fun(In) when is_function(In) -> In, tuple(_);
           (In) -> In, _, _, _, _
        end,
    _(_),
    {F} = _,
    F(ok).

%%====================================================================
%% Private functions
%%====================================================================

tuple(Term) -> {Term}.

op_case(Element) ->
    tuple(Element),
    case _ of
        {a} ->
            tuple(a),
            _;
        {b} ->
            tuple(b);
        _ -> undefined
    end,
    tuple(_).

op_if(Boolean) ->
    if Boolean ->
           a,
           tuple(_);
       true ->
           tuple(b)
    end,
    tuple(_).

