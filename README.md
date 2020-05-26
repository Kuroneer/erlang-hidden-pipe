# Hidden Pipe

Erlang `parse_transform` that changes all `_` in expressions to the result of
the previous expression in the same expressions block.

It's possible to write code that behaves similarly to the `|>` (pipe) operator.

## In detail:

This parse transforms searchs for a specific variable name in expressions
(`_` by default, because it's unbound in expressions). If it finds it
anywhere but in the 1st expression in an expression block, it's switched with
a new, automatic variable that matches the previous expression.

Thus, the following (incorrect pure erlang) code

```erlang
foo() ->
    bar(),
    case _ of
      baz ->
        a(),
        b(_)
    end,
    baz(_).
```

gets transformed internally into (the automatic vars have stranger names,
though)

```erlang
foo() ->
    _Auto1 = bar(),
    _AUto3 = case _Auto1 of
      baz ->
        _Auto2 = a(),
        b(_Auto2)
    end,
    baz(_Auto3).
```

And it allows methods to be 'magically' chained with the use of `_` (Akin to
`|>`, but without the operator and with magic variables because I did not want
to deal with the tokenizer):

```erlang
pipe_example() ->
    "Erlang parse_transforms are really flexible", string:uppercase(_), string:split(_, " ", all),
    ["ERLANG", "PARSE_TRANSFORMS" | _] = _.

pipe_block_example() ->
    ["Erlang", "parse_transforms"] = begin "Erlang parse_transforms", string:split(_, " ") end.

-define(pipe, begin).
pipe_block_example2() ->
    ["Erlang", "parse_transforms"] = ?pipe "Erlang parse_transforms", string:split(_, " ") end,
    ok.

```

Keep in mind that the Patterns are unchanged, so you can still do things like
```erlang
{_, B} = {a, b},
<<0 || _ <- lists:seq(0,256)>>,
[A || << A, _>> <= <<0,1,2,3,4,5>>],

```
More examples can be found in the [tests dir](test/examples)

## How to use:

Include it as a dependency and add the `parse_transform` compile option to each
module or globally:

```erlang
-compile({parse_transform, hidden_pipe}).
```

The `-hidden_pipe({to_substitute, atom()}).` and `-hidden_pipe({verbose, boolean()}).`
attributes allow you to control the behaviour of this module.

## Disclaimer:

I've yet to decide if this `parse_transform` is a good or a terrible idea.

## Run tests:
```
rebar3 ct
```

## Authors

* **Jose M Perez Ramos** - [Kuroneer](https://github.com/Kuroneer)

## License

[MIT License](LICENSE)

