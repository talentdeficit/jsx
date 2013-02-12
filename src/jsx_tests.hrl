%% data and helper functions for tests

-export([init/1, handle_event/2]).
-export([empty_array/0, deep_array/0, really_deep_array/0]).
-export([empty_object/0]).
-export([literals/0, naked_literals/0]).
-export([integers/0, naked_integers/0]).
-export([floats/0, naked_floats/0]).
-export([decodeables/0]).


-include_lib("eunit/include/eunit.hrl").


%% test handler
init([]) -> [].

handle_event(end_json, State) -> lists:reverse([end_json] ++ State);
handle_event(Event, State) -> [Event] ++ State.


empty_array() -> [{"[]", <<"[]">>, [], [start_array, end_array]}].

deep_array() ->
    [Test] = empty_array(),
    [repeat(fun wrap_with_array/1, Test, 10)].

really_deep_array() ->
    [Test] = empty_array(),
    [repeat(fun wrap_with_array/1, Test, 1000)].


empty_object() -> [{"{}", <<"{}">>, [{}], [start_object, end_object]}].


naked_integers() ->
    Raw = [
        1, 2, 3,
        127, 128, 129,
        255, 256, 257,
        65534, 65535, 65536,
        18446744073709551616,
        18446744073709551617
    ],
    [
        {
            integer_to_list(X),
            list_to_binary(integer_to_list(X)),
            X,
            [{integer, X}]
        }
        || X <- Raw ++ [ -1 * Y || Y <- Raw ] ++ [0]
    ].

integers() ->
    [ wrap_with_array(Test) || Test <- naked_integers() ]
    ++ [ wrap_with_object(Test) || Test <- naked_integers() ]
    ++ [listify("naked integers", naked_integers())]
    ++ [objectify("naked integers", naked_integers())].


naked_floats() ->
    Raw = [
        0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
        1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9,
        1234567890.0987654321,
        0.0e0,
        1234567890.0987654321e16,
        0.1e0, 0.1e1, 0.1e2, 0.1e4, 0.1e8, 0.1e16, 0.1e308,
        1.0e0, 1.0e1, 1.0e2, 1.0e4, 1.0e8, 1.0e16, 1.0e308,
        2.2250738585072014e-308,    %% min normalized float
        1.7976931348623157e308,     %% max normalized float
        5.0e-324,                   %% min denormalized float
        2.225073858507201e-308      %% max denormalized float
    ],
    [
        {
            sane_float_to_list(X),
            list_to_binary(sane_float_to_list(X)),
            X,
            [{float, X}]
        }
        || X <- Raw ++ [ -1 * Y || Y <- Raw ]
    ].

floats() ->
    [ wrap_with_array(Test) || Test <- naked_floats() ]
    ++ [ wrap_with_object(Test) || Test <- naked_floats() ]
    ++ [listify("naked floats", naked_floats())]
    ++ [objectify("naked floats", naked_floats())].


decodeables() ->
    Tests  =  [
        {"-0.0", <<"-0.0">>, 0.0, [{float, 0.0}]},
        {"1e0", <<"1e0">>, 1.0, [{float, 1.0}]},
        {"0e0", <<"0e0">>, 0.0, [{float, 0.0}]},
        {"1e4", <<"1e4">>, 1.0e4, [{float, 1.0e4}]},
        {"0e4", <<"0e4">>, 0.0, [{float, 0.0}]},
        {"-1e0", <<"-1e0">>, -1.0, [{float, -1.0}]},
        {"-0", <<"-0">>, 0, [{integer, 0}]}
    ],
    [ wrap_with_array(Test) || Test <- Tests ]
    ++ [ wrap_with_object(Test) || Test <- Tests ]
    ++ [listify("naked decodeables", Tests)]
    ++ [objectify("naked decodeables", Tests)].


sane_float_to_list(X) ->
    [Output] = io_lib:format("~p", [X]),
    Output.


naked_literals() ->
    [
        {
            atom_to_list(Literal),
            atom_to_binary(Literal, unicode),
            Literal,
            [{literal, Literal}]
        }
        || Literal <- [true, false, null]
    ].

literals() ->
    [ wrap_with_array(Test) || Test <- naked_literals() ]
    ++ [ wrap_with_object(Test) || Test <- naked_literals() ]
    ++ [listify("naked literals", naked_literals())]
    ++ [objectify("naked literals", naked_literals())].


wrap_with_array({Title, JSON, Term, Events}) ->
    {
        "[" ++ Title ++ "]",
        <<"[", JSON/binary, "]">>,
        [Term],
        [start_array] ++ Events ++ [end_array]
    }.


wrap_with_object({Title, JSON, Term, Events}) ->
    {
        "{\"key\":" ++ Title ++ "}",
        <<"{\"key\":", JSON/binary, "}">>,
        [{<<"key">>, Term}],
        [start_object, {key, <<"key">>}] ++ Events ++ [end_object]
    }.


repeat(_, Test, 0) -> Test;
repeat(Fun, Test, Times) -> repeat(Fun, Fun(Test), Times - 1).


listify(Title, [{_, JSON, Term, Events}|Rest]) -> do_listify(Rest, {Title, JSON, [Term], Events}).

do_listify([], {Title, JSON, Term, Events}) ->
    {
        Title,
        <<"["/utf8, JSON/binary, "]"/utf8>>,
        Term,
        [start_array] ++ Events ++ [end_array]
    };
do_listify([Test|Rest], Acc) ->
    {Title, A, M, X} = Acc,
    {_, B, N, Y} = Test,
    do_listify(Rest, {Title, <<A/binary, ","/utf8, B/binary>>, M ++ [N], X ++ Y}).


objectify(Title, [{_, JSON, Term, Events}|Rest]) ->
    do_objectify(
        Rest,
        {Title, <<"\"", JSON/binary, "\":", JSON/binary>>, [{JSON, Term}], [{key, JSON}] ++ Events}
    ).

do_objectify([], {Title, JSON, Term, Events}) ->
    {
        Title,
        <<"{"/utf8, JSON/binary, "}"/utf8>>,
        Term,
        [start_object] ++ Events ++ [end_object]
    };
do_objectify([Test|Rest], Acc) ->
    {Title, A, M, X} = Acc,
    {_, B, N, Y} = Test,
    do_objectify(Rest, {
        Title,
        <<A/binary, ",\"", B/binary, "\":"/utf8, B/binary>>,
        M ++ [{B, N}],
        X ++ [{key, B}] ++ Y
    }).


listify_test_() ->
    {"listify test", ?_assertEqual(
                {
                    "listify test",
                    <<"[true,1,\"hello world\",{}]">>,
                    [true, 1, <<"hello world">>, [{}]],
                    [
                        start_array,
                        {literal, true},
                        {integer, 1},
                        {string, <<"hello world">>},
                        start_object,
                        end_object,
                        end_array
                    ]
                },
                listify("listify test", [
                    {"true", <<"true">>, true, [{literal, true}]},
                    {"1", <<"1">>, 1, [{integer, 1}]},
                    {"hello world", <<"\"hello world\"">>, <<"hello world">>, [{string, <<"hello world">>}]},
                    {"{}", <<"{}">>, [{}], [start_object, end_object]}
                ])
    )}.


objectify_test_() ->
    {"objectify test", ?_assertEqual(
                {
                    "objectify test",
                    <<"{\"true\":true,\"1\":1,\"\"hello world\"\":\"hello world\",\"[]\":[]}">>,
                    [{<<"true">>, true}, {<<"1">>, 1}, {<<"\"hello world\"">>, <<"hello world">>}, {<<"[]">>, []}],
                    [
                        start_object,
                        {key, <<"true">>},
                        {literal, true},
                        {key, <<"1">>},
                        {integer, 1},
                        {key, <<"\"hello world\"">>},
                        {string, <<"hello world">>},
                        {key, <<"[]">>},
                        start_array,
                        end_array,
                        end_object
                    ]
                },
                objectify("objectify test", [
                    {"true", <<"true">>, true, [{literal, true}]},
                    {"1", <<"1">>, 1, [{integer, 1}]},
                    {"hello world", <<"\"hello world\"">>, <<"hello world">>, [{string, <<"hello world">>}]},
                    {"[]", <<"[]">>, [], [start_array, end_array]}
                ])
    )}.