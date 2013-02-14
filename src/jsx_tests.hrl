%% data and helper functions for tests

-export([init/1, handle_event/2]).
-export([test_cases/0]).


-include_lib("eunit/include/eunit.hrl").


%% test handler
init([]) -> [].

handle_event(end_json, State) -> lists:reverse([end_json] ++ State);
handle_event(Event, State) -> [Event] ++ State.


test_cases() ->
    empty_array()
    ++ nested_array()
    ++ empty_object()
    ++ nested_object()
    ++ strings()
    ++ literals()
    ++ integers()
    ++ floats()
    ++ compound_object().


empty_array() -> [{"[]", <<"[]">>, [], [start_array, end_array]}].

nested_array() ->
    [{
        "[[[]]]",
        <<"[[[]]]">>,
        [[[]]],
        [start_array, start_array, start_array, end_array, end_array, end_array]
    }].


empty_object() -> [{"{}", <<"{}">>, [{}], [start_object, end_object]}].

nested_object() ->
    [{
        "{\"key\":{\"key\":{}}}",
        <<"{\"key\":{\"key\":{}}}">>,
        [{<<"key">>, [{<<"key">>, [{}]}]}],
        [
            start_object,
                {key, <<"key">>},
                start_object,
                    {key, <<"key">>},
                    start_object,
                    end_object,
                end_object,
            end_object
        ]
    }].


naked_strings() ->
    Raw = [
        "",
        "hello world"
    ],
    [
        {
            String,
            <<"\"", (list_to_binary(String))/binary, "\"">>,
            list_to_binary(String),
            [{string, list_to_binary(String)}]
        }
        || String <- Raw
    ].

strings() ->
    naked_strings()
    ++ [ wrap_with_array(Test) || Test <- naked_strings() ]
    ++ [ wrap_with_object(Test) || Test <- naked_strings() ].


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
    naked_integers()
    ++ [ wrap_with_array(Test) || Test <- naked_integers() ]
    ++ [ wrap_with_object(Test) || Test <- naked_integers() ].


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
    naked_floats()
    ++ [ wrap_with_array(Test) || Test <- naked_floats() ]
    ++ [ wrap_with_object(Test) || Test <- naked_floats() ].


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
    naked_literals()
    ++ [ wrap_with_array(Test) || Test <- naked_literals() ]
    ++ [ wrap_with_object(Test) || Test <- naked_literals() ].


compound_object() ->
    [{
        "[{\"alpha\":[1,2,3],\"beta\":{\"alpha\":[1.0,2.0,3.0],\"beta\":[true,false]}},[{}]]",
        <<"[{\"alpha\":[1,2,3],\"beta\":{\"alpha\":[1.0,2.0,3.0],\"beta\":[true,false]}},[{}]]">>,
        [[{<<"alpha">>, [1, 2, 3]}, {<<"beta">>, [{<<"alpha">>, [1.0, 2.0, 3.0]}, {<<"beta">>, [true, false]}]}], [[{}]]],
        [
            start_array,
                start_object,
                    {key, <<"alpha">>},
                    start_array,
                        {integer, 1},
                        {integer, 2},
                        {integer, 3},
                    end_array,
                    {key, <<"beta">>},
                    start_object,
                        {key, <<"alpha">>},
                        start_array,
                            {float, 1.0},
                            {float, 2.0},
                            {float, 3.0},
                        end_array,
                        {key, <<"beta">>},
                        start_array,
                            {literal, true},
                            {literal, false},
                        end_array,
                    end_object,
                end_object,
                start_array,
                    start_object,
                    end_object,
                end_array,
            end_array
        ]
    }].


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


sane_float_to_list(X) ->
    [Output] = io_lib:format("~p", [X]),
    Output.