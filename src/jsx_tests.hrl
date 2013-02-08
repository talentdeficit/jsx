%% data and helper functions for tests

-export([init/1, handle_event/2]).
-export([empty_array/0, empty_object/0]).
-export([literals/0, naked_literals/0]).
-export([integers/0, naked_integers/0]).


-include_lib("eunit/include/eunit.hrl").


%% test handler
init([]) -> [].

handle_event(end_json, State) -> lists:reverse([end_json] ++ State);
handle_event(Event, State) -> [Event] ++ State.


empty_array() -> [{"[]", <<"[]">>, [], [start_array, end_array]}].
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
    ] ++ [{"-0", <<"-0">>, 0, [{integer, 0}]}].

integers() ->
    [ wrap_with_array(Test) || Test <- naked_integers() ]
        ++ [ wrap_with_object(Test) || Test <- naked_integers() ].


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
        ++ [ wrap_with_object(Test) || Test <- naked_literals() ].


wrap_with_array({Title, JSON, Term, Events}) ->
    {
        "[" ++ Title ++ "]",
        <<"[", JSON/binary, "]">>,
        [Term],
        [start_array, Events, end_array]
    }.


wrap_with_object({Title, JSON, Term, Events}) ->
    {
        "{\"key\":" ++ Title ++ "}",
        <<"{\"key\":", JSON/binary, "}">>,
        [{<<"key">>, Term}],
        [start_object, {key, <<"key">>}, Events, end_object]
    }.