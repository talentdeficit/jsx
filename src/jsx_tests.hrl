%% data and helper functions for tests

-export([init/1, handle_event/2]).
-export([empty_array/0, empty_object/0]).

-include_lib("eunit/include/eunit.hrl").


%% test handler
init([]) -> [].

handle_event(end_json, State) -> lists:reverse([end_json] ++ State);
handle_event(Event, State) -> [Event] ++ State.


empty_array() -> [{"empty array", <<"[]">>, [], [start_array, end_array, end_json]}].
empty_object() -> [{"empty object", <<"{}">>, [{}], [start_object, end_object, end_json]}].