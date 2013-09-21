%% The MIT License

%% Copyright (c) 2010-2013 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.


-module(jsx_to_term).

-export([to_term/2]).
-export([init/1, handle_event/2]).


-record(config, {
    labels = binary,
    post_decode = false
}).

-type config() :: list().
-export_type([config/0]).


-type json_value() :: list({binary(), json_value()})
    | list(json_value())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().


-spec to_term(Source::binary(), Config::config()) -> json_value().

to_term(Source, Config) when is_list(Config) ->
    (jsx:decoder(?MODULE, Config, jsx_config:extract_config(Config)))(Source).


parse_config(Config) -> parse_config(Config, #config{}).

parse_config([{labels, Val}|Rest], Config)
        when Val == binary; Val == atom; Val == existing_atom; Val == attempt_atom ->
    parse_config(Rest, Config#config{labels = Val});
parse_config([labels|Rest], Config) ->
    parse_config(Rest, Config#config{labels = binary});
parse_config([{post_decode, F}|Rest], Config=#config{post_decode=false}) when is_function(F, 1) ->
    parse_config(Rest, Config#config{post_decode=F});
parse_config([{K, _}|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config)
        ; false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([K|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config)
        ; false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([], Config) ->
    Config.

-type state() :: {[any()], #config{}}.
-spec init(Config::proplists:proplist()) -> state().

init(Config) -> {[[]], parse_config(Config)}.

-spec handle_event(Event::any(), State::state()) -> state().

handle_event(end_json, {[[Terms]], _Config}) -> Terms;

handle_event(start_object, {Terms, Config}) -> {[[]|Terms], Config};
handle_event(end_object, {[[], {key, Key}, Last|Terms], Config}) ->
    {[[{Key, post_decode([{}], Config)}] ++ Last] ++ Terms, Config};
handle_event(end_object, {[Object, {key, Key}, Last|Terms], Config}) ->
    {[[{Key, post_decode(lists:reverse(Object), Config)}] ++ Last] ++ Terms, Config};
handle_event(end_object, {[[], Last|Terms], Config}) ->
    {[[post_decode([{}], Config)] ++ Last] ++ Terms, Config};
handle_event(end_object, {[Object, Last|Terms], Config}) ->
    {[[post_decode(lists:reverse(Object), Config)] ++ Last] ++ Terms, Config};

handle_event(start_array, {Terms, Config}) -> {[[]|Terms], Config};
handle_event(end_array, {[List, {key, Key}, Last|Terms], Config}) ->
    {[[{Key, post_decode(lists:reverse(List), Config)}] ++ Last] ++ Terms, Config};
handle_event(end_array, {[List, Last|Terms], Config}) ->
    {[[post_decode(lists:reverse(List), Config)] ++ Last] ++ Terms, Config};

handle_event({key, Key}, {Terms, Config}) -> {[{key, format_key(Key, Config)}] ++ Terms, Config};

handle_event({_, Event}, {[{key, Key}, Last|Terms], Config}) ->
    {[[{Key, post_decode(Event, Config)}] ++ Last] ++ Terms, Config};
handle_event({_, Event}, {[Last|Terms], Config}) ->
    {[[post_decode(Event, Config)] ++ Last] ++ Terms, Config}.


format_key(Key, Config) ->
    case Config#config.labels of
        binary -> Key
        ; atom -> binary_to_atom(Key, utf8)
        ; existing_atom -> binary_to_existing_atom(Key, utf8)
        ; attempt_atom ->
            try binary_to_existing_atom(Key, utf8) of
                Result -> Result
            catch
                error:badarg -> Key
            end
    end.


post_decode(Value, #config{post_decode=false}) -> Value;
post_decode(Value, Config) -> (Config#config.post_decode)(Value).


%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    %% for post_decode tests
    F = fun(X) -> X end,
    G = fun(X, Y) -> {X, Y} end,
    [
        {"empty config", ?_assertEqual(#config{}, parse_config([]))},
        {"implicit binary labels", ?_assertEqual(#config{}, parse_config([labels]))},
        {"binary labels", ?_assertEqual(#config{}, parse_config([{labels, binary}]))},
        {"atom labels", ?_assertEqual(#config{labels=atom}, parse_config([{labels, atom}]))},
        {"existing atom labels", ?_assertEqual(
            #config{labels=existing_atom},
            parse_config([{labels, existing_atom}])
        )},
        {"sloppy existing atom labels", ?_assertEqual(
            #config{labels=attempt_atom},
            parse_config([{labels, attempt_atom}])
        )},
        {"post decode", ?_assertEqual(
            #config{post_decode=F},
            parse_config([{post_decode, F}])
        )},
        {"post decode wrong arity", ?_assertError(badarg, parse_config([{post_decode, G}]))},
        {"invalid opt flag", ?_assertError(badarg, parse_config([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_config([{error, true}]))}
    ].


format_key_test_() ->
    [
        {"binary key", ?_assertEqual(<<"key">>, format_key(<<"key">>, #config{labels=binary}))},
        {"atom key", ?_assertEqual(key, format_key(<<"key">>, #config{labels=atom}))},
        {"existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #config{labels=existing_atom})
        )},
        {"nonexisting atom key", ?_assertError(
            badarg,
            format_key(<<"nonexistentatom">>, #config{labels=existing_atom})
        )},
        {"sloppy existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #config{labels=attempt_atom})
        )},
        {"nonexisting atom key", ?_assertEqual(
            <<"nonexistentatom">>,
            format_key(<<"nonexistentatom">>, #config{labels=attempt_atom})
        )}
    ].


post_decoders_test_() ->
    Events = [
        [{}],
        [{<<"key">>, <<"value">>}],
        [{<<"true">>, true}, {<<"false">>, false}, {<<"null">>, null}],
        [],
        [<<"string">>],
        [true, false, null],
        true,
        false,
        null,
        <<"hello">>,
        <<"world">>,
        1,
        1.0
    ],
    [
        {"no post_decode", ?_assertEqual(
            Events,
            [ post_decode(Event, #config{}) || Event <- Events ]
        )},
        {"replace arrays with empty arrays", ?_assertEqual(
            [
                [{}],
                [{<<"key">>, <<"value">>}],
                [{<<"true">>, true}, {<<"false">>, false}, {<<"null">>, null}],
                [],
                [],
                [],
                true,
                false,
                null,
                <<"hello">>,
                <<"world">>,
                1,
                1.0
            ],
            [ post_decode(Event, #config{
                    post_decode=fun([T|_] = V) when is_tuple(T) -> V; (V) when is_list(V) -> []; (V) -> V end
                }) || Event <- Events
            ]
        )},
        {"replace objects with empty objects", ?_assertEqual(
            [
                [{}],
                [{}],
                [{}],
                [],
                [<<"string">>],
                [true, false, null],
                true,
                false,
                null,
                <<"hello">>,
                <<"world">>,
                1,
                1.0
            ],
            [ post_decode(Event, #config{
                    post_decode=fun([T|_]) when is_tuple(T) -> [{}]; (V) -> V end
                }) || Event <- Events
            ]
        )},
        {"replace all non-array/non-object values with false", ?_assertEqual(
            [
                [{}],
                [{<<"key">>, <<"value">>}],
                [{<<"true">>, true}, {<<"false">>, false}, {<<"null">>, null}],
                [],
                [<<"string">>],
                [true, false, null],
                false,
                false,
                false,
                false,
                false,
                false,
                false
            ],
            [ post_decode(Event, #config{
                    post_decode=fun(V) when is_list(V) -> V; (_) -> false end
                }) || Event <- Events
            ]
        )},
        {"atoms_to_strings", ?_assertEqual(
            [
                [{}],
                [{<<"key">>, <<"value">>}],
                [{<<"true">>, true}, {<<"false">>, false}, {<<"null">>, null}],
                [],
                [<<"string">>],
                [true, false, null],
                <<"true">>,
                <<"false">>,
                <<"null">>,
                <<"hello">>,
                <<"world">>,
                1,
                1.0
            ],
            [ post_decode(Event, #config{
                    post_decode=fun(V) when is_atom(V) -> unicode:characters_to_binary(atom_to_list(V)); (V) -> V end
                }) || Event <- Events
            ]
        )}
    ].


handle_event_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                Term,
                lists:foldl(fun handle_event/2, {[[]], #config{}}, Events ++ [end_json])
            )
        } || {Title, _, Term, Events} <- Data
    ].


-endif.
