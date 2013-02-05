%% The MIT License

%% Copyright (c) 2011 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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


-record(opts, {
    labels = binary,
    post_decode = false
}).

-type opts() :: list().

-type json_value() :: list({binary(), json_value()})
    | list(json_value())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().


-spec to_term(Source::binary(), Opts::opts()) -> json_value().

to_term(Source, Opts) when is_list(Opts) ->
    (jsx:decoder(?MODULE, Opts, jsx_utils:extract_opts(Opts)))(Source).


parse_opts(Opts) -> parse_opts(Opts, #opts{}).

parse_opts([{labels, Val}|Rest], Opts)
        when Val == binary; Val == atom; Val == existing_atom ->
    parse_opts(Rest, Opts#opts{labels = Val});
parse_opts([labels|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{labels = binary});
parse_opts([{post_decode, F}|Rest], Opts=#opts{post_decode=false}) when is_function(F, 1) ->
    parse_opts(Rest, Opts#opts{post_decode=F});
parse_opts([{K, _}|Rest] = Options, Opts) ->
    case lists:member(K, jsx_utils:valid_flags()) of
        true -> parse_opts(Rest, Opts)
        ; false -> erlang:error(badarg, [Options, Opts])
    end;
parse_opts([K|Rest] = Options, Opts) ->
    case lists:member(K, jsx_utils:valid_flags()) of
        true -> parse_opts(Rest, Opts)
        ; false -> erlang:error(badarg, [Options, Opts])
    end;
parse_opts([], Opts) ->
    Opts.


init(Opts) -> {[[]], parse_opts(Opts)}.


handle_event(end_json, {[[Terms]], _Opts}) -> Terms;

handle_event(start_object, {Terms, Opts}) -> {[[]|Terms], Opts};
handle_event(end_object, {[[], {key, Key}, Last|Terms], Opts}) ->
    {[[{Key, post_decode([{}], Opts)}] ++ Last] ++ Terms, Opts};
handle_event(end_object, {[Object, {key, Key}, Last|Terms], Opts}) ->
    {[[{Key, post_decode(lists:reverse(Object), Opts)}] ++ Last] ++ Terms, Opts};
handle_event(end_object, {[[], Last|Terms], Opts}) ->
    {[[post_decode([{}], Opts)] ++ Last] ++ Terms, Opts};
handle_event(end_object, {[Object, Last|Terms], Opts}) ->
    {[[post_decode(lists:reverse(Object), Opts)] ++ Last] ++ Terms, Opts};

handle_event(start_array, {Terms, Opts}) -> {[[]|Terms], Opts};
handle_event(end_array, {[List, {key, Key}, Last|Terms], Opts}) ->
    {[[{Key, post_decode(lists:reverse(List), Opts)}] ++ Last] ++ Terms, Opts};
handle_event(end_array, {[List, Last|Terms], Opts}) ->
    {[[post_decode(lists:reverse(List), Opts)] ++ Last] ++ Terms, Opts};

handle_event({key, Key}, {Terms, Opts}) -> {[{key, format_key(Key, Opts)}] ++ Terms, Opts};

handle_event({_, Event}, {[{key, Key}, Last|Terms], Opts}) ->
    {[[{Key, post_decode(Event, Opts)}] ++ Last] ++ Terms, Opts};
handle_event({_, Event}, {[Last|Terms], Opts}) ->
    {[[post_decode(Event, Opts)] ++ Last] ++ Terms, Opts}.


format_key(Key, Opts) ->
    case Opts#opts.labels of
        binary -> Key
        ; atom -> binary_to_atom(Key, utf8)
        ; existing_atom -> binary_to_existing_atom(Key, utf8)
    end.


post_decode(Value, #opts{post_decode=false}) -> Value;
post_decode(Value, Opts) -> (Opts#opts.post_decode)(Value).


%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


opts_test_() ->
    %% for post_decode tests
    F = fun(X) -> X end,
    G = fun(X, Y) -> {X, Y} end,
    [
        {"empty opts", ?_assertEqual(#opts{}, parse_opts([]))},
        {"implicit binary labels", ?_assertEqual(#opts{}, parse_opts([labels]))},
        {"binary labels", ?_assertEqual(#opts{}, parse_opts([{labels, binary}]))},
        {"atom labels", ?_assertEqual(#opts{labels=atom}, parse_opts([{labels, atom}]))},
        {"existing atom labels", ?_assertEqual(
            #opts{labels=existing_atom},
            parse_opts([{labels, existing_atom}])
        )},
        {"post decode", ?_assertEqual(
            #opts{post_decode=F},
            parse_opts([{post_decode, F}])
        )},
        {"post decode wrong arity", ?_assertError(badarg, parse_opts([{post_decode, G}]))},
        {"invalid opt flag", ?_assertError(badarg, parse_opts([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_opts([{error, true}]))}
    ].


format_key_test_() ->
    [
        {"binary key", ?_assertEqual(<<"key">>, format_key(<<"key">>, #opts{labels=binary}))},
        {"atom key", ?_assertEqual(key, format_key(<<"key">>, #opts{labels=atom}))},
        {"existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #opts{labels=existing_atom})
        )},
        {"nonexisting atom key", ?_assertError(
            badarg,
            format_key(<<"nonexistentatom">>, #opts{labels=existing_atom})
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
            [ post_decode(Event, #opts{}) || Event <- Events ]
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
            [ post_decode(Event, #opts{
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
            [ post_decode(Event, #opts{
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
            [ post_decode(Event, #opts{
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
            [ post_decode(Event, #opts{
                    post_decode=fun(V) when is_atom(V) -> unicode:characters_to_binary(atom_to_list(V)); (V) -> V end
                }) || Event <- Events
            ]
        )}
    ].

-endif.
