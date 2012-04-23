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
parse_opts([{post_decode, _}|_] = Options, Opts) ->
    erlang:error(badarg, [Options, Opts]);
parse_opts([_|Rest], Opts) ->
    parse_opts(Rest, Opts);
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
handle_event(end_array, {[Current, Last|Terms], Opts}) ->
    {[[post_decode(lists:reverse(Current), Opts)] ++ Last] ++ Terms, Opts};

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

basic_test_() ->
    [
        {"empty object", ?_assertEqual(to_term(<<"{}">>, []), [{}])},
        {"simple object", ?_assertEqual(to_term(<<"{\"key\": true}">>, []), [{<<"key">>, true}])},
        {"less simple object", ?_assertEqual(
            to_term(<<"{\"a\": 1, \"b\": 2}">>, []),
            [{<<"a">>, 1}, {<<"b">>, 2}]
        )},
        {"nested object", ?_assertEqual(
            to_term(<<"{\"key\": {\"key\": true}}">>, []),
            [{<<"key">>, [{<<"key">>, true}]}]
        )},
        {"empty array", ?_assert(to_term(<<"[]">>, []) =:= [])},
        {"list of lists", ?_assertEqual(to_term(<<"[[],[],[]]">>, []), [[], [], []])},
        {"list of strings", ?_assertEqual(to_term(<<"[\"hi\", \"there\"]">>, []), [<<"hi">>, <<"there">>])},
        {"list of numbers", ?_assertEqual(to_term(<<"[1, 2.0, 3e4, -5]">>, []), [1, 2.0, 3.0e4, -5])},
        {"list of literals", ?_assertEqual(to_term(<<"[true,false,null]">>, []), [true,false,null])},
        {"list of objects", ?_assertEqual(
            to_term(<<"[{}, {\"a\":1, \"b\":2}, {\"key\":[true,false]}]">>, []),
            [[{}], [{<<"a">>,1},{<<"b">>,2}], [{<<"key">>,[true,false]}]]
        )}
    ].

comprehensive_test_() ->
    {"comprehensive test", ?_assertEqual(to_term(comp_json(), []), comp_term())}.

comp_json() ->
    <<"[
        {\"a key\": {\"a key\": -17.346, \"another key\": 3e152, \"last key\": 14}},
        [0,1,2,3,4,5],
        [{\"a\": \"a\", \"b\": \"b\"}, {\"c\": \"c\", \"d\": \"d\"}],
        [true, false, null],
        {},
        [],
        [{},{}],
        {\"key\": [], \"another key\": {}}    
    ]">>.

comp_term() ->
    [
        [{<<"a key">>, [{<<"a key">>, -17.346}, {<<"another key">>, 3.0e152}, {<<"last key">>, 14}]}],
        [0,1,2,3,4,5],
        [[{<<"a">>, <<"a">>}, {<<"b">>, <<"b">>}], [{<<"c">>, <<"c">>}, {<<"d">>, <<"d">>}]],
        [true, false, null],
        [{}],
        [],
        [[{}], [{}]],
        [{<<"key">>, []}, {<<"another key">>, [{}]}]
    ].

atom_labels_test_() ->
    {"atom labels test", ?_assertEqual(to_term(comp_json(), [{labels, atom}]), atom_term())}.

atom_term() ->
    [
        [{'a key', [{'a key', -17.346}, {'another key', 3.0e152}, {'last key', 14}]}],
        [0,1,2,3,4,5],
        [[{a, <<"a">>}, {b, <<"b">>}], [{'c', <<"c">>}, {'d', <<"d">>}]],
        [true, false, null],
        [{}],
        [],
        [[{}], [{}]],
        [{key, []}, {'another key', [{}]}]
    ].

naked_test_() ->
    [
        {"naked integer", ?_assertEqual(to_term(<<"123">>, []), 123)},
        {"naked float", ?_assertEqual(to_term(<<"-4.32e-17">>, []), -4.32e-17)},
        {"naked literal", ?_assertEqual(to_term(<<"true">>, []), true)},
        {"naked string", ?_assertEqual(to_term(<<"\"string\"">>, []), <<"string">>)}
    ].

post_decoders_test_() ->
    JSON = <<"{\"object\": {
        \"literals\": [true, false, null],
        \"strings\": [\"foo\", \"bar\", \"baz\"],
        \"numbers\": [1, 1.0, 1e0]
    }}">>,
    [
        {"no post_decode", ?_assertEqual(
            to_term(JSON, []),
            [{<<"object">>, [
                {<<"literals">>, [true, false, null]},
                {<<"strings">>, [<<"foo">>, <<"bar">>, <<"baz">>]},
                {<<"numbers">>, [1, 1.0, 1.0]}
            ]}]
        )},
        {"replace arrays with empty arrays", ?_assertEqual(
            to_term(JSON, [{post_decode, fun([T|_] = V)  when is_tuple(T) -> V; (V) when is_list(V) -> []; (V) -> V end}]),
            [{<<"object">>, [{<<"literals">>, []}, {<<"strings">>, []}, {<<"numbers">>, []}]}]
        )},
        {"replace objects with empty objects", ?_assertEqual(
            to_term(JSON, [{post_decode, fun(V) when is_list(V) -> [{}]; (V) -> V end}]),
            [{}]
        )},
        {"replace all non-list values with false", ?_assertEqual(
            to_term(JSON, [{post_decode, fun(V) when is_list(V) -> V; (_) -> false end}]),
            [{<<"object">>, [
                {<<"literals">>, [false, false, false]},
                {<<"strings">>, [false, false, false]},
                {<<"numbers">>, [false, false, false]}
            ]}]            
        )},
        {"atoms_to_strings", ?_assertEqual(
            to_term(JSON, [{post_decode, fun(V) when is_atom(V) -> unicode:characters_to_binary(atom_to_list(V)); (V) -> V end}]),
            [{<<"object">>, [
                {<<"literals">>, [<<"true">>, <<"false">>, <<"null">>]},
                {<<"strings">>, [<<"foo">>, <<"bar">>, <<"baz">>]},
                {<<"numbers">>, [1, 1.0, 1.0]}
            ]}]
        )}
    ].
    
-endif.
