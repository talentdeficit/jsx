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
    labels = binary
}).

-type opts() :: list().


-spec to_term(Source::(binary() | list()), Opts::opts()) ->
    list({binary(), any()}).
    
to_term(Source, Opts) when is_list(Opts) ->
    (jsx:decoder(?MODULE, Opts, jsx_utils:extract_opts(Opts)))(Source).



parse_opts(Opts) -> parse_opts(Opts, #opts{}).

parse_opts([{labels, Val}|Rest], Opts)
        when Val == binary; Val == atom; Val == existing_atom ->
    parse_opts(Rest, Opts#opts{labels = Val});
parse_opts([labels|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{labels = binary});
parse_opts([_|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts([], Opts) ->
    Opts.



init(Opts) -> {[[]], parse_opts(Opts)}.



handle_event(end_json, {[[Terms]], _Opts}) -> Terms;

handle_event(start_object, {Terms, Opts}) -> {[[]|Terms], Opts};
handle_event(end_object, {[[], {key, Key}, Last|Terms], Opts}) ->
    {[[{Key, [{}]}] ++ Last] ++ Terms, Opts};
handle_event(end_object, {[Object, {key, Key}, Last|Terms], Opts}) ->
    {[[{Key, lists:reverse(Object)}] ++ Last] ++ Terms, Opts};
handle_event(end_object, {[[], Last|Terms], Opts}) ->
    {[[[{}]] ++ Last] ++ Terms, Opts};
handle_event(end_object, {[Object, Last|Terms], Opts}) ->
    {[[lists:reverse(Object)] ++ Last] ++ Terms, Opts};
    
handle_event(start_array, {Terms, Opts}) -> {[[]|Terms], Opts};
handle_event(end_array, {[List, {key, Key}, Last|Terms], Opts}) ->
    {[[{Key, lists:reverse(List)}] ++ Last] ++ Terms, Opts};
handle_event(end_array, {[Current, Last|Terms], Opts}) ->
    {[[lists:reverse(Current)] ++ Last] ++ Terms, Opts};

handle_event({key, Key}, {Terms, Opts}) -> {[{key, format_key(Key, Opts)}] ++ Terms, Opts};

handle_event({_, Event}, {[{key, Key}, Last|Terms], Opts}) ->
    {[[{Key, Event}] ++ Last] ++ Terms, Opts};
handle_event({_, Event}, {[Last|Terms], Opts}) ->
    {[[Event] ++ Last] ++ Terms, Opts}.



format_key(Key, Opts) ->
    case Opts#opts.labels of
        binary -> Key
        ; atom -> binary_to_atom(Key, utf8)
        ; existing_atom -> binary_to_existing_atom(Key, utf8)
    end.



%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    [
        {"empty object", ?_assert(to_term(<<"{}">>, []) =:= [{}])},
        {"simple object", ?_assert(to_term(<<"{\"key\": true}">>, []) =:= [{<<"key">>, true}])},
        {"less simple object",
            ?_assert(to_term(<<"{\"a\": 1, \"b\": 2}">>, []) =:= [{<<"a">>, 1}, {<<"b">>, 2}])
        },
        {"nested object",
            ?_assert(to_term(<<"{\"key\": {\"key\": true}}">>, []) =:= [{<<"key">>, [{<<"key">>, true}]}])
        },
        {"empty array", ?_assert(to_term(<<"[]">>, []) =:= [])},
        {"list of lists",
            ?_assert(to_term(<<"[[],[],[]]">>, []) =:= [[], [], []])
        },
        {"list of strings",
            ?_assert(to_term(<<"[\"hi\", \"there\"]">>, []) =:= [<<"hi">>, <<"there">>])
        },
        {"list of numbers",
            ?_assert(to_term(<<"[1, 2.0, 3e4, -5]">>, []) =:= [1, 2.0, 3.0e4, -5])
        },
        {"list of literals",
            ?_assert(to_term(<<"[true,false,null]">>, []) =:= [true,false,null])
        },
        {"list of objects",
            ?_assert(to_term(<<"[{}, {\"a\":1, \"b\":2}, {\"key\":[true,false]}]">>, [])
                =:= [[{}], [{<<"a">>,1},{<<"b">>,2}], [{<<"key">>,[true,false]}]])
        }
    ].

comprehensive_test_() ->
    {"comprehensive test", ?_assert(to_term(comp_json(), []) =:= comp_term())}.

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
    {"atom labels test", ?_assert(to_term(comp_json(), [{labels, atom}]) =:= atom_term())}.

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
        {"naked integer", ?_assert(to_term(<<"123">>, []) =:= 123)},
        {"naked float", ?_assert(to_term(<<"-4.32e-17">>, []) =:= -4.32e-17)},
        {"naked literal", ?_assert(to_term(<<"true">>, []) =:= true)},
        {"naked string", ?_assert(to_term(<<"\"string\"">>, []) =:= <<"string">>)}
    ].
    
-endif.
