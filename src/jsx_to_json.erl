%% The MIT License

%% Copyright (c) 2010 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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


-module(jsx_to_json).

-export([to_json/2, format/2]).
-export([init/1, handle_event/2]).


-record(opts, {
    space = 0,
    indent = 0,
    depth = 0,
    pre_encode = false
}).

-type opts() :: list().


-spec to_json(Source::any(), Opts::opts()) -> binary().
    
to_json(Source, Opts) when is_list(Opts) ->
    (jsx:encoder(?MODULE, Opts, jsx_utils:extract_opts(Opts ++ [escaped_strings])))(Source).


-spec format(Source::binary(), Opts::opts()) -> binary().
    
format(Source, Opts) when is_binary(Source) andalso is_list(Opts) ->
    (jsx:decoder(?MODULE, Opts, jsx_utils:extract_opts(Opts ++ [escaped_strings])))(Source).


parse_opts(Opts) -> parse_opts(Opts, #opts{}).

parse_opts([{space, Val}|Rest], Opts) when is_integer(Val), Val > 0 ->
    parse_opts(Rest, Opts#opts{space = Val});
parse_opts([space|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{space = 1});
parse_opts([{indent, Val}|Rest], Opts) when is_integer(Val), Val > 0 ->
    parse_opts(Rest, Opts#opts{indent = Val});
parse_opts([indent|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{indent = 1});
parse_opts([{pre_encode, F}|Rest], Opts=#opts{pre_encode=false}) when is_function(F, 1) ->
    parse_opts(Rest, Opts#opts{pre_encode=F});
parse_opts([{pre_encode, _}|_] = Options, Opts) ->
    erlang:error(badarg, [Options, Opts]);
parse_opts([_|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts([], Opts) ->
    Opts.



-define(start_object, <<"{">>).
-define(start_array, <<"[">>).
-define(end_object, <<"}">>).
-define(end_array, <<"]">>).
-define(colon, <<":">>).
-define(comma, <<",">>).
-define(quote, <<"\"">>).
-define(space, <<" ">>).
-define(newline, <<"\n">>).



init(Opts) -> {start, [], parse_opts(Opts)}.



handle_event(Event, {start, Acc, Opts}) ->
    case Event of
        {Type, Value} -> {[], [Acc, encode(Type, Value, Opts)], Opts}
        ; start_object -> {[object_start], [Acc, ?start_object], Opts}
        ; start_array -> {[array_start], [Acc, ?start_array], Opts}
    end;
handle_event(Event, {[object_start|Stack], Acc, OldOpts = #opts{depth = Depth}}) ->
    Opts = OldOpts#opts{depth = Depth + 1},
    case Event of
        {key, Key} ->
            {[object_value|Stack], [Acc, indent(Opts), encode(string, Key, Opts), ?colon, space(Opts)], Opts}
        ; end_object ->
            {Stack, [Acc, ?end_object], OldOpts}
    end;
handle_event(Event, {[object_value|Stack], Acc, Opts}) ->
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[key|Stack], [Acc, encode(Type, Value, Opts)], Opts}
        ; start_object -> {[object_start, key|Stack], [Acc, ?start_object], Opts}
        ; start_array -> {[array_start, key|Stack], [Acc, ?start_array], Opts}
    end;
handle_event(Event, {[key|Stack], Acc, Opts = #opts{depth = Depth}}) ->
    case Event of
        {key, Key} ->
            {[object_value|Stack], [Acc, ?comma, indent_or_space(Opts), encode(string, Key, Opts), ?colon, space(Opts)], Opts}
        ; end_object ->
            NewOpts = Opts#opts{depth = Depth - 1},
            {Stack, [Acc, indent(NewOpts), ?end_object], NewOpts}
    end;
handle_event(Event, {[array_start|Stack], Acc, OldOpts = #opts{depth = Depth}}) ->
    Opts = OldOpts#opts{depth = Depth + 1},
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[array|Stack], [Acc, indent(Opts), encode(Type, Value, Opts)], Opts}
        ; start_object -> {[object_start, array|Stack], [Acc, indent(Opts), ?start_object], Opts}
        ; start_array -> {[array_start, array|Stack], [Acc, indent(Opts), ?start_array], Opts}
        ; end_array -> {Stack, [Acc, ?end_array], OldOpts}
    end;
handle_event(Event, {[array|Stack], Acc, Opts = #opts{depth = Depth}}) ->
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[array|Stack], [Acc, ?comma, indent_or_space(Opts), encode(Type, Value, Opts)], Opts}
        ; end_array ->
            NewOpts = Opts#opts{depth = Depth - 1},
            {Stack, [Acc, indent(NewOpts), ?end_array], NewOpts}
        ; start_object -> {[object_start, array|Stack], [Acc, ?comma, indent_or_space(Opts), ?start_object], Opts}
        ; start_array -> {[array_start, array|Stack], [Acc, ?comma, indent_or_space(Opts), ?start_array], Opts}
    end;
handle_event(end_json, {[], Acc, _Opts}) -> unicode:characters_to_binary(Acc, utf8).


encode(string, String, _Opts) ->
    [?quote, String, ?quote];
encode(literal, Literal, _Opts) ->
    erlang:atom_to_list(Literal);
encode(integer, Integer, _Opts) ->
    erlang:integer_to_list(Integer);
encode(float, Float, _Opts) ->
    [Output] = io_lib:format("~p", [Float]), Output.


space(Opts) ->
    case Opts#opts.space of
        0 -> []
        ; X when X > 0 -> [ ?space || _ <- lists:seq(1, X) ]
    end.


indent(Opts) ->
    case Opts#opts.indent of
        0 -> []
        ; X when X > 0 ->
            Indent = [ ?space || _ <- lists:seq(1, X) ],
            indent(Indent, Opts#opts.depth, [?newline])
    end.

indent(_Indent, 0, Acc) -> Acc;
indent(Indent, N, Acc) -> indent(Indent, N - 1, [Acc, Indent]).


indent_or_space(Opts) ->
    case Opts#opts.indent > 0 of
        true -> indent(Opts)
        ; false -> space(Opts)
    end.


%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_format_test_() ->
    [
        {"empty object", ?_assertEqual(<<"{}">>, format(<<"{}">>, []))},
        {"empty array", ?_assertEqual(<<"[]">>, format(<<"[]">>, []))},
        {"naked integer", ?_assertEqual(<<"123">>, format(<<"123">>, []))},
        {"naked float", ?_assertEqual(<<"1.23">>, format(<<"1.23">>, []))},
        {"naked string", ?_assertEqual(<<"\"hi\"">>, format(<<"\"hi\"">>, []))},
        {"naked string with control character", ?_assertEqual(
            <<"\"hi\\n\"">>, format(<<"\"hi\\n\"">>, [])
        )},
        {"naked literal", ?_assertEqual(<<"true">>, format(<<"true">>, []))},
        {"simple object", ?_assertEqual(
            <<"{\"key\":\"value\"}">>,
            format(<<"  { \"key\"  :\n\t \"value\"\r\r\r\n }  ">>, [])
        )},
        {"really simple object", ?_assertEqual(<<"{\"k\":\"v\"}">>, format(<<"{\"k\":\"v\"}">>, []) )},
        {"nested object", ?_assertEqual(
            <<"{\"k\":{\"k\":\"v\"},\"j\":{}}">>,
            format(<<"{\"k\":{\"k\":\"v\"}, \"j\":{}}">>, [])
        )},
        {"simple array", ?_assertEqual(
            <<"[true,false,null]">>,
            format(<<" [\n\ttrue,\n\tfalse  ,  \n \tnull\n] ">>, [])
        )},
        {"really simple array", ?_assertEqual(<<"[1]">>, format(<<"[1]">>, []))},
        {"nested array", ?_assertEqual(<<"[[[]]]">>, format(<<"[[[]]]">>, []))},
        {"nested structures", ?_assertEqual(
            <<"[{\"key\":\"value\",\"another key\":\"another value\",\"a list\":[true,false]},[[{}]]]">>,
            format(<<"[
                {
                    \"key\":\"value\",
                    \"another key\": \"another value\",
                    \"a list\": [true, false]
                },
                [[{}]]
            ]">>, [])
        )},
        {"simple nested structure",
            ?_assertEqual(
                <<"[[],{\"k\":[[],{}],\"j\":{}},[]]">>,
                format(<<"[[],{\"k\":[[],{}],\"j\":{}},[]]">>, [])
            )
        }
    ].

basic_to_json_test_() ->
    [
        {"empty object", ?_assertEqual(<<"{}">>, to_json([{}], []))},
        {"empty array", ?_assertEqual(<<"[]">>, to_json([], []))},
        {"naked integer", ?_assertEqual(<<"123">>, to_json(123, []))},
        {"naked float", ?_assertEqual(<<"1.23">>, to_json(1.23, []) )},
        {"naked string", ?_assertEqual(<<"\"hi\"">>, to_json(<<"hi">>, []))},
        {"naked string with control character", ?_assertEqual(
            <<"\"hi\\n\"">>, to_json(<<"hi\n">>, [])
        )},
        {"naked literal", ?_assertEqual(<<"true">>, to_json(true, []))},
        {"simple object", ?_assertEqual(
            <<"{\"key\":\"value\"}">>,
            to_json(
                [{<<"key">>, <<"value">>}],
                []
            )
        )},
        {"simple proplist", ?_assertEqual(
            <<"[{\"key1\":\"value\",\"key2\":3}]">>,
            to_json(
                [ [{key1, <<"value">>}, {key2, 3}] ],
                []
            )
        )},
        {"nested object", ?_assertEqual(
            <<"{\"k\":{\"k\":\"v\"},\"j\":{}}">>,
            to_json(
                [{<<"k">>,[{<<"k">>,<<"v">>}]},{<<"j">>,[{}]}],
                []
            )
        )},
        {"simple array", ?_assertEqual(<<"[true,false,null]">>, to_json([true, false, null], []))},
        {"really simple array", ?_assertEqual(<<"[1]">>, to_json([1], []))},
        {"nested array", ?_assertEqual(<<"[[[]]]">>, to_json([[[]]], []))},
        {"nested structures", ?_assertEqual(
            <<"[{\"key\":\"value\",\"another key\":\"another value\",\"a list\":[true,false]},[[{}]]]">>,
            to_json(
                [
                    [
                        {<<"key">>, <<"value">>},
                        {<<"another key">>, <<"another value">>},
                        {<<"a list">>, [true, false]}
                    ],
                    [[[{}]]]
                ],
                []
            )
        )},
        {"simple nested structure", ?_assertEqual(
            <<"[[],{\"k\":[[],{}],\"j\":{}},[]]">>,
            to_json(
                [[], [{<<"k">>, [[], [{}]]}, {<<"j">>, [{}]}], []],
                []
            )
        )}
    ].

%% This pre-encoder transforms undefined to nulls and most atoms to binary strings
pre_encode_to_json_test_() ->
    Pre = fun(undefined) -> null;
             (true) -> true;
             (false) -> false;
             (null) -> null;
             (X) when is_atom(X) -> atom_to_binary(X,utf8);
             (X) -> X
          end,
    Opts = [{pre_encode, Pre}],
    [
        {"empty object", ?_assertEqual(<<"{}">>, to_json([{}], Opts))},
        {"empty array", ?_assertEqual(<<"[]">>, to_json([], Opts))},
        {"naked integer", ?_assertEqual(<<"123">>, to_json(123, Opts))},
        {"naked float", ?_assertEqual(<<"1.23">>, to_json(1.23, Opts) )},
        {"naked string", ?_assertEqual(<<"\"hi\"">>, to_json(<<"hi">>, Opts))},
        {"naked string with control character", ?_assertEqual(
            <<"\"hi\\n\"">>, to_json(<<"hi\n">>, Opts)
        )},
        {"naked literal", ?_assertEqual(<<"true">>, to_json(true, Opts))},
        {"naked undefined", ?_assertEqual(<<"null">>, to_json(undefined, Opts))},
        {"simple object", ?_assertEqual(
            <<"{\"key\":\"value\"}">>,
            to_json( [{<<"key">>, <<"value">>}], Opts) 
        )},
        {"simple proplist", ?_assertEqual(
            <<"[{\"key1\":\"value\",\"key2\":3}]">>,
            to_json(
                [ [{key1, <<"value">>}, {key2, 3}] ],
                Opts
            )
        )},
        {"simple atom_proplist", ?_assertEqual(
            <<"[{\"key1\":\"value\",\"key2\":3}]">>,
            to_json(
                [ [{key1, value}, {key2, 3}] ],
                Opts
            )
        )},
        {"nested object", ?_assertEqual(
            <<"{\"k\":{\"k\":\"v\"},\"j\":{}}">>,
            to_json(
                [{<<"k">>,[{<<"k">>,<<"v">>}]},{<<"j">>,[{}]}],
                Opts
            )
        )},
        {"simple array", ?_assertEqual(<<"[true,false,null]">>, to_json([true, false, null], Opts))},
        {"really simple array", ?_assertEqual(<<"[1]">>, to_json([1], Opts))},
        {"nested array", ?_assertEqual(<<"[[[]]]">>, to_json([[[]]], Opts))},
        {"nested structures", ?_assertEqual(
            <<"[{\"key\":\"value\",\"another key\":\"another value\",\"a list\":[true,false]},[[{}]]]">>,
            to_json(
                [
                    [
                        {<<"key">>, <<"value">>},
                        {<<"another key">>, <<"another value">>},
                        {<<"a list">>, [true, false]}
                    ],
                    [[[{}]]]
                ],
                Opts
            )
        )},
        {"simple nested structure", ?_assertEqual(
            <<"[[],{\"k\":[[],{}],\"j\":{}},[]]">>,
            to_json(
                [[], [{<<"k">>, [[], [{}]]}, {<<"j">>, [{}]}], []],
                Opts
            )
        )}
    ].

opts_test_() ->
    [
        {"unspecified indent/space", ?_assertEqual(
            format(<<" [\n\ttrue,\n\tfalse,\n\tnull\n] ">>, [space, indent]),
            <<"[\n true,\n false,\n null\n]">>
        )},
        {"specific indent/space", ?_assertEqual(
            format(
                <<"\n{\n\"key\"  :  [],\n\"another key\"  :  true\n}\n">>, 
                [{space, 2}, {indent, 3}]
            ),
            <<"{\n   \"key\":  [],\n   \"another key\":  true\n}">>
        )},
        {"nested structures", ?_assertEqual(
            format(
                <<"[{\"key\":\"value\", \"another key\": \"another value\"}, [[true, false, null]]]">>, 
                [{space, 2}, {indent, 2}]
            ),
            <<"[\n  {\n    \"key\":  \"value\",\n    \"another key\":  \"another value\"\n  },\n  [\n    [\n      true,\n      false,\n      null\n    ]\n  ]\n]">>
        )},
        {"array spaces", ?_assertEqual(
            format(<<"[1,2,3]">>, [{space, 2}]),
            <<"[1,  2,  3]">>
        )},
        {"object spaces", ?_assertEqual(
            format(<<"{\"a\":true,\"b\":true,\"c\":true}">>, [{space, 2}]),
            <<"{\"a\":  true,  \"b\":  true,  \"c\":  true}">>
        )},
        {"array indent", ?_assertEqual(
            format(<<"[1.23, 1.23, 1.23]">>, [{indent, 2}]),
            <<"[\n  1.23,\n  1.23,\n  1.23\n]">>
        )},
        {"object indent", ?_assertEqual(
            format(<<"{\"a\":true,\"b\":true,\"c\":true}">>, [{indent, 2}]),
            <<"{\n  \"a\":true,\n  \"b\":true,\n  \"c\":true\n}">>
        )}
    ].

    
-endif.
