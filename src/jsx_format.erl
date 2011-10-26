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


-module(jsx_format).

-export([format/2]).


-record(opts, {
    output_encoding = utf8,
    space = 0,
    indent = 0,
    depth = 0
}).

-type opts() :: [].


-spec format(Source::(binary() | list()), Opts::opts()) -> binary().
    
format(Source, Opts) when (is_binary(Source) andalso is_list(Opts))
        orelse (is_list(Source) andalso is_list(Opts)) ->
    jsx:fold(fun fold/2,
        {start, [], parse_opts(Opts)},
        Source,
        extract_opts(Opts)
    ).


parse_opts(Opts) -> parse_opts(Opts, #opts{}).

parse_opts([{output_encoding, Val}|Rest], Opts) when Val == utf8 ->
    parse_opts(Rest, Opts#opts{output_encoding = Val});
parse_opts([{space, Val}|Rest], Opts) when is_integer(Val), Val > 0 ->
    parse_opts(Rest, Opts#opts{space = Val});
parse_opts([space|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{space = 1});
parse_opts([{indent, Val}|Rest], Opts) when is_integer(Val), Val > 0 ->
    parse_opts(Rest, Opts#opts{indent = Val});
parse_opts([indent|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{indent = 1});
parse_opts([_|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts([], Opts) ->
    Opts.


extract_opts(Opts) ->
    extract_parser_opts(Opts, []).

extract_parser_opts([], Acc) -> Acc;     
extract_parser_opts([{K,V}|Rest], Acc) ->
    case lists:member(K, [loose_unicode, escape_forward_slash, explicit_end]) of
        true -> [{K,V}] ++ Acc
        ; false -> extract_parser_opts(Rest, Acc)
    end;
extract_parser_opts([K|Rest], Acc) ->
    case lists:member(K, [loose_unicode, escape_forward_slash, explicit_end]) of
        true -> [K] ++ Acc
        ; false -> extract_parser_opts(Rest, Acc)
    end.


-define(start_object, <<"{">>).
-define(start_array, <<"[">>).
-define(end_object, <<"}">>).
-define(end_array, <<"]">>).
-define(colon, <<":">>).
-define(comma, <<",">>).
-define(quote, <<"\"">>).
-define(space, <<" ">>).
-define(newline, <<"\n">>).


fold(Event, {start, Acc, Opts}) ->
    case Event of
        {Type, Value} -> {[], [Acc, encode(Type, Value)], Opts}
        ; start_object -> {[object_start], [Acc, ?start_object], Opts}
        ; start_array -> {[array_start], [Acc, ?start_array], Opts}
    end;
fold(Event, {[object_start|Stack], Acc, OldOpts = #opts{depth = Depth}}) ->
    Opts = OldOpts#opts{depth = Depth + 1},
    case Event of
        {key, Key} ->
            {[object_value|Stack], [Acc, indent(Opts), encode(string, Key), ?colon, space(Opts)], Opts}
        ; end_object ->
            {Stack, [Acc, ?end_object], OldOpts}
    end;
fold(Event, {[object_value|Stack], Acc, Opts}) ->
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[key|Stack], [Acc, encode(Type, Value)], Opts}
        ; start_object -> {[object_start, key|Stack], [Acc, ?start_object], Opts}
        ; start_array -> {[array_start, key|Stack], [Acc, ?start_array], Opts}
    end;
fold(Event, {[key|Stack], Acc, Opts = #opts{depth = Depth}}) ->
    case Event of
        {key, Key} ->
            {[object_value|Stack], [Acc, ?comma, indent_or_space(Opts), encode(string, Key), ?colon, space(Opts)], Opts}
        ; end_object ->
            NewOpts = Opts#opts{depth = Depth - 1},
            {Stack, [Acc, indent(NewOpts), ?end_object], NewOpts}
    end;
fold(Event, {[array_start|Stack], Acc, OldOpts = #opts{depth = Depth}}) ->
    Opts = OldOpts#opts{depth = Depth + 1},
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[array|Stack], [Acc, indent(Opts), encode(Type, Value)], Opts}
        ; start_object -> {[object_start, array|Stack], [Acc, indent(Opts), ?start_object], Opts}
        ; start_array -> {[array_start, array|Stack], [Acc, indent(Opts), ?start_array], Opts}
        ; end_array -> {Stack, [Acc, ?end_array], OldOpts}
    end;
fold(Event, {[array|Stack], Acc, Opts = #opts{depth = Depth}}) ->
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[array|Stack], [Acc, ?comma, indent_or_space(Opts), encode(Type, Value)], Opts}
        ; end_array ->
            NewOpts = Opts#opts{depth = Depth - 1},
            {Stack, [Acc, indent(NewOpts), ?end_array], NewOpts}
        ; start_object -> {[object_start, array|Stack], [Acc, ?comma, indent_or_space(Opts), ?start_object], Opts}
        ; start_array -> {[array_start, array|Stack], [Acc, ?comma, indent_or_space(Opts), ?start_array], Opts}
    end;
fold(end_json, {[], Acc, Opts}) -> encode(Acc, Opts).


encode(Acc, Opts) when is_list(Acc) ->
    case Opts#opts.output_encoding of
        iolist -> Acc
        ; utf8 -> unicode:characters_to_binary(Acc, utf8)
        ; _ -> erlang:error(badarg)
    end;
encode(string, String) ->
    [?quote, String, ?quote];
encode(literal, Literal) ->
    erlang:atom_to_list(Literal);
encode(integer, Integer) ->
    erlang:integer_to_list(Integer);
encode(float, Float) ->
    jsx_utils:nice_decimal(Float).


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

basic_test_() ->
    [
        {"empty object", ?_assert(format(<<"{}">>, []) =:= <<"{}">>)},
        {"empty array", ?_assert(format(<<"[]">>, []) =:= <<"[]">>)},
        {"naked integer", ?_assert(format(<<"123">>, []) =:= <<"123">>)},
        {"naked float", ?_assert(format(<<"1.23">>, []) =:= <<"1.23">>)},
        {"naked string", ?_assert(format(<<"\"hi\"">>, []) =:= <<"\"hi\"">>)},
        {"naked literal", ?_assert(format(<<"true">>, []) =:= <<"true">>)},
        {"simple object", 
            ?_assert(format(<<"  { \"key\"  :\n\t \"value\"\r\r\r\n }  ">>, 
                    []
                ) =:= <<"{\"key\":\"value\"}">>
            )
        },
        {"really simple object",
            ?_assert(format(<<"{\"k\":\"v\"}">>, []) =:= <<"{\"k\":\"v\"}">>)
        },
        {"simple array", 
            ?_assert(format(<<" [\n\ttrue,\n\tfalse  ,  \n \tnull\n] ">>, 
                    []
                ) =:= <<"[true,false,null]">>
            )
        },
        {"really simple array", ?_assert(format(<<"[1]">>, []) =:= <<"[1]">>)},
        {"nested structures", 
            ?_assert(format(
                    <<"[{\"key\":\"value\", 
                            \"another key\": \"another value\",
                            \"a list\": [true, false]
                        }, 
                        [[{}]]
                    ]">>, []
                ) =:= <<"[{\"key\":\"value\",\"another key\":\"another value\",\"a list\":[true,false]},[[{}]]]">>
            )
        }
    ].

opts_test_() ->
    [
        {"unspecified indent/space", 
            ?_assert(format(<<" [\n\ttrue,\n\tfalse,\n\tnull\n] ">>, 
                    [space, indent]
                ) =:= <<"[\n true,\n false,\n null\n]">>
            )
        },
        {"specific indent/space", 
            ?_assert(format(
                    <<"\n{\n\"key\"  :  [],\n\"another key\"  :  true\n}\n">>, 
                    [{space, 2}, {indent, 3}]
                ) =:= <<"{\n   \"key\":  [],\n   \"another key\":  true\n}">>
            )
        },
        {"nested structures", 
            ?_assert(format(
                    <<"[{\"key\":\"value\", 
                            \"another key\": \"another value\"
                        }, 
                        [[true, false, null]]
                    ]">>, 
                    [{space, 2}, {indent, 2}]
                ) =:= <<"[\n  {\n    \"key\":  \"value\",\n    \"another key\":  \"another value\"\n  },\n  [\n    [\n      true,\n      false,\n      null\n    ]\n  ]\n]">>
            )
        },
        {"array spaces", 
            ?_assert(format(<<"[1,2,3]">>, 
                    [{space, 2}]
                ) =:= <<"[1,  2,  3]">>
            )
        },
        {"object spaces",
            ?_assert(format(<<"{\"a\":true,\"b\":true,\"c\":true}">>,
                    [{space, 2}]
                ) =:= <<"{\"a\":  true,  \"b\":  true,  \"c\":  true}">>
            )
        },
        {"array indent", 
            ?_assert(format(<<"[1.0, 2.0, 3.0]">>, 
                    [{indent, 2}]
                ) =:= <<"[\n  1.0,\n  2.0,\n  3.0\n]">>
            )
        },
        {"object indent",
            ?_assert(format(<<"{\"a\":true,\"b\":true,\"c\":true}">>,
                    [{indent, 2}]
                ) =:= <<"{\n  \"a\":true,\n  \"b\":true,\n  \"c\":true\n}">>
            )
        }
    ].

terms_test_() ->
    [
        {"terms",
            ?_assert(format([start_object,
                {key, <<"key">>},
                {string, <<"value">>},
                end_object,
                end_json
            ], []) =:= <<"{\"key\":\"value\"}">>
        )}
    ].
    
-endif.