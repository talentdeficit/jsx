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
    depth = 0
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
        ; X when X > 0 -> binary:copy(?space, X)
    end.


indent(Opts) ->
    case Opts#opts.indent of
        0 -> []
        ; X when X > 0 ->
            Indent = binary:copy(?space, X),
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


opts_test_() ->
    [
        {"empty opts", ?_assertEqual(#opts{}, parse_opts([]))},
        {"unspecified indent/space", ?_assertEqual(
            #opts{space=1, indent=1},
            parse_opts([space, indent])
        )},
        {"specific indent", ?_assertEqual(
            #opts{indent=4},
            parse_opts([{indent, 4}])
        )},
        {"specific space", ?_assertEqual(
            #opts{space=2},
            parse_opts([{space, 2}])
        )},
        {"specific space and indent", ?_assertEqual(
            #opts{space=2, indent=2},
            parse_opts([{space, 2}, {indent, 2}])
        )},
        {"invalid opt flag", ?_assertError(badarg, parse_opts([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_opts([{error, true}]))}
    ].


space_test_() ->
    [
        {"no space", ?_assertEqual([], space(#opts{space=0}))},
        {"one space", ?_assertEqual(<<" ">>, space(#opts{space=1}))},
        {"four spaces", ?_assertEqual(<<"    ">>, space(#opts{space=4}))}
    ].


indent_test_() ->
    [
        {"no indent", ?_assertEqual([], indent(#opts{indent=0, depth=1}))},
        {"indent 1 depth 1", ?_assertEqual(
            [[?newline], ?space],
            indent(#opts{indent=1, depth=1})
        )},
        {"indent 1 depth 2", ?_assertEqual(
            [[[?newline], ?space], ?space],
            indent(#opts{indent=1, depth=2})
        )},
        {"indent 4 depth 1", ?_assertEqual(
            [[?newline], <<"    ">>],
            indent(#opts{indent=4, depth=1})
        )},
        {"indent 4 depth 2", ?_assertEqual(
            [[[?newline], <<"    ">>], <<"    ">>],
            indent(#opts{indent=4, depth=2})
        )}
    ].


indent_or_space_test_() ->
    [
        {"no indent so space", ?_assertEqual(
            <<" ">>,
            indent_or_space(#opts{space=1, indent=0, depth=1})
        )},
        {"indent so no space", ?_assertEqual(
            [[?newline], ?space],
            indent_or_space(#opts{space=1, indent=1, depth=1})
        )}
    ].


format_test_() ->
    [
        {"0.0", ?_assert(encode(float, 0.0, #opts{}) =:= "0.0")},
        {"1.0", ?_assert(encode(float, 1.0, #opts{}) =:= "1.0")},
        {"-1.0", ?_assert(encode(float, -1.0, #opts{}) =:= "-1.0")},
        {"3.1234567890987654321", 
            ?_assert(
                encode(float, 3.1234567890987654321, #opts{}) =:= "3.1234567890987655")
        },
        {"1.0e23", ?_assert(encode(float, 1.0e23, #opts{}) =:= "1.0e23")},
        {"0.3", ?_assert(encode(float, 3.0/10.0, #opts{}) =:= "0.3")},
        {"0.0001", ?_assert(encode(float, 0.0001, #opts{}) =:= "0.0001")},
        {"0.00001", ?_assert(encode(float, 0.00001, #opts{}) =:= "1.0e-5")},
        {"0.00000001", ?_assert(encode(float, 0.00000001, #opts{}) =:= "1.0e-8")},
        {"1.0e-323", ?_assert(encode(float, 1.0e-323, #opts{}) =:= "1.0e-323")},
        {"1.0e308", ?_assert(encode(float, 1.0e308, #opts{}) =:= "1.0e308")},
        {"min normalized float", 
            ?_assert(
                encode(float, math:pow(2, -1022), #opts{}) =:= "2.2250738585072014e-308"
            )
        },
        {"max normalized float", 
            ?_assert(
                encode(float, (2 - math:pow(2, -52)) * math:pow(2, 1023), #opts{}) 
                    =:= "1.7976931348623157e308"
            )
        },
        {"min denormalized float", 
            ?_assert(encode(float, math:pow(2, -1074), #opts{}) =:= "5.0e-324")
        },
        {"max denormalized float", 
            ?_assert(
                encode(float, (1 - math:pow(2, -52)) * math:pow(2, -1022), #opts{}) 
                    =:= "2.225073858507201e-308"
            )
        }
    ].


handle_event_test_() ->
    Data = jsx:empty_array()
        ++ jsx:deep_array()
        ++ jsx:really_deep_array()
        ++ jsx:empty_object()
        ++ jsx:literals()
        ++ jsx:naked_literals()
        ++ jsx:integers()
        ++ jsx:naked_integers()
        ++ jsx:floats()
        ++ jsx:naked_floats(),
    [
        {
            Title, ?_assertEqual(
                JSON,
                lists:foldl(fun handle_event/2, {start, [], #opts{}}, Events ++ [end_json])
            )
        } || {Title, JSON, _, Events} <- Data
    ].


-endif.