%% The MIT License

%% Copyright (c) 2010-2013 alisdair sullivan <alisdairsullivan@yahoo.ca>

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


-record(config, {
    space = 0,
    indent = 0,
    depth = 0
}).

-type config() :: list().
-export_type([config/0]).


-spec to_json(Source::any(), Config::config()) -> binary().

to_json(Source, Config) when is_list(Config) ->
    (jsx:encoder(?MODULE, Config, jsx_config:extract_config(Config ++ [escaped_strings])))(Source).


-spec format(Source::binary(), Config::config()) -> binary().

format(Source, Config) when is_binary(Source) andalso is_list(Config) ->
    (jsx:decoder(?MODULE, Config, jsx_config:extract_config(Config ++ [escaped_strings])))(Source);
format(_, _) -> erlang:error(badarg).


parse_config(Config) -> parse_config(Config, #config{}).

parse_config([{space, Val}|Rest], Config) when is_integer(Val), Val > 0 ->
    parse_config(Rest, Config#config{space = Val});
parse_config([space|Rest], Config) ->
    parse_config(Rest, Config#config{space = 1});
parse_config([{indent, Val}|Rest], Config) when is_integer(Val), Val > 0 ->
    parse_config(Rest, Config#config{indent = Val});
parse_config([indent|Rest], Config) ->
    parse_config(Rest, Config#config{indent = 1});
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



-define(start_object, <<"{">>).
-define(start_array, <<"[">>).
-define(end_object, <<"}">>).
-define(end_array, <<"]">>).
-define(colon, <<":">>).
-define(comma, <<",">>).
-define(quote, <<"\"">>).
-define(space, <<" ">>).
-define(newline, <<"\n">>).


-type state() :: {any(), unicode:charlist(), #config{}}.
-spec init(Config::proplists:proplist()) -> state().

init(Config) -> {start, [], parse_config(Config)}.

-spec handle_event(Event::any(), State::state()) -> state().

handle_event(Event, {start, Acc, Config}) ->
    case Event of
        {Type, Value} -> {[], [Acc, encode(Type, Value, Config)], Config}
        ; start_object -> {[object_start], [Acc, ?start_object], Config}
        ; start_array -> {[array_start], [Acc, ?start_array], Config}
    end;
handle_event(Event, {[object_start|Stack], Acc, OldConfig = #config{depth = Depth}}) ->
    Config = OldConfig#config{depth = Depth + 1},
    case Event of
        {key, Key} ->
            {[object_value|Stack], [Acc, indent(Config), encode(string, Key, Config), ?colon, space(Config)], Config}
        ; end_object ->
            {Stack, [Acc, ?end_object], OldConfig}
    end;
handle_event(Event, {[object_value|Stack], Acc, Config}) ->
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[key|Stack], [Acc, encode(Type, Value, Config)], Config}
        ; start_object -> {[object_start, key|Stack], [Acc, ?start_object], Config}
        ; start_array -> {[array_start, key|Stack], [Acc, ?start_array], Config}
    end;
handle_event(Event, {[key|Stack], Acc, Config = #config{depth = Depth}}) ->
    case Event of
        {key, Key} ->
            {[object_value|Stack], [Acc, ?comma, indent_or_space(Config), encode(string, Key, Config), ?colon, space(Config)], Config}
        ; end_object ->
            NewConfig = Config#config{depth = Depth - 1},
            {Stack, [Acc, indent(NewConfig), ?end_object], NewConfig}
    end;
handle_event(Event, {[array_start|Stack], Acc, OldConfig = #config{depth = Depth}}) ->
    Config = OldConfig#config{depth = Depth + 1},
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[array|Stack], [Acc, indent(Config), encode(Type, Value, Config)], Config}
        ; start_object -> {[object_start, array|Stack], [Acc, indent(Config), ?start_object], Config}
        ; start_array -> {[array_start, array|Stack], [Acc, indent(Config), ?start_array], Config}
        ; end_array -> {Stack, [Acc, ?end_array], OldConfig}
    end;
handle_event(Event, {[array|Stack], Acc, Config = #config{depth = Depth}}) ->
    case Event of
        {Type, Value} when Type == string; Type == literal;
                Type == integer; Type == float ->
            {[array|Stack], [Acc, ?comma, indent_or_space(Config), encode(Type, Value, Config)], Config}
        ; end_array ->
            NewConfig = Config#config{depth = Depth - 1},
            {Stack, [Acc, indent(NewConfig), ?end_array], NewConfig}
        ; start_object -> {[object_start, array|Stack], [Acc, ?comma, indent_or_space(Config), ?start_object], Config}
        ; start_array -> {[array_start, array|Stack], [Acc, ?comma, indent_or_space(Config), ?start_array], Config}
    end;
handle_event(end_json, {[], Acc, _Config}) -> unicode:characters_to_binary(Acc, utf8).


encode(string, String, _Config) ->
    [?quote, String, ?quote];
encode(literal, Literal, _Config) ->
    erlang:atom_to_list(Literal);
encode(integer, Integer, _Config) ->
    erlang:integer_to_list(Integer);
encode(float, Float, _Config) ->
    [Output] = io_lib:format("~p", [Float]), Output.


space(Config) ->
    case Config#config.space of
        0 -> []
        ; X when X > 0 -> binary:copy(?space, X)
    end.


indent(Config) ->
    case Config#config.indent of
        0 -> []
        ; X when X > 0 ->
            Indent = binary:copy(?space, X),
            indent(Indent, Config#config.depth, [?newline])
    end.

indent(_Indent, 0, Acc) -> Acc;
indent(Indent, N, Acc) -> indent(Indent, N - 1, [Acc, Indent]).


indent_or_space(Config) ->
    case Config#config.indent > 0 of
        true -> indent(Config)
        ; false -> space(Config)
    end.


%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"empty config", ?_assertEqual(#config{}, parse_config([]))},
        {"unspecified indent/space", ?_assertEqual(
            #config{space=1, indent=1},
            parse_config([space, indent])
        )},
        {"specific indent", ?_assertEqual(
            #config{indent=4},
            parse_config([{indent, 4}])
        )},
        {"specific space", ?_assertEqual(
            #config{space=2},
            parse_config([{space, 2}])
        )},
        {"specific space and indent", ?_assertEqual(
            #config{space=2, indent=2},
            parse_config([{space, 2}, {indent, 2}])
        )},
        {"invalid opt flag", ?_assertError(badarg, parse_config([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_config([{error, true}]))}
    ].


space_test_() ->
    [
        {"no space", ?_assertEqual([], space(#config{space=0}))},
        {"one space", ?_assertEqual(<<" ">>, space(#config{space=1}))},
        {"four spaces", ?_assertEqual(<<"    ">>, space(#config{space=4}))}
    ].


indent_test_() ->
    [
        {"no indent", ?_assertEqual([], indent(#config{indent=0, depth=1}))},
        {"indent 1 depth 1", ?_assertEqual(
            [[?newline], ?space],
            indent(#config{indent=1, depth=1})
        )},
        {"indent 1 depth 2", ?_assertEqual(
            [[[?newline], ?space], ?space],
            indent(#config{indent=1, depth=2})
        )},
        {"indent 4 depth 1", ?_assertEqual(
            [[?newline], <<"    ">>],
            indent(#config{indent=4, depth=1})
        )},
        {"indent 4 depth 2", ?_assertEqual(
            [[[?newline], <<"    ">>], <<"    ">>],
            indent(#config{indent=4, depth=2})
        )}
    ].


indent_or_space_test_() ->
    [
        {"no indent so space", ?_assertEqual(
            <<" ">>,
            indent_or_space(#config{space=1, indent=0, depth=1})
        )},
        {"indent so no space", ?_assertEqual(
            [[?newline], ?space],
            indent_or_space(#config{space=1, indent=1, depth=1})
        )}
    ].


format_test_() ->
    [
        {"0.0", ?_assert(encode(float, 0.0, #config{}) =:= "0.0")},
        {"1.0", ?_assert(encode(float, 1.0, #config{}) =:= "1.0")},
        {"-1.0", ?_assert(encode(float, -1.0, #config{}) =:= "-1.0")},
        {"3.1234567890987654321", 
            ?_assert(
                encode(float, 3.1234567890987654321, #config{}) =:= "3.1234567890987655")
        },
        {"1.0e23", ?_assert(encode(float, 1.0e23, #config{}) =:= "1.0e23")},
        {"0.3", ?_assert(encode(float, 3.0/10.0, #config{}) =:= "0.3")},
        {"0.0001", ?_assert(encode(float, 0.0001, #config{}) =:= "0.0001")},
        {"0.00001", ?_assert(encode(float, 0.00001, #config{}) =:= "1.0e-5")},
        {"0.00000001", ?_assert(encode(float, 0.00000001, #config{}) =:= "1.0e-8")},
        {"1.0e-323", ?_assert(encode(float, 1.0e-323, #config{}) =:= "1.0e-323")},
        {"1.0e308", ?_assert(encode(float, 1.0e308, #config{}) =:= "1.0e308")},
        {"min normalized float", 
            ?_assert(
                encode(float, math:pow(2, -1022), #config{}) =:= "2.2250738585072014e-308"
            )
        },
        {"max normalized float", 
            ?_assert(
                encode(float, (2 - math:pow(2, -52)) * math:pow(2, 1023), #config{}) 
                    =:= "1.7976931348623157e308"
            )
        },
        {"min denormalized float", 
            ?_assert(encode(float, math:pow(2, -1074), #config{}) =:= "5.0e-324")
        },
        {"max denormalized float", 
            ?_assert(
                encode(float, (1 - math:pow(2, -52)) * math:pow(2, -1022), #config{}) 
                    =:= "2.225073858507201e-308"
            )
        }
    ].


handle_event_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                JSON,
                lists:foldl(fun handle_event/2, {start, [], #config{}}, Events ++ [end_json])
            )
        } || {Title, JSON, _, Events} <- Data
    ].


-endif.
