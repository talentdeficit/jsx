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


%% @hidden hide this module from edoc, exported functions are internal to jsx
%%   and may be altered or removed without notice


-module(jsx_format).


-export([format/2]).


-include("./include/jsx_common.hrl").
-include("./include/jsx_format.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



-spec format(JSON::binary(), Opts::format_opts()) -> binary() | iolist().
    
format(JSON, Opts) when is_binary(JSON) ->
    P = jsx:parser(extract_parser_opts(Opts)),
    format(fun() -> P(JSON) end, Opts);
format(F, OptsList) when is_function(F) ->
    Opts = parse_opts(OptsList, #format_opts{}),
    {Continue, String} = format_something(F(), Opts, 0),
    case Continue() of
        {event, end_json, _} -> encode(String, Opts)
        ; _ -> {error, badarg}
    end.


parse_opts([{indent, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#format_opts{indent = Val});
parse_opts([indent|Rest], Opts) ->
    parse_opts(Rest, Opts#format_opts{indent = 1});
parse_opts([{space, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#format_opts{space = Val});
parse_opts([space|Rest], Opts) ->
    parse_opts(Rest, Opts#format_opts{space = 1});
parse_opts([{output_encoding, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#format_opts{output_encoding = Val});
parse_opts([_|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts([], Opts) ->
    Opts.


extract_parser_opts(Opts) ->
    [ {K, V} || {K, V} <- Opts, lists:member(K, [comments, encoding]) ].
    

format_something({event, start_object, Next}, Opts, Level) ->
    case Next() of
        {event, end_object, Continue} ->
            {Continue, [?start_object, ?end_object]}
        ; Event ->
            {Continue, Object} = format_object(Event, [], Opts, Level + 1),
            {Continue, [?start_object, 
                Object, 
                indent(Opts, Level), 
                ?end_object
            ]}
    end;
format_something({event, start_array, Next}, Opts, Level) ->
    case Next() of
        {event, end_array, Continue} ->
            {Continue, [?start_array, ?end_array]}
        ; Event ->
            {Continue, Object} = format_array(Event, [], Opts, Level + 1),
            {Continue, [?start_array, Object, indent(Opts, Level), ?end_array]}
    end;
format_something({event, {Type, Value}, Next}, _Opts, _Level) ->
    {Next, [encode(Type, Value)]}.
    
    
format_object({event, end_object, Next}, Acc, _Opts, _Level) ->
    {Next, Acc};
format_object({event, {key, Key}, Next}, Acc, Opts, Level) ->
    {Continue, Value} = format_something(Next(), Opts, Level),
    case Continue() of
        {event, end_object, NextNext} -> 
            {NextNext, [Acc, 
                indent(Opts, Level), 
                encode(string, Key), 
                ?colon, 
                space(Opts), 
                Value
            ]}
        ; Else -> 
            format_object(Else, 
                [Acc, 
                    indent(Opts, Level), 
                    encode(string, Key), 
                    ?colon, 
                    space(Opts), 
                    Value, 
                    ?comma, 
                    space(Opts)
                ], 
                Opts, 
                Level
            )
    end.


format_array({event, end_array, Next}, Acc, _Opts, _Level) ->
    {Next, Acc};
format_array(Event, Acc, Opts, Level) ->
    {Continue, Value} = format_something(Event, Opts, Level),
    case Continue() of
        {event, end_array, NextNext} ->
            {NextNext, [Acc, indent(Opts, Level), Value]}
        ; Else ->
            format_array(Else, 
                [Acc, 
                    indent(Opts, Level),
                    Value, 
                    ?comma, 
                    space(Opts)
                ], 
                Opts, 
                Level
            )
    end.


encode(Acc, Opts) when is_list(Acc) ->
    case Opts#format_opts.output_encoding of
        iolist -> Acc
        ; UTF when ?is_utf_encoding(UTF) -> 
            unicode:characters_to_binary(Acc, utf8, UTF)
        ; _ -> erlang:throw(badarg)
    end;
encode(string, String) ->
    [?quote, String, ?quote];
encode(literal, Literal) ->
    erlang:atom_to_list(Literal);
encode(_, Number) ->
    Number.


indent(Opts, Level) ->
    case Opts#format_opts.indent of
        0 -> []
        ; X when X > 0 ->
            Indent = [ ?space || _ <- lists:seq(1, X) ],
            indent(Indent, Level, [?newline])
    end.

indent(_Indent, 0, Acc) ->
    Acc;
indent(Indent, N, Acc) ->
    indent(Indent, N - 1, [Acc, Indent]).
    
    
space(Opts) ->
    case Opts#format_opts.space of
        0 -> []
        ; X when X > 0 -> [ ?space || _ <- lists:seq(1, X) ]
    end.
    

%% eunit tests

-ifdef(TEST).

minify_test_() ->
    [
        {"minify object", 
            ?_assert(format(<<"  { \"key\"  :\n\t \"value\"\r\r\r\n }  ">>, 
                    []
                ) =:= <<"{\"key\":\"value\"}">>
            )
        },
        {"minify array", 
            ?_assert(format(<<" [\n\ttrue,\n\tfalse,\n\tnull\n] ">>, 
                    []
                ) =:= <<"[true,false,null]">>
            )
        }
    ].
    
opts_test_() ->
    [
        {"unspecified indent/space", 
            ?_assert(format(<<" [\n\ttrue,\n\tfalse,\n\tnull\n] ">>, 
                    [space, indent]
                ) =:= <<"[\n true, \n false, \n null\n]">>
            )
        },
        {"specific indent/space", 
            ?_assert(format(
                    <<"\n{\n\"key\"  :  [],\n\"another key\"  :  true\n}\n">>, 
                    [{space, 2}, {indent, 3}]
                ) =:= <<"{\n   \"key\":  [],  \n   \"another key\":  true\n}">>
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
                ) =:= <<"[\n  {\n    \"key\":  \"value\",  \n    \"another key\":  \"another value\"\n  },  \n  [\n    [\n      true,  \n      false,  \n      null\n    ]\n  ]\n]">>
            )
        },
        {"just spaces", 
            ?_assert(format(<<"[1,2,3]">>, 
                    [{space, 2}]
                ) =:= <<"[1,  2,  3]">>
            )
        },
        {"just indent", 
            ?_assert(format(<<"[1.0, 2.0, 3.0]">>, 
                    [{indent, 2}]
                ) =:= <<"[\n  1.0,\n  2.0,\n  3.0\n]">>
            )
        }
    ].
    
-endif.