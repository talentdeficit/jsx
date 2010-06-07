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


-module(jsx_prettify).
-author("alisdairsullivan@yahoo.ca").


-export([pretty/2, jsx_event/2]).

-record(opts, {
    indent = 4
}).


pretty(JSON, Opts) ->
    Init = init(parse_opts(Opts, #opts{})),
    P = jsx:decoder({jsx_prettify, jsx_event, Init}, []),
    case P(JSON) of
        {incomplete, _} -> {error, badjson}
        ; {error, badjson} -> {error, badjson}
        ; {Result, _} -> Result
    end.    
    

parse_opts([{indent, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{indent = Val});
parse_opts([], Opts) ->
    Opts.


init(Opts) ->
    {[], Opts#opts.indent, 0, new}.
    
    
jsx_event(start_object, {Acc, Indent, Level, value}) ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ "{", Indent, Level + 1, new};
jsx_event(start_object, {Acc, Indent, Level, new}) ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ "{", Indent, Level + 1, new};
jsx_event(start_object, {Acc, Indent, Level, _}) ->
    {Acc ++ "{", Indent, Level + 1, new};
    
jsx_event(start_array, {Acc, Indent, Level, value}) ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ "[", Indent, Level + 1, new};
jsx_event(start_array, {Acc, Indent, Level, new}) ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ "[", Indent, Level + 1, new};
jsx_event(start_array, {Acc, Indent, Level, _}) ->
    {Acc ++ "[", Indent, Level + 1, new};

jsx_event(end_object, {Acc, Indent, Level, value}) ->
    {Acc ++ "\n" ++ indent(Indent, Level - 1) ++ "}", Indent, Level - 1, value};
jsx_event(end_object, {Acc, Indent, Level, new}) ->
    {Acc ++ "}", Indent, Level - 1, value};

jsx_event(end_array, {Acc, Indent, Level, value}) ->
    {Acc ++ "\n" ++ indent(Indent, Level - 1) ++ "]", Indent, Level - 1, value};
jsx_event(end_array, {Acc, Indent, Level, new}) ->
    {Acc ++ "]", Indent, Level - 1, value};

jsx_event({key, Key}, {Acc, Indent, Level, value}) ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ "\"" ++ Key ++ "\": ", Indent, Level, key};
jsx_event({key, Key}, {Acc, Indent, Level, _}) ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ "\"" ++ Key ++ "\": ", Indent, Level, key};

jsx_event({Type, Value}, {Acc, Indent, Level, value})  ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ format(Type, Value), Indent, Level, value};
jsx_event({Type, Value}, {Acc, Indent, Level, new})  ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ format(Type, Value), Indent, Level, value};
jsx_event({Type, Value}, {Acc, Indent, Level, key}) ->
    {Acc ++ format(Type, Value), Indent, Level, value};

jsx_event(reset, {_, Indent, _, _}) ->
    {[], Indent, 0, new};
jsx_event(end_of_json, {Acc, _, _, _}) ->
    Acc.
    

format(string, String) ->
    "\"" ++ String ++ "\"";
format(literal, Literal) ->
    erlang:atom_to_list(Literal);
format(_, Number) ->
    Number.
    

indent(Indent, Level) ->
    [ 16#20 || _ <- lists:seq(1, Indent * Level) ].
    