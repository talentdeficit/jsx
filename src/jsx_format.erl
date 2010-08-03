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
-author("alisdairsullivan@yahoo.ca").

-export([pp/2]).

-include("./include/jsx_types.hrl").



-record(opts, {
    space = 0,
    indent = 0,
    output_encoding = iolist,
    strict = true
}).



-define(newline, $\n).
-define(space, 16#20).  %% ascii code for space
-define(quote, $\").
-define(comma, $,).
-define(colon, $:).
-define(start_object, ${).
-define(end_object, $}).
-define(start_array, $[).
-define(end_array, $]).



-spec format(JSON::binary(), Opts::format_opts()) -> binary() | iolist().
    
pp(F, Opts) when is_function(F) ->
    prettify(F(), [], parse_opts(Opts, #opts{}), 0, start);
    
pp(JSON, Opts) when is_binary(JSON) ->
    P = jsx:parser(extract_parser_opts(Opts)),
    prettify(P(JSON), [], parse_opts(Opts, #opts{}), 0, start). 


parse_opts([{indent, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{indent = Val});
parse_opts([indent|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{indent = 1});
parse_opts([{space, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{space = Val});
parse_opts([space|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{space = 1});
parse_opts([{output_encoding, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{output_encoding = Val});
parse_opts([], Opts) ->
    Opts.


extract_parser_opts(Opts) ->
    [ {K, V} || {K, V} <- Opts, lists:member(K, [comments, encoding]) ].
    

prettify({event, start_object, Next}, Acc, Opts, Level, start) ->
    prettify(Next(), [Acc, ?start_object], Opts, Level + 1, new);
prettify({event, start_object, Next}, Acc, Opts, Level, _) ->
    prettify(Next(), 
            [Acc, ?comma, space(Opts), indent(Opts, Level), ?start_object], 
            Opts, 
            Level + 1, 
            new);

prettify({event, start_array, Next}, Acc, Opts, Level, start) ->
    prettify(Next(), [Acc, ?start_array], Opts, Level + 1, new);
prettify({event, start_array, Next}, Acc, Opts, Level, _) ->
    prettify(Next(),
            [Acc, ?comma, space(Opts), indent(Opts, Level), ?start_array],
            Opts,
            Level + 1,
            new);

prettify({event, end_object, Next}, Acc, Opts, Level, value) ->
    DeLevel = Level - 1,
    prettify(Next(), [Acc, indent(Opts, DeLevel), ?end_object], Opts, DeLevel, value);
prettify({event, end_object, Next}, Acc, Opts, Level, new) ->
    prettify(Next(), [Acc, ?end_object], Opts, Level - 1, value);
    
prettify({event, end_array, Next}, Acc, Opts, Level, value) ->
    DeLevel = Level - 1,
    prettify(Next(), [Acc, indent(Opts, DeLevel), ?end_array], Opts, DeLevel, value);
prettify({event, end_array, Next}, Acc, Opts, Level, new) ->
    prettify(Next(), [Acc, ?end_array], Opts, Level - 1, value);

prettify({event, {key, Key}, Next}, Acc, Opts, Level, value) ->
    prettify(Next(), 
            [Acc, ?comma, space(Opts), indent(Opts, Level), format(string, Key), ?colon, space(Opts)], 
            Opts, 
            Level, 
            key);
prettify({event, {key, Key}, Next}, Acc, Opts, Level, _) ->
    prettify(Next(), 
            [Acc, indent(Opts, Level), format(string, Key), ?colon, space(Opts)], 
            Opts, 
            Level, 
            key);

prettify({event, {Type, Value}, Next}, Acc, Opts, Level, value)  ->
    prettify(Next(), 
            [Acc, ?comma, space(Opts), indent(Opts, Level), format(Type, Value)], 
            Opts, 
            Level, 
            value);
prettify({event, {Type, Value}, Next}, Acc, Opts, Level, new)  ->
    prettify(Next(), [Acc, indent(Opts, Level), format(Type, Value)], Opts, Level, value);
prettify({event, {Type, Value}, Next}, Acc, Opts, Level, key) ->
    prettify(Next(), [Acc, format(Type, Value)], Opts, Level, value);
prettify({event, {Type, Value}, Next}, _Acc, Opts, Level, start) ->
    case Opts#opts.strict of
        true -> erlang:throw(badarg)
        ; false -> prettify(Next(), [format(Type, Value)], Opts, Level, error)
    end;

prettify({event, end_json, Next}, Acc, Opts, _, _) ->
    case Next() of
        {incomplete, More} -> case More(end_stream) of
            ok -> encode(Acc, Opts)
            ; _ -> erlang:throw(badarg)
        end
        ; _ -> erlang:throw(badarg)
    end;
    
prettify(_, _, _, _, error) -> erlang:throw(badarg).

format(string, String) ->
    [?quote, String, ?quote];
format(literal, Literal) ->
    erlang:atom_to_list(Literal);
format(_, Number) ->
    Number.


indent(Opts, Level) ->
    case Opts#opts.indent of
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
    case Opts#opts.space of
        0 -> []
        ; X when X > 0 -> [ ?space || _ <- lists:seq(1, X) ]
    end.


-define(is_utf_encoding(X),
    X == utf8; X == utf16; X == utf32; X == {utf16, little}; X == {utf32, little}
).    
    
encode(Acc, Opts) ->
    case Opts#opts.output_encoding of
        iolist -> Acc
        ; UTF when ?is_utf_encoding(UTF) -> unicode:characters_to_binary(Acc, utf8, UTF)
        ; _ -> erlang:throw(badarg)
    end.