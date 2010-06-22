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


-module(jsx).
-author("alisdairsullivan@yahoo.ca").

%% the core parser api
-export([parser/0, parser/1]).

%% example usage of core api
-export([is_json/1, is_json/2]).
-export([decode/1, decode/2, decode/3, decode/4]).
-export([fold/1, fold/2, fold/3, fold/4]).

%% types for function specifications
-include("jsx_types.hrl").

    

-spec parser() -> jsx_parser().
-spec parser(Opts::jsx_opts()) -> jsx_parser().

parser() ->
    parser([]).

parser(OptsList) ->
    F = case proplists:get_value(encoding, OptsList, auto) of
        utf8 -> fun jsx_utf8:parse/2
        ; utf16 -> fun jsx_utf16:parse/2
        ; utf32 -> fun jsx_utf32:parse/2
        ; {utf16, little} -> fun jsx_utf16le:parse/2
        ; {utf32, little} -> fun jsx_utf32le:parse/2
        ; auto -> fun detect_encoding/2
    end,
    start(F, OptsList).
    
start(F, OptsList) ->
    Opts = parse_opts(OptsList),
    fun(Stream) -> F(Stream, Opts) end.


-spec is_json(JSON::json()) -> true | false.
-spec is_json(JSON::json(), Opts::jsx_opts()) -> true | false.

is_json(JSON) ->
    is_json(JSON, []).

is_json(JSON, Opts) ->
    case fold(fun(end_json, ok) -> true ;(_, _) -> ok end, ok, JSON, Opts) of
        {incomplete, _} -> false
        ; {error, _} -> false
        ; {ok, true} -> true
    end.
 

-spec decode(JSON::json()) -> {ok, [jsx_event(),...]} | {error, atom()}.
-spec decode(JSON::json(), Parse::jsx_opts() | jsx_parser()) -> {ok, [jsx_event(),...]} | {error, atom()}. 
-spec decode(F::fun((jsx_event(), any()) -> any()), 
        Acc::any(), 
        JSON::json()) -> 
    {ok, any()} | {error, atom()}.
-spec decode(F::fun((jsx_event(), any()) -> any()), 
        Acc::any(), 
        JSON::json(), 
        Parse::jsx_opts() | jsx_parser()) -> 
    {ok, any()} | {error, atom()}.

decode(JSON) ->
    decode(JSON, []).
    
decode(JSON, Parse) ->
    F = fun(end_json, S) -> lists:reverse(S) ;(E, S) -> [E] ++ S end,
    decode(F, [], JSON, Parse).

decode(F, Acc, JSON) ->
    decode(F, Acc, JSON, []).

decode(F, Acc, JSON, Parse) ->
    case fold(F, Acc, JSON, Parse) of
        {ok, Result} -> {ok, Result}
        ; _ -> {error, badjson}
    end.
        
-spec fold(JSON::json()) -> 
    {ok, [jsx_event(),...]} | {incomplete, jsx_parser(), fun(() -> jsx_parser_result())} | {error, atom()}.
-spec fold(JSON::json(), Parse::jsx_opts() | jsx_parser()) -> 
    {ok, [jsx_event(),...]} | {incomplete, jsx_parser(), fun(() -> jsx_parser_result())} | {error, atom()}.
-spec fold(F::fun((jsx_event(), any()) -> any()), 
            Acc::any(), 
            JSON::json()) -> 
        {ok, any()} | {incomplete, jsx_parser(), fun(() -> jsx_parser_result())} | {error, atom()}.
-spec fold(F::fun((jsx_event(), any()) -> any()), 
            Acc::any(), 
            JSON::json(), 
            Opts::jsx_opts()) -> 
        {ok, any()} | {incomplete, jsx_parser(), fun(() -> jsx_parser_result())} | {error, atom()}
    ; (F::fun((jsx_event(), any()) -> any()), 
            Acc::any(), 
            JSON::json(), 
            Parser::jsx_parser()) -> 
        {ok, any()} | {incomplete, jsx_parser(), fun(() -> jsx_parser_result())} | {error, atom()}.
    
fold(JSON) ->
    fold(JSON, []).

fold(JSON, Parse) ->
    F = fun(end_json, S) -> lists:reverse(S) ;(E, S) -> [E] ++ S end,
    fold(F, [], JSON, Parse).
        
fold(F, Acc, JSON) ->
    P = jsx:parser(),
    fold(F, Acc, JSON, P).
    
fold(F, Acc, JSON, Opts) when is_list(Opts) ->
    P = jsx:parser(Opts),
    fold(F, Acc, JSON, P);
fold(F, Acc, JSON, P) ->
    fold_loop(F, Acc, P(JSON)).
 
fold_loop(F, Acc, {incomplete, Next, Force}) ->
    case Force() of
        {event, Val, End} -> 
            case End() of
                {event, end_json, _} -> {ok, F(end_json, F(Val, Acc))}
                ; _ -> {incomplete, fun(Bin) -> fold_loop(F, Acc, Next(Bin)) end}
            end
        ; _ -> {incomplete, fun(Bin) -> fold_loop(F, Acc, Next(Bin)) end}
    end;
fold_loop(_, _, {error, Error}) -> {error, Error};
fold_loop(F, Acc, {event, end_json, _}) -> {ok, F(end_json, Acc)};
fold_loop(F, Acc, {event, Event, Next}) -> fold_loop(F, F(Event, Acc), Next()).

    
    
%% option parsing

%% converts a proplist into a tuple
parse_opts(Opts) ->
    parse_opts(Opts, {false, codepoint, false}).

parse_opts([], Opts) ->
    Opts;    
parse_opts([{comments, Value}|Rest], {_Comments, EscapedUnicode, Stream}) ->
    true = lists:member(Value, [true, false]),
    parse_opts(Rest, {Value, EscapedUnicode, Stream});
parse_opts([{escaped_unicode, Value}|Rest], {Comments, _EscapedUnicode, Stream}) ->
    true = lists:member(Value, [ascii, codepoint, none]),
    parse_opts(Rest, {Comments, Value, Stream});
parse_opts([{stream_mode, Value}|Rest], {Comments, EscapedUnicode, _Stream}) ->
    true = lists:member(Value, [true, false]),
    parse_opts(Rest, {Comments, EscapedUnicode, Value});
parse_opts([{encoding, _}|Rest], Opts) ->
    parse_opts(Rest, Opts).
    
   
%% encoding detection   
    
%% first check to see if there's a bom, if not, use the rfc4627 method for determining
%%   encoding. this function makes some assumptions about the validity of the stream
%%   which may delay failure later than if an encoding is explicitly provided
    
%% utf8 bom detection    
detect_encoding(<<16#ef, 16#bb, 16#bf, Rest/binary>>, Opts) ->
    jsx_utf8:parse(Rest, Opts);    
    
%% utf32-little bom detection (this has to come before utf16-little)
detect_encoding(<<16#ff, 16#fe, 0, 0, Rest/binary>>, Opts) ->
    jsx_utf32le:parse(Rest, Opts);    
    
%% utf16-big bom detection
detect_encoding(<<16#fe, 16#ff, Rest/binary>>, Opts) ->
    jsx_utf16:parse(Rest, Opts);
    
%% utf16-little bom detection
detect_encoding(<<16#ff, 16#fe, Rest/binary>>, Opts) ->
    jsx_utf16le:parse(Rest, Opts);
    
%% utf32-big bom detection
detect_encoding(<<0, 0, 16#fe, 16#ff, Rest/binary>>, Opts) ->
    jsx_utf32:parse(Rest, Opts);
    

%% utf32-little null order detection
detect_encoding(<<X, 0, 0, 0, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    jsx_utf32le:parse(JSON, Opts);
    
%% utf16-big null order detection
detect_encoding(<<0, X, 0, Y, _Rest/binary>> = JSON, Opts) when X =/= 0, Y =/= 0 ->
    jsx_utf16:parse(JSON, Opts);
    
%% utf16-little null order detection
detect_encoding(<<X, 0, Y, 0, _Rest/binary>> = JSON, Opts) when X =/= 0, Y =/= 0 ->
    jsx_utf16le:parse(JSON, Opts);

%% utf32-big null order detection
detect_encoding(<<0, 0, 0, X, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    jsx_utf32:parse(JSON, Opts);
    
%% utf8 null order detection
detect_encoding(<<X, Y, _Rest/binary>> = JSON, Opts) when X =/= 0, Y =/= 0 ->
    jsx_utf8:parse(JSON, Opts);
    
%% a problem, to autodetect naked single digits' encoding, there is not enough data
%%   to conclusively determine the encoding correctly. below is an attempt to solve
%%   the problem
detect_encoding(<<X>>, Opts) when X =/= 0 ->
    {incomplete, 
        fun(Stream) -> detect_encoding(<<X, Stream/binary>>, Opts) end,
        fun() -> try 
                {incomplete, _, Force} = jsx_utf8:parse(<<X>>, Opts),
                Force()
                catch error:function_clause -> {error, badjson} 
            end 
        end
    };
detect_encoding(<<0, X>>, Opts) when X =/= 0 ->
    {incomplete, 
        fun(Stream) -> detect_encoding(<<0, X, Stream/binary>>, Opts) end,
        fun() -> try 
                {incomplete, _, Force} = jsx_utf16:parse(<<0, X>>, Opts),
                Force()
                catch error:function_clause -> {error, badjson} 
            end 
        end
    };
detect_encoding(<<X, 0>>, Opts) when X =/= 0 ->
    {incomplete, 
        fun(Stream) -> detect_encoding(<<X, 0, Stream/binary>>, Opts) end,
        fun() -> try 
                {incomplete, _, Force} = jsx_utf16le:parse(<<X, 0>>, Opts),
                Force()
                catch error:function_clause -> {error, badjson} 
            end 
        end
    };
    
%% not enough input, request more
detect_encoding(Bin, Opts) ->
    {incomplete, 
        fun(Stream) -> detect_encoding(<<Bin/binary, Stream/binary>>, Opts) end,
        fun() -> {error, badjson} end
    }.