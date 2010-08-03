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
-export([term_to_json/1, term_to_json/2]).
-export([json_to_term/1, json_to_term/2]).
-export([is_json/1, is_json/2]).
-export([format/1, format/2]).

%% types for function specifications
-include("./include/jsx_types.hrl").

%% opts record
-record(opts, {
    comments = false,
    escaped_unicode = codepoint,
    multi_term = false,
    encoding = auto
}).


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
    case parse_opts(OptsList) of 
        {error, badopt} -> {error, badopt}
        ; Opts -> fun(Stream) -> F(Stream, Opts) end
    end.
    
    
-spec term_to_json(JSON::json()) -> binary().
-spec term_to_json(JSON::json(), Opts::encoder_opts()) -> binary().

term_to_json(JSON) ->
    term_to_json(JSON, []).

term_to_json(JSON, Opts) ->
    json_encoder:term_to_json(JSON, Opts).


-spec json_to_term(JSON::binary()) -> json().
-spec json_to_term(JSON::binary(), Opts::decoder_opts()) -> json().    

json_to_term(JSON) ->
    json_to_term(JSON, []).

json_to_term(JSON, Opts) ->
    json_decoder:json_to_term(JSON, Opts).


-spec is_json(JSON::binary()) -> true | false.
-spec is_json(JSON::binary(), Opts::verify_opts()) -> true | false.

is_json(JSON) ->
    is_json(JSON, []).    

is_json(JSON, Opts) ->
    json_verify:is_json(JSON, Opts).


-spec format(JSON::binary()) -> binary() | iolist().
-spec format(JSON::binary(), Opts::format_opts()) -> binary() | iolist().

format(JSON) ->
    format(JSON, []).    

format(JSON, Opts) ->
    json_pp:pp(JSON, Opts).    


%% ----------------------------------------------------------------------------
%% internal functions
%% ----------------------------------------------------------------------------

%% option parsing

%% converts a proplist into a tuple
parse_opts(Opts) ->
    parse_opts(Opts, #opts{}).

parse_opts([], Opts) ->
    Opts;    
parse_opts([{comments, Value}|Rest], Opts) ->
    true = lists:member(Value, [true, false]),
    parse_opts(Rest, Opts#opts{comments = Value});
parse_opts([{escaped_unicode, Value}|Rest], Opts) ->
    true = lists:member(Value, [ascii, codepoint, none]),
    parse_opts(Rest, Opts#opts{escaped_unicode = Value});
parse_opts([{multi_term, Value}|Rest], Opts) ->
    true = lists:member(Value, [true, false]),
    parse_opts(Rest, Opts#opts{multi_term = Value});
parse_opts([{encoding, _}|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts(_, _) ->
    {error, badopt}.
    
   
%% encoding detection   
%% first check to see if there's a bom, if not, use the rfc4627 method for determining
%%   encoding. this function makes some assumptions about the validity of the stream
%%   which may delay failure later than if an encoding is explicitly provided
    
%% utf8 bom detection    
detect_encoding(<<16#ef, 16#bb, 16#bf, Rest/binary>>, Opts) -> jsx_utf8:parse(Rest, Opts);    
%% utf32-little bom detection (this has to come before utf16-little or it'll match that)
detect_encoding(<<16#ff, 16#fe, 0, 0, Rest/binary>>, Opts) -> jsx_utf32le:parse(Rest, Opts);        
%% utf16-big bom detection
detect_encoding(<<16#fe, 16#ff, Rest/binary>>, Opts) -> jsx_utf16:parse(Rest, Opts);
%% utf16-little bom detection
detect_encoding(<<16#ff, 16#fe, Rest/binary>>, Opts) -> jsx_utf16le:parse(Rest, Opts);
%% utf32-big bom detection
detect_encoding(<<0, 0, 16#fe, 16#ff, Rest/binary>>, Opts) -> jsx_utf32:parse(Rest, Opts);
    
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
        fun(end_stream) ->
                try
                    {incomplete, Next} = jsx_utf8:parse(<<X>>, Opts),
                    Next(end_stream)
                    catch error:function_clause -> {error, badjson}
                end
            ; (Stream) -> detect_encoding(<<X, Stream/binary>>, Opts) 
        end
    };
detect_encoding(<<0, X>>, Opts) when X =/= 0 ->
    {incomplete,
        fun(end_stream) ->
                try
                    {incomplete, Next} = jsx_utf16:parse(<<0, X>>, Opts),
                    Next(end_stream)
                    catch error:function_clause -> {error, badjson}
                end
            ; (Stream) -> detect_encoding(<<0, X, Stream/binary>>, Opts) 
        end
    };
detect_encoding(<<X, 0>>, Opts) when X =/= 0 ->
    {incomplete,
        fun(end_stream) ->
                try
                    {incomplete, Next} = jsx_utf16le:parse(<<X, 0>>, Opts),
                    Next(end_stream)
                    catch error:function_clause -> {error, badjson}
                end
            ; (Stream) -> detect_encoding(<<X, 0, Stream/binary>>, Opts)
        end
    };
    
%% not enough input, request more
detect_encoding(Bin, Opts) ->
    {incomplete,
        fun(end_stream) -> {error, badjson}
            ; (Stream) -> detect_encoding(<<Bin/binary, Stream/binary>>, Opts) 
        end
    }.