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


%% @author Alisdair Sullivan <alisdairsullivan@yahoo.ca>
%% @copyright 2010 Alisdair Sullivan
%% @version really, really beta
%% @doc this module defines the interface to the jsx json parsing library


-module(jsx).
-author("alisdairsullivan@yahoo.ca").


%% the core parser api
-export([parser/0, parser/1]).
-export([term_to_json/1, term_to_json/2]).
-export([json_to_term/1, json_to_term/2]).
-export([is_json/1, is_json/2]).
-export([format/1, format/2]).
-export([eventify/1]).


%% function and type specifications
-include("./include/jsx.hrl").


%% @type jsx_parser() = (binary()) -> jsx_parser_result().

%% @type jsx_parser_result() = {event, jsx_event(), (() -> jsx_parser_result())}
%%    | {incomplete, jsx_parser()}
%%    | {error, badjson}.

%% @type jsx_event() = start_object
%%    | end_object
%%    | start_array
%%    | end_array
%%    | end_json
%%    | {key, unicode_string()}
%%    | {string, unicode_string()}
%%    | {integer, unicode_string()}
%%    | {float, unicode_string()}
%%    | {literal, true}
%%    | {literal, false}
%%    | {literal, null}.

%% @type unicode_string() = [integer()].

%% @type jsx_opts() = [jsx_opt()].
%% @type jsx_opt() = {comments, true | false}
%%    | {escaped_unicode, ascii | codepoint | none}
%%    | {multi_term, true | false}
%%    | {encoding, auto | supported_utf()}.

%% @type supported_utf() = utf8 | utf16 | {utf16, little} | utf32 | {utf32, little}.

%% @type eep0018() = eep0018_object() | eep0018_array().

%% @type eep0018_array() = [eep0018_term()].
%% @type eep0018_object() = [{eep0018_key(), eep0018_term()}].

%% @type eep0018_key() = binary() | atom().

%% @type eep0018_term() = eep0018_array() | eep0018_object() | eep0018_string() | eep0018_number() | true | false | null.

%% @type eep0018_string() = binary().

%% @type eep0018_number() = float() | integer().

%% @type encoder_opts() = [encoder_opt()].
%% @type encoder_opt() = {strict, true | false}
%%    | {encoding, supported_utf()}
%%    | {space, integer()}
%%    | space
%%    | {indent, integer()}
%%    | indent.


%% @type decoder_opts() = [decoder_opt()].
%% @type decoder_opt() = {strict, true | false}
%%    | {comments, true | false}
%%    | {encoding, supported_utf()}
%%    | {label, atom | binary | existing_atom}
%%    | {float, true | false}.


%% @type verify_opts() = [verify_opt()].
%% @type verify_opt() = {strict, true | false}
%%    | {encoding, auto | supported_utf()}
%%    | {comments, true | false}.


%% @type format_opts() = [format_opt()].
%% @type format_opt() = {strict, true | false}
%%    | {encoding, auto | supported_utf()}
%%    | {comments, true | false}
%%    | {space, integer()}
%%    | space
%%    | {indent, integer()}
%%    | indent
%%    | {output_encoding, supported_utf()}.


%% @spec parser() -> jsx_parser()
%% @equiv parser([])

parser() ->
    parser([]).
    
%% @spec parser(Opts::jsx_opts()) -> jsx_parser()    
%% @doc
%% produces a function which takes a binary which may or may not represent an encoded json document and returns a generator
%%
%% ```
%%      options:
%%
%%        {comments, true | false}
%%          if true, json documents that contain c style (/* ... */) comments
%%          will be parsed as if they did not contain any comments. default is
%%          false
%%    
%%        {encoded_unicode, ascii | codepoint | none}
%%          if a \uXXXX escape sequence is encountered within a key or string,
%%          this option controls how it is interpreted. none makes no attempt
%%          to interpret the value, leaving it unconverted. ascii will convert
%%          any value that falls within the ascii range. codepoint will convert
%%          any value that is a valid unicode codepoint. note that unicode
%%          non-characters (including badly formed surrogates) will never be
%%          converted. codepoint is the default
%%
%%        {encoding, auto | utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
%%          attempt to parse the binary using the specified encoding. auto will
%%          auto detect any supported encoding and is the default
%%
%%        {multi_term, true | false}
%%          usually, documents will be parsed in full before the end_json
%%          event is emitted. setting this option to true will instead emit
%%          the end_json event as soon as a valid document is parsed and then
%%          reset the parser to it's initial state and attempt to parse the
%%          remainder as a new json document. this allows streams containing
%%          multiple documents to be parsed correctly
%% '''
%% @end

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
    

%% @spec json_to_term(JSON::binary()) -> eep0018()
%% @equiv json_to_term(JSON, [])

json_to_term(JSON) ->
    json_to_term(JSON, []).
    
%% @spec json_to_term(JSON::binary(), Opts::decoder_opts()) -> eep0018()
%% @doc
%% produces an eep0018 representation of a binary encoded json document
%%
%% ```
%%      options:
%% 
%%        {strict, true | false}
%%          by default, attempting to convert unwrapped json values (numbers, strings and
%%          the atoms true, false and null) result in a badarg exception. if strict equals
%%          false, these are instead decoded to their equivalent eep0018 value. default is
%%          false
%%     
%%        {encoding, auto | utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
%%          assume the binary is encoded using the specified binary. default is auto, which
%%          attempts to autodetect the encoding
%%     
%%        {comments, true | false}
%%          if true, json documents that contain c style (/* ... */) comments
%%          will be parsed as if they did not contain any comments. default is
%%          false
%%     
%%        {label, atom | existing_atom | binary}
%%          json keys (labels) are decoded to utf8 encoded binaries, atoms or 
%%          existing_atoms (atom if it exists, binary otherwise) as specified by 
%%          this option. default is binary
%%         
%%        {float, true | false}
%%          return all numbers as floats. default is false
%% '''
%% @end

json_to_term(JSON, Opts) ->
    jsx_eep0018:json_to_term(JSON, Opts).


%% @spec term_to_json(JSON::eep0018()) -> binary()
%% @equiv term_to_json(JSON, [])

term_to_json(JSON) ->
    term_to_json(JSON, []).
    
%% @spec term_to_json(JSON::eep0018(), Opts::encoder_opts()) -> binary()
%% @doc
%% takes the erlang representation of a json object (as defined in eep0018) and returns a (binary encoded) json string
%%
%% ```    
%%      options:
%%      
%%        {strict, true | false}
%%          by default, attempting to convert unwrapped json values (numbers, 
%%          strings and the atoms true, false and null) result in a badarg exception. 
%%          if strict equals false, these are instead json encoded. default is false
%%        
%%          note that there is a problem of ambiguity when parsing unwrapped json
%%          numbers that requires special handling, see the [[notes|technotes]]
%%        
%%        {encoding, utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
%%          the encoding of the resulting binary. default is utf8
%%        
%%        space
%%        {space, N}
%%          place N spaces after each colon and comma in the resulting binary. space 
%%          implies {space, 1}. default is zero
%%          
%%        indent 
%%          {indent, N}
%%          indent each 'level' of the json structure by N spaces. indent implies 
%%          {indent, 1}. default is zero
%% '''
%% @end        

term_to_json(JSON, Opts) ->
    jsx_eep0018:term_to_json(JSON, Opts).


%% @spec is_json(JSON::binary()) -> true | false
%% @equiv is_json(JSON, [])

is_json(JSON) ->
    is_json(JSON, []).
    
%% @spec is_json(JSON::binary(), verify_opts()) -> true | false
%% @doc
%% returns true if the binary is an encoded json document, false otherwise
%%
%% ```
%%      options:
%%
%%        {strict, true | false}
%%          by default,  unwrapped json values (numbers, strings and the atoms 
%%          true, false and null) return false. if strict equals true, is_json
%%          returns true. default is false
%%  
%%        {encoding, auto | utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
%%          assume the binary is encoded using the specified binary. default is auto, 
%%          which attempts to autodetect the encoding
%%  
%%        {comments, true | false}
%%          if true, json documents that contain c style (/* ... */) comments
%%          will be parsed as if they did not contain any comments. default is
%%          false
%% '''
%% @end

is_json(JSON, Opts) ->
    jsx_verify:is_json(JSON, Opts).


%% @spec format(JSON::binary()) -> binary()
%% @equiv format(JSON, [])

format(JSON) ->
    format(JSON, []).
    
%% @spec format(JSON::binary(), Opts::format_opts()) -> binary()
%% @doc
%% formats a binary encoded json string according to the options chose. the defaults will produced a string stripped of all whitespace
%%
%% ```
%%      options:
%%
%%        {strict, true | false}
%%          by default,  unwrapped json values (numbers, strings and the atoms 
%%          true, false and null) result in an error. if strict equals true, they
%%          are treated as valid json. default is false
%%  
%%        {encoding, auto | utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
%%          assume the binary is encoded using the specified binary. default is auto, 
%%          which attempts to autodetect the encoding
%%    
%%        {output_encoding, utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
%%          the encoding of the resulting binary. default is utf8
%%    
%%        {comments, true | false}
%%          if true, json documents that contain c style (/* ... */) comments
%%          will be parsed as if they did not contain any comments. default is
%%          false 
%%    
%%        space
%%          {space, N}
%%          place N spaces after each colon and comma in the resulting binary. space 
%%          implies {space, 1}. default is zero
%%
%%        indent 
%%          {indent, N}
%%          indent each 'level' of the json structure by N spaces. indent implies 
%%          {indent, 1}. default is zero
%% '''
%% @end

format(JSON, Opts) ->
    jsx_format:format(JSON, Opts).
    

%% @spec eventify(List::list()) -> jsx_parser_result()
%% @doc fake the jsx api for any list. useful if you want to serialize a structure to json using the pretty printer, or verify a sequence could be valid json
eventify([]) ->
    fun() -> {incomplete, fun(List) when is_list(List) -> eventify(List); (_) -> erlang:error(badarg) end} end;    
eventify([Next|Rest]) ->
    fun() -> {event, Next, eventify(Rest)} end.  


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