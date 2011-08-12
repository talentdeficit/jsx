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



%% opts record for decoder/encoder
-record(opts, {
    loose_unicode = false,
    encoding = auto,
    escape_forward_slash = false,
    iterate = false
}).



-define(is_utf_encoding(X),
    X == utf8
    ; X == utf16
    ; X == utf32
    ; X == {utf16, little}
    ; X == {utf32, little}
).



-type jsx_opts() :: [jsx_opt()].
-type jsx_opt() :: multi_term
    | loose_unicode
    | escape_forward_slashes
    | {encoding, auto 
        | utf8
        | utf16
        | {utf16, little}
        | utf32
        | {utf32, little}
    }.
    

-type jsx_event() :: start_object
    | end_object
    | start_array
    | end_array
    | end_json
    | {key, list()}
    | {string, list()}
    | {integer, integer()}
    | {float, float()}
    | {literal, true}
    | {literal, false}
    | {literal, null}.


-type jsx_encodeable() :: jsx_event() | [jsx_encodeable()].
    

-type jsx_iterator() :: jsx_decoder() | jsx_encoder().

    
%% this probably doesn't work properly
-type jsx_decoder() :: fun((binary()) -> jsx_iterator_result()).



-type jsx_encoder() :: fun((jsx_encodeable()) -> jsx_iterator_result()).

-type jsx_iterator_result() :: 
    {jsx, jsx_event(), fun(() -> jsx_iterator_result())}
    | {jsx, incomplete, jsx_iterator()}
    | {error, {badjson, any()}}.



-type supported_utf() :: utf8 
    | utf16 
    | {utf16, little} 
    | utf32 
    | {utf32, little}.



%% json specification
-type jsx_array() :: [jsx_term()] | [].
-type jsx_object() :: [{jsx_key(), jsx_term()}] | [{}].

-type jsx_key() :: binary().

-type jsx_term() :: jsx_array() 
    | jsx_object() 
    | jsx_string() 
    | jsx_number() 
    | true | false | null.

-type jsx_string() :: binary().

-type jsx_number() :: float() | integer().


-type encoder_opts() :: [encoder_opt()].
-type encoder_opt() :: {strict, true | false}
    | {encoding, supported_utf()}
    | {space, integer()}
    | space
    | {indent, integer()}
    | indent.


-type decoder_opts() :: [decoder_opt()].
-type decoder_opt() :: {strict, true | false}
    | {encoding, supported_utf()}.


-type verify_opts() :: [verify_opt()].
-type verify_opt() :: {encoding, auto | supported_utf()}
    | {repeated_keys, true | false}
    | {naked_values, true | false}.


-type format_opts() :: [format_opt()].
-type format_opt() :: {encoding, auto | supported_utf()}
    | {space, integer()}
    | space
    | {indent, integer()}
    | indent
    | {output_encoding, supported_utf()}.