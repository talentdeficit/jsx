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



-define(is_utf_encoding(X),
    X == utf8
        ; X == utf16
        ; X == utf32
        ; X == {utf16, little}
        ; X == {utf32, little}
).


-type jsx_opts() :: [jsx_opt()].
-type jsx_opt() :: {comments, true | false}
    | {escaped_unicode, ascii | codepoint | none}
    | {multi_term, true | false}
    | {encoding, auto 
    | utf8 
    | utf16 
    | {utf16, little} 
    | utf32 
    | {utf32, little} 
}.


%% events emitted by the parser and component types
-type unicode_codepoint() :: 0..16#10ffff.
-type unicode_string() :: [unicode_codepoint()].

-type jsx_event() :: start_object
    | end_object
    | start_array
    | end_array
    | end_json
    | {key, unicode_string()}
    | {string, unicode_string()}
    | {integer, unicode_string()}
    | {float, unicode_string()}
    | {literal, true}
    | {literal, false}
    | {literal, null}.
    
    
%% this probably doesn't work properly
-type jsx_parser() :: fun((binary()) -> jsx_parser_result()).

-type jsx_parser_result() :: 
    {event, jsx_event(), fun(() -> jsx_parser_result())}
    | {incomplete, jsx_parser()}
    | {error, badjson}.


-type supported_utf() :: utf8 
    | utf16 
    | {utf16, little} 
    | utf32 
    | {utf32, little}.


%% eep0018 json specification
-type eep0018() :: eep0018_object() | eep0018_array().

-type eep0018_array() :: [eep0018_term()].
-type eep0018_object() :: [{eep0018_key(), eep0018_term()}].

-type eep0018_key() :: binary() | atom().

-type eep0018_term() :: eep0018_array() 
    | eep0018_object() 
    | eep0018_string() 
    | eep0018_number() 
    | true | false | null.

-type eep0018_string() :: binary().

-type eep0018_number() :: float() | integer().


-type encoder_opts() :: [encoder_opt()].
-type encoder_opt() :: {strict, true | false}
    | {encoding, supported_utf()}
    | {space, integer()}
    | space
    | {indent, integer()}
    | indent.


-type decoder_opts() :: [decoder_opt()].
-type decoder_opt() :: {strict, true | false}
    | {comments, true | false}
    | {encoding, supported_utf()}
    | {label, atom | binary | existing_atom}
    | {float, true | false}.


-type verify_opts() :: [verify_opt()].
-type verify_opt() :: {strict, true | false}
    | {encoding, auto | supported_utf()}
    | {comments, true | false}.


-type format_opts() :: [format_opt()].
-type format_opt() :: {strict, true | false}
    | {encoding, auto | supported_utf()}
    | {comments, true | false}
    | {space, integer()}
    | space
    | {indent, integer()}
    | indent
    | {output_encoding, supported_utf()}.