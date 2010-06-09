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


%% unsure of how to specify a binary with a complex structure like utfx encoded
%%   binaries. this should be further limited somehow probably.

-type json() :: binary().


-type jsx_opts() :: [jsx_opt()].
-type jsx_opt() :: {comments, true | false}
    | {escaped_unicode, ascii | codepoint | none}
    | {stream_mode, true | false}
    | {encoding, auto | utf8 | utf16 | utf16le | utf32 | utf32le }.


%% events emitted by the parser and component types

-type unicode_codepoint() :: 0..16#10ffff.
-type unicode_string() :: [unicode_codepoint()].

-type jsx_event() :: start_object
    | end_object
    | start_array
    | end_array
    | end_of_json
    | reset
    | {key, unicode_string()}
    | {string, unicode_string()}
    | {integer, unicode_string()}
    | {float, unicode_string()}
    | {literal, true}
    | {literal, false}
    | {literal, null}.
    
    
%% this probably doesn't work properly

-type jsx_parser() :: fun((json()) -> {[jsx_event(),...], jsx_parser()} 
    | {incomplete, jsx_parser()}
    | {error, badjson}
).