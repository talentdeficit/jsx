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

-include("./include/jsx_common.hrl").



-spec parse(JSON::eep0018(), Opts::jsx_opts()) -> jsx_parser_result().


%% option flags

-define(comments_enabled(X), {_, true, _, _, _} = X).
-define(escaped_unicode_to_ascii(X), {_, _, ascii, _, _} = X).
-define(escaped_unicode_to_codepoint(X), {_, _, codepoint, _, _} = X).
-define(multi_term(X), {_, _, _, true, _} = X).

%% whitespace
-define(space, 16#20).
-define(tab, 16#09).
-define(cr, 16#0D).
-define(newline, 16#0A).

%% object delimiters
-define(start_object, 16#7B).
-define(end_object, 16#7D).

%% array delimiters
-define(start_array, 16#5B).
-define(end_array, 16#5D).

%% kv seperator
-define(comma, 16#2C).
-define(quote, 16#22).
-define(colon, 16#3A).

%% string escape sequences
-define(escape, 16#5C).
-define(rsolidus, 16#5C).
-define(solidus, 16#2F).
-define(formfeed, 16#0C).
-define(backspace, 16#08).
-define(unicode, 16#75).

%% math
-define(zero, 16#30).
-define(decimalpoint, 16#2E).
-define(negative, 16#2D).
-define(positive, 16#2B).

%% comments
-define(star, 16#2a).


%% some useful guards
-define(is_hex(Symbol),
    (Symbol >= $a andalso Symbol =< $z); (Symbol >= $A andalso Symbol =< $Z); 
        (Symbol >= $0 andalso Symbol =< $9)
).

-define(is_nonzero(Symbol),
    Symbol >= $1 andalso Symbol =< $9
).

-define(is_noncontrol(Symbol),
    Symbol >= ?space
).

-define(is_whitespace(Symbol),
    Symbol =:= ?space; Symbol =:= ?tab; Symbol =:= ?cr; Symbol =:= ?newline
).



%% compilation macros for unified decoder
-ifdef(utf8).
-define(encoding, utf8).
-define(partial_codepoint(Bin), byte_size(Bin) < 1).
-endif.

-ifdef(utf16).
-define(encoding, utf16).
-define(partial_codepoint(Bin), byte_size(Bin) < 2).
-endif.

-ifdef(utf16le).
-define(encoding, utf16-little).
-define(partial_codepoint(Bin), byte_size(Bin) < 2).
-endif.
    
-ifdef(utf32).
-define(encoding, utf32).
-define(partial_codepoint(Bin), byte_size(Bin) < 4).
-endif.

-ifdef(utf32le).
-define(encoding, utf32-little).
-define(partial_codepoint(Bin), byte_size(Bin) < 4).
-endif.