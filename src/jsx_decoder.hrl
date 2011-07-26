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


%% this is the implementation of the utf backends for the jsx decoder. it's 
%%   included by the various jsx_utfxx.erl frontends and all modifications to 
%%   this file should take that into account


-spec decoder(OptsList::jsx_opts()) -> jsx_decoder().

%% opts record for decoder
-record(opts, {
    multi_term = false,
    encoding = auto
}).


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


%% partial codepoint max size differs across encodings
-ifdef(utf8).
-define(utfx, utf8).
-define(partial_codepoint(Bin), byte_size(Bin) < 1).
-endif.

-ifdef(utf16).
-define(utfx, utf16).
-define(partial_codepoint(Bin), byte_size(Bin) < 2).
-endif.

-ifdef(utf16le).
-define(utfx, utf16-little).
-define(partial_codepoint(Bin), byte_size(Bin) < 2).
-endif.
    
-ifdef(utf32).
-define(utfx, utf32).
-define(partial_codepoint(Bin), byte_size(Bin) < 4).
-endif.

-ifdef(utf32le).
-define(utfx, utf32-little).
-define(partial_codepoint(Bin), byte_size(Bin) < 4).
-endif.


-export([decoder/1]).



decoder(OptsList) ->
    case parse_opts(OptsList) of 
        {error, badopt} -> {error, badopt}
        ; Opts -> fun(JSON) -> start(JSON, [], Opts) end
    end.
    

%% converts a proplist into a tuple
parse_opts(Opts) ->
    parse_opts(Opts, #opts{}).

parse_opts([], Opts) ->
    Opts;
parse_opts([{multi_term, Value}|Rest], Opts) ->
    true = lists:member(Value, [true, false]),
    parse_opts(Rest, Opts#opts{multi_term=Value});
parse_opts([multi_term|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{multi_term=true});
parse_opts([{encoding, _}|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts(_, _) ->
    {error, badarg}.



start(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) -> 
    start(Rest, Stack, Opts);
start(<<?start_object/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, start_object, fun() -> object(Rest, [key|Stack], Opts) end};
start(<<?start_array/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, start_array, fun() -> array(Rest, [array|Stack], Opts) end};
start(<<?quote/?utfx, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts);
start(<<$t/?utfx, Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
start(<<$f/?utfx, Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
start(<<$n/?utfx, Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
start(<<?negative/?utfx, Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
start(<<?zero/?utfx, Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
start(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S]);
start(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    start(<<Bin/binary, Stream/binary>>, Stack, Opts) 
            end}
        ; false -> {error, {badjson, Bin}}
    end.


maybe_done(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, Stack, Opts);
maybe_done(<<?end_object/?utfx, Rest/binary>>, [object|Stack], Opts) ->
    {jsx, end_object, fun() -> maybe_done(Rest, Stack, Opts) end};
maybe_done(<<?end_array/?utfx, Rest/binary>>, [array|Stack], Opts) ->
    {jsx, end_array, fun() -> maybe_done(Rest, Stack, Opts) end};
maybe_done(<<?comma/?utfx, Rest/binary>>, [object|Stack], Opts) ->
    key(Rest, [key|Stack], Opts);
maybe_done(<<?comma/?utfx, Rest/binary>>, [array|_] = Stack, Opts) ->
    value(Rest, Stack, Opts);
maybe_done(Rest, [], #opts{multi_term=true}=Opts) ->
    {jsx, end_json, fun() -> start(Rest, [], Opts) end};
maybe_done(Rest, [], Opts) ->
    done(Rest, Opts);
maybe_done(Bin, Stack, Opts) -> 
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    maybe_done(<<Bin/binary, Stream/binary>>, Stack, Opts) 
            end}
        ; false -> {error, {badjson, Bin}}
    end.


done(<<S/?utfx, Rest/binary>>, Opts) when ?is_whitespace(S) ->
    done(Rest, Opts);
done(<<>>, Opts) ->
    {jsx, end_json, fun() -> 
        {jsx, incomplete, fun(end_stream) -> 
                {error, {badjson, <<>>}}
            ; (Stream) -> 
                done(Stream, Opts) 
        end} 
    end};
done(Bin, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    done(<<Bin/binary, Stream/binary>>, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


object(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) ->
    object(Rest, Stack, Opts);
object(<<?quote/?utfx, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts);
object(<<?end_object/?utfx, Rest/binary>>, [key|Stack], Opts) ->
    {jsx, end_object, fun() -> maybe_done(Rest, Stack, Opts) end};
object(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    object(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


array(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) -> 
    array(Rest, Stack, Opts);       
array(<<?quote/?utfx, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts);
array(<<$t/?utfx, Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
array(<<$f/?utfx, Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
array(<<$n/?utfx, Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
array(<<?negative/?utfx, Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
array(<<?zero/?utfx, Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
array(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S]);
array(<<?start_object/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, start_object, fun() -> object(Rest, [key|Stack], Opts) end};
array(<<?start_array/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, start_array, fun() -> array(Rest, [array|Stack], Opts) end};
array(<<?end_array/?utfx, Rest/binary>>, [array|Stack], Opts) ->
    {jsx, end_array, fun() -> maybe_done(Rest, Stack, Opts) end};
array(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    array(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


value(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) -> 
    value(Rest, Stack, Opts);
value(<<?quote/?utfx, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts);
value(<<$t/?utfx, Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
value(<<$f/?utfx, Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
value(<<$n/?utfx, Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
value(<<?negative/?utfx, Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
value(<<?zero/?utfx, Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
value(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S]);
value(<<?start_object/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, start_object, fun() -> object(Rest, [key|Stack], Opts) end};
value(<<?start_array/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, start_array, fun() -> array(Rest, [array|Stack], Opts) end};
value(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    value(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


colon(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) ->
    colon(Rest, Stack, Opts);
colon(<<?colon/?utfx, Rest/binary>>, [key|Stack], Opts) ->
    value(Rest, [object|Stack], Opts);
colon(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    colon(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


key(<<S/?utfx, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) ->
    key(Rest, Stack, Opts);        
key(<<?quote/?utfx, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts);
key(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    key(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


%% string has an additional parameter, an accumulator (Acc) used to hold the 
%%   intermediate representation of the string being parsed. using a list of 
%%   integers representing unicode codepoints is faster than constructing 
%%   binaries, many of which will be converted back to lists by the user anyways
%% string uses partial_utf/1 to cease parsing when invalid encodings are 
%%   encountered rather than just checking remaining binary size like other 
%%   states
string(Bin, Stack, Opts) -> string(Bin, Stack, Opts, <<>>).


string(<<?quote/?utfx, Rest/binary>>, [key|_] = Stack, Opts, Acc) ->
    {jsx, {key, Acc}, fun() -> colon(Rest, Stack, Opts) end};
string(<<?quote/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    {jsx, {string, Acc}, fun() -> 
        maybe_done(Rest, Stack, Opts)
    end};
string(<<?rsolidus/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    escape(Rest, Stack, Opts, Acc);   
string(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) when ?is_noncontrol(S) ->
    string(Rest, Stack, Opts, <<Acc/binary, S/utf8>>);
string(Bin, Stack, Opts, Acc) ->
    case partial_utf(Bin) of 
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    string(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.

    
-ifdef(utf8).
partial_utf(<<>>) -> true;
partial_utf(<<X>>) when X >= 16#c2, X =< 16#df -> true;
partial_utf(<<X, Rest/binary>>) when X >= 16#e0, X =< 16#ef ->
    case Rest of
        <<>> -> true
        ; <<Y>> when Y >= 16#80, Y =< 16#bf -> true
    end;
partial_utf(<<X, Rest/binary>>) when X >= 16#f0, X =< 16#f4 ->
    case Rest of
        <<>> -> true
        ; <<Y>> when Y >= 16#80, Y =< 16#bf -> true
        ; <<Y, Z>> when Y >= 16#80, Y =< 16#bf, Z >= 16#80, Z =< 16#bf -> true
    end;
partial_utf(_) -> false.    
-endif.    

-ifdef(utf16).
partial_utf(<<>>) -> 
    true;
%% this case is not strictly true, there are single bytes that should be 
%%   rejected, but they're rare enough they can be ignored
partial_utf(<<_X>>) ->
    true;
partial_utf(<<X, _Y>>) when X >= 16#d8, X =< 16#df ->
    true;
partial_utf(<<X, _Y, Z>>) when X >= 16#d8, X =< 16#df, Z >= 16#dc, Z =< 16#df ->
    true;
partial_utf(_) ->
    false.
-endif.

-ifdef(utf16le).    
partial_utf(<<>>) -> true;
%% this case is not strictly true, there are single bytes that should be 
%%   rejected, but they're rare enough they can be ignored
partial_utf(<<_X>>) -> true;
partial_utf(<<_Y, X>>) when X >= 16#d8, X =< 16#df -> true;
partial_utf(<<_Y, X, _Z>>) when X >= 16#d8, X =< 16#df -> true;
partial_utf(_) -> false.
-endif.

-ifdef(utf32).
partial_utf(<<_:32>>) -> false;
partial_utf(_) -> true.
-endif.

-ifdef(utf32le).
partial_utf(<<_:32>>) -> false;
partial_utf(_) -> true.
-endif.


%% only thing to note here is the additional accumulator passed to 
%%   escaped_unicode used to hold the codepoint sequence. unescessary, but nicer 
%%   than using the string accumulator
escape(<<$b/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, <<Acc/binary, "\b">>);
escape(<<$f/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, <<Acc/binary, "\f">>);
escape(<<$n/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, <<Acc/binary, "\n">>);
escape(<<$r/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, <<Acc/binary, "\r">>);
escape(<<$t/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, <<Acc/binary, "\t">>);
escape(<<$u/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    escaped_unicode(Rest, Stack, Opts, Acc, []);      
escape(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) 
        when S =:= ?quote; S =:= ?solidus; S =:= ?rsolidus ->
    string(Rest, Stack, Opts, <<Acc/binary, S/utf8>>);
escape(Bin, Stack, Opts, Acc) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    escape(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


%% this code is ugly and unfortunate, but so is json's handling of escaped 
%%   unicode codepoint sequences.
%% fuck json escaping. new rule: if it's not a valid codepoint, it's an error
escaped_unicode(<<D/?utfx, Rest/binary>>, Stack, Opts, String, [C, B, A]) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        %% high surrogate, we need a low surrogate next
        X when X >= 16#d800, X =< 16#dbff ->
            low_surrogate(Rest, Stack, Opts, String, X)
        %% non-characters, you're not allowed to exchange these
        ; X when X == 16#fffe; X == 16#ffff ->
            {error, {badjson, <<D/?utfx, Rest/binary>>}}
        %% allowing interchange of null bytes allows attackers to forge
        %%   malicious streams
        ; X when X == 16#0000 ->
            {error, {badjson, <<D/?utfx, Rest/binary>>}}
        %% anything else
        ; X ->
            string(Rest, Stack, Opts, <<String/binary, X/utf8>>)
    end;
escaped_unicode(<<S/?utfx, Rest/binary>>, Stack, Opts, String, Acc) 
        when ?is_hex(S) ->
    escaped_unicode(Rest, Stack, Opts, String, [S] ++ Acc);
escaped_unicode(Bin, Stack, Opts, String, Acc) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    escaped_unicode(<<Bin/binary, Stream/binary>>, 
                        Stack, 
                        Opts, 
                        String, 
                        Acc
                    ) 
            end}
        ; false -> {error, {badjson, Bin}}
    end.


low_surrogate(<<?rsolidus/?utfx, Rest/binary>>, Stack, Opts, String, High) ->
    low_surrogate_u(Rest, Stack, Opts, String, High);
low_surrogate(Bin, Stack, Opts, String, High) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    low_surrogate(<<Bin/binary, Stream/binary>>, 
                        Stack, 
                        Opts, 
                        String,
                        High
                    ) 
            end}
        ; false -> {error, {badjson, Bin}}
    end.

    
    
low_surrogate_u(<<$u/?utfx, Rest/binary>>, Stack, Opts, String, High) ->
    low_surrogate(Rest, Stack, Opts, String, [], High);
low_surrogate_u(Bin, Stack, Opts, String, High) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    low_surrogate_u(<<Bin/binary, Stream/binary>>, 
                        Stack, 
                        Opts, 
                        String,
                        High
                    ) 
            end}
        ; false -> {error, {badjson, Bin}}
    end.



low_surrogate(<<D/?utfx, Rest/binary>>, Stack, Opts, String, [C, B, A], High) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >= 16#dc00, X =< 16#dfff ->
            string(Rest,
                Stack,
                Opts,
                <<String/binary, (surrogate_to_codepoint(High, X))/utf8>>
            )
        %% not a low surrogate, bad bad bad
        ; X ->
            {error, {badjson, <<X/?utfx, Rest/binary>>}}
    end;
low_surrogate(<<S/?utfx, Rest/binary>>, Stack, Opts, String, Acc, High) 
        when ?is_hex(S) ->
    low_surrogate(Rest, Stack, Opts, String, [S] ++ Acc, High);
low_surrogate(Bin, Stack, Opts, String, Acc, High) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    low_surrogate(<<Bin/binary, Stream/binary>>, 
                        Stack, 
                        Opts, 
                        String, 
                        Acc,
                        High
                    ) 
            end}
        ; false -> {error, {badjson, Bin}}
    end.


%% stole this from the unicode spec    
surrogate_to_codepoint(High, Low) ->
    io:format("~p ~p~n", [High, Low]),
    (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000.


%% like strings, numbers are collected in an intermediate accumulator before
%%   being emitted to the callback handler
negative(<<$0/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    zero(Rest, Stack, Opts, "0" ++ Acc);
negative(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S] ++ Acc);
negative(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    negative(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


zero(<<?end_object/?utfx, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        {jsx, end_object, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
zero(<<?end_array/?utfx, Rest/binary>>, [array|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        {jsx, end_array, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
zero(<<?comma/?utfx, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        key(Rest, [key|Stack], Opts)
    end};
zero(<<?comma/?utfx, Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        value(Rest, Stack, Opts)
    end};
zero(<<?decimalpoint/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    initial_decimal(Rest, Stack, Opts, {Acc, []});
zero(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) when ?is_whitespace(S) ->
    {jsx, format_number(Acc), fun() -> 
        maybe_done(Rest, Stack, Opts)
    end};
zero(<<>>, [], Opts, Acc) ->
    {jsx, incomplete, fun(end_stream) ->
            {jsx, format_number(Acc), fun() ->
                {jsx, end_json, fun() -> zero(<<>>, [], Opts, Acc) end}
            end}
        ; (Stream) -> zero(Stream, [], Opts, Acc)
    end};
zero(Bin, Stack, Opts, Acc) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    zero(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


integer(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S] ++ Acc);
integer(<<?end_object/?utfx, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        {jsx, end_object, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
integer(<<?end_array/?utfx, Rest/binary>>, [array|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        {jsx, end_array, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
integer(<<?comma/?utfx, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() ->
        key(Rest, [key|Stack], Opts)
    end};
integer(<<?comma/?utfx, Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {jsx, format_number(Acc), fun() ->
        value(Rest, Stack, Opts)
    end};
integer(<<?decimalpoint/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    initial_decimal(Rest, Stack, Opts, {Acc, []});
integer(<<?zero/?utfx, Rest/binary>>, Stack, Opts, Acc) ->
    integer(Rest, Stack, Opts, [?zero] ++ Acc);
integer(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) when S =:= $e; S =:= $E ->
    e(Rest, Stack, Opts, {lists:reverse(Acc), [], []});
integer(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) when ?is_whitespace(S) ->
    {jsx, format_number(Acc), fun() ->
        maybe_done(Rest, Stack, Opts)
    end};
integer(<<>>, [], Opts, Acc) ->
    {jsx, incomplete, fun(end_stream) ->
            {jsx, format_number(Acc), fun() ->
                {jsx, end_json, fun() -> integer(<<>>, [], Opts, Acc) end}
            end}
        ; (Stream) -> integer(Stream, [], Opts, Acc)
    end};
integer(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true ->
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    integer(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


initial_decimal(<<S/?utfx, Rest/binary>>, Stack, Opts, {Int, Frac})
        when S =:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Stack, Opts, {Int, [S] ++ Frac});
initial_decimal(Bin, Stack, Opts, Acc) ->
    case ?partial_codepoint(Bin) of
        true ->
            {jsx, incomplete, fun(end_stream) ->
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    initial_decimal(<<Bin/binary, Stream/binary>>,
                        Stack,
                        Opts,
                        Acc
                    ) 
            end}
        ; false -> {error, {badjson, Bin}}
    end.


decimal(<<S/?utfx, Rest/binary>>, Stack, Opts, {Int, Frac})
        when S=:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Stack, Opts, {Int, [S] ++ Frac});
decimal(<<?end_object/?utfx, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        {jsx, end_object, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
decimal(<<?end_array/?utfx, Rest/binary>>, [array|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        {jsx, end_array, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
decimal(<<?comma/?utfx, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() ->
        key(Rest, [key|Stack], Opts)
    end};
decimal(<<?comma/?utfx, Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {jsx, format_number(Acc), fun() ->
        value(Rest, Stack, Opts)
    end};
decimal(<<S/?utfx, Rest/binary>>, Stack, Opts, {Int, Frac})
        when S =:= $e; S =:= $E ->
    e(Rest, Stack, Opts, {Int, Frac, []});
decimal(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) when ?is_whitespace(S) ->
    {jsx, format_number(Acc), fun() ->
        maybe_done(Rest, Stack, Opts)
    end};
decimal(<<>>, [], Opts, Acc) ->
    {jsx, incomplete, fun(end_stream) ->
            {jsx, format_number(Acc), fun() ->
                {jsx, end_json, fun() -> decimal(<<>>, [], Opts, Acc) end}
            end}
        ; (Stream) -> decimal(Stream, [], Opts, Acc)
    end};
decimal(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    decimal(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


e(<<S/?utfx, Rest/binary>>, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Stack, Opts, {Int, Frac, [S] ++ Exp});   
e(<<S/?utfx, Rest/binary>>, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?positive; S =:= ?negative ->
    ex(Rest, Stack, Opts, {Int, Frac, [S] ++ Exp});
e(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) ->
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    e(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


ex(<<S/?utfx, Rest/binary>>, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Stack, Opts, {Int, Frac, [S] ++ Exp});
ex(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) ->
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    ex(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


exp(<<S/?utfx, Rest/binary>>, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Stack, Opts, {Int, Frac, [S] ++ Exp});
exp(<<?end_object/?utfx, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        {jsx, end_object, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
exp(<<?end_array/?utfx, Rest/binary>>, [array|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() -> 
        {jsx, end_array, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
exp(<<?comma/?utfx, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {jsx, format_number(Acc), fun() ->
        key(Rest, [key|Stack], Opts)
    end};
exp(<<?comma/?utfx, Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {jsx, format_number(Acc), fun() ->
        value(Rest, Stack, Opts)
    end};
exp(<<S/?utfx, Rest/binary>>, Stack, Opts, Acc) when ?is_whitespace(S) ->
    {jsx, format_number(Acc), fun() ->
        maybe_done(Rest, Stack, Opts)
    end};
exp(<<>>, [], Opts, Acc) ->
    {jsx, incomplete, fun(end_stream) ->
            {jsx, format_number(Acc), fun() ->
                {jsx, end_json, fun() -> exp(<<>>, [], Opts, Acc) end}
            end}
        ; (Stream) -> exp(Stream, [], Opts, Acc)
    end};
exp(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true ->
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) -> 
                    exp(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


format_number(Int) when is_list(Int) ->
    {integer, list_to_integer(lists:reverse(Int))};
format_number({Int, Frac}) ->
    {float, list_to_float(lists:reverse(Frac ++ "." ++ Int))};
format_number({Int, [], Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e0." ++ Int))};
format_number({Int, Frac, Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e" ++ Frac ++ "." ++ Int))}.
            
         


tr(<<$r/?utfx, Rest/binary>>, Stack, Opts) ->
    tru(Rest, Stack, Opts);
tr(Bin, Stack, Opts) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    tr(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


tru(<<$u/?utfx, Rest/binary>>, Stack, Opts) ->
    true(Rest, Stack, Opts);
tru(Bin, Stack, Opts) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    tru(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


true(<<$e/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, {literal, true}, fun() -> maybe_done(Rest, Stack, Opts) end};
true(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    true(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


fa(<<$a/?utfx, Rest/binary>>, Stack, Opts) ->
    fal(Rest, Stack, Opts);
fa(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    fa(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.
    

fal(<<$l/?utfx, Rest/binary>>, Stack, Opts) ->
    fals(Rest, Stack, Opts);
fal(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    fal(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}        
        ; false -> {error, {badjson, Bin}}
    end.
    

fals(<<$s/?utfx, Rest/binary>>, Stack, Opts) ->
    false(Rest, Stack, Opts);
fals(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    fals(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.
    

false(<<$e/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, {literal, false}, fun() -> maybe_done(Rest, Stack, Opts) end};
false(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    false(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


nu(<<$u/?utfx, Rest/binary>>, Stack, Opts) ->
    nul(Rest, Stack, Opts);
nu(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    nu(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


nul(<<$l/?utfx, Rest/binary>>, Stack, Opts) ->
    null(Rest, Stack, Opts);
nul(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    nul(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.


null(<<$l/?utfx, Rest/binary>>, Stack, Opts) ->
    {jsx, {literal, null}, fun() -> maybe_done(Rest, Stack, Opts) end};
null(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {jsx, incomplete, fun(end_stream) -> 
                    {error, {badjson, Bin}}
                ; (Stream) ->
                    null(<<Bin/binary, Stream/binary>>, Stack, Opts)
            end}
        ; false -> {error, {badjson, Bin}}
    end.