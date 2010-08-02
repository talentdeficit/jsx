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


%% this is a template for the utf8, utf16, utf16le, utf32 and utf32le decoders. it should
%%   not be compiled directly, see the build script in /priv for details

-module(?name).
-author("alisdairsullivan@yahoo.ca").

-export([parse/2]).

-include("./include/jsx_decoder.hrl").
-include("./include/jsx_types.hrl").


-spec parse(JSON::json(), Opts::jsx_opts()) -> jsx_parser_result().

parse(JSON, Opts) ->
    start(JSON, [], Opts).


start(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) -> 
    start(Rest, Stack, Opts);
start(<<?start_object/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, start_object, fun() -> object(Rest, [key|Stack], Opts) end};
start(<<?start_array/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, start_array, fun() -> array(Rest, [array|Stack], Opts) end};
start(<<?quote/?encoding, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
start(<<$t/?encoding, Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
start(<<$f/?encoding, Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
start(<<$n/?encoding, Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
start(<<?negative/?encoding, Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
start(<<?zero/?encoding, Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
start(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S]);
start(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts)) ->
    maybe_comment(Rest, fun(Resume) -> start(Resume, Stack, Opts) end);
start(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> start(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.

maybe_done(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, Stack, Opts);
maybe_done(<<?end_object/?encoding, Rest/binary>>, [object|Stack], Opts) ->
    {event, end_object, fun() -> maybe_done(Rest, Stack, Opts) end};
maybe_done(<<?end_array/?encoding, Rest/binary>>, [array|Stack], Opts) ->
    {event, end_array, fun() -> maybe_done(Rest, Stack, Opts) end};
maybe_done(<<?comma/?encoding, Rest/binary>>, [object|Stack], Opts) ->
    key(Rest, [key|Stack], Opts);
maybe_done(<<?comma/?encoding, Rest/binary>>, [array|_] = Stack, Opts) ->
    value(Rest, Stack, Opts);
maybe_done(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts)) ->
    maybe_comment(Rest, fun(Resume) -> maybe_done(Resume, Stack, Opts) end);
maybe_done(Rest, [], ?multi_term(Opts)) ->
    {event, end_json, fun() -> start(Rest, [], Opts) end};
maybe_done(Rest, [], Opts) ->
    {event, end_json, fun() -> done(Rest, Opts) end};
maybe_done(Bin, Stack, Opts) -> 
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> case Bin == <<>> of true -> ok; false -> {error, badjson} end
                    ; (Stream) -> maybe_done(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.
    
done(<<S/?encoding, Rest/binary>>, Opts) when ?is_whitespace(S) ->
    done(Rest, Opts);
done(<<?solidus/?encoding, Rest/binary>>, ?comments_enabled(Opts)) ->
    maybe_comment(Rest, fun(Resume) -> done(Resume, Opts) end);
done(Bin, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> case Bin == <<>> of true -> ok; false -> {error, badjson} end 
                    ; (Stream) -> done(<<Bin/binary, Stream/binary>>, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


object(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) ->
    object(Rest, Stack, Opts);
object(<<?quote/?encoding, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
object(<<?end_object/?encoding, Rest/binary>>, [key|Stack], Opts) ->
    {event, end_object, fun() -> maybe_done(Rest, Stack, Opts) end};
object(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts)) ->
    maybe_comment(Rest, fun(Resume) -> object(Resume, Stack, Opts) end);
object(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> object(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


array(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) -> 
    array(Rest, Stack, Opts);       
array(<<?quote/?encoding, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
array(<<$t/?encoding, Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
array(<<$f/?encoding, Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
array(<<$n/?encoding, Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
array(<<?negative/?encoding, Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
array(<<?zero/?encoding, Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
array(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S]);
array(<<?start_object/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, start_object, fun() -> object(Rest, [key|Stack], Opts) end};
array(<<?start_array/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, start_array, fun() -> array(Rest, [array|Stack], Opts) end};
array(<<?end_array/?encoding, Rest/binary>>, [array|Stack], Opts) ->
    {event, end_array, fun() -> maybe_done(Rest, Stack, Opts) end};
array(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts)) ->
    maybe_comment(Rest, fun(Resume) -> array(Resume, Stack, Opts) end);
array(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> array(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


value(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) -> 
    value(Rest, Stack, Opts);
value(<<?quote/?encoding, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
value(<<$t/?encoding, Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
value(<<$f/?encoding, Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
value(<<$n/?encoding, Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
value(<<?negative/?encoding, Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
value(<<?zero/?encoding, Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
value(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S]);
value(<<?start_object/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, start_object, fun() -> object(Rest, [key|Stack], Opts) end};
value(<<?start_array/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, start_array, fun() -> array(Rest, [array|Stack], Opts) end};
value(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts)) ->
    maybe_comment(Rest, fun(Resume) -> value(Resume, Stack, Opts) end);
value(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> value(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


colon(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) ->
    colon(Rest, Stack, Opts);
colon(<<?colon/?encoding, Rest/binary>>, [key|Stack], Opts) ->
    value(Rest, [object|Stack], Opts);
colon(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts)) ->
    maybe_comment(Rest, fun(Resume) -> colon(Resume, Stack, Opts) end);
colon(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> colon(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


key(<<S/?encoding, Rest/binary>>, Stack, Opts) when ?is_whitespace(S) ->
    key(Rest, Stack, Opts);        
key(<<?quote/?encoding, Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
key(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts)) ->
    maybe_comment(Rest, fun(Resume) -> key(Resume, Stack, Opts) end);
key(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> key(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


%% string has an additional parameter, an accumulator (Acc) used to hold the intermediate
%%   representation of the string being parsed. using a list of integers representing
%%   unicode codepoints is faster than constructing binaries, many of which will be
%%   converted back to lists by the user anyways
%% string uses partial_utf/1 to cease parsing when invalid encodings are encountered
%%   rather than just checking remaining binary size like other states
string(<<?quote/?encoding, Rest/binary>>, [key|_] = Stack, Opts, Acc) ->
    {event, {key, lists:reverse(Acc)}, fun() -> colon(Rest, Stack, Opts) end};
string(<<?quote/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    {event, {string, lists:reverse(Acc)}, fun() -> maybe_done(Rest, Stack, Opts) end};
string(<<?rsolidus/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    escape(Rest, Stack, Opts, Acc);   
string(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_noncontrol(S) ->
    string(Rest, Stack, Opts, [S] ++ Acc);
string(Bin, Stack, Opts, Acc) ->
    case partial_utf(Bin) of 
            true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> string(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
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
partial_utf(<<>>) -> true;
%% this case is not strictly true, there are single bytes that should be rejected, but
%%   they're rare enough they can be ignored
partial_utf(<<_X>>) -> true;
partial_utf(<<X, _Y>>) when X >= 16#d8, X =< 16#df -> true;
partial_utf(<<X, _Y, Z>>) when X >= 16#d8, X =< 16#df, Z >= 16#dc, Z =< 16#df -> true;
partial_utf(_) -> false.
-endif.

-ifdef(utf16le).    
partial_utf(<<>>) -> true;
%% this case is not strictly true, there are single bytes that should be rejected, but
%%   they're rare enough they can be ignored
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


%% only thing to note here is the additional accumulator passed to escaped_unicode used
%%   to hold the codepoint sequence. unescessary, but nicer than using the string 
%%   accumulator
escape(<<$b/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\b" ++ Acc);
escape(<<$f/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\f" ++ Acc);
escape(<<$n/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\n" ++ Acc);
escape(<<$r/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\r" ++ Acc);
escape(<<$t/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\t" ++ Acc);
escape(<<$u/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    escaped_unicode(Rest, Stack, Opts, Acc, []);      
escape(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) 
        when S =:= ?quote; S =:= ?solidus; S =:= ?rsolidus ->
    string(Rest, Stack, Opts, [S] ++ Acc);
escape(Bin, Stack, Opts, Acc) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> escape(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


%% this code is ugly and unfortunate, but so is json's handling of escaped unicode
%%   codepoint sequences. if the ascii option is present, the sequence is converted
%%   to a codepoint and inserted into the string if it represents an ascii value. if 
%%   the codepoint option is present the sequence is converted and inserted as long
%%   as it represents a valid unicode codepoint. this means non-characters 
%%   representable in 16 bits are not converted (the utf16 surrogates and the two
%%   special non-characters). any other option and no conversion is done
escaped_unicode(<<D/?encoding, Rest/binary>>, 
        Stack, 
        ?escaped_unicode_to_ascii(Opts), 
        String, 
        [C, B, A]) 
            when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X < 128 ->
            string(Rest, Stack, Opts, [X] ++ String)
        ; _ ->
            string(Rest, Stack, Opts, [D, C, B, A, $u, ?rsolidus] ++ String)
    end;
escaped_unicode(<<D/?encoding, Rest/binary>>, 
        Stack, 
        ?escaped_unicode_to_codepoint(Opts), 
        String, 
        [C, B, A]) 
            when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >= 16#dc00, X =< 16#dfff ->
            case check_acc_for_surrogate(String) of
                false ->
                    string(Rest, Stack, Opts, [D, C, B, A, $u, ?rsolidus] ++ String)
                ; {Y, NewString} ->
                    string(Rest, Stack, Opts, [surrogate_to_codepoint(Y, X)] ++ NewString)
            end
        ; X when X < 16#d800; X > 16#dfff, X < 16#fffe ->
            string(Rest, Stack, Opts, [X] ++ String) 
        ; _ ->
            string(Rest, Stack, Opts, [D, C, B, A, $u, ?rsolidus] ++ String)
    end;
escaped_unicode(<<D/?encoding, Rest/binary>>, Stack, Opts, String, [C, B, A]) when ?is_hex(D) ->
    string(Rest, Stack, Opts, [D, C, B, A, $u, ?rsolidus] ++ String);
escaped_unicode(<<S/?encoding, Rest/binary>>, Stack, Opts, String, Acc) when ?is_hex(S) ->
    escaped_unicode(Rest, Stack, Opts, String, [S] ++ Acc);
escaped_unicode(Bin, Stack, Opts, String, Acc) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> escaped_unicode(<<Bin/binary, Stream/binary>>, Stack, Opts, String, Acc)
                end
            }
        ; false -> {error, badjson}
    end.

%% upon encountering a low pair json/hex encoded value, check to see if there's a high
%%   value already in the accumulator
check_acc_for_surrogate([D, C, B, A, $u, ?rsolidus|Rest])
        when ?is_hex(D), ?is_hex(C), ?is_hex(B), ?is_hex(A) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >=16#d800, X =< 16#dbff ->
            {X, Rest};
        _ ->
            false
    end;
check_acc_for_surrogate(_) ->
    false.

%% stole this from the unicode spec    
surrogate_to_codepoint(High, Low) ->
    (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000.


%% like strings, numbers are collected in an intermediate accumulator before
%%   being emitted to the callback handler
negative(<<$0/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    zero(Rest, Stack, Opts, "0" ++ Acc);
negative(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S] ++ Acc);
negative(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> negative(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


zero(<<?end_object/?encoding, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> 
        {event, end_object, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
zero(<<?end_array/?encoding, Rest/binary>>, [array|Stack], Opts, Acc) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> 
        {event, end_array, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
zero(<<?comma/?encoding, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> key(Rest, [key|Stack], Opts) end};
zero(<<?comma/?encoding, Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> value(Rest, Stack, Opts) end};
zero(<<?decimalpoint/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    initial_decimal(Rest, Stack, Opts, [?decimalpoint] ++ Acc);
zero(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_whitespace(S) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> maybe_done(Rest, Stack, Opts) end};
zero(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts), Acc) ->
    maybe_comment(Rest, fun(Resume) -> zero(Resume, Stack, Opts, Acc) end);
zero(<<>>, [], Opts, Acc) ->
    {incomplete, 
        fun
            (end_stream) -> {event, {integer, lists:reverse(Acc)}, fun() -> maybe_done(<<>>, [], Opts) end}
            ; (Stream) -> zero(Stream, [], Opts, Acc)
        end
    };
zero(Bin, Stack, Opts, Acc) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> zero(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


integer(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_nonzero(S) ->
    integer(Rest, Stack, Opts, [S] ++ Acc);
integer(<<?end_object/?encoding, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> 
        {event, end_object, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
integer(<<?end_array/?encoding, Rest/binary>>, [array|Stack], Opts, Acc) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> 
        {event, end_array, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
integer(<<?comma/?encoding, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> key(Rest, [key|Stack], Opts) end};
integer(<<?comma/?encoding, Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> value(Rest, Stack, Opts) end};
integer(<<?decimalpoint/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    initial_decimal(Rest, Stack, Opts, [?decimalpoint] ++ Acc);
integer(<<?zero/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    integer(Rest, Stack, Opts, [?zero] ++ Acc);
integer(<<$e/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    e(Rest, Stack, Opts, "e0." ++ Acc);
integer(<<$E/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    e(Rest, Stack, Opts, "e0." ++ Acc);
integer(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_whitespace(S) ->
    {event, {integer, lists:reverse(Acc)}, fun() -> maybe_done(Rest, Stack, Opts) end};
integer(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts), Acc) ->
    maybe_comment(Rest, fun(Resume) -> integer(Resume, Stack, Opts, Acc) end);
integer(<<>>, [], Opts, Acc) ->
    {incomplete, 
        fun
            (end_stream) -> {event, {integer, lists:reverse(Acc)}, fun() -> maybe_done(<<>>, [], Opts) end}
            ; (Stream) -> integer(Stream, [], Opts, Acc)
        end
    };
integer(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> integer(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


initial_decimal(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_nonzero(S) ->
    decimal(Rest, Stack, Opts, [S] ++ Acc);
initial_decimal(<<?zero/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    decimal(Rest, Stack, Opts, [?zero] ++ Acc);
initial_decimal(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> initial_decimal(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


decimal(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_nonzero(S) ->
    decimal(Rest, Stack, Opts, [S] ++ Acc);
decimal(<<?end_object/?encoding, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event, {float, lists:reverse(Acc)}, fun() -> 
        {event, end_object, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
decimal(<<?end_array/?encoding, Rest/binary>>, [array|Stack], Opts, Acc) ->
    {event, {float, lists:reverse(Acc)}, fun() -> 
        {event, end_array, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
decimal(<<?comma/?encoding, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event, {float, lists:reverse(Acc)}, fun() -> key(Rest, [key|Stack], Opts) end};
decimal(<<?comma/?encoding, Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {event, {float, lists:reverse(Acc)}, fun() -> value(Rest, Stack, Opts) end};
decimal(<<?zero/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    decimal(Rest, Stack, Opts, [?zero] ++ Acc);
decimal(<<$e/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    e(Rest, Stack, Opts, "e" ++ Acc);
decimal(<<$E/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    e(Rest, Stack, Opts, "e" ++ Acc);
decimal(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_whitespace(S) ->
    {event, {float, lists:reverse(Acc)}, fun() -> maybe_done(Rest, Stack, Opts) end};
decimal(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts), Acc) ->
    maybe_comment(Rest, fun(Resume) -> decimal(Resume, Stack, Opts, Acc) end);
decimal(<<>>, [], Opts, Acc) ->
    {incomplete, 
        fun
            (end_stream) -> {event, {float, lists:reverse(Acc)}, fun() -> maybe_done(<<>>, [], Opts) end}
            ; (Stream) -> decimal(Stream, [], Opts, Acc)
        end
    };
decimal(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> decimal(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


e(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Stack, Opts, [S] ++ Acc);   
e(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when S =:= ?positive; S =:= ?negative ->
    ex(Rest, Stack, Opts, [S] ++ Acc);
e(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> e(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


ex(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Stack, Opts, [S] ++ Acc);
ex(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> ex(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


exp(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_nonzero(S) ->
    exp(Rest, Stack, Opts, [S] ++ Acc);
exp(<<?end_object/?encoding, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event, {float, lists:reverse(Acc)}, fun() -> 
        {event, end_object, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
exp(<<?end_array/?encoding, Rest/binary>>, [array|Stack], Opts, Acc) ->
    {event, {float, lists:reverse(Acc)}, fun() -> 
        {event, end_array, fun() -> maybe_done(Rest, Stack, Opts) end}
    end};
exp(<<?comma/?encoding, Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event, {float, lists:reverse(Acc)}, fun() -> key(Rest, [key|Stack], Opts) end};
exp(<<?comma/?encoding, Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {event, {float, lists:reverse(Acc)}, fun() -> value(Rest, Stack, Opts) end};
exp(<<?zero/?encoding, Rest/binary>>, Stack, Opts, Acc) ->
    exp(Rest, Stack, Opts, [?zero] ++ Acc);
exp(<<S/?encoding, Rest/binary>>, Stack, Opts, Acc) when ?is_whitespace(S) ->
    {event, {float, lists:reverse(Acc)}, fun() -> maybe_done(Rest, Stack, Opts) end};
exp(<<?solidus/?encoding, Rest/binary>>, Stack, ?comments_enabled(Opts), Acc) ->
    maybe_comment(Rest, fun(Resume) -> exp(Resume, Stack, Opts, Acc) end);
exp(<<>>, [], Opts, Acc) ->
    {incomplete, 
        fun
            (end_stream) -> {event, {float, lists:reverse(Acc)}, fun() -> maybe_done(<<>>, [], Opts) end}
            ; (Stream) -> exp(Stream, [], Opts, Acc)
        end
    };
exp(Bin, Stack, Opts, Acc) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> exp(<<Bin/binary, Stream/binary>>, Stack, Opts, Acc)
                end
            }
        ; false -> {error, badjson}
    end.


tr(<<$r/?encoding, Rest/binary>>, Stack, Opts) ->
    tru(Rest, Stack, Opts);
tr(Bin, Stack, Opts) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> tr(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


tru(<<$u/?encoding, Rest/binary>>, Stack, Opts) ->
    true(Rest, Stack, Opts);
tru(Bin, Stack, Opts) ->  
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> tru(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


true(<<$e/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, {literal, true}, fun() -> maybe_done(Rest, Stack, Opts) end};
true(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> true(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


fa(<<$a/?encoding, Rest/binary>>, Stack, Opts) ->
    fal(Rest, Stack, Opts);
fa(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> fa(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.
    

fal(<<$l/?encoding, Rest/binary>>, Stack, Opts) ->
    fals(Rest, Stack, Opts);
fal(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> fal(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.
    

fals(<<$s/?encoding, Rest/binary>>, Stack, Opts) ->
    false(Rest, Stack, Opts);
fals(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> fals(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.
    

false(<<$e/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, {literal, false}, fun() -> maybe_done(Rest, Stack, Opts) end};
false(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> false(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


nu(<<$u/?encoding, Rest/binary>>, Stack, Opts) ->
    nul(Rest, Stack, Opts);
nu(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> nu(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


nul(<<$l/?encoding, Rest/binary>>, Stack, Opts) ->
    null(Rest, Stack, Opts);
nul(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> nul(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


null(<<$l/?encoding, Rest/binary>>, Stack, Opts) ->
    {event, {literal, null}, fun() -> maybe_done(Rest, Stack, Opts) end};
null(Bin, Stack, Opts) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> null(<<Bin/binary, Stream/binary>>, Stack, Opts)
                end
            }
        ; false -> {error, badjson}
    end.


%% comments are c style, ex: /* blah blah */ 
%%   any unicode character is valid in a comment except the */ sequence which ends
%%   the comment. they're implemented as a closure called when the comment ends that
%%   returns execution to the point where the comment began. comments are not 
%%   reported in any way, simply parsed.
maybe_comment(<<?star/?encoding, Rest/binary>>, Resume) ->
    comment(Rest, Resume);
maybe_comment(Bin, Resume) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> maybe_comment(<<Bin/binary, Stream/binary>>, Resume)
                end
            }
        ; false -> {error, badjson}
    end.


comment(<<?star/?encoding, Rest/binary>>, Resume) ->
    maybe_comment_done(Rest, Resume);
comment(<<_/?encoding, Rest/binary>>, Resume) ->
    comment(Rest, Resume);
comment(Bin, Resume) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> comment(<<Bin/binary, Stream/binary>>, Resume)
                end
            }
        ; false -> {error, badjson}
    end.


maybe_comment_done(<<?solidus/?encoding, Rest/binary>>, Resume) ->
    Resume(Rest);
maybe_comment_done(<<_/?encoding, Rest/binary>>, Resume) ->
    comment(Rest, Resume);
maybe_comment_done(Bin, Resume) ->
    case ?partial_codepoint(Bin) of
        true -> 
            {incomplete, 
                fun(end_stream) -> {error, badjson} 
                    ; (Stream) -> maybe_comment_done(<<Bin/binary, Stream/binary>>, Resume)
                end
            }
        ; false -> {error, badjson}
    end.