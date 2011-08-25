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


-export([decoder/1]).


%% exported solely to facilitate stupid trick i shouldn't be using
-export([start/4,
    maybe_done/4,
    done/4,
    object/4,
    array/4,
    value/4,
    colon/4,
    key/4,
    string/5,
    escape/5,
    escaped_unicode/6,
    low_surrogate/6,
    low_surrogate_u/6,
    low_surrogate/7,
    negative/5,
    zero/5,
    integer/5,
    initial_decimal/5,
    decimal/5,
    e/5,
    ex/5,
    exp/5,
    tr/4,
    tru/4,
    true/4,
    fa/4,
    fal/4,
    fals/4,
    false/4,
    nu/4,
    nul/4,
    null/4
]).



-spec decoder(Opts::#opts{}) -> jsx_decoder().

decoder(Opts) ->
    fun(JSON) -> start(JSON, [], [], Opts) end.


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
    (Symbol >= ?space)
).

-define(is_whitespace(Symbol),
    Symbol =:= ?space; Symbol =:= ?tab; Symbol =:= ?cr; Symbol =:= ?newline
).


%% utf8 is the default encoding
-define(utf8, true).

-ifdef(utf16).
-undef(utf8).
-define(encoding, utf16).
-define(utfx, utf16).
-define(partial_codepoint(Bin), byte_size(Bin) < 2).
-endif.

-ifdef(utf16le).
-undef(utf8).
-define(encoding, utf16le).
-define(utfx, utf16-little).
-define(partial_codepoint(Bin), byte_size(Bin) < 2).
-endif.
    
-ifdef(utf32).
-undef(utf8).
-define(encoding, utf32).
-define(utfx, utf32).
-define(partial_codepoint(Bin), byte_size(Bin) < 4).
-endif.

-ifdef(utf32le).
-undef(utf8).
-define(encoding, utf32le).
-define(utfx, utf32-little).
-define(partial_codepoint(Bin), byte_size(Bin) < 4).
-endif.

-ifdef(utf8).
-define(encoding, utf8).
-define(utfx, utf8).
-define(partial_codepoint(Bin), byte_size(Bin) < 1).
-endif.

%% when parsing strings, the naive detection of partial codepoints is
%%   insufficient. this incredibly anal function should detect all badly formed
%%   utf sequences
-ifdef(utf8).
partial_utf(<<>>) -> true;
partial_utf(<<X>>) when X >= 16#c2, X =< 16#df -> true;
partial_utf(<<X, Rest/binary>>) when X >= 16#e0, X =< 16#ef ->
    case Rest of
        <<>> -> true
        ; <<Y>> when Y >= 16#80, Y =< 16#bf -> true
        ; _ -> false
    end;
partial_utf(<<X, Rest/binary>>) when X >= 16#f0, X =< 16#f4 ->
    case Rest of
        <<>> -> true
        ; <<Y>> when Y >= 16#80, Y =< 16#bf -> true
        ; <<Y, Z>> when Y >= 16#80, Y =< 16#bf, Z >= 16#80, Z =< 16#bf -> true
        ; _ -> false
    end;
partial_utf(_) -> false.    
-endif.    

-ifdef(utf16).
partial_utf(<<>>) -> true;
partial_utf(<<_X>>) -> true;
partial_utf(<<X, _Y>>) when X >= 16#d8, X =< 16#df -> true;
partial_utf(<<X, _Y, Z>>) when X >= 16#d8, X =< 16#df, Z >= 16#dc, Z =< 16#df ->
    true;
partial_utf(_) -> false.
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
partial_utf(<<>>) -> true;
partial_utf(<<_>>) -> true;
partial_utf(<<_, _>>) -> true;
partial_utf(<<_, _, _>>) -> true;
partial_utf(_) -> false.
-endif.

-ifdef(utf32le).
partial_utf(<<>>) -> true;
partial_utf(<<_>>) -> true;
partial_utf(<<_, _>>) -> true;
partial_utf(<<_, _, _>>) -> true;
partial_utf(_) -> false.
-endif.


incomplete(State, Bin, T, Args) ->
    case ?partial_codepoint(Bin) of
        true ->
            {jsx, incomplete, fun(Stream)
                when is_binary(Stream) ->
                    erlang:apply(?MODULE,
                        State,
                        [<<Bin/binary, Stream/binary>>, T] ++ Args
                    )
                ; (Else) -> {error, {badjson, Else}}
            end}
        ; false -> {error, {badjson, Bin}}
    end.


-ifndef(emit).

%% takes a list of `events` to present to client code and formats them
%%    appropriately
iterate_wrapper([], Next) -> Next();
iterate_wrapper([Event|Events], Next) ->
    {jsx, Event, fun() ->
        iterate_wrapper(Events, Next)
    end}.


-define(emit(Event, State, Rest, T, Stack, Opts),
    State(Rest, Event ++ T, Stack, Opts)
).


done(<<S/?utfx, Rest/binary>>, T, [], Opts) when ?is_whitespace(S) ->
    done(Rest, T, [], Opts);
done(<<>>, T, [], Opts=#opts{iterate = true}) ->
    iterate_wrapper(lists:reverse([end_json] ++ T), fun() ->
        incomplete(done, <<>>, [], [[], Opts])
    end);
done(<<>>, T, [], Opts) ->
    {jsx, lists:reverse([end_json] ++ T), fun(end_stream) ->
            done(<<>>, T, [], Opts)
        ; (Stream) ->
            done(Stream, T, [], Opts)
    end};
done(Bin, T, [], Opts) ->
    incomplete(done, Bin, T, [[], Opts]).


-else.

-define(next(State, Rest, T, Stack, Opts),
    State(Rest, T, Stack, Opts)
).

done(<<S/?utfx, Rest/binary>>, T, [], Opts) when ?is_whitespace(S) ->
    done(Rest, T, Opts);
done(<<>>, T, [], Opts) ->
    ?emit([end_json], done, Rest, T, [], Opts);
done(Bin, T, [], Opts) ->
    incomplete(done, Bin, T, [[], Opts]).


-endif.

start(<<?start_object/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([start_object], object, Rest, T, [key|Stack], Opts);
start(<<?start_array/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([start_array], array, Rest, T, [array|Stack], Opts);
start(<<?quote/?utfx, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
start(<<$t/?utfx, Rest/binary>>, T, Stack, Opts) ->
    tr(Rest, T, Stack, Opts);
start(<<$f/?utfx, Rest/binary>>, T, Stack, Opts) ->
    fa(Rest, T, Stack, Opts);
start(<<$n/?utfx, Rest/binary>>, T, Stack, Opts) ->
    nu(Rest, T, Stack, Opts);
start(<<?negative/?utfx, Rest/binary>>, T, Stack, Opts) ->
    negative(Rest, T, Stack, Opts, "-");
start(<<?zero/?utfx, Rest/binary>>, T, Stack, Opts) ->
    zero(Rest, T, Stack, Opts, "0");
start(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S]);
start(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) -> 
    start(Rest, T, Stack, Opts);
start(Bin, T, Stack, Opts) ->
    incomplete(start, Bin, T, [Stack, Opts]).


maybe_done(<<?end_object/?utfx, Rest/binary>>, T, [object|Stack], Opts) ->
    ?emit([end_object], maybe_done, Rest, T, Stack, Opts);
maybe_done(<<?end_array/?utfx, Rest/binary>>, T, [array|Stack], Opts) ->
    ?emit([end_array], maybe_done, Rest, T, Stack, Opts);
maybe_done(<<?comma/?utfx, Rest/binary>>, T, [object|Stack], Opts) ->
    key(Rest, T, [key|Stack], Opts);
maybe_done(<<?comma/?utfx, Rest/binary>>, T, [array|_] = Stack, Opts) ->
    value(Rest, T, Stack, Opts);
maybe_done(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, T, Stack, Opts);
maybe_done(Rest, T, [], Opts) ->
    done(Rest, T, [], Opts);
maybe_done(Bin, T, Stack, Opts) -> 
    incomplete(maybe_done, Bin, T, [Stack, Opts]).


object(<<?quote/?utfx, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
object(<<?end_object/?utfx, Rest/binary>>, T, [key|Stack], Opts) ->
    ?emit([end_object], maybe_done, Rest, T, Stack, Opts);
object(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) ->
    object(Rest, T, Stack, Opts);
object(Bin, T, Stack, Opts) ->
    incomplete(object, Bin, T, [Stack, Opts]).

   
array(<<?quote/?utfx, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
array(<<$t/?utfx, Rest/binary>>, T, Stack, Opts) ->
    tr(Rest, T, Stack, Opts);
array(<<$f/?utfx, Rest/binary>>, T, Stack, Opts) ->
    fa(Rest, T, Stack, Opts);
array(<<$n/?utfx, Rest/binary>>, T, Stack, Opts) ->
    nu(Rest, T, Stack, Opts);
array(<<?negative/?utfx, Rest/binary>>, T, Stack, Opts) ->
    negative(Rest, T, Stack, Opts, "-");
array(<<?zero/?utfx, Rest/binary>>, T, Stack, Opts) ->
    zero(Rest, T, Stack, Opts, "0");
array(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S]);
array(<<?start_object/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([start_object], object, Rest, T, [key|Stack], Opts);
array(<<?start_array/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([start_array], array, Rest, T, [array|Stack], Opts);
array(<<?end_array/?utfx, Rest/binary>>, T, [array|Stack], Opts) ->
    maybe_done(Rest, [end_array] ++ T, Stack, Opts);
array(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) -> 
    array(Rest, T, Stack, Opts);    
array(Bin, T, Stack, Opts) ->
    incomplete(array, Bin, T, [Stack, Opts]).


value(<<?quote/?utfx, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
value(<<$t/?utfx, Rest/binary>>, T, Stack, Opts) ->
    tr(Rest, T, Stack, Opts);
value(<<$f/?utfx, Rest/binary>>, T, Stack, Opts) ->
    fa(Rest, T, Stack, Opts);
value(<<$n/?utfx, Rest/binary>>, T, Stack, Opts) ->
    nu(Rest, T, Stack, Opts);
value(<<?negative/?utfx, Rest/binary>>, T, Stack, Opts) ->
    negative(Rest, T, Stack, Opts, "-");
value(<<?zero/?utfx, Rest/binary>>, T, Stack, Opts) ->
    zero(Rest, T, Stack, Opts, "0");
value(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S]);
value(<<?start_object/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([start_object], object, Rest, T, [key|Stack], Opts);
value(<<?start_array/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([start_array], array, Rest, T, [array|Stack], Opts);
value(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) -> 
    value(Rest, T, Stack, Opts);
value(Bin, T, Stack, Opts) ->
    incomplete(value, Bin, T, [Stack, Opts]).


colon(<<?colon/?utfx, Rest/binary>>, T, [key|Stack], Opts) ->
    value(Rest, T, [object|Stack], Opts);
colon(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) ->
    colon(Rest, T, Stack, Opts);
colon(Bin, T, Stack, Opts) ->
    incomplete(colon, Bin, T, [Stack, Opts]).


key(<<?quote/?utfx, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
key(<<S/?utfx, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) ->
    key(Rest, T, Stack, Opts);        
key(Bin, T, Stack, Opts) ->
    incomplete(key, Bin, T, [Stack, Opts]).


%% string has an additional parameter, an accumulator (Acc) used to hold the 
%%   intermediate representation of the string being parsed. using a list of 
%%   integers representing unicode codepoints is faster than constructing 
%%   binaries, there's a branch kicking around which proves it
%% string uses partial_utf/1 to cease parsing when invalid encodings are 
%%   encountered rather than just checking remaining binary size like other 
%%   states to eliminate certain incomplete states
string(<<?quote/?utfx, Rest/binary>>, T, [key|_] = Stack, Opts, Acc) ->
    ?emit([{key, lists:reverse(Acc)}], colon, Rest, T, Stack, Opts);
string(<<?quote/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    ?emit([{string, lists:reverse(Acc)}], maybe_done, Rest, T, Stack, Opts);
string(<<?rsolidus/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    escape(Rest, T, Stack, Opts, Acc);
%% things get dumb here. erlang doesn't properly restrict unicode non-characters
%%   so you can't trust the codepoints it returns always
%% the range 32..16#fdcf is safe, so allow that
string(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc)
        when ?is_noncontrol(S), S < 16#fdd0 ->
    string(Rest, T, Stack, Opts, [S] ++ Acc);
%% the range 16#fdf0..16#fffd is also safe
string(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc)
        when S > 16#fdef, S < 16#fffe ->
    string(Rest, T, Stack, Opts, [S] ++ Acc);
%% yes, i think it's insane too
string(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc)
        when S > 16#ffff andalso
            S =/= 16#1fffe andalso S =/= 16#1ffff andalso
            S =/= 16#2fffe andalso S =/= 16#2ffff andalso
            S =/= 16#3fffe andalso S =/= 16#3ffff andalso
            S =/= 16#4fffe andalso S =/= 16#4ffff andalso
            S =/= 16#5fffe andalso S =/= 16#5ffff andalso
            S =/= 16#6fffe andalso S =/= 16#6ffff andalso
            S =/= 16#7fffe andalso S =/= 16#7ffff andalso
            S =/= 16#8fffe andalso S =/= 16#8ffff andalso
            S =/= 16#9fffe andalso S =/= 16#9ffff andalso
            S =/= 16#afffe andalso S =/= 16#affff andalso
            S =/= 16#bfffe andalso S =/= 16#bffff andalso
            S =/= 16#cfffe andalso S =/= 16#cffff andalso
            S =/= 16#dfffe andalso S =/= 16#dffff andalso
            S =/= 16#efffe andalso S =/= 16#effff andalso
            S =/= 16#ffffe andalso S =/= 16#fffff andalso
            S =/= 16#10fffe andalso S =/= 16#10ffff ->
    string(Rest, T, Stack, Opts, [S] ++ Acc);
string(Bin, T, Stack, Opts, Acc) ->
    case partial_utf(Bin) of 
        true ->
            {jsx, incomplete, fun(Stream)
                when is_binary(Stream) ->
                    string(<<Bin/binary, Stream/binary>>, T, Stack, Opts, Acc)
                ; (Else) ->
                    {error, {badjson, Else}}
            end}
        ; false ->
            case Opts#opts.loose_unicode of
                true -> noncharacter(Bin, T, Stack, Opts, Acc)
                ; false -> {error, {badjson, Bin}}
            end
    end.

    
%% we don't need to guard against partial utf here, because it's already taken
%%   care of in string. theoretically, the last clause of noncharacter/4 is
%%   unreachable
-ifdef(utf8).
%% non-characters erlang doesn't recognize as non-characters, idiotically
noncharacter(<<S/utf8, Rest/binary>>, T, Stack, Opts, Acc)
        when ?is_noncontrol(S) ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% u+fffe and u+ffff
noncharacter(<<239, 191, X, Rest/binary>>, T, Stack, Opts, Acc) 
        when X == 190; X == 191 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% surrogates
noncharacter(<<237, X, _, Rest/binary>>, T, Stack, Opts, Acc) when X >= 160 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
noncharacter(Bin, _T, _Stack, _Opts, _Acc) ->
    {error, {badjson, Bin}}.
-endif.

-ifdef(utf16).
%% non-characters blah blah
noncharacter(<<S/utf16, Rest/binary>>, T, Stack, Opts, Acc)
        when ?is_noncontrol(S) ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% u+ffff and u+fffe
noncharacter(<<255, X, Rest/binary>>, T, Stack, Opts, Acc)
        when X == 254; X == 255 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% surrogates
noncharacter(<<X, _, Rest/binary>>, T, Stack, Opts, Acc)
        when X >= 216, X =< 223 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
noncharacter(Bin, _T, _Stack, _Opts, _Acc) ->
    {error, {badjson, Bin}}.
-endif.

-ifdef(utf16le).
%% non-characters blah blah
noncharacter(<<S/utf16-little, Rest/binary>>, T, Stack, Opts, Acc)
        when ?is_noncontrol(S) ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% u+ffff and u+fffe
noncharacter(<<X, 255, Rest/binary>>, T, Stack, Opts, Acc)
        when X == 254; X == 255 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% surrogates
noncharacter(<<_, X, Rest/binary>>, T, Stack, Opts, Acc)
        when X >= 216, X =< 223 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
noncharacter(Bin, _T, _Stack, _Opts, _Acc) ->
    {error, {badjson, Bin}}.
-endif.

-ifdef(utf32).
%% non-characters blah blah
noncharacter(<<S/utf32, Rest/binary>>, T, Stack, Opts, Acc)
        when ?is_noncontrol(S) ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% u+ffff and u+fffe
noncharacter(<<0, 0, 255, X, Rest/binary>>, T, Stack, Opts, Acc)
        when X == 254; X == 255 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% surrogates
noncharacter(<<0, 0, X, _, Rest/binary>>, T, Stack, Opts, Acc)
        when X >= 216, X =< 223 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
noncharacter(Bin, _T, _Stack, _Opts, _Acc) ->
    {error, {badjson, Bin}}.
-endif.

-ifdef(utf32le).
%% non-characters blah blah
noncharacter(<<S/utf32-little, Rest/binary>>, T, Stack, Opts, Acc)
        when ?is_noncontrol(S) ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% u+ffff and u+fffe
noncharacter(<<X, 255, 0, 0, Rest/binary>>, T, Stack, Opts, Acc)
        when X == 254; X == 255 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
%% surrogates
noncharacter(<<_, X, 0, 0, Rest/binary>>, T, Stack, Opts, Acc)
        when X >= 216, X =< 223 ->
    string(Rest, T, Stack, Opts, [16#fffd] ++ Acc);
noncharacter(Bin, _T, _Stack, _Opts, _Acc) ->
    {error, {badjson, Bin}}.
-endif.


%% only thing to note here is the additional accumulator passed to 
%%   escaped_unicode used to hold the codepoint sequence. unescessary, but nicer 
%%   than using the string accumulator
escape(<<$b/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\b" ++ Acc);
escape(<<$f/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\f" ++ Acc);
escape(<<$n/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\n" ++ Acc);
escape(<<$r/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\r" ++ Acc);
escape(<<$t/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\t" ++ Acc);
escape(<<$u/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    escaped_unicode(Rest, T, Stack, Opts, Acc, []);      
escape(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc) 
        when S =:= ?quote; S =:= ?solidus; S =:= ?rsolidus ->
    string(Rest, T, Stack, Opts, [S] ++ Acc);
escape(Bin, T, Stack, Opts, Acc) ->
    incomplete(escape, Bin, T, [Stack, Opts, Acc]).


%% this code is ugly and unfortunate, but so is json's handling of escaped 
%%   unicode codepoint sequences.
escaped_unicode(<<D/?utfx, Rest/binary>>, T, Stack, Opts, String, [C, B, A]) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        %% high surrogate, we need a low surrogate next
        X when X >= 16#d800, X =< 16#dbff ->
            low_surrogate(Rest, T, Stack, Opts, String, X)
        %% non-characters, you're not allowed to exchange these
        ; X when X == 16#fffe; X == 16#ffff; X >= 16#fdd0, X =< 16#fdef ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, T, Stack, Opts, [16#fffd] ++ String)
                ; false ->    
                    {error, {badjson, <<D/?utfx, Rest/binary>>}}
            end
        %% allowing interchange of null bytes allows attackers to forge
        %%   malicious streams
        ; X when X == 16#0000 ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, T, Stack, Opts, [16#fffd] ++ String)
                ; false ->    
                    {error, {badjson, <<D/?utfx, Rest/binary>>}}
            end
        %% anything else
        ; X ->
            string(Rest, T, Stack, Opts, [X] ++ String)
    end;
escaped_unicode(<<S/?utfx, Rest/binary>>, T, Stack, Opts, String, Acc) 
        when ?is_hex(S) ->
    escaped_unicode(Rest, T, Stack, Opts, String, [S] ++ Acc);
escaped_unicode(Bin, T, Stack, Opts, String, Acc) ->
    incomplete(escaped_unicode, Bin, T, [Stack, Opts, String, Acc]).


low_surrogate(<<?rsolidus/?utfx, Rest/binary>>, T, Stack, Opts, String, High) ->
    low_surrogate_u(Rest, T, Stack, Opts, String, High);
%% not an escaped codepoint, our high codepoint is illegal. dispatch back to
%%   string to handle
low_surrogate(<<S/?utfx, Rest/binary>> = Bin, T, Stack, Opts, String, _) ->
    case Opts#opts.loose_unicode of
        true ->
            string(Bin, T, Stack, Opts, [16#fffd] ++ String)
        ; false ->
            {error, {badjson, <<S/?utfx, Rest/binary>>}}
    end;
low_surrogate(Bin, T, Stack, Opts, String, High) ->
    incomplete(low_surrogate, Bin, T, [Stack, Opts, String, High]).


low_surrogate_u(<<$u/?utfx, Rest/binary>>, T, Stack, Opts, String, H) ->
    low_surrogate(Rest, T, Stack, Opts, String, [], H);
%% not a low surrogate, dispatch back to string to handle, including the
%%   rsolidus we parsed previously
low_surrogate_u(<<S/?utfx, Rest/binary>> = Bin, T, Stack, Opts, String, _) ->
    case Opts#opts.loose_unicode of
        true ->
            string(<<?rsolidus/?utfx, Bin/binary>>,
                T,
                Stack,
                Opts,
                [16#fffd] ++ String
            )
        ; false ->
            {error, {badjson, <<S/?utfx, Rest/binary>>}}
    end;
low_surrogate_u(Bin, T, Stack, Opts, String, H) ->
    incomplete(low_surrogate_u, Bin, T, [Stack, Opts, String, H]).


low_surrogate(<<D/?utfx, Rest/binary>>, T, Stack, Opts, String, [C, B, A], H) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >= 16#dc00, X =< 16#dfff ->
            V = surrogate_to_codepoint(H, X),
            case V rem 16#10000 of Y when Y == 16#fffe; Y == 16#ffff ->
                    case Opts#opts.loose_unicode of
                        true ->
                            string(Rest, T, Stack, Opts, [16#fffd] ++ String)
                        ; false ->    
                            {error, {badjson, <<D/?utfx, Rest/binary>>}}
                    end
                ; _ ->
                    string(Rest, T, Stack, Opts, [V] ++ String)
            end
        %% not a low surrogate, bad bad bad
        ; _ ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, T, Stack, Opts, [16#fffd, 16#fffd] ++ String)
                ; false ->    
                    {error, {badjson, <<D/?utfx, Rest/binary>>}}
            end
    end;
low_surrogate(<<S/?utfx, Rest/binary>>, T, Stack, Opts, String, Acc, H) 
        when ?is_hex(S) ->
    low_surrogate(Rest, T, Stack, Opts, String, [S] ++ Acc, H);
low_surrogate(Bin, T, Stack, Opts, String, Acc, H) ->
    incomplete(low_surrogate, Bin, T, [Stack, Opts, String, Acc, H]).


%% stole this from the unicode spec    
surrogate_to_codepoint(High, Low) ->
    (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000.


%% like strings, numbers are collected in an intermediate accumulator before
%%   being emitted to the callback handler
negative(<<$0/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    zero(Rest, T, Stack, Opts, "0" ++ Acc);
negative(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S] ++ Acc);
negative(Bin, T, Stack, Opts, Acc) ->  
    incomplete(negative, Bin, T, [Stack, Opts, Acc]).


zero(<<?end_object/?utfx, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?emit([end_object, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
zero(<<?end_array/?utfx, Rest/binary>>, T, [array|Stack], Opts, Acc) ->
    ?emit([end_array, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
zero(<<?comma/?utfx, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?emit([format_number(Acc)], key, Rest, T, [key|Stack], Opts);
zero(<<?comma/?utfx, Rest/binary>>, T, [array|_] = Stack, Opts, Acc) ->
    ?emit([format_number(Acc)], value, Rest, T, Stack, Opts);
zero(<<?decimalpoint/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    initial_decimal(Rest, T, Stack, Opts, {Acc, []});
zero(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc) when ?is_whitespace(S) ->
    ?emit([format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
zero(<<>>, T, [], Opts, Acc) ->
    {jsx, incomplete, fun(end_stream) ->
            ?emit([format_number(Acc)], done, <<>>, T, [], Opts)
        ; (Stream) when is_binary(Stream) ->
            zero(Stream, T, [], Opts, Acc)
        ; (Else) -> {error, {badjson, Else}}
    end};
zero(Bin, T, Stack, Opts, Acc) ->
    incomplete(zero, Bin, T, [Stack, Opts, Acc]).


integer(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S] ++ Acc);
integer(<<?end_object/?utfx, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?emit([end_object, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
integer(<<?end_array/?utfx, Rest/binary>>, T, [array|Stack], Opts, Acc) ->
    ?emit([end_array, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
integer(<<?comma/?utfx, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?emit([format_number(Acc)], key, Rest, T, [key|Stack], Opts);
integer(<<?comma/?utfx, Rest/binary>>, T, [array|_] = Stack, Opts, Acc) ->
    ?emit([format_number(Acc)], value, Rest, T, Stack, Opts);
integer(<<?decimalpoint/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    initial_decimal(Rest, T, Stack, Opts, {Acc, []});
integer(<<?zero/?utfx, Rest/binary>>, T, Stack, Opts, Acc) ->
    integer(Rest, T, Stack, Opts, [?zero] ++ Acc);
integer(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc) when S =:= $e; S =:= $E ->
    e(Rest, T, Stack, Opts, {Acc, [], []});
integer(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc) when ?is_whitespace(S) ->
    ?emit([format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
integer(<<>>, T, [], Opts, Acc) ->
    {jsx, incomplete, fun(end_stream) ->
            ?emit([format_number(Acc)], done, <<>>, T, [], Opts)
        ; (Stream) when is_binary(Stream) ->
            integer(Stream, T, [], Opts, Acc)
        ; (Else) -> {error, {badjson, Else}}
    end};
integer(Bin, T, Stack, Opts, Acc) ->  
    incomplete(integer, Bin, T, [Stack, Opts, Acc]).


initial_decimal(<<S/?utfx, Rest/binary>>, T, Stack, Opts, {Int, Frac})
        when S =:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, T, Stack, Opts, {Int, [S] ++ Frac});
initial_decimal(Bin, T, Stack, Opts, Acc) ->
    incomplete(initial_decimal, Bin, T, [Stack, Opts, Acc]).


decimal(<<S/?utfx, Rest/binary>>, T, Stack, Opts, {Int, Frac})
        when S=:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, T, Stack, Opts, {Int, [S] ++ Frac});
decimal(<<?end_object/?utfx, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?emit([end_object, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
decimal(<<?end_array/?utfx, Rest/binary>>, T, [array|Stack], Opts, Acc) ->
    ?emit([end_array, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
decimal(<<?comma/?utfx, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?emit([format_number(Acc)], key, Rest, T, [key|Stack], Opts);
decimal(<<?comma/?utfx, Rest/binary>>, T, [array|_] = Stack, Opts, Acc) ->
    ?emit([format_number(Acc)], value, Rest, T, Stack, Opts);
decimal(<<S/?utfx, Rest/binary>>, T, Stack, Opts, {Int, Frac})
        when S =:= $e; S =:= $E ->
    e(Rest, T, Stack, Opts, {Int, Frac, []});
decimal(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc) when ?is_whitespace(S) ->
    ?emit([format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
decimal(<<>>, T, [], Opts, Acc) ->
    {jsx, incomplete, fun(end_stream) ->
            ?emit([format_number(Acc)], done, <<>>, T, [], Opts)
        ; (Stream) when is_binary(Stream) ->
            decimal(Stream, T, [], Opts, Acc)
        ; (Else) -> {error, {badjson, Else}}
    end};
decimal(Bin, T, Stack, Opts, Acc) ->  
    incomplete(decimal, Bin, T, [Stack, Opts, Acc]).


e(<<S/?utfx, Rest/binary>>, T, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, T, Stack, Opts, {Int, Frac, [S] ++ Exp});   
e(<<S/?utfx, Rest/binary>>, T, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?positive; S =:= ?negative ->
    ex(Rest, T, Stack, Opts, {Int, Frac, [S] ++ Exp});
e(Bin, T, Stack, Opts, Acc) ->  
    incomplete(e, Bin, T, [Stack, Opts, Acc]).


ex(<<S/?utfx, Rest/binary>>, T, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, T, Stack, Opts, {Int, Frac, [S] ++ Exp});
ex(Bin, T, Stack, Opts, Acc) ->  
    incomplete(ex, Bin, T, [Stack, Opts, Acc]).


exp(<<S/?utfx, Rest/binary>>, T, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, T, Stack, Opts, {Int, Frac, [S] ++ Exp});
exp(<<?end_object/?utfx, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?emit([end_object, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
exp(<<?end_array/?utfx, Rest/binary>>, T, [array|Stack], Opts, Acc) ->
    ?emit([end_array, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
exp(<<?comma/?utfx, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?emit([format_number(Acc)], key, Rest, T, [key|Stack], Opts);
exp(<<?comma/?utfx, Rest/binary>>, T, [array|_] = Stack, Opts, Acc) ->
    ?emit([format_number(Acc)], value, Rest, T, Stack, Opts);
exp(<<S/?utfx, Rest/binary>>, T, Stack, Opts, Acc) when ?is_whitespace(S) ->
    ?emit([format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
exp(<<>>, T, [], Opts, Acc) ->
    {jsx, incomplete, fun(end_stream) ->
            ?emit([format_number(Acc)], done, <<>>, T, [], Opts)
        ; (Stream) when is_binary(Stream) ->
            exp(Stream, T, [], Opts, Acc)
        ; (Else) -> {error, {badjson, Else}}
    end};
exp(Bin, T, Stack, Opts, Acc) ->  
    incomplete(exp, Bin, T, [Stack, Opts, Acc]).


format_number(Int) when is_list(Int) ->
    {integer, list_to_integer(lists:reverse(Int))};
format_number({Int, Frac}) ->
    {float, list_to_float(lists:reverse(Frac ++ "." ++ Int))};
format_number({Int, [], Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e0." ++ Int))};
format_number({Int, Frac, Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e" ++ Frac ++ "." ++ Int))}.


tr(<<$r/?utfx, Rest/binary>>, T, Stack, Opts) ->
    tru(Rest, T, Stack, Opts);
tr(Bin, T, Stack, Opts) ->  
    incomplete(tr, Bin, T, [Stack, Opts]).


tru(<<$u/?utfx, Rest/binary>>, T, Stack, Opts) ->
    true(Rest, T, Stack, Opts);
tru(Bin, T, Stack, Opts) ->  
    incomplete(tru, Bin, T, [Stack, Opts]).


true(<<$e/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([{literal, true}], maybe_done, Rest, T, Stack, Opts);
true(Bin, T, Stack, Opts) ->
    incomplete(true, Bin, T, [Stack, Opts]).


fa(<<$a/?utfx, Rest/binary>>, T, Stack, Opts) ->
    fal(Rest, T, Stack, Opts);
fa(Bin, T, Stack, Opts) ->
    incomplete(fa, Bin, T, [Stack, Opts]).
    

fal(<<$l/?utfx, Rest/binary>>, T, Stack, Opts) ->
    fals(Rest, T, Stack, Opts);
fal(Bin, T, Stack, Opts) ->
    incomplete(fal, Bin, T, [Stack, Opts]).
    

fals(<<$s/?utfx, Rest/binary>>, T, Stack, Opts) ->
    false(Rest, T, Stack, Opts);
fals(Bin, T, Stack, Opts) ->
    incomplete(fals, Bin, T, [Stack, Opts]).
    

false(<<$e/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([{literal, false}], maybe_done, Rest, T, Stack, Opts);
false(Bin, T, Stack, Opts) ->
    incomplete(false, Bin, T, [Stack, Opts]).


nu(<<$u/?utfx, Rest/binary>>, T, Stack, Opts) ->
    nul(Rest, T, Stack, Opts);
nu(Bin, T, Stack, Opts) ->
    incomplete(nu, Bin, T, [Stack, Opts]).


nul(<<$l/?utfx, Rest/binary>>, T, Stack, Opts) ->
    null(Rest, T, Stack, Opts);
nul(Bin, T, Stack, Opts) ->
    incomplete(nul, Bin, T, [Stack, Opts]).


null(<<$l/?utfx, Rest/binary>>, T, Stack, Opts) ->
    ?emit([{literal, null}], maybe_done, Rest, T, Stack, Opts);
null(Bin, T, Stack, Opts) ->
    incomplete(null, Bin, T, [Stack, Opts]).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


noncharacters_test_() ->
    [
        {"noncharacters - badjson",
            ?_assertEqual(check_bad(noncharacters()), [])
        },
        {"noncharacters - replaced",
            ?_assertEqual(check_replaced(noncharacters()), [])
        }
    ].

extended_noncharacters_test_() ->
    [
        {"extended noncharacters - badjson",
            ?_assertEqual(check_bad(extended_noncharacters()), [])
        },
        {"extended noncharacters - replaced",
            ?_assertEqual(check_replaced(extended_noncharacters()), [])
        }
    ].

surrogates_test_() ->
    [
        {"surrogates - badjson",
            ?_assertEqual(check_bad(surrogates()), [])
        },
        {"surrogates - replaced",
            ?_assertEqual(check_replaced(surrogates()), [])
        }
    ].

control_test_() ->
    [
        {"control characters - badjson",
            ?_assertEqual(check_bad(control_characters()), [])
        }
    ].

reserved_test_() ->
    [
        {"reserved noncharacters - badjson",
            ?_assertEqual(check_bad(reserved_space()), [])
        },
        {"reserved noncharacters - replaced",
            ?_assertEqual(check_replaced(reserved_space()), [])
        }
    ].

zero_test_() ->
    [
        {"nullbyte - badjson",
            ?_assertEqual(check_bad(zero()), [])
        }
    ].
    
good_characters_test_() ->
    [
        {"acceptable codepoints",
            ?_assertEqual(check_good(good()), [])
        },
        {"acceptable extended",
            ?_assertEqual(check_good(good_extended()), [])
        }
    ].
    

check_bad(List) ->
    lists:dropwhile(fun({_, {error, badjson}}) -> true ; (_) -> false end,
        check(List, [], [])
    ).

check_replaced(List) ->
    lists:dropwhile(fun({_, [{string, [16#fffd]}|_]}) ->
                true
            ; (_) -> 
                false 
        end,
        check(List, [loose_unicode], [])
    ).

check_good(List) ->
    lists:dropwhile(fun({_, [{string, _}]}) -> true ; (_) -> false end,
        check(List, [], [])
    ).

check([], _Opts, Acc) -> Acc;
check([H|T], Opts, Acc) ->
    R = decode(to_fake_utf(H, ?encoding), Opts),
    check(T, Opts, [{H, R}] ++ Acc).


decode(JSON, Opts) ->
    case (jsx:decoder(Opts))(JSON) of
        {jsx, Events, _} -> loop(Events, [])
        ; {error, {badjson, _}} -> {error, badjson}
    end.


loop([end_json], Acc) -> lists:reverse(Acc);
loop([Event|Events], Acc) -> loop(Events, [Event] ++ Acc);
loop(_, _) -> {error, badjson}.
    


noncharacters() -> lists:seq(16#fffe, 16#ffff).
    
extended_noncharacters() ->
    [16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff]
        ++ [16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff]
        ++ [16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff]
        ++ [16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff]
        ++ [16#9fffe, 16#9ffff, 16#afffe, 16#affff]
        ++ [16#bfffe, 16#bffff, 16#cfffe, 16#cffff]
        ++ [16#dfffe, 16#dffff, 16#efffe, 16#effff]
        ++ [16#ffffe, 16#fffff, 16#10fffe, 16#10ffff].

surrogates() -> lists:seq(16#d800, 16#dfff).

control_characters() -> lists:seq(1, 31).

reserved_space() -> lists:seq(16#fdd0, 16#fdef).

zero() -> [0].

good() -> [32, 33]
            ++ lists:seq(16#23, 16#5b)
            ++ lists:seq(16#5d, 16#d7ff)
            ++ lists:seq(16#e000, 16#fdcf)
            ++ lists:seq(16#fdf0, 16#fffd).
            
good_extended() -> lists:seq(16#100000, 16#10fffd).

%% erlang refuses to encode certain codepoints, so fake them all
to_fake_utf(N, utf8) when N < 16#0080 -> <<34/utf8, N:8, 34/utf8>>;
to_fake_utf(N, utf8) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<34/utf8, 2#110:3, Y:5, 2#10:2, X:6, 34/utf8>>; 
to_fake_utf(N, utf8) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<34/utf8, 2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6, 34/utf8>>;
to_fake_utf(N, utf8) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<34/utf8, 2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6, 34/utf8>>;

to_fake_utf(N, utf16) when N < 16#10000 -> <<34/utf16, N:16, 34/utf16>>;
to_fake_utf(N, utf16) -> <<34/utf16, N/utf16, 34/utf16>>;

to_fake_utf(N, utf16le) when N < 16#10000 ->
    <<A:8, B:8>> = <<N:16>>,
    <<34, 0, B:8, A:8, 34, 0>>;
to_fake_utf(N, utf16le) -> <<34/utf16-little, N/utf16-little, 34/utf16-little>>;

to_fake_utf(N, utf32) -> <<34/utf32, N:32, 34/utf32>>;

to_fake_utf(N, utf32le) ->
    <<A:8, B:8, C:8, D:8>> = <<N:32>>,
    <<34/utf32-little, D:8, C:8, B:8, A:8, 34/utf32-little>>.


-endif.