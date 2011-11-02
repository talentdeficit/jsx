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


-module(jsx_decoder).

-export([decoder/1]).


-spec decoder(Opts::jsx:opts()) -> jsx:decoder().

decoder(Opts) ->
    fun(JSON) -> start(JSON, [], [], jsx_utils:parse_opts(Opts)) end.


-include("../include/jsx_opts.hrl").


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


%% error, incomplete and event macros
-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


-ifndef(incomplete).
-define(incomplete(State, Rest, Out, Stack, Opts),
    {incomplete, fun(Stream) when is_binary(Stream) ->
                State(<<Rest/binary, Stream/binary>>, Out, Stack, Opts)
            ; (end_stream) ->
                case State(<<Rest/binary, <<" ">>/binary>>,
                        Out,
                        Stack,
                        Opts#opts{explicit_end=false}) of
                    {incomplete, _} -> ?error([Rest, Out, Stack, Opts])
                    ; {ok, Events} -> {ok, Events}
                end
        end
    }
).
-endif.


-ifndef(event).
-define(event(Event, State, Rest, Out, Stack, Opts),
    State(Rest, Event ++ Out, Stack, Opts)
).
-endif.

-define(new_seq(), []).
-define(new_seq(C), [C]).

-define(acc_seq(Seq, C), [C] ++ Seq).

-define(end_seq(Seq), unicode:characters_to_binary(lists:reverse(Seq))).


start(<<?start_object, Rest/binary>>, Out, Stack, Opts) ->
    ?event([start_object], object, Rest, Out, [key|Stack], Opts);
start(<<?start_array, Rest/binary>>, Out, Stack, Opts) ->
    ?event([start_array], array, Rest, Out, [array|Stack], Opts);
start(<<?quote, Rest/binary>>, Out, Stack, Opts) ->
    string(Rest, Out, [?new_seq()|Stack], Opts);
start(<<$t, Rest/binary>>, Out, Stack, Opts) ->
    tr(Rest, Out, Stack, Opts);
start(<<$f, Rest/binary>>, Out, Stack, Opts) ->
    fa(Rest, Out, Stack, Opts);
start(<<$n, Rest/binary>>, Out, Stack, Opts) ->
    nu(Rest, Out, Stack, Opts);
start(<<?negative, Rest/binary>>, Out, Stack, Opts) ->
    negative(Rest, Out, [?new_seq($-)|Stack], Opts);
start(<<?zero, Rest/binary>>, Out, Stack, Opts) ->
    zero(Rest, Out, [?new_seq($0)|Stack], Opts);
start(<<S/utf8, Rest/binary>>, Out, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Out, [?new_seq(S)|Stack], Opts);
start(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_whitespace(S) -> 
    start(Rest, Out, Stack, Opts);
start(<<>>, Out, Stack, Opts) ->
    ?incomplete(start, <<>>, Out, Stack, Opts);
start(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


object(<<?quote, Rest/binary>>, Out, Stack, Opts) ->
    string(Rest, Out, [?new_seq()|Stack], Opts);
object(<<?end_object, Rest/binary>>, Out, [key|Stack], Opts) ->
    ?event([end_object], maybe_done, Rest, Out, Stack, Opts);
object(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_whitespace(S) ->
    object(Rest, Out, Stack, Opts);
object(<<>>, Out, Stack, Opts) ->
    ?incomplete(object, <<>>, Out, Stack, Opts);
object(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).

   
array(<<?quote, Rest/binary>>, Out, Stack, Opts) ->
    string(Rest, Out, [?new_seq()|Stack], Opts);
array(<<$t, Rest/binary>>, Out, Stack, Opts) ->
    tr(Rest, Out, Stack, Opts);
array(<<$f, Rest/binary>>, Out, Stack, Opts) ->
    fa(Rest, Out, Stack, Opts);
array(<<$n, Rest/binary>>, Out, Stack, Opts) ->
    nu(Rest, Out, Stack, Opts);
array(<<?negative, Rest/binary>>, Out, Stack, Opts) ->
    negative(Rest, Out, [?new_seq($-)|Stack], Opts);
array(<<?zero, Rest/binary>>, Out, Stack, Opts) ->
    zero(Rest, Out, [?new_seq($0)|Stack], Opts);
array(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Out, [?new_seq(S)|Stack], Opts);
array(<<?start_object, Rest/binary>>, Out, Stack, Opts) ->
    ?event([start_object], object, Rest, Out, [key|Stack], Opts);
array(<<?start_array, Rest/binary>>, Out, Stack, Opts) ->
    ?event([start_array], array, Rest, Out, [array|Stack], Opts);
array(<<?end_array, Rest/binary>>, Out, [array|Stack], Opts) ->
    maybe_done(Rest, [end_array] ++ Out, Stack, Opts);
array(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_whitespace(S) -> 
    array(Rest, Out, Stack, Opts);    
array(<<>>, Out, Stack, Opts) ->
    ?incomplete(array, <<>>, Out, Stack, Opts);
array(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


value(<<?quote, Rest/binary>>, Out, Stack, Opts) ->
    string(Rest, Out, [?new_seq()|Stack], Opts);
value(<<$t, Rest/binary>>, Out, Stack, Opts) ->
    tr(Rest, Out, Stack, Opts);
value(<<$f, Rest/binary>>, Out, Stack, Opts) ->
    fa(Rest, Out, Stack, Opts);
value(<<$n, Rest/binary>>, Out, Stack, Opts) ->
    nu(Rest, Out, Stack, Opts);
value(<<?negative, Rest/binary>>, Out, Stack, Opts) ->
    negative(Rest, Out, [?new_seq($-)|Stack], Opts);
value(<<?zero, Rest/binary>>, Out, Stack, Opts) ->
    zero(Rest, Out, [?new_seq($0)|Stack], Opts);
value(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Out, [?new_seq(S)|Stack], Opts);
value(<<?start_object, Rest/binary>>, Out, Stack, Opts) ->
    ?event([start_object], object, Rest, Out, [key|Stack], Opts);
value(<<?start_array, Rest/binary>>, Out, Stack, Opts) ->
    ?event([start_array], array, Rest, Out, [array|Stack], Opts);
value(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_whitespace(S) -> 
    value(Rest, Out, Stack, Opts);
value(<<>>, Out, Stack, Opts) ->
    ?incomplete(value, <<>>, Out, Stack, Opts);
value(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


colon(<<?colon, Rest/binary>>, Out, [key|Stack], Opts) ->
    value(Rest, Out, [object|Stack], Opts);
colon(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_whitespace(S) ->
    colon(Rest, Out, Stack, Opts);
colon(<<>>, Out, Stack, Opts) ->
    ?incomplete(colon, <<>>, Out, Stack, Opts);
colon(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


key(<<?quote, Rest/binary>>, Out, Stack, Opts) ->
    string(Rest, Out, [?new_seq()|Stack], Opts);
key(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_whitespace(S) ->
    key(Rest, Out, Stack, Opts);        
key(<<>>, Out, Stack, Opts) ->
    ?incomplete(key, <<>>, Out, Stack, Opts);
key(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


%% string appends it's output to the term at the top of the stack. for
%%   efficiency the strings are build in reverse order and reversed before
%%   being added to the output stream
%% string uses partial_utf/1 to cease parsing when invalid encodings are 
%%   encountered rather than just checking remaining binary size like other 
%%   states to eliminate certain incomplete states
%% when parsing strings, the naive detection of partial codepoints is
%%   insufficient. this incredibly anal function should detect all badly formed
%%   utf sequences
partial_utf(<<>>) -> true;
partial_utf(<<X>>) when X >= 16#c2, X =< 16#f4 -> true;
partial_utf(<<X, Y>>) when X >= 16#e0, X =< 16#f4, Y >= 16#80, Y =< 16#bf -> true;
partial_utf(<<X, Y, Z>>) 
        when X >= 16#f0, X =< 16#f4,
            Y >= 16#80, Y =< 16#bf,
            Z >= 16#80, Z =< 16#bf ->
    true;
partial_utf(_) -> false.


string(<<?quote/utf8, Rest/binary>>, Out, [Acc, key|Stack], Opts) ->
    ?event([{key, ?end_seq(Acc)}], colon, Rest, Out, [key|Stack], Opts);
string(<<?quote/utf8, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    ?event([{string, ?end_seq(Acc)}], maybe_done, Rest, Out, Stack, Opts);
string(<<?rsolidus/utf8, Rest/binary>>, Out, Stack, Opts) ->
    escape(Rest, Out, Stack, Opts);
%% things get dumb here. erlang doesn't properly restrict unicode non-characters
%%   so you can't trust the codepoints it returns always
%% the range 32..16#fdcf is safe, so allow that
string(<<S/utf8, Rest/binary>>, Out, [Acc|Stack], Opts)
        when ?is_noncontrol(S), S < 16#fdd0 ->
    string(Rest, Out, [?acc_seq(Acc, S)|Stack], Opts);
%% the range 16#fdf0..16#fffd is also safe
string(<<S/utf8, Rest/binary>>, Out, [Acc|Stack], Opts)
        when S > 16#fdef, S < 16#fffe ->
    string(Rest, Out, [?acc_seq(Acc, S)|Stack], Opts);
%% yes, i think it's insane too
string(<<S/utf8, Rest/binary>>, Out, [Acc|Stack], Opts)
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
    string(Rest, Out, [?acc_seq(Acc, S)|Stack], Opts);
string(Bin, Out, Stack, Opts) ->
    case partial_utf(Bin) of 
        true -> ?incomplete(string, Bin, Out, Stack, Opts)
        ; false ->
            case Opts#opts.loose_unicode of
                true -> noncharacter(Bin, Out, Stack, Opts)
                ; false -> ?error([Bin, Out, Stack, Opts])
            end
    end.
    
%% we don't need to guard against partial utf here, because it's already taken
%%   care of in string. theoretically, the last clause of noncharacter/4 is
%%   unreachable
%% non-characters erlang doesn't recognize as non-characters, idiotically
noncharacter(<<S/utf8, Rest/binary>>, Out, [Acc|Stack], Opts)
        when ?is_noncontrol(S) ->
    string(Rest, Out, [?acc_seq(Acc, 16#fffd)|Stack], Opts);
%% u+fffe and u+ffff
noncharacter(<<239, 191, X, Rest/binary>>, Out, [Acc|Stack], Opts) 
        when X == 190; X == 191 ->
    string(Rest, Out, [?acc_seq(Acc, 16#fffd)|Stack], Opts);
%% surrogates
noncharacter(<<237, X, _, Rest/binary>>, Out, [Acc|Stack], Opts) when X >= 160 ->
    string(Rest, Out, [?acc_seq(Acc, 16#fffd)|Stack], Opts);
noncharacter(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


escape(<<$b, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    string(Rest, Out, [?acc_seq(Acc, $\b)|Stack], Opts);
escape(<<$f, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    string(Rest, Out, [?acc_seq(Acc, $\f)|Stack], Opts);
escape(<<$n, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    string(Rest, Out, [?acc_seq(Acc, $\n)|Stack], Opts);
escape(<<$r, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    string(Rest, Out, [?acc_seq(Acc, $\r)|Stack], Opts);
escape(<<$t, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    string(Rest, Out, [?acc_seq(Acc, $\t)|Stack], Opts);
escape(<<$u, Rest/binary>>, Out, Stack, Opts) ->
    escaped_unicode(Rest, Out, [?new_seq()|Stack], Opts);      
escape(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) 
        when S =:= ?quote; S =:= ?solidus; S =:= ?rsolidus ->
    string(Rest, Out, [?acc_seq(Acc, S)|Stack], Opts);
escape(<<>>, Out, Stack, Opts) ->
    ?incomplete(escape, <<>>, Out, Stack, Opts);
escape(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


%% this code is ugly and unfortunate, but so is json's handling of escaped 
%%   unicode codepoint sequences.
escaped_unicode(<<D, Rest/binary>>, Out, [[C,B,A], Acc|Stack], Opts) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        %% high surrogate, we need a low surrogate next
        X when X >= 16#d800, X =< 16#dbff ->
            low_surrogate(Rest, Out, [X, Acc|Stack], Opts)
        %% non-characters, you're not allowed to exchange these
        ; X when X == 16#fffe; X == 16#ffff; X >= 16#fdd0, X =< 16#fdef ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, Out, [?acc_seq(Acc, 16#fffd)|Stack], Opts)
                ; false ->    
                    ?error([<<D, Rest/binary>>, Out, [[C,B,A], Acc|Stack], Opts])
            end
        %% allowing interchange of null bytes allows attackers to forge
        %%   malicious streams
        ; X when X == 16#0000 ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, Out, [?acc_seq(Acc, 16#fffd)|Stack], Opts)
                ; false ->    
                    ?error([<<D, Rest/binary>>, Out, [[C,B,A], Acc|Stack], Opts])
            end
        %% anything else
        ; X ->
            string(Rest, Out, [?acc_seq(Acc, X)|Stack], Opts)
    end;
escaped_unicode(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) 
        when ?is_hex(S) ->
    escaped_unicode(Rest, Out, [?acc_seq(Acc, S)|Stack], Opts);
escaped_unicode(<<>>, Out, Stack, Opts) ->
    ?incomplete(escaped_unicode, <<>>, Out, Stack, Opts);
escaped_unicode(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


low_surrogate(<<?rsolidus, Rest/binary>>, Out, Stack, Opts) ->
    low_surrogate_u(Rest, Out, Stack, Opts);
%% not an escaped codepoint, our high codepoint is illegal. dispatch back to
%%   string to handle
low_surrogate(<<S, Rest/binary>> = Bin, Out, [High, String|Stack], Opts) ->
    case Opts#opts.loose_unicode of
        true ->
            string(Bin, Out, [?acc_seq(String, 16#fffd)|Stack], Opts)
        ; false ->
            ?error([<<S, Rest/binary>>, Out, [High, String|Stack], Opts])
    end;
low_surrogate(<<>>, Out, Stack, Opts) ->
    ?incomplete(low_surrogate, <<>>, Out, Stack, Opts);
low_surrogate(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


low_surrogate_u(<<$u, Rest/binary>>, Out, Stack, Opts) ->
    low_surrogate_v(Rest, Out, [?new_seq()|Stack], Opts);
low_surrogate_u(<<>>, Out, Stack, Opts) ->
    ?incomplete(low_surrogate_u, <<>>, Out, Stack, Opts);
%% not a low surrogate, dispatch back to string to handle, including the
%%   rsolidus we parsed previously
low_surrogate_u(Bin, Out, [High, String|Stack], Opts) ->
    case Opts#opts.loose_unicode of
        true ->
            string(<<?rsolidus, Bin/binary>>, Out, [?acc_seq(String, 16#fffd)|Stack], Opts)
        ; false ->
            ?error([Bin, Out, [High, String|Stack], Opts])
    end.


low_surrogate_v(<<D, Rest/binary>>, Out, [[C,B,A], High, String|Stack], Opts) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >= 16#dc00, X =< 16#dfff ->
            V = surrogate_to_codepoint(High, X),
            case V rem 16#10000 of Y when Y == 16#fffe; Y == 16#ffff ->
                    case Opts#opts.loose_unicode of
                        true ->
                            string(Rest, Out, [?acc_seq(String, 16#fffd)|Stack], Opts)
                        ; false ->    
                            ?error([<<D, Rest/binary>>, Out, [[C,B,A], High, String|Stack], Opts])
                    end
                ; _ ->
                    string(Rest, Out, [?acc_seq(String, V)|Stack], Opts)
            end
        %% not a low surrogate, bad bad bad
        ; _ ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, Out, [?acc_seq(?acc_seq(String, 16#fffd), 16#fffd)|Stack], Opts)
                ; false ->    
                    ?error([<<D, Rest/binary>>, Out, [[C,B,A], High, String|Stack], Opts])
            end
    end;
low_surrogate_v(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) 
        when ?is_hex(S) ->
    low_surrogate_v(Rest, Out, [?acc_seq(Acc, S)|Stack], Opts);
low_surrogate_v(<<>>, Out, Stack, Opts) ->
    ?incomplete(low_surrogate_v, <<>>, Out, Stack, Opts);
low_surrogate_v(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


%% stole this from the unicode spec    
surrogate_to_codepoint(High, Low) ->
    (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000.


%% like strings, numbers are collected in an intermediate accumulator before
%%   being emitted to the callback handler
negative(<<$0, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    zero(Rest, Out, ["0" ++ Acc|Stack], Opts);
negative(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) when ?is_nonzero(S) ->
    integer(Rest, Out, [[S] ++ Acc|Stack], Opts);
negative(<<>>, Out, Stack, Opts) ->  
    ?incomplete(negative, <<>>, Out, Stack, Opts);
negative(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


zero(<<?end_object, Rest/binary>>, Out, [Acc, object|Stack], Opts) ->
    ?event([end_object, format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
zero(<<?end_array, Rest/binary>>, Out, [Acc, array|Stack], Opts) ->
    ?event([end_array, format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
zero(<<?comma, Rest/binary>>, Out, [Acc, object|Stack], Opts) ->
    ?event([format_number(Acc)], key, Rest, Out, [key|Stack], Opts);
zero(<<?comma, Rest/binary>>, Out, [Acc, array|Stack], Opts) ->
    ?event([format_number(Acc)], value, Rest, Out, [array|Stack], Opts);
zero(<<?decimalpoint, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    initial_decimal(Rest, Out, [{Acc, []}|Stack], Opts);
zero(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) when ?is_whitespace(S) ->
    ?event([format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
zero(<<>>, Out, [Acc|Stack], Opts = #opts{explicit_end=false}) ->
    ?event([format_number(Acc)], maybe_done, <<>>, Out, Stack, Opts);
zero(<<>>, Out, Stack, Opts) ->
    ?incomplete(zero, <<>>, Out, Stack, Opts);
zero(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


integer(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) when ?is_nonzero(S) ->
    integer(Rest, Out, [[S] ++ Acc|Stack], Opts);
integer(<<?end_object, Rest/binary>>, Out, [Acc, object|Stack], Opts) ->
    ?event([end_object, format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
integer(<<?end_array, Rest/binary>>, Out, [Acc, array|Stack], Opts) ->
    ?event([end_array, format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
integer(<<?comma, Rest/binary>>, Out, [Acc, object|Stack], Opts) ->
    ?event([format_number(Acc)], key, Rest, Out, [key|Stack], Opts);
integer(<<?comma, Rest/binary>>, Out, [Acc, array|Stack], Opts) ->
    ?event([format_number(Acc)], value, Rest, Out, [array|Stack], Opts);
integer(<<?decimalpoint, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    initial_decimal(Rest, Out, [{Acc, []}|Stack], Opts);
integer(<<?zero, Rest/binary>>, Out, [Acc|Stack], Opts) ->
    integer(Rest, Out, [[?zero] ++ Acc|Stack], Opts);
integer(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) when S =:= $e; S =:= $E ->
    e(Rest, Out, [{Acc, [], []}|Stack], Opts);
integer(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) when ?is_whitespace(S) ->
    ?event([format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
integer(<<>>, Out, [Acc|Stack], Opts = #opts{explicit_end=false}) ->
    ?event([format_number(Acc)], maybe_done, <<>>, Out, Stack, Opts);
integer(<<>>, Out, Stack, Opts) ->
    ?incomplete(integer, <<>>, Out, Stack, Opts);
integer(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


initial_decimal(<<S, Rest/binary>>, Out, [{Int, Frac}|Stack], Opts)
        when S =:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Out, [{Int, [S] ++ Frac}|Stack], Opts);
initial_decimal(<<>>, Out, Stack, Opts) ->
    ?incomplete(initial_decimal, <<>>, Out, Stack, Opts);
initial_decimal(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


decimal(<<S, Rest/binary>>, Out, [{Int, Frac}|Stack], Opts)
        when S=:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Out, [{Int, [S] ++ Frac}|Stack], Opts);
decimal(<<?end_object, Rest/binary>>, Out, [Acc, object|Stack], Opts) ->
    ?event([end_object, format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
decimal(<<?end_array, Rest/binary>>, Out, [Acc, array|Stack], Opts) ->
    ?event([end_array, format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
decimal(<<?comma, Rest/binary>>, Out, [Acc, object|Stack], Opts) ->
    ?event([format_number(Acc)], key, Rest, Out, [key|Stack], Opts);
decimal(<<?comma, Rest/binary>>, Out, [Acc, array|Stack], Opts) ->
    ?event([format_number(Acc)], value, Rest, Out, [array|Stack], Opts);
decimal(<<S, Rest/binary>>, Out, [{Int, Frac}|Stack], Opts)
        when S =:= $e; S =:= $E ->
    e(Rest, Out, [{Int, Frac, []}|Stack], Opts);
decimal(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) when ?is_whitespace(S) ->
    ?event([format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
decimal(<<>>, Out, [Acc|Stack], Opts = #opts{explicit_end=false}) ->
    ?event([format_number(Acc)], maybe_done, <<>>, Out, Stack, Opts);
decimal(<<>>, Out, Stack, Opts) ->
    ?incomplete(decimal, <<>>, Out, Stack, Opts);
decimal(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


e(<<S, Rest/binary>>, Out, [{Int, Frac, Exp}|Stack], Opts)
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Out, [{Int, Frac, [S] ++ Exp}|Stack], Opts);   
e(<<S, Rest/binary>>, Out, [{Int, Frac, Exp}|Stack], Opts)
        when S =:= ?positive; S =:= ?negative ->
    ex(Rest, Out, [{Int, Frac, [S] ++ Exp}|Stack], Opts);
e(<<>>, Out, Stack, Opts) ->  
    ?incomplete(e, <<>>, Out, Stack, Opts);
e(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


ex(<<S, Rest/binary>>, Out, [{Int, Frac, Exp}|Stack], Opts)
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Out, [{Int, Frac, [S] ++ Exp}|Stack], Opts);
ex(<<>>, Out, Stack, Opts) ->  
    ?incomplete(ex, <<>>, Out, Stack, Opts);
ex(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


exp(<<S, Rest/binary>>, Out, [{Int, Frac, Exp}|Stack], Opts)
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Out, [{Int, Frac, [S] ++ Exp}|Stack], Opts);
exp(<<?end_object, Rest/binary>>, Out, [Acc, object|Stack], Opts) ->
    ?event([end_object, format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
exp(<<?end_array, Rest/binary>>, Out, [Acc, array|Stack], Opts) ->
    ?event([end_array, format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
exp(<<?comma, Rest/binary>>, Out, [Acc, object|Stack], Opts) ->
    ?event([format_number(Acc)], key, Rest, Out, [key|Stack], Opts);
exp(<<?comma, Rest/binary>>, Out, [Acc, array|Stack], Opts) ->
    ?event([format_number(Acc)], value, Rest, Out, [array|Stack], Opts);
exp(<<S, Rest/binary>>, Out, [Acc|Stack], Opts) when ?is_whitespace(S) ->
    ?event([format_number(Acc)], maybe_done, Rest, Out, Stack, Opts);
exp(<<>>, Out, [Acc|Stack], Opts = #opts{explicit_end=false}) ->
    ?event([format_number(Acc)], maybe_done, <<>>, Out, Stack, Opts);
exp(<<>>, Out, Stack, Opts) ->
    ?incomplete(exp, <<>>, Out, Stack, Opts);
exp(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


format_number(Int) when is_list(Int) ->
    {integer, list_to_integer(lists:reverse(Int))};
format_number({Int, Frac}) ->
    {float, list_to_float(lists:reverse(Frac ++ "." ++ Int))};
format_number({Int, [], Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e0." ++ Int))};
format_number({Int, Frac, Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e" ++ Frac ++ "." ++ Int))}.


tr(<<$r, Rest/binary>>, Out, Stack, Opts) ->
    tru(Rest, Out, Stack, Opts);
tr(<<>>, Out, Stack, Opts) ->  
    ?incomplete(tr, <<>>, Out, Stack, Opts);
tr(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


tru(<<$u, Rest/binary>>, Out, Stack, Opts) ->
    true(Rest, Out, Stack, Opts);
tru(<<>>, Out, Stack, Opts) ->  
    ?incomplete(tru, <<>>, Out, Stack, Opts);
tru(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


true(<<$e, Rest/binary>>, Out, Stack, Opts) ->
    ?event([{literal, true}], maybe_done, Rest, Out, Stack, Opts);
true(<<>>, Out, Stack, Opts) ->  
    ?incomplete(true, <<>>, Out, Stack, Opts);
true(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


fa(<<$a, Rest/binary>>, Out, Stack, Opts) ->
    fal(Rest, Out, Stack, Opts);
fa(<<>>, Out, Stack, Opts) ->  
    ?incomplete(fa, <<>>, Out, Stack, Opts);
fa(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).
    

fal(<<$l, Rest/binary>>, Out, Stack, Opts) ->
    fals(Rest, Out, Stack, Opts);
fal(<<>>, Out, Stack, Opts) ->  
    ?incomplete(fal, <<>>, Out, Stack, Opts);
fal(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).
    

fals(<<$s, Rest/binary>>, Out, Stack, Opts) ->
    false(Rest, Out, Stack, Opts);
fals(<<>>, Out, Stack, Opts) ->  
    ?incomplete(fals, <<>>, Out, Stack, Opts);
fals(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).
    

false(<<$e, Rest/binary>>, Out, Stack, Opts) ->
    ?event([{literal, false}], maybe_done, Rest, Out, Stack, Opts);
false(<<>>, Out, Stack, Opts) ->  
    ?incomplete(false, <<>>, Out, Stack, Opts);
false(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


nu(<<$u, Rest/binary>>, Out, Stack, Opts) ->
    nul(Rest, Out, Stack, Opts);
nu(<<>>, Out, Stack, Opts) ->  
    ?incomplete(nu, <<>>, Out, Stack, Opts);
nu(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


nul(<<$l, Rest/binary>>, Out, Stack, Opts) ->
    null(Rest, Out, Stack, Opts);
nul(<<>>, Out, Stack, Opts) ->  
    ?incomplete(nul, <<>>, Out, Stack, Opts);
nul(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


null(<<$l, Rest/binary>>, Out, Stack, Opts) ->
    ?event([{literal, null}], maybe_done, Rest, Out, Stack, Opts);
null(<<>>, Out, Stack, Opts) ->  
    ?incomplete(null, <<>>, Out, Stack, Opts);
null(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


maybe_done(<<?end_object, Rest/binary>>, Out, [object|Stack], Opts) ->
    ?event([end_object], maybe_done, Rest, Out, Stack, Opts);
maybe_done(<<?end_array, Rest/binary>>, Out, [array|Stack], Opts) ->
    ?event([end_array], maybe_done, Rest, Out, Stack, Opts);
maybe_done(<<?comma, Rest/binary>>, Out, [object|Stack], Opts) ->
    key(Rest, Out, [key|Stack], Opts);
maybe_done(<<?comma, Rest/binary>>, Out, [array|_] = Stack, Opts) ->
    value(Rest, Out, Stack, Opts);
maybe_done(<<S, Rest/binary>>, Out, Stack, Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, Out, Stack, Opts);
maybe_done(<<>>, Out, Stack, Opts) when length(Stack) > 0 -> 
    ?incomplete(maybe_done, <<>>, Out, Stack, Opts);
maybe_done(Rest, Out, [], Opts) ->
    ?event([end_json], done, Rest, Out, [], Opts);
maybe_done(Bin, Out, Stack, Opts) ->
    ?error([Bin, Out, Stack, Opts]).


done(<<S, Rest/binary>>, Out, [], Opts) when ?is_whitespace(S) ->
    done(Rest, Out, [], Opts);
done(<<>>, Out, [], Opts = #opts{explicit_end=true}) ->
    {incomplete, fun(Stream) when is_binary(Stream) ->
                done(<<Stream/binary>>, Out, [], Opts)
            ; (end_stream) ->
                {ok, lists:reverse(Out)}
        end
    };
done(<<>>, Out, [], _Opts) -> {ok, lists:reverse(Out)};
done(Bin, Out, Stack, Opts) -> ?error([Bin, Out, Stack, Opts]).



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
    lists:dropwhile(fun({_, [{string, <<16#fffd/utf8>>}|_]}) ->
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
    R = decode(to_fake_utf(H, utf8), Opts),
    check(T, Opts, [{H, R}] ++ Acc).


decode(JSON, Opts) ->
    try
        {ok, Events} = (decoder(Opts))(JSON),
        loop(Events, [])
    catch
        error:badarg -> {error, badjson}
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
    <<34/utf8, 2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6, 34/utf8>>.


-endif.