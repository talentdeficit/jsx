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
-define(incomplete(State, Rest, T, Stack, Opts),
    {ok, lists:reverse(T), fun(Stream) when is_binary(Stream) ->
            State(<<Stream/binary, Rest/binary>>, [], Stack, Opts)
        end
    }
).
-define(incomplete(State, Rest, T, Stack, Opts, Acc),
    {ok, T, fun(Stream) when is_binary(Stream) ->
            State(<<Stream/binary, Rest/binary>>, [], Stack, Opts, Acc)
        end
    }
).
-endif.


-ifndef(event).
-define(event(Event, State, Rest, T, Stack, Opts),
    State(Rest, Event ++ T, Stack, Opts)
).
-endif.


start(<<?start_object, Rest/binary>>, T, Stack, Opts) ->
    ?event([start_object], object, Rest, T, [key|Stack], Opts);
start(<<?start_array, Rest/binary>>, T, Stack, Opts) ->
    ?event([start_array], array, Rest, T, [array|Stack], Opts);
start(<<?quote, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
start(<<$t, Rest/binary>>, T, Stack, Opts) ->
    tr(Rest, T, Stack, Opts);
start(<<$f, Rest/binary>>, T, Stack, Opts) ->
    fa(Rest, T, Stack, Opts);
start(<<$n, Rest/binary>>, T, Stack, Opts) ->
    nu(Rest, T, Stack, Opts);
start(<<?negative, Rest/binary>>, T, Stack, Opts) ->
    negative(Rest, T, Stack, Opts, "-");
start(<<?zero, Rest/binary>>, T, Stack, Opts) ->
    zero(Rest, T, Stack, Opts, "0");
start(<<S/utf8, Rest/binary>>, T, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S]);
start(<<S, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) -> 
    start(Rest, T, Stack, Opts);
start(<<>>, T, Stack, Opts) ->
    ?incomplete(start, <<>>, T, Stack, Opts);
start(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


object(<<?quote, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
object(<<?end_object, Rest/binary>>, T, [key|Stack], Opts) ->
    ?event([end_object], maybe_done, Rest, T, Stack, Opts);
object(<<S, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) ->
    object(Rest, T, Stack, Opts);
object(<<>>, T, Stack, Opts) ->
    ?incomplete(object, <<>>, T, Stack, Opts);
object(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).

   
array(<<?quote, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
array(<<$t, Rest/binary>>, T, Stack, Opts) ->
    tr(Rest, T, Stack, Opts);
array(<<$f, Rest/binary>>, T, Stack, Opts) ->
    fa(Rest, T, Stack, Opts);
array(<<$n, Rest/binary>>, T, Stack, Opts) ->
    nu(Rest, T, Stack, Opts);
array(<<?negative, Rest/binary>>, T, Stack, Opts) ->
    negative(Rest, T, Stack, Opts, "-");
array(<<?zero, Rest/binary>>, T, Stack, Opts) ->
    zero(Rest, T, Stack, Opts, "0");
array(<<S, Rest/binary>>, T, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S]);
array(<<?start_object, Rest/binary>>, T, Stack, Opts) ->
    ?event([start_object], object, Rest, T, [key|Stack], Opts);
array(<<?start_array, Rest/binary>>, T, Stack, Opts) ->
    ?event([start_array], array, Rest, T, [array|Stack], Opts);
array(<<?end_array, Rest/binary>>, T, [array|Stack], Opts) ->
    maybe_done(Rest, [end_array] ++ T, Stack, Opts);
array(<<S, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) -> 
    array(Rest, T, Stack, Opts);    
array(<<>>, T, Stack, Opts) ->
    ?incomplete(array, <<>>, T, Stack, Opts);
array(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


value(<<?quote, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
value(<<$t, Rest/binary>>, T, Stack, Opts) ->
    tr(Rest, T, Stack, Opts);
value(<<$f, Rest/binary>>, T, Stack, Opts) ->
    fa(Rest, T, Stack, Opts);
value(<<$n, Rest/binary>>, T, Stack, Opts) ->
    nu(Rest, T, Stack, Opts);
value(<<?negative, Rest/binary>>, T, Stack, Opts) ->
    negative(Rest, T, Stack, Opts, "-");
value(<<?zero, Rest/binary>>, T, Stack, Opts) ->
    zero(Rest, T, Stack, Opts, "0");
value(<<S, Rest/binary>>, T, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S]);
value(<<?start_object, Rest/binary>>, T, Stack, Opts) ->
    ?event([start_object], object, Rest, T, [key|Stack], Opts);
value(<<?start_array, Rest/binary>>, T, Stack, Opts) ->
    ?event([start_array], array, Rest, T, [array|Stack], Opts);
value(<<S, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) -> 
    value(Rest, T, Stack, Opts);
value(<<>>, T, Stack, Opts) ->
    ?incomplete(value, <<>>, T, Stack, Opts);
value(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


colon(<<?colon, Rest/binary>>, T, [key|Stack], Opts) ->
    value(Rest, T, [object|Stack], Opts);
colon(<<S, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) ->
    colon(Rest, T, Stack, Opts);
colon(<<>>, T, Stack, Opts) ->
    ?incomplete(colon, <<>>, T, Stack, Opts);
colon(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


key(<<?quote, Rest/binary>>, T, Stack, Opts) ->
    string(Rest, T, Stack, Opts, []);
key(<<S, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) ->
    key(Rest, T, Stack, Opts);        
key(<<>>, T, Stack, Opts) ->
    ?incomplete(key, <<>>, T, Stack, Opts);
key(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


%% string has an additional parameter, an accumulator (Acc) used to hold the 
%%   intermediate representation of the string being parsed. using a list of 
%%   integers representing unicode codepoints is faster than constructing 
%%   binaries, there's a branch kicking around which proves it
%% string uses partial_utf/1 to cease parsing when invalid encodings are 
%%   encountered rather than just checking remaining binary size like other 
%%   states to eliminate certain incomplete states
%% when parsing strings, the naive detection of partial codepoints is
%%   insufficient. this incredibly anal function should detect all badly formed
%%   utf sequences
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

string(<<?quote, Rest/binary>>, T, [key|_] = Stack, Opts, Acc) ->
    ?event([{key, lists:reverse(Acc)}], colon, Rest, T, Stack, Opts);
string(<<?quote, Rest/binary>>, T, Stack, Opts, Acc) ->
    ?event([{string, lists:reverse(Acc)}], maybe_done, Rest, T, Stack, Opts);
string(<<?rsolidus, Rest/binary>>, T, Stack, Opts, Acc) ->
    escape(Rest, T, Stack, Opts, Acc);
%% things get dumb here. erlang doesn't properly restrict unicode non-characters
%%   so you can't trust the codepoints it returns always
%% the range 32..16#fdcf is safe, so allow that
string(<<S/utf8, Rest/binary>>, T, Stack, Opts, Acc)
        when ?is_noncontrol(S), S < 16#fdd0 ->
    string(Rest, T, Stack, Opts, [S] ++ Acc);
%% the range 16#fdf0..16#fffd is also safe
string(<<S/utf8, Rest/binary>>, T, Stack, Opts, Acc)
        when S > 16#fdef, S < 16#fffe ->
    string(Rest, T, Stack, Opts, [S] ++ Acc);
%% yes, i think it's insane too
string(<<S/utf8, Rest/binary>>, T, Stack, Opts, Acc)
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
        true -> ?incomplete(string, Bin, T, Stack, Opts, Acc)
        ; false ->
            case Opts#opts.loose_unicode of
                true -> noncharacter(Bin, T, Stack, Opts, Acc)
                ; false -> ?error([Bin, T, Stack, Opts, Acc])
            end
    end.
    
%% we don't need to guard against partial utf here, because it's already taken
%%   care of in string. theoretically, the last clause of noncharacter/4 is
%%   unreachable
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
noncharacter(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


escape(<<$b, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\b" ++ Acc);
escape(<<$f, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\f" ++ Acc);
escape(<<$n, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\n" ++ Acc);
escape(<<$r, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\r" ++ Acc);
escape(<<$t, Rest/binary>>, T, Stack, Opts, Acc) ->
    string(Rest, T, Stack, Opts, "\t" ++ Acc);
escape(<<$u, Rest/binary>>, T, Stack, Opts, Acc) ->
    escaped_unicode(Rest, T, Stack, Opts, {[], Acc});      
escape(<<S, Rest/binary>>, T, Stack, Opts, Acc) 
        when S =:= ?quote; S =:= ?solidus; S =:= ?rsolidus ->
    string(Rest, T, Stack, Opts, [S] ++ Acc);
escape(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(escape, <<>>, T, Stack, Opts, Acc);
escape(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


%% this code is ugly and unfortunate, but so is json's handling of escaped 
%%   unicode codepoint sequences.
escaped_unicode(<<D, Rest/binary>>, T, Stack, Opts, {[C, B, A], String}) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        %% high surrogate, we need a low surrogate next
        X when X >= 16#d800, X =< 16#dbff ->
            low_surrogate(Rest, T, Stack, Opts, {X, String})
        %% non-characters, you're not allowed to exchange these
        ; X when X == 16#fffe; X == 16#ffff; X >= 16#fdd0, X =< 16#fdef ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, T, Stack, Opts, [16#fffd] ++ String)
                ; false ->    
                    ?error([<<D, Rest/binary>>, T, Stack, Opts, {[C, B, A], String}])
            end
        %% allowing interchange of null bytes allows attackers to forge
        %%   malicious streams
        ; X when X == 16#0000 ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, T, Stack, Opts, [16#fffd] ++ String)
                ; false ->    
                    ?error([<<D, Rest/binary>>, T, Stack, Opts, {[C, B, A], String}])
            end
        %% anything else
        ; X ->
            string(Rest, T, Stack, Opts, [X] ++ String)
    end;
escaped_unicode(<<S, Rest/binary>>, T, Stack, Opts, {Acc, String}) 
        when ?is_hex(S) ->
    escaped_unicode(Rest, T, Stack, Opts, {[S] ++ Acc, String});
escaped_unicode(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(escaped_unicode, <<>>, T, Stack, Opts, Acc);
escaped_unicode(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


low_surrogate(<<?rsolidus, Rest/binary>>, T, Stack, Opts, Acc) ->
    low_surrogate_u(Rest, T, Stack, Opts, Acc);
%% not an escaped codepoint, our high codepoint is illegal. dispatch back to
%%   string to handle
low_surrogate(<<S, Rest/binary>> = Bin, T, Stack, Opts, {High, String}) ->
    case Opts#opts.loose_unicode of
        true ->
            string(Bin, T, Stack, Opts, [16#fffd] ++ String)
        ; false ->
            ?error([<<S, Rest/binary>>, T, Stack, Opts, {High, String}])
    end;
low_surrogate(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(low_surrogate, <<>>, T, Stack, Opts, Acc);
low_surrogate(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


low_surrogate_u(<<$u, Rest/binary>>, T, Stack, Opts, {High, String}) ->
    low_surrogate_v(Rest, T, Stack, Opts, {[], High, String});
%% not a low surrogate, dispatch back to string to handle, including the
%%   rsolidus we parsed previously
low_surrogate_u(<<S, Rest/binary>> = Bin, T, Stack, Opts, {High, String}) ->
    case Opts#opts.loose_unicode of
        true ->
            string(<<?rsolidus, Bin/binary>>,
                T,
                Stack,
                Opts,
                [16#fffd] ++ String
            )
        ; false ->
            ?error([<<S, Rest/binary>>, T, Stack, Opts, {High, String}])
    end;
low_surrogate_u(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(low_surrogate_u, <<>>, T, Stack, Opts, Acc);
low_surrogate_u(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


low_surrogate_v(<<D, Rest/binary>>, T, Stack, Opts, {[C, B, A], High, String}) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >= 16#dc00, X =< 16#dfff ->
            V = surrogate_to_codepoint(High, X),
            case V rem 16#10000 of Y when Y == 16#fffe; Y == 16#ffff ->
                    case Opts#opts.loose_unicode of
                        true ->
                            string(Rest, T, Stack, Opts, [16#fffd] ++ String)
                        ; false ->    
                            ?error([<<D, Rest/binary>>, T, Stack, Opts, {[C, B, A], High, String}])
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
                    ?error([<<D, Rest/binary>>, T, Stack, Opts, {[C, B, A], High, String}])
            end
    end;
low_surrogate_v(<<S, Rest/binary>>, T, Stack, Opts, {Low, High, String}) 
        when ?is_hex(S) ->
    low_surrogate_v(Rest, T, Stack, Opts, {[S] ++ Low, High, String});
low_surrogate_v(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(low_surrogate_v, <<>>, T, Stack, Opts, Acc);
low_surrogate_v(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


%% stole this from the unicode spec    
surrogate_to_codepoint(High, Low) ->
    (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000.


%% like strings, numbers are collected in an intermediate accumulator before
%%   being emitted to the callback handler
negative(<<$0, Rest/binary>>, T, Stack, Opts, Acc) ->
    zero(Rest, T, Stack, Opts, "0" ++ Acc);
negative(<<S, Rest/binary>>, T, Stack, Opts, Acc) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S] ++ Acc);
negative(<<>>, T, Stack, Opts, Acc) ->  
    ?incomplete(negative, <<>>, T, Stack, Opts, Acc);
negative(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


zero(<<?end_object, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?event([end_object, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
zero(<<?end_array, Rest/binary>>, T, [array|Stack], Opts, Acc) ->
    ?event([end_array, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
zero(<<?comma, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?event([format_number(Acc)], key, Rest, T, [key|Stack], Opts);
zero(<<?comma, Rest/binary>>, T, [array|_] = Stack, Opts, Acc) ->
    ?event([format_number(Acc)], value, Rest, T, Stack, Opts);
zero(<<?decimalpoint, Rest/binary>>, T, Stack, Opts, Acc) ->
    initial_decimal(Rest, T, Stack, Opts, {Acc, []});
zero(<<S, Rest/binary>>, T, Stack, Opts, Acc) when ?is_whitespace(S) ->
    ?event([format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
zero(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(zero, <<>>, T, Stack, Opts, Acc);
zero(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


integer(<<S, Rest/binary>>, T, Stack, Opts, Acc) when ?is_nonzero(S) ->
    integer(Rest, T, Stack, Opts, [S] ++ Acc);
integer(<<?end_object, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?event([end_object, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
integer(<<?end_array, Rest/binary>>, T, [array|Stack], Opts, Acc) ->
    ?event([end_array, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
integer(<<?comma, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?event([format_number(Acc)], key, Rest, T, [key|Stack], Opts);
integer(<<?comma, Rest/binary>>, T, [array|_] = Stack, Opts, Acc) ->
    ?event([format_number(Acc)], value, Rest, T, Stack, Opts);
integer(<<?decimalpoint, Rest/binary>>, T, Stack, Opts, Acc) ->
    initial_decimal(Rest, T, Stack, Opts, {Acc, []});
integer(<<?zero, Rest/binary>>, T, Stack, Opts, Acc) ->
    integer(Rest, T, Stack, Opts, [?zero] ++ Acc);
integer(<<S, Rest/binary>>, T, Stack, Opts, Acc) when S =:= $e; S =:= $E ->
    e(Rest, T, Stack, Opts, {Acc, [], []});
integer(<<S, Rest/binary>>, T, Stack, Opts, Acc) when ?is_whitespace(S) ->
    ?event([format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
integer(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(integer, <<>>, T, Stack, Opts, Acc);
integer(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


initial_decimal(<<S, Rest/binary>>, T, Stack, Opts, {Int, Frac})
        when S =:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, T, Stack, Opts, {Int, [S] ++ Frac});
initial_decimal(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(initial_decimal, <<>>, T, Stack, Opts, Acc);
initial_decimal(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


decimal(<<S, Rest/binary>>, T, Stack, Opts, {Int, Frac})
        when S=:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, T, Stack, Opts, {Int, [S] ++ Frac});
decimal(<<?end_object, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?event([end_object, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
decimal(<<?end_array, Rest/binary>>, T, [array|Stack], Opts, Acc) ->
    ?event([end_array, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
decimal(<<?comma, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?event([format_number(Acc)], key, Rest, T, [key|Stack], Opts);
decimal(<<?comma, Rest/binary>>, T, [array|_] = Stack, Opts, Acc) ->
    ?event([format_number(Acc)], value, Rest, T, Stack, Opts);
decimal(<<S, Rest/binary>>, T, Stack, Opts, {Int, Frac})
        when S =:= $e; S =:= $E ->
    e(Rest, T, Stack, Opts, {Int, Frac, []});
decimal(<<S, Rest/binary>>, T, Stack, Opts, Acc) when ?is_whitespace(S) ->
    ?event([format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
decimal(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(decimal, <<>>, T, Stack, Opts, Acc);
decimal(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


e(<<S, Rest/binary>>, T, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, T, Stack, Opts, {Int, Frac, [S] ++ Exp});   
e(<<S, Rest/binary>>, T, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?positive; S =:= ?negative ->
    ex(Rest, T, Stack, Opts, {Int, Frac, [S] ++ Exp});
e(<<>>, T, Stack, Opts, Acc) ->  
    ?incomplete(e, <<>>, T, Stack, Opts, Acc);
e(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


ex(<<S, Rest/binary>>, T, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, T, Stack, Opts, {Int, Frac, [S] ++ Exp});
ex(<<>>, T, Stack, Opts, Acc) ->  
    ?incomplete(ex, <<>>, T, Stack, Opts, Acc);
ex(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


exp(<<S, Rest/binary>>, T, Stack, Opts, {Int, Frac, Exp})
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, T, Stack, Opts, {Int, Frac, [S] ++ Exp});
exp(<<?end_object, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?event([end_object, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
exp(<<?end_array, Rest/binary>>, T, [array|Stack], Opts, Acc) ->
    ?event([end_array, format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
exp(<<?comma, Rest/binary>>, T, [object|Stack], Opts, Acc) ->
    ?event([format_number(Acc)], key, Rest, T, [key|Stack], Opts);
exp(<<?comma, Rest/binary>>, T, [array|_] = Stack, Opts, Acc) ->
    ?event([format_number(Acc)], value, Rest, T, Stack, Opts);
exp(<<S, Rest/binary>>, T, Stack, Opts, Acc) when ?is_whitespace(S) ->
    ?event([format_number(Acc)], maybe_done, Rest, T, Stack, Opts);
exp(<<>>, T, Stack, Opts, Acc) ->
    ?incomplete(exp, <<>>, T, Stack, Opts, Acc);
exp(Bin, T, Stack, Opts, Acc) ->
    ?error([Bin, T, Stack, Opts, Acc]).


format_number(Int) when is_list(Int) ->
    {integer, list_to_integer(lists:reverse(Int))};
format_number({Int, Frac}) ->
    {float, list_to_float(lists:reverse(Frac ++ "." ++ Int))};
format_number({Int, [], Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e0." ++ Int))};
format_number({Int, Frac, Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e" ++ Frac ++ "." ++ Int))}.


tr(<<$r, Rest/binary>>, T, Stack, Opts) ->
    tru(Rest, T, Stack, Opts);
tr(<<>>, T, Stack, Opts) ->  
    ?incomplete(tr, <<>>, T, Stack, Opts);
tr(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


tru(<<$u, Rest/binary>>, T, Stack, Opts) ->
    true(Rest, T, Stack, Opts);
tru(<<>>, T, Stack, Opts) ->  
    ?incomplete(tru, <<>>, T, Stack, Opts);
tru(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


true(<<$e, Rest/binary>>, T, Stack, Opts) ->
    ?event([{literal, true}], maybe_done, Rest, T, Stack, Opts);
true(<<>>, T, Stack, Opts) ->  
    ?incomplete(true, <<>>, T, Stack, Opts);
true(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


fa(<<$a, Rest/binary>>, T, Stack, Opts) ->
    fal(Rest, T, Stack, Opts);
fa(<<>>, T, Stack, Opts) ->  
    ?incomplete(fa, <<>>, T, Stack, Opts);
fa(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).
    

fal(<<$l, Rest/binary>>, T, Stack, Opts) ->
    fals(Rest, T, Stack, Opts);
fal(<<>>, T, Stack, Opts) ->  
    ?incomplete(fal, <<>>, T, Stack, Opts);
fal(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).
    

fals(<<$s, Rest/binary>>, T, Stack, Opts) ->
    false(Rest, T, Stack, Opts);
fals(<<>>, T, Stack, Opts) ->  
    ?incomplete(fals, <<>>, T, Stack, Opts);
fals(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).
    

false(<<$e, Rest/binary>>, T, Stack, Opts) ->
    ?event([{literal, false}], maybe_done, Rest, T, Stack, Opts);
false(<<>>, T, Stack, Opts) ->  
    ?incomplete(false, <<>>, T, Stack, Opts);
false(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


nu(<<$u, Rest/binary>>, T, Stack, Opts) ->
    nul(Rest, T, Stack, Opts);
nu(<<>>, T, Stack, Opts) ->  
    ?incomplete(nu, <<>>, T, Stack, Opts);
nu(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


nul(<<$l, Rest/binary>>, T, Stack, Opts) ->
    null(Rest, T, Stack, Opts);
nul(<<>>, T, Stack, Opts) ->  
    ?incomplete(nul, <<>>, T, Stack, Opts);
nul(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


null(<<$l, Rest/binary>>, T, Stack, Opts) ->
    ?event([{literal, null}], maybe_done, Rest, T, Stack, Opts);
null(<<>>, T, Stack, Opts) ->  
    ?incomplete(null, <<>>, T, Stack, Opts);
null(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


maybe_done(<<?end_object, Rest/binary>>, T, [object|Stack], Opts) ->
    ?event([end_object], maybe_done, Rest, T, Stack, Opts);
maybe_done(<<?end_array, Rest/binary>>, T, [array|Stack], Opts) ->
    ?event([end_array], maybe_done, Rest, T, Stack, Opts);
maybe_done(<<?comma, Rest/binary>>, T, [object|Stack], Opts) ->
    key(Rest, T, [key|Stack], Opts);
maybe_done(<<?comma, Rest/binary>>, T, [array|_] = Stack, Opts) ->
    value(Rest, T, Stack, Opts);
maybe_done(<<S, Rest/binary>>, T, Stack, Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, T, Stack, Opts);
maybe_done(Rest, T, [], Opts) ->
    ?event([end_json], done, Rest, T, [], Opts);
maybe_done(<<>>, T, Stack, Opts) -> 
    ?incomplete(maybe_done, <<>>, T, Stack, Opts);
maybe_done(Bin, T, Stack, Opts) ->
    ?error([Bin, T, Stack, Opts]).


done(<<S, Rest/binary>>, T, [], Opts) when ?is_whitespace(S) ->
    done(Rest, T, [], Opts);
done(<<>>, T, [], Opts) -> ?incomplete(done, <<>>, T, [], Opts);
done(Bin, T, [], Opts) -> ?error([Bin, T, [], Opts]).