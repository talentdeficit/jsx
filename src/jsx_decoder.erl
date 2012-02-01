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

-export([decoder/3]).


-spec decoder(Handler::module(), State::any(), Opts::jsx:opts()) -> jsx:decoder().

decoder(Handler, State, Opts) ->
    fun(JSON) -> value(JSON, {Handler, State}, [], jsx_utils:parse_opts(Opts)) end.



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
-define(incomplete(State, Rest, Handler, Stack, Opts),
    {incomplete, fun(Stream) when is_binary(Stream) ->
                State(<<Rest/binary, Stream/binary>>, Handler, Stack, Opts)
            ; (end_stream) ->
                case State(<<Rest/binary, <<" ">>/binary>>,
                        Handler,
                        Stack,
                        Opts#opts{explicit_end=false}) of
                    {incomplete, _} -> ?error([Rest, Handler, Stack, Opts])
                    ; Events -> Events
                end
        end
    }
).
-endif.


-define(new_seq(), []).
-define(new_seq(C), [C]).

-define(acc_seq(Seq, C), [C] ++ Seq).

-define(end_seq(Seq), unicode:characters_to_binary(lists:reverse(Seq))).


value(<<?quote, Rest/binary>>, Handler, Stack, Opts) ->
    string(Rest, Handler, [?new_seq()|Stack], Opts);
value(<<$t, Rest/binary>>, Handler, Stack, Opts) ->
    tr(Rest, Handler, Stack, Opts);
value(<<$f, Rest/binary>>, Handler, Stack, Opts) ->
    fa(Rest, Handler, Stack, Opts);
value(<<$n, Rest/binary>>, Handler, Stack, Opts) ->
    nu(Rest, Handler, Stack, Opts);
value(<<?negative, Rest/binary>>, Handler, Stack, Opts) ->
    negative(Rest, Handler, [?new_seq($-)|Stack], Opts);
value(<<?zero, Rest/binary>>, Handler, Stack, Opts) ->
    zero(Rest, Handler, [?new_seq($0)|Stack], Opts);
value(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Handler, [?new_seq(S)|Stack], Opts);
value(<<?start_object, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    object(Rest, {Handler, Handler:handle_event(start_object, State)}, [key|Stack], Opts);
value(<<?start_array, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    array(Rest, {Handler, Handler:handle_event(start_array, State)}, [array|Stack], Opts);
value(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) -> 
    value(Rest, Handler, Stack, Opts);
value(<<>>, Handler, Stack, Opts) ->
    ?incomplete(value, <<>>, Handler, Stack, Opts);
value(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


object(<<?quote, Rest/binary>>, Handler, Stack, Opts) ->
    string(Rest, Handler, [?new_seq()|Stack], Opts);
object(<<?end_object, Rest/binary>>, {Handler, State}, [key|Stack], Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event(end_object, State)}, Stack, Opts);
object(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) ->
    object(Rest, Handler, Stack, Opts);
object(<<>>, Handler, Stack, Opts) ->
    ?incomplete(object, <<>>, Handler, Stack, Opts);
object(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).

   
array(<<?quote, Rest/binary>>, Handler, Stack, Opts) ->
    string(Rest, Handler, [?new_seq()|Stack], Opts);
array(<<$t, Rest/binary>>, Handler, Stack, Opts) ->
    tr(Rest, Handler, Stack, Opts);
array(<<$f, Rest/binary>>, Handler, Stack, Opts) ->
    fa(Rest, Handler, Stack, Opts);
array(<<$n, Rest/binary>>, Handler, Stack, Opts) ->
    nu(Rest, Handler, Stack, Opts);
array(<<?negative, Rest/binary>>, Handler, Stack, Opts) ->
    negative(Rest, Handler, [?new_seq($-)|Stack], Opts);
array(<<?zero, Rest/binary>>, Handler, Stack, Opts) ->
    zero(Rest, Handler, [?new_seq($0)|Stack], Opts);
array(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Handler, [?new_seq(S)|Stack], Opts);
array(<<?start_object, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    object(Rest, {Handler, Handler:handle_event(start_object, State)}, [key|Stack], Opts);
array(<<?start_array, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    array(Rest, {Handler, Handler:handle_event(start_array, State)}, [array|Stack], Opts);
array(<<?end_array, Rest/binary>>, {Handler, State}, [array|Stack], Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event(end_array, State)}, Stack, Opts);
array(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) -> 
    array(Rest, Handler, Stack, Opts);    
array(<<>>, Handler, Stack, Opts) ->
    ?incomplete(array, <<>>, Handler, Stack, Opts);
array(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


colon(<<?colon, Rest/binary>>, Handler, [key|Stack], Opts) ->
    value(Rest, Handler, [object|Stack], Opts);
colon(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) ->
    colon(Rest, Handler, Stack, Opts);
colon(<<>>, Handler, Stack, Opts) ->
    ?incomplete(colon, <<>>, Handler, Stack, Opts);
colon(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


key(<<?quote, Rest/binary>>, Handler, Stack, Opts) ->
    string(Rest, Handler, [?new_seq()|Stack], Opts);
key(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) ->
    key(Rest, Handler, Stack, Opts);        
key(<<>>, Handler, Stack, Opts) ->
    ?incomplete(key, <<>>, Handler, Stack, Opts);
key(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


%% string appends it's output to the term at the top of the stack. for
%%   efficiency the strings are build in reverse order and reversed before
%%   being added to the output stream
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


string(<<?quote/utf8, Rest/binary>>, {Handler, State}, [Acc, key|Stack], Opts) ->
    colon(Rest,
        {Handler, Handler:handle_event({key, ?end_seq(Acc)}, State)},
        [key|Stack],
        Opts
    );
string(<<?quote/utf8, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event({string, ?end_seq(Acc)}, State)},
        Stack,
        Opts
    );
string(<<?rsolidus/utf8, Rest/binary>>, Handler, Stack, Opts) ->
    escape(Rest, Handler, Stack, Opts);
%% things get dumb here. erlang doesn't properly restrict unicode non-characters
%%   so you can't trust the codepoints it returns always
%% the range 32..16#fdcf is safe, so allow that
string(<<S/utf8, Rest/binary>>, Handler, [Acc|Stack], Opts)
        when ?is_noncontrol(S), S < 16#fdd0 ->
    string(Rest, Handler, [?acc_seq(Acc, S)|Stack], Opts);
%% the range 16#fdf0..16#fffd is also safe
string(<<S/utf8, Rest/binary>>, Handler, [Acc|Stack], Opts)
        when S > 16#fdef, S < 16#fffe ->
    string(Rest, Handler, [?acc_seq(Acc, S)|Stack], Opts);
%% yes, i think it's insane too
string(<<S/utf8, Rest/binary>>, Handler, [Acc|Stack], Opts)
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
    string(Rest, Handler, [?acc_seq(Acc, S)|Stack], Opts);
string(Bin, Handler, Stack, Opts) ->
    case partial_utf(Bin) of 
        true -> ?incomplete(string, Bin, Handler, Stack, Opts)
        ; false ->
            case Opts#opts.loose_unicode of
                true -> noncharacter(Bin, Handler, Stack, Opts)
                ; false -> ?error([Bin, Handler, Stack, Opts])
            end
    end.
    
%% we don't need to guard against partial utf here, because it's already taken
%%   care of in string. theoretically, the last clause of noncharacter/4 is
%%   unreachable
%% non-characters erlang doesn't recognize as non-characters
noncharacter(<<S/utf8, Rest/binary>>, Handler, [Acc|Stack], Opts)
        when ?is_noncontrol(S) ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts);
%% u+fffe and u+ffff
noncharacter(<<239, 191, X, Rest/binary>>, Handler, [Acc|Stack], Opts) 
        when X == 190; X == 191 ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts);
%% surrogates
noncharacter(<<237, X, _, Rest/binary>>, Handler, [Acc|Stack], Opts) when X >= 160 ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts);
noncharacter(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


escape(<<$b, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, $\b)|Stack], Opts);
escape(<<$f, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, $\f)|Stack], Opts);
escape(<<$n, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, $\n)|Stack], Opts);
escape(<<$r, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, $\r)|Stack], Opts);
escape(<<$t, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, $\t)|Stack], Opts);
escape(<<$u, Rest/binary>>, Handler, Stack, Opts) ->
    escaped_unicode(Rest, Handler, [?new_seq()|Stack], Opts);      
escape(<<S, Rest/binary>>, Handler, [Acc|Stack], Opts) 
        when S =:= ?quote; S =:= ?solidus; S =:= ?rsolidus ->
    string(Rest, Handler, [?acc_seq(Acc, S)|Stack], Opts);
escape(<<>>, Handler, Stack, Opts) ->
    ?incomplete(escape, <<>>, Handler, Stack, Opts);
escape(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


%% this code is ugly and unfortunate, but so is json's handling of escaped 
%%   unicode codepoint sequences.
escaped_unicode(<<D, Rest/binary>>, Handler, [[C,B,A], Acc|Stack], Opts) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        %% high surrogate, we need a low surrogate next
        X when X >= 16#d800, X =< 16#dbff ->
            low_surrogate(Rest, Handler, [X, Acc|Stack], Opts)
        %% non-characters, you're not allowed to exchange these
        ; X when X == 16#fffe; X == 16#ffff; X >= 16#fdd0, X =< 16#fdef ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts)
                ; false ->    
                    ?error([<<D, Rest/binary>>, Handler, [[C,B,A], Acc|Stack], Opts])
            end
        %% anything else
        ; X ->
            string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Opts)
    end;
escaped_unicode(<<S, Rest/binary>>, Handler, [Acc|Stack], Opts) 
        when ?is_hex(S) ->
    escaped_unicode(Rest, Handler, [?acc_seq(Acc, S)|Stack], Opts);
escaped_unicode(<<>>, Handler, Stack, Opts) ->
    ?incomplete(escaped_unicode, <<>>, Handler, Stack, Opts);
escaped_unicode(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


low_surrogate(<<?rsolidus, Rest/binary>>, Handler, Stack, Opts) ->
    low_surrogate_u(Rest, Handler, Stack, Opts);
%% not an escaped codepoint, our high codepoint is illegal. dispatch back to
%%   string to handle
low_surrogate(<<S, Rest/binary>> = Bin, Handler, [High, String|Stack], Opts) ->
    case Opts#opts.loose_unicode of
        true ->
            string(Bin, Handler, [?acc_seq(String, 16#fffd)|Stack], Opts)
        ; false ->
            ?error([<<S, Rest/binary>>, Handler, [High, String|Stack], Opts])
    end;
low_surrogate(<<>>, Handler, Stack, Opts) ->
    ?incomplete(low_surrogate, <<>>, Handler, Stack, Opts);
low_surrogate(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


low_surrogate_u(<<$u, Rest/binary>>, Handler, Stack, Opts) ->
    low_surrogate_v(Rest, Handler, [?new_seq()|Stack], Opts);
low_surrogate_u(<<>>, Handler, Stack, Opts) ->
    ?incomplete(low_surrogate_u, <<>>, Handler, Stack, Opts);
%% not a low surrogate, dispatch back to string to handle, including the
%%   rsolidus we parsed previously
low_surrogate_u(Bin, Handler, [High, String|Stack], Opts) ->
    case Opts#opts.loose_unicode of
        true ->
            string(<<?rsolidus, Bin/binary>>, Handler, [?acc_seq(String, 16#fffd)|Stack], Opts)
        ; false ->
            ?error([Bin, Handler, [High, String|Stack], Opts])
    end.


low_surrogate_v(<<D, Rest/binary>>, Handler, [[C,B,A], High, String|Stack], Opts) 
        when ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >= 16#dc00, X =< 16#dfff ->
            V = surrogate_to_codepoint(High, X),
            case V rem 16#10000 of Y when Y == 16#fffe; Y == 16#ffff ->
                    case Opts#opts.loose_unicode of
                        true ->
                            string(Rest, Handler, [?acc_seq(String, 16#fffd)|Stack], Opts)
                        ; false ->    
                            ?error([<<D, Rest/binary>>, Handler, [[C,B,A], High, String|Stack], Opts])
                    end
                ; _ ->
                    string(Rest, Handler, [?acc_seq(String, V)|Stack], Opts)
            end
        %% not a low surrogate, bad bad bad
        ; _ ->
            case Opts#opts.loose_unicode of
                true ->
                    string(Rest, Handler, [?acc_seq(?acc_seq(String, 16#fffd), 16#fffd)|Stack], Opts)
                ; false ->    
                    ?error([<<D, Rest/binary>>, Handler, [[C,B,A], High, String|Stack], Opts])
            end
    end;
low_surrogate_v(<<S, Rest/binary>>, Handler, [Acc|Stack], Opts) 
        when ?is_hex(S) ->
    low_surrogate_v(Rest, Handler, [?acc_seq(Acc, S)|Stack], Opts);
low_surrogate_v(<<>>, Handler, Stack, Opts) ->
    ?incomplete(low_surrogate_v, <<>>, Handler, Stack, Opts);
low_surrogate_v(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


%% stole this from the unicode spec    
surrogate_to_codepoint(High, Low) ->
    (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000.


%% like strings, numbers are collected in an intermediate accumulator before
%%   being emitted to the callback handler
negative(<<$0, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    zero(Rest, Handler, ["0" ++ Acc|Stack], Opts);
negative(<<S, Rest/binary>>, Handler, [Acc|Stack], Opts) when ?is_nonzero(S) ->
    integer(Rest, Handler, [[S] ++ Acc|Stack], Opts);
negative(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(negative, <<>>, Handler, Stack, Opts);
negative(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


zero(<<?end_object, Rest/binary>>, {Handler, State}, [Acc, object|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event(end_object,
            Handler:handle_event(format_number(Acc), State)
        )},
        Stack,
        Opts
    );
zero(<<?end_array, Rest/binary>>, {Handler, State}, [Acc, array|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event(end_array,
            Handler:handle_event(format_number(Acc), State)
        )},
        Stack,
        Opts
    );
zero(<<?comma, Rest/binary>>, {Handler, State}, [Acc, object|Stack], Opts) ->
    key(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [key|Stack], Opts);
zero(<<?comma, Rest/binary>>, {Handler, State}, [Acc, array|Stack], Opts) ->
    value(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [array|Stack], Opts);
zero(<<?decimalpoint, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    initial_decimal(Rest, Handler, [{Acc, []}|Stack], Opts);
zero(<<S, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, Stack, Opts);
zero(<<>>, {Handler, State}, [Acc|Stack], Opts = #opts{explicit_end=false}) ->
    maybe_done(<<>>, {Handler, Handler:handle_event(format_number(Acc), State)}, Stack, Opts);
zero(<<>>, Handler, Stack, Opts) ->
    ?incomplete(zero, <<>>, Handler, Stack, Opts);
zero(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


integer(<<S, Rest/binary>>, Handler, [Acc|Stack], Opts) when ?is_nonzero(S) ->
    integer(Rest, Handler, [[S] ++ Acc|Stack], Opts);
integer(<<?end_object, Rest/binary>>, {Handler, State}, [Acc, object|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event(end_object,
            Handler:handle_event(format_number(Acc), State)
        )},
        Stack,
        Opts
    );
integer(<<?end_array, Rest/binary>>, {Handler, State}, [Acc, array|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event(end_array,
            Handler:handle_event(format_number(Acc), State)
        )},
        Stack,
        Opts
    );
integer(<<?comma, Rest/binary>>, {Handler, State}, [Acc, object|Stack], Opts) ->
    key(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [key|Stack], Opts);
integer(<<?comma, Rest/binary>>, {Handler, State}, [Acc, array|Stack], Opts) ->
    value(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [array|Stack], Opts);
integer(<<?decimalpoint, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    initial_decimal(Rest, Handler, [{Acc, []}|Stack], Opts);
integer(<<?zero, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    integer(Rest, Handler, [[?zero] ++ Acc|Stack], Opts);
integer(<<S, Rest/binary>>, Handler, [Acc|Stack], Opts) when S =:= $e; S =:= $E ->
    e(Rest, Handler, [{Acc, [], []}|Stack], Opts);
integer(<<S, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, Stack, Opts);
integer(<<>>, {Handler, State}, [Acc|Stack], Opts = #opts{explicit_end=false}) ->
    maybe_done(<<>>, {Handler, Handler:handle_event(format_number(Acc), State)}, Stack, Opts);
integer(<<>>, Handler, Stack, Opts) ->
    ?incomplete(integer, <<>>, Handler, Stack, Opts);
integer(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


initial_decimal(<<S, Rest/binary>>, Handler, [{Int, Frac}|Stack], Opts)
        when S =:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Handler, [{Int, [S] ++ Frac}|Stack], Opts);
initial_decimal(<<>>, Handler, Stack, Opts) ->
    ?incomplete(initial_decimal, <<>>, Handler, Stack, Opts);
initial_decimal(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


decimal(<<S, Rest/binary>>, Handler, [{Int, Frac}|Stack], Opts)
        when S=:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Handler, [{Int, [S] ++ Frac}|Stack], Opts);
decimal(<<?end_object, Rest/binary>>, {Handler, State}, [Acc, object|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event(end_object,
            Handler:handle_event(format_number(Acc), State)
        )},
        Stack,
        Opts
    );
decimal(<<?end_array, Rest/binary>>, {Handler, State}, [Acc, array|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event(end_array,
            Handler:handle_event(format_number(Acc), State)
        )},
        Stack,
        Opts
    );
decimal(<<?comma, Rest/binary>>, {Handler, State}, [Acc, object|Stack], Opts) ->
    key(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [key|Stack], Opts);
decimal(<<?comma, Rest/binary>>, {Handler, State}, [Acc, array|Stack], Opts) ->
    value(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [array|Stack], Opts);
decimal(<<S, Rest/binary>>, Handler, [{Int, Frac}|Stack], Opts)
        when S =:= $e; S =:= $E ->
    e(Rest, Handler, [{Int, Frac, []}|Stack], Opts);
decimal(<<S, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, Stack, Opts);
decimal(<<>>, {Handler, State}, [Acc|Stack], Opts = #opts{explicit_end=false}) ->
    maybe_done(<<>>, {Handler, Handler:handle_event(format_number(Acc), State)}, Stack, Opts);
decimal(<<>>, Handler, Stack, Opts) ->
    ?incomplete(decimal, <<>>, Handler, Stack, Opts);
decimal(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


e(<<S, Rest/binary>>, Handler, [{Int, Frac, Exp}|Stack], Opts)
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, [{Int, Frac, [S] ++ Exp}|Stack], Opts);   
e(<<S, Rest/binary>>, Handler, [{Int, Frac, Exp}|Stack], Opts)
        when S =:= ?positive; S =:= ?negative ->
    ex(Rest, Handler, [{Int, Frac, [S] ++ Exp}|Stack], Opts);
e(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(e, <<>>, Handler, Stack, Opts);
e(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


ex(<<S, Rest/binary>>, Handler, [{Int, Frac, Exp}|Stack], Opts)
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, [{Int, Frac, [S] ++ Exp}|Stack], Opts);
ex(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(ex, <<>>, Handler, Stack, Opts);
ex(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


exp(<<S, Rest/binary>>, Handler, [{Int, Frac, Exp}|Stack], Opts)
        when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, [{Int, Frac, [S] ++ Exp}|Stack], Opts);
exp(<<?end_object, Rest/binary>>, {Handler, State}, [Acc, object|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event(end_object,
            Handler:handle_event(format_number(Acc), State)
        )},
        Stack,
        Opts
    );
exp(<<?end_array, Rest/binary>>, {Handler, State}, [Acc, array|Stack], Opts) ->
    maybe_done(Rest,
        {Handler, Handler:handle_event(end_array,
            Handler:handle_event(format_number(Acc), State)
        )},
        Stack,
        Opts
    );
exp(<<?comma, Rest/binary>>, {Handler, State}, [Acc, object|Stack], Opts) ->
    key(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [key|Stack], Opts);
exp(<<?comma, Rest/binary>>, {Handler, State}, [Acc, array|Stack], Opts) ->
    value(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [array|Stack], Opts);
exp(<<S, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, Stack, Opts);
exp(<<>>, {Handler, State}, [Acc|Stack], Opts = #opts{explicit_end=false}) ->
    maybe_done(<<>>, {Handler, Handler:handle_event(format_number(Acc), State)}, Stack, Opts);
exp(<<>>, Handler, Stack, Opts) ->
    ?incomplete(exp, <<>>, Handler, Stack, Opts);
exp(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


format_number(Int) when is_list(Int) ->
    {integer, list_to_integer(lists:reverse(Int))};
format_number({Int, Frac}) ->
    {float, list_to_float(lists:reverse(Frac ++ "." ++ Int))};
format_number({Int, [], Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e0." ++ Int))};
format_number({Int, Frac, Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e" ++ Frac ++ "." ++ Int))}.


tr(<<$r, Rest/binary>>, Handler, Stack, Opts) ->
    tru(Rest, Handler, Stack, Opts);
tr(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(tr, <<>>, Handler, Stack, Opts);
tr(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


tru(<<$u, Rest/binary>>, Handler, Stack, Opts) ->
    true(Rest, Handler, Stack, Opts);
tru(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(tru, <<>>, Handler, Stack, Opts);
tru(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


true(<<$e, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event({literal, true}, State)}, Stack, Opts);
true(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(true, <<>>, Handler, Stack, Opts);
true(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


fa(<<$a, Rest/binary>>, Handler, Stack, Opts) ->
    fal(Rest, Handler, Stack, Opts);
fa(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(fa, <<>>, Handler, Stack, Opts);
fa(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).
    

fal(<<$l, Rest/binary>>, Handler, Stack, Opts) ->
    fals(Rest, Handler, Stack, Opts);
fal(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(fal, <<>>, Handler, Stack, Opts);
fal(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).
    

fals(<<$s, Rest/binary>>, Handler, Stack, Opts) ->
    false(Rest, Handler, Stack, Opts);
fals(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(fals, <<>>, Handler, Stack, Opts);
fals(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).
    

false(<<$e, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event({literal, false}, State)}, Stack, Opts);
false(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(false, <<>>, Handler, Stack, Opts);
false(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


nu(<<$u, Rest/binary>>, Handler, Stack, Opts) ->
    nul(Rest, Handler, Stack, Opts);
nu(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(nu, <<>>, Handler, Stack, Opts);
nu(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


nul(<<$l, Rest/binary>>, Handler, Stack, Opts) ->
    null(Rest, Handler, Stack, Opts);
nul(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(nul, <<>>, Handler, Stack, Opts);
nul(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


null(<<$l, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event({literal, null}, State)}, Stack, Opts);
null(<<>>, Handler, Stack, Opts) ->  
    ?incomplete(null, <<>>, Handler, Stack, Opts);
null(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


maybe_done(<<?end_object, Rest/binary>>, {Handler, State}, [object|Stack], Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event(end_object, State)}, Stack, Opts);
maybe_done(<<?end_array, Rest/binary>>, {Handler, State}, [array|Stack], Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event(end_array, State)}, Stack, Opts);
maybe_done(<<?comma, Rest/binary>>, Handler, [object|Stack], Opts) ->
    key(Rest, Handler, [key|Stack], Opts);
maybe_done(<<?comma, Rest/binary>>, Handler, [array|_] = Stack, Opts) ->
    value(Rest, Handler, Stack, Opts);
maybe_done(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) ->
    maybe_done(Rest, Handler, Stack, Opts);
maybe_done(<<>>, Handler, Stack, Opts) when length(Stack) > 0 -> 
    ?incomplete(maybe_done, <<>>, Handler, Stack, Opts);
maybe_done(Rest, {Handler, State}, [], Opts) ->
    done(Rest, {Handler, Handler:handle_event(end_json, State)}, [], Opts);
maybe_done(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


done(<<S, Rest/binary>>, Handler, [], Opts) when ?is_whitespace(S) ->
    done(Rest, Handler, [], Opts);
done(<<>>, {Handler, State}, [], Opts = #opts{explicit_end=true}) ->
    {incomplete, fun(Stream) when is_binary(Stream) ->
                done(<<Stream/binary>>, {Handler, State}, [], Opts)
            ; (end_stream) -> State
        end
    };
done(<<>>, {_Handler, State}, [], _Opts) -> State;
done(Bin, Handler, Stack, Opts) -> ?error([Bin, Handler, Stack, Opts]).



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
    lists:dropwhile(fun({_, [{string, <<16#fffd/utf8>>}|_]}) -> true
            ; (_) -> false 
        end,
        check(List, [loose_unicode], [])
    ).

check_good(List) ->
    lists:dropwhile(fun({_, [{string, _}|_]}) -> true ; (_) -> false end,
        check(List, [], [])
    ).

check([], _Opts, Acc) -> Acc;
check([H|T], Opts, Acc) ->
    R = decode(to_fake_utf(H, utf8), Opts),
    check(T, Opts, [{H, R}] ++ Acc).


decode(JSON, Opts) ->
    try
        (decoder(jsx, [], Opts))(JSON)
    catch
        error:badarg -> {error, badjson}
    end.
    


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