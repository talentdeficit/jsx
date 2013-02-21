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


-spec decoder(Handler::module(), State::any(), Config::jsx:config()) -> jsx:decoder().

decoder(Handler, State, Config) ->
    fun(JSON) -> start(JSON, {Handler, Handler:init(State)}, [], jsx_utils:parse_config(Config)) end.


-include("jsx_config.hrl").


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
-define(doublequote, 16#22).
-define(singlequote, 16#27).
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
-define(star, 16#2A).


%% some useful guards
-define(is_hex(Symbol),
    (Symbol >= $a andalso Symbol =< $z);
    (Symbol >= $A andalso Symbol =< $Z);
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
-define(incomplete(State, Rest, Handler, Stack, Config),
    {incomplete, fun(Stream) when is_binary(Stream) ->
                State(<<Rest/binary, Stream/binary>>, Handler, Stack, Config)
            ; (end_stream) ->
                case State(<<Rest/binary, <<" ">>/binary>>,
                        Handler,
                        Stack,
                        Config#config{explicit_end=false}) of
                    {incomplete, _} -> ?error([Rest, Handler, Stack, Config])
                    ; Events -> Events
                end
        end
    }
).
-endif.


-define(new_seq(), []).
-define(new_seq(C), [C]).

-define(acc_seq(Seq, C), [C] ++ Seq).
-define(acc_seq(Seq, C, D), [C, D] ++ Seq).

-define(end_seq(Seq), unicode:characters_to_binary(lists:reverse(Seq))).


handle_event([], Handler, _Config) -> Handler;
handle_event([Event|Rest], Handler, Config) -> handle_event(Rest, handle_event(Event, Handler, Config), Config);
handle_event(Event, {Handler, State}, _Config) -> {Handler, Handler:handle_event(Event, State)}.


start(<<16#ef, Rest/binary>>, Handler, Stack, Config) ->
    maybe_bom(Rest, Handler, Stack, Config);
start(<<>>, Handler, Stack, Config) ->
    ?incomplete(start, <<>>, Handler, Stack, Config);
start(Bin, Handler, Stack, Config) ->
    value(Bin, Handler, Stack, Config).


maybe_bom(<<16#bb, Rest/binary>>, Handler, Stack, Config) ->
    definitely_bom(Rest, Handler, Stack, Config);
maybe_bom(<<>>, Handler, Stack, Config) ->
    ?incomplete(maybe_bom, <<>>, Handler, Stack, Config);
maybe_bom(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


definitely_bom(<<16#bf, Rest/binary>>, Handler, Stack, Config) ->
    value(Rest, Handler, Stack, Config);
definitely_bom(<<>>, Handler, Stack, Config) ->
    ?incomplete(definitely_bom, <<>>, Handler, Stack, Config);
definitely_bom(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


value(<<?doublequote, Rest/binary>>, Handler, Stack, Config) ->
    string(Rest, Handler, [string|Stack], Config);
value(<<?singlequote, Rest/binary>>, Handler, Stack, Config = #config{single_quoted_strings=true}) ->
    string(Rest, Handler, [single_quoted_string|Stack], Config);
value(<<$t, Rest/binary>>, Handler, Stack, Config) ->
    tr(Rest, Handler, Stack, Config);
value(<<$f, Rest/binary>>, Handler, Stack, Config) ->
    fa(Rest, Handler, Stack, Config);
value(<<$n, Rest/binary>>, Handler, Stack, Config) ->
    nu(Rest, Handler, Stack, Config);
value(<<?negative, Rest/binary>>, Handler, Stack, Config) ->
    negative(Rest, Handler, [[$-]|Stack], Config);
value(<<?zero, Rest/binary>>, Handler, Stack, Config) ->
    zero(Rest, Handler, [[$0]|Stack], Config);
value(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_nonzero(S) ->
    integer(Rest, Handler, [[S]|Stack], Config);
value(<<?start_object, Rest/binary>>, Handler, Stack, Config) ->
    object(Rest, handle_event(start_object, Handler, Config), [key|Stack], Config);
value(<<?start_array, Rest/binary>>, Handler, Stack, Config) ->
    array(Rest, handle_event(start_array, Handler, Config), [array|Stack], Config);
value(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    value(Rest, Handler, Stack, Config);
value(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, [value|Stack], Config);
value(<<>>, Handler, Stack, Config) ->
    ?incomplete(value, <<>>, Handler, Stack, Config);
value(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


object(<<?doublequote, Rest/binary>>, Handler, Stack, Config) ->
    string(Rest, Handler, [string|Stack], Config);
object(<<?singlequote, Rest/binary>>, Handler, Stack, Config = #config{single_quoted_strings=true}) ->
    string(Rest, Handler, [single_quoted_string|Stack], Config);
object(<<?end_object, Rest/binary>>, Handler, [key|Stack], Config) ->
    maybe_done(Rest, handle_event(end_object, Handler, Config), Stack, Config);
object(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    object(Rest, Handler, Stack, Config);
object(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, [object|Stack], Config);
object(<<>>, Handler, Stack, Config) ->
    ?incomplete(object, <<>>, Handler, Stack, Config);
object(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


array(<<?end_array, Rest/binary>>, Handler, [array|Stack], Config) ->
    maybe_done(Rest, handle_event(end_array, Handler, Config), Stack, Config);
array(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    array(Rest, Handler, Stack, Config);
array(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, [array|Stack], Config);
array(<<>>, Handler, Stack, Config) ->
    ?incomplete(array, <<>>, Handler, Stack, Config);
array(Bin, Handler, Stack, Config) ->
    value(Bin, Handler, Stack, Config).


colon(<<?colon, Rest/binary>>, Handler, [key|Stack], Config) ->
    value(Rest, Handler, [object|Stack], Config);
colon(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    colon(Rest, Handler, Stack, Config);
colon(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, [colon|Stack], Config);
colon(<<>>, Handler, Stack, Config) ->
    ?incomplete(colon, <<>>, Handler, Stack, Config);
colon(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


key(<<?doublequote, Rest/binary>>, Handler, Stack, Config) ->
    string(Rest, Handler, [string|Stack], Config);
key(<<?singlequote, Rest/binary>>, Handler, Stack, Config = #config{single_quoted_strings=true}) ->
    string(Rest, Handler, [single_quoted_string|Stack], Config);
key(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    key(Rest, Handler, Stack, Config);
key(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, [key|Stack], Config);
key(<<>>, Handler, Stack, Config) ->
    ?incomplete(key, <<>>, Handler, Stack, Config);
key(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


string(Bin, Handler, Stack, Config) -> string(Bin, Handler, Stack, Config, <<>>).

string(Bin, Handler, [StringType|Stack], Config, Acc) ->
    Length = cut(Bin),
    <<String:Length/binary, Rest/binary>> = Bin,
    case Rest of
        <<?doublequote, Rem/binary>> when StringType == string ->
            finish_string(Rem, Handler, Stack, Config, <<Acc/binary, String/binary>>);
        <<?singlequote, Rem/binary>> when StringType == single_quoted_string ->
            finish_string(Rem, Handler, Stack, Config, <<Acc/binary, String/binary>>);
        <<Codepoint/utf8, Rem/binary>> when Codepoint == ?doublequote; Codepoint == ?singlequote ->
            string(Rem, Handler, [StringType|Stack], Config, <<Acc/binary, String/binary, Codepoint/utf8>>)
    end.


finish_string(Rest, Handler, [key|_] = Stack, Config, Acc) ->
    State = handle_event({key, Acc}, Handler, Config),
    colon(Rest, State, Stack, Config);
finish_string(Rest, Handler, Stack, Config, Acc) ->
    State = handle_event({string, Acc}, Handler, Config),
    maybe_done(Rest, State, Stack, Config).


cut(Bin) -> cut(Bin, 0).

cut(<<32, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<33, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<35, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<36, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<37, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<38, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<40, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<41, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<42, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<43, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<44, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<45, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<46, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<48, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<49, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<50, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<51, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<52, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<53, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<54, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<55, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<56, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<57, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<58, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<59, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<60, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<61, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<62, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<63, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<64, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<65, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<66, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<67, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<68, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<69, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<70, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<71, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<72, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<73, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<74, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<75, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<76, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<77, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<78, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<79, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<80, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<81, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<82, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<83, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<84, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<85, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<86, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<87, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<88, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<89, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<90, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<91, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<93, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<94, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<95, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<96, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<97, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<98, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<99, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<100, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<101, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<102, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<103, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<104, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<105, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<106, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<107, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<108, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<109, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<110, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<111, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<112, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<113, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<114, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<115, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<116, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<117, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<118, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<119, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<120, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<121, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<122, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<123, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<124, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<125, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<126, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<127, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<X/utf8, Rest/binary>>, N) when X >= 128, X < 16#0800 -> cut(Rest, N + 2);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#0800, X < 16#2028 -> cut(Rest, N + 3);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#202a, X < 16#d800 -> cut(Rest, N + 3);
cut(<<X/utf8, Rest/binary>>, N) when X > 16#dfff, X < 16#fdd0 -> cut(Rest, N + 3);
cut(<<X/utf8, Rest/binary>>, N) when X > 16#fdef, X < 16#fffe -> cut(Rest, N + 3);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#10000, X < 16#1fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#20000, X < 16#2fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#30000, X < 16#3fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#40000, X < 16#4fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#50000, X < 16#5fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#60000, X < 16#6fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#70000, X < 16#7fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#80000, X < 16#8fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#90000, X < 16#9fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#a0000, X < 16#afffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#b0000, X < 16#bfffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#c0000, X < 16#cfffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#d0000, X < 16#dfffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#e0000, X < 16#efffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#f0000, X < 16#ffffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#100000, X < 16#10fffe -> cut(Rest, N + 4);
cut(_, N) -> N.



%% like strings, numbers are collected in an intermediate accumulator before
%%   being emitted to the callback handler
negative(<<$0, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    zero(Rest, Handler, ["0" ++ Acc|Stack], Config);
negative(<<S, Rest/binary>>, Handler, [Acc|Stack], Config) when ?is_nonzero(S) ->
    integer(Rest, Handler, [[S] ++ Acc|Stack], Config);
negative(<<>>, Handler, Stack, Config) ->
    ?incomplete(negative, <<>>, Handler, Stack, Config);
negative(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


zero(<<?end_object, Rest/binary>>, Handler, [Acc, object|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_object], Handler, Config), Stack, Config);
zero(<<?end_array, Rest/binary>>, Handler, [Acc, array|Stack], Config) ->
    maybe_done(Rest, handle_event(end_array, handle_event(format_number(Acc), Handler, Config), Config), Stack, Config);
zero(<<?comma, Rest/binary>>, Handler, [Acc, object|Stack], Config) ->
    key(Rest, handle_event(format_number(Acc), Handler, Config), [key|Stack], Config);
zero(<<?comma, Rest/binary>>, Handler, [Acc, array|Stack], Config) ->
    value(Rest, handle_event(format_number(Acc), Handler, Config), [array|Stack], Config);
zero(<<?decimalpoint, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    initial_decimal(Rest, Handler, [{Acc, []}|Stack], Config);
zero(<<S, Rest/binary>>, Handler, [Acc|Stack], Config) when S =:= $e; S =:= $E ->
    e(Rest, Handler, [{Acc, [], []}|Stack], Config);
zero(<<S, Rest/binary>>, Handler, [Acc|Stack], Config) when ?is_whitespace(S) ->
    maybe_done(Rest, handle_event(format_number(Acc), Handler, Config), Stack, Config);
zero(<<?solidus, Rest/binary>>, Handler, [Acc|Stack], Config=#config{comments=true}) ->
    comment(Rest, handle_event(format_number(Acc), Handler, Config), [maybe_done|Stack], Config);
zero(<<>>, Handler, [Acc|[]], Config = #config{explicit_end=false}) ->
    maybe_done(<<>>, handle_event(format_number(Acc), Handler, Config), [], Config);
zero(<<>>, Handler, Stack, Config) ->
    ?incomplete(zero, <<>>, Handler, Stack, Config);
zero(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


integer(<<S, Rest/binary>>, Handler, [Acc|Stack], Config) when ?is_nonzero(S) ->
    integer(Rest, Handler, [[S] ++ Acc|Stack], Config);
integer(<<?end_object, Rest/binary>>, Handler, [Acc, object|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_object], Handler, Config), Stack, Config);
integer(<<?end_array, Rest/binary>>, Handler, [Acc, array|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_array], Handler, Config), Stack, Config);
integer(<<?comma, Rest/binary>>, Handler, [Acc, object|Stack], Config) ->
    key(Rest, handle_event(format_number(Acc), Handler, Config), [key|Stack], Config);
integer(<<?comma, Rest/binary>>, Handler, [Acc, array|Stack], Config) ->
    value(Rest, handle_event(format_number(Acc), Handler, Config), [array|Stack], Config);
integer(<<?decimalpoint, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    initial_decimal(Rest, Handler, [{Acc, []}|Stack], Config);
integer(<<?zero, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    integer(Rest, Handler, [[?zero] ++ Acc|Stack], Config);
integer(<<S, Rest/binary>>, Handler, [Acc|Stack], Config) when S =:= $e; S =:= $E ->
    e(Rest, Handler, [{Acc, [], []}|Stack], Config);
integer(<<S, Rest/binary>>, Handler, [Acc|Stack], Config) when ?is_whitespace(S) ->
    maybe_done(Rest, handle_event(format_number(Acc), Handler, Config), Stack, Config);
integer(<<?solidus, Rest/binary>>, Handler, [Acc|Stack], Config=#config{comments=true}) ->
    comment(Rest, handle_event(format_number(Acc), Handler, Config), [maybe_done|Stack], Config);
integer(<<>>, Handler, [Acc|[]], Config = #config{explicit_end=false}) ->
    maybe_done(<<>>, handle_event(format_number(Acc), Handler, Config), [], Config);
integer(<<>>, Handler, Stack, Config) ->
    ?incomplete(integer, <<>>, Handler, Stack, Config);
integer(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


initial_decimal(<<S, Rest/binary>>, Handler, [{Int, Frac}|Stack], Config) when S =:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Handler, [{Int, [S] ++ Frac}|Stack], Config);
initial_decimal(<<>>, Handler, Stack, Config) ->
    ?incomplete(initial_decimal, <<>>, Handler, Stack, Config);
initial_decimal(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


decimal(<<S, Rest/binary>>, Handler, [{Int, Frac}|Stack], Config)
        when S=:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Handler, [{Int, [S] ++ Frac}|Stack], Config);
decimal(<<?end_object, Rest/binary>>, Handler, [Acc, object|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_object], Handler, Config), Stack, Config);
decimal(<<?end_array, Rest/binary>>, Handler, [Acc, array|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_array], Handler, Config), Stack, Config);
decimal(<<?comma, Rest/binary>>, Handler, [Acc, object|Stack], Config) ->
    key(Rest, handle_event(format_number(Acc), Handler, Config), [key|Stack], Config);
decimal(<<?comma, Rest/binary>>, Handler, [Acc, array|Stack], Config) ->
    value(Rest, handle_event(format_number(Acc), Handler, Config), [array|Stack], Config);
decimal(<<S, Rest/binary>>, Handler, [{Int, Frac}|Stack], Config) when S =:= $e; S =:= $E ->
    e(Rest, Handler, [{Int, Frac, []}|Stack], Config);
decimal(<<S, Rest/binary>>, Handler, [Acc|Stack], Config) when ?is_whitespace(S) ->
    maybe_done(Rest, handle_event(format_number(Acc), Handler, Config), Stack, Config);
decimal(<<?solidus, Rest/binary>>, Handler, [Acc|Stack], Config=#config{comments=true}) ->
    comment(Rest, handle_event(format_number(Acc), Handler, Config), [maybe_done|Stack], Config);
decimal(<<>>, Handler, [Acc|[]], Config = #config{explicit_end=false}) ->
    maybe_done(<<>>, handle_event(format_number(Acc), Handler, Config), [], Config);
decimal(<<>>, Handler, Stack, Config) ->
    ?incomplete(decimal, <<>>, Handler, Stack, Config);
decimal(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


e(<<S, Rest/binary>>, Handler, [{Int, Frac, Exp}|Stack], Config) when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, [{Int, Frac, [S] ++ Exp}|Stack], Config);
e(<<S, Rest/binary>>, Handler, [{Int, Frac, Exp}|Stack], Config) when S =:= ?positive; S =:= ?negative ->
    ex(Rest, Handler, [{Int, Frac, [S] ++ Exp}|Stack], Config);
e(<<>>, Handler, Stack, Config) ->
    ?incomplete(e, <<>>, Handler, Stack, Config);
e(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


ex(<<S, Rest/binary>>, Handler, [{Int, Frac, Exp}|Stack], Config) when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, [{Int, Frac, [S] ++ Exp}|Stack], Config);
ex(<<>>, Handler, Stack, Config) ->
    ?incomplete(ex, <<>>, Handler, Stack, Config);
ex(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


exp(<<S, Rest/binary>>, Handler, [{Int, Frac, Exp}|Stack], Config) when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, [{Int, Frac, [S] ++ Exp}|Stack], Config);
exp(<<?end_object, Rest/binary>>, Handler, [Acc, object|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_object], Handler, Config), Stack, Config);
exp(<<?end_array, Rest/binary>>, Handler, [Acc, array|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_array], Handler, Config), Stack, Config);
exp(<<?comma, Rest/binary>>, Handler, [Acc, object|Stack], Config) ->
    key(Rest, handle_event(format_number(Acc), Handler, Config), [key|Stack], Config);
exp(<<?comma, Rest/binary>>, Handler, [Acc, array|Stack], Config) ->
    value(Rest, handle_event(format_number(Acc), Handler, Config), [array|Stack], Config);
exp(<<S, Rest/binary>>, Handler, [Acc|Stack], Config) when ?is_whitespace(S) ->
    maybe_done(Rest, handle_event(format_number(Acc), Handler, Config), Stack, Config);
exp(<<?solidus, Rest/binary>>, Handler, [Acc|Stack], Config=#config{comments=true}) ->
    comment(Rest, handle_event(format_number(Acc), Handler, Config), [maybe_done|Stack], Config);
exp(<<>>, Handler, [Acc|[]], Config = #config{explicit_end=false}) ->
    maybe_done(<<>>, handle_event(format_number(Acc), Handler, Config), [], Config);
exp(<<>>, Handler, Stack, Config) ->
    ?incomplete(exp, <<>>, Handler, Stack, Config);
exp(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


format_number(Int) when is_list(Int) ->
    {integer, list_to_integer(lists:reverse(Int))};
format_number({Int, Frac}) ->
    {float, list_to_float(lists:reverse(Frac ++ "." ++ Int))};
format_number({Int, [], Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e0." ++ Int))};
format_number({Int, Frac, Exp}) ->
    {float, list_to_float(lists:reverse(Exp ++ "e" ++ Frac ++ "." ++ Int))}.


tr(<<$r, Rest/binary>>, Handler, Stack, Config) ->
    tru(Rest, Handler, Stack, Config);
tr(<<>>, Handler, Stack, Config) ->
    ?incomplete(tr, <<>>, Handler, Stack, Config);
tr(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


tru(<<$u, Rest/binary>>, Handler, Stack, Config) ->
    true(Rest, Handler, Stack, Config);
tru(<<>>, Handler, Stack, Config) ->
    ?incomplete(tru, <<>>, Handler, Stack, Config);
tru(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


true(<<$e, Rest/binary>>, Handler, Stack, Config) ->
    maybe_done(Rest, handle_event({literal, true}, Handler, Config), Stack, Config);
true(<<>>, Handler, Stack, Config) ->
    ?incomplete(true, <<>>, Handler, Stack, Config);
true(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


fa(<<$a, Rest/binary>>, Handler, Stack, Config) ->
    fal(Rest, Handler, Stack, Config);
fa(<<>>, Handler, Stack, Config) ->
    ?incomplete(fa, <<>>, Handler, Stack, Config);
fa(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


fal(<<$l, Rest/binary>>, Handler, Stack, Config) ->
    fals(Rest, Handler, Stack, Config);
fal(<<>>, Handler, Stack, Config) ->
    ?incomplete(fal, <<>>, Handler, Stack, Config);
fal(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


fals(<<$s, Rest/binary>>, Handler, Stack, Config) ->
    false(Rest, Handler, Stack, Config);
fals(<<>>, Handler, Stack, Config) ->
    ?incomplete(fals, <<>>, Handler, Stack, Config);
fals(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


false(<<$e, Rest/binary>>, Handler, Stack, Config) ->
    maybe_done(Rest, handle_event({literal, false}, Handler, Config), Stack, Config);
false(<<>>, Handler, Stack, Config) ->
    ?incomplete(false, <<>>, Handler, Stack, Config);
false(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


nu(<<$u, Rest/binary>>, Handler, Stack, Config) ->
    nul(Rest, Handler, Stack, Config);
nu(<<>>, Handler, Stack, Config) ->
    ?incomplete(nu, <<>>, Handler, Stack, Config);
nu(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


nul(<<$l, Rest/binary>>, Handler, Stack, Config) ->
    null(Rest, Handler, Stack, Config);
nul(<<>>, Handler, Stack, Config) ->
    ?incomplete(nul, <<>>, Handler, Stack, Config);
nul(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


null(<<$l, Rest/binary>>, Handler, Stack, Config) ->
    maybe_done(Rest, handle_event({literal, null}, Handler, Config), Stack, Config);
null(<<>>, Handler, Stack, Config) ->
    ?incomplete(null, <<>>, Handler, Stack, Config);
null(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


comment(<<?solidus, Rest/binary>>, Handler, Stack, Config) ->
    single_comment(Rest, Handler, Stack, Config);
comment(<<?star, Rest/binary>>, Handler, Stack, Config) ->
    multi_comment(Rest, Handler, Stack, Config);
comment(<<>>, Handler, Stack, Config) ->
    ?incomplete(comment, <<>>, Handler, Stack, Config);
comment(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


single_comment(<<?newline, Rest/binary>>, Handler, Stack, Config) ->
    end_comment(Rest, Handler, Stack, Config);
single_comment(<<_/utf8, Rest/binary>>, Handler, Stack, Config) ->
    single_comment(Rest, Handler, Stack, Config);
single_comment(<<>>, Handler, [done], Config=#config{explicit_end=false}) ->
    end_comment(<<>>, Handler, [done], Config);
single_comment(<<>>, Handler, Stack, Config) ->
    ?incomplete(single_comment, <<>>, Handler, Stack, Config);
single_comment(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


multi_comment(<<?star, Rest/binary>>, Handler, Stack, Config) ->
    end_multi_comment(Rest, Handler, Stack, Config);
multi_comment(<<_S/utf8, Rest/binary>>, Handler, Stack, Config) ->
    multi_comment(Rest, Handler, Stack, Config);
multi_comment(<<>>, Handler, Stack, Config) ->
    ?incomplete(multi_comment, <<>>, Handler, Stack, Config);
multi_comment(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


end_multi_comment(<<?solidus, Rest/binary>>, Handler, Stack, Config) ->
    end_comment(Rest, Handler, Stack, Config);
end_multi_comment(<<_S/utf8, Rest/binary>>, Handler, Stack, Config) ->
    multi_comment(Rest, Handler, Stack, Config);
end_multi_comment(<<>>, Handler, Stack, Config) ->
    ?incomplete(end_multi_comment, <<>>, Handler, Stack, Config);
end_multi_comment(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


end_comment(Rest, Handler, [Resume|Stack], Config) ->
    case Resume of
        value -> value(Rest, Handler, Stack, Config)
        ; object -> object(Rest, Handler, Stack, Config)
        ; array -> array(Rest, Handler, Stack, Config)
        ; colon -> colon(Rest, Handler, Stack, Config)
        ; key -> key(Rest, Handler, Stack, Config)
        ; maybe_done -> maybe_done(Rest, Handler, Stack, Config)
        ; done -> done(Rest, Handler, Stack, Config)
    end.


maybe_done(Rest, Handler, [], Config) ->
    done(Rest, handle_event(end_json, Handler, Config), [], Config);
maybe_done(<<?end_object, Rest/binary>>, Handler, [object|Stack], Config) ->
    maybe_done(Rest, handle_event(end_object, Handler, Config), Stack, Config);
maybe_done(<<?end_array, Rest/binary>>, Handler, [array|Stack], Config) ->
    maybe_done(Rest, handle_event(end_array, Handler, Config), Stack, Config);
maybe_done(<<?comma, Rest/binary>>, Handler, [object|Stack], Config) ->
    key(Rest, Handler, [key|Stack], Config);
maybe_done(<<?comma, Rest/binary>>, Handler, [array|_] = Stack, Config) ->
    value(Rest, Handler, Stack, Config);
maybe_done(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    maybe_done(Rest, Handler, Stack, Config);
maybe_done(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, [maybe_done|Stack], Config);
maybe_done(<<>>, Handler, Stack, Config) when length(Stack) > 0 ->
    ?incomplete(maybe_done, <<>>, Handler, Stack, Config);
maybe_done(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


done(<<S, Rest/binary>>, Handler, [], Config) when ?is_whitespace(S) ->
    done(Rest, Handler, [], Config);
done(<<?solidus, Rest/binary>>, Handler, [], Config=#config{comments=true}) ->
    comment(Rest, Handler, [done], Config);
done(<<>>, {Handler, State}, [], Config = #config{explicit_end=true}) ->
    {incomplete, fun(Stream) when is_binary(Stream) ->
                done(<<Stream/binary>>, {Handler, State}, [], Config)
            ; (end_stream) -> State
        end
    };
done(<<>>, {_Handler, State}, [], _Config) -> State;
done(Bin, Handler, Stack, Config) -> ?error([Bin, Handler, Stack, Config]).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


decode_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                Events ++ [end_json],
                start(JSON, {jsx, []}, [], #config{})
            )
        } || {Title, JSON, _, Events} <- Data
    ].


%% all these numbers have different representation in erlang than in javascript and
%%  do not roundtrip like most integers/floats
special_number_test_() ->
    [
        {"-0", ?_assertEqual(
            [{integer, 0}, end_json],
            start(<<"-0">>, {jsx, []}, [], #config{})
        )},
        {"-0.0", ?_assertEqual(
            [{float, 0.0}, end_json],
            start(<<"-0.0">>, {jsx, []}, [], #config{})
        )},
        {"0e0", ?_assertEqual(
            [{float, 0.0}, end_json],
            start(<<"0e0">>, {jsx, []}, [], #config{})
        )},
        {"0e4", ?_assertEqual(
            [{float, 0.0}, end_json],
            start(<<"0e4">>, {jsx, []}, [], #config{})
        )},
        {"1e0", ?_assertEqual(
            [{float, 1.0}, end_json],
            start(<<"1e0">>, {jsx, []}, [], #config{})
        )},
        {"-1e0", ?_assertEqual(
            [{float, -1.0}, end_json],
            start(<<"-1e0">>, {jsx, []}, [], #config{})
        )},
        {"1e4", ?_assertEqual(
            [{float, 1.0e4}, end_json],
            start(<<"1e4">>, {jsx, []}, [], #config{})
        )}
    ].


decode(JSON, Config) ->
    try
        (decoder(jsx, [], Config))(JSON)
    catch
        error:badarg -> {error, badarg}
    end.


comments_test_() ->
    [
        {"preceeding // comment", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<"// comment ", ?newline, "[]">>, [comments])
        )},
        {"preceeding /**/ comment", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<"/* comment */[]">>, [comments])
        )},
        {"trailing // comment", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<"[]// comment", ?newline>>, [comments])
        )},
        {"trailing // comment (no newline)", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<"[]// comment">>, [comments])
        )},
        {"trailing /**/ comment", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<"[] /* comment */">>, [comments])
        )},
        {"// comment inside array", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<"[ // comment", ?newline, "]">>, [comments])
        )},
        {"/**/ comment inside array", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<"[ /* comment */ ]">>, [comments])
        )},
        {"// comment at beginning of array", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ // comment", ?newline, "true", ?newline, "]">>, [comments])
        )},
        {"/**/ comment at beginning of array", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ /* comment */ true ]">>, [comments])
        )},
        {"// comment at end of array", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ true // comment", ?newline, "]">>, [comments])
        )},
        {"/**/ comment at end of array", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ true /* comment */ ]">>, [comments])
        )},
        {"// comment midarray (post comma)", ?_assertEqual(
            [start_array, {literal, true}, {literal, false}, end_array, end_json],
            decode(<<"[ true, // comment", ?newline, "false ]">>, [comments])
        )},
        {"/**/ comment midarray (post comma)", ?_assertEqual(
            [start_array, {literal, true}, {literal, false}, end_array, end_json],
            decode(<<"[ true, /* comment */ false ]">>, [comments])
        )},
        {"// comment midarray (pre comma)", ?_assertEqual(
            [start_array, {literal, true}, {literal, false}, end_array, end_json],
            decode(<<"[ true// comment", ?newline, ", false ]">>, [comments])
        )},
        {"/**/ comment midarray (pre comma)", ?_assertEqual(
            [start_array, {literal, true}, {literal, false}, end_array, end_json],
            decode(<<"[ true/* comment */, false ]">>, [comments])
        )},
        {"// comment inside object", ?_assertEqual(
            [start_object, end_object, end_json],
            decode(<<"{ // comment", ?newline, "}">>, [comments])
        )},
        {"/**/ comment inside object", ?_assertEqual(
            [start_object, end_object, end_json],
            decode(<<"{ /* comment */ }">>, [comments])
        )},
        {"// comment at beginning of object", ?_assertEqual(
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            decode(<<"{ // comment", ?newline, " \"key\": true", ?newline, "}">>, [comments])
        )},
        {"/**/ comment at beginning of object", ?_assertEqual(
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            decode(<<"{ /* comment */ \"key\": true }">>, [comments])
        )},
        {"// comment at end of object", ?_assertEqual(
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            decode(<<"{ \"key\": true // comment", ?newline, "}">>, [comments])
        )},
        {"/**/ comment at end of object", ?_assertEqual(
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            decode(<<"{ \"key\": true /* comment */ }">>, [comments])
        )},
        {"// comment midobject (post comma)", ?_assertEqual(
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ],
            decode(<<"{ \"x\": true, // comment", ?newline, "\"y\": false }">>, [comments])
        )},
        {"/**/ comment midobject (post comma)", ?_assertEqual(
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ],
            decode(<<"{ \"x\": true, /* comment */", ?newline, "\"y\": false }">>, [comments])
        )},
        {"// comment midobject (pre comma)", ?_assertEqual(
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ],
            decode(<<"{ \"x\": true// comment", ?newline, ", \"y\": false }">>, [comments])
        )},
        {"/**/ comment midobject (pre comma)", ?_assertEqual(
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ],
            decode(<<"{ \"x\": true/* comment */", ?newline, ", \"y\": false }">>, [comments])
        )},
        {"// comment precolon", ?_assertEqual(
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            decode(<<"{ \"key\" // comment", ?newline, ": true }">>, [comments])
        )},
        {"/**/ comment precolon", ?_assertEqual(
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            decode(<<"{ \"key\"/* comment */: true }">>, [comments])
        )},
        {"// comment postcolon", ?_assertEqual(
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            decode(<<"{ \"key\": // comment", ?newline, " true }">>, [comments])
        )},
        {"/**/ comment postcolon", ?_assertEqual(
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            decode(<<"{ \"key\":/* comment */ true }">>, [comments])
        )},
        {"// comment terminating zero", ?_assertEqual(
            [start_array, {integer, 0}, end_array, end_json],
            decode(<<"[ 0// comment", ?newline, "]">>, [comments])
        )},
        {"// comment terminating integer", ?_assertEqual(
            [start_array, {integer, 1}, end_array, end_json],
            decode(<<"[ 1// comment", ?newline, "]">>, [comments])
        )},
        {"// comment terminating float", ?_assertEqual(
            [start_array, {float, 1.0}, end_array, end_json],
            decode(<<"[ 1.0// comment", ?newline, "]">>, [comments])
        )},
        {"// comment terminating exp", ?_assertEqual(
            [start_array, {float, 1.0e1}, end_array, end_json],
            decode(<<"[ 1e1// comment", ?newline, "]">>, [comments])
        )},
        {"/**/ comment terminating zero", ?_assertEqual(
            [start_array, {integer, 0}, end_array, end_json],
            decode(<<"[ 0/* comment */ ]">>, [comments])
        )},
        {"/**/ comment terminating integer", ?_assertEqual(
            [start_array, {integer, 1}, end_array, end_json],
            decode(<<"[ 1/* comment */ ]">>, [comments])
        )},
        {"/**/ comment terminating float", ?_assertEqual(
            [start_array, {float, 1.0}, end_array, end_json],
            decode(<<"[ 1.0/* comment */ ]">>, [comments])
        )},
        {"/**/ comment terminating exp", ?_assertEqual(
            [start_array, {float, 1.0e1}, end_array, end_json],
            decode(<<"[ 1e1/* comment */ ]">>, [comments])
        )},
        {"/**/ comment following /**/ comment", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[/* comment *//* comment */true]">>, [comments])
        )},
        {"/**/ comment following // comment", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[// comment", ?newline, "/* comment */true]">>, [comments])
        )},
        {"// comment following /**/ comment", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[/* comment */// comment", ?newline, "true]">>, [comments])
        )},
        {"// comment following // comment", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[// comment", ?newline, "// comment", ?newline, "true]">>, [comments])
        )}
    ].


codepoints() ->
    unicode:characters_to_binary(
        [32, 33]
        ++ lists:seq(35, 38)
        ++ lists:seq(40, 46)
        ++ lists:seq(48, 91)
        ++ lists:seq(93, 16#2027)
        ++ lists:seq(16#202a, 16#d7ff)
        ++ lists:seq(16#e000, 16#fdcf)
        ++ lists:seq(16#fdf0, 16#fffd)
    ).

extended_codepoints() ->
    unicode:characters_to_binary(
        lists:seq(16#10000, 16#1fffd) ++ [
            16#20000, 16#30000, 16#40000, 16#50000, 16#60000,
            16#70000, 16#80000, 16#90000, 16#a0000, 16#b0000,
            16#c0000, 16#d0000, 16#e0000, 16#f0000, 16#100000
        ]
    ).

reserved_space() -> [ to_fake_utf8(N) || N <- lists:seq(16#fdd0, 16#fdef) ].

surrogates() -> [ to_fake_utf8(N) || N <- lists:seq(16#d800, 16#dfff) ].

noncharacters() -> [ to_fake_utf8(N) || N <- lists:seq(16#fffe, 16#ffff) ].

extended_noncharacters() ->
    [ to_fake_utf8(N) || N <- [16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff]
        ++ [16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff]
        ++ [16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff]
        ++ [16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff]
        ++ [16#9fffe, 16#9ffff, 16#afffe, 16#affff]
        ++ [16#bfffe, 16#bffff, 16#cfffe, 16#cffff]
        ++ [16#dfffe, 16#dffff, 16#efffe, 16#effff]
        ++ [16#ffffe, 16#fffff, 16#10fffe, 16#10ffff]
    ].

%% erlang refuses to decode certain codepoints, so fake them all
to_fake_utf8(N) when N < 16#0080 -> <<34/utf8, N:8, 34/utf8>>;
to_fake_utf8(N) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<34/utf8, 2#110:3, Y:5, 2#10:2, X:6, 34/utf8>>;
to_fake_utf8(N) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<34/utf8, 2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6, 34/utf8>>;
to_fake_utf8(N) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<34/utf8, 2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6, 34/utf8>>.


clean_string_test_() ->
    [
        {"clean codepoints test", ?_assertEqual(
            [{string, codepoints()}, end_json],
            decode(<<34, (codepoints())/binary, 34>>, [])
        )},
        {"clean extended codepoints test", ?_assertEqual(
            [{string, extended_codepoints()}, end_json],
            decode(<<34, (extended_codepoints())/binary, 34>>, [])
        )}
    ].


single_quoted_string_test_() ->
    [
        {"single quoted string", ?_assertEqual(
            [{string, <<"hello world">>}, end_json],
            decode(<<39, "hello world", 39>>, [single_quoted_strings])
        )},
        {"single quoted string with embedded double quotes", ?_assertEqual(
            [{string, <<"quoth the raven, \"nevermore\"">>}, end_json],
            decode(<<39, "quoth the raven, \"nevermore\"", 39>>, [single_quoted_strings])
        )},
        {"string with embedded single quotes", ?_assertEqual(
            [{string, <<"quoth the raven, 'nevermore'">>}, end_json],
            decode(<<34, "quoth the raven, 'nevermore'", 34>>, [])
        )}
    ].


-endif.