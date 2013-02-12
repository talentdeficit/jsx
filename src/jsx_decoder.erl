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
    string(Rest, Handler, [?new_seq()|Stack], Config);
value(<<?singlequote, Rest/binary>>, Handler, Stack, Config = #config{single_quoted_strings=true}) ->
    string(Rest, Handler, [?new_seq(), single_quote|Stack], Config);
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
    string(Rest, Handler, [?new_seq()|Stack], Config);
object(<<?singlequote, Rest/binary>>, Handler, Stack, Config = #config{single_quoted_strings=true}) ->
    string(Rest, Handler, [?new_seq(), single_quote|Stack], Config);
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
    string(Rest, Handler, [?new_seq()|Stack], Config);
key(<<?singlequote, Rest/binary>>, Handler, Stack, Config = #config{single_quoted_strings=true}) ->
    string(Rest, Handler, [?new_seq(), single_quote|Stack], Config);
key(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    key(Rest, Handler, Stack, Config);
key(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, [key|Stack], Config);
key(<<>>, Handler, Stack, Config) ->
    ?incomplete(key, <<>>, Handler, Stack, Config);
key(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


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


%% explicitly whitelist ascii set for better efficiency (seriously, it's worth
%%  almost a 20% increase)
string(<<32, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 32)|Stack], Config);
string(<<33, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 33)|Stack], Config);
string(<<?doublequote, Rest/binary>>, Handler, S, Config) ->
    case S of
        [Acc, key|Stack] ->
            colon(Rest, handle_event({key, ?end_seq(Acc)}, Handler, Config), [key|Stack], Config);
        [_Acc, single_quote|_Stack] ->
            ?error([<<?doublequote, Rest/binary>>, Handler, S, Config]);
        [Acc|Stack] ->
            maybe_done(Rest, handle_event({string, ?end_seq(Acc)}, Handler, Config), Stack, Config)
    end;
string(<<35, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 35)|Stack], Config);
string(<<36, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 36)|Stack], Config);
string(<<37, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 37)|Stack], Config);
string(<<38, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 38)|Stack], Config);
string(<<?singlequote, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    case Config#config.single_quoted_strings of
        true ->
            case Stack of
                [single_quote, key|S] ->
                    colon(Rest, handle_event({key, ?end_seq(Acc)}, Handler, Config), [key|S], Config)
                ; [single_quote|S] ->
                    maybe_done(Rest, handle_event({string, ?end_seq(Acc)}, Handler, Config), S, Config)
                ; _ ->
                    string(Rest, Handler, [?acc_seq(Acc, maybe_replace(?singlequote, Config))|Stack], Config)
            end
        ; false ->
            string(Rest, Handler, [?acc_seq(Acc, ?singlequote)|Stack], Config)
    end;
string(<<40, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 40)|Stack], Config);
string(<<41, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 41)|Stack], Config);
string(<<42, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 42)|Stack], Config);
string(<<43, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 43)|Stack], Config);
string(<<44, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 44)|Stack], Config);
string(<<45, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 45)|Stack], Config);
string(<<46, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 46)|Stack], Config);
string(<<$/, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($/, Config))|Stack], Config);
string(<<48, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 48)|Stack], Config);
string(<<49, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 49)|Stack], Config);
string(<<50, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 50)|Stack], Config);
string(<<51, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 51)|Stack], Config);
string(<<52, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 52)|Stack], Config);
string(<<53, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 53)|Stack], Config);
string(<<54, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 54)|Stack], Config);
string(<<55, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 55)|Stack], Config);
string(<<56, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 56)|Stack], Config);
string(<<57, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 57)|Stack], Config);
string(<<58, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 58)|Stack], Config);
string(<<59, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 59)|Stack], Config);
string(<<60, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 60)|Stack], Config);
string(<<61, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 61)|Stack], Config);
string(<<62, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 62)|Stack], Config);
string(<<63, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 63)|Stack], Config);
string(<<64, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 64)|Stack], Config);
string(<<65, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 65)|Stack], Config);
string(<<66, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 66)|Stack], Config);
string(<<67, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 67)|Stack], Config);
string(<<68, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 68)|Stack], Config);
string(<<69, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 69)|Stack], Config);
string(<<70, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 70)|Stack], Config);
string(<<71, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 71)|Stack], Config);
string(<<72, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 72)|Stack], Config);
string(<<73, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 73)|Stack], Config);
string(<<74, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 74)|Stack], Config);
string(<<75, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 75)|Stack], Config);
string(<<76, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 76)|Stack], Config);
string(<<77, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 77)|Stack], Config);
string(<<78, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 78)|Stack], Config);
string(<<79, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 79)|Stack], Config);
string(<<80, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 80)|Stack], Config);
string(<<81, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 81)|Stack], Config);
string(<<82, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 82)|Stack], Config);
string(<<83, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 83)|Stack], Config);
string(<<84, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 84)|Stack], Config);
string(<<85, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 85)|Stack], Config);
string(<<86, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 86)|Stack], Config);
string(<<87, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 87)|Stack], Config);
string(<<88, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 88)|Stack], Config);
string(<<89, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 89)|Stack], Config);
string(<<90, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 90)|Stack], Config);
string(<<91, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 91)|Stack], Config);
string(<<?rsolidus/utf8, Rest/binary>>, Handler, Stack, Config) ->
    escape(Rest, Handler, Stack, Config);
string(<<93, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 93)|Stack], Config);
string(<<94, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 94)|Stack], Config);
string(<<95, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 95)|Stack], Config);
string(<<96, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 96)|Stack], Config);
string(<<97, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 97)|Stack], Config);
string(<<98, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 98)|Stack], Config);
string(<<99, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 99)|Stack], Config);
string(<<100, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 100)|Stack], Config);
string(<<101, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 101)|Stack], Config);
string(<<102, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 102)|Stack], Config);
string(<<103, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 103)|Stack], Config);
string(<<104, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 104)|Stack], Config);
string(<<105, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 105)|Stack], Config);
string(<<106, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 106)|Stack], Config);
string(<<107, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 107)|Stack], Config);
string(<<108, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 108)|Stack], Config);
string(<<109, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 109)|Stack], Config);
string(<<110, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 110)|Stack], Config);
string(<<111, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 111)|Stack], Config);
string(<<112, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 112)|Stack], Config);
string(<<113, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 113)|Stack], Config);
string(<<114, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 114)|Stack], Config);
string(<<115, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 115)|Stack], Config);
string(<<116, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 116)|Stack], Config);
string(<<117, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 117)|Stack], Config);
string(<<118, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 118)|Stack], Config);
string(<<119, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 119)|Stack], Config);
string(<<120, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 120)|Stack], Config);
string(<<121, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 121)|Stack], Config);
string(<<122, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 122)|Stack], Config);
string(<<123, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 123)|Stack], Config);
string(<<124, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 124)|Stack], Config);
string(<<125, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 125)|Stack], Config);
string(<<126, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 126)|Stack], Config);
string(<<127, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 127)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#20, X < 16#2028 ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X == 16#2028; X == 16#2029 ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace(X, Config))|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X > 16#2029, X < 16#d800 ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X > 16#dfff, X < 16#fdd0 ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X > 16#fdef, X < 16#fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#10000, X < 16#1fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#20000, X < 16#2fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#30000, X < 16#3fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#40000, X < 16#4fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#50000, X < 16#5fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#60000, X < 16#6fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#70000, X < 16#7fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#80000, X < 16#8fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#90000, X < 16#9fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#a0000, X < 16#afffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#b0000, X < 16#bfffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#c0000, X < 16#cfffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#d0000, X < 16#dfffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#e0000, X < 16#efffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#f0000, X < 16#ffffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 16#100000, X < 16#10fffe ->
    string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Config);
string(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    case Config#config.replaced_bad_utf8 of
        true -> noncharacter(<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config)
        ; false -> ?error([<<X/utf8, Rest/binary>>, Handler, [Acc|Stack], Config])
    end;
string(Bin, Handler, Stack, Config) ->
    case partial_utf(Bin) of
        true -> ?incomplete(string, Bin, Handler, Stack, Config)
        ; false ->
            case Config#config.replaced_bad_utf8 of
                true -> noncharacter(Bin, Handler, Stack, Config)
                ; false -> ?error([Bin, Handler, Stack, Config])
            end
    end.


%% we don't need to guard against partial utf here, because it's already taken
%%   care of in string
%% surrogates
noncharacter(<<237, X, _, Rest/binary>>, Handler, [Acc|Stack], Config) when X >= 160 ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Config);
%% u+fffe and u+ffff for R14BXX
noncharacter(<<239, 191, X, Rest/binary>>, Handler, [Acc|Stack], Config) when X == 190; X == 191 ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Config);
%% u+xfffe, u+xffff and other noncharacters
noncharacter(<<_/utf8, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Config);
%% overlong encodings and missing continuations of a 2 byte sequence
noncharacter(<<X, Rest/binary>>, Handler, Stack, Config) when X >= 192, X =< 223 ->
    strip_continuations(Rest, Handler, [1|Stack], Config);
%% overlong encodings and missing continuations of a 3 byte sequence
noncharacter(<<X, Rest/binary>>, Handler, Stack, Config) when X >= 224, X =< 239 ->
    strip_continuations(Rest, Handler, [2|Stack], Config);
%% overlong encodings and missing continuations of a 4 byte sequence
noncharacter(<<X, Rest/binary>>, Handler, Stack, Config) when X >= 240, X =< 247 ->
    strip_continuations(Rest, Handler, [3|Stack], Config);
%% unexpected bytes, including orphan continuations
noncharacter(<<_, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Config);
noncharacter(<<>>, Handler, Stack, Config) ->
    ?incomplete(noncharacter, <<>>, Handler, Stack, Config).


%% strips continuation bytes after bad utf bytes, guards against both too short
%%  and overlong sequences. N is the maximum number of bytes to strip
strip_continuations(Rest, Handler, [0, Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Config);
strip_continuations(<<X, Rest/binary>>, Handler,  [N|Stack], Config) when X >= 128, X =< 191 ->
    strip_continuations(Rest, Handler, [N - 1|Stack], Config);
%% incomplete
strip_continuations(<<>>, Handler, Stack, Config) ->
    ?incomplete(strip_continuations, <<>>, Handler, Stack, Config);
%% not a continuation byte, dispatch back to string
strip_continuations(Rest, Handler, [_, Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Config).


escape(<<$b, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($\b, Config))|Stack], Config);
escape(<<$f, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($\f, Config))|Stack], Config);
escape(<<$n, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($\n, Config))|Stack], Config);
escape(<<$r, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($\r, Config))|Stack], Config);
escape(<<$t, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($\t, Config))|Stack], Config);
escape(<<?rsolidus, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($\\, Config))|Stack], Config);
escape(<<?solidus, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($/, Config))|Stack], Config);
escape(<<?doublequote, Rest/binary>>, Handler, [Acc|Stack], Config) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace($\", Config))|Stack], Config);
escape(<<?singlequote, Rest/binary>>, Handler, [Acc|Stack], Config = #config{single_quoted_strings=true}) ->
    string(Rest, Handler, [?acc_seq(Acc, maybe_replace(?singlequote, Config))|Stack], Config);
escape(<<$u, Rest/binary>>, Handler, Stack, Config) ->
    escaped_unicode(Rest, Handler, Stack, Config);
escape(<<>>, Handler, Stack, Config) ->
    ?incomplete(escape, <<>>, Handler, Stack, Config);
escape(Bin, Handler, [Acc|Stack], Config=#config{ignored_bad_escapes=true}) ->
    string(Bin, Handler, [?acc_seq(Acc, ?rsolidus)|Stack], Config);
escape(Bin, Handler, Stack, Config) ->
    ?error([Bin, Handler, Stack, Config]).


%% this code is ugly and unfortunate, but so is json's handling of escaped
%%   unicode codepoint sequences.
escaped_unicode(<<A, B, C, D, Rest/binary>>, Handler, [Acc|Stack], Config)
        when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        %% high surrogate, dispatch to low surrogate
        X when X >= 16#d800, X =< 16#dbff ->
            low_surrogate(Rest, Handler, [X, Acc|Stack], Config)
        %% low surrogate, illegal in this position
        ; X when X >= 16#dc00, X =< 16#dfff ->
            case Config#config.replaced_bad_utf8 of
                true -> string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Config)
                ; false -> ?error([<<A, B, C, D, Rest/binary>>, Handler, [Acc|Stack], Config])
            end
        %% anything else
        ; X -> string(Rest, Handler, [?acc_seq(Acc, maybe_replace(X, Config))|Stack], Config)
    end;
escaped_unicode(Bin, Handler, Stack, Config) ->
    case is_partial_escape(Bin) of
        true -> ?incomplete(escaped_unicode, Bin, Handler, Stack, Config)
        ; false -> ?error([Bin, Handler, Stack, Config])
    end.


is_partial_escape(<<A, B, C>>) when ?is_hex(A), ?is_hex(B), ?is_hex(C) -> true;
is_partial_escape(<<A, B>>) when ?is_hex(A), ?is_hex(B) -> true;
is_partial_escape(<<A>>) when ?is_hex(A) -> true;
is_partial_escape(<<>>) -> true;
is_partial_escape(_) -> false.


low_surrogate(<<?rsolidus, $u, A, B, C, D, Rest/binary>>, Handler, [High, Acc|Stack], Config)
        when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >= 16#dc00, X =< 16#dfff ->
            Y = surrogate_to_codepoint(High, X),
            case (Y =< 16#d800 orelse Y >= 16#e000) of
                true -> string(Rest, Handler, [?acc_seq(Acc, Y)|Stack], Config)
                ; false ->
                    case Config#config.replaced_bad_utf8 of
                        true ->
                            string(Rest, Handler, [?acc_seq(Acc, 16#fffd, 16#fffd)|Stack], Config)
                        ; false ->
                            ?error([<<?rsolidus, $u, A, B, C, D, Rest/binary>>, Handler, [High, Acc|Stack], Config])
                    end
            end
        ; _ ->
            case Config#config.replaced_bad_utf8 of
                true -> string(Rest, Handler, [?acc_seq(Acc, 16#fffd, 16#fffd)|Stack], Config)
                ; false -> ?error([<<?rsolidus, $u, A, B, C, D, Rest/binary>>, Handler, [High, Acc|Stack], Config])
            end
    end;
low_surrogate(Bin, Handler, [High, Acc|Stack], Config) ->
    case is_partial_low(Bin) of
        true -> ?incomplete(low_surrogate, Bin, Handler, [High, Acc|Stack], Config)
        ; false ->
            case Config#config.replaced_bad_utf8 of
                true -> string(Bin, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Config)
                ; false -> ?error([Bin, Handler, [High, Acc|Stack], Config])
            end
    end.


is_partial_low(<<?rsolidus, $u, A, B, C>>) when ?is_hex(A), ?is_hex(B), ?is_hex(C) -> true;
is_partial_low(<<?rsolidus, $u, A, B>>) when ?is_hex(A), ?is_hex(B) -> true;
is_partial_low(<<?rsolidus, $u, A>>) when ?is_hex(A) -> true;
is_partial_low(<<?rsolidus, $u>>) -> true;
is_partial_low(<<?rsolidus>>) -> true;
is_partial_low(<<>>) -> true;
is_partial_low(_) -> false.


%% stole this from the unicode spec
surrogate_to_codepoint(High, Low) ->
    (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000.


maybe_replace(X, #config{dirty_strings=true}) when is_integer(X) -> [X];
maybe_replace($\b, #config{escaped_strings=true}) -> [$\\, $b];
maybe_replace($\t, #config{escaped_strings=true}) -> [$\\, $t];
maybe_replace($\n, #config{escaped_strings=true}) -> [$\\, $n];
maybe_replace($\f, #config{escaped_strings=true}) -> [$\\, $f];
maybe_replace($\r, #config{escaped_strings=true}) -> [$\\, $r];
maybe_replace($\", #config{escaped_strings=true}) -> [$\\, $\"];
maybe_replace($', Config=#config{escaped_strings=true}) ->
    case Config#config.single_quoted_strings of
        true -> [$\\, $']
        ; false -> [$']
    end;
maybe_replace($/, Config=#config{escaped_strings=true}) ->
    case Config#config.escaped_forward_slashes of
        true -> [$\\, $/]
        ; false -> [$/]
    end;
maybe_replace($\\, #config{escaped_strings=true}) -> [$\\, $\\];
maybe_replace(X, Config=#config{escaped_strings=true})  when X == 16#2028; X == 16#2029 ->
    case Config#config.unescaped_jsonp of
        true -> [X]
        ; false -> jsx_utils:json_escape_sequence(X)
    end;
maybe_replace(X, #config{escaped_strings=true}) when X < 32 ->
    jsx_utils:json_escape_sequence(X);
maybe_replace(X, _Config) -> [X].



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


xcode(Bin) -> xcode(Bin, []).

xcode(Bin, Config) ->
    Size = size(Bin),
    try jsx:to_term(<<34, Bin:Size/binary, 34>>, Config)
    catch error:badarg -> {error, badarg}
    end.


is_bad({error, badarg}) -> true;
is_bad(_) -> false.


bad_utf8_test_() ->
    [
        {"orphan continuation byte u+0080",
            ?_assert(is_bad(xcode(<<16#0080>>)))
        },
        {"orphan continuation byte u+0080 replaced",
            ?_assertEqual(xcode(<<16#0080>>, [replaced_bad_utf8]), <<16#fffd/utf8>>)
        },
        {"orphan continuation byte u+00bf",
            ?_assert(is_bad(xcode(<<16#00bf>>)))
        },
        {"orphan continuation byte u+00bf replaced",
            ?_assertEqual(xcode(<<16#00bf>>, [replaced_bad_utf8]), <<16#fffd/utf8>>)
        },
        {"2 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 2))/binary>>)))
        },
        {"2 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 2))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 2)
            )
        },
        {"3 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 3))/binary>>)))
        },
        {"3 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 3))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 3)
            )
        },
        {"4 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 4))/binary>>)))
        },
        {"4 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 4))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 4)
            )
        },
        {"5 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 5))/binary>>)))
        },
        {"5 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 5))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 5)
            )
        },
        {"6 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 6))/binary>>)))
        },
        {"6 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 6))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 6)
            )
        },
        {"all continuation bytes",
            ?_assert(is_bad(xcode(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>)))
        },
        {"all continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf)))
            )
        },
        {"lonely start byte",
            ?_assert(is_bad(xcode(<<16#00c0>>)))
        },
        {"lonely start byte replaced",
            ?_assertEqual(
                xcode(<<16#00c0>>, [replaced_bad_utf8]),
                <<16#fffd/utf8>>
            )
        },
        {"lonely start bytes (2 byte)",
            ?_assert(is_bad(xcode(<<16#00c0, 32, 16#00df>>)))
        },
        {"lonely start bytes (2 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00c0, 32, 16#00df>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"lonely start bytes (3 byte)",
            ?_assert(is_bad(xcode(<<16#00e0, 32, 16#00ef>>)))
        },
        {"lonely start bytes (3 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00e0, 32, 16#00ef>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"lonely start bytes (4 byte)",
            ?_assert(is_bad(xcode(<<16#00f0, 32, 16#00f7>>)))
        },
        {"lonely start bytes (4 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00f0, 32, 16#00f7>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"missing continuation byte (3 byte)",
            ?_assert(is_bad(xcode(<<224, 160, 32>>)))
        },
        {"missing continuation byte (3 byte) replaced",
            ?_assertEqual(
                xcode(<<224, 160, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"missing continuation byte (4 byte missing one)",
            ?_assert(is_bad(xcode(<<240, 144, 128, 32>>)))
        },
        {"missing continuation byte2 (4 byte missing one) replaced",
            ?_assertEqual(
                xcode(<<240, 144, 128, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"missing continuation byte (4 byte missing two)",
            ?_assert(is_bad(xcode(<<240, 144, 32>>)))
        },
        {"missing continuation byte2 (4 byte missing two) replaced",
            ?_assertEqual(
                xcode(<<240, 144, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (2 byte)",
            ?_assert(is_bad(xcode(<<16#c0, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (2 byte) replaced",
            ?_assertEqual(
                xcode(<<16#c0, 16#af, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (3 byte)",
            ?_assert(is_bad(xcode(<<16#e0, 16#80, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (3 byte) replaced",
            ?_assertEqual(
                xcode(<<16#e0, 16#80, 16#af, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (4 byte)",
            ?_assert(is_bad(xcode(<<16#f0, 16#80, 16#80, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (4 byte) replaced",
            ?_assertEqual(
                xcode(<<16#f0, 16#80, 16#80, 16#af, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 2 byte sequence",
            ?_assert(is_bad(xcode(<<16#c1, 16#bf, 32>>)))
        },
        {"highest overlong 2 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#c1, 16#bf, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 3 byte sequence",
            ?_assert(is_bad(xcode(<<16#e0, 16#9f, 16#bf, 32>>)))
        },
        {"highest overlong 3 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#e0, 16#9f, 16#bf, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 4 byte sequence",
            ?_assert(is_bad(xcode(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>)))
        },
        {"highest overlong 4 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        }
    ].


decode(JSON, Config) ->
    try
        (decoder(jsx, [], Config))(JSON)
    catch
        error:badarg -> {error, badarg}
    end.


ignored_bad_escapes_test_() ->
    [
        {"ignore unrecognized escape sequence", ?_assertEqual(
            decode(<<"[\"\\x25\"]">>, [ignored_bad_escapes]),
            [start_array, {string, <<"\\x25">>}, end_array, end_json]
        )}
    ].


comments_test_() ->
    [
        {"preceeding // comment", ?_assertEqual(
            decode(<<"// comment ", ?newline, "[]">>, [comments]),
            [start_array, end_array, end_json]
        )},
        {"preceeding /**/ comment", ?_assertEqual(
            decode(<<"/* comment */[]">>, [comments]),
            [start_array, end_array, end_json]
        )},
        {"trailing // comment", ?_assertEqual(
            decode(<<"[]// comment", ?newline>>, [comments]),
            [start_array, end_array, end_json]
        )},
        {"trailing // comment (no newline)", ?_assertEqual(
            decode(<<"[]// comment">>, [comments]),
            [start_array, end_array, end_json]
        )},
        {"trailing /**/ comment", ?_assertEqual(
            decode(<<"[] /* comment */">>, [comments]),
            [start_array, end_array, end_json]
        )},
        {"// comment inside array", ?_assertEqual(
            decode(<<"[ // comment", ?newline, "]">>, [comments]),
            [start_array, end_array, end_json]
        )},
        {"/**/ comment inside array", ?_assertEqual(
            decode(<<"[ /* comment */ ]">>, [comments]),
            [start_array, end_array, end_json]
        )},
        {"// comment at beginning of array", ?_assertEqual(
            decode(<<"[ // comment", ?newline, "true", ?newline, "]">>, [comments]),
            [start_array, {literal, true}, end_array, end_json]
        )},
        {"/**/ comment at beginning of array", ?_assertEqual(
            decode(<<"[ /* comment */ true ]">>, [comments]),
            [start_array, {literal, true}, end_array, end_json]
        )},
        {"// comment at end of array", ?_assertEqual(
            decode(<<"[ true // comment", ?newline, "]">>, [comments]),
            [start_array, {literal, true}, end_array, end_json]
        )},
        {"/**/ comment at end of array", ?_assertEqual(
            decode(<<"[ true /* comment */ ]">>, [comments]),
            [start_array, {literal, true}, end_array, end_json]
        )},
        {"// comment midarray (post comma)", ?_assertEqual(
            decode(<<"[ true, // comment", ?newline, "false ]">>, [comments]),
            [start_array, {literal, true}, {literal, false}, end_array, end_json]
        )},
        {"/**/ comment midarray (post comma)", ?_assertEqual(
            decode(<<"[ true, /* comment */ false ]">>, [comments]),
            [start_array, {literal, true}, {literal, false}, end_array, end_json]
        )},
        {"// comment midarray (pre comma)", ?_assertEqual(
            decode(<<"[ true// comment", ?newline, ", false ]">>, [comments]),
            [start_array, {literal, true}, {literal, false}, end_array, end_json]
        )},
        {"/**/ comment midarray (pre comma)", ?_assertEqual(
            decode(<<"[ true/* comment */, false ]">>, [comments]),
            [start_array, {literal, true}, {literal, false}, end_array, end_json]
        )},
        {"// comment inside object", ?_assertEqual(
            decode(<<"{ // comment", ?newline, "}">>, [comments]),
            [start_object, end_object, end_json]
        )},
        {"/**/ comment inside object", ?_assertEqual(
            decode(<<"{ /* comment */ }">>, [comments]),
            [start_object, end_object, end_json]
        )},
        {"// comment at beginning of object", ?_assertEqual(
            decode(<<"{ // comment", ?newline, " \"key\": true", ?newline, "}">>, [comments]),
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )},
        {"/**/ comment at beginning of object", ?_assertEqual(
            decode(<<"{ /* comment */ \"key\": true }">>, [comments]),
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )},
        {"// comment at end of object", ?_assertEqual(
            decode(<<"{ \"key\": true // comment", ?newline, "}">>, [comments]),
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )},
        {"/**/ comment at end of object", ?_assertEqual(
            decode(<<"{ \"key\": true /* comment */ }">>, [comments]),
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )},
        {"// comment midobject (post comma)", ?_assertEqual(
            decode(<<"{ \"x\": true, // comment", ?newline, "\"y\": false }">>, [comments]),
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ]
        )},
        {"/**/ comment midobject (post comma)", ?_assertEqual(
            decode(<<"{ \"x\": true, /* comment */", ?newline, "\"y\": false }">>, [comments]),
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ]
        )},
        {"// comment midobject (pre comma)", ?_assertEqual(
            decode(<<"{ \"x\": true// comment", ?newline, ", \"y\": false }">>, [comments]),
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ]
        )},
        {"/**/ comment midobject (pre comma)", ?_assertEqual(
            decode(<<"{ \"x\": true/* comment */", ?newline, ", \"y\": false }">>, [comments]),
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ]
        )},
        {"// comment precolon", ?_assertEqual(
            decode(<<"{ \"key\" // comment", ?newline, ": true }">>, [comments]),
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )},
        {"/**/ comment precolon", ?_assertEqual(
            decode(<<"{ \"key\"/* comment */: true }">>, [comments]),
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )},
        {"// comment postcolon", ?_assertEqual(
            decode(<<"{ \"key\": // comment", ?newline, " true }">>, [comments]),
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )},
        {"/**/ comment postcolon", ?_assertEqual(
            decode(<<"{ \"key\":/* comment */ true }">>, [comments]),
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )},
        {"// comment terminating zero", ?_assertEqual(
            decode(<<"[ 0// comment", ?newline, "]">>, [comments]),
            [start_array, {integer, 0}, end_array, end_json]
        )},
        {"// comment terminating integer", ?_assertEqual(
            decode(<<"[ 1// comment", ?newline, "]">>, [comments]),
            [start_array, {integer, 1}, end_array, end_json]
        )},
        {"// comment terminating float", ?_assertEqual(
            decode(<<"[ 1.0// comment", ?newline, "]">>, [comments]),
            [start_array, {float, 1.0}, end_array, end_json]
        )},
        {"// comment terminating exp", ?_assertEqual(
            decode(<<"[ 1e1// comment", ?newline, "]">>, [comments]),
            [start_array, {float, 1.0e1}, end_array, end_json]
        )},
        {"/**/ comment terminating zero", ?_assertEqual(
            decode(<<"[ 0/* comment */ ]">>, [comments]),
            [start_array, {integer, 0}, end_array, end_json]
        )},
        {"/**/ comment terminating integer", ?_assertEqual(
            decode(<<"[ 1/* comment */ ]">>, [comments]),
            [start_array, {integer, 1}, end_array, end_json]
        )},
        {"/**/ comment terminating float", ?_assertEqual(
            decode(<<"[ 1.0/* comment */ ]">>, [comments]),
            [start_array, {float, 1.0}, end_array, end_json]
        )},
        {"/**/ comment terminating exp", ?_assertEqual(
            decode(<<"[ 1e1/* comment */ ]">>, [comments]),
            [start_array, {float, 1.0e1}, end_array, end_json]
        )},
        {"/**/ comment following /**/ comment", ?_assertEqual(
            decode(<<"[/* comment *//* comment */true]">>, [comments]),
            [start_array, {literal, true}, end_array, end_json]
        )},
        {"/**/ comment following // comment", ?_assertEqual(
            decode(<<"[// comment", ?newline, "/* comment */true]">>, [comments]),
            [start_array, {literal, true}, end_array, end_json]
        )},
        {"// comment following /**/ comment", ?_assertEqual(
            decode(<<"[/* comment */// comment", ?newline, "true]">>, [comments]),
            [start_array, {literal, true}, end_array, end_json]
        )},
        {"// comment following // comment", ?_assertEqual(
            decode(<<"[// comment", ?newline, "// comment", ?newline, "true]">>, [comments]),
            [start_array, {literal, true}, end_array, end_json]
        )}
    ].


escaped_forward_slashes_test_() ->
    [
        {"escape forward slash test", ?_assertEqual(
            decode(<<"[ \" \/ \" ]">>, [escaped_forward_slashes]),
            [start_array, {string, <<" / ">>}, end_array, end_json]
        )}
    ].


escapes_test_() ->
    [
        {"backspace escape", ?_assertEqual(decode(<<"\"\\b\"">>, [escaped_strings]), [{string, <<"\\b">>}, end_json])},
        {"formfeed escape", ?_assertEqual(decode(<<"\"\\f\"">>, [escaped_strings]), [{string, <<"\\f">>}, end_json])},
        {"newline escape", ?_assertEqual(decode(<<"\"\\n\"">>, [escaped_strings]), [{string, <<"\\n">>}, end_json])},
        {"carriage return escape", ?_assertEqual(decode(<<"\"\\r\"">>, [escaped_strings]), [{string, <<"\\r">>}, end_json])},
        {"tab escape", ?_assertEqual(decode(<<"\"\\t\"">>, [escaped_strings]), [{string, <<"\\t">>}, end_json])},
        {"quote escape", ?_assertEqual(decode(<<"\"\\\"\"">>, [escaped_strings]), [{string, <<"\\\"">>}, end_json])},
        {"single quote escape", ?_assertEqual(decode(<<"\"'\"">>, [escaped_strings, single_quoted_strings]), [{string, <<"\\'">>}, end_json])},
        {"naked single quote escape", ?_assertEqual(decode(<<"'\\''">>, [escaped_strings, single_quoted_strings]), [{string, <<"\\'">>}, end_json])},
        {"no single quote escape", ?_assertEqual(decode(<<"\"'\"">>, [escaped_strings]), [{string, <<"'">>}, end_json])},
        {"forward slash escape", ?_assertEqual(decode(<<"\"/\"">>, [escaped_strings, escaped_forward_slashes]), [{string, <<"\\/">>}, end_json])},
        {"no forward slash escape", ?_assertEqual(decode(<<"\"/\"">>, [escaped_strings]), [{string, <<"/">>}, end_json])},
        {"back slash escape", ?_assertEqual(decode(<<"\"\\\\\"">>, [escaped_strings]), [{string, <<"\\\\">>}, end_json])},
        {"jsonp escape", ?_assertEqual(
            decode(<<$\", 16#2028/utf8, 16#2029/utf8, $\">>, [escaped_strings]),
            [{string, <<"\\u2028\\u2029">>}, end_json]
        )},
        {"no jsonp escape", ?_assertEqual(
            decode(<<$\", 16#2028/utf8, 16#2029/utf8, $\">>, [escaped_strings, unescaped_jsonp]),
            [{string, <<16#2028/utf8, 16#2029/utf8>>}, end_json]
        )},
        {"control escape", ?_assertEqual(decode(<<$\", "\\u0000"/utf8, $\">>, [escaped_strings]), [{string, <<"\\u0000">>}, end_json])},
        {"dirty strings", ?_assertEqual(decode(<<"\"\\n\"">>, [escaped_strings, dirty_strings]), [{string, <<"\n">>}, end_json])},
        {"ignore bad escapes", ?_assertEqual(decode(<<"\"\\x25\"">>, [escaped_strings, ignored_bad_escapes]), [{string, <<"\\x25">>}, end_json])}
    ].


noncharacters_test_() ->
    [
        {"noncharacters - badarg",
            ?_assert(check_bad(noncharacters()))
        },
        {"noncharacters - replaced",
            ?_assert(check_replaced(noncharacters()))
        }
    ].


extended_noncharacters_test_() ->
    [
        {"extended noncharacters - badarg",
            ?_assert(check_bad(extended_noncharacters()))
        },
        {"extended noncharacters - replaced",
            ?_assert(check_replaced(extended_noncharacters()))
        }
    ].


surrogates_test_() ->
    [
        {"surrogates - badarg",
            ?_assert(check_bad(surrogates()))
        },
        {"surrogates - replaced",
            ?_assert(check_replaced(surrogates()))
        }
    ].


control_test_() ->
    [
        {"control characters - badarg",
            ?_assert(check_bad(control_characters()))
        }
    ].


reserved_test_() ->
    [
        {"reserved noncharacters - badarg",
            ?_assert(check_bad(reserved_space()))
        },
        {"reserved noncharacters - replaced",
            ?_assert(check_replaced(reserved_space()))
        }
    ].


good_characters_test_() ->
    [
        {"acceptable codepoints",
            ?_assert(check_good(good()))
        },
        {"acceptable codepoints - escaped_strings",
            ?_assert(check_good(good(), [escaped_strings]))
        },
        {"acceptable codepoints - replaced_bad_utf8",
            ?_assert(check_good(good(), [replaced_bad_utf8]))
        },
        {"acceptable codepoints - escaped_strings + replaced_bad_utf8",
            ?_assert(check_good(good(), [escaped_strings, replaced_bad_utf8]))
        },
        {"acceptable extended",
            ?_assert(check_good(good_extended()))
        }
    ].


check_bad(List) ->
    [] == lists:dropwhile(fun({_, {error, badarg}}) -> true ; (_) -> false end,
        check(List, [], [])
    ).


check_replaced(List) ->
    [] == lists:dropwhile(fun({_, [{string, <<16#fffd/utf8>>}|_]}) -> true ; (_) -> false
        end,
        check(List, [replaced_bad_utf8], [])
    ).


check_good(List) -> check_good(List, []).

check_good(List, Config) ->
    [] == lists:dropwhile(fun({_, [{string, _}|_]}) -> true ; (_) -> false end,
        check(List, Config, [])
    ).


check([], _Config, Acc) -> Acc;
check([H|T], Config, Acc) ->
    R = decode(to_fake_utf(H, utf8), Config),
    check(T, Config, [{H, R}] ++ Acc).


noncharacters() -> lists:seq(16#fffe, 16#ffff).

extended_noncharacters() ->
    [
        16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff,
        16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff,
        16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff,
        16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff,
        16#9fffe, 16#9ffff, 16#afffe, 16#affff,
        16#bfffe, 16#bffff, 16#cfffe, 16#cffff,
        16#dfffe, 16#dffff, 16#efffe, 16#effff,
        16#ffffe, 16#fffff, 16#10fffe, 16#10ffff
    ].

surrogates() -> lists:seq(16#d800, 16#dfff).

control_characters() -> lists:seq(1, 31).

reserved_space() -> lists:seq(16#fdd0, 16#fdef).

good() ->
    [32, 33]
        ++ lists:seq(16#23, 16#5b)
        ++ lists:seq(16#5d, 16#d7ff)
        ++ lists:seq(16#e000, 16#fdcf)
        ++ lists:seq(16#fdf0, 16#fffd).

good_extended() ->
    lists:seq(16#10000, 16#1fffd) ++
    [
        16#10000, 16#20000, 16#30000, 16#40000, 16#50000,
        16#60000, 16#70000, 16#80000, 16#90000, 16#a0000,
        16#b0000, 16#c0000, 16#d0000, 16#e0000, 16#f0000
    ] ++ lists:seq(16#100000, 16#10fffd).


%% erlang refuses to decode certain codepoints, so fake them all
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


decode_test_() ->
    Data = jsx:universals()
        ++ jsx:decodeables(),
    [
        {
            Title, ?_assertEqual(
                Events ++ [end_json],
                start(JSON, {jsx, []}, [], #config{})
            )
        } || {Title, JSON, _, Events} <- Data
    ].

-endif.