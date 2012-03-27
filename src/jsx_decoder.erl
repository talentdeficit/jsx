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
    fun(JSON) ->
        value(
            JSON,
            {Handler, Handler:init(State)},
            [],
            jsx_utils:parse_opts(Opts)
        )
    end.



-include("jsx_opts.hrl").


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
-define(acc_seq(Seq, C, D), [C, D] ++ Seq).

-define(end_seq(Seq), unicode:characters_to_binary(lists:reverse(Seq))).


value(<<?doublequote, Rest/binary>>, Handler, Stack, Opts) ->
    string(Rest, Handler, [?new_seq()|Stack], Opts);
value(<<?singlequote, Rest/binary>>, Handler, Stack, Opts = #opts{single_quotes=true}) ->
    string(Rest, Handler, [?new_seq(), single_quote|Stack], Opts);
value(<<$t, Rest/binary>>, Handler, Stack, Opts) ->
    tr(Rest, Handler, Stack, Opts);
value(<<$f, Rest/binary>>, Handler, Stack, Opts) ->
    fa(Rest, Handler, Stack, Opts);
value(<<$n, Rest/binary>>, Handler, Stack, Opts) ->
    nu(Rest, Handler, Stack, Opts);
value(<<?negative, Rest/binary>>, Handler, Stack, Opts) ->
    negative(Rest, Handler, [[$-]|Stack], Opts);
value(<<?zero, Rest/binary>>, Handler, Stack, Opts) ->
    zero(Rest, Handler, [[$0]|Stack], Opts);
value(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Handler, [[S]|Stack], Opts);
value(<<?start_object, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    object(Rest, {Handler, Handler:handle_event(start_object, State)}, [key|Stack], Opts);
value(<<?start_array, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    array(Rest, {Handler, Handler:handle_event(start_array, State)}, [array|Stack], Opts);
value(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) -> 
    value(Rest, Handler, Stack, Opts);
value(<<?solidus, Rest/binary>>, Handler, Stack, Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> value(R, H, S, O) end,
    comment(Rest, Handler, [Resume|Stack], Opts);
value(<<>>, Handler, Stack, Opts) ->
    ?incomplete(value, <<>>, Handler, Stack, Opts);
value(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


object(<<?doublequote, Rest/binary>>, Handler, Stack, Opts) ->
    string(Rest, Handler, [?new_seq()|Stack], Opts);
object(<<?singlequote, Rest/binary>>, Handler, Stack, Opts = #opts{single_quotes=true}) ->
    string(Rest, Handler, [?new_seq(), single_quote|Stack], Opts);
object(<<?end_object, Rest/binary>>, {Handler, State}, [key|Stack], Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event(end_object, State)}, Stack, Opts);
object(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) ->
    object(Rest, Handler, Stack, Opts);
object(<<?solidus, Rest/binary>>, Handler, Stack, Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> object(R, H, S, O) end,
    comment(Rest, Handler, [Resume|Stack], Opts);
object(<<>>, Handler, Stack, Opts) ->
    ?incomplete(object, <<>>, Handler, Stack, Opts);
object(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).

   
array(<<?doublequote, Rest/binary>>, Handler, Stack, Opts) ->
    string(Rest, Handler, [?new_seq()|Stack], Opts);
array(<<?singlequote, Rest/binary>>, Handler, Stack, Opts = #opts{single_quotes=true}) ->
    string(Rest, Handler, [?new_seq(), single_quote|Stack], Opts);
array(<<$t, Rest/binary>>, Handler, Stack, Opts) ->
    tr(Rest, Handler, Stack, Opts);
array(<<$f, Rest/binary>>, Handler, Stack, Opts) ->
    fa(Rest, Handler, Stack, Opts);
array(<<$n, Rest/binary>>, Handler, Stack, Opts) ->
    nu(Rest, Handler, Stack, Opts);
array(<<?negative, Rest/binary>>, Handler, Stack, Opts) ->
    negative(Rest, Handler, [[$-]|Stack], Opts);
array(<<?zero, Rest/binary>>, Handler, Stack, Opts) ->
    zero(Rest, Handler, [[$0]|Stack], Opts);
array(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_nonzero(S) ->
    integer(Rest, Handler, [[S]|Stack], Opts);
array(<<?start_object, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    object(Rest, {Handler, Handler:handle_event(start_object, State)}, [key|Stack], Opts);
array(<<?start_array, Rest/binary>>, {Handler, State}, Stack, Opts) ->
    array(Rest, {Handler, Handler:handle_event(start_array, State)}, [array|Stack], Opts);
array(<<?end_array, Rest/binary>>, {Handler, State}, [array|Stack], Opts) ->
    maybe_done(Rest, {Handler, Handler:handle_event(end_array, State)}, Stack, Opts);
array(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) -> 
    array(Rest, Handler, Stack, Opts);
array(<<?solidus, Rest/binary>>, Handler, Stack, Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> array(R, H, S, O) end,
    comment(Rest, Handler, [Resume|Stack], Opts);
array(<<>>, Handler, Stack, Opts) ->
    ?incomplete(array, <<>>, Handler, Stack, Opts);
array(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


colon(<<?colon, Rest/binary>>, Handler, [key|Stack], Opts) ->
    value(Rest, Handler, [object|Stack], Opts);
colon(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) ->
    colon(Rest, Handler, Stack, Opts);
colon(<<?solidus, Rest/binary>>, Handler, Stack, Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> colon(R, H, S, O) end,
    comment(Rest, Handler, [Resume|Stack], Opts);
colon(<<>>, Handler, Stack, Opts) ->
    ?incomplete(colon, <<>>, Handler, Stack, Opts);
colon(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


key(<<?doublequote, Rest/binary>>, Handler, Stack, Opts) ->
    string(Rest, Handler, [?new_seq()|Stack], Opts);
key(<<?singlequote, Rest/binary>>, Handler, Stack, Opts = #opts{single_quotes=true}) ->
    string(Rest, Handler, [?new_seq(), single_quote|Stack], Opts);
key(<<S, Rest/binary>>, Handler, Stack, Opts) when ?is_whitespace(S) ->
    key(Rest, Handler, Stack, Opts);
key(<<?solidus, Rest/binary>>, Handler, Stack, Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> key(R, H, S, O) end,
    comment(Rest, Handler, [Resume|Stack], Opts);       
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


%% explicitly whitelist ascii set for better efficiency (seriously, it's worth
%%  almost a 20% increase)
string(<<32, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 32)|Stack], Opts);
string(<<33, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 33)|Stack], Opts);
string(<<?doublequote, Rest/binary>>, {Handler, State}, S, Opts) ->
    case S of
        [Acc, key|Stack] ->
            colon(Rest, {Handler, Handler:handle_event({key, ?end_seq(Acc)}, State)}, [key|Stack], Opts);
        [_Acc, single_quote|_Stack] ->
            ?error([<<?doublequote, Rest/binary>>, {Handler, State}, S, Opts]);
        [Acc|Stack] ->
            maybe_done(Rest, {Handler, Handler:handle_event({string, ?end_seq(Acc)}, State)}, Stack, Opts)
    end;
string(<<35, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 35)|Stack], Opts);
string(<<36, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 36)|Stack], Opts);
string(<<37, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 37)|Stack], Opts);
string(<<38, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 38)|Stack], Opts);
string(<<?singlequote, Rest/binary>>, {Handler, State}, S, Opts = #opts{single_quotes=true}) ->
    case S of
        [Acc, single_quote, key|Stack] ->
            colon(Rest, {Handler, Handler:handle_event({key, ?end_seq(Acc)}, State)}, [key|Stack], Opts);
        [Acc, single_quote|Stack] ->
            maybe_done(Rest, {Handler, Handler:handle_event({string, ?end_seq(Acc)}, State)}, Stack, Opts);
        [Acc|Stack] ->
            string(Rest, {Handler, State}, [?acc_seq(Acc, ?singlequote)|Stack], Opts)
    end;
string(<<40, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 40)|Stack], Opts);
string(<<41, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 41)|Stack], Opts);
string(<<42, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 42)|Stack], Opts);
string(<<43, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 43)|Stack], Opts);
string(<<44, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 44)|Stack], Opts);
string(<<45, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 45)|Stack], Opts);
string(<<46, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 46)|Stack], Opts);
string(<<47, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 47)|Stack], Opts);
string(<<48, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 48)|Stack], Opts);
string(<<49, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 49)|Stack], Opts);
string(<<50, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 50)|Stack], Opts);
string(<<51, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 51)|Stack], Opts);
string(<<52, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 52)|Stack], Opts);
string(<<53, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 53)|Stack], Opts);
string(<<54, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 54)|Stack], Opts);
string(<<55, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 55)|Stack], Opts);
string(<<56, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 56)|Stack], Opts);
string(<<57, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 57)|Stack], Opts);
string(<<58, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 58)|Stack], Opts);
string(<<59, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 59)|Stack], Opts);
string(<<60, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 60)|Stack], Opts);
string(<<61, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 61)|Stack], Opts);
string(<<62, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 62)|Stack], Opts);
string(<<63, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 63)|Stack], Opts);
string(<<64, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 64)|Stack], Opts);
string(<<65, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 65)|Stack], Opts);
string(<<66, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 66)|Stack], Opts);
string(<<67, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 67)|Stack], Opts);
string(<<68, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 68)|Stack], Opts);
string(<<69, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 69)|Stack], Opts);
string(<<70, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 70)|Stack], Opts);
string(<<71, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 71)|Stack], Opts);
string(<<72, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 72)|Stack], Opts);
string(<<73, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 73)|Stack], Opts);
string(<<74, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 74)|Stack], Opts);
string(<<75, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 75)|Stack], Opts);
string(<<76, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 76)|Stack], Opts);
string(<<77, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 77)|Stack], Opts);
string(<<78, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 78)|Stack], Opts);
string(<<79, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 79)|Stack], Opts);
string(<<80, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 80)|Stack], Opts);
string(<<81, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 81)|Stack], Opts);
string(<<82, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 82)|Stack], Opts);
string(<<83, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 83)|Stack], Opts);
string(<<84, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 84)|Stack], Opts);
string(<<85, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 85)|Stack], Opts);
string(<<86, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 86)|Stack], Opts);
string(<<87, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 87)|Stack], Opts);
string(<<88, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 88)|Stack], Opts);
string(<<89, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 89)|Stack], Opts);
string(<<90, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 90)|Stack], Opts);
string(<<91, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 91)|Stack], Opts);
string(<<?rsolidus/utf8, Rest/binary>>, Handler, Stack, Opts) ->
    escape(Rest, Handler, Stack, Opts);
string(<<93, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 93)|Stack], Opts);
string(<<94, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 94)|Stack], Opts);
string(<<95, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 95)|Stack], Opts);
string(<<96, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 96)|Stack], Opts);
string(<<97, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 97)|Stack], Opts);
string(<<98, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 98)|Stack], Opts);
string(<<99, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 99)|Stack], Opts);
string(<<100, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 100)|Stack], Opts);
string(<<101, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 101)|Stack], Opts);
string(<<102, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 102)|Stack], Opts);
string(<<103, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 103)|Stack], Opts);
string(<<104, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 104)|Stack], Opts);
string(<<105, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 105)|Stack], Opts);
string(<<106, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 106)|Stack], Opts);
string(<<107, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 107)|Stack], Opts);
string(<<108, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 108)|Stack], Opts);
string(<<109, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 109)|Stack], Opts);
string(<<110, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 110)|Stack], Opts);
string(<<111, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 111)|Stack], Opts);
string(<<112, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 112)|Stack], Opts);
string(<<113, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 113)|Stack], Opts);
string(<<114, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 114)|Stack], Opts);
string(<<115, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 115)|Stack], Opts);
string(<<116, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 116)|Stack], Opts);
string(<<117, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 117)|Stack], Opts);
string(<<118, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 118)|Stack], Opts);
string(<<119, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 119)|Stack], Opts);
string(<<120, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 120)|Stack], Opts);
string(<<121, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 121)|Stack], Opts);
string(<<122, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 122)|Stack], Opts);
string(<<123, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 123)|Stack], Opts);
string(<<124, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 124)|Stack], Opts);
string(<<125, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 125)|Stack], Opts);
string(<<126, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 126)|Stack], Opts);
string(<<127, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 127)|Stack], Opts);
string(<<S/utf8, Rest/binary>>, Handler, [Acc|Stack], Opts) when ?is_noncontrol(S) ->
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
%%   care of in string
%% surrogates
noncharacter(<<237, X, _, Rest/binary>>, Handler, [Acc|Stack], Opts) when X >= 160 ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts);
%% u+fffe and u+ffff for R14BXX
noncharacter(<<239, 191, X, Rest/binary>>, Handler, [Acc|Stack], Opts) when X == 190; X == 191 ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts);
%% bad utf8
noncharacter(<<_, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts).


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
escape(<<?rsolidus, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, $\\)|Stack], Opts);
escape(<<?solidus, Rest/binary>>, Handler, [Acc|Stack], Opts=#opts{escape_forward_slash=true}) ->
    string(Rest, Handler, [?acc_seq(Acc, $/)|Stack], Opts);
escape(<<?doublequote, Rest/binary>>, Handler, [Acc|Stack], Opts) ->
    string(Rest, Handler, [?acc_seq(Acc, $\")|Stack], Opts);
escape(<<?singlequote, Rest/binary>>, Handler, [Acc|Stack], Opts = #opts{single_quotes=true}) ->
    string(Rest, Handler, [?acc_seq(Acc, ?singlequote)|Stack], Opts);
escape(<<$u, Rest/binary>>, Handler, Stack, Opts) ->
    escaped_unicode(Rest, Handler, Stack, Opts);
escape(<<>>, Handler, Stack, Opts) ->
    ?incomplete(escape, <<>>, Handler, Stack, Opts);
escape(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


%% this code is ugly and unfortunate, but so is json's handling of escaped 
%%   unicode codepoint sequences.
escaped_unicode(<<A, B, C, D, Rest/binary>>, Handler, [Acc|Stack], Opts)
        when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        %% high surrogate, dispatch to low surrogate
        X when X >= 16#d800, X =< 16#dbff ->
            low_surrogate(Rest, Handler, [X, Acc|Stack], Opts)
        %% low surrogate, illegal in this position
        ; X when X >= 16#dc00, X =< 16#dfff ->
            case Opts#opts.loose_unicode of
                true -> string(Rest, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts)
                ; false -> ?error([<<A, B, C, D, Rest/binary>>, Handler, [Acc|Stack], Opts])
            end
        %% anything else
        ; X -> string(Rest, Handler, [?acc_seq(Acc, X)|Stack], Opts)
    end;
escaped_unicode(Bin, Handler, Stack, Opts) ->
    case is_partial_escape(Bin) of
        true -> ?incomplete(escaped_unicode, Bin, Handler, Stack, Opts)
        ; false -> ?error([Bin, Handler, Stack, Opts])
    end.


is_partial_escape(<<A, B, C>>) when ?is_hex(A), ?is_hex(B), ?is_hex(C) -> true;
is_partial_escape(<<A, B>>) when ?is_hex(A), ?is_hex(B) -> true;
is_partial_escape(<<A>>) when ?is_hex(A) -> true;
is_partial_escape(<<>>) -> true;
is_partial_escape(_) -> false.


low_surrogate(<<?rsolidus, $u, A, B, C, D, Rest/binary>>, Handler, [High, Acc|Stack], Opts)
        when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        X when X >= 16#dc00, X =< 16#dfff -> 
            Y = surrogate_to_codepoint(High, X),
            case (Y =< 16#d800 orelse Y >= 16#e000) of
                true -> string(Rest, Handler, [?acc_seq(Acc, Y)|Stack], Opts)
                ; false ->
                    case Opts#opts.loose_unicode of
                        true ->
                            string(Rest, Handler, [?acc_seq(Acc, 16#fffd, 16#fffd)|Stack], Opts)
                        ; false ->
                            ?error([<<?rsolidus, $u, A, B, C, D, Rest/binary>>, Handler, [High, Acc|Stack], Opts])
                    end
            end
        ; _ ->
            case Opts#opts.loose_unicode of
                true -> string(Rest, Handler, [?acc_seq(Acc, 16#fffd, 16#fffd)|Stack], Opts)
                ; false -> ?error([<<?rsolidus, $u, A, B, C, D, Rest/binary>>, Handler, [High, Acc|Stack], Opts])
            end
    end;
low_surrogate(Bin, Handler, [High, Acc|Stack], Opts) ->
    case is_partial_low(Bin) of
        true -> ?incomplete(low_surrogate, Bin, Handler, [High, Acc|Stack], Opts)
        ; false ->
            case Opts#opts.loose_unicode of
                true -> string(Bin, Handler, [?acc_seq(Acc, 16#fffd)|Stack], Opts)
                ; false -> ?error([Bin, Handler, [High, Acc|Stack], Opts])
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
zero(<<?solidus, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> maybe_done(R, H, S, O) end,
    comment(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [Resume|Stack], Opts);  
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
integer(<<?solidus, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> maybe_done(R, H, S, O) end,
    comment(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [Resume|Stack], Opts); 
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
decimal(<<?solidus, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> maybe_done(R, H, S, O) end,
    comment(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [Resume|Stack], Opts); 
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
exp(<<?solidus, Rest/binary>>, {Handler, State}, [Acc|Stack], Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> maybe_done(R, H, S, O) end,
    comment(Rest, {Handler, Handler:handle_event(format_number(Acc), State)}, [Resume|Stack], Opts); 
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


comment(<<?solidus, Rest/binary>>, Handler, Stack, Opts) ->
    single_comment(Rest, Handler, Stack, Opts);
comment(<<?star, Rest/binary>>, Handler, Stack, Opts) ->
    multi_comment(Rest, Handler, Stack, Opts);
comment(<<>>, Handler, Stack, Opts) ->
    ?incomplete(comment, <<>>, Handler, Stack, Opts);
comment(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


single_comment(<<?newline, Rest/binary>>, Handler, [Resume|Stack], Opts) ->
    Resume(Rest, Handler, Stack, Opts);
single_comment(<<>>, Handler, [Resume|Stack], Opts) ->
    Resume(<<>>, Handler, Stack, Opts);
single_comment(<<_S/utf8, Rest/binary>>, Handler, Stack, Opts) ->
    single_comment(Rest, Handler, Stack, Opts);
single_comment(<<>>, Handler, Stack, Opts) ->
    ?incomplete(single_comment, <<>>, Handler, Stack, Opts);
single_comment(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


multi_comment(<<?star, Rest/binary>>, Handler, Stack, Opts) ->
    end_multi_comment(Rest, Handler, Stack, Opts);
multi_comment(<<_S/utf8, Rest/binary>>, Handler, Stack, Opts) ->
    multi_comment(Rest, Handler, Stack, Opts);
multi_comment(<<>>, Handler, Stack, Opts) ->
    ?incomplete(multi_comment, <<>>, Handler, Stack, Opts);
multi_comment(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


end_multi_comment(<<?solidus, Rest/binary>>, Handler, [Resume|Stack], Opts) ->
    Resume(Rest, Handler, Stack, Opts);
end_multi_comment(<<_S/utf8, Rest/binary>>, Handler, Stack, Opts) ->
    multi_comment(Rest, Handler, Stack, Opts);
end_multi_comment(<<>>, Handler, Stack, Opts) ->
    ?incomplete(end_multi_comment, <<>>, Handler, Stack, Opts);
end_multi_comment(Bin, Handler, Stack, Opts) ->
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
maybe_done(<<?solidus, Rest/binary>>, Handler, Stack, Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> maybe_done(R, H, S, O) end,
    comment(Rest, Handler, [Resume|Stack], Opts);
maybe_done(<<>>, Handler, Stack, Opts) when length(Stack) > 0 -> 
    ?incomplete(maybe_done, <<>>, Handler, Stack, Opts);
maybe_done(Rest, {Handler, State}, [], Opts) ->
    done(Rest, {Handler, Handler:handle_event(end_json, State)}, [], Opts);
maybe_done(Bin, Handler, Stack, Opts) ->
    ?error([Bin, Handler, Stack, Opts]).


done(<<S, Rest/binary>>, Handler, [], Opts) when ?is_whitespace(S) ->
    done(Rest, Handler, [], Opts);
done(<<?solidus, Rest/binary>>, Handler, [], Opts=#opts{comments=true}) ->
    Resume = fun(R, H, S, O) -> done(R, H, S, O) end,
    comment(Rest, Handler, [Resume], Opts);
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
        )}
    ].

escape_forward_slash_test_() ->
    [
        {"escape forward slash test", ?_assertEqual(
            decode(<<"[ \" \/ \" ]">>, [escape_forward_slash]),
            [start_array, {string, <<" / ">>}, end_array, end_json]
        )}
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
    
good_characters_test_() ->
    [
        {"acceptable codepoints",
            ?_assertEqual(check_good(good()), [])
        },
        {"acceptable extended",
            ?_assertEqual(check_good(good_extended()), [])
        }
    ].

malformed_test_() ->
    [
        {"malformed codepoint with 1 byte",
            ?_assertEqual({error, badjson}, decode(<<128>>))
        },
        {"malformed codepoint with 2 bytes",
            ?_assertEqual({error, badjson}, decode(<<128, 192>>))
        },
        {"malformed codepoint with 3 bytes",
            ?_assertEqual({error, badjson}, decode(<<128, 192, 192>>))
        },
        {"malformed codepoint with 4 bytes",
            ?_assertEqual({error, badjson}, decode(<<128, 192, 192, 192>>))
        }
    ].

malformed_replaced_test_() ->
    F = <<16#fffd/utf8>>,
    [
        {"malformed codepoint with 1 byte",
            ?_assertEqual(
                [{string, <<F/binary>>}, end_json],
                decode(<<34, 128, 34>>, [loose_unicode])
            )
        },
        {"malformed codepoint with 2 bytes",
            ?_assertEqual(
                [{string, <<F/binary, F/binary>>}, end_json],
                decode(<<34, 128, 192, 34>>, [loose_unicode])
            )
        },
        {"malformed codepoint with 3 bytes",
            ?_assertEqual(
                [{string, <<F/binary, F/binary, F/binary>>}, end_json],
                decode(<<34, 128, 192, 192, 34>>, [loose_unicode])
            )
        },
        {"malformed codepoint with 4 bytes",
            ?_assertEqual(
                [{string, <<F/binary, F/binary, F/binary, F/binary>>}, end_json],
                decode(<<34, 128, 192, 192, 192, 34>>, [loose_unicode])
            )
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


decode(JSON) -> decode(JSON, []).

decode(JSON, Opts) ->
    try
        (decoder(jsx, [], Opts))(JSON)
    catch
        error:badarg -> {error, badjson}
    end.


surrogates() -> lists:seq(16#d800, 16#dfff).

control_characters() -> lists:seq(1, 31).

good() -> [32, 33] ++ lists:seq(16#23, 16#5b) ++ lists:seq(16#5d, 16#d7ff) ++ lists:seq(16#e000, 16#ffff).
            
good_extended() -> lists:seq(16#100000, 16#10ffff).

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