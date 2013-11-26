%% The MIT License

%% Copyright (c) 2010-2013 alisdair sullivan <alisdairsullivan@yahoo.ca>

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

%% inline sequence accumulation, handle_event, format_number and maybe_replace
-compile({inline, [new_seq/0, new_seq/1, acc_seq/2, end_seq/1, end_seq/2]}).
-compile({inline, [handle_event/3]}).
-compile({inline, [format_number/1]}).
-compile({inline, [maybe_replace/2]}).
-compile({inline, [doublequote/5, singlequote/5]}).

-export([decoder/3, resume/6]).


-spec decoder(Handler::module(), State::any(), Config::list()) -> jsx:decoder().

decoder(Handler, State, Config) ->
    fun(JSON) -> start(JSON, {Handler, Handler:init(State)}, [], jsx_config:parse_config(Config)) end.


%% resume allows continuation from interrupted decoding without having to explicitly export
%%  all states
-spec resume(
        Rest::binary(),
        State::atom(),
        Handler::{atom(), any()},
        Acc::any(),
        Stack::list(atom()),
        Config::jsx:config()
    ) -> jsx:decoder() | {incomplete, jsx:decoder()}.

resume(Rest, State, Handler, Acc, Stack, Config) ->
    case State of
        start -> start(Rest, Handler, Stack, Config);
        value -> value(Rest, Handler, Stack, Config);
        object -> object(Rest, Handler, Stack, Config);
        array -> array(Rest, Handler, Stack, Config);
        colon -> colon(Rest, Handler, Stack, Config);
        key -> key(Rest, Handler, Stack, Config);
        string -> string(Rest, Handler, Acc, Stack, Config);
        integer -> integer(Rest, Handler, Acc, Stack, Config);
        decimal -> decimal(Rest, Handler, Acc, Stack, Config);
        exp -> exp(Rest, Handler, Acc, Stack, Config);
        true -> true(Rest, Handler, Stack, Config);
        false -> false(Rest, Handler, Stack, Config);
        null -> null(Rest, Handler, Stack, Config);
        comment -> comment(Rest, Handler, Acc, Stack, Config);
        maybe_done -> maybe_done(Rest, Handler, Stack, Config);
        done -> done(Rest, Handler, Stack, Config)
    end.


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
-define(rsolidus, 16#5C).
-define(solidus, 16#2F).

%% math
-define(zero, 16#30).
-define(decimalpoint, 16#2E).
-define(negative, 16#2D).
-define(positive, 16#2B).

%% comments
-define(star, 16#2A).


%% some useful guards
-define(is_hex(Symbol),
    (Symbol >= $a andalso Symbol =< $f) orelse
    (Symbol >= $A andalso Symbol =< $F) orelse
    (Symbol >= $0 andalso Symbol =< $9)
).

-define(is_nonzero(Symbol),
    Symbol >= $1 andalso Symbol =< $9
).

-define(is_whitespace(Symbol),
    Symbol =:= ?space; Symbol =:= ?tab; Symbol =:= ?cr; Symbol =:= ?newline
).


%% error is a macro so the stack trace shows the error site when possible
-ifndef(error).
-define(error(State, Bin, Handler, Acc, Stack, Config),
    case Config#config.error_handler of
        false -> erlang:error(badarg);
        F -> F(Bin, {decoder, State, Handler, Acc, Stack}, jsx_config:config_to_list(Config))
    end
).
-define(error(State, Bin, Handler, Stack, Config),
    ?error(State, Bin, Handler, null, Stack, Config)
).
-endif.


incomplete(State, Rest, Handler, Stack, Config) ->
    incomplete(State, Rest, Handler, unused, Stack, Config).

incomplete(State, Rest, Handler, Acc, Stack, Config=#config{incomplete_handler=false}) ->
    {incomplete, fun(Stream) when is_binary(Stream) ->
                resume(<<Rest/binary, Stream/binary>>, State, Handler, Acc, Stack, Config);
            (end_stream) ->
                case resume(<<Rest/binary, ?space/utf8>>, State, Handler, Acc, Stack, Config#config{explicit_end=false}) of
                    {incomplete, _} -> ?error(State, Rest, Handler, Acc, Stack, Config);
                    Else -> Else
                end
        end
    };
incomplete(State, Rest, Handler, Acc, Stack, Config=#config{incomplete_handler=F}) ->
    F(Rest, {decoder, State, Handler, Acc, Stack}, jsx_config:config_to_list(Config)).


%% lists are benchmarked to be faster (tho higher in memory usage) than binaries
new_seq() -> [].
new_seq(C) -> [C].

acc_seq(Seq, C) when is_list(C) -> lists:reverse(C) ++ Seq;
acc_seq(Seq, C) -> [C] ++ Seq.

end_seq(Seq) -> unicode:characters_to_binary(lists:reverse(Seq)).

end_seq(Seq, #config{dirty_strings=true}) -> list_to_binary(lists:reverse(Seq));
end_seq(Seq, _) -> end_seq(Seq).


handle_event([], Handler, _Config) -> Handler;
handle_event([Event|Rest], Handler, Config) ->
    handle_event(Rest, handle_event(Event, Handler, Config), Config);
handle_event(Event, {Handler, State}, _Config) ->
    {Handler, Handler:handle_event(Event, State)}.


start(<<16#ef, 16#bb, 16#bf, Rest/binary>>, Handler, Stack, Config) ->
    value(Rest, Handler, Stack, Config);
start(<<16#ef, 16#bb>>, Handler, Stack, Config) ->
    incomplete(start, <<16#ef, 16#bb>>, Handler, Stack, Config);
start(<<16#ef>>, Handler, Stack, Config) ->
    incomplete(start, <<16#ef>>, Handler, Stack, Config);
start(<<>>, Handler, Stack, Config) ->
    incomplete(start, <<>>, Handler, Stack, Config);
start(Bin, Handler, Stack, Config) ->
    value(Bin, Handler, Stack, Config).


value(<<?doublequote, Rest/binary>>, Handler, Stack, Config) ->
    string(Rest, Handler, new_seq(), Stack, Config);
value(<<?singlequote, Rest/binary>>, Handler, Stack, Config=#config{single_quoted_strings=true}) ->
    string(Rest, Handler, new_seq(), [singlequote|Stack], Config);
value(<<$t, Rest/binary>>, Handler, Stack, Config) ->
    true(Rest, Handler, Stack, Config);
value(<<$f, Rest/binary>>, Handler, Stack, Config) ->
    false(Rest, Handler, Stack, Config);
value(<<$n, Rest/binary>>, Handler, Stack, Config) ->
    null(Rest, Handler, Stack, Config);
value(<<?negative, Rest/binary>>, Handler, Stack, Config) ->
    negative(Rest, Handler, new_seq($-), Stack, Config);
value(<<?zero, Rest/binary>>, Handler, Stack, Config) ->
    zero(Rest, Handler, new_seq($0), Stack, Config);
value(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_nonzero(S) ->
    integer(Rest, Handler, new_seq(S), Stack, Config);
value(<<?start_object, Rest/binary>>, Handler, Stack, Config) ->
    object(Rest, handle_event(start_object, Handler, Config), [key|Stack], Config);
value(<<?start_array, Rest/binary>>, Handler, Stack, Config) ->
    array(Rest, handle_event(start_array, Handler, Config), [array|Stack], Config);
value(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    value(Rest, Handler, Stack, Config);
value(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, value, [comment|Stack], Config);
value(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, value, [multicomment|Stack], Config);
value(<<?solidus>>, Handler, Stack, Config=#config{comments=true}) ->
    incomplete(value, <<?solidus>>, Handler, Stack, Config);
value(<<>>, Handler, Stack, Config) ->
    incomplete(value, <<>>, Handler, Stack, Config);
value(Bin, Handler, Stack, Config) ->
    ?error(value, Bin, Handler, Stack, Config).


object(<<?doublequote, Rest/binary>>, Handler, Stack, Config) ->
    string(Rest, Handler, new_seq(), Stack, Config);
object(<<?singlequote, Rest/binary>>, Handler, Stack, Config=#config{single_quoted_strings=true}) ->
    string(Rest, Handler, new_seq(), [singlequote|Stack], Config);
object(<<?end_object, Rest/binary>>, Handler, [key|Stack], Config) ->
    maybe_done(Rest, handle_event(end_object, Handler, Config), Stack, Config);
object(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    object(Rest, Handler, Stack, Config);
object(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, object, [comment|Stack], Config);
object(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, object, [multicomment|Stack], Config);
object(<<?solidus>>, Handler, Stack, Config=#config{comments=true}) ->
    incomplete(object, <<?solidus>>, Handler, Stack, Config);
object(<<>>, Handler, Stack, Config) ->
    incomplete(object, <<>>, Handler, Stack, Config);
object(Bin, Handler, Stack, Config) ->
    ?error(object, Bin, Handler, Stack, Config).


array(<<?end_array, Rest/binary>>, Handler, [array|Stack], Config) ->
    maybe_done(Rest, handle_event(end_array, Handler, Config), Stack, Config);
array(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    array(Rest, Handler, Stack, Config);
array(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, array, [comment|Stack], Config);
array(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, array, [multicomment|Stack], Config);
array(<<?solidus>>, Handler, Stack, Config=#config{comments=true}) ->
    incomplete(array, <<?solidus>>, Handler, Stack, Config);
array(<<>>, Handler, Stack, Config) ->
    incomplete(array, <<>>, Handler, Stack, Config);
array(Bin, Handler, Stack, Config) ->
    value(Bin, Handler, Stack, Config).


colon(<<?colon, Rest/binary>>, Handler, [key|Stack], Config) ->
    value(Rest, Handler, [object|Stack], Config);
colon(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    colon(Rest, Handler, Stack, Config);
colon(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, colon, [comment|Stack], Config);
colon(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, colon, [multicomment|Stack], Config);
colon(<<?solidus>>, Handler, Stack, Config=#config{comments=true}) ->
    incomplete(colon, <<?solidus>>, Handler, Stack, Config);
colon(<<>>, Handler, Stack, Config) ->
    incomplete(colon, <<>>, Handler, Stack, Config);
colon(Bin, Handler, Stack, Config) ->
    ?error(colon, Bin, Handler, Stack, Config).


key(<<?doublequote, Rest/binary>>, Handler, Stack, Config) ->
    string(Rest, Handler, new_seq(), Stack, Config);
key(<<?singlequote, Rest/binary>>, Handler, Stack, Config=#config{single_quoted_strings=true}) ->
    string(Rest, Handler, new_seq(), [singlequote|Stack], Config);
key(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    key(Rest, Handler, Stack, Config);
key(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, key, [comment|Stack], Config);
key(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, key, [multicomment|Stack], Config);
key(<<?solidus>>, Handler, Stack, Config=#config{comments=true}) ->
    incomplete(key, <<?solidus>>, Handler, Stack, Config);
key(<<>>, Handler, Stack, Config) ->
    incomplete(key, <<>>, Handler, Stack, Config);
key(Bin, Handler, Stack, Config) ->
    ?error(key, Bin, Handler, Stack, Config).


%% explicitly whitelist ascii set for faster parsing. really? really. someone should
%%  submit a patch that unrolls simple guards
%% note that if you encounter an error from string and you can't find the clause that
%%  caused it here, it might be in unescape below
string(<<32, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 32), Stack, Config);
string(<<33, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 33), Stack, Config);
string(<<?doublequote, Rest/binary>>, Handler, Acc, Stack, Config) ->
    doublequote(Rest, Handler, Acc, Stack, Config);
string(<<35, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 35), Stack, Config);
string(<<36, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 36), Stack, Config);
string(<<37, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 37), Stack, Config);
string(<<38, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 38), Stack, Config);
string(<<?singlequote, Rest/binary>>, Handler, Acc, Stack, Config) ->
    singlequote(Rest, Handler, Acc, Stack, Config);
string(<<40, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 40), Stack, Config);
string(<<41, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 41), Stack, Config);
string(<<42, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 42), Stack, Config);
string(<<43, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 43), Stack, Config);
string(<<44, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 44), Stack, Config);
string(<<45, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 45), Stack, Config);
string(<<46, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 46), Stack, Config);
string(<<?solidus, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace(?solidus, Config)), Stack, Config);
string(<<48, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 48), Stack, Config);
string(<<49, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 49), Stack, Config);
string(<<50, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 50), Stack, Config);
string(<<51, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 51), Stack, Config);
string(<<52, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 52), Stack, Config);
string(<<53, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 53), Stack, Config);
string(<<54, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 54), Stack, Config);
string(<<55, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 55), Stack, Config);
string(<<56, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 56), Stack, Config);
string(<<57, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 57), Stack, Config);
string(<<58, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 58), Stack, Config);
string(<<59, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 59), Stack, Config);
string(<<60, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 60), Stack, Config);
string(<<61, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 61), Stack, Config);
string(<<62, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 62), Stack, Config);
string(<<63, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 63), Stack, Config);
string(<<64, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 64), Stack, Config);
string(<<65, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 65), Stack, Config);
string(<<66, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 66), Stack, Config);
string(<<67, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 67), Stack, Config);
string(<<68, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 68), Stack, Config);
string(<<69, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 69), Stack, Config);
string(<<70, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 70), Stack, Config);
string(<<71, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 71), Stack, Config);
string(<<72, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 72), Stack, Config);
string(<<73, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 73), Stack, Config);
string(<<74, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 74), Stack, Config);
string(<<75, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 75), Stack, Config);
string(<<76, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 76), Stack, Config);
string(<<77, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 77), Stack, Config);
string(<<78, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 78), Stack, Config);
string(<<79, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 79), Stack, Config);
string(<<80, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 80), Stack, Config);
string(<<81, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 81), Stack, Config);
string(<<82, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 82), Stack, Config);
string(<<83, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 83), Stack, Config);
string(<<84, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 84), Stack, Config);
string(<<85, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 85), Stack, Config);
string(<<86, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 86), Stack, Config);
string(<<87, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 87), Stack, Config);
string(<<88, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 88), Stack, Config);
string(<<89, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 89), Stack, Config);
string(<<90, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 90), Stack, Config);
string(<<91, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 91), Stack, Config);
string(<<?rsolidus/utf8, Rest/binary>>, Handler, Acc, Stack, Config) ->
    unescape(Rest, Handler, Acc, Stack, Config);
string(<<93, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 93), Stack, Config);
string(<<94, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 94), Stack, Config);
string(<<95, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 95), Stack, Config);
string(<<96, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 96), Stack, Config);
string(<<97, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 97), Stack, Config);
string(<<98, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 98), Stack, Config);
string(<<99, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 99), Stack, Config);
string(<<100, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 100), Stack, Config);
string(<<101, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 101), Stack, Config);
string(<<102, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 102), Stack, Config);
string(<<103, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 103), Stack, Config);
string(<<104, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 104), Stack, Config);
string(<<105, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 105), Stack, Config);
string(<<106, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 106), Stack, Config);
string(<<107, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 107), Stack, Config);
string(<<108, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 108), Stack, Config);
string(<<109, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 109), Stack, Config);
string(<<110, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 110), Stack, Config);
string(<<111, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 111), Stack, Config);
string(<<112, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 112), Stack, Config);
string(<<113, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 113), Stack, Config);
string(<<114, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 114), Stack, Config);
string(<<115, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 115), Stack, Config);
string(<<116, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 116), Stack, Config);
string(<<117, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 117), Stack, Config);
string(<<118, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 118), Stack, Config);
string(<<119, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 119), Stack, Config);
string(<<120, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 120), Stack, Config);
string(<<121, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 121), Stack, Config);
string(<<122, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 122), Stack, Config);
string(<<123, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 123), Stack, Config);
string(<<124, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 124), Stack, Config);
string(<<125, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 125), Stack, Config);
string(<<126, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 126), Stack, Config);
string(<<127, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, 127), Stack, Config);
string(<<C, Rest/binary>>, Handler, Acc, Stack, Config=#config{dirty_strings=true}) ->
    string(Rest, Handler, acc_seq(Acc, C), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#20, X < 16#2028 ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X == 16#2028; X == 16#2029 ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace(X, Config)), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X > 16#2029, X < 16#d800 ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X > 16#dfff, X < 16#fdd0 ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X > 16#fdef, X < 16#fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#10000, X < 16#1fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#20000, X < 16#2fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#30000, X < 16#3fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#40000, X < 16#4fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#50000, X < 16#5fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#60000, X < 16#6fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#70000, X < 16#7fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#80000, X < 16#8fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#90000, X < 16#9fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#a0000, X < 16#afffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#b0000, X < 16#bfffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#c0000, X < 16#cfffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#d0000, X < 16#dfffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#e0000, X < 16#efffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#f0000, X < 16#ffffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
string(<<X/utf8, Rest/binary>>, Handler, Acc, Stack, Config) when X >= 16#100000, X < 16#10fffe ->
    string(Rest, Handler, acc_seq(Acc, X), Stack, Config);
%% partial utf8 codepoints. check that input could possibly be valid before attempting
%%  to correct
string(<<>>, Handler, Acc, Stack, Config) ->
    incomplete(string, <<>>, Handler, Acc, Stack, Config);
string(<<X>>, Handler, Acc, Stack, Config) when X >= 16#c2, X =< 16#f4 ->
    incomplete(string, <<X>>, Handler, Acc, Stack, Config);
string(<<X, Y>>, Handler, Acc, Stack, Config) when X >= 16#e0, X =< 16#f4, Y >= 16#80, Y =< 16#bf ->
    incomplete(string, <<X, Y>>, Handler, Acc, Stack, Config);
string(<<X, Y, Z>>, Handler, Acc, Stack, Config)
        when X >= 16#f0, X =< 16#f4,
            Y >= 16#80, Y =< 16#bf,
            Z >= 16#80, Z =< 16#bf ->
    incomplete(string, <<X, Y, Z>>, Handler, Acc, Stack, Config); 
%% surrogates
string(<<237, X, _, Rest/binary>>, Handler, Acc, Stack, Config=#config{replaced_bad_utf8=true})
        when X >= 160 ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
%% u+xfffe, u+xffff, control codes and other noncharacters
string(<<_/utf8, Rest/binary>>, Handler, Acc, Stack, Config=#config{replaced_bad_utf8=true}) ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
%% u+fffe and u+ffff for R14BXX (subsequent runtimes will happily match the
%%  preceeding clause
string(<<239, 191, X, Rest/binary>>, Handler, Acc, Stack, Config=#config{replaced_bad_utf8=true})
        when X == 190; X == 191 ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
%% overlong encodings and missing continuations of a 2 byte sequence
string(<<X, Rest/binary>>, Handler, Acc, Stack, Config=#config{replaced_bad_utf8=true})
        when X >= 192, X =< 223 ->
    strip_continuations(Rest, Handler, Acc, Stack, Config, 1);
%% overlong encodings and missing continuations of a 3 byte sequence
string(<<X, Rest/binary>>, Handler, Acc, Stack, Config=#config{replaced_bad_utf8=true})
        when X >= 224, X =< 239 ->
    strip_continuations(Rest, Handler, Acc, Stack, Config, 2);
%% overlong encodings and missing continuations of a 4 byte sequence
string(<<X, Rest/binary>>, Handler, Acc, Stack, Config=#config{replaced_bad_utf8=true})
        when X >= 240, X =< 247 ->
    strip_continuations(Rest, Handler, Acc, Stack, Config, 3);
%% incompletes and unexpected bytes, including orphan continuations
string(<<_, Rest/binary>>, Handler, Acc, Stack, Config=#config{replaced_bad_utf8=true}) ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
string(Bin, Handler, Acc, Stack, Config) ->
    ?error(string, Bin, Handler, Acc, Stack, Config).


doublequote(<<Rest/binary>>, Handler, Acc, [key|_] = Stack, Config) ->
    colon(Rest, handle_event({key, end_seq(Acc, Config)}, Handler, Config), Stack, Config);
doublequote(<<Rest/binary>>, Handler, Acc, [singlequote|_] = Stack, Config) ->
    string(Rest, Handler,acc_seq(Acc, maybe_replace(?doublequote, Config)), Stack, Config);
doublequote(<<>>, Handler, Acc, [singlequote|_] = Stack, Config) ->
    incomplete(string, <<?doublequote>>, Handler, Acc, Stack, Config);
doublequote(<<Rest/binary>>, Handler, Acc, Stack, Config) ->
    maybe_done(Rest, handle_event({string, end_seq(Acc, Config)}, Handler, Config), Stack, Config).


singlequote(<<Rest/binary>>, Handler, Acc, [singlequote, key|Stack], Config) ->
    colon(Rest, handle_event({key, end_seq(Acc, Config)}, Handler, Config), [key|Stack], Config);
singlequote(<<Rest/binary>>, Handler, Acc, [singlequote|Stack], Config) ->
    maybe_done(Rest, handle_event({string, end_seq(Acc, Config)}, Handler, Config), Stack, Config);
singlequote(<<Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, ?singlequote), Stack, Config).


%% strips continuation bytes after bad utf bytes, guards against both too short
%%  and overlong sequences. N is the maximum number of bytes to strip
strip_continuations(<<Rest/binary>>, Handler, Acc, Stack, Config, 0) ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
strip_continuations(<<X, Rest/binary>>, Handler, Acc, Stack, Config, N) when X >= 128, X =< 191 ->
    strip_continuations(Rest, Handler, Acc, Stack, Config, N - 1);
%% if end of input is reached before stripping the max number of continuations
%%  possible magic numbers are reinserted into the stream that get us back to
%%  the same state without complicated machinery
strip_continuations(<<>>, Handler, Acc, Stack, Config, N) ->
    case N of
        1 -> incomplete(string, <<192>>, Handler, Acc, Stack, Config);
        2 -> incomplete(string, <<224>>, Handler, Acc, Stack, Config);
        3 -> incomplete(string, <<240>>, Handler, Acc, Stack, Config)
    end;
%% not a continuation byte, insert a replacement character for sequence thus
%%  far and dispatch back to string
strip_continuations(<<Rest/binary>>, Handler, Acc, Stack, Config, _) ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config).


%% this all gets really gross and should probably eventually be folded into
%%  but for now it fakes being part of string on incompletes and errors
unescape(<<C, Rest/binary>>, Handler, Acc, Stack, Config=#config{dirty_strings=true}) ->
    case C of
        ?doublequote -> string(Rest, Handler, acc_seq(Acc, C), Stack, Config);
        ?rsolidus -> string(<<?rsolidus/utf8, Rest/binary>>, Handler, acc_seq(Acc, ?rsolidus), Stack, Config);
        _ -> string(Rest, Handler, acc_seq(Acc, [?rsolidus, C]), Stack, Config)
    end;
unescape(<<$b, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace($\b, Config)), Stack, Config);
unescape(<<$f, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace($\f, Config)), Stack, Config);
unescape(<<$n, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace($\n, Config)), Stack, Config);
unescape(<<$r, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace($\r, Config)), Stack, Config);
unescape(<<$t, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace($\t, Config)), Stack, Config);
unescape(<<?doublequote, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace($\", Config)), Stack, Config);
unescape(<<?singlequote, Rest/binary>>, Handler, Acc, Stack, Config=#config{single_quoted_strings=true}) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace(?singlequote, Config)), Stack, Config);
unescape(<<?rsolidus, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace($\\, Config)), Stack, Config);
unescape(<<?solidus, Rest/binary>>, Handler, Acc, Stack, Config) ->
    string(Rest, Handler, acc_seq(Acc, maybe_replace($/, Config)), Stack, Config);
unescape(<<$u, $d, A, B, C, ?rsolidus, $u, $d, X, Y, Z, Rest/binary>>, Handler, Acc, Stack, Config)
        when (A == $8 orelse A == $9 orelse A == $a orelse A == $b),
             (X == $c orelse X == $d orelse X == $e orelse X == $f),
             ?is_hex(B), ?is_hex(C), ?is_hex(Y), ?is_hex(Z)
        ->
    High = erlang:list_to_integer([$d, A, B, C], 16),
    Low = erlang:list_to_integer([$d, X, Y, Z], 16),
    Codepoint = (High - 16#d800) * 16#400 + (Low - 16#dc00) + 16#10000,
    string(Rest, Handler, acc_seq(Acc, Codepoint), Stack, Config);
unescape(<<$u, $d, A, B, C, ?rsolidus, $u, W, X, Y, Z, Rest/binary>>, Handler, Acc, Stack, Config)
        when (A == $8 orelse A == $9 orelse A == $a orelse A == $b),
             ?is_hex(B), ?is_hex(C), ?is_hex(W), ?is_hex(X), ?is_hex(Y), ?is_hex(Z)
        ->
    case Config#config.replaced_bad_utf8 of
        true -> string(Rest, Handler, acc_seq(Acc, [16#fffd, 16#fffd]), Stack, Config);
        false -> ?error(<<$u, $d, A, B, C, ?rsolidus, $u, W, X, Y, Z, Rest/binary>>, Handler, Acc, Stack, Config)
    end;
unescape(<<$u, $d, A, B, C, ?rsolidus, Rest/binary>>, Handler, Acc, Stack, Config)
        when (A == $8 orelse A == $9 orelse A == $a orelse A == $b),
             ?is_hex(B), ?is_hex(C)
        ->
    incomplete(string, <<?rsolidus, $u, $d, A, B, C, ?rsolidus, Rest/binary>>, Handler, Acc, Stack, Config);
unescape(<<$u, $d, A, B, C>>, Handler, Acc, Stack, Config)
        when (A == $8 orelse A == $9 orelse A == $a orelse A == $b),
             ?is_hex(B), ?is_hex(C)
        ->
    incomplete(string, <<?rsolidus, $u, $d, A, B, C>>, Handler, Acc, Stack, Config);
unescape(<<$u, A, B, C, D, Rest/binary>>, Handler, Acc, Stack, Config)
        when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
    case erlang:list_to_integer([A, B, C, D], 16) of
        Codepoint when Codepoint < 16#d800; Codepoint > 16#dfff ->
            string(Rest, Handler, acc_seq(Acc, maybe_replace(Codepoint, Config)), Stack, Config);
        _ when Config#config.replaced_bad_utf8 ->
            string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
        _ -> ?error(string, <<?rsolidus, $u, A, B, C, D, Rest/binary>>, Handler, Acc, Stack, Config)
    end;
unescape(Bin, Handler, Acc, Stack, Config=#config{ignored_bad_escapes=true}) ->
    string(Bin, Handler, acc_seq(Acc, ?rsolidus), Stack, Config);
unescape(Bin, Handler, Acc, Stack, Config) ->
    case is_partial_escape(Bin) of
        true -> incomplete(string, <<?rsolidus/utf8, Bin/binary>>, Handler, Acc, Stack, Config);
        false -> ?error(string, <<?rsolidus, Bin/binary>>, Handler, Acc, Stack, Config)
    end.


is_partial_escape(<<$u, A, B, C>>) when ?is_hex(A), ?is_hex(B), ?is_hex(C) -> true;
is_partial_escape(<<$u, A, B>>) when ?is_hex(A), ?is_hex(B) -> true;
is_partial_escape(<<$u, A>>) when ?is_hex(A) -> true;
is_partial_escape(<<$u>>) -> true;
is_partial_escape(<<>>) -> true;
is_partial_escape(_) -> false.


maybe_replace(C, #config{dirty_strings=true}) -> C;
maybe_replace($\b, #config{escaped_strings=true}) -> [$\\, $b];
maybe_replace($\t, #config{escaped_strings=true}) -> [$\\, $t];
maybe_replace($\n, #config{escaped_strings=true}) -> [$\\, $n];
maybe_replace($\f, #config{escaped_strings=true}) -> [$\\, $f];
maybe_replace($\r, #config{escaped_strings=true}) -> [$\\, $r];
maybe_replace($\", #config{escaped_strings=true}) -> [$\\, $\"];
maybe_replace($/, Config=#config{escaped_strings=true}) ->
    case Config#config.escaped_forward_slashes of
        true -> [$\\, $/]
        ; false -> $/
    end;
maybe_replace($\\, #config{escaped_strings=true}) -> [$\\, $\\];
maybe_replace(X, Config=#config{escaped_strings=true})  when X == 16#2028; X == 16#2029 ->
    case Config#config.unescaped_jsonp of
        true -> X
        ; false -> json_escape_sequence(X)
    end;
maybe_replace(X, #config{escaped_strings=true}) when X < 32 -> json_escape_sequence(X);
maybe_replace(X, _Config) -> X.


%% convert a codepoint to it's \uXXXX equiv.
json_escape_sequence(X) ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    [$\\, $u, (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))].


%% ascii "1" is [49], "2" is [50], etc...
to_hex(10) -> $a;
to_hex(11) -> $b;
to_hex(12) -> $c;
to_hex(13) -> $d;
to_hex(14) -> $e;
to_hex(15) -> $f;
to_hex(X) -> X + 48.


%% like in strings, there's some pseudo states in here that will never
%%  show up in errors or incompletes. some show up in value, some show
%%  up in integer, decimal or exp
negative(<<$0, Rest/binary>>, Handler, Acc, Stack, Config) ->
    zero(Rest, Handler, acc_seq(Acc, $0), Stack, Config);
negative(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when ?is_nonzero(S) ->
    integer(Rest, Handler, acc_seq(Acc, S), Stack, Config);
negative(<<>>, Handler, [?negative], Stack, Config) ->
    incomplete(value, <<?negative>>, Handler, Stack, Config);
negative(Bin, Handler, Acc, Stack, Config) ->
    ?error(value, <<?negative, Bin/binary>>, Handler, Acc, Stack, Config).


zero(<<?decimalpoint, Rest/binary>>, Handler, Acc, Stack, Config) ->
    decimal(Rest, Handler, acc_seq(Acc, ?decimalpoint), Stack, Config);
zero(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= $e; S =:= $E ->
    e(Rest, Handler, acc_seq(Acc, ".0e"), Stack, Config);
zero(<<>>, Handler, Acc, [], Config=#config{explicit_end=false}) ->
    finish_number(<<>>, Handler, {zero, Acc}, [], Config);
zero(<<>>, Handler, Acc, Stack, Config) ->
    incomplete(value, (end_seq(Acc)), Handler, Stack, Config);
zero(Bin, Handler, Acc, Stack, Config) ->
    finish_number(Bin, Handler, {zero, Acc}, Stack, Config).


integer(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= ?zero; ?is_nonzero(S) ->
    integer(Rest, Handler, acc_seq(Acc, S), Stack, Config);
integer(<<?decimalpoint, Rest/binary>>, Handler, Acc, Stack, Config) ->
    initialdecimal(Rest, Handler, acc_seq(Acc, ?decimalpoint), Stack, Config);
integer(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= $e; S =:= $E ->
    e(Rest, Handler, acc_seq(Acc, ".0e"), Stack, Config);
integer(Bin, Handler, Acc, Stack, Config) ->
    finish_number(Bin, Handler, {integer, Acc}, Stack, Config).


initialdecimal(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Handler, acc_seq(Acc, S), Stack, Config);
initialdecimal(<<>>, Handler, [?decimalpoint|Acc], Stack, Config) ->
    incomplete(integer, <<?decimalpoint>>, Handler, Acc, Stack, Config);
initialdecimal(Bin, Handler, Acc, Stack, Config) ->
    ?error(decimal, Bin, Handler, Acc, Stack, Config).


decimal(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= ?zero; ?is_nonzero(S) ->
    decimal(Rest, Handler, acc_seq(Acc, S), Stack, Config);
decimal(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= $e; S =:= $E ->
    e(Rest, Handler, acc_seq(Acc, $e), Stack, Config);
decimal(Bin, Handler, Acc, Stack, Config) ->
    finish_number(Bin, Handler, {decimal, Acc}, Stack, Config).


e(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, acc_seq(Acc, S), Stack, Config);
e(<<Sign, Rest/binary>>, Handler, Acc, Stack, Config) when Sign =:= ?positive; Sign =:= ?negative ->
    ex(Rest, Handler, acc_seq(Acc, Sign), Stack, Config);
e(<<>>, Handler, [$e|Acc], Stack, Config) ->
    incomplete(decimal, <<$e>>, Handler, Acc, Stack, Config);
e(Bin, Handler, Acc, Stack, Config) ->
    ?error(decimal, <<$e, Bin/binary>>, Handler, Acc, Stack, Config).


ex(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, acc_seq(Acc, S), Stack, Config);
ex(<<>>, Handler, [S, $e|Acc], Stack, Config) ->
    incomplete(decimal, <<$e, S/utf8>>, Handler, Acc, Stack, Config);
ex(Bin, Handler, [S, $e|Acc], Stack, Config) ->
    ?error(decimal, <<$e, S, Bin/binary>>, Handler, Acc, Stack, Config).


exp(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when S =:= ?zero; ?is_nonzero(S) ->
    exp(Rest, Handler, acc_seq(Acc, S), Stack, Config);
exp(Bin, Handler, Acc, Stack, Config) ->
    finish_number(Bin, Handler, {exp, Acc}, Stack, Config).


finish_number(Rest, Handler, Acc, [], Config=#config{explicit_end=false}) ->
    maybe_done(Rest, handle_event(format_number(Acc), Handler, Config), [], Config);
finish_number(<<?end_object, Rest/binary>>, Handler, Acc, [object|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_object], Handler, Config), Stack, Config);
finish_number(<<?end_array, Rest/binary>>, Handler, Acc, [array|Stack], Config) ->
    maybe_done(Rest, handle_event([format_number(Acc), end_array], Handler, Config), Stack, Config);
finish_number(<<?comma, Rest/binary>>, Handler, Acc, [object|Stack], Config) ->
    key(Rest, handle_event(format_number(Acc), Handler, Config), [key|Stack], Config);
finish_number(<<?comma, Rest/binary>>, Handler, Acc, [array|Stack], Config) ->
    value(Rest, handle_event(format_number(Acc), Handler, Config), [array|Stack], Config);
finish_number(<<S, Rest/binary>>, Handler, Acc, Stack, Config) when ?is_whitespace(S) ->
    maybe_done(Rest, handle_event(format_number(Acc), Handler, Config), Stack, Config);
finish_number(<<?solidus, ?solidus, Rest/binary>>, Handler, Acc, Stack, Config=#config{comments=true}) ->
    comment(Rest, handle_event(format_number(Acc), Handler, Config), maybe_done, [comment|Stack], Config);
finish_number(<<?solidus, ?star, Rest/binary>>, Handler, Acc, Stack, Config=#config{comments=true}) ->
    comment(Rest, handle_event(format_number(Acc), Handler, Config), maybe_done, [multicomment|Stack], Config);
finish_number(<<?solidus>>, Handler, Acc, Stack, Config=#config{comments=true}) ->
    incomplete(maybe_done, <<?solidus>>, handle_event(format_number(Acc), Handler, Config), Stack, Config);
finish_number(<<>>, Handler, {NumType, Acc}, Stack, Config) ->
    case NumType of
        integer -> incomplete(integer, <<>>, Handler, Acc, Stack, Config);
        decimal -> incomplete(decimal, <<>>, Handler, Acc, Stack, Config);
        exp -> incomplete(exp, <<>>, Handler, Acc, Stack, Config)
    end;
finish_number(Bin, Handler, {NumType, Acc}, Stack, Config) ->
    case NumType of
        integer -> ?error(integer, Bin, Handler, Acc, Stack, Config);
        decimal -> ?error(decimal, Bin, Handler, Acc, Stack, Config);
        exp -> ?error(exp, Bin, Handler, Acc, Stack, Config);
        zero ->
            [$0|OldAcc] = Acc,
            ?error(value, <<$0, Bin/binary>>, Handler, OldAcc, Stack, Config)
    end.

format_number({zero, Acc}) -> {integer, list_to_integer(lists:reverse(Acc))};
format_number({integer, Acc}) -> {integer, list_to_integer(lists:reverse(Acc))};
format_number({decimal, Acc}) -> {float, list_to_float(lists:reverse(Acc))};
format_number({exp, Acc}) -> {float, list_to_float(lists:reverse(Acc))}.


true(<<$r, $u, $e, Rest/binary>>, Handler, Stack, Config) ->
    maybe_done(Rest, handle_event({literal, true}, Handler, Config), Stack, Config);
true(<<$r, $u>>, Handler, Stack, Config) ->
    incomplete(true, <<$r, $u>>, Handler, Stack, Config);
true(<<$r>>, Handler, Stack, Config) ->
    incomplete(true, <<$r>>, Handler, Stack, Config);
true(<<>>, Handler, Stack, Config) ->
    incomplete(true, <<>>, Handler, Stack, Config);
true(Bin, Handler, Stack, Config) ->
    ?error(true, Bin, Handler, Stack, Config).


false(<<$a, $l, $s, $e, Rest/binary>>, Handler, Stack, Config) ->
    maybe_done(Rest, handle_event({literal, false}, Handler, Config), Stack, Config);
false(<<$a, $l, $s>>, Handler, Stack, Config) ->
    incomplete(false, <<$a, $l, $s>>, Handler, Stack, Config);
false(<<$a, $l>>, Handler, Stack, Config) ->
    incomplete(false, <<$a, $l>>, Handler, Stack, Config);
false(<<$a>>, Handler, Stack, Config) ->
    incomplete(false, <<$a>>, Handler, Stack, Config);
false(<<>>, Handler, Stack, Config) ->
    incomplete(false, <<>>, Handler, Stack, Config);
false(Bin, Handler, Stack, Config) ->
    ?error(false, Bin, Handler, Stack, Config).


null(<<$u, $l, $l, Rest/binary>>, Handler, Stack, Config) ->
    maybe_done(Rest, handle_event({literal, null}, Handler, Config), Stack, Config);
null(<<$u, $l>>, Handler, Stack, Config) ->
    incomplete(null, <<$u, $l>>, Handler, Stack, Config);
null(<<$u>>, Handler, Stack, Config) ->
    incomplete(null, <<$u>>, Handler, Stack, Config);
null(<<>>, Handler, Stack, Config) ->
    incomplete(null, <<>>, Handler, Stack, Config);
null(Bin, Handler, Stack, Config) ->
    ?error(null, Bin, Handler, Stack, Config).


comment(<<?newline, Rest/binary>>, Handler, Resume, [comment|Stack], Config) ->
    resume(Rest, Resume, Handler, unused, Stack, Config);
comment(<<?solidus, ?star, Rest/binary>>, Handler, Resume, Stack, Config) ->
    comment(Rest, Handler, Resume, [multicomment|Stack], Config);
comment(<<?solidus>>, Handler, Resume, [multicomment|_] = Stack, Config) ->
    incomplete(comment, <<?solidus>>, Handler, Resume, Stack, Config);
comment(<<?star, ?solidus, Rest/binary>>, Handler, Resume, [multicomment|Stack], Config) ->
    case Stack of
        [multicomment|_] -> comment(Rest, Handler, Resume, Stack, Config);
        _ -> resume(Rest, Resume, Handler, unused, Stack, Config)
    end;
comment(<<?star>>, Handler, Resume, [multicomment|_] = Stack, Config) ->
    incomplete(comment, <<?star>>, Handler, Resume, Stack, Config);
comment(<<_/utf8, Rest/binary>>, Handler, Resume, Stack, Config) ->
    comment(Rest, Handler, Resume, Stack, Config);
comment(<<_, Rest/binary>>, Handler, Resume, Stack, Config=#config{replaced_bad_utf8=true}) ->
    comment(Rest, Handler, Resume, Stack, Config);
comment(<<>>, Handler, done, [Comment], Config=#config{explicit_end=false})
        when Comment == comment; Comment == multicomment ->
    resume(<<>>, done, Handler, unused, [], Config);
comment(<<>>, Handler, Resume, Stack, Config) ->
    incomplete(comment, <<>>, Handler, Resume, Stack, Config);
comment(Bin, Handler, Resume, Stack, Config) ->
    ?error(comment, Bin, Handler, Resume, Stack, Config).


maybe_done(<<Rest/binary>>, Handler, [], Config) ->
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
maybe_done(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, maybe_done, [comment|Stack], Config);
maybe_done(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, maybe_done, [multicomment|Stack], Config);
maybe_done(<<?solidus>>, Handler, Stack, Config=#config{comments=true}) ->
    incomplete(maybe_done, <<?solidus>>, Handler, Stack, Config);
maybe_done(<<>>, Handler, Stack, Config) when length(Stack) > 0 ->
    incomplete(maybe_done, <<>>, Handler, Stack, Config);
maybe_done(Bin, Handler, Stack, Config) ->
    ?error(maybe_done, Bin, Handler, Stack, Config).


done(<<S, Rest/binary>>, Handler, [], Config) when ?is_whitespace(S) ->
    done(Rest, Handler, [], Config);
done(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, done, [comment|Stack], Config);
done(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config=#config{comments=true}) ->
    comment(Rest, Handler, done, [multicomment|Stack], Config);
done(<<?solidus>>, Handler, Stack, Config=#config{comments=true}) ->
    incomplete(done, <<?solidus>>, Handler, Stack, Config);
done(<<>>, {Handler, State}, [], Config=#config{explicit_end=true}) ->
    incomplete(done, <<>>, {Handler, State}, [], Config);
done(<<>>, {_Handler, State}, [], _Config) -> State;
done(Bin, Handler, Stack, Config) -> ?error(done, Bin, Handler, Stack, Config).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


json_to_bytes(JSON) -> json_to_bytes(JSON, []).

json_to_bytes(<<>>, Acc) -> [<<>>] ++ lists:reverse(Acc);
json_to_bytes(<<X, Rest/binary>>, Acc) -> json_to_bytes(Rest, [<<X>>] ++ Acc).


decode(JSON, Config) ->
    Chunk = try
        start(JSON, {jsx, []}, [], jsx_config:parse_config(Config))
    catch
        error:badarg -> {error, badarg}
    end,
    Incremental = try
        Final = lists:foldl(
            fun(Byte, Decoder) -> {incomplete, F} = Decoder(Byte), F end,
            decoder(jsx, [], [explicit_end] ++ Config),
            json_to_bytes(JSON)
        ),
        Final(end_stream)
    catch
        error:badarg -> {error, badarg}
    end,
    ?assert(Chunk == Incremental),
    Chunk.


decode_test_() ->
    Data = jsx:test_cases(),
    [{Title, ?_assertEqual(Events ++ [end_json], decode(JSON, []))}
        || {Title, JSON, _, Events} <- Data
    ].


%% all these numbers have different representation in erlang than in javascript and
%%  do not roundtrip like most integers/floats
special_number_test_() ->
    [
        {"-0", ?_assertEqual(
            [{integer, 0}, end_json],
            decode(<<"-0">>, [])
        )},
        {"-0.0", ?_assertEqual(
            [{float, 0.0}, end_json],
            decode(<<"-0.0">>, [])
        )},
        {"0e0", ?_assertEqual(
            [{float, 0.0}, end_json],
            decode(<<"0e0">>, [])
        )},
        {"0e4", ?_assertEqual(
            [{float, 0.0}, end_json],
            decode(<<"0e4">>, [])
        )},
        {"1e0", ?_assertEqual(
            [{float, 1.0}, end_json],
            decode(<<"1e0">>, [])
        )},
        {"-1e0", ?_assertEqual(
            [{float, -1.0}, end_json],
            decode(<<"-1e0">>, [])
        )},
        {"1e4", ?_assertEqual(
            [{float, 1.0e4}, end_json],
            decode(<<"1e4">>, [])
        )},
        {"number terminated by whitespace", ?_assertEqual(
            [start_array, {integer, 1}, end_array, end_json],
            decode(<<"[ 1 ]">>, [])
        )},
        {"number terminated by comma", ?_assertEqual(
            [start_array, {integer, 1}, {integer, 1}, end_array, end_json],
            decode(<<"[ 1, 1 ]">>, [])
        )},
        {"number terminated by comma in object", ?_assertEqual(
            [start_object, {key, <<"x">>}, {integer, 1}, {key, <<"y">>}, {integer, 1}, end_object, end_json],
            decode(<<"{\"x\": 1, \"y\": 1}">>, [])
        )}
    ].


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
        )},
        {"/**/ comment inside /**/ comment", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ /* /* comment */ */ true ]">>, [comments])
        )},
        {"/**/ comment with /", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ /* / */ true ]">>, [comments])
        )},
        {"/**/ comment with *", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ /* * */ true ]">>, [comments])
        )},
        {"// comment with badutf", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ // comment ", 16#00c0, " ", ?newline, "true]">>, [comments, replaced_bad_utf8])
        )},
        {"/**/ comment with badutf", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ /* comment ", 16#00c0, " */ true]">>, [comments, replaced_bad_utf8])
        )},
        {"/**/ comment with badutf preceeded by /", ?_assertEqual(
            [start_array, {literal, true}, end_array, end_json],
            decode(<<"[ /* comment /", 16#00c0, " */ true]">>, [comments, replaced_bad_utf8])
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
        {"clean codepoints", ?_assertEqual(
            [{string, codepoints()}, end_json],
            decode(<<34, (codepoints())/binary, 34>>, [])
        )},
        {"clean extended codepoints", ?_assertEqual(
            [{string, extended_codepoints()}, end_json],
            decode(<<34, (extended_codepoints())/binary, 34>>, [])
        )},
        {"error reserved space", ?_assertEqual(
            lists:duplicate(length(reserved_space()), {error, badarg}),
            lists:map(fun(Codepoint) -> decode(Codepoint, []) end, reserved_space())
        )},
        {"error surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), {error, badarg}),
            lists:map(fun(Codepoint) -> decode(Codepoint, []) end, surrogates())
        )},
        {"error noncharacters", ?_assertEqual(
            lists:duplicate(length(noncharacters()), {error, badarg}),
            lists:map(fun(Codepoint) -> decode(Codepoint, []) end, noncharacters())
        )},
        {"error extended noncharacters", ?_assertEqual(
            lists:duplicate(length(extended_noncharacters()), {error, badarg}),
            lists:map(fun(Codepoint) -> decode(Codepoint, []) end, extended_noncharacters())
        )},
        {"clean reserved space", ?_assertEqual(
            lists:duplicate(length(reserved_space()), [{string, <<16#fffd/utf8>>}, end_json]),
            lists:map(fun(Codepoint) -> decode(Codepoint, [replaced_bad_utf8]) end, reserved_space())
        )},
        {"clean surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), [{string, <<16#fffd/utf8>>}, end_json]),
            lists:map(fun(Codepoint) -> decode(Codepoint, [replaced_bad_utf8]) end, surrogates())
        )},
        {"clean noncharacters", ?_assertEqual(
            lists:duplicate(length(noncharacters()), [{string, <<16#fffd/utf8>>}, end_json]),
            lists:map(fun(Codepoint) -> decode(Codepoint, [replaced_bad_utf8]) end, noncharacters())
        )},
        {"clean extended noncharacters", ?_assertEqual(
            lists:duplicate(length(extended_noncharacters()), [{string, <<16#fffd/utf8>>}, end_json]),
            lists:map(fun(Codepoint) -> decode(Codepoint, [replaced_bad_utf8]) end, extended_noncharacters())
        )},
        {"dirty \\uwxyz", ?_assertEqual(
            [{string, <<"\\uwxyz">>}, end_json],
            decode(<<34, "\\uwxyz", 34>>, [dirty_strings])
        )},
        {"dirty \\x23", ?_assertEqual(
            [{string, <<"\\x23">>}, end_json],
            decode(<<34, "\\x23", 34>>, [dirty_strings])
        )},
        {"dirty 0", ?_assertEqual(
            [{string, <<0>>}, end_json],
            decode(<<34, 0, 34>>, [dirty_strings])
        )},
        {"dirty 0\"0", ?_assertEqual(
            [{string, <<0, ?doublequote, 0>>}, end_json],
            decode(<<34, 0, ?rsolidus, ?doublequote, 0, 34>>, [dirty_strings])
        )},
        {"dirty 0\"0", ?_assertEqual(
            [{string, <<0, ?rsolidus, ?doublequote, 0>>}, end_json],
            decode(<<34, 0, ?rsolidus, ?rsolidus, ?doublequote, 0, 34>>, [dirty_strings])
        )},
        {"dirty 16#d800", ?_assertEqual(
            [{string, <<237, 160, 128>>}, end_json],
            decode(<<34, 237, 160, 128, 34>>, [dirty_strings])
        )},
        {"dirty 16#10ffff", ?_assertEqual(
            [{string, <<244, 143, 191, 191>>}, end_json],
            decode(<<34, 244, 143, 191, 191, 34>>, [dirty_strings])
        )},
        {"dirty /", ?_assertEqual(
            [{string, <<$/>>}, end_json],
            decode(<<34, $/, 34>>, [dirty_strings, escaped_forward_slashes])
        )},
        {"dirty <<194, 129>>", ?_assertEqual(
            [{string, <<194, 129>>}, end_json],
            decode(<<34, 194, 129, 34>>, [dirty_strings])
        )}
    ].


decode_bad_utf(String, Config) ->
    case decode(<<34, String/binary, 34>>, Config) of
        {error, badarg} -> erlang:error(badarg);
        [{string, S}, end_json] -> S
    end.

bad_utf8_test_() ->
    [
        {"noncharacter u+fffe", ?_assertError(
            badarg,
            decode_bad_utf(<<239, 191, 190>>, [])
        )},
        {"noncharacter u+fffe replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            decode_bad_utf(<<239, 191, 190>>, [replaced_bad_utf8])
        )},
        {"noncharacter u+ffff", ?_assertError(
            badarg,
            decode_bad_utf(<<239, 191, 191>>, [])
        )},
        {"noncharacter u+ffff replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            decode_bad_utf(<<239, 191, 191>>, [replaced_bad_utf8])
        )},
        {"orphan continuation byte u+0080", ?_assertError(
            badarg,
            decode_bad_utf(<<16#0080>>, [])
        )},
        {"orphan continuation byte u+0080 replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            decode_bad_utf(<<16#0080>>, [replaced_bad_utf8])
        )},
        {"orphan continuation byte u+00bf", ?_assertError(
            badarg,
            decode_bad_utf(<<16#00bf>>, [])
        )},
        {"orphan continuation byte u+00bf replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            decode_bad_utf(<<16#00bf>>, [replaced_bad_utf8])
        )},
        {"2 continuation bytes", ?_assertError(
            badarg,
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 2))/binary>>, [])
        )},
        {"2 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 2),
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 2))/binary>>, [replaced_bad_utf8])
        )},
        {"3 continuation bytes", ?_assertError(
            badarg,
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 3))/binary>>, [])
        )},
        {"3 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 3),
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 3))/binary>>, [replaced_bad_utf8])
        )},
        {"4 continuation bytes", ?_assertError(
            badarg,
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 4))/binary>>, [])
        )},
        {"4 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 4),
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 4))/binary>>, [replaced_bad_utf8])
        )},
        {"5 continuation bytes", ?_assertError(
            badarg,
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 5))/binary>>, [])
        )},
        {"5 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 5),
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 5))/binary>>, [replaced_bad_utf8])
        )},
        {"6 continuation bytes", ?_assertError(
            badarg,
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 6))/binary>>, [])
        )},
        {"6 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 6),
            decode_bad_utf(<<(binary:copy(<<16#0080>>, 6))/binary>>, [replaced_bad_utf8])
        )},
        {"all continuation bytes", ?_assertError(
            badarg,
            decode_bad_utf(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, [])
        )},
        {"all continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf))),
            decode_bad_utf(
                <<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>,
                [replaced_bad_utf8]
            )
        )},
        {"lonely start byte", ?_assertError(
            badarg,
            decode_bad_utf(<<16#00c0>>, [])
        )},
        {"lonely start byte replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            decode_bad_utf(<<16#00c0>>, [replaced_bad_utf8])
        )},
        {"lonely start bytes (2 byte)", ?_assertError(
            badarg,
            decode_bad_utf(<<16#00c0, 32, 16#00df>>, [])
        )},
        {"lonely start bytes (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            decode_bad_utf(<<16#00c0, 32, 16#00df>>, [replaced_bad_utf8])
        )},
        {"lonely start bytes (3 byte)", ?_assertError(
            badarg,
            decode_bad_utf(<<16#00e0, 32, 16#00ef>>, [])
        )},
        {"lonely start bytes (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            decode_bad_utf(<<16#00e0, 32, 16#00ef>>, [replaced_bad_utf8])
        )},
        {"lonely start bytes (4 byte)", ?_assertError(
            badarg,
            decode_bad_utf(<<16#00f0, 32, 16#00f7>>, [])
        )},
        {"lonely start bytes (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            decode_bad_utf(<<16#00f0, 32, 16#00f7>>, [replaced_bad_utf8])
        )},
        {"missing continuation byte (3 byte)", ?_assertError(
            badarg,
            decode_bad_utf(<<224, 160, 32>>, [])
        )},
        {"missing continuation byte (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<224, 160, 32>>, [replaced_bad_utf8])
        )},
        {"missing continuation byte (4 byte missing one)", ?_assertError(
            badarg,
            decode_bad_utf(<<240, 144, 128, 32>>, [])
        )},
        {"missing continuation byte (4 byte missing one) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<240, 144, 128, 32>>, [replaced_bad_utf8])
        )},
        {"missing continuation byte (4 byte missing two)", ?_assertError(
            badarg,
            decode_bad_utf(<<240, 144, 32>>, [])
        )},
        {"missing continuation byte (4 byte missing two) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<240, 144, 32>>, [replaced_bad_utf8])
        )},
        {"overlong encoding of u+002f (2 byte)", ?_assertError(
            badarg,
            decode_bad_utf(<<16#c0, 16#af, 32>>, [])
        )},
        {"overlong encoding of u+002f (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<16#c0, 16#af, 32>>, [replaced_bad_utf8])
        )},
        {"overlong encoding of u+002f (3 byte)", ?_assertError(
            badarg,
            decode_bad_utf(<<16#e0, 16#80, 16#af, 32>>, [])
        )},
        {"overlong encoding of u+002f (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<16#e0, 16#80, 16#af, 32>>, [replaced_bad_utf8])
        )},
        {"overlong encoding of u+002f (4 byte)", ?_assertError(
            badarg,
            decode_bad_utf(<<16#f0, 16#80, 16#80, 16#af, 32>>, [])
        )},
        {"overlong encoding of u+002f (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<16#f0, 16#80, 16#80, 16#af, 32>>, [replaced_bad_utf8])
        )},
        {"highest overlong 2 byte sequence", ?_assertError(
            badarg,
            decode_bad_utf(<<16#c1, 16#bf, 32>>, [])
        )},
        {"highest overlong 2 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<16#c1, 16#bf, 32>>, [replaced_bad_utf8])
        )},
        {"highest overlong 3 byte sequence", ?_assertError(
            badarg,
            decode_bad_utf(<<16#e0, 16#9f, 16#bf, 32>>, [])
        )},
        {"highest overlong 3 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<16#e0, 16#9f, 16#bf, 32>>, [replaced_bad_utf8])
        )},
        {"highest overlong 4 byte sequence", ?_assertError(
            badarg,
            decode_bad_utf(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, [])
        )},
        {"highest overlong 4 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            decode_bad_utf(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, [replaced_bad_utf8])
        )}
    ].


unescape(Bin, Config) ->
    case decode(<<34, Bin/binary, 34>>, Config) of
        [{string, String}, end_json] -> String;
        {error, badarg} -> erlang:error(badarg)
    end.

unescape_test_() ->
    [
        {"unescape backspace", ?_assertEqual(
            <<"\b">>,
            unescape(<<"\\b"/utf8>>, [])
        )},
        {"unescape tab", ?_assertEqual(
            <<"\t">>,
            unescape(<<"\\t"/utf8>>, [])
        )},
        {"unescape newline", ?_assertEqual(
            <<"\n">>,
            unescape(<<"\\n"/utf8>>, [])
        )},
        {"unescape formfeed", ?_assertEqual(
            <<"\f">>,
            unescape(<<"\\f"/utf8>>, [])
        )},
        {"unescape carriage return", ?_assertEqual(
            <<"\r">>,
            unescape(<<"\\r"/utf8>>, [])
        )},
        {"unescape quote", ?_assertEqual(
            <<"\"">>,
            unescape(<<"\\\""/utf8>>, [])
        )},
        {"unescape single quote", ?_assertEqual(
            <<"'">>,
            unescape(<<"\\'"/utf8>>, [single_quoted_strings])
        )},
        {"unescape solidus", ?_assertEqual(
            <<"/">>,
            unescape(<<"\\/"/utf8>>, [])
        )},
        {"unescape reverse solidus", ?_assertEqual(
            <<"\\">>,
            unescape(<<"\\\\"/utf8>>, [])
        )},
        {"unescape control", ?_assertEqual(
            <<0>>,
            unescape(<<"\\u0000"/utf8>>, [])
        )},
        {"unescape surrogate pair", ?_assertEqual(
            <<16#10000/utf8>>,
            unescape(<<"\\ud800\\udc00"/utf8>>, [])
        )},
        {"replace bad high surrogate", ?_assertEqual(
            <<16#fffd/utf8>>,
            unescape(<<"\\udc00"/utf8>>, [replaced_bad_utf8])
        )},
        {"do not unescape bad high surrogate", ?_assertError(
            badarg,
            unescape(<<"\\udc00"/utf8>>, [])
        )},
        {"replace naked high surrogate", ?_assertEqual(
            <<16#fffd/utf8, "hello world">>,
            unescape(<<"\\ud800hello world"/utf8>>, [replaced_bad_utf8])
        )},
        {"do not unescape naked high surrogate", ?_assertError(
            badarg,
            unescape(<<"\\ud800hello world"/utf8>>, [])
        )},
        {"replace naked low surrogate", ?_assertEqual(
            <<16#fffd/utf8, "hello world">>,
            unescape(<<"\\udc00hello world"/utf8>>, [replaced_bad_utf8])
        )},
        {"do not unescape naked low surrogate", ?_assertError(
            badarg,
            unescape(<<"\\udc00hello world"/utf8>>, [])
        )},
        {"replace bad surrogate pair", ?_assertEqual(
            <<16#fffd/utf8, 16#fffd/utf8>>,
            unescape(<<"\\ud800\\u0000">>, [replaced_bad_utf8])
        )},
        {"do not unescape bad surrogate pair", ?_assertError(
            badarg,
            unescape(<<"\\ud800\\u0000">>, [])
        )},
        {"bad pseudo escape sequence", ?_assertError(
            badarg,
            unescape(<<"\\uabcg">>, [])
        )}
    ].


maybe_escape(Bin, Config) ->
    [{string, String}, end_json] = decode(Bin, Config),
    String.

escape_test_() ->
    [
        {"maybe_escape backspace", ?_assertEqual(
            <<"\\b">>,
            maybe_escape(<<34, "\\b"/utf8, 34>>, [escaped_strings])
        )},
        {"don't escape backspace", ?_assertEqual(
            <<"\b">>,
            maybe_escape(<<34, "\\b"/utf8, 34>>, [])
        )},
        {"maybe_escape tab", ?_assertEqual(
            <<"\\t">>,
            maybe_escape(<<34, "\\t"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape newline", ?_assertEqual(
            <<"\\n">>,
            maybe_escape(<<34, "\\n"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape formfeed", ?_assertEqual(
            <<"\\f">>,
            maybe_escape(<<34, "\\f"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape carriage return", ?_assertEqual(
            <<"\\r">>,
            maybe_escape(<<34, "\\r"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape quote", ?_assertEqual(
            <<"\\\"">>,
            maybe_escape(<<34, "\\\""/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape forward slash", ?_assertEqual(
            <<"\\/">>,
            maybe_escape(<<34, "/"/utf8, 34>>, [escaped_strings, escaped_forward_slashes])
        )},
        {"do not maybe_escape forward slash", ?_assertEqual(
            <<"/">>,
            maybe_escape(<<34, "/"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape backslash", ?_assertEqual(
            <<"\\\\">>,
            maybe_escape(<<34, "\\\\"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape jsonp (u2028)", ?_assertEqual(
            <<"\\u2028">>,
            maybe_escape(<<34, 16#2028/utf8, 34>>, [escaped_strings])
        )},
        {"do not maybe_escape jsonp (u2028)", ?_assertEqual(
            <<16#2028/utf8>>,
            maybe_escape(<<34, 16#2028/utf8, 34>>, [escaped_strings, unescaped_jsonp])
        )},
        {"maybe_escape jsonp (u2029)", ?_assertEqual(
            <<"\\u2029">>,
            maybe_escape(<<34, 16#2029/utf8, 34>>, [escaped_strings])
        )},
        {"do not maybe_escape jsonp (u2029)", ?_assertEqual(
            <<16#2029/utf8>>,
            maybe_escape(<<34, 16#2029/utf8, 34>>, [escaped_strings, unescaped_jsonp])
        )},
        {"maybe_escape u0000", ?_assertEqual(
            <<"\\u0000">>,
            maybe_escape(<<34, "\\u0000"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0001", ?_assertEqual(
            <<"\\u0001">>,
            maybe_escape(<<34, "\\u0001"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0002", ?_assertEqual(
            <<"\\u0002">>,
            maybe_escape(<<34, "\\u0002"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0003", ?_assertEqual(
            <<"\\u0003">>,
            maybe_escape(<<34, "\\u0003"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0004", ?_assertEqual(
            <<"\\u0004">>,
            maybe_escape(<<34, "\\u0004"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0005", ?_assertEqual(
            <<"\\u0005">>,
            maybe_escape(<<34, "\\u0005"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0006", ?_assertEqual(
            <<"\\u0006">>,
            maybe_escape(<<34, "\\u0006"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0007", ?_assertEqual(
            <<"\\u0007">>,
            maybe_escape(<<34, "\\u0007"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u000b", ?_assertEqual(
            <<"\\u000b">>,
            maybe_escape(<<34, "\\u000b"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u000e", ?_assertEqual(
            <<"\\u000e">>,
            maybe_escape(<<34, "\\u000e"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u000f", ?_assertEqual(
            <<"\\u000f">>,
            maybe_escape(<<34, "\\u000f"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0010", ?_assertEqual(
            <<"\\u0010">>,
            maybe_escape(<<34, "\\u0010"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0011", ?_assertEqual(
            <<"\\u0011">>,
            maybe_escape(<<34, "\\u0011"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0012", ?_assertEqual(
            <<"\\u0012">>,
            maybe_escape(<<34, "\\u0012"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0013", ?_assertEqual(
            <<"\\u0013">>,
            maybe_escape(<<34, "\\u0013"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0014", ?_assertEqual(
            <<"\\u0014">>,
            maybe_escape(<<34, "\\u0014"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0015", ?_assertEqual(
            <<"\\u0015">>,
            maybe_escape(<<34, "\\u0015"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0016", ?_assertEqual(
            <<"\\u0016">>,
            maybe_escape(<<34, "\\u0016"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0017", ?_assertEqual(
            <<"\\u0017">>,
            maybe_escape(<<34, "\\u0017"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0018", ?_assertEqual(
            <<"\\u0018">>,
            maybe_escape(<<34, "\\u0018"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u0019", ?_assertEqual(
            <<"\\u0019">>,
            maybe_escape(<<34, "\\u0019"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u001a", ?_assertEqual(
            <<"\\u001a">>,
            maybe_escape(<<34, "\\u001a"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u001b", ?_assertEqual(
            <<"\\u001b">>,
            maybe_escape(<<34, "\\u001b"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u001c", ?_assertEqual(
            <<"\\u001c">>,
            maybe_escape(<<34, "\\u001c"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u001d", ?_assertEqual(
            <<"\\u001d">>,
            maybe_escape(<<34, "\\u001d"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u001e", ?_assertEqual(
            <<"\\u001e">>,
            maybe_escape(<<34, "\\u001e"/utf8, 34>>, [escaped_strings])
        )},
        {"maybe_escape u001f", ?_assertEqual(
            <<"\\u001f">>,
            maybe_escape(<<34, "\\u001f"/utf8, 34>>, [escaped_strings])
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
        )},
        {"escaped single quote", ?_assertEqual(
            [{string, <<"quoth the raven, 'nevermore'">>}, end_json],
            decode(<<39, "quoth the raven, \\'nevermore\\'", 39>>, [single_quoted_strings])
        )},
        {"single quoted key", ?_assertEqual(
            [start_object,
                {key, <<"key">>}, {string, <<"value">>},
                {key, <<"another key">>}, {string, <<"another value">>},
            end_object, end_json],
            decode(<<"{'key':'value','another key':'another value'}">>, [single_quoted_strings])
        )}
    ].


ignored_bad_escapes_test_() ->
    [
        {"ignore unrecognized escape sequence", ?_assertEqual(
            [{string, <<"\\x25">>}, end_json],
            decode(<<"\"\\x25\"">>, [ignored_bad_escapes])
        )}
    ].


bom_test_() ->
    [
        {"bom", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<16#ef, 16#bb, 16#bf, "[]"/utf8>>, [])
        )}
    ].


error_test_() ->
    Decode = fun(JSON, Config) -> start(JSON, {jsx, []}, [], jsx_config:parse_config(Config)) end,
    [
        {"maybe_bom error", ?_assertError(
            badarg,
            Decode(<<16#ef, 0>>, [])
        )},
        {"definitely_bom error", ?_assertError(
            badarg,
            Decode(<<16#ef, 16#bb, 0>>, [])
        )},
        {"value error", ?_assertError(
            badarg,
            Decode(<<0>>, [])
        )},
        {"object error", ?_assertError(
            badarg,
            Decode(<<"{"/utf8, 0>>, [])
        )},
        {"colon error", ?_assertError(
            badarg,
            Decode(<<"{\"\""/utf8, 0>>, [])
        )},
        {"key error", ?_assertError(
            badarg,
            Decode(<<"{\"\":1,"/utf8, 0>>, [])
        )},
        {"negative error", ?_assertError(
            badarg,
            Decode(<<"-"/utf8, 0>>, [])
        )},
        {"zero error", ?_assertError(
            badarg,
            Decode(<<"0"/utf8, 0>>, [explicit_end])
        )},
        {"integer error", ?_assertError(
            badarg,
            Decode(<<"1"/utf8, 0>>, [explicit_end])
        )},
        {"decimal error", ?_assertError(
            badarg,
            Decode(<<"1.0"/utf8, 0>>, [explicit_end])
        )},
        {"exp error", ?_assertError(
            badarg,
            Decode(<<"1.0e1"/utf8, 0>>, [explicit_end])
        )},
        {"e error", ?_assertError(
            badarg,
            Decode(<<"1e"/utf8, 0>>, [])
        )},
        {"ex error", ?_assertError(
            badarg,
            Decode(<<"1e+"/utf8, 0>>, [])
        )},
        {"exp error", ?_assertError(
            badarg,
            Decode(<<"1.e"/utf8>>, [])
        )},
        {"true error", ?_assertError(
            badarg,
            Decode(<<"tru"/utf8, 0>>, [])
        )},
        {"false error", ?_assertError(
            badarg,
            Decode(<<"fals"/utf8, 0>>, [])
        )},
        {"null error", ?_assertError(
            badarg,
            Decode(<<"nul"/utf8, 0>>, [])
        )},
        {"maybe_done error", ?_assertError(
            badarg,
            Decode(<<"[[]"/utf8, 0>>, [])
        )},
        {"done error", ?_assertError(
            badarg,
            Decode(<<"[]"/utf8, 0>>, [])
        )},
        {"comment error", ?_assertError(
            badarg,
            Decode(<<"[ / ]">>, [comments])
        )},
        {"single_comment error", ?_assertError(
            badarg,
            Decode(<<"[ //"/utf8, 192>>, [comments])
        )},
        {"multi_comment error", ?_assertError(
            badarg,
            Decode(<<"[ /*"/utf8, 192>>, [comments])
        )}
    ].


custom_error_handler_test_() ->
    Decode = fun(JSON, Config) -> start(JSON, {jsx, []}, [], jsx_config:parse_config(Config)) end,
    Error = fun(Rest, {_, State, _, _, _}, _) -> {State, Rest} end,
    [
        {"maybe_bom error", ?_assertEqual(
            {value, <<16#ef, 0>>},
            Decode(<<16#ef, 0>>, [{error_handler, Error}])
        )},
        {"definitely_bom error", ?_assertEqual(
            {value, <<16#ef, 16#bb, 0>>},
            Decode(<<16#ef, 16#bb, 0>>, [{error_handler, Error}])
        )},
        {"value error", ?_assertEqual(
            {value, <<0>>},
            Decode(<<0>>, [{error_handler, Error}])
        )},
        {"object error", ?_assertEqual(
            {object, <<0>>},
            Decode(<<"{"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"colon error", ?_assertEqual(
            {colon, <<0>>},
            Decode(<<"{\"\""/utf8, 0>>, [{error_handler, Error}])
        )},
        {"key error", ?_assertEqual(
            {key, <<0>>},
            Decode(<<"{\"\":1,"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"negative error", ?_assertEqual(
            {value, <<"-"/utf8, 0>>},
            Decode(<<"-"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"zero error", ?_assertEqual(
            {value, <<"0"/utf8, 0>>},
            Decode(<<"0"/utf8, 0>>, [explicit_end, {error_handler, Error}])
        )},
        {"integer error", ?_assertEqual(
            {integer, <<0>>},
            Decode(<<"1"/utf8, 0>>, [explicit_end, {error_handler, Error}])
        )},
        {"decimal error", ?_assertEqual(
            {decimal, <<0>>},
            Decode(<<"1.0"/utf8, 0>>, [explicit_end, {error_handler, Error}])
        )},
        {"exp error", ?_assertEqual(
            {exp, <<0>>},
            Decode(<<"1.0e1"/utf8, 0>>, [explicit_end, {error_handler, Error}])
        )},
        {"e error", ?_assertEqual(
            {decimal, <<$e, 0>>},
            Decode(<<"1e"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"ex error", ?_assertEqual(
            {decimal, <<$e, ?positive, 0>>},
            Decode(<<"1e+"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"exp error", ?_assertEqual(
            {decimal, <<$e>>},
            Decode(<<"1.e"/utf8>>, [{error_handler, Error}])
        )},
        {"true error", ?_assertEqual(
            {true, <<"ru"/utf8, 0>>},
            Decode(<<"tru"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"false error", ?_assertEqual(
            {false, <<"als"/utf8, 0>>},
            Decode(<<"fals"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"null error", ?_assertEqual(
            {null, <<"ul"/utf8, 0>>},
            Decode(<<"nul"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"maybe_done error", ?_assertEqual(
            {maybe_done, <<0>>},
            Decode(<<"[[]"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"done error", ?_assertEqual(
            {done, <<0>>},
            Decode(<<"[]"/utf8, 0>>, [{error_handler, Error}])
        )},
        {"comment error", ?_assertEqual(
            {value, <<"/ ]"/utf8>>},
            Decode(<<"[ / ]">>, [{error_handler, Error}, comments])
        )},
        {"single_comment error", ?_assertEqual(
            {comment, <<192>>},
            Decode(<<"[ //"/utf8, 192>>, [{error_handler, Error}, comments])
        )},
        {"multi_comment error", ?_assertEqual(
            {comment, <<192>>},
            Decode(<<"[ /*"/utf8, 192>>, [{error_handler, Error}, comments])
        )}
    ].


custom_incomplete_handler_test_() ->
    Decode = fun(JSON, Config) -> start(JSON, {jsx, []}, [], jsx_config:parse_config(Config)) end,
    [
        {"custom incomplete handler", ?_assertError(
            badarg,
            Decode(<<>>, [{incomplete_handler, fun(_, _, _) -> erlang:error(badarg) end}])
        )}
    ].


-endif.
