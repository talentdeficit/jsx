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
        zero -> zero(Rest, Handler, Acc, Stack, Config);
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


incomplete(State, Rest, Handler, Stack, Config = #config{stream=false}) ->
    ?error(State, Rest, Handler, Stack, Config);
incomplete(State, Rest, Handler, Stack, Config) ->
    incomplete(State, Rest, Handler, unused, Stack, Config).

    
incomplete(State, Rest, Handler, Acc, Stack, Config = #config{stream=false}) ->
    ?error(State, Rest, Handler, Acc, Stack, Config);
incomplete(State, Rest, Handler, Acc, Stack, Config = #config{incomplete_handler=false}) ->
    {incomplete, fun(Stream) when is_binary(Stream) ->
                resume(<<Rest/binary, Stream/binary>>, State, Handler, Acc, Stack, Config);
            (end_stream) ->
                case resume(<<Rest/binary, ?space/utf8>>, State, Handler, Acc, Stack, Config#config{stream=false}) of
                    {incomplete, _} -> ?error(State, Rest, Handler, Acc, Stack, Config);
                    Else -> Else
                end
        end
    };
incomplete(State, Rest, Handler, Acc, Stack, Config = #config{incomplete_handler=F}) ->
    F(Rest, {decoder, State, Handler, Acc, Stack}, jsx_config:config_to_list(Config)).


%% lists are benchmarked to be faster (tho higher in memory usage) than binaries
new_seq() -> [].
new_seq(C) -> [C].

acc_seq(Seq, C) when is_list(C) -> lists:reverse(C) ++ Seq;
acc_seq(Seq, C) -> [C] ++ Seq.

end_seq(Seq) -> unicode:characters_to_binary(lists:reverse(Seq)).

end_seq(Seq, #config{dirty_strings=true}) -> list_to_binary(lists:reverse(Seq));
end_seq(Seq, _) -> end_seq(Seq).


handle_event(Event, {Handler, State}, _Config) -> {Handler, Handler:handle_event(Event, State)}.


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
value(<<?singlequote, Rest/binary>>, Handler, Stack, Config=#config{strict_single_quotes=false}) ->
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
value(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{strict_comments=true}) ->
    ?error(value, <<?solidus, Rest/binary>>, Handler, Stack, Config);
value(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, value, [comment|Stack], Config);
value(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, value, [multicomment|Stack], Config);
value(<<?solidus>>, Handler, Stack, Config) ->
    incomplete(value, <<?solidus>>, Handler, Stack, Config);
value(<<>>, Handler, Stack, Config) ->
    incomplete(value, <<>>, Handler, Stack, Config);
value(Bin, Handler, Stack, Config) ->
    ?error(value, Bin, Handler, Stack, Config).


object(<<?doublequote, Rest/binary>>, Handler, Stack, Config) ->
    string(Rest, Handler, new_seq(), Stack, Config);
object(<<?singlequote, Rest/binary>>, Handler, Stack, Config=#config{strict_single_quotes=false}) ->
    string(Rest, Handler, new_seq(), [singlequote|Stack], Config);
object(<<?end_object, Rest/binary>>, Handler, [key|Stack], Config) ->
    maybe_done(Rest, handle_event(end_object, Handler, Config), Stack, Config);
object(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    object(Rest, Handler, Stack, Config);
object(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{strict_comments=true}) ->
    ?error(object, <<?solidus, Rest/binary>>, Handler, Stack, Config);
object(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, object, [comment|Stack], Config);
object(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, object, [multicomment|Stack], Config);
object(<<?solidus>>, Handler, Stack, Config) ->
    incomplete(object, <<?solidus>>, Handler, Stack, Config);
object(<<>>, Handler, Stack, Config) ->
    incomplete(object, <<>>, Handler, Stack, Config);
object(Bin, Handler, Stack, Config) ->
    ?error(object, Bin, Handler, Stack, Config).


array(<<?end_array, Rest/binary>>, Handler, [array|Stack], Config) ->
    maybe_done(Rest, handle_event(end_array, Handler, Config), Stack, Config);
array(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    array(Rest, Handler, Stack, Config);
array(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{strict_comments=true}) ->
    value(<<?solidus, Rest/binary>>, Handler, Stack, Config);
array(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, array, [comment|Stack], Config);
array(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, array, [multicomment|Stack], Config);
array(<<?solidus>>, Handler, Stack, Config) ->
    incomplete(array, <<?solidus>>, Handler, Stack, Config);
array(<<>>, Handler, Stack, Config) ->
    incomplete(array, <<>>, Handler, Stack, Config);
array(Bin, Handler, Stack, Config) ->
    value(Bin, Handler, Stack, Config).


colon(<<?colon, Rest/binary>>, Handler, [key|Stack], Config) ->
    value(Rest, Handler, [object|Stack], Config);
colon(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    colon(Rest, Handler, Stack, Config);
colon(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{strict_comments=true}) ->
    ?error(colon, <<?solidus, Rest/binary>>, Handler, Stack, Config);
colon(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, colon, [comment|Stack], Config);
colon(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, colon, [multicomment|Stack], Config);
colon(<<?solidus>>, Handler, Stack, Config) ->
    incomplete(colon, <<?solidus>>, Handler, Stack, Config);
colon(<<>>, Handler, Stack, Config) ->
    incomplete(colon, <<>>, Handler, Stack, Config);
colon(Bin, Handler, Stack, Config) ->
    ?error(colon, Bin, Handler, Stack, Config).


key(<<?doublequote, Rest/binary>>, Handler, Stack, Config) ->
    string(Rest, Handler, new_seq(), Stack, Config);
key(<<?singlequote, Rest/binary>>, Handler, Stack, Config=#config{strict_single_quotes=false}) ->
    string(Rest, Handler, new_seq(), [singlequote|Stack], Config);
key(<<S, Rest/binary>>, Handler, Stack, Config) when ?is_whitespace(S) ->
    key(Rest, Handler, Stack, Config);
key(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{strict_comments=true}) ->
    ?error(key, <<?solidus, Rest/binary>>, Handler, Stack, Config);
key(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, key, [comment|Stack], Config);
key(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, key, [multicomment|Stack], Config);
key(<<?solidus>>, Handler, Stack, Config) ->
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
string(<<237, X, _, Rest/binary>>, Handler, Acc, Stack, Config=#config{strict_utf8=false})
        when X >= 160 ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
%% u+xfffe, u+xffff, control codes and other noncharacters
string(<<_/utf8, Rest/binary>>, Handler, Acc, Stack, Config=#config{strict_utf8=false}) ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
%% u+fffe and u+ffff for R14BXX (subsequent runtimes will happily match the
%%  preceeding clause
string(<<239, 191, X, Rest/binary>>, Handler, Acc, Stack, Config=#config{strict_utf8=false})
        when X == 190; X == 191 ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
%% overlong encodings and missing continuations of a 2 byte sequence
string(<<X, Rest/binary>>, Handler, Acc, Stack, Config=#config{strict_utf8=false})
        when X >= 192, X =< 223 ->
    strip_continuations(Rest, Handler, Acc, Stack, Config, 1);
%% overlong encodings and missing continuations of a 3 byte sequence
string(<<X, Rest/binary>>, Handler, Acc, Stack, Config=#config{strict_utf8=false})
        when X >= 224, X =< 239 ->
    strip_continuations(Rest, Handler, Acc, Stack, Config, 2);
%% overlong encodings and missing continuations of a 4 byte sequence
string(<<X, Rest/binary>>, Handler, Acc, Stack, Config=#config{strict_utf8=false})
        when X >= 240, X =< 247 ->
    strip_continuations(Rest, Handler, Acc, Stack, Config, 3);
%% incompletes and unexpected bytes, including orphan continuations
string(<<_, Rest/binary>>, Handler, Acc, Stack, Config=#config{strict_utf8=false}) ->
    string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config);
string(Bin, Handler, Acc, Stack, Config) -> ?error(string, Bin, Handler, Acc, Stack, Config).


doublequote(Rest, Handler, Acc, [key|_] = Stack, Config) ->
    colon(Rest, handle_event({key, end_seq(Acc, Config)}, Handler, Config), Stack, Config);
doublequote(Rest, Handler, Acc, [singlequote|_] = Stack, Config) ->
    string(Rest, Handler,acc_seq(Acc, maybe_replace(?doublequote, Config)), Stack, Config);
doublequote(<<>>, Handler, Acc, [singlequote|_] = Stack, Config) ->
    incomplete(string, <<?doublequote>>, Handler, Acc, Stack, Config);
doublequote(Rest, Handler, Acc, Stack, Config) ->
    maybe_done(Rest, handle_event({string, end_seq(Acc, Config)}, Handler, Config), Stack, Config).


singlequote(Rest, Handler, Acc, [singlequote, key|Stack], Config) ->
    colon(Rest, handle_event({key, end_seq(Acc, Config)}, Handler, Config), [key|Stack], Config);
singlequote(Rest, Handler, Acc, [singlequote|Stack], Config) ->
    maybe_done(Rest, handle_event({string, end_seq(Acc, Config)}, Handler, Config), Stack, Config);
singlequote(Rest, Handler, Acc, Stack, Config) ->
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
unescape(<<?rsolidus, Rest/binary>>, Handler, Acc, Stack, Config=#config{dirty_strings=true}) ->
    string(<<?rsolidus, Rest/binary>>, Handler, acc_seq(Acc, ?rsolidus), Stack, Config);
unescape(<<C, Rest/binary>>, Handler, Acc, Stack, Config=#config{dirty_strings=true}) ->
    string(Rest, Handler, acc_seq(Acc, [?rsolidus, C]), Stack, Config);
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
unescape(<<?singlequote, Rest/binary>>, Handler, Acc, Stack, Config=#config{strict_single_quotes=false}) ->
    string(Rest, Handler, acc_seq(Acc, ?singlequote), Stack, Config);
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
    case Config#config.strict_utf8 of
        true -> ?error(<<$u, $d, A, B, C, ?rsolidus, $u, W, X, Y, Z, Rest/binary>>, Handler, Acc, Stack, Config);
        false -> string(Rest, Handler, acc_seq(Acc, [16#fffd, 16#fffd]), Stack, Config)
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
        _ when Config#config.strict_utf8 ->
            ?error(string, <<?rsolidus, $u, A, B, C, D, Rest/binary>>, Handler, Acc, Stack, Config);
        _ -> string(Rest, Handler, acc_seq(Acc, 16#fffd), Stack, Config)
    end;
unescape(Bin, Handler, Acc, Stack, Config) ->
    case is_partial_escape(Bin) of
        true -> incomplete(string, <<?rsolidus/utf8, Bin/binary>>, Handler, Acc, Stack, Config);
        false -> case Config#config.strict_escapes of
                true -> ?error(string, <<?rsolidus, Bin/binary>>, Handler, Acc, Stack, Config);
                false -> string(Bin, Handler, acc_seq(Acc, ?rsolidus), Stack, Config)
            end
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


finish_number(Rest, Handler, Acc, [], Config=#config{stream=false}) ->
    maybe_done(Rest, handle_event(format_number(Acc), Handler, Config), [], Config);
finish_number(<<>>, Handler, {NumType, Acc}, Stack, Config) ->
    incomplete(NumType, <<>>, Handler, Acc, Stack, Config);
finish_number(Rest, Handler, Acc, Stack, Config) ->
    maybe_done(Rest, handle_event(format_number(Acc), Handler, Config), Stack, Config).


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
comment(<<_, Rest/binary>>, Handler, Resume, Stack, Config=#config{strict_utf8=false}) ->
    comment(Rest, Handler, Resume, Stack, Config);
comment(<<>>, Handler, done, [Comment], Config=#config{stream=false})
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
maybe_done(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{strict_comments=true}) ->
    ?error(maybe_done, <<?solidus, Rest/binary>>, Handler, Stack, Config);
maybe_done(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, maybe_done, [comment|Stack], Config);
maybe_done(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, maybe_done, [multicomment|Stack], Config);
maybe_done(<<?solidus>>, Handler, Stack, Config) ->
    incomplete(maybe_done, <<?solidus>>, Handler, Stack, Config);
maybe_done(<<>>, Handler, Stack, Config) when length(Stack) > 0 ->
    incomplete(maybe_done, <<>>, Handler, Stack, Config);
maybe_done(Bin, Handler, Stack, Config) ->
    ?error(maybe_done, Bin, Handler, Stack, Config).


done(<<S, Rest/binary>>, Handler, [], Config) when ?is_whitespace(S) ->
    done(Rest, Handler, [], Config);
done(<<?solidus, Rest/binary>>, Handler, Stack, Config=#config{strict_comments=true}) ->
    ?error(done, <<?solidus, Rest/binary>>, Handler, Stack, Config);
done(<<?solidus, ?solidus, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, done, [comment|Stack], Config);
done(<<?solidus, ?star, Rest/binary>>, Handler, Stack, Config) ->
    comment(Rest, Handler, done, [multicomment|Stack], Config);
done(<<?solidus>>, Handler, Stack, Config) ->
    incomplete(done, <<?solidus>>, Handler, Stack, Config);
done(<<>>, {Handler, State}, [], Config=#config{stream=true}) ->
    incomplete(done, <<>>, {Handler, State}, [], Config);
done(<<>>, {_Handler, State}, [], _Config) -> State;
done(Bin, Handler, Stack, Config) -> ?error(done, Bin, Handler, Stack, Config).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


json_to_bytes(JSON) -> json_to_bytes(JSON, []).

json_to_bytes(<<>>, Acc) -> [<<>>] ++ lists:reverse(Acc);
json_to_bytes(<<X, Rest/binary>>, Acc) -> json_to_bytes(Rest, [<<X>>] ++ Acc).


decode(JSON) -> decode(JSON, []).
decode(JSON, Config) -> (decoder(jsx, [], Config))(JSON).


incremental_decode(JSON) -> incremental_decode(JSON, []).
incremental_decode(JSON, Config) ->
    Final = lists:foldl(
        fun(Byte, Decoder) -> {incomplete, F} = Decoder(Byte), F end,
        decoder(jsx, [], [stream] ++ Config),
        json_to_bytes(JSON)
    ),
    Final(end_stream).


%% all these numbers have different representation in erlang than in javascript and
%%  do not roundtrip like most integers/floats
special_number_test_() ->
    Cases = [
        % {title, test form, json, opt flags}
        {"-0", [{integer, 0}, end_json], <<"-0">>},
        {"-0.0", [{float, 0.0}, end_json], <<"-0.0">>},
        {"0e0", [{float, 0.0}, end_json], <<"0e0">>},
        {"0e4", [{float, 0.0}, end_json], <<"0e4">>},
        {"1e0", [{float, 1.0}, end_json], <<"1e0">>},
        {"-1e0", [{float, -1.0}, end_json], <<"-1e0">>},
        {"1e4", [{float, 1.0e4}, end_json], <<"1e4">>},
        {"number terminated by whitespace", 
            [start_array, {integer, 1}, end_array, end_json],
            <<"[ 1 ]">>
        },
        {"number terminated by comma",
            [start_array, {integer, 1}, {integer, 1}, end_array, end_json],
            <<"[ 1, 1 ]">>
        },
        {"number terminated by comma in object",
            [start_object, {key, <<"x">>}, {integer, 1}, {key, <<"y">>}, {integer, 1}, end_object, end_json],
            <<"{\"x\": 1, \"y\": 1}">>
        }
    ],
    [{Title, ?_assertEqual(Events, decode(JSON))}
        || {Title, Events, JSON} <- Cases
    ] ++
    [{Title ++ " (incremental)", ?_assertEqual(Events, incremental_decode(JSON))}
        || {Title, Events, JSON} <- Cases
    ]. 


comments_test_() ->
    Cases = [
        % {title, test form, json, opt flags}
        {"preceeding // comment",
            [start_array, end_array, end_json],
            <<"// comment ", ?newline, "[]">>
        },
        {"preceeding /**/ comment",
            [start_array, end_array, end_json],
            <<"/* comment */[]">>
        },
        {"trailing // comment",
            [start_array, end_array, end_json],
            <<"[]// comment", ?newline>>
        },
        {"trailing // comment (no newline)",
            [start_array, end_array, end_json],
            <<"[]// comment">>
        },
        {"trailing /**/ comment",
            [start_array, end_array, end_json],
            <<"[] /* comment */">>
        },
        {"// comment inside array",
            [start_array, end_array, end_json],
            <<"[ // comment", ?newline, "]">>
        },
        {"/**/ comment inside array",
            [start_array, end_array, end_json],
            <<"[ /* comment */ ]">>
        },
        {"// comment at beginning of array",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ // comment", ?newline, "true", ?newline, "]">>
        },
        {"/**/ comment at beginning of array",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ /* comment */ true ]">>
        },
        {"// comment at end of array",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ true // comment", ?newline, "]">>
        },
        {"/**/ comment at end of array",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ true /* comment */ ]">>
        },
        {"// comment midarray (post comma)",
            [start_array, {literal, true}, {literal, false}, end_array, end_json],
            <<"[ true, // comment", ?newline, "false ]">>
        },
        {"/**/ comment midarray (post comma)",
            [start_array, {literal, true}, {literal, false}, end_array, end_json],
            <<"[ true, /* comment */ false ]">>
        },
        {"// comment midarray (pre comma)",
            [start_array, {literal, true}, {literal, false}, end_array, end_json],
            <<"[ true// comment", ?newline, ", false ]">>
        },
        {"/**/ comment midarray (pre comma)",
            [start_array, {literal, true}, {literal, false}, end_array, end_json],
            <<"[ true/* comment */, false ]">>
        },
        {"// comment inside object",
            [start_object, end_object, end_json],
            <<"{ // comment", ?newline, "}">>
        },
        {"/**/ comment inside object",
            [start_object, end_object, end_json],
            <<"{ /* comment */ }">>
        },
        {"// comment at beginning of object",
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            <<"{ // comment", ?newline, " \"key\": true", ?newline, "}">>
        },
        {"/**/ comment at beginning of object",
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            <<"{ /* comment */ \"key\": true }">>
        },
        {"// comment at end of object",
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            <<"{ \"key\": true // comment", ?newline, "}">>
        },
        {"/**/ comment at end of object",
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            <<"{ \"key\": true /* comment */ }">>
        },
        {"// comment midobject (post comma)",
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ],
            <<"{ \"x\": true, // comment", ?newline, "\"y\": false }">>
        },
        {"/**/ comment midobject (post comma)",
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ],
            <<"{ \"x\": true, /* comment */", ?newline, "\"y\": false }">>
        },
        {"// comment midobject (pre comma)",
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ],
            <<"{ \"x\": true// comment", ?newline, ", \"y\": false }">>
        },
        {"/**/ comment midobject (pre comma)",
            [
                start_object,
                {key, <<"x">>},
                {literal, true},
                {key, <<"y">>},
                {literal, false},
                end_object,
                end_json
            ],
            <<"{ \"x\": true/* comment */", ?newline, ", \"y\": false }">>
        },
        {"// comment precolon",
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            <<"{ \"key\" // comment", ?newline, ": true }">>
        },
        {"/**/ comment precolon",
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            <<"{ \"key\"/* comment */: true }">>
        },
        {"// comment postcolon",
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            <<"{ \"key\": // comment", ?newline, " true }">>
        },
        {"/**/ comment postcolon",
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json],
            <<"{ \"key\":/* comment */ true }">>
        },
        {"// comment terminating zero",
            [start_array, {integer, 0}, end_array, end_json],
            <<"[ 0// comment", ?newline, "]">>
        },
        {"// comment terminating integer",
            [start_array, {integer, 1}, end_array, end_json],
            <<"[ 1// comment", ?newline, "]">>
        },
        {"// comment terminating float",
            [start_array, {float, 1.0}, end_array, end_json],
            <<"[ 1.0// comment", ?newline, "]">>
        },
        {"// comment terminating exp",
            [start_array, {float, 1.0e1}, end_array, end_json],
            <<"[ 1e1// comment", ?newline, "]">>
        },
        {"/**/ comment terminating zero",
            [start_array, {integer, 0}, end_array, end_json],
            <<"[ 0/* comment */ ]">>
        },
        {"/**/ comment terminating integer",
            [start_array, {integer, 1}, end_array, end_json],
            <<"[ 1/* comment */ ]">>
        },
        {"/**/ comment terminating float",
            [start_array, {float, 1.0}, end_array, end_json],
            <<"[ 1.0/* comment */ ]">>
        },
        {"/**/ comment terminating exp",
            [start_array, {float, 1.0e1}, end_array, end_json],
            <<"[ 1e1/* comment */ ]">>
        },
        {"/**/ comment following /**/ comment",
            [start_array, {literal, true}, end_array, end_json],
            <<"[/* comment *//* comment */true]">>
        },
        {"/**/ comment following // comment",
            [start_array, {literal, true}, end_array, end_json],
            <<"[// comment", ?newline, "/* comment */true]">>
        },
        {"// comment following /**/ comment",
            [start_array, {literal, true}, end_array, end_json],
            <<"[/* comment */// comment", ?newline, "true]">>
        },
        {"// comment following // comment",
            [start_array, {literal, true}, end_array, end_json],
            <<"[// comment", ?newline, "// comment", ?newline, "true]">>
        },
        {"/**/ comment inside /**/ comment",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ /* /* comment */ */ true ]">>
        },
        {"/**/ comment with /",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ /* / */ true ]">>
        },
        {"/**/ comment with *",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ /* * */ true ]">>
        },
        {"// comment with badutf",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ // comment ", 16#00c0, " ", ?newline, "true]">>
        },
        {"/**/ comment with badutf",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ /* comment ", 16#00c0, " */ true]">>
        },
        {"/**/ comment with badutf preceeded by /",
            [start_array, {literal, true}, end_array, end_json],
            <<"[ /* comment /", 16#00c0, " */ true]">>
        }
    ],
    [{Title, ?_assertEqual(Events, decode(JSON))}
        || {Title, Events, JSON} <- Cases
    ] ++
    [{Title ++ " (incremental)", ?_assertEqual(Events, incremental_decode(JSON))}
        || {Title, Events, JSON} <- Cases
    ] ++
    % error when `{strict, [comments]}` is present
    [{Title, ?_assertError(badarg, decode(JSON, [{strict, [comments]}]))}
        || {Title, _Events, JSON} <- Cases
    ] ++
    [{Title ++ " (incremental)", ?_assertError(
            badarg,
            incremental_decode(JSON, [{strict, [comments]}])
        )} || {Title, _Events, JSON} <- Cases
    ].


no_comments_test_() ->
    Cases = [
        {"// comment with badutf",
            badarg,
            <<"[ // comment ", 16#00c0, " ", ?newline, "true]">>,
            [{strict, [utf8]}]
        },
        {"/**/ comment with badutf",
            badarg,
            <<"[ /* comment ", 16#00c0, " */ true]">>,
            [{strict, [utf8]}]
        },
        {"/**/ comment with badutf preceeded by /",
            badarg,
            <<"[ /* comment /", 16#00c0, " */ true]">>,
            [{strict, [utf8]}]
        }
    ],
    [{Title, ?_assertError(Error, decode(JSON, Config))}
        || {Title, Error, JSON, Config} <- Cases
    ] ++
    [{Title ++ " (incremental)", ?_assertError(Error, incremental_decode(JSON, Config))}
        || {Title, Error, JSON, Config} <- Cases
    ].


% doing the full unicode range takes foreverrrrrrr so just do boundaries
% excludes characters that may need escaping
codepoints() ->
    [32, 33] ++
        lists:seq(35, 46) ++
        lists:seq(48, 91) ++
        lists:seq(93, 127) ++
        [16#2027, 16#202a, 16#d7ff, 16#e000, 16#fdcf, 16#fdf0, 16#fffd] ++
        [16#10000, 16#1fffd, 16#20000, 16#30000, 16#40000, 16#50000] ++
        [16#60000, 16#70000, 16#80000, 16#90000, 16#a0000, 16#b0000] ++
    [16#c0000, 16#d0000, 16#e0000, 16#f0000, 16#100000].

reserved_space() -> lists:seq(16#fdd0, 16#fdef).

surrogates() -> lists:seq(16#d800, 16#dfff).

noncharacters() -> lists:seq(16#fffe, 16#ffff).

extended_noncharacters() ->
    [16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff] ++
        [16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff] ++
        [16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff] ++
        [16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff] ++
        [16#9fffe, 16#9ffff, 16#afffe, 16#affff] ++
        [16#bfffe, 16#bffff, 16#cfffe, 16#cffff] ++
        [16#dfffe, 16#dffff, 16#efffe, 16#effff] ++
    [16#ffffe, 16#fffff, 16#10fffe, 16#10ffff].


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
    Clean = codepoints(),
    Dirty = reserved_space() ++ surrogates() ++ noncharacters() ++ extended_noncharacters(),
    % clean codepoints
    [{"clean u+" ++ integer_to_list(Codepoint, 16), ?_assertEqual(
            [{string, <<Codepoint/utf8>>}, end_json],
            decode(<<34/utf8, Codepoint/utf8, 34/utf8>>)
        )} || Codepoint <- Clean
    ] ++
    % bad codepoints replaced by u+FFFD
    [{"clean u+" ++ integer_to_list(Codepoint, 16), ?_assertEqual(
            [{string, <<16#fffd/utf8>>}, end_json],
            decode(to_fake_utf8(Codepoint))
        )} || Codepoint <- Dirty
    ] ++
    % bad codepoints that cause errors
    [{"dirty u+" ++ integer_to_list(Codepoint, 16), ?_assertError(
            badarg,
            decode(to_fake_utf8(Codepoint), [{strict, [utf8]}])
        )} || Codepoint <- Dirty
    ].


dirty_string_test_() ->
    Cases = [
        {"dirty \\n",
            [start_array, {string, <<"\\n">>}, end_array, end_json],
            <<"[\"\\n\"]">>,
            [dirty_strings]
        },
        {"dirty \\uwxyz",
            [start_array, {string, <<"\\uwxyz">>}, end_array, end_json],
            <<"[\"\\uwxyz\"]">>,
            [dirty_strings]
        },
        {"dirty \\x23",
            [start_array, {string, <<"\\x23">>}, end_array, end_json],
            <<"[\"\\x23\"]">>,
            [dirty_strings]
        },
        {"dirty 0",
            [start_array, {string, <<0>>}, end_array, end_json],
            <<"[\"", 0, "\"]">>,
            [dirty_strings]
        },
        {"dirty 0\\\"0",
            [start_array, {string, <<0, ?rsolidus, ?doublequote, 0>>}, end_array, end_json],
            <<"[\"", 0, ?rsolidus, ?doublequote, 0, "\"]">>,
            [dirty_strings]
        },
        {"dirty 0\\\\\"0",
            [start_array, {string, <<0, ?rsolidus, ?rsolidus, ?doublequote, 0>>}, end_array, end_json],
            <<"[\"", 0, ?rsolidus, ?rsolidus, ?doublequote, 0, "\"]">>,
            [dirty_strings]
        },
        {"dirty 16#d800",
            [start_array, {string, <<237, 160, 128>>}, end_array, end_json],
            <<"[\"", 237, 160, 128, "\"]">>,
            [dirty_strings]
        },
        {"dirty 16#10ffff",
            [start_array, {string, <<244, 143, 191, 191>>}, end_array, end_json],
            <<"[\"", 244, 143, 191, 191, "\"]">>,
            [dirty_strings]
        },
        {"dirty /",
            [start_array, {string, <<$/>>}, end_array, end_json],
            <<"[\"", $/, "\"]">>,
            [dirty_strings, escaped_forward_slashes]
        },
        {"dirty <<194, 129>>",
            [start_array, {string, <<194, 129>>}, end_array, end_json],
            <<"[\"", 194, 129, "\"]">>,
            [dirty_strings]
        }
    ],
    [{Title, ?_assertEqual(Events, decode(JSON, Config))}
        || {Title, Events, JSON, Config} <- Cases
    ] ++
    % ensure `dirty_strings` and `strict` interact properly
    [{Title, ?_assertEqual(Events, decode(JSON, Config ++ [strict]))}
        || {Title, Events, JSON, Config} <- Cases
    ] ++
    [{Title ++ " (incremental)", ?_assertEqual(Events, incremental_decode(JSON, Config))}
        || {Title, Events, JSON, Config} <- Cases
    ].


bad_utf8_test_() ->
    Cases = [
        {"noncharacter u+fffe", <<16#fffd/utf8>>, <<239, 191, 190>>},
        {"noncharacter u+ffff", <<16#fffd/utf8>>, <<239, 191, 191>>},
        {"orphan continuation byte u+0080", <<16#fffd/utf8>>, <<16#0080>>},
        {"orphan continuation byte u+00bf", <<16#fffd/utf8>>, <<16#00bf>>},
        {"2 continuation bytes",
            binary:copy(<<16#fffd/utf8>>, 2),
            <<(binary:copy(<<16#0080>>, 2))/binary>>
        },
        {"3 continuation bytes",
            binary:copy(<<16#fffd/utf8>>, 3),
            <<(binary:copy(<<16#0080>>, 3))/binary>>
        },
        {"4 continuation bytes",
            binary:copy(<<16#fffd/utf8>>, 4),
            <<(binary:copy(<<16#0080>>, 4))/binary>>
        },
        {"5 continuation bytes",
            binary:copy(<<16#fffd/utf8>>, 5),
            <<(binary:copy(<<16#0080>>, 5))/binary>>
        },
        {"6 continuation bytes",
            binary:copy(<<16#fffd/utf8>>, 6),
            <<(binary:copy(<<16#0080>>, 6))/binary>>
        },
        {"all continuation bytes",
            binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf))),
            <<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>
        },
        {"lonely start byte", <<16#fffd/utf8>>, <<16#00c0>>},
        {"lonely start bytes (2 byte)",
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            <<16#00c0, 32, 16#00df>>
        },
        {"lonely start bytes (3 byte)",
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            <<16#00e0, 32, 16#00ef>>
        },
        {"lonely start bytes (4 byte)",
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            <<16#00f0, 32, 16#00f7>>
        },
        {"missing continuation byte (3 byte)", <<16#fffd/utf8, 32>>, <<224, 160, 32>>},
        {"missing continuation byte (4 byte missing one)",
            <<16#fffd/utf8, 32>>,
            <<240, 144, 128, 32>>
        },
        {"missing continuation byte (4 byte missing two)",
            <<16#fffd/utf8, 32>>,
            <<240, 144, 32>>
        },
        {"overlong encoding of u+002f (2 byte)",
            <<16#fffd/utf8, 32>>,
            <<16#c0, 16#af, 32>>
        },
        {"overlong encoding of u+002f (3 byte)",
            <<16#fffd/utf8, 32>>,
            <<16#e0, 16#80, 16#af, 32>>
        },
        {"overlong encoding of u+002f (4 byte)",
            <<16#fffd/utf8, 32>>,
            <<16#f0, 16#80, 16#80, 16#af, 32>>
        },
        {"highest overlong 2 byte sequence",
            <<16#fffd/utf8, 32>>,
            <<16#c1, 16#bf, 32>>
        },
        {"highest overlong 3 byte sequence",
            <<16#fffd/utf8, 32>>,
            <<16#e0, 16#9f, 16#bf, 32>>
        },
        {"highest overlong 4 byte sequence",
            <<16#fffd/utf8, 32>>,
            <<16#f0, 16#8f, 16#bf, 16#bf, 32>>
        }
    ],
    [{Title, ?_assertError(
            badarg,
            decode(<<34, JSON/binary, 34>>, [{strict, [utf8]}])
        )} || {Title, _, JSON} <- Cases
    ] ++
    [{Title ++ " (incremental)", ?_assertError(
            badarg,
            incremental_decode(<<34, JSON/binary, 34>>, [{strict, [utf8]}])
        )} || {Title, _, JSON} <- Cases
    ] ++
    [{Title ++ " replaced", ?_assertEqual(
            [{string, Replacement}, end_json],
            decode(<<34, JSON/binary, 34>>)
        )} || {Title, Replacement, JSON} <- Cases
    ] ++
    [{Title ++ " replaced (incremental)", ?_assertEqual(
            [{string, Replacement}, end_json],
            incremental_decode(<<34, JSON/binary, 34>>)
        )} || {Title, Replacement, JSON} <- Cases
    ].


unescape_test_() ->
    Cases = [
        {"unescape backspace", <<"\b">>, <<"\\b"/utf8>>},
        {"unescape tab", <<"\t">>, <<"\\t"/utf8>>},
        {"unescape newline", <<"\n">>, <<"\\n"/utf8>>},
        {"unescape formfeed", <<"\f">>, <<"\\f"/utf8>>},
        {"unescape carriage return", <<"\r">>, <<"\\r"/utf8>>},
        {"unescape quote", <<"\"">>, <<"\\\""/utf8>>},
        {"unescape solidus", <<"/">>, <<"\\/"/utf8>>},
        {"unescape reverse solidus", <<"\\">>, <<"\\\\"/utf8>>},
        {"unescape control", <<0>>, <<"\\u0000"/utf8>>},
        {"unescape surrogate pair", <<16#10000/utf8>>, <<"\\ud800\\udc00"/utf8>>},
        {"replace bad high surrogate", <<16#fffd/utf8>>, <<"\\udc00"/utf8>>},
        {"replace naked high surrogate",
            <<16#fffd/utf8, "hello world">>,
            <<"\\ud800hello world"/utf8>>
        },
        {"replace naked low surrogate",
            <<16#fffd/utf8, "hello world">>,
            <<"\\udc00hello world"/utf8>>
        },
        {"replace bad surrogate pair", <<16#fffd/utf8, 16#fffd/utf8>>, <<"\\ud800\\u0000">>}
    ],
    [{Title, ?_assertEqual([{string, Escaped}, end_json], decode(<<34, JSON/binary, 34>>))}
        || {Title, Escaped, JSON} <- Cases
    ] ++
    [{Title ++ " (incremental)", ?_assertEqual(
            [{string, Escaped}, end_json],
            incremental_decode(<<34, JSON/binary, 34>>)
        )} || {Title, Escaped, JSON} <- Cases
    ].


bad_escaped_surrogate_test_() ->
    Cases = [
        {"do not unescape bad high surrogate", <<"\\udc00">>},
        {"do not unescape naked high surrogate", <<"\\ud800hello world">>},
        {"do not unescape naked low surrogate", <<"\\udc00hello world">>},
        {"do not unescape bad surrogate pair", <<"\\ud800\\u0000">>}
    ],
    [{Title, ?_assertError(badarg, decode(<<34, JSON/binary, 34>>, [{strict, [utf8]}]))}
        || {Title, JSON} <- Cases
    ].


escape_test_() ->
    Cases = [
        {"backspace", <<"\b">>, <<"\\b">>},
        {"tab", <<"\t">>, <<"\\t">>},
        {"newline", <<"\n">>, <<"\\n">>},
        {"formfeed", <<"\f">>, <<"\\f">>},
        {"carriage return", <<"\r">>, <<"\\r">>},
        {"quote", <<"\"">>, <<"\\\"">>},
        {"backslash", <<"\\">>, <<"\\\\">>},
        {"control", <<0>>, <<"\\u0000">>}
    ],
    [{"escape " ++ Title, ?_assertEqual(
            [{string, Escaped}, end_json],
            decode(<<34, Escaped/binary, 34>>, [escaped_strings])
        )} || {Title, _Unescaped, Escaped} <- Cases
    ] ++
    [{"do not escape " ++ Title, ?_assertEqual(
            [{string, Unescaped}, end_json],
            decode(<<34, Escaped/binary, 34>>)
        )} || {Title, Unescaped, Escaped} <- Cases
    ].


special_escape_test_() ->
    Cases = [
        {"escape forward slash", <<"\\/">>, <<"/"/utf8>>, [escaped_forward_slashes]},
        {"do not escape forward slash", <<"/">>, <<"/"/utf8>>, []},
        {"escape jsonp", <<"\\u2028">>, <<16#2028/utf8>>, []},
        {"do not escape jsonp", <<16#2028/utf8>>, <<16#2028/utf8>>, [unescaped_jsonp]}
    ],
    [{Title, ?_assertEqual(
            [{string, Expect}, end_json],
            decode(<<34, Raw/binary, 34>>, [escaped_strings] ++ Config)
        )} || {Title, Expect, Raw, Config} <- Cases
    ].


single_quoted_string_test_() ->
    Cases = [
        {"single quoted string", [{string, <<"hello world">>}, end_json], <<39, "hello world", 39>>},    
        {"single quoted string with embedded double quotes",
            [{string, <<"quoth the raven, \"nevermore\"">>}, end_json],
            <<39, "quoth the raven, \"nevermore\"", 39>>
        },
        {"escaped single quote",
            [{string, <<"quoth the raven, 'nevermore'">>}, end_json],
            <<39, "quoth the raven, \\'nevermore\\'", 39>>
        },
        {"single quoted key",
            [start_object,
                {key, <<"key">>}, {string, <<"value">>},
                {key, <<"another key">>}, {string, <<"another value">>},
            end_object, end_json],
            <<"{'key':'value','another key':'another value'}">>
        }
    ],
    [{Title, ?_assertEqual(Expect, decode(Raw, []))} || {Title, Expect, Raw} <- Cases] ++
    [{Title, ?_assertError(
            badarg,
            decode(Raw, [{strict, [single_quotes]}])
        )} || {Title, _Expect, Raw} <- Cases
    ].


embedded_single_quoted_string_test_() ->
    [
        {"string with embedded single quotes", ?_assertEqual(
            [{string, <<"quoth the raven, 'nevermore'">>}, end_json],
            decode(<<34, "quoth the raven, 'nevermore'", 34>>, [])
        )},
        {"string with embedded single quotes", ?_assertEqual(
            [{string, <<"quoth the raven, 'nevermore'">>}, end_json],
            decode(<<34, "quoth the raven, 'nevermore'", 34>>, [{strict, [single_quotes]}])
        )}
    ].
    


ignored_bad_escapes_test_() ->
    [
        {"ignore unrecognized escape sequence", ?_assertEqual(
            [{string, <<"\\x25">>}, end_json],
            decode(<<"\"\\x25\"">>, [])
        )}
    ].


bom_test_() ->
    [
        {"bom", ?_assertEqual(
            [start_array, end_array, end_json],
            decode(<<16#ef, 16#bb, 16#bf, "[]"/utf8>>, [])
        )}
    ].


incomplete_test_() ->
    [
        {"stream false", ?_assertError(
            badarg,
            decode(<<"{">>)
        )},
        {"stream true", ?_assertMatch(
            {incomplete, _},
            decode(<<"{">>, [stream])
        )},
        {"complete input", ?_assertMatch(
            {incomplete, _},
            decode(<<"{}">>, [stream])
        )}
    ].


error_test_() ->
    Cases = [
        {"maybe_bom error", <<16#ef, 0>>},
        {"definitely_bom error", <<16#ef, 16#bb, 0>>},
        {"object error", <<"{"/utf8, 0>>},
        {"colon error", <<"{\"\""/utf8, 0>>},
        {"key error", <<"{\"\":1,"/utf8, 0>>},
        {"value error", <<0>>},
        {"negative error", <<"-"/utf8, 0>>},
        {"zero error", <<"0"/utf8, 0>>},
        {"integer error", <<"1"/utf8, 0>>},
        {"decimal error", <<"1.0"/utf8, 0>>},
        {"e error", <<"1e"/utf8, 0>>},
        {"ex error", <<"1e+"/utf8, 0>>},
        {"exp error", <<"1e1"/utf8, 0>>},
        {"exp error", <<"1.0e1"/utf8, 0>>},
        {"exp error", <<"1.e"/utf8>>},
        {"true error", <<"tru"/utf8, 0>>},
        {"false error", <<"fals"/utf8, 0>>},
        {"null error", <<"nul"/utf8, 0>>},
        {"maybe_done error", <<"[[]"/utf8, 0>>},
        {"done error", <<"[]"/utf8, 0>>}
    ],
    [{Title, ?_assertError(badarg, decode(State))} || {Title, State} <- Cases].


custom_incomplete_handler_test_() ->
    [
        {"custom incomplete handler", ?_assertError(
            incomplete,
            decode(<<>>, [{incomplete_handler, fun(_, _, _) -> erlang:error(incomplete) end}, stream])
        )}
    ].


-endif.
