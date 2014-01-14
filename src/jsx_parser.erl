%% The MIT License

%% Copyright (c) 2010-2013 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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


-module(jsx_parser).

-export([parser/3, resume/5]).
-export([init/1, handle_event/2]).


-spec parser(Handler::module(), State::any(), Config::list()) -> jsx:parser().

parser(Handler, State, Config) ->
    fun(Tokens) -> value(Tokens, {Handler, Handler:init(State)}, [], jsx_config:parse_config(Config)) end.


%% resume allows continuation from interrupted decoding without having to explicitly export
%%  all states
-spec resume(
        Rest::jsx:token(),
        State::atom(),
        Handler::{atom(), any()},
        Stack::list(atom()),
        Config::jsx:config()
    ) -> jsx:parser() | {incomplete, jsx:parser()}.

resume(Rest, State, Handler, Stack, Config) ->
    case State of
        value -> value(Rest, Handler, Stack, Config);
        object -> object(Rest, Handler, Stack, Config);
        array -> array(Rest, Handler, Stack, Config);
        maybe_done -> maybe_done(Rest, Handler, Stack, Config);
        done -> done(Rest, Handler, Stack, Config)
    end.


-include("jsx_config.hrl").


%% error, incomplete and event macros
-ifndef(error).
-define(error(State, Terms, Handler, Stack, Config),
    case Config#config.error_handler of
        false -> erlang:error(badarg);
        F -> F(Terms, {parser, State, Handler, Stack}, jsx_config:config_to_list(Config))
    end

).
-endif.


incomplete(State, Handler, Stack, Config=#config{stream=false}) ->
    ?error(State, [], Handler, Stack, Config);
incomplete(State, Handler, Stack, Config=#config{incomplete_handler=false}) ->
    {incomplete, fun(end_stream) ->
                case resume([end_json], State, Handler, Stack, Config) of
                    {incomplete, _} -> ?error(State, [], Handler, Stack, Config);
                    Else -> Else
                end;
            (Tokens) ->
                resume(Tokens, State, Handler, Stack, Config)
            end
    };
incomplete(State, Handler, Stack, Config=#config{incomplete_handler=F}) ->
    F([], {parser, State, Handler, Stack}, jsx_config:config_to_list(Config)).


handle_event(Event, {Handler, State}, _Config) -> {Handler, Handler:handle_event(Event, State)}.


value([start_object|Tokens], Handler, Stack, Config) ->
    object(Tokens, handle_event(start_object, Handler, Config), [object|Stack], Config);
value([start_array|Tokens], Handler, Stack, Config) ->
    array(Tokens, handle_event(start_array, Handler, Config), [array|Stack], Config);
value([{literal, Literal}|Tokens], Handler, Stack, Config) when Literal == true; Literal == false; Literal == null ->
    maybe_done(Tokens, handle_event({literal, Literal}, Handler, Config), Stack, Config);
value([Literal|Tokens], Handler, Stack, Config) when Literal == true; Literal == false; Literal == null ->
    value([{literal, Literal}] ++ Tokens, Handler, Stack, Config);
value([{integer, Number}|Tokens], Handler, Stack, Config) when is_integer(Number) ->
    maybe_done(Tokens, handle_event({integer, Number}, Handler, Config), Stack, Config);
value([{float, Number}|Tokens], Handler, Stack, Config) when is_float(Number) ->
    maybe_done(Tokens, handle_event({float, Number}, Handler, Config), Stack, Config);
value([{number, Number}|Tokens], Handler, Stack, Config) when is_integer(Number) ->
    value([{integer, Number}] ++ Tokens, Handler, Stack, Config);
value([{number, Number}|Tokens], Handler, Stack, Config) when is_float(Number) ->
    value([{float, Number}] ++ Tokens, Handler, Stack, Config);
value([Number|Tokens], Handler, Stack, Config) when is_integer(Number) ->
    value([{integer, Number}] ++ Tokens, Handler, Stack, Config);
value([Number|Tokens], Handler, Stack, Config) when is_float(Number) ->
    value([{float, Number}] ++ Tokens, Handler, Stack, Config);
value([{string, String}|Tokens], Handler, Stack, Config) when is_binary(String) ->
    case clean_string(String, Tokens, Handler, Stack, Config) of
        Clean when is_binary(Clean) ->
            maybe_done(Tokens, handle_event({string, Clean}, Handler, Config), Stack, Config);
        Error -> Error
    end;
value([String|Tokens], Handler, Stack, Config) when is_binary(String) ->
    value([{string, String}] ++ Tokens, Handler, Stack, Config);
value([String|Tokens], Handler, Stack, Config) when is_atom(String) ->
    value([{string, atom_to_binary(String, utf8)}] ++ Tokens, Handler, Stack, Config);
value([{raw, Raw}|Tokens], Handler, Stack, Config) when is_binary(Raw) ->
    value((jsx:decoder(?MODULE, [], []))(Raw) ++ Tokens, Handler, Stack, Config);
value([], Handler, Stack, Config) ->
    incomplete(value, Handler, Stack, Config);
value(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error(value, BadTokens, Handler, Stack, Config);
value(Token, Handler, Stack, Config) ->
    value([Token], Handler, Stack, Config).

object([end_object|Tokens], Handler, [object|Stack], Config) ->
    maybe_done(Tokens, handle_event(end_object, Handler, Config), Stack, Config);
object([{key, Key}|Tokens], Handler, Stack, Config) when is_atom(Key); is_binary(Key); is_integer(Key) ->
    case clean_string(fix_key(Key), Tokens, Handler, Stack, Config) of
        Clean when is_binary(Clean) ->
            value(Tokens, handle_event({key, Clean}, Handler, Config), Stack, Config);
        Error -> Error
    end;
object([Key|Tokens], Handler, Stack, Config) when is_atom(Key); is_binary(Key); is_integer(Key) ->
    case clean_string(fix_key(Key), Tokens, Handler, Stack, Config) of
        Clean when is_binary(Clean) ->
            value(Tokens, handle_event({key, Clean}, Handler, Config), Stack, Config);
        Error -> Error
    end;
object([], Handler, Stack, Config) ->
    incomplete(object, Handler, Stack, Config);
object(Token, Handler, Stack, Config) ->
    object([Token], Handler, Stack, Config).

array([end_array|Tokens], Handler, [array|Stack], Config) ->
    maybe_done(Tokens, handle_event(end_array, Handler, Config), Stack, Config);
array([], Handler, Stack, Config) ->
    incomplete(array, Handler, Stack, Config);
array(Tokens, Handler, Stack, Config) when is_list(Tokens) ->
    value(Tokens, Handler, Stack, Config);
array(Token, Handler, Stack, Config) ->
    array([Token], Handler, Stack, Config).

maybe_done([end_json], Handler, [], Config) ->
    done([end_json], Handler, [], Config);
maybe_done(Tokens, Handler, [object|_] = Stack, Config) when is_list(Tokens) ->
    object(Tokens, Handler, Stack, Config);
maybe_done(Tokens, Handler, [array|_] = Stack, Config) when is_list(Tokens) ->
    array(Tokens, Handler, Stack, Config);
maybe_done([], Handler, Stack, Config) ->
    incomplete(maybe_done, Handler, Stack, Config);
maybe_done(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error(maybe_done, BadTokens, Handler, Stack, Config);
maybe_done(Token, Handler, Stack, Config) ->
    maybe_done([Token], Handler, Stack, Config).

done([], Handler, [], Config=#config{stream=true}) ->
    incomplete(done, Handler, [], Config);
done(Tokens, Handler, [], Config) when Tokens == [end_json]; Tokens == [] ->
    {_, State} = handle_event(end_json, Handler, Config),
    State;
done(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error(done, BadTokens, Handler, Stack, Config);
done(Token, Handler, Stack, Config) ->
    done([Token], Handler, Stack, Config).


fix_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
fix_key(Key) when is_integer(Key) -> list_to_binary(integer_to_list(Key));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Tokens, Handler, Stack, Config) ->
    case clean_string(Bin, Config) of
        {error, badarg} -> ?error(string, [{string, Bin}|Tokens], Handler, Stack, Config);
        String -> String
    end.

clean_string(Bin, #config{dirty_strings=true}) -> Bin;
clean_string(Bin, Config) -> clean(Bin, [], Config).


%% escape and/or replace bad codepoints if requested
clean(<<>>, Acc, _Config) -> unicode:characters_to_binary(lists:reverse(Acc));
clean(<<0, Rest/binary>>, Acc, Config) -> maybe_replace(0, Rest, Acc, Config);
clean(<<1, Rest/binary>>, Acc, Config) -> maybe_replace(1, Rest, Acc, Config);
clean(<<2, Rest/binary>>, Acc, Config) -> maybe_replace(2, Rest, Acc, Config);
clean(<<3, Rest/binary>>, Acc, Config) -> maybe_replace(3, Rest, Acc, Config);
clean(<<4, Rest/binary>>, Acc, Config) -> maybe_replace(4, Rest, Acc, Config);
clean(<<5, Rest/binary>>, Acc, Config) -> maybe_replace(5, Rest, Acc, Config);
clean(<<6, Rest/binary>>, Acc, Config) -> maybe_replace(6, Rest, Acc, Config);
clean(<<7, Rest/binary>>, Acc, Config) -> maybe_replace(7, Rest, Acc, Config);
clean(<<8, Rest/binary>>, Acc, Config) -> maybe_replace(8, Rest, Acc, Config);
clean(<<9, Rest/binary>>, Acc, Config) -> maybe_replace(9, Rest, Acc, Config);
clean(<<10, Rest/binary>>, Acc, Config) -> maybe_replace(10, Rest, Acc, Config);
clean(<<11, Rest/binary>>, Acc, Config) -> maybe_replace(11, Rest, Acc, Config);
clean(<<12, Rest/binary>>, Acc, Config) -> maybe_replace(12, Rest, Acc, Config);
clean(<<13, Rest/binary>>, Acc, Config) -> maybe_replace(13, Rest, Acc, Config);
clean(<<14, Rest/binary>>, Acc, Config) -> maybe_replace(14, Rest, Acc, Config);
clean(<<15, Rest/binary>>, Acc, Config) -> maybe_replace(15, Rest, Acc, Config);
clean(<<16, Rest/binary>>, Acc, Config) -> maybe_replace(16, Rest, Acc, Config);
clean(<<17, Rest/binary>>, Acc, Config) -> maybe_replace(17, Rest, Acc, Config);
clean(<<18, Rest/binary>>, Acc, Config) -> maybe_replace(18, Rest, Acc, Config);
clean(<<19, Rest/binary>>, Acc, Config) -> maybe_replace(19, Rest, Acc, Config);
clean(<<20, Rest/binary>>, Acc, Config) -> maybe_replace(20, Rest, Acc, Config);
clean(<<21, Rest/binary>>, Acc, Config) -> maybe_replace(21, Rest, Acc, Config);
clean(<<22, Rest/binary>>, Acc, Config) -> maybe_replace(22, Rest, Acc, Config);
clean(<<23, Rest/binary>>, Acc, Config) -> maybe_replace(23, Rest, Acc, Config);
clean(<<24, Rest/binary>>, Acc, Config) -> maybe_replace(24, Rest, Acc, Config);
clean(<<25, Rest/binary>>, Acc, Config) -> maybe_replace(25, Rest, Acc, Config);
clean(<<26, Rest/binary>>, Acc, Config) -> maybe_replace(26, Rest, Acc, Config);
clean(<<27, Rest/binary>>, Acc, Config) -> maybe_replace(27, Rest, Acc, Config);
clean(<<28, Rest/binary>>, Acc, Config) -> maybe_replace(28, Rest, Acc, Config);
clean(<<29, Rest/binary>>, Acc, Config) -> maybe_replace(29, Rest, Acc, Config);
clean(<<30, Rest/binary>>, Acc, Config) -> maybe_replace(30, Rest, Acc, Config);
clean(<<31, Rest/binary>>, Acc, Config) -> maybe_replace(31, Rest, Acc, Config);
clean(<<32, Rest/binary>>, Acc, Config) -> clean(Rest, [32] ++ Acc, Config);
clean(<<33, Rest/binary>>, Acc, Config) -> clean(Rest, [33] ++ Acc, Config);
clean(<<34, Rest/binary>>, Acc, Config) -> maybe_replace(34, Rest, Acc, Config);
clean(<<35, Rest/binary>>, Acc, Config) -> clean(Rest, [35] ++ Acc, Config);
clean(<<36, Rest/binary>>, Acc, Config) -> clean(Rest, [36] ++ Acc, Config);
clean(<<37, Rest/binary>>, Acc, Config) -> clean(Rest, [37] ++ Acc, Config);
clean(<<38, Rest/binary>>, Acc, Config) -> clean(Rest, [38] ++ Acc, Config);
clean(<<39, Rest/binary>>, Acc, Config) -> clean(Rest, [39] ++ Acc, Config);
clean(<<40, Rest/binary>>, Acc, Config) -> clean(Rest, [40] ++ Acc, Config);
clean(<<41, Rest/binary>>, Acc, Config) -> clean(Rest, [41] ++ Acc, Config);
clean(<<42, Rest/binary>>, Acc, Config) -> clean(Rest, [42] ++ Acc, Config);
clean(<<43, Rest/binary>>, Acc, Config) -> clean(Rest, [43] ++ Acc, Config);
clean(<<44, Rest/binary>>, Acc, Config) -> clean(Rest, [44] ++ Acc, Config);
clean(<<45, Rest/binary>>, Acc, Config) -> clean(Rest, [45] ++ Acc, Config);
clean(<<46, Rest/binary>>, Acc, Config) -> clean(Rest, [46] ++ Acc, Config);
clean(<<47, Rest/binary>>, Acc, Config) -> maybe_replace(47, Rest, Acc, Config);
clean(<<48, Rest/binary>>, Acc, Config) -> clean(Rest, [48] ++ Acc, Config);
clean(<<49, Rest/binary>>, Acc, Config) -> clean(Rest, [49] ++ Acc, Config);
clean(<<50, Rest/binary>>, Acc, Config) -> clean(Rest, [50] ++ Acc, Config);
clean(<<51, Rest/binary>>, Acc, Config) -> clean(Rest, [51] ++ Acc, Config);
clean(<<52, Rest/binary>>, Acc, Config) -> clean(Rest, [52] ++ Acc, Config);
clean(<<53, Rest/binary>>, Acc, Config) -> clean(Rest, [53] ++ Acc, Config);
clean(<<54, Rest/binary>>, Acc, Config) -> clean(Rest, [54] ++ Acc, Config);
clean(<<55, Rest/binary>>, Acc, Config) -> clean(Rest, [55] ++ Acc, Config);
clean(<<56, Rest/binary>>, Acc, Config) -> clean(Rest, [56] ++ Acc, Config);
clean(<<57, Rest/binary>>, Acc, Config) -> clean(Rest, [57] ++ Acc, Config);
clean(<<58, Rest/binary>>, Acc, Config) -> clean(Rest, [58] ++ Acc, Config);
clean(<<59, Rest/binary>>, Acc, Config) -> clean(Rest, [59] ++ Acc, Config);
clean(<<60, Rest/binary>>, Acc, Config) -> clean(Rest, [60] ++ Acc, Config);
clean(<<61, Rest/binary>>, Acc, Config) -> clean(Rest, [61] ++ Acc, Config);
clean(<<62, Rest/binary>>, Acc, Config) -> clean(Rest, [62] ++ Acc, Config);
clean(<<63, Rest/binary>>, Acc, Config) -> clean(Rest, [63] ++ Acc, Config);
clean(<<64, Rest/binary>>, Acc, Config) -> clean(Rest, [64] ++ Acc, Config);
clean(<<65, Rest/binary>>, Acc, Config) -> clean(Rest, [65] ++ Acc, Config);
clean(<<66, Rest/binary>>, Acc, Config) -> clean(Rest, [66] ++ Acc, Config);
clean(<<67, Rest/binary>>, Acc, Config) -> clean(Rest, [67] ++ Acc, Config);
clean(<<68, Rest/binary>>, Acc, Config) -> clean(Rest, [68] ++ Acc, Config);
clean(<<69, Rest/binary>>, Acc, Config) -> clean(Rest, [69] ++ Acc, Config);
clean(<<70, Rest/binary>>, Acc, Config) -> clean(Rest, [70] ++ Acc, Config);
clean(<<71, Rest/binary>>, Acc, Config) -> clean(Rest, [71] ++ Acc, Config);
clean(<<72, Rest/binary>>, Acc, Config) -> clean(Rest, [72] ++ Acc, Config);
clean(<<73, Rest/binary>>, Acc, Config) -> clean(Rest, [73] ++ Acc, Config);
clean(<<74, Rest/binary>>, Acc, Config) -> clean(Rest, [74] ++ Acc, Config);
clean(<<75, Rest/binary>>, Acc, Config) -> clean(Rest, [75] ++ Acc, Config);
clean(<<76, Rest/binary>>, Acc, Config) -> clean(Rest, [76] ++ Acc, Config);
clean(<<77, Rest/binary>>, Acc, Config) -> clean(Rest, [77] ++ Acc, Config);
clean(<<78, Rest/binary>>, Acc, Config) -> clean(Rest, [78] ++ Acc, Config);
clean(<<79, Rest/binary>>, Acc, Config) -> clean(Rest, [79] ++ Acc, Config);
clean(<<80, Rest/binary>>, Acc, Config) -> clean(Rest, [80] ++ Acc, Config);
clean(<<81, Rest/binary>>, Acc, Config) -> clean(Rest, [81] ++ Acc, Config);
clean(<<82, Rest/binary>>, Acc, Config) -> clean(Rest, [82] ++ Acc, Config);
clean(<<83, Rest/binary>>, Acc, Config) -> clean(Rest, [83] ++ Acc, Config);
clean(<<84, Rest/binary>>, Acc, Config) -> clean(Rest, [84] ++ Acc, Config);
clean(<<85, Rest/binary>>, Acc, Config) -> clean(Rest, [85] ++ Acc, Config);
clean(<<86, Rest/binary>>, Acc, Config) -> clean(Rest, [86] ++ Acc, Config);
clean(<<87, Rest/binary>>, Acc, Config) -> clean(Rest, [87] ++ Acc, Config);
clean(<<88, Rest/binary>>, Acc, Config) -> clean(Rest, [88] ++ Acc, Config);
clean(<<89, Rest/binary>>, Acc, Config) -> clean(Rest, [89] ++ Acc, Config);
clean(<<90, Rest/binary>>, Acc, Config) -> clean(Rest, [90] ++ Acc, Config);
clean(<<91, Rest/binary>>, Acc, Config) -> clean(Rest, [91] ++ Acc, Config);
clean(<<92, Rest/binary>>, Acc, Config) -> maybe_replace(92, Rest, Acc, Config);
clean(<<93, Rest/binary>>, Acc, Config) -> clean(Rest, [93] ++ Acc, Config);
clean(<<94, Rest/binary>>, Acc, Config) -> clean(Rest, [94] ++ Acc, Config);
clean(<<95, Rest/binary>>, Acc, Config) -> clean(Rest, [95] ++ Acc, Config);
clean(<<96, Rest/binary>>, Acc, Config) -> clean(Rest, [96] ++ Acc, Config);
clean(<<97, Rest/binary>>, Acc, Config) -> clean(Rest, [97] ++ Acc, Config);
clean(<<98, Rest/binary>>, Acc, Config) -> clean(Rest, [98] ++ Acc, Config);
clean(<<99, Rest/binary>>, Acc, Config) -> clean(Rest, [99] ++ Acc, Config);
clean(<<100, Rest/binary>>, Acc, Config) -> clean(Rest, [100] ++ Acc, Config);
clean(<<101, Rest/binary>>, Acc, Config) -> clean(Rest, [101] ++ Acc, Config);
clean(<<102, Rest/binary>>, Acc, Config) -> clean(Rest, [102] ++ Acc, Config);
clean(<<103, Rest/binary>>, Acc, Config) -> clean(Rest, [103] ++ Acc, Config);
clean(<<104, Rest/binary>>, Acc, Config) -> clean(Rest, [104] ++ Acc, Config);
clean(<<105, Rest/binary>>, Acc, Config) -> clean(Rest, [105] ++ Acc, Config);
clean(<<106, Rest/binary>>, Acc, Config) -> clean(Rest, [106] ++ Acc, Config);
clean(<<107, Rest/binary>>, Acc, Config) -> clean(Rest, [107] ++ Acc, Config);
clean(<<108, Rest/binary>>, Acc, Config) -> clean(Rest, [108] ++ Acc, Config);
clean(<<109, Rest/binary>>, Acc, Config) -> clean(Rest, [109] ++ Acc, Config);
clean(<<110, Rest/binary>>, Acc, Config) -> clean(Rest, [110] ++ Acc, Config);
clean(<<111, Rest/binary>>, Acc, Config) -> clean(Rest, [111] ++ Acc, Config);
clean(<<112, Rest/binary>>, Acc, Config) -> clean(Rest, [112] ++ Acc, Config);
clean(<<113, Rest/binary>>, Acc, Config) -> clean(Rest, [113] ++ Acc, Config);
clean(<<114, Rest/binary>>, Acc, Config) -> clean(Rest, [114] ++ Acc, Config);
clean(<<115, Rest/binary>>, Acc, Config) -> clean(Rest, [115] ++ Acc, Config);
clean(<<116, Rest/binary>>, Acc, Config) -> clean(Rest, [116] ++ Acc, Config);
clean(<<117, Rest/binary>>, Acc, Config) -> clean(Rest, [117] ++ Acc, Config);
clean(<<118, Rest/binary>>, Acc, Config) -> clean(Rest, [118] ++ Acc, Config);
clean(<<119, Rest/binary>>, Acc, Config) -> clean(Rest, [119] ++ Acc, Config);
clean(<<120, Rest/binary>>, Acc, Config) -> clean(Rest, [120] ++ Acc, Config);
clean(<<121, Rest/binary>>, Acc, Config) -> clean(Rest, [121] ++ Acc, Config);
clean(<<122, Rest/binary>>, Acc, Config) -> clean(Rest, [122] ++ Acc, Config);
clean(<<123, Rest/binary>>, Acc, Config) -> clean(Rest, [123] ++ Acc, Config);
clean(<<124, Rest/binary>>, Acc, Config) -> clean(Rest, [124] ++ Acc, Config);
clean(<<125, Rest/binary>>, Acc, Config) -> clean(Rest, [125] ++ Acc, Config);
clean(<<126, Rest/binary>>, Acc, Config) -> clean(Rest, [126] ++ Acc, Config);
clean(<<127, Rest/binary>>, Acc, Config) -> clean(Rest, [127] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X == 16#2028; X == 16#2029 ->
    maybe_replace(X, Rest, Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X < 16#d800 ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X > 16#dfff, X < 16#fdd0 ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X > 16#fdef, X < 16#fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#10000, X < 16#1fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#20000, X < 16#2fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#30000, X < 16#3fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#40000, X < 16#4fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#50000, X < 16#5fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#60000, X < 16#6fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#70000, X < 16#7fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#80000, X < 16#8fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#90000, X < 16#9fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#a0000, X < 16#afffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#b0000, X < 16#bfffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#c0000, X < 16#cfffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#d0000, X < 16#dfffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#e0000, X < 16#efffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#f0000, X < 16#ffffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#100000, X < 16#10fffe ->
    clean(Rest, [X] ++ Acc, Config);
%% surrogates
clean(<<237, X, _, Rest/binary>>, Acc, Config) when X >= 160 ->
    maybe_replace(surrogate, Rest, Acc, Config);
%% noncharacters
clean(<<_/utf8, Rest/binary>>, Acc, Config) ->
    maybe_replace(noncharacter, Rest, Acc, Config);
%% u+fffe and u+ffff for R14BXX
clean(<<239, 191, X, Rest/binary>>, Acc, Config) when X == 190; X == 191 ->
    maybe_replace(noncharacter, Rest, Acc, Config);
%% overlong encodings and missing continuations of a 2 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 192, X =< 223 ->
    maybe_replace(badutf, strip_continuations(Rest, 1), Acc, Config);
%% overlong encodings and missing continuations of a 3 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 224, X =< 239 ->
    maybe_replace(badutf, strip_continuations(Rest, 2), Acc, Config);
%% overlong encodings and missing continuations of a 4 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 240, X =< 247 ->
    maybe_replace(badutf, strip_continuations(Rest, 3), Acc, Config);
clean(<<_, Rest/binary>>, Acc, Config) ->
    maybe_replace(badutf, Rest, Acc, Config).


strip_continuations(Bin, 0) -> Bin;
strip_continuations(<<X, Rest/binary>>, N) when X >= 128, X =< 191 ->
    strip_continuations(Rest, N - 1);
%% not a continuation byte
strip_continuations(Bin, _) -> Bin.


maybe_replace($\b, Rest, Acc, Config=#config{escaped_strings=true}) ->
    clean(Rest, [$b, $\\] ++ Acc, Config);
maybe_replace($\t, Rest, Acc, Config=#config{escaped_strings=true}) ->
    clean(Rest, [$t, $\\] ++ Acc, Config);
maybe_replace($\n, Rest, Acc, Config=#config{escaped_strings=true}) ->
    clean(Rest, [$n, $\\] ++ Acc, Config);
maybe_replace($\f, Rest, Acc, Config=#config{escaped_strings=true}) ->
    clean(Rest, [$f, $\\] ++ Acc, Config);
maybe_replace($\r, Rest, Acc, Config=#config{escaped_strings=true}) ->
    clean(Rest, [$r, $\\] ++ Acc, Config);
maybe_replace($\", Rest, Acc, Config=#config{escaped_strings=true}) ->
    clean(Rest, [$\", $\\] ++ Acc, Config);
maybe_replace($/, Rest, Acc, Config=#config{escaped_strings=true}) ->
    case Config#config.escaped_forward_slashes of
        true -> clean(Rest, [$/, $\\] ++ Acc, Config);
        false -> clean(Rest, [$/] ++ Acc, Config)
    end;
maybe_replace($\\, Rest, Acc, Config=#config{escaped_strings=true}) ->
    clean(Rest, [$\\, $\\] ++ Acc, Config);
maybe_replace(X, Rest, Acc, Config=#config{escaped_strings=true})  when X == 16#2028; X == 16#2029 ->
    case Config#config.unescaped_jsonp of
        true -> clean(Rest, [X] ++ Acc, Config);
        false -> clean(Rest, lists:reverse(json_escape_sequence(X)) ++ Acc, Config)
    end;
maybe_replace(X, Rest, Acc, Config=#config{escaped_strings=true}) when X < 32 ->
    clean(Rest, lists:reverse(json_escape_sequence(X)) ++ Acc, Config);
maybe_replace(Atom, _, _, #config{strict_utf8=true}) when is_atom(Atom) -> {error, badarg};
maybe_replace(noncharacter, Rest, Acc, Config) -> clean(Rest, [16#fffd] ++ Acc, Config);
maybe_replace(surrogate, Rest, Acc, Config) -> clean(Rest, [16#fffd] ++ Acc, Config);
maybe_replace(badutf, Rest, Acc, Config) -> clean(Rest, [16#fffd] ++ Acc, Config);
maybe_replace(X, Rest, Acc, Config) -> clean(Rest, [X] ++ Acc, Config).


%% convert a codepoint to it's \uXXXX equiv.
json_escape_sequence(X) ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    [$\\, $u, (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))].


to_hex(10) -> $a;
to_hex(11) -> $b;
to_hex(12) -> $c;
to_hex(13) -> $d;
to_hex(14) -> $e;
to_hex(15) -> $f;
to_hex(X) -> X + 48.    %% ascii "1" is [49], "2" is [50], etc...


%% for raw input
-spec init(proplists:proplist()) -> list().

init([]) -> [].

-spec handle_event(Event::any(), Acc::list()) -> list().

handle_event(end_json, State) -> lists:reverse(State);
handle_event(Event, State) -> [Event] ++ State.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


parse(Events, Config) -> value(Events, {jsx, []}, [], jsx_config:parse_config(Config)).


error_test_() ->
    [
        {"value error", ?_assertError(badarg, parse([self()], []))},
        {"maybe_done error", ?_assertError(badarg, parse([start_array, end_array, start_array, end_json], []))},
        {"done error", ?_assertError(badarg, parse([{string, <<"">>}, {literal, true}, end_json], []))},
        {"string error", ?_assertError(badarg, parse([{string, <<239, 191, 191>>}, end_json], [strict_utf8]))}
    ].


custom_error_handler_test_() ->
    Error = fun(Rest, {_, State, _, _}, _) -> {State, Rest} end,
    [
        {"value error", ?_assertEqual(
            {value, [self()]},
            parse([self()], [{error_handler, Error}])
        )},
        {"maybe_done error", ?_assertEqual(
            {maybe_done, [start_array, end_json]},
            parse([start_array, end_array, start_array, end_json], [{error_handler, Error}])
        )},
        {"done error", ?_assertEqual(
            {maybe_done, [{literal, true}, end_json]},
            parse([{string, <<"">>}, {literal, true}, end_json], [{error_handler, Error}])
        )},
        {"string error", ?_assertEqual(
            {string, [{string, <<239, 191, 191>>}, end_json]},
            parse([{string, <<239, 191, 191>>}, end_json], [{error_handler, Error}, strict])
        )}
    ].


incomplete_test_() ->
    Cases = [
        {"incomplete value", []},
        {"incomplete object", [start_object]},
        {"incomplete array", [start_array]},
        {"incomplete maybe_done", [start_array, end_array]}
    ],
    [{Title, ?_assertError(badarg, parse(Events, []))}
        || {Title, Events} <- Cases
    ].


custom_incomplete_handler_test_() ->
    [
        {"custom incomplete handler", ?_assertError(
            badarg,
            parse([], [{incomplete_handler, fun(_, _, _) -> erlang:error(badarg) end}])
        )}
    ].


raw_test_() ->
    Parse = fun(Events, Config) -> (parser(?MODULE, [], Config))(Events ++ [end_json]) end,
    [
        {"raw empty list", ?_assertEqual(
            [start_array, end_array],
            Parse([{raw, <<"[]">>}], [])
        )},
        {"raw empty object", ?_assertEqual(
            [start_object, end_object],
            Parse([{raw, <<"{}">>}], [])
        )},
        {"raw chunk inside stream", ?_assertEqual(
            [start_object, {key, <<"key">>}, start_array, {literal, true}, end_array, end_object],
            Parse([start_object, {key, <<"key">>}, {raw, <<"[true]">>}, end_object], [])
        )}
    ].


%% erlang refuses to encode certain codepoints, so fake them
to_fake_utf8(N) when N < 16#0080 -> <<N:8>>;
to_fake_utf8(N) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<2#110:3, Y:5, 2#10:2, X:6>>;
to_fake_utf8(N) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6>>;
to_fake_utf8(N) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6>>.


codepoints() ->
    unicode:characters_to_binary(
        [32, 33]
        ++ lists:seq(35, 46)
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


clean_string_test_() ->
    [
        {"clean codepoints", ?_assertEqual(
            codepoints(),
            clean_string(codepoints(), #config{})
        )},
        {"clean extended codepoints", ?_assertEqual(
            extended_codepoints(),
            clean_string(extended_codepoints(), #config{})
        )},
        {"escape path codepoints", ?_assertEqual(
            codepoints(),
            clean_string(codepoints(), #config{escaped_strings=true})
        )},
        {"escape path extended codepoints", ?_assertEqual(
            extended_codepoints(),
            clean_string(extended_codepoints(), #config{escaped_strings=true})
        )},
        {"error reserved space", ?_assertEqual(
            lists:duplicate(length(reserved_space()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{strict_utf8=true}) end, reserved_space())
        )},
        {"error surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{strict_utf8=true}) end, surrogates())
        )},
        {"error noncharacters", ?_assertEqual(
            lists:duplicate(length(noncharacters()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{strict_utf8=true}) end, noncharacters())
        )},
        {"error extended noncharacters", ?_assertEqual(
            lists:duplicate(length(extended_noncharacters()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{strict_utf8=true}) end, extended_noncharacters())
        )},
        {"clean reserved space", ?_assertEqual(
            lists:duplicate(length(reserved_space()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, reserved_space())
        )},
        {"clean surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, surrogates())
        )},
        {"clean noncharacters", ?_assertEqual(
            lists:duplicate(length(noncharacters()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, noncharacters())
        )},
        {"clean extended noncharacters", ?_assertEqual(
            lists:duplicate(length(extended_noncharacters()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, extended_noncharacters())
        )}
    ].


escape_test_() ->
    [
        {"maybe_escape backspace", ?_assertEqual(
            <<"\\b">>,
            clean_string(<<16#0008/utf8>>, #config{escaped_strings=true})
        )},
        {"don't escape backspace", ?_assertEqual(
            <<"\b">>,
            clean_string(<<16#0008/utf8>>, #config{})
        )},
        {"maybe_escape tab", ?_assertEqual(
            <<"\\t">>,
            clean_string(<<16#0009/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape newline", ?_assertEqual(
            <<"\\n">>,
            clean_string(<<16#000a/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape formfeed", ?_assertEqual(
            <<"\\f">>,
            clean_string(<<16#000c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape carriage return", ?_assertEqual(
            <<"\\r">>,
            clean_string(<<16#000d/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape quote", ?_assertEqual(
            <<"\\\"">>,
            clean_string(<<16#0022/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape forward slash", ?_assertEqual(
            <<"\\/">>,
            clean_string(<<16#002f/utf8>>, #config{escaped_strings=true, escaped_forward_slashes=true})
        )},
        {"do not maybe_escape forward slash", ?_assertEqual(
            <<"/">>,
            clean_string(<<16#002f/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape backslash", ?_assertEqual(
            <<"\\\\">>,
            clean_string(<<16#005c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape jsonp (u2028)", ?_assertEqual(
            <<"\\u2028">>,
            clean_string(<<16#2028/utf8>>, #config{escaped_strings=true})
        )},
        {"do not maybe_escape jsonp (u2028)", ?_assertEqual(
            <<16#2028/utf8>>,
            clean_string(<<16#2028/utf8>>, #config{escaped_strings=true, unescaped_jsonp=true})
        )},
        {"maybe_escape jsonp (u2029)", ?_assertEqual(
            <<"\\u2029">>,
            clean_string(<<16#2029/utf8>>, #config{escaped_strings=true})
        )},
        {"do not maybe_escape jsonp (u2029)", ?_assertEqual(
            <<16#2029/utf8>>,
            clean_string(<<16#2029/utf8>>, #config{escaped_strings=true, unescaped_jsonp=true})
        )},
        {"maybe_escape u0000", ?_assertEqual(
            <<"\\u0000">>,
            clean_string(<<16#0000/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0001", ?_assertEqual(
            <<"\\u0001">>,
            clean_string(<<16#0001/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0002", ?_assertEqual(
            <<"\\u0002">>,
            clean_string(<<16#0002/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0003", ?_assertEqual(
            <<"\\u0003">>,
            clean_string(<<16#0003/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0004", ?_assertEqual(
            <<"\\u0004">>,
            clean_string(<<16#0004/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0005", ?_assertEqual(
            <<"\\u0005">>,
            clean_string(<<16#0005/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0006", ?_assertEqual(
            <<"\\u0006">>,
            clean_string(<<16#0006/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0007", ?_assertEqual(
            <<"\\u0007">>,
            clean_string(<<16#0007/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000b", ?_assertEqual(
            <<"\\u000b">>,
            clean_string(<<16#000b/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000e", ?_assertEqual(
            <<"\\u000e">>,
            clean_string(<<16#000e/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000f", ?_assertEqual(
            <<"\\u000f">>,
            clean_string(<<16#000f/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0010", ?_assertEqual(
            <<"\\u0010">>,
            clean_string(<<16#0010/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0011", ?_assertEqual(
            <<"\\u0011">>,
            clean_string(<<16#0011/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0012", ?_assertEqual(
            <<"\\u0012">>,
            clean_string(<<16#0012/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0013", ?_assertEqual(
            <<"\\u0013">>,
            clean_string(<<16#0013/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0014", ?_assertEqual(
            <<"\\u0014">>,
            clean_string(<<16#0014/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0015", ?_assertEqual(
            <<"\\u0015">>,
            clean_string(<<16#0015/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0016", ?_assertEqual(
            <<"\\u0016">>,
            clean_string(<<16#0016/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0017", ?_assertEqual(
            <<"\\u0017">>,
            clean_string(<<16#0017/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0018", ?_assertEqual(
            <<"\\u0018">>,
            clean_string(<<16#0018/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0019", ?_assertEqual(
            <<"\\u0019">>,
            clean_string(<<16#0019/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001a", ?_assertEqual(
            <<"\\u001a">>,
            clean_string(<<16#001a/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001b", ?_assertEqual(
            <<"\\u001b">>,
            clean_string(<<16#001b/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001c", ?_assertEqual(
            <<"\\u001c">>,
            clean_string(<<16#001c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001d", ?_assertEqual(
            <<"\\u001d">>,
            clean_string(<<16#001d/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001e", ?_assertEqual(
            <<"\\u001e">>,
            clean_string(<<16#001e/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001f", ?_assertEqual(
            <<"\\u001f">>,
            clean_string(<<16#001f/utf8>>, #config{escaped_strings=true})
        )}
    ].


bad_utf8_test_() ->
    [
        {"noncharacter u+fffe", ?_assertEqual(
            {error, badarg},
            clean_string(to_fake_utf8(16#fffe), #config{strict_utf8=true})
        )},
        {"noncharacter u+fffe replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(to_fake_utf8(16#fffe), #config{})
        )},
        {"noncharacter u+ffff", ?_assertEqual(
            {error, badarg},
            clean_string(to_fake_utf8(16#ffff), #config{strict_utf8=true})
        )},
        {"noncharacter u+ffff replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(to_fake_utf8(16#ffff), #config{})
        )},
        {"orphan continuation byte u+0080", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#0080>>, #config{strict_utf8=true})
        )},
        {"orphan continuation byte u+0080 replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#0080>>, #config{})
        )},
        {"orphan continuation byte u+00bf", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00bf>>, #config{strict_utf8=true})
        )},
        {"orphan continuation byte u+00bf replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00bf>>, #config{})
        )},
        {"2 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #config{strict_utf8=true})
        )},
        {"2 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 2),
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #config{})
        )},
        {"3 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #config{strict_utf8=true})
        )},
        {"3 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 3),
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #config{})
        )},
        {"4 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #config{strict_utf8=true})
        )},
        {"4 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 4),
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #config{})
        )},
        {"5 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #config{strict_utf8=true})
        )},
        {"5 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 5),
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #config{})
        )},
        {"6 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #config{strict_utf8=true})
        )},
        {"6 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 6),
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #config{})
        )},
        {"all continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, #config{strict_utf8=true})
        )},
        {"all continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf))),
            clean_string(
                <<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>,
                #config{}
            )
        )},
        {"lonely start byte", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00c0>>, #config{strict_utf8=true})
        )},
        {"lonely start byte replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00c0>>, #config{})
        )},
        {"lonely start bytes (2 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00c0, 32, 16#00df>>, #config{strict_utf8=true})
        )},
        {"lonely start bytes (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00c0, 32, 16#00df>>, #config{})
        )},
        {"lonely start bytes (3 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00e0, 32, 16#00ef>>, #config{strict_utf8=true})
        )},
        {"lonely start bytes (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00e0, 32, 16#00ef>>, #config{})
        )},
        {"lonely start bytes (4 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00f0, 32, 16#00f7>>, #config{strict_utf8=true})
        )},
        {"lonely start bytes (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00f0, 32, 16#00f7>>, #config{})
        )},
        {"missing continuation byte (3 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<224, 160, 32>>, #config{strict_utf8=true})
        )},
        {"missing continuation byte (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<224, 160, 32>>, #config{})
        )},
        {"missing continuation byte (4 byte missing one)", ?_assertEqual(
            {error, badarg},
            clean_string(<<240, 144, 128, 32>>, #config{strict_utf8=true})
        )},
        {"missing continuation byte (4 byte missing one) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 128, 32>>, #config{})
        )},
        {"missing continuation byte (4 byte missing two)", ?_assertEqual(
            {error, badarg},
            clean_string(<<240, 144, 32>>, #config{strict_utf8=true})
        )},
        {"missing continuation byte (4 byte missing two) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (2 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#c0, 16#af, 32>>, #config{strict_utf8=true})
        )},
        {"overlong encoding of u+002f (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c0, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (3 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #config{strict_utf8=true})
        )},
        {"overlong encoding of u+002f (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (4 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #config{strict_utf8=true})
        )},
        {"overlong encoding of u+002f (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #config{})
        )},
        {"highest overlong 2 byte sequence", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#c1, 16#bf, 32>>, #config{strict_utf8=true})
        )},
        {"highest overlong 2 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c1, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 3 byte sequence", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #config{strict_utf8=true})
        )},
        {"highest overlong 3 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 4 byte sequence", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #config{strict_utf8=true})
        )},
        {"highest overlong 4 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #config{})
        )}
    ].


json_escape_sequence_test_() ->
    [
        {"json escape sequence test - 16#0000", ?_assertEqual(json_escape_sequence(16#0000), "\\u0000")},
        {"json escape sequence test - 16#abc", ?_assertEqual(json_escape_sequence(16#abc), "\\u0abc")},
        {"json escape sequence test - 16#def", ?_assertEqual(json_escape_sequence(16#def), "\\u0def")}
    ].


fix_key_test_() ->
    [
        {"binary key", ?_assertEqual(fix_key(<<"foo">>), <<"foo">>)},
        {"atom key", ?_assertEqual(fix_key(foo), <<"foo">>)},
        {"integer key", ?_assertEqual(fix_key(123), <<"123">>)}
    ].

-endif.
