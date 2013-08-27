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

-ifdef(TEST).
-export([clean_string/2, json_escape_sequence/1]).
-endif.


-spec parser(Handler::module(), State::any(), Config::jsx:config()) -> jsx:parser().

parser(Handler, State, Config) ->
    fun(Tokens) -> value(Tokens, {Handler, Handler:init(State)}, [], jsx_config:parse_config(Config)) end.


%% resume allows continuation from interrupted decoding without having to explicitly export
%%  all states
-spec resume(
        Rest::binary(),
        State::atom(),
        Handler::{atom(), any()},
        Stack::list(atom()),
        Config::jsx:config()
    ) -> jsx:parser().

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


handle_event([], Handler, _Config) -> Handler;
handle_event([Event|Rest], Handler, Config) -> handle_event(Rest, handle_event(Event, Handler, Config), Config);
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
object([{key, Key}|Tokens], Handler, Stack, Config) when is_atom(Key); is_binary(Key) ->
    case clean_string(fix_key(Key), Tokens, Handler, Stack, Config) of
        Clean when is_binary(Clean) ->
            value(Tokens, handle_event({key, Clean}, Handler, Config), Stack, Config);
        Error -> Error
    end;
object([Key|Tokens], Handler, Stack, Config) when is_atom(Key); is_binary(Key) ->
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

done([], Handler, [], Config=#config{explicit_end=true}) ->
    incomplete(done, Handler, [], Config);
done(Tokens, Handler, [], Config) when Tokens == [end_json]; Tokens == [] ->
    {_, State} = handle_event(end_json, Handler, Config),
    State;
done(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error(done, BadTokens, Handler, Stack, Config);
done(Token, Handler, Stack, Config) ->
    done([Token], Handler, Stack, Config).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Tokens, Handler, Stack, Config) ->
    case clean_string(Bin, Config) of
        {error, badarg} -> ?error(string, [{string, Bin}|Tokens], Handler, Stack, Config);
        String -> String
    end.

clean_string(Bin, #config{dirty_strings=true}) -> Bin;
clean_string(Bin, Config) ->
    case Config#config.replaced_bad_utf8 orelse Config#config.escaped_strings of
        true -> clean(Bin, [], Config);
        false -> ensure_clean(Bin)
    end.


ensure_clean(Bin) ->
    case is_clean(Bin) of
        ok -> Bin;
        {error, badarg} -> {error, badarg}
    end.


%% fast path for no escaping and no correcting, throws error if string is 'bad'
is_clean(<<>>) -> ok;
is_clean(<<0, Rest/binary>>) -> is_clean(Rest);
is_clean(<<1, Rest/binary>>) -> is_clean(Rest);
is_clean(<<2, Rest/binary>>) -> is_clean(Rest);
is_clean(<<3, Rest/binary>>) -> is_clean(Rest);
is_clean(<<4, Rest/binary>>) -> is_clean(Rest);
is_clean(<<5, Rest/binary>>) -> is_clean(Rest);
is_clean(<<6, Rest/binary>>) -> is_clean(Rest);
is_clean(<<7, Rest/binary>>) -> is_clean(Rest);
is_clean(<<8, Rest/binary>>) -> is_clean(Rest);
is_clean(<<9, Rest/binary>>) -> is_clean(Rest);
is_clean(<<10, Rest/binary>>) -> is_clean(Rest);
is_clean(<<11, Rest/binary>>) -> is_clean(Rest);
is_clean(<<12, Rest/binary>>) -> is_clean(Rest);
is_clean(<<13, Rest/binary>>) -> is_clean(Rest);
is_clean(<<14, Rest/binary>>) -> is_clean(Rest);
is_clean(<<15, Rest/binary>>) -> is_clean(Rest);
is_clean(<<16, Rest/binary>>) -> is_clean(Rest);
is_clean(<<17, Rest/binary>>) -> is_clean(Rest);
is_clean(<<18, Rest/binary>>) -> is_clean(Rest);
is_clean(<<19, Rest/binary>>) -> is_clean(Rest);
is_clean(<<20, Rest/binary>>) -> is_clean(Rest);
is_clean(<<21, Rest/binary>>) -> is_clean(Rest);
is_clean(<<22, Rest/binary>>) -> is_clean(Rest);
is_clean(<<23, Rest/binary>>) -> is_clean(Rest);
is_clean(<<24, Rest/binary>>) -> is_clean(Rest);
is_clean(<<25, Rest/binary>>) -> is_clean(Rest);
is_clean(<<26, Rest/binary>>) -> is_clean(Rest);
is_clean(<<27, Rest/binary>>) -> is_clean(Rest);
is_clean(<<28, Rest/binary>>) -> is_clean(Rest);
is_clean(<<29, Rest/binary>>) -> is_clean(Rest);
is_clean(<<30, Rest/binary>>) -> is_clean(Rest);
is_clean(<<31, Rest/binary>>) -> is_clean(Rest);
is_clean(<<32, Rest/binary>>) -> is_clean(Rest);
is_clean(<<33, Rest/binary>>) -> is_clean(Rest);
is_clean(<<34, Rest/binary>>) -> is_clean(Rest);
is_clean(<<35, Rest/binary>>) -> is_clean(Rest);
is_clean(<<36, Rest/binary>>) -> is_clean(Rest);
is_clean(<<37, Rest/binary>>) -> is_clean(Rest);
is_clean(<<38, Rest/binary>>) -> is_clean(Rest);
is_clean(<<39, Rest/binary>>) -> is_clean(Rest);
is_clean(<<40, Rest/binary>>) -> is_clean(Rest);
is_clean(<<41, Rest/binary>>) -> is_clean(Rest);
is_clean(<<42, Rest/binary>>) -> is_clean(Rest);
is_clean(<<43, Rest/binary>>) -> is_clean(Rest);
is_clean(<<44, Rest/binary>>) -> is_clean(Rest);
is_clean(<<45, Rest/binary>>) -> is_clean(Rest);
is_clean(<<46, Rest/binary>>) -> is_clean(Rest);
is_clean(<<47, Rest/binary>>) -> is_clean(Rest);
is_clean(<<48, Rest/binary>>) -> is_clean(Rest);
is_clean(<<49, Rest/binary>>) -> is_clean(Rest);
is_clean(<<50, Rest/binary>>) -> is_clean(Rest);
is_clean(<<51, Rest/binary>>) -> is_clean(Rest);
is_clean(<<52, Rest/binary>>) -> is_clean(Rest);
is_clean(<<53, Rest/binary>>) -> is_clean(Rest);
is_clean(<<54, Rest/binary>>) -> is_clean(Rest);
is_clean(<<55, Rest/binary>>) -> is_clean(Rest);
is_clean(<<56, Rest/binary>>) -> is_clean(Rest);
is_clean(<<57, Rest/binary>>) -> is_clean(Rest);
is_clean(<<58, Rest/binary>>) -> is_clean(Rest);
is_clean(<<59, Rest/binary>>) -> is_clean(Rest);
is_clean(<<60, Rest/binary>>) -> is_clean(Rest);
is_clean(<<61, Rest/binary>>) -> is_clean(Rest);
is_clean(<<62, Rest/binary>>) -> is_clean(Rest);
is_clean(<<63, Rest/binary>>) -> is_clean(Rest);
is_clean(<<64, Rest/binary>>) -> is_clean(Rest);
is_clean(<<65, Rest/binary>>) -> is_clean(Rest);
is_clean(<<66, Rest/binary>>) -> is_clean(Rest);
is_clean(<<67, Rest/binary>>) -> is_clean(Rest);
is_clean(<<68, Rest/binary>>) -> is_clean(Rest);
is_clean(<<69, Rest/binary>>) -> is_clean(Rest);
is_clean(<<70, Rest/binary>>) -> is_clean(Rest);
is_clean(<<71, Rest/binary>>) -> is_clean(Rest);
is_clean(<<72, Rest/binary>>) -> is_clean(Rest);
is_clean(<<73, Rest/binary>>) -> is_clean(Rest);
is_clean(<<74, Rest/binary>>) -> is_clean(Rest);
is_clean(<<75, Rest/binary>>) -> is_clean(Rest);
is_clean(<<76, Rest/binary>>) -> is_clean(Rest);
is_clean(<<77, Rest/binary>>) -> is_clean(Rest);
is_clean(<<78, Rest/binary>>) -> is_clean(Rest);
is_clean(<<79, Rest/binary>>) -> is_clean(Rest);
is_clean(<<80, Rest/binary>>) -> is_clean(Rest);
is_clean(<<81, Rest/binary>>) -> is_clean(Rest);
is_clean(<<82, Rest/binary>>) -> is_clean(Rest);
is_clean(<<83, Rest/binary>>) -> is_clean(Rest);
is_clean(<<84, Rest/binary>>) -> is_clean(Rest);
is_clean(<<85, Rest/binary>>) -> is_clean(Rest);
is_clean(<<86, Rest/binary>>) -> is_clean(Rest);
is_clean(<<87, Rest/binary>>) -> is_clean(Rest);
is_clean(<<88, Rest/binary>>) -> is_clean(Rest);
is_clean(<<89, Rest/binary>>) -> is_clean(Rest);
is_clean(<<90, Rest/binary>>) -> is_clean(Rest);
is_clean(<<91, Rest/binary>>) -> is_clean(Rest);
is_clean(<<92, Rest/binary>>) -> is_clean(Rest);
is_clean(<<93, Rest/binary>>) -> is_clean(Rest);
is_clean(<<94, Rest/binary>>) -> is_clean(Rest);
is_clean(<<95, Rest/binary>>) -> is_clean(Rest);
is_clean(<<96, Rest/binary>>) -> is_clean(Rest);
is_clean(<<97, Rest/binary>>) -> is_clean(Rest);
is_clean(<<98, Rest/binary>>) -> is_clean(Rest);
is_clean(<<99, Rest/binary>>) -> is_clean(Rest);
is_clean(<<100, Rest/binary>>) -> is_clean(Rest);
is_clean(<<101, Rest/binary>>) -> is_clean(Rest);
is_clean(<<102, Rest/binary>>) -> is_clean(Rest);
is_clean(<<103, Rest/binary>>) -> is_clean(Rest);
is_clean(<<104, Rest/binary>>) -> is_clean(Rest);
is_clean(<<105, Rest/binary>>) -> is_clean(Rest);
is_clean(<<106, Rest/binary>>) -> is_clean(Rest);
is_clean(<<107, Rest/binary>>) -> is_clean(Rest);
is_clean(<<108, Rest/binary>>) -> is_clean(Rest);
is_clean(<<109, Rest/binary>>) -> is_clean(Rest);
is_clean(<<110, Rest/binary>>) -> is_clean(Rest);
is_clean(<<111, Rest/binary>>) -> is_clean(Rest);
is_clean(<<112, Rest/binary>>) -> is_clean(Rest);
is_clean(<<113, Rest/binary>>) -> is_clean(Rest);
is_clean(<<114, Rest/binary>>) -> is_clean(Rest);
is_clean(<<115, Rest/binary>>) -> is_clean(Rest);
is_clean(<<116, Rest/binary>>) -> is_clean(Rest);
is_clean(<<117, Rest/binary>>) -> is_clean(Rest);
is_clean(<<118, Rest/binary>>) -> is_clean(Rest);
is_clean(<<119, Rest/binary>>) -> is_clean(Rest);
is_clean(<<120, Rest/binary>>) -> is_clean(Rest);
is_clean(<<121, Rest/binary>>) -> is_clean(Rest);
is_clean(<<122, Rest/binary>>) -> is_clean(Rest);
is_clean(<<123, Rest/binary>>) -> is_clean(Rest);
is_clean(<<124, Rest/binary>>) -> is_clean(Rest);
is_clean(<<125, Rest/binary>>) -> is_clean(Rest);
is_clean(<<126, Rest/binary>>) -> is_clean(Rest);
is_clean(<<127, Rest/binary>>) -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X < 16#d800 -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X > 16#dfff, X < 16#fdd0 -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X > 16#fdef, X < 16#fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#10000, X < 16#1fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#20000, X < 16#2fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#30000, X < 16#3fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#40000, X < 16#4fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#50000, X < 16#5fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#60000, X < 16#6fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#70000, X < 16#7fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#80000, X < 16#8fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#90000, X < 16#9fffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#a0000, X < 16#afffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#b0000, X < 16#bfffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#c0000, X < 16#cfffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#d0000, X < 16#dfffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#e0000, X < 16#efffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#f0000, X < 16#ffffe -> is_clean(Rest);
is_clean(<<X/utf8, Rest/binary>>) when X >= 16#100000, X < 16#10fffe -> is_clean(Rest);
is_clean(_Bin) -> {error, badarg}.


%% escape and/or replace bad codepoints if requested
clean(<<>>, Acc, _Config) -> unicode:characters_to_binary(lists:reverse(Acc));
clean(<<0, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(0, Config) ++ Acc, Config);
clean(<<1, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(1, Config) ++ Acc, Config);
clean(<<2, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(2, Config) ++ Acc, Config);
clean(<<3, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(3, Config) ++ Acc, Config);
clean(<<4, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(4, Config) ++ Acc, Config);
clean(<<5, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(5, Config) ++ Acc, Config);
clean(<<6, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(6, Config) ++ Acc, Config);
clean(<<7, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(7, Config) ++ Acc, Config);
clean(<<8, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(8, Config) ++ Acc, Config);
clean(<<9, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(9, Config) ++ Acc, Config);
clean(<<10, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(10, Config) ++ Acc, Config);
clean(<<11, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(11, Config) ++ Acc, Config);
clean(<<12, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(12, Config) ++ Acc, Config);
clean(<<13, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(13, Config) ++ Acc, Config);
clean(<<14, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(14, Config) ++ Acc, Config);
clean(<<15, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(15, Config) ++ Acc, Config);
clean(<<16, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(16, Config) ++ Acc, Config);
clean(<<17, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(17, Config) ++ Acc, Config);
clean(<<18, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(18, Config) ++ Acc, Config);
clean(<<19, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(19, Config) ++ Acc, Config);
clean(<<20, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(20, Config) ++ Acc, Config);
clean(<<21, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(21, Config) ++ Acc, Config);
clean(<<22, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(22, Config) ++ Acc, Config);
clean(<<23, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(23, Config) ++ Acc, Config);
clean(<<24, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(24, Config) ++ Acc, Config);
clean(<<25, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(25, Config) ++ Acc, Config);
clean(<<26, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(26, Config) ++ Acc, Config);
clean(<<27, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(27, Config) ++ Acc, Config);
clean(<<28, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(28, Config) ++ Acc, Config);
clean(<<29, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(29, Config) ++ Acc, Config);
clean(<<30, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(30, Config) ++ Acc, Config);
clean(<<31, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(31, Config) ++ Acc, Config);
clean(<<32, Rest/binary>>, Acc, Config) -> clean(Rest, [32] ++ Acc, Config);
clean(<<33, Rest/binary>>, Acc, Config) -> clean(Rest, [33] ++ Acc, Config);
clean(<<34, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(34, Config) ++ Acc, Config);
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
clean(<<47, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(47, Config) ++ Acc, Config);
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
clean(<<92, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(92, Config) ++ Acc, Config);
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
    clean(Rest, maybe_replace(X, Config) ++ Acc, Config);
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
    clean(Rest, maybe_replace(surrogate, Config) ++ Acc, Config);
%% noncharacters
clean(<<_/utf8, Rest/binary>>, Acc, Config) ->
    clean(Rest, maybe_replace(noncharacter, Config) ++ Acc, Config);
%% u+fffe and u+ffff for R14BXX
clean(<<239, 191, X, Rest/binary>>, Acc, Config) when X == 190; X == 191 ->
    clean(Rest, maybe_replace(noncharacter, Config) ++ Acc, Config);
%% overlong encodings and missing continuations of a 2 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 192, X =< 223 ->
    clean(strip_continuations(Rest, 1), maybe_replace(badutf, Config) ++ Acc, Config);
%% overlong encodings and missing continuations of a 3 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 224, X =< 239 ->
    clean(strip_continuations(Rest, 2), maybe_replace(badutf, Config) ++ Acc, Config);
%% overlong encodings and missing continuations of a 4 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 240, X =< 247 ->
    clean(strip_continuations(Rest, 3), maybe_replace(badutf, Config) ++ Acc, Config);
clean(<<_, Rest/binary>>, Acc, Config) ->
    clean(Rest, maybe_replace(badutf, Config) ++ Acc, Config).


strip_continuations(Bin, 0) -> Bin;
strip_continuations(<<X, Rest/binary>>, N) when X >= 128, X =< 191 ->
    strip_continuations(Rest, N - 1);
%% not a continuation byte
strip_continuations(Bin, _) -> Bin.


maybe_replace($\b, #config{escaped_strings=true}) -> [$b, $\\];
maybe_replace($\t, #config{escaped_strings=true}) -> [$t, $\\];
maybe_replace($\n, #config{escaped_strings=true}) -> [$n, $\\];
maybe_replace($\f, #config{escaped_strings=true}) -> [$f, $\\];
maybe_replace($\r, #config{escaped_strings=true}) -> [$r, $\\];
maybe_replace($\", #config{escaped_strings=true}) -> [$\", $\\];
maybe_replace($/, Config=#config{escaped_strings=true}) ->
    case Config#config.escaped_forward_slashes of
        true -> [$/, $\\];
        false -> [$/]
    end;
maybe_replace($\\, #config{escaped_strings=true}) -> [$\\, $\\];
maybe_replace(X, Config=#config{escaped_strings=true})  when X == 16#2028; X == 16#2029 ->
    case Config#config.unescaped_jsonp of
        true -> [X];
        false -> lists:reverse(json_escape_sequence(X))
    end;
maybe_replace(X, #config{escaped_strings=true}) when X < 32 ->
    lists:reverse(json_escape_sequence(X));
maybe_replace(noncharacter, #config{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(surrogate, #config{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(badutf, #config{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(_, _) -> {error, badarg}.


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
init([]) -> [].

handle_event(end_json, State) -> lists:reverse(State);
handle_event(Event, State) -> [Event] ++ State.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


parse(Events, Config) ->
    Chunk = try
        value(Events ++ [end_json], {jsx, []}, [], jsx_config:parse_config(Config))
    catch
        error:badarg -> {error, badarg}
    end,
    Incremental = try
        Final = lists:foldl(
            fun(Event, Parser) -> {incomplete, F} = Parser(Event), F end,
            parser(jsx, [], [explicit_end] ++ Config),
            lists:map(fun(X) -> [X] end, Events)
        ),
        Final(end_stream)
    catch
        error:badarg -> {error, badarg}
    end,
    ?assert(Chunk == Incremental),
    Chunk.


parse_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                Events ++ [end_json],
                parse(Events, [])
            )
        } || {Title, _, _, Events} <- Data
    ].


parse_error(Events, Config) -> value(Events, {jsx, []}, [], jsx_config:parse_config(Config)).


error_test_() ->
    [
        {"value error", ?_assertError(badarg, parse_error([self()], []))},
        {"maybe_done error", ?_assertError(badarg, parse_error([start_array, end_array, start_array, end_json], []))},
        {"done error", ?_assertError(badarg, parse_error([{string, <<"">>}, {literal, true}, end_json], []))},
        {"string error", ?_assertError(badarg, parse_error([{string, <<239, 191, 191>>}, end_json], []))}
    ].


custom_error_handler_test_() ->
    Error = fun(Rest, {_, State, _, _}, _) -> {State, Rest} end,
    [
        {"value error", ?_assertEqual(
            {value, [self()]},
            parse_error([self()], [{error_handler, Error}])
        )},
        {"maybe_done error", ?_assertEqual(
            {maybe_done, [start_array, end_json]},
            parse_error([start_array, end_array, start_array, end_json], [{error_handler, Error}])
        )},
        {"done error", ?_assertEqual(
            {maybe_done, [{literal, true}, end_json]},
            parse_error([{string, <<"">>}, {literal, true}, end_json], [{error_handler, Error}])
        )},
        {"string error", ?_assertEqual(
            {string, [{string, <<239, 191, 191>>}, end_json]},
            parse_error([{string, <<239, 191, 191>>}, end_json], [{error_handler, Error}])
        )}
    ].


custom_incomplete_handler_test_() ->
    [
        {"custom incomplete handler", ?_assertError(
            badarg,
            parse_error([], [{incomplete_handler, fun(_, _, _) -> erlang:error(badarg) end}])
        )}
    ].


raw_test_() ->
    [
        {"raw empty list", ?_assertEqual(
            [start_array, end_array, end_json],
            parse([{raw, <<"[]">>}], [])
        )},
        {"raw empty object", ?_assertEqual(
            [start_object, end_object, end_json],
            parse([{raw, <<"{}">>}], [])
        )},
        {"raw chunk inside stream", ?_assertEqual(
            [start_object, {key, <<"key">>}, start_array, {literal, true}, end_array, end_object, end_json],
            parse([start_object, {key, <<"key">>}, {raw, <<"[true]">>}, end_object], [])
        )}
    ].


-endif.