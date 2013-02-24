%% The MIT License

%% Copyright (c) 2012 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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

-export([parser/3]).


-spec parser(Handler::module(), State::any(), Config::jsx:config()) -> jsx:parser().

parser(Handler, State, Config) ->
    fun(Tokens) -> value(Tokens, {Handler, Handler:init(State)}, [], jsx_utils:parse_config(Config)) end.


-include("jsx_config.hrl").


%% error, incomplete and event macros
-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


-ifndef(incomplete).
-define(incomplete(State, Handler, Stack, Config),
    {incomplete, fun(end_stream) ->
                case State([end_json],
                        Handler,
                        Stack,
                        Config) of
                    {incomplete, _} -> ?error([Handler, Stack, Config])
                    ; Events -> Events
                end
            ; (Tokens) ->
                State(Tokens, Handler, Stack, Config)
        end
    }
).
-endif.


handle_event([], Handler, _Config) -> Handler;
handle_event([Event|Rest], Handler, Config) -> handle_event(Rest, handle_event(Event, Handler, Config), Config);
handle_event(Event, {Handler, State}, _Config) -> {Handler, Handler:handle_event(Event, State)}.


value([start_object|Tokens], Handler, Stack, Config) ->
    object(Tokens, handle_event(start_object, Handler, Config), [object|Stack], Config);
value([start_array|Tokens], Handler, Stack, Config) ->
    array(Tokens, handle_event(start_array, Handler, Config), [array|Stack], Config);
value([{literal, true}|Tokens], Handler, [], Config) ->
    done(Tokens, handle_event({literal, true}, Handler, Config), [], Config);
value([{literal, false}|Tokens], Handler, [], Config) ->
    done(Tokens, handle_event({literal, false}, Handler, Config), [], Config);
value([{literal, null}|Tokens], Handler, [], Config) ->
    done(Tokens, handle_event({literal, null}, Handler, Config), [], Config);
value([{literal, true}|Tokens], Handler, Stack, Config) ->
    maybe_done(Tokens, handle_event({literal, true}, Handler, Config), Stack, Config);
value([{literal, false}|Tokens], Handler, Stack, Config) ->
    maybe_done(Tokens, handle_event({literal, false}, Handler, Config), Stack, Config);
value([{literal, null}|Tokens], Handler, Stack, Config) ->
    maybe_done(Tokens, handle_event({literal, null}, Handler, Config), Stack, Config);
value([Literal|Tokens], Handler, Stack, Config) when Literal == true; Literal == false; Literal == null ->
    value([{literal, Literal}] ++ Tokens, Handler, Stack, Config);
value([{integer, Number}|Tokens], Handler, [], Config) when is_integer(Number) ->
    done(Tokens, handle_event({integer, Number}, Handler, Config), [], Config);
value([{float, Number}|Tokens], Handler, [], Config) when is_float(Number) ->
    done(Tokens, handle_event({float, Number}, Handler, Config), [], Config);
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
value([{string, String}|Tokens], Handler, [], Config) when is_binary(String) ->
    done(Tokens, handle_event({string, clean_string(String, Config)}, Handler, Config), [], Config);
value([{string, String}|Tokens], Handler, Stack, Config) when is_binary(String) ->
    maybe_done(Tokens, handle_event({string, clean_string(String, Config)}, Handler, Config), Stack, Config);
value([String|Tokens], Handler, Stack, Config) when is_binary(String) ->
    value([{string, String}] ++ Tokens, Handler, Stack, Config);
value([], Handler, Stack, Config) ->
    ?incomplete(value, Handler, Stack, Config);
value(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Config]);
value(Token, Handler, Stack, Config) ->
    value([Token], Handler, Stack, Config).

object([end_object|Tokens], Handler, [object|Stack], Config) ->
    maybe_done(Tokens, handle_event(end_object, Handler, Config), Stack, Config);
object([{key, Key}|Tokens], Handler, Stack, Config) when is_atom(Key); is_binary(Key) ->
    value(Tokens, handle_event({key, clean_string(fix_key(Key), Config)}, Handler, Config), Stack, Config);
object([Key|Tokens], Handler, Stack, Config) when is_atom(Key); is_binary(Key) ->
    object([{key, Key}] ++ Tokens, Handler, Stack, Config);
object([], Handler, Stack, Config) ->
    ?incomplete(object, Handler, Stack, Config);
object(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Config]);
object(Token, Handler, Stack, Config) ->
    object([Token], Handler, Stack, Config).

array([end_array|Tokens], Handler, [array|Stack], Config) ->
    maybe_done(Tokens, handle_event(end_array, Handler, Config), Stack, Config);
array([], Handler, Stack, Config) ->
    ?incomplete(array, Handler, Stack, Config);
array(Tokens, Handler, Stack, Config) when is_list(Tokens) ->
    value(Tokens, Handler, Stack, Config);
array(Token, Handler, Stack, Config) ->
    array([Token], Handler, Stack, Config).

maybe_done([end_json], Handler, [], Config) ->
    done([], Handler, [], Config);
maybe_done(Tokens, Handler, [object|_] = Stack, Config) when is_list(Tokens) ->
    object(Tokens, Handler, Stack, Config);
maybe_done(Tokens, Handler, [array|_] = Stack, Config) when is_list(Tokens) ->
    array(Tokens, Handler, Stack, Config);
maybe_done([], Handler, Stack, Config) ->
    ?incomplete(maybe_done, Handler, Stack, Config);
maybe_done(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Config]);
maybe_done(Token, Handler, Stack, Config) ->
    maybe_done([Token], Handler, Stack, Config).

done(Tokens, Handler, [], Config) when Tokens == [end_json]; Tokens == [] ->
    {_, State} = handle_event(end_json, Handler, Config),
    State;
done(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Config]);
done(Token, Handler, Stack, Config) ->
    done([Token], Handler, Stack, Config).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Config) -> jsx_utils:clean_string(Bin, Config).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


decode_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                Events ++ [end_json],
                value(Events ++ [end_json], {jsx, []}, [], #config{})
            )
        } || {Title, _, _, Events} <- Data
    ].


-endif.