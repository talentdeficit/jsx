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


%handle_event([], Handler, _Config) -> Handler;
%handle_event([Event|Rest], Handler, Config) -> handle_event(Rest, handle_event(Event, Handler, Config), Config);
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
    case clean_string(String, Tokens, Handler, [], Config) of
        Clean when is_binary(Clean) ->
            done(Tokens, handle_event({string, Clean}, Handler, Config), [], Config);
        Error -> Error
    end;
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


%% for raw input
-spec init(proplists:proplist()) -> list().

init([]) -> [].

-spec handle_event(Event::any(), Acc::list()) -> list().

handle_event(end_json, State) -> lists:reverse(State);
handle_event(Event, State) -> [Event] ++ State.


-include("jsx_strings.hrl").


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
            {done, [{literal, true}, end_json]},
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
