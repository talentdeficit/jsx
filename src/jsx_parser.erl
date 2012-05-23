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


-spec parser(Handler::module(), State::any(), Opts::jsx:opts()) -> jsx:parser().

parser(Handler, State, Opts) ->
    fun(Tokens) -> value(Tokens, {Handler, Handler:init(State)}, [], jsx_utils:parse_opts(Opts)) end.


-include("jsx_opts.hrl").


%% error, incomplete and event macros
-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


-ifndef(incomplete).
-define(incomplete(State, Handler, Stack, Opts),
    {incomplete, fun(Tokens) ->
                State(Tokens, Handler, Stack, Opts)
            ; (end_stream) ->
                case State([end_json],
                        Handler,
                        Stack,
                        Opts#opts{explicit_end=false}) of
                    {incomplete, _} -> ?error([Handler, Stack, Opts])
                    ; Events -> Events
                end
        end
    }
).
-endif.


handle_event([], Handler, _Opts) -> Handler;
handle_event([Event|Rest], Handler, Opts) -> handle_event(Rest, handle_event(Event, Handler, Opts), Opts);
handle_event(Event, {Handler, State}, _Opts) -> {Handler, Handler:handle_event(Event, State)}.


value([start_object|Tokens], Handler, Stack, Opts) ->
    object(Tokens, handle_event(start_object, Handler, Opts), [object|Stack], Opts);
value([start_array|Tokens], Handler, Stack, Opts) ->
    array(Tokens, handle_event(start_array, Handler, Opts), [array|Stack], Opts);
value([{literal, true}|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, true}, Handler, Opts), Stack, Opts);
value([true|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, true}, Handler, Opts), Stack, Opts);
value([{literal, false}|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, false}, Handler, Opts), Stack, Opts);
value([false|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, false}, Handler, Opts), Stack, Opts);
value([{literal, null}|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, null}, Handler, Opts), Stack, Opts);
value([null|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, null}, Handler, Opts), Stack, Opts);
value([{integer, Number}|Tokens], Handler, Stack, Opts) when is_integer(Number) ->
    maybe_done(Tokens, handle_event({integer, Number}, Handler, Opts), Stack, Opts);
value([{float, Number}|Tokens], Handler, Stack, Opts) when is_float(Number) ->
    maybe_done(Tokens, handle_event({float, Number}, Handler, Opts), Stack, Opts);
value([{number, Number}|Tokens], Handler, Stack, Opts) when is_integer(Number) ->
    maybe_done(Tokens, handle_event({integer, Number}, Handler, Opts), Stack, Opts);
value([{number, Number}|Tokens], Handler, Stack, Opts) when is_float(Number) ->
    maybe_done(Tokens, handle_event({float, Number}, Handler, Opts), Stack, Opts);
value([Number|Tokens], Handler, Stack, Opts) when is_integer(Number) ->
    maybe_done(Tokens, handle_event({integer, Number}, Handler, Opts), Stack, Opts);
value([Number|Tokens], Handler, Stack, Opts) when is_float(Number) ->
    maybe_done(Tokens, handle_event({float, Number}, Handler, Opts), Stack, Opts);
value([{string, String}|Tokens], Handler, Stack, Opts) when is_binary(String) ->
    maybe_done(Tokens, handle_event({string, String}, Handler, Opts), Stack, Opts);
value(BadTokens, Handler, Stack, Opts) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Opts]);
value(Token, Handler, Stack, Opts) ->
    value([Token], Handler, Stack, Opts).

object([end_object|Tokens], Handler, [object|Stack], Opts) ->
    maybe_done(Tokens, handle_event(end_object, Handler, Opts), Stack, Opts);
object([{key, Key}|Tokens], Handler, Stack, Opts) when is_binary(Key) ->
    value(Tokens, handle_event({key, Key}, Handler, Opts), Stack, Opts);
object(BadTokens, Handler, Stack, Opts) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Opts]);
object(Token, Handler, Stack, Opts) ->
    object([Token], Handler, Stack, Opts).

array([end_array|Tokens], Handler, [array|Stack], Opts) ->
    maybe_done(Tokens, handle_event(end_array, Handler, Opts), Stack, Opts);
array(Tokens, Handler, Stack, Opts) when is_list(Tokens) ->
    value(Tokens, Handler, Stack, Opts);
array(Token, Handler, Stack, Opts) ->
    array([Token], Handler, Stack, Opts).

maybe_done([end_json], Handler, [], Opts) ->
    {_, State} = handle_event(end_json, Handler, Opts),
    State;
maybe_done(Tokens, Handler, [object|_] = Stack, Opts) when is_list(Tokens) ->
    object(Tokens, Handler, Stack, Opts);
maybe_done(Tokens, Handler, [array|_] = Stack, Opts) when is_list(Tokens) ->
    array(Tokens, Handler, Stack, Opts);
maybe_done(BadTokens, Handler, Stack, Opts) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Opts]);
maybe_done(Token, Handler, Stack, Opts) ->
    maybe_done([Token], Handler, Stack, Opts).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode(Term) -> encode(Term, []).

encode(Term, Opts) ->
    try (parser(jsx, [], Opts))(Term)
    catch _:_ -> {error, badjson}
    end.


encode_test_() ->    
    [
        {"naked string", ?_assertEqual(
            encode([{string, <<"a string\n">>}, end_json]), [{string, <<"a string\n">>}, end_json]
        )},
        {"naked integer - simple rep", ?_assertEqual(
            encode([123, end_json]), [{integer, 123}, end_json]
        )},
        {"naked integer - alt rep", ?_assertEqual(
            encode([{number, 123}, end_json]), [{integer, 123}, end_json]
        )},
        {"naked integer - full rep", ?_assertEqual(
            encode([{integer, 123}, end_json]), [{integer, 123}, end_json]
        )},
        {"naked float - simple rep", ?_assertEqual(
            encode([1.23, end_json]), [{float, 1.23}, end_json]
        )},
        {"naked float - alt rep", ?_assertEqual(
            encode([{number, 1.23}, end_json]), [{float, 1.23}, end_json]
        )},
        {"naked float - full rep", ?_assertEqual(
            encode([{float, 1.23}, end_json]), [{float, 1.23}, end_json]
        )},
        {"naked literal - simple red", ?_assertEqual(
            encode([null, end_json]), [{literal, null}, end_json]
        )},
        {"naked literal - full rep", ?_assertEqual(
            encode([{literal, null}, end_json]), [{literal, null}, end_json]
        )},
        {"empty object", ?_assertEqual(
            encode([start_object, end_object, end_json]), [start_object, end_object, end_json]
        )},
        {"empty list", ?_assertEqual(
            encode([start_array, end_array, end_json]), [start_array, end_array, end_json]
        )},
        {"simple list", ?_assertEqual(
                encode([
                    start_array,
                    {integer, 1},
                    {integer, 2},
                    {integer, 3},
                    {literal, true},
                    {literal, false},
                    end_array,
                    end_json
                ]),
                [
                    start_array,
                    {integer, 1},
                    {integer, 2},
                    {integer, 3},
                    {literal, true},
                    {literal, false},
                    end_array,
                    end_json
                ]
            )
        },
        {"simple object", ?_assertEqual(
                encode([
                    start_object,
                    {key, <<"a">>},
                    {literal, true},
                    {key, <<"b">>},
                    {literal, false},
                    end_object,
                    end_json
                ]),
                [
                    start_object,
                    {key, <<"a">>},
                    {literal, true},
                    {key, <<"b">>},
                    {literal, false},
                    end_object,
                    end_json
                ]
            )
        },
        {"complex term", ?_assertEqual(
                encode([
                    start_object,
                    {key, <<"a">>},
                    {literal, true},
                    {key, <<"b">>},
                    {literal, false},
                    {key, <<"c">>},
                    start_array,
                        {integer, 1},
                        {integer, 2},
                        {integer, 3},
                    end_array,
                    {key, <<"d">>},
                    start_object,
                        {key, <<"key">>},
                        {string, <<"value">>},
                    end_object,
                    end_object,
                    end_json
                ]),
                [
                    start_object,
                    {key, <<"a">>},
                    {literal, true},
                    {key, <<"b">>},
                    {literal, false},
                    {key, <<"c">>},
                    start_array,
                        {integer, 1},
                        {integer, 2},
                        {integer, 3},
                    end_array,
                    {key, <<"d">>},
                    start_object,
                        {key, <<"key">>},
                        {string, <<"value">>},
                    end_object,
                    end_object,
                    end_json
                ]
            )
        }
    ].

-endif.