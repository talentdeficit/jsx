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


-module(jsx_encoder).

-export([encoder/3]).

-spec encoder(Handler::module(), State::any(), Config::list()) -> jsx:encoder().

encoder(Handler, State, Config) ->
    fun(JSON) ->
        start(
            JSON,
            {Handler, Handler:init(State)},
            jsx_config:parse_config(Config)
        )
    end.



-include("jsx_config.hrl").


-ifndef(error).
-define(error(State, Term, Handler, Config),
    case Config#config.error_handler of
        false -> erlang:error(badarg);
        F -> erlang:throw(F(Term, {encoder, State, Handler}, jsx_config:config_to_list(Config)))
    end
).
-endif.


start(Term, {Handler, State}, Config) ->
    try Handler:handle_event(end_json, value(pre_encode(Term, Config), {Handler, State}, Config))
    catch
        throw:Error -> Error;
        Type:Value -> erlang:Type(Value)
    end.


value(String, {Handler, State}, Config) when is_binary(String) ->
    Handler:handle_event({string, clean_string(String, {Handler, State}, Config)}, State);
value(Float, {Handler, State}, _Config) when is_float(Float) ->
    Handler:handle_event({float, Float}, State);
value(Int, {Handler, State}, _Config) when is_integer(Int) ->
    Handler:handle_event({integer, Int}, State);
value(Literal, {Handler, State}, _Config)
        when Literal == true; Literal == false; Literal == null ->
    Handler:handle_event({literal, Literal}, State);
value([{}], {Handler, State}, _Config) ->
    Handler:handle_event(end_object, Handler:handle_event(start_object, State));
value([], {Handler, State}, _Config) ->
    Handler:handle_event(end_array, Handler:handle_event(start_array, State));
value(List, Handler, Config) when is_list(List) ->
    list_or_object(List, Handler, Config);
value(Term, Handler, Config) -> ?error(value, Term, Handler, Config).


list_or_object([Term|Rest], {Handler, State}, Config) ->
    case pre_encode(Term, Config) of
        {K, V} when is_atom(K); is_binary(K) ->
            object([{K, V}|Rest], {Handler, Handler:handle_event(start_object, State)}, Config)
        ; T ->
            list([T|Rest], {Handler, Handler:handle_event(start_array, State)}, Config)
    end.


object([{Key, Value}, Next|Rest], {Handler, State}, Config) when is_atom(Key); is_binary(Key) ->
    V = pre_encode(Value, Config),
    object(
        [pre_encode(Next, Config)|Rest],
        {
            Handler,
            value(
                V,
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), {Handler, State}, Config)}, State)},
                Config
            )
        },
        Config
    );
object([{Key, Value}], {Handler, State}, Config) when is_atom(Key); is_binary(Key) ->
    object(
        [],
        {
            Handler,
            value(
                pre_encode(Value, Config),
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), {Handler, State}, Config)}, State)},
                Config
            )
        },
        Config
    );
object([], {Handler, State}, _Config) -> Handler:handle_event(end_object, State);
object(Term, Handler, Config) -> ?error(object, Term, Handler, Config).


list([Value, Next|Rest], {Handler, State}, Config) ->
    list([pre_encode(Next, Config)|Rest], {Handler, value(Value, {Handler, State}, Config)}, Config);
list([Value], {Handler, State}, Config) ->
    list([], {Handler, value(Value, {Handler, State}, Config)}, Config);
list([], {Handler, State}, _Config) -> Handler:handle_event(end_array, State).

pre_encode(Value, #config{pre_encode=false}) -> Value;
pre_encode(Value, Config) -> (Config#config.pre_encode)(Value).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Handler, Config) ->
    case clean_string(Bin, Config) of
        {error, badarg} -> ?error(string, Bin, Handler, Config);
        String -> String
    end.



-include("jsx_strings.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


encode_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                Events ++ [end_json],
                start(Term, {jsx, []}, #config{})
            )
        } || {Title, _, Term, Events} <- Data
    ].


encode(Term, Config) -> start(Term, {jsx, []}, jsx_config:parse_config(Config)).

pre_encoders_test_() ->
    Term = [
        {<<"object">>, [
            {<<"literals">>, [true, false, null]},
            {<<"strings">>, [<<"foo">>, <<"bar">>, <<"baz">>]},
            {<<"numbers">>, [1, 1.0, 1.0e0]}
        ]}
    ],
    [
        {"no pre encode", ?_assertEqual(
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array,
                            {literal, true}, {literal, false}, {literal, null},
                        end_array,
                        {key, <<"strings">>}, start_array,
                            {string, <<"foo">>}, {string, <<"bar">>}, {string, <<"baz">>},
                        end_array,
                        {key, <<"numbers">>}, start_array,
                            {integer, 1}, {float, 1.0}, {float, 1.0},
                        end_array,
                    end_object,
                end_object,
                end_json
            ],
            encode(Term, [])
        )},
        {"replace lists with empty lists", ?_assertEqual(
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array, end_array,
                        {key, <<"strings">>}, start_array, end_array,
                        {key, <<"numbers">>}, start_array, end_array,
                    end_object,
                end_object,
                end_json
            ],
            encode(Term, [{pre_encode, fun(V) -> case V of [{_,_}|_] -> V; [{}] -> V; V when is_list(V) -> []; _ -> V end end}])
        )},
        {"replace objects with empty objects", ?_assertEqual(
            [
                start_object,
                end_object,
                end_json
            ],
            encode(Term, [{pre_encode, fun(V) -> case V of [{_,_}|_] -> [{}]; _ -> V end end}])
        )},
        {"replace all non-list and non_tuple values with false", ?_assertEqual(
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array,
                            {literal, false}, {literal, false}, {literal, false},
                        end_array,
                        {key, <<"strings">>}, start_array,
                            {literal, false}, {literal, false}, {literal, false},
                        end_array,
                        {key, <<"numbers">>}, start_array,
                            {literal, false}, {literal, false}, {literal, false},
                        end_array,
                    end_object,
                end_object,
                end_json
            ],
            encode(Term, [{pre_encode, fun(V) when is_list(V); is_tuple(V) -> V; (_) -> false end}])
        )},
        {"replace all atoms with atom_to_list", ?_assertEqual(
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array,
                            {string, <<"true">>}, {string, <<"false">>}, {string, <<"null">>},
                        end_array,
                        {key, <<"strings">>}, start_array,
                            {string, <<"foo">>}, {string, <<"bar">>}, {string, <<"baz">>},
                        end_array,
                        {key, <<"numbers">>}, start_array,
                            {integer, 1}, {float, 1.0}, {float, 1.0},
                        end_array,
                    end_object,
                end_object,
                end_json
            ],
            encode(Term, [{pre_encode, fun(V) when is_atom(V) -> unicode:characters_to_binary(atom_to_list(V)); (V) -> V end}])
        )},
        {"pre_encode tuple", ?_assertEqual(
            [
                start_array,
                    {integer, 1}, {integer, 2}, {integer, 3},
                end_array,
                end_json
            ],
            encode({1, 2, 3}, [{pre_encode, fun(Tuple) when is_tuple(Tuple) -> tuple_to_list(Tuple); (V) -> V end}])
        )},
        {"pre_encode 2-tuples", ?_assertEqual(
            [
                start_object,
                    {key, <<"two">>}, {integer, 2}, {key, <<"three">>}, {integer, 3},
                end_object,
                end_json
            ],
            encode([{two, 1}, {three, 2}], [{pre_encode, fun({K, V}) -> {K, V + 1}; (V) -> V end}])
        )},
        {"pre_encode one field record", ?_assertEqual(
            [
                start_object,
                    {key, <<"bar">>}, {literal, false},
                end_object,
                end_json
            ],
            encode([{foo, bar}], [{pre_encode, fun({foo, V}) -> {V, undefined}; (undefined) -> false; (V) -> V end}])
        )},
        {"pre_encode list", ?_assertEqual(
            [
                start_array,
                    {integer, 2}, {integer, 3}, {integer, 4},
                end_array,
                end_json
            ],
            encode([1,2,3], [{pre_encode, fun(X) when is_integer(X) -> X + 1; (V) -> V end}])
        )}
    ].

error_test_() ->
    [
        {"value error", ?_assertError(badarg, encode(self(), []))},
        {"string error", ?_assertError(badarg, encode(<<239, 191, 191>>, []))}
    ].

custom_error_handler_test_() ->
    Error = fun(Term, {_, State, _}, _) -> {State, Term} end, 
    [
        {"value error", ?_assertEqual(
            {value, self()},
            encode(self(), [{error_handler, Error}])
        )},
        {"string error", ?_assertEqual(
            {string, <<239, 191, 191>>},
            encode(<<239, 191, 191>>, [{error_handler, Error}])
        )}
    ].

-endif.
