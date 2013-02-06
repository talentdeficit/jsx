%% The MIT License

%% Copyright (c) 2011 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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

-spec encoder(Handler::module(), State::any(), Opts::jsx:opts()) -> jsx:encoder().

encoder(Handler, State, Opts) ->
    fun(JSON) ->
        start(
            JSON,
            {Handler, Handler:init(State)},
            jsx_utils:parse_opts(Opts)
        )
    end.



-include("jsx_opts.hrl").


-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


start(Term, {Handler, State}, Opts) ->
    Handler:handle_event(end_json, value(pre_encode(Term, Opts), {Handler, State}, Opts)).


value(String, {Handler, State}, Opts) when is_binary(String) ->
    Handler:handle_event({string, clean_string(String, Opts)}, State);
value(Float, {Handler, State}, _Opts) when is_float(Float) ->
    Handler:handle_event({float, Float}, State);
value(Int, {Handler, State}, _Opts) when is_integer(Int) ->
    Handler:handle_event({integer, Int}, State);
value(Literal, {Handler, State}, _Opts)
        when Literal == true; Literal == false; Literal == null ->
    Handler:handle_event({literal, Literal}, State);
value([{}], {Handler, State}, _Opts) ->
    Handler:handle_event(end_object, Handler:handle_event(start_object, State));
value([], {Handler, State}, _Opts) ->
    Handler:handle_event(end_array, Handler:handle_event(start_array, State));
value([Tuple|_] = List, Handler, Opts) when is_tuple(Tuple) ->
    list_or_object(List, Handler, Opts);
value(List, Handler, Opts) when is_list(List) ->
    list_or_object(List, Handler, Opts);
value(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


list_or_object([Term|Rest], {Handler, State}, Opts) ->
    case pre_encode(Term, Opts) of
        {K, V} ->
            object([{K, V}|Rest], {Handler, Handler:handle_event(start_object, State)}, Opts)
        ; T ->
            list([T|Rest], {Handler, Handler:handle_event(start_array, State)}, Opts)
    end.


object([{Key, Value}, Next|Rest], {Handler, State}, Opts) when is_atom(Key); is_binary(Key) ->
    V = pre_encode(Value, Opts),
    object(
        [pre_encode(Next, Opts)|Rest],
        {
            Handler,
            value(
                V,
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), Opts)}, State)},
                Opts
            )
        },
        Opts
    );
object([{Key, Value}], {Handler, State}, Opts) when is_atom(Key); is_binary(Key) ->
    object(
        [],
        {
            Handler,
            value(
                pre_encode(Value, Opts),
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), Opts)}, State)},
                Opts
            )
        },
        Opts
    );
object([], {Handler, State}, _Opts) -> Handler:handle_event(end_object, State);
object(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


list([Value, Next|Rest], {Handler, State}, Opts) ->
    list([pre_encode(Next, Opts)|Rest], {Handler, value(Value, {Handler, State}, Opts)}, Opts);
list([Value], {Handler, State}, Opts) ->
	list([], {Handler, value(Value, {Handler, State}, Opts)}, Opts);
list([], {Handler, State}, _Opts) -> Handler:handle_event(end_array, State);
list(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


pre_encode(Value, #opts{pre_encode=false}) -> Value;
pre_encode(Value, Opts) -> (Opts#opts.pre_encode)(Value).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Opts) -> jsx_utils:clean_string(Bin, Opts).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


encode(Term) -> encode(Term, []).

encode(Term, Opts) ->
    try (encoder(jsx, [], Opts))(Term)
    catch _:_ -> {error, badarg}
    end.


encode_test_() ->
    [
        {"naked string", ?_assertEqual(encode(<<"a string\n">>), [{string, <<"a string\n">>}, end_json])},
        {"escaped naked string", ?_assertEqual(encode(<<"a string\n">>, [escaped_strings]), [{string, <<"a string\\n">>}, end_json])},
        {"naked integer", ?_assertEqual(encode(123), [{integer, 123}, end_json])},
        {"naked float", ?_assertEqual(encode(1.23), [{float, 1.23}, end_json])},
        {"naked literal", ?_assertEqual(encode(null), [{literal, null}, end_json])},
        {"empty object", ?_assertEqual(encode([{}]), [start_object, end_object, end_json])},
        {"empty list", ?_assertEqual(encode([]), [start_array, end_array, end_json])},
        {"simple list", ?_assertEqual(
                encode([1,2,3,true,false]),
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
                encode([{<<"a">>, true}, {<<"b">>, false}]),
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
                    {<<"a">>, true},
                    {<<"b">>, false},
                    {<<"c">>, [1,2,3]},
                    {<<"d">>, [{<<"key">>, <<"value">>}]}
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
        },
        {"atom keys", ?_assertEqual(
                encode([{key, <<"value">>}]),
                [start_object, {key, <<"key">>}, {string, <<"value">>}, end_object, end_json]
            )
        }
    ].


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
            encode(Term, []),
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
            ]
        )},
        {"replace lists with empty lists", ?_assertEqual(
            encode(Term, [{pre_encode, fun(V) -> case V of [{_,_}|_] -> V; [{}] -> V; V when is_list(V) -> []; _ -> V end end}]),
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array, end_array,
                        {key, <<"strings">>}, start_array, end_array,
                        {key, <<"numbers">>}, start_array, end_array,
                    end_object,
                end_object,
                end_json
            ]
        )},
        {"replace objects with empty objects", ?_assertEqual(
            encode(Term, [{pre_encode, fun(V) -> case V of [{_,_}|_] -> [{}]; _ -> V end end}]),
            [
                start_object,
                end_object,
                end_json
            ]
        )},
        {"replace all non-list and non_tuple values with false", ?_assertEqual(
            encode(Term, [{pre_encode, fun(V) when is_list(V); is_tuple(V) -> V; (_) -> false end}]),
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
            ]
        )},
        {"replace all atoms with atom_to_list", ?_assertEqual(
            encode(Term, [{pre_encode, fun(V) when is_atom(V) -> unicode:characters_to_binary(atom_to_list(V)); (V) -> V end}]),
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
            ]
        )},
        {"pre_encode tuple", ?_assertEqual(
            encode({1, 2, 3}, [{pre_encode, fun(Tuple) when is_tuple(Tuple) -> tuple_to_list(Tuple); (V) -> V end}]),
            [
                start_array,
                    {integer, 1}, {integer, 2}, {integer, 3},
                end_array,
                end_json
            ]
        )},
        {"pre_encode 2-tuples", ?_assertEqual(
            encode([{two, 1}, {three, 2}], [{pre_encode, fun({K, V}) -> {K, V + 1}; (V) -> V end}]),
            [
                start_object,
                    {key, <<"two">>}, {integer, 2}, {key, <<"three">>}, {integer, 3},
                end_object,
                end_json
            ]
        )},
        {"pre_encode one field record", ?_assertEqual(
            encode([{foo, bar}], [{pre_encode, fun({foo, V}) -> {V, undefined}; (undefined) -> false; (V) -> V end}]),
            [
                start_object,
                    {key, <<"bar">>}, {literal, false},
                end_object,
                end_json
            ]
        )},
        {"pre_encode list", ?_assertEqual(
            encode([1,2,3], [{pre_encode, fun(X) when is_integer(X) -> X + 1; (V) -> V end}]),
            [
                start_array,
                    {integer, 2}, {integer, 3}, {integer, 4},
                end_array,
            end_json
            ]
        )}
    ].


-endif.