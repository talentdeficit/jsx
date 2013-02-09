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

pre_encode(Value, #opts{pre_encode=false}) -> Value; pre_encode(Value, Opts) ->
(Opts#opts.pre_encode)(Value).


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


escapes_test_() ->
    [
        {"backspace escape", ?_assertEqual(encode(<<"\b">>, [escaped_strings]), [{string, <<"\\b">>}, end_json])},
        {"formfeed escape", ?_assertEqual(encode(<<"\f">>, [escaped_strings]), [{string, <<"\\f">>}, end_json])},
        {"newline escape", ?_assertEqual(encode(<<"\n">>, [escaped_strings]), [{string, <<"\\n">>}, end_json])},
        {"carriage return escape", ?_assertEqual(encode(<<"\r">>, [escaped_strings]), [{string, <<"\\r">>}, end_json])},
        {"tab escape", ?_assertEqual(encode(<<"\t">>, [escaped_strings]), [{string, <<"\\t">>}, end_json])},
        {"quote escape", ?_assertEqual(encode(<<"\"">>, [escaped_strings]), [{string, <<"\\\"">>}, end_json])},
        {"single quote escape", ?_assertEqual(encode(<<"'">>, [escaped_strings, single_quoted_strings]), [{string, <<"\\'">>}, end_json])},
        {"no single quote escape", ?_assertEqual(encode(<<"'">>, [escaped_strings]), [{string, <<"'">>}, end_json])},
        {"forward slash escape", ?_assertEqual(encode(<<"/">>, [escaped_strings, escaped_forward_slashes]), [{string, <<"\\/">>}, end_json])},
        {"no forward slash escape", ?_assertEqual(encode(<<"/">>, [escaped_strings]), [{string, <<"/">>}, end_json])},
        {"back slash escape", ?_assertEqual(encode(<<"\\">>, [escaped_strings]), [{string, <<"\\\\">>}, end_json])},
        {"jsonp escape", ?_assertEqual(
            encode(<<16#2028/utf8, 16#2029/utf8>>, [escaped_strings]),
            [{string, <<"\\u2028\\u2029">>}, end_json]
        )},
        {"no jsonp escape", ?_assertEqual(
            encode(<<16#2028/utf8, 16#2029/utf8>>, [escaped_strings, unescaped_jsonp]),
            [{string, <<16#2028/utf8, 16#2029/utf8>>}, end_json]
        )},
        {"control escape", ?_assertEqual(encode(<<0>>, [escaped_strings]), [{string, <<"\\u0000">>}, end_json])},
        {"dirty strings", ?_assertEqual(encode(<<"\n">>, [escaped_strings, dirty_strings]), [{string, <<"\n">>}, end_json])},
        {"ignore bad escapes", ?_assertEqual(encode(<<"\\x25">>, [escaped_strings, ignored_bad_escapes]), [{string, <<"\\\\x25">>}, end_json])}
    ].


surrogates_test_() ->
    [
        {"surrogates - badarg",
            ?_assert(check_bad(surrogates()))
        },
        {"surrogates - replaced",
            ?_assert(check_replaced(surrogates()))
        }
    ].


good_characters_test_() ->
    [
        {"acceptable codepoints",
            ?_assert(check_good(good()))
        },
        {"acceptable codepoints - escaped_strings",
            ?_assert(check_good(good(), [escaped_strings]))
        },
        {"acceptable codepoints - replaced_bad_utf8",
            ?_assert(check_good(good(), [escaped_strings]))
        },
        {"acceptable codepoints - escaped_strings + replaced_bad_utf8",
            ?_assert(check_good(good(), [escaped_strings, replaced_bad_utf8]))
        },
        {"acceptable extended",
            ?_assert(check_good(good_extended()))
        },
        {"acceptable extended - escaped_strings",
            ?_assert(check_good(good_extended(), [escaped_strings]))
        },
        {"acceptable extended - escaped_strings",
            ?_assert(check_good(good_extended(), [replaced_bad_utf8]))
        }
    ].


reserved_test_() ->
    [
        {"reserved noncharacters - badarg",
            ?_assert(check_bad(reserved_space()))
        },
        {"reserved noncharacters - replaced",
            ?_assert(check_replaced(reserved_space()))
        }
    ].


noncharacters_test_() ->
    [
        {"noncharacters - badarg",
            ?_assert(check_bad(noncharacters()))
        },
        {"noncharacters - replaced",
            ?_assert(check_replaced(noncharacters()))
        }
    ].


extended_noncharacters_test_() ->
    [
        {"extended noncharacters - badarg",
            ?_assert(check_bad(extended_noncharacters()))
        },
        {"extended noncharacters - replaced",
            ?_assert(check_replaced(extended_noncharacters()))
        }
    ].


check_bad(List) ->
    [] == lists:dropwhile(fun({_, {error, badarg}}) -> true ; (_) -> false end,
        check(List, [], [])
    ).


check_replaced(List) ->
    [] == lists:dropwhile(fun({_, [{string, <<16#fffd/utf8>>}|_]}) -> true ; (_) -> false
        end,
        check(List, [replaced_bad_utf8], [])
    ).


check_good(List) -> check_good(List, []).

check_good(List, Opts) ->
    [] == lists:dropwhile(fun({_, [{string, _}|_]}) -> true ; (_) -> false end,
        check(List, Opts, [])
    ).


check([], _Opts, Acc) -> Acc;
check([H|T], Opts, Acc) ->
    R = encode(to_fake_utf(H, utf8), Opts),
    check(T, Opts, [{H, R}] ++ Acc).


noncharacters() -> lists:seq(16#fffe, 16#ffff).

extended_noncharacters() ->
    [16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff]
        ++ [16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff]
        ++ [16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff]
        ++ [16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff]
        ++ [16#9fffe, 16#9ffff, 16#afffe, 16#affff]
        ++ [16#bfffe, 16#bffff, 16#cfffe, 16#cffff]
        ++ [16#dfffe, 16#dffff, 16#efffe, 16#effff]
        ++ [16#ffffe, 16#fffff, 16#10fffe, 16#10ffff].

surrogates() -> lists:seq(16#d800, 16#dfff).

reserved_space() -> lists:seq(16#fdd0, 16#fdef).

good() -> lists:seq(16#0000, 16#d7ff) ++ lists:seq(16#e000, 16#fdcf) ++ lists:seq(16#fdf0, 16#fffd).

good_extended() -> [16#10000, 16#20000, 16#30000, 16#40000, 16#50000,
        16#60000, 16#70000, 16#80000, 16#90000, 16#a0000,
        16#b0000, 16#c0000, 16#d0000, 16#e0000, 16#f0000
    ] ++ lists:seq(16#100000, 16#10fffd).


%% erlang refuses to encode certain codepoints, so fake them all
to_fake_utf(N, utf8) when N < 16#0080 -> <<N:8>>;
to_fake_utf(N, utf8) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<2#110:3, Y:5, 2#10:2, X:6>>;
to_fake_utf(N, utf8) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6>>;
to_fake_utf(N, utf8) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6>>.


-endif.