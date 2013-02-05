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
    {incomplete, fun(end_stream) ->
                case State([end_json],
                        Handler,
                        Stack,
                        Opts) of
                    {incomplete, _} -> ?error([Handler, Stack, Opts])
                    ; Events -> Events
                end
            ; (Tokens) ->
                State(Tokens, Handler, Stack, Opts)
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
value([{literal, true}|Tokens], Handler, [], Opts) ->
    done(Tokens, handle_event({literal, true}, Handler, Opts), [], Opts);
value([{literal, false}|Tokens], Handler, [], Opts) ->
    done(Tokens, handle_event({literal, false}, Handler, Opts), [], Opts);
value([{literal, null}|Tokens], Handler, [], Opts) ->
    done(Tokens, handle_event({literal, null}, Handler, Opts), [], Opts);
value([{literal, true}|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, true}, Handler, Opts), Stack, Opts);
value([{literal, false}|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, false}, Handler, Opts), Stack, Opts);
value([{literal, null}|Tokens], Handler, Stack, Opts) ->
    maybe_done(Tokens, handle_event({literal, null}, Handler, Opts), Stack, Opts);
value([Literal|Tokens], Handler, Stack, Opts) when Literal == true; Literal == false; Literal == null ->
    value([{literal, Literal}] ++ Tokens, Handler, Stack, Opts);
value([{integer, Number}|Tokens], Handler, [], Opts) when is_integer(Number) ->
    done(Tokens, handle_event({integer, Number}, Handler, Opts), [], Opts);
value([{float, Number}|Tokens], Handler, [], Opts) when is_float(Number) ->
    done(Tokens, handle_event({float, Number}, Handler, Opts), [], Opts);
value([{integer, Number}|Tokens], Handler, Stack, Opts) when is_integer(Number) ->
    maybe_done(Tokens, handle_event({integer, Number}, Handler, Opts), Stack, Opts);
value([{float, Number}|Tokens], Handler, Stack, Opts) when is_float(Number) ->
    maybe_done(Tokens, handle_event({float, Number}, Handler, Opts), Stack, Opts);
value([{number, Number}|Tokens], Handler, Stack, Opts) when is_integer(Number) ->
    value([{integer, Number}] ++ Tokens, Handler, Stack, Opts);
value([{number, Number}|Tokens], Handler, Stack, Opts) when is_float(Number) ->
    value([{float, Number}] ++ Tokens, Handler, Stack, Opts);
value([Number|Tokens], Handler, Stack, Opts) when is_integer(Number) ->
    value([{integer, Number}] ++ Tokens, Handler, Stack, Opts);
value([Number|Tokens], Handler, Stack, Opts) when is_float(Number) ->
    value([{float, Number}] ++ Tokens, Handler, Stack, Opts);
value([{string, String}|Tokens], Handler, [], Opts) when is_binary(String) ->
    done(Tokens, handle_event({string, clean_string(String, Opts)}, Handler, Opts), [], Opts);
value([{string, String}|Tokens], Handler, Stack, Opts) when is_binary(String) ->
    maybe_done(Tokens, handle_event({string, clean_string(String, Opts)}, Handler, Opts), Stack, Opts);
value([String|Tokens], Handler, Stack, Opts) when is_binary(String) ->
    value([{string, String}] ++ Tokens, Handler, Stack, Opts);
value([], Handler, Stack, Opts) ->
    ?incomplete(value, Handler, Stack, Opts);
value(BadTokens, Handler, Stack, Opts) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Opts]);
value(Token, Handler, Stack, Opts) ->
    value([Token], Handler, Stack, Opts).

object([end_object|Tokens], Handler, [object|Stack], Opts) ->
    maybe_done(Tokens, handle_event(end_object, Handler, Opts), Stack, Opts);
object([{key, Key}|Tokens], Handler, Stack, Opts) when is_atom(Key); is_binary(Key) ->
    value(Tokens, handle_event({key, clean_string(fix_key(Key), Opts)}, Handler, Opts), Stack, Opts);
object([Key|Tokens], Handler, Stack, Opts) when is_atom(Key); is_binary(Key) ->
    value(Tokens, handle_event({key, clean_string(fix_key(Key), Opts)}, Handler, Opts), Stack, Opts);
object([], Handler, Stack, Opts) ->
    ?incomplete(object, Handler, Stack, Opts);
object(BadTokens, Handler, Stack, Opts) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Opts]);
object(Token, Handler, Stack, Opts) ->
    object([Token], Handler, Stack, Opts).

array([end_array|Tokens], Handler, [array|Stack], Opts) ->
    maybe_done(Tokens, handle_event(end_array, Handler, Opts), Stack, Opts);
array([], Handler, Stack, Opts) ->
    ?incomplete(array, Handler, Stack, Opts);
array(Tokens, Handler, Stack, Opts) when is_list(Tokens) ->
    value(Tokens, Handler, Stack, Opts);
array(Token, Handler, Stack, Opts) ->
    array([Token], Handler, Stack, Opts).

maybe_done([end_json], Handler, [], Opts) ->
    done([], Handler, [], Opts);
maybe_done(Tokens, Handler, [object|_] = Stack, Opts) when is_list(Tokens) ->
    object(Tokens, Handler, Stack, Opts);
maybe_done(Tokens, Handler, [array|_] = Stack, Opts) when is_list(Tokens) ->
    array(Tokens, Handler, Stack, Opts);
maybe_done([], Handler, Stack, Opts) ->
    ?incomplete(maybe_done, Handler, Stack, Opts);
maybe_done(BadTokens, Handler, Stack, Opts) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Opts]);
maybe_done(Token, Handler, Stack, Opts) ->
    maybe_done([Token], Handler, Stack, Opts).

done(Tokens, Handler, [], Opts) when Tokens == [end_json]; Tokens == [] ->
    {_, State} = handle_event(end_json, Handler, Opts),
    State;
done(BadTokens, Handler, Stack, Opts) when is_list(BadTokens) ->
    ?error([BadTokens, Handler, Stack, Opts]);
done(Token, Handler, Stack, Opts) ->
    done([Token], Handler, Stack, Opts).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Opts) -> jsx_utils:clean_string(Bin, Opts).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


incomplete_test_() ->
    F = parser(jsx, [], []),
    [
        {"incomplete test", ?_assertEqual(
            begin
                {incomplete, A} = F(start_object),
                {incomplete, B} = A(key),
                {incomplete, C} = B(true),
                {incomplete, D} = C(end_object),
                D(end_json)
            end,
            [start_object, {key, <<"key">>}, {literal, true}, end_object, end_json]
        )}
    ].

encode(Term) -> encode(Term, []).

encode(Term, Opts) ->
    try (parser(jsx, [], Opts))(Term)
    catch error:badarg -> {error, badarg}
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
        {"naked literal - simple rep", ?_assertEqual(
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
        },
        {"atom keys", ?_assertEqual(
            encode([start_object, {key, key}, {string, <<"value">>}, end_object, end_json]),
            [start_object, {key, <<"key">>}, {string, <<"value">>}, end_object, end_json]
        )}
    ].

encode_failures_test_() ->
    [
        {"unwrapped values", ?_assertEqual(
            {error, badarg},
            encode([{string, <<"a string\n">>}, {string, <<"a string\n">>}, end_json])
        )},
        {"unbalanced array", ?_assertEqual(
            {error, badarg},
            encode([start_array, end_array, end_array, end_json])
        )},
        {"premature finish", ?_assertEqual(
            {error, badarg},
            encode([start_object, {key, <<"key">>, start_array, end_json}])
        )},
        {"really premature finish", ?_assertEqual(
            {error, badarg},
            encode([end_json])
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