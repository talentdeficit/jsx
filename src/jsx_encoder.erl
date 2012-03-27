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
    Handler:handle_event(end_json, value(Term, {Handler, State}, Opts)).


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
value(List, {Handler, State}, Opts) when is_list(List) ->
    list_or_object(List, {Handler, State}, Opts);
value(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


list_or_object([Tuple|_] = List, {Handler, State}, Opts) when is_tuple(Tuple) ->
    object(List, {Handler, Handler:handle_event(start_object, State)}, Opts);
list_or_object(List, {Handler, State}, Opts) ->
    list(List, {Handler, Handler:handle_event(start_array, State)}, Opts).


object([{Key, Value}|Rest], {Handler, State}, Opts) ->
    object(
        Rest,
        {
            Handler,
            value(
                Value,
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), Opts)}, State)},
                Opts
            )
        },
        Opts
    );
object([], {Handler, State}, _Opts) -> Handler:handle_event(end_object, State);
object(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


list([Value|Rest], {Handler, State}, Opts) ->
    list(Rest, {Handler, value(Value, {Handler, State}, Opts)}, Opts);
list([], {Handler, State}, _Opts) -> Handler:handle_event(end_array, State);
list(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Opts) ->
    case Opts#opts.json_escape of
        true -> jsx_utils:json_escape(Bin, Opts);
        false -> 
            case is_clean(Bin) of
                true -> Bin;
                false -> clean_string(Bin, [], Opts)
            end
    end.


is_clean(<<>>) -> true;
is_clean(<<_/utf8, Rest/binary>>) -> is_clean(Rest);
is_clean(_) -> false.


clean_string(Bin, _Acc, Opts=#opts{loose_unicode=false}) -> ?error([Bin, Opts]);
clean_string(<<>>, Acc, _Opts) -> unicode:characters_to_binary(lists:reverse(Acc));
clean_string(<<X/utf8, Rest/binary>>, Acc, Opts) -> clean_string(Rest, [X] ++ Acc, Opts);
%% surrogates
clean_string(<<237, X, _, Rest/binary>>, Acc, Opts) when X >= 160 -> clean_string(Rest, [16#fffd] ++ Acc, Opts);
%% bad codepoints
clean_string(<<_, Rest/binary>>, Acc, Opts) -> clean_string(Rest, [16#fffd] ++ Acc, Opts).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode(Term) -> (encoder(jsx, [], []))(Term).

encode(Term, Opts) ->
    try (encoder(jsx, [], Opts))(Term)
    catch _:_ -> {error, badjson}
    end.


encode_test_() ->    
    [
        {"naked string", ?_assertEqual(encode(<<"a string">>), [{string, <<"a string">>}, end_json])},
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

surrogates_test_() ->
    [
        {"surrogates - badjson",
            ?_assertEqual(check_bad(surrogates()), [])
        },
        {"surrogates - replaced",
            ?_assertEqual(check_replaced(surrogates()), [])
        }
    ].
    
good_characters_test_() ->
    [
        {"acceptable codepoints",
            ?_assertEqual(check_good(good()), [])
        },
        {"acceptable extended",
            ?_assertEqual(check_good(good_extended()), [])
        }
    ].

malformed_test_() ->
    [
        {"malformed codepoint with 1 byte", ?_assertError(badarg, encode(<<128>>))},
        {"malformed codepoint with 2 bytes", ?_assertError(badarg, encode(<<128, 192>>))},
        {"malformed codepoint with 3 bytes", ?_assertError(badarg, encode(<<128, 192, 192>>))},
        {"malformed codepoint with 4 bytes", ?_assertError(badarg, encode(<<128, 192, 192, 192>>))}
    ].

malformed_replaced_test_() ->
    F = <<16#fffd/utf8>>,
    [
        {"malformed codepoint with 1 byte",
            ?_assertEqual(
                [{string, <<F/binary>>}, end_json],
                encode(<<128>>, [loose_unicode])
            )
        },
        {"malformed codepoint with 2 bytes",
            ?_assertEqual(
                [{string, <<F/binary, F/binary>>}, end_json],
                encode(<<128, 192>>, [loose_unicode])
            )
        },
        {"malformed codepoint with 3 bytes",
            ?_assertEqual(
                [{string, <<F/binary, F/binary, F/binary>>}, end_json],
                encode(<<128, 192, 192>>, [loose_unicode])
            )
        },
        {"malformed codepoint with 4 bytes",
            ?_assertEqual(
                [{string, <<F/binary, F/binary, F/binary, F/binary>>}, end_json],
                encode(<<128, 192, 192, 192>>, [loose_unicode])
            )
        }
    ].

check_bad(List) ->
    lists:dropwhile(fun({_, {error, badjson}}) -> true ; (_) -> false end,
        check(List, [], [])
    ).

check_replaced(List) ->
    lists:dropwhile(fun({_, [{string, <<16#fffd/utf8>>}|_]}) -> true
            ; (_) -> false 
        end,
        check(List, [loose_unicode], [])
    ).

check_good(List) ->
    lists:dropwhile(fun({_, [{string, _}|_]}) -> true ; (_) -> false end,
        check(List, [], [])
    ).

check([], _Opts, Acc) -> Acc;
check([H|T], Opts, Acc) ->
    R = encode(to_fake_utf(H, utf8), Opts),
    check(T, Opts, [{H, R}] ++ Acc).
    

surrogates() -> lists:seq(16#d800, 16#dfff).

good() -> lists:seq(1, 16#d7ff) ++ lists:seq(16#e000, 16#fffd).
            
good_extended() -> lists:seq(16#100000, 16#10ffff).

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