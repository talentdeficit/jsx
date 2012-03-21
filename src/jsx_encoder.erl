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
    Handler:handle_event({string, check_string(String, {Handler, State}, Opts)}, State);
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
                {Handler, Handler:handle_event({key, check_string(fix_key(Key), {Handler, State}, Opts)}, State)},
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


check_string(String, Handler, Opts) ->
    case check_string(String) of
        true -> String;
        false ->
            case Opts#opts.loose_unicode of
                true -> clean_string(String, <<>>);
                false -> erlang:error(badarg, [String, Handler, Opts])
            end
    end.

check_string(<<C/utf8, Rest/binary>>) when C < 16#fdd0 ->
    check_string(Rest);
check_string(<<C/utf8, Rest/binary>>) when C > 16#fdef, C < 16#fffe ->
    check_string(Rest);
check_string(<<C/utf8, Rest/binary>>)
        when C =/= 16#fffe andalso C =/= 16#ffff andalso
            C =/= 16#1fffe andalso C =/= 16#1ffff andalso
            C =/= 16#2fffe andalso C =/= 16#2ffff andalso
            C =/= 16#3fffe andalso C =/= 16#3ffff andalso
            C =/= 16#4fffe andalso C =/= 16#4ffff andalso
            C =/= 16#5fffe andalso C =/= 16#5ffff andalso
            C =/= 16#6fffe andalso C =/= 16#6ffff andalso
            C =/= 16#7fffe andalso C =/= 16#7ffff andalso
            C =/= 16#8fffe andalso C =/= 16#8ffff andalso
            C =/= 16#9fffe andalso C =/= 16#9ffff andalso
            C =/= 16#afffe andalso C =/= 16#affff andalso
            C =/= 16#bfffe andalso C =/= 16#bffff andalso
            C =/= 16#cfffe andalso C =/= 16#cffff andalso
            C =/= 16#dfffe andalso C =/= 16#dffff andalso
            C =/= 16#efffe andalso C =/= 16#effff andalso
            C =/= 16#ffffe andalso C =/= 16#fffff andalso
            C =/= 16#10fffe andalso C =/= 16#10ffff ->
    check_string(Rest);
check_string(<<>>) -> true;
check_string(<<_, _/binary>>) -> false.

clean_string(<<C/utf8, Rest/binary>>, Acc) when C >= 16#fdd0, C =< 16#fdef ->
    io:format("1: ~p~n", [C]),
    clean_string(Rest, <<Acc/binary, 16#fffd/utf8>>);
clean_string(<<C/utf8, Rest/binary>>, Acc)
        when C == 16#fffe orelse C == 16#ffff orelse
            C == 16#1fffe orelse C == 16#1ffff orelse
            C == 16#2fffe orelse C == 16#2ffff orelse
            C == 16#3fffe orelse C == 16#3ffff orelse
            C == 16#4fffe orelse C == 16#4ffff orelse
            C == 16#5fffe orelse C == 16#5ffff orelse
            C == 16#6fffe orelse C == 16#6ffff orelse
            C == 16#7fffe orelse C == 16#7ffff orelse
            C == 16#8fffe orelse C == 16#8ffff orelse
            C == 16#9fffe orelse C == 16#9ffff orelse
            C == 16#afffe orelse C == 16#affff orelse
            C == 16#bfffe orelse C == 16#bffff orelse
            C == 16#cfffe orelse C == 16#cffff orelse
            C == 16#dfffe orelse C == 16#dffff orelse
            C == 16#efffe orelse C == 16#effff orelse
            C == 16#ffffe orelse C == 16#fffff orelse
            C == 16#10fffe orelse C == 16#10ffff ->
    io:format("2: ~p~n", [C]),
    clean_string(Rest, <<Acc/binary, 16#fffd/utf8>>);
clean_string(<<C/utf8, Rest/binary>>, Acc) ->
    io:format("3: ~p~n", [C]),
    clean_string(Rest, <<Acc/binary, C/utf8>>);
clean_string(<<>>, Acc) -> Acc.
    


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode(Term) -> (encoder(jsx, [], []))(Term).

encode(Term, Opts) -> (encoder(jsx, [], Opts))(Term).


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
        },
        {"bad string", ?_assertError(
                badarg,
                encode([<<"a bad string: ", 16#ffff/utf8>>])
            )
        },
        {"allow bad string", ?_assertEqual(
                encode([<<"a bad string: ", 16#1ffff/utf8>>], [loose_unicode]),
                [start_array, {string, <<"a bad string: ", 16#fffd/utf8>>}, end_array, end_json]
            )
        }
    ].

-endif.