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
    fun(JSON) -> start(JSON, {Handler, State}, jsx_utils:parse_opts(Opts)) end.



-include("../include/jsx_opts.hrl").


-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


start(Term, {Handler, State}, Opts) ->
    Handler:handle_event(end_json, value(Term, {Handler, State}, Opts)).


value(String, {Handler, State}, _Opts) when is_binary(String) ->
    Handler:handle_event({string, String}, State);
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
    object(Rest, {Handler,
            value(Value, {Handler, Handler:handle_event({key, fix_key(Key)}, State)}, Opts)
        }, Opts);
object([], {Handler, State}, _Opts) -> Handler:handle_event(end_object, State);
object(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


list([Value|Rest], {Handler, State}, Opts) ->
    list(Rest, {Handler, value(Value, {Handler, State}, Opts)}, Opts);
list([], {Handler, State}, _Opts) -> Handler:handle_event(end_array, State);
list(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


fix_key(Key) when is_binary(Key) -> Key;
fix_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode(Term) -> (encoder(jsx, [], []))(Term).


encode_test_() ->    
    [
        {"naked string", ?_assert(encode(<<"a string">>)
            =:= [{string, <<"a string">>}, end_json])
        },
        {"naked integer", ?_assert(encode(123)
            =:= [{integer, 123}, end_json])
        },
        {"naked float", ?_assert(encode(1.23)
            =:= [{float, 1.23}, end_json])
        },
        {"naked literal", ?_assert(encode(null)
            =:= [{literal, null}, end_json])
        },
        {"empty object", ?_assert(encode([{}])
            =:= [start_object, end_object, end_json])
        },
        {"empty list", ?_assert(encode([])
            =:= [start_array, end_array, end_json])
        },
        {"simple list", ?_assert(encode([1,2,3,true,false])
            =:= [start_array,
                    {integer, 1},
                    {integer, 2},
                    {integer, 3},
                    {literal, true},
                    {literal, false},
                    end_array,
                end_json])
        },
        {"simple object", ?_assert(encode([{<<"a">>, true}, {<<"b">>, false}])
            =:= [start_object,
                    {key, <<"a">>},
                    {literal, true},
                    {key, <<"b">>},
                    {literal, false},
                    end_object,
                end_json])
        },
        {"complex term", ?_assert(encode([
                {<<"a">>, true},
                {<<"b">>, false},
                {<<"c">>, [1,2,3]},
                {<<"d">>, [{<<"key">>, <<"value">>}]}
            ]) =:= [start_object,
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
                end_json])
        },
        {"atom keys", ?_assert(encode([{key, <<"value">>}])
            =:= [start_object,
                    {key, <<"key">>},
                    {string, <<"value">>},
                    end_object,
                end_json])
        }
    ].

-endif.