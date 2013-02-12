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

-export([encoder/3, pre_encode/2]).

-spec encoder(Handler::module(), State::any(), Config::jsx:config()) -> jsx:encoder().

encoder(Handler, State, Config) ->
    fun(JSON) ->
        start(
            JSON,
            {Handler, Handler:init(State)},
            jsx_utils:parse_config(Config)
        )
    end.



-include("jsx_config.hrl").


-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


start(Term, {Handler, State}, Config) ->
    Handler:handle_event(end_json, value(pre_encode(Term, Config), {Handler, State}, Config)).


value(String, {Handler, State}, Config) when is_binary(String) ->
    Handler:handle_event({string, clean_string(String, Config)}, State);
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
value([Tuple|_] = List, Handler, Config) when is_tuple(Tuple) ->
    list_or_object(List, Handler, Config);
value(List, Handler, Config) when is_list(List) ->
    list_or_object(List, Handler, Config);
value(Term, Handler, Config) -> ?error([Term, Handler, Config]).


list_or_object([Term|Rest], {Handler, State}, Config) ->
    case pre_encode(Term, Config) of
        {K, V} ->
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
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), Config)}, State)},
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
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), Config)}, State)},
                Config
            )
        },
        Config
    );
object([], {Handler, State}, _Config) -> Handler:handle_event(end_object, State);
object(Term, Handler, Config) -> ?error([Term, Handler, Config]).


list([Value, Next|Rest], {Handler, State}, Config) ->
    list([pre_encode(Next, Config)|Rest], {Handler, value(Value, {Handler, State}, Config)}, Config);
list([Value], {Handler, State}, Config) ->
	list([], {Handler, value(Value, {Handler, State}, Config)}, Config);
list([], {Handler, State}, _Config) -> Handler:handle_event(end_array, State);
list(Term, Handler, Config) -> ?error([Term, Handler, Config]).


pre_encode(Value, #config{pre_encode=false}) -> io:format("~p~n", [Value]), Value;
pre_encode(Value, Config) -> (Config#config.pre_encode)(Value).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Config) -> jsx_utils:clean_string(Bin, Config).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


encode_test_() ->
    Data = jsx:universals(),
    [
        {
            Title, ?_assertEqual(
                Events ++ [end_json],
                start(Term, {jsx, []}, #config{})
            )
        } || {Title, _, Term, Events} <- Data
    ].


-endif.