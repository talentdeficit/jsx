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

-spec encoder(Handler::module(), State::any(), Config::jsx:config()) -> jsx:encoder().

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
    try Handler:handle_event(end_json, value(Term, {Handler, State}, Config))
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


list_or_object([{K, V}|Rest], {Handler, State}, Config) when is_atom(K); is_binary(K) ->
    object([{K, V}|Rest], {Handler, Handler:handle_event(start_object, State)}, Config);
list_or_object(Terms, {Handler, State}, Config) when is_list(Terms) ->
    list(Terms, {Handler, Handler:handle_event(start_array, State)}, Config).


object([{Key, Value}, Next|Rest], {Handler, State}, Config) when is_atom(Key); is_binary(Key) ->
    object([Next|Rest], {Handler,
            value(
                Value,
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
                Value,
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), {Handler, State}, Config)}, State)},
                Config
            )
        },
        Config
    );
object([], {Handler, State}, _Config) -> Handler:handle_event(end_object, State).


list([Value, Next|Rest], {Handler, State}, Config) ->
    list([Next|Rest], {Handler, value(Value, {Handler, State}, Config)}, Config);
list([Value], {Handler, State}, Config) ->
    list([], {Handler, value(Value, {Handler, State}, Config)}, Config);
list([], {Handler, State}, _Config) -> Handler:handle_event(end_array, State).


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