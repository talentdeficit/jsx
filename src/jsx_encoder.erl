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
-include("jsx_config.hrl").
-export([encoder/3, encode/2, encode/3]).

-spec encoder(Handler::module(), State::any(), Config::jsx_config:options()) -> jsx:encoder().

encoder(Handler, State, Config) ->
    Parser = jsx:parser(Handler, State, Config),
    fun(Term) -> Parser(encode(Term, jsx_config:parse_config(Config)) ++ [end_json]) end.


-spec encode(Term::any(), Config::jsx_config:options()) -> [any(), ...].

encode(Term, Config) -> encode(Term, ?MODULE, Config).


-spec encode(Term::any(), EntryPoint::module(), Config::jsx_config:options()) -> [any(), ...].

encode(Map, _EntryPoint, _Config) when is_map(Map), map_size(Map) < 1 ->
    [start_object, end_object];
encode(Term, EntryPoint, Config) when is_map(Term) ->
    [start_object] ++ unpack(Term, EntryPoint, Config);
encode(Term, EntryPoint, Config) -> encode_(Term, EntryPoint, Config).

encode_([], _EntryPoint, _Config) -> [start_array, end_array];
encode_([{}], _EntryPoint,#config{tuples_to_lists = false}) -> [start_object, end_object];

%% datetime special case
encode_([{{_,_,_},{_,_,_}} = DateTime|Rest], EntryPoint, #config{tuples_to_lists = false} = Config) ->
    [start_array] ++ [DateTime] ++ unhitch(Rest, EntryPoint, Config);
encode_({{A,B,C},{D,E,F}} = DateTime, _EntryPoint, #config{tuples_to_lists = true, disable_timestamp_heuristics = false} = _Config)
    when is_integer(A), is_integer(B), is_integer(C), is_integer(D), is_integer(E), is_integer(F)  ->
   [DateTime];
encode_({A,B,C} = Timestamp, _EntryPoint, #config{tuples_to_lists = true, disable_timestamp_heuristics = false} = _Config)
    when is_integer(A), is_integer(B), is_integer(C)  ->
   [Timestamp];

encode_([{_, _}|_] = Term, EntryPoint, #config{tuples_to_lists = false} = Config) ->
    [start_object] ++ unzip(Term, EntryPoint, Config);
encode_(Term, EntryPoint, Config) when is_list(Term) ->
    [start_array] ++ unhitch(Term, EntryPoint, Config);
encode_(T, EntryPoint, #config{tuples_to_lists = true} = Config) when is_tuple(T)->
    encode_(tuple_to_list(T), EntryPoint, Config);
encode_(Else, _EntryPoint, _Config) -> [Else].


unzip([{K, V}|Rest], EntryPoint, Config) when is_integer(K); is_binary(K); is_atom(K) ->
    [K] ++ EntryPoint:encode(V, EntryPoint, Config) ++ unzip(Rest, EntryPoint, Config);
unzip([], _, _) -> [end_object];
unzip(_, _, _) -> erlang:error(badarg).


unhitch([V|Rest], EntryPoint, Config) ->
    EntryPoint:encode(V, EntryPoint, Config) ++ unhitch(Rest, EntryPoint, Config);
unhitch([], _, _) -> [end_array].

unpack(Map, EntryPoint, Config) -> unpack(Map, maps:keys(Map), EntryPoint, Config).

unpack(Map, [K|Rest], EntryPoint, Config) when is_integer(K); is_binary(K); is_atom(K) ->
    [K] ++ EntryPoint:encode(maps:get(K, Map), EntryPoint, Config) ++ unpack(Map, Rest, EntryPoint, Config);
unpack(_, [], _, _) -> [end_object].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


parser(Term, Opts) -> (jsx:parser(jsx, [], Opts))(Term).


error_test_() ->
    [
        {"value error", ?_assertError(badarg, parser(self(), []))},
        {"string error", ?_assertError(badarg, parser(<<239, 191, 191>>, [strict]))}
    ].

custom_error_handler_test_() ->
    Error = fun(Term, {_, State, _, _}, _) -> {State, Term} end,
    [
        {"value error", ?_assertEqual(
            {value, [self()]},
            parser(self(), [{error_handler, Error}])
        )},
        {"string error", ?_assertEqual(
            {value, [{string, <<237, 160, 128>>}]},
            parser(<<237, 160, 128>>, [{error_handler, Error}, strict])
        )}
    ].

improper_lists_test_() ->
    [
        {"improper proplist", ?_assertError(
          badarg,
          encode([{<<"key">>, <<"value">>}, false], #config{})
        )},
        {"improper list", ?_assertError(
          badarg,
          encode([{literal, true}, false, null], #config{})
        )}
    ].

-endif.
