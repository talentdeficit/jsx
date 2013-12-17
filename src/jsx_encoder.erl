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

-export([encoder/3, encode/1, encode/2, unzip/1]).

-spec encoder(Handler::module(), State::any(), Config::list()) -> jsx:encoder().

encoder(Handler, State, Config) ->
    Parser = jsx:parser(Handler, State, Config),
    fun(Term) -> Parser(encode(Term) ++ [end_json]) end.


-spec encode(Term::any()) -> any().

encode(Term) -> encode(Term, ?MODULE).


-spec encode(Term::any(), EntryPoint::module()) -> any().

encode([], _EntryPoint) -> [start_array, end_array];
encode([{}], _EntryPoint) -> [start_object, end_object];

encode([{_, _}|_] = Term, EntryPoint) ->
    lists:flatten(
        [start_object] ++ [ EntryPoint:encode(T, EntryPoint) || T <- unzip(Term) ] ++ [end_object]
    );
encode(Term, EntryPoint) when is_list(Term) ->
    lists:flatten(
        [start_array] ++ [ EntryPoint:encode(T, EntryPoint) || T <- Term ] ++ [end_array]
    );

encode(Else, _EntryPoint) -> [Else].


unzip(List) -> unzip(List, []).

unzip([], Acc) -> lists:reverse(Acc);
unzip([{K, V}|Rest], Acc) when is_binary(K); is_atom(K); is_integer(K) -> unzip(Rest, [V, K] ++ Acc).


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
            {string, [{string, <<239, 191, 191>>}]},
            parser(<<239, 191, 191>>, [{error_handler, Error}, strict])
        )}
    ].

-endif.
