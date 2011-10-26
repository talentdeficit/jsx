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

-export([encoder/1]).


-spec encoder(OptsList::jsx:opts()) -> jsx:encoder().

encoder(OptsList) ->
    fun(Forms) -> start(Forms, [], [], jsx_utils:parse_opts(OptsList)) end.


-include("../include/jsx_opts.hrl").


-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


-ifndef(incomplete).
-define(incomplete(State, T, Stack, Opts),
    {incomplete, fun(Stream) when is_list(Stream) ->
            State(Stream, T, Stack, Opts)
        end
    }
).
-endif.


-ifndef(event).
-define(event(Event, State, Rest, T, Stack, Opts),
    State(Rest, Event ++ T, Stack, Opts)
).
-endif.




start([{string, String}], [], [], Opts) when is_binary(String); is_list(String) ->
    {ok,
        [{string,
            unicode:characters_to_list(jsx_utils:json_escape(String, Opts))},
            end_json
        ]
    };
start([{float, Float}], [], [], _Opts) when is_float(Float) ->
    {ok,
        [{float, Float}, end_json]
    };
start([{integer, Int}], [], [], _Opts) when is_integer(Int) ->
    {ok,
        [{integer, Int}, end_json]
    };
start([{literal, Atom}], [], [], _Opts)
        when Atom == true; Atom == false; Atom == null ->
    {ok,
        [{literal, Atom}, end_json]
    };
%% third parameter is a stack to match end_foos to start_foos
start(Forms, [], [], Opts) when is_list(Forms) ->
    list_or_object(Forms, [], [], Opts);
start(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).


list_or_object([start_object|Forms], T, Stack, Opts) ->
    ?event([start_object], key, Forms, T, [object] ++ Stack, Opts);
list_or_object([start_array|Forms], T, Stack, Opts) ->
    ?event([start_array], value, Forms, T, [array] ++ Stack, Opts);
list_or_object([], T, Stack, Opts) ->
    ?incomplete(list_or_object, T, Stack, Opts);
list_or_object(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).

 
key([{key, Key}|Forms], T, Stack, Opts) when is_binary(Key); is_list(Key) ->
    ?event([{key,
            unicode:characters_to_list(jsx_utils:json_escape(Key, Opts))
        }],
        value, Forms, T, Stack, Opts
    );
key([end_object|Forms], T, [object|Stack], Opts) ->
    ?event([end_object], maybe_done, Forms, T, Stack, Opts);
key([], T, Stack, Opts) -> ?incomplete(key, T, Stack, Opts);
key(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).


value([{string, S}|Forms], T, Stack, Opts) when is_binary(S); is_list(S) ->
    ?event([{string, unicode:characters_to_list(jsx_utils:json_escape(S, Opts))}],
        maybe_done, Forms, T, Stack, Opts
    );
value([{float, F}|Forms], T, Stack, Opts) when is_float(F) ->
    ?event([{float, F}], maybe_done, Forms, T, Stack, Opts);
value([{integer, I}|Forms], T, Stack, Opts) when is_integer(I) ->
    ?event([{integer, I}], maybe_done, Forms, T, Stack, Opts);
value([{literal, L}|Forms], T, Stack, Opts)
        when L == true; L == false; L == null ->
    ?event([{literal, L}], maybe_done, Forms, T, Stack, Opts);
value([start_object|Forms], T, Stack, Opts) ->
    ?event([start_object], key, Forms, T, [object] ++ Stack, Opts);
value([start_array|Forms], T, Stack, Opts) ->
    ?event([start_array], maybe_done, Forms, T, [array] ++ Stack, Opts);
value([end_array|Forms], T, [array|Stack], Opts) ->
    ?event([end_array], maybe_done, Forms, T, Stack, Opts);
value([], T, Stack, Opts) -> ?incomplete(value, T, Stack, Opts);
value(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).


maybe_done([end_json], T, [], Opts) ->
    ?event([end_json], done, [], T, [], Opts);
maybe_done([end_object|Forms], T, [object|Stack], Opts) ->
    ?event([end_object], maybe_done, Forms, T, Stack, Opts);
maybe_done([end_array|Forms], T, [array|Stack], Opts) ->
    ?event([end_array], maybe_done, Forms, T, Stack, Opts);
maybe_done(Forms, T, [object|_] = Stack, Opts) -> key(Forms, T, Stack, Opts);
maybe_done(Forms, T, [array|_] = Stack, Opts) -> value(Forms, T, Stack, Opts);
maybe_done([], T, Stack, Opts) -> ?incomplete(maybe_done, T, Stack, Opts);
maybe_done(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).


done([], T, [], _Opts) ->
    {ok, lists:reverse(T)};
done(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode(Terms) ->    
    try case (jsx:encoder([]))(Terms) of
            {ok, Terms} -> true
        end
    catch
        error:badarg -> false
    end.


encode_test_() ->    
    [
        {"empty object", ?_assert(encode([start_object, end_object, end_json]))},
        {"empty array", ?_assert(encode([start_array, end_array, end_json]))},
        {"nested empty objects", ?_assert(encode([start_object,
            {key, "empty object"},
            start_object,
            {key, "empty object"},
            start_object,
            end_object,
            end_object,
            end_object,
            end_json
        ]))},
        {"nested empty arrays", ?_assert(encode([start_array,
            start_array,
            start_array,
            end_array,
            end_array,
            end_array,
            end_json
        ]))},
        {"simple object", ?_assert(encode([start_object, 
            {key, "a"},
            {string, "hello"},
            {key, "b"},
            {integer, 1},
            {key, "c"},
            {float, 1.0},
            {key, "d"},
            {literal, true},
            end_object,
            end_json
        ]))},
        {"simple array", ?_assert(encode([start_array,
            {string, "hello"},
            {integer, 1},
            {float, 1.0},
            {literal, true},
            end_array,
            end_json
        ]))},
        {"unbalanced array", ?_assertNot(encode([start_array,
            end_array,
            blerg,
            end_array,
            end_json
        ]))},
        {"naked string", ?_assert((jsx:scanner())([{string, "hello"}])
            =:= {ok, [{string, "hello"}, end_json]}
        )},
        {"naked literal", ?_assert((jsx:scanner())([{literal, true}])
            =:= {ok, [{literal, true}, end_json]}
        )},
        {"naked integer", ?_assert((jsx:scanner())([{integer, 1}])
            =:= {ok, [{integer, 1}, end_json]}
        )},
        {"naked string", ?_assert((jsx:scanner())([{float, 1.0}])
            =:= {ok, [{float, 1.0}, end_json]}
        )}
    ].

-endif.