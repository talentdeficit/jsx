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


-export([encoder/0]).


-include("jsx_common.hrl").



-spec encoder() -> jsx_encoder().

encoder() -> fun(Forms) -> start(Forms) end.
    

-define(ENDJSON,
    {event, end_json, fun() -> 
        {incomplete, fun(Forms) -> {error, {badjson, Forms}} end} 
    end}
).

    
start({string, String}) when is_list(String) ->
    {event, {string, String}, fun() -> ?ENDJSON end};
start({float, Float}) when is_list(Float) ->
    {event, {float, Float}, fun() -> ?ENDJSON end};
start({integer, Int}) when is_list(Int) ->
    {event, {integer, Int}, fun() -> ?ENDJSON end};
start({literal, Atom}) when Atom == true; Atom == false; Atom == null ->
    {event, {literal, Atom}, fun() -> ?ENDJSON end};
%% second parameter is a stack to match end_foos to start_foos
start(Forms) -> list_or_object(Forms, []).


list_or_object([start_object|Forms], Stack) ->
    {event, start_object, fun() -> key(Forms, [object] ++ Stack) end};
list_or_object([start_array|Forms], Stack) ->
    {event, start_array, fun() -> value(Forms, [array] ++ Stack) end};
list_or_object([], Stack) ->
    {incomplete, fun(end_stream) -> 
            {error, {badjson, []}}
        ; (Stream) -> 
            list_or_object(Stream, Stack) 
    end};
list_or_object(Forms, _) -> {error, {badjson, Forms}}.

 
key([{key, Key}|Forms], Stack) when is_list(Key) ->
    {event, {key, Key}, fun() -> value(Forms, Stack) end};
key([end_object|Forms], [object|Stack]) ->
    {event, end_object, fun() -> maybe_done(Forms, Stack) end};
key([], Stack) ->
    {incomplete, fun(end_stream) -> 
            {error, {badjson, []}}
        ; (Stream) -> 
            key(Stream, Stack) 
    end};
key(Forms, _) -> {error, {badjson, Forms}}.


value([{string, S}|Forms], Stack) when is_list(S) ->
    {event, {string, S}, fun() -> maybe_done(Forms, Stack) end};
value([{float, F}|Forms],  Stack) when is_list(F) ->
    {event, {float, F}, fun() -> maybe_done(Forms, Stack) end};
value([{integer, I}|Forms], Stack) when is_list(I) ->
    {event, {integer, I}, fun() -> maybe_done(Forms, Stack) end};
value([{literal, L}|Forms], Stack) when L == true; L == false; L == null ->
    {event, {literal, L}, fun() -> maybe_done(Forms, Stack) end};
value([start_object|Forms], Stack) ->
    {event, start_object, fun() -> key(Forms, [object] ++ Stack) end};
value([start_array|Forms], Stack) ->
    {event, start_array, fun() -> value(Forms, [array] ++ Stack) end};
value([end_array|Forms], [array|Stack]) ->
    {event, end_array, fun() -> maybe_done(Forms, Stack) end};
value([], Stack) ->
    {incomplete, fun(end_stream) -> 
            {error, {badjson, []}}
        ; (Stream) -> 
            value(Stream, Stack) 
    end};
value(Forms, _) -> {error, {badjson, Forms}}.


maybe_done([], []) -> ?ENDJSON;
maybe_done([end_object|Forms], [object|Stack]) ->
    {event, end_object, fun() -> maybe_done(Forms, Stack) end};
maybe_done([end_array|Forms], [array|Stack]) ->
    {event, end_array, fun() -> maybe_done(Forms, Stack) end};
maybe_done(Forms, [object|_] = Stack) -> key(Forms, Stack);
maybe_done(Forms, [array|_] = Stack) -> value(Forms, Stack);
maybe_done([], Stack) ->
    {incomplete, fun(end_stream) -> 
            {error, {badjson, []}}
        ; (Stream) -> 
            maybe_done(Stream, Stack) 
    end};
maybe_done(Forms, _) -> {error, {badjson, Forms}}.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


encode(Term) ->
    case loop((encoder())(Term), []) of
        %% unwrap naked values
        {ok, [Term]} -> true
        ; {ok, Term} -> true
        ; {error, badjson} -> false
    end.


loop({error, _}, _Acc) ->
    {error, badjson};
loop({event, end_json, Next}, Acc) ->
    {incomplete, F} = Next(),
    {error, {badjson, []}} = F([]),
    {ok, lists:reverse(Acc)};
loop({event, Event, Next}, Acc) -> loop(Next(), [Event] ++ Acc).


encode_test_() ->    
    [
        {"empty object", ?_assert(encode([start_object, end_object]) =:= true)},
        {"empty array", ?_assert(encode([start_array, end_array]) =:= true)},
        {"nested empty objects", ?_assert(encode([start_object,
            {key, "empty object"},
            start_object,
            {key, "empty object"},
            start_object,
            end_object,
            end_object,
            end_object
        ]) =:= true)},
        {"nested empty arrays", ?_assert(encode([start_array,
            start_array,
            start_array,
            end_array,
            end_array,
            end_array
        ]) =:= true)},
        {"simple object", ?_assert(encode([start_object, 
            {key, "a"},
            {string, "hello"},
            {key, "b"},
            {integer, "1"},
            {key, "c"},
            {float, "1.0"},
            {key, "d"},
            {literal, true},
            end_object
        ]) =:= true)},
        {"simple array", ?_assert(encode([start_array,
            {string, "hello"},
            {integer, "1"},
            {float, "1.0"},
            {literal, true},
            end_array
        ]) =:= true)},
        {"unbalanced array", ?_assert(encode([start_array,
            end_array,
            end_array
        ]) =:= false)},
        {"naked string", ?_assert(encode({string, "hello"}) =:= true)},
        {"naked literal", ?_assert(encode({literal, true}) =:= true)},
        {"naked integer", ?_assert(encode({integer, "1"}) =:= true)},
        {"naked float", ?_assert(encode({float, "1.0"}) =:= true)}
    ].

-endif.
    
