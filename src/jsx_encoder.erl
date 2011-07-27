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


-include("jsx_common.hrl").


-record(opts, {
    multi_term = false,
    encoding = auto
}).


-spec encoder(Opts::jsx_opts()) -> jsx_encoder().

encoder(Opts) -> fun(Forms) -> start(Forms, Opts) end.
    

-define(ENDJSON,
    {jsx, end_json, fun() -> 
        {jsx, incomplete, fun(Forms) -> {error, {badjson, Forms}} end} 
    end}
).

    
start({string, String}, _Opts) when is_binary(String) ->
    {jsx, {string, json_escape(String)}, fun() -> ?ENDJSON end};
start({float, Float}, _Opts) when is_float(Float) ->
    {jsx, {float, Float}, fun() -> ?ENDJSON end};
start({integer, Int}, _Opts) when is_integer(Int) ->
    {jsx, {integer, Int}, fun() -> ?ENDJSON end};
start({literal, Atom}, _Opts) when Atom == true; Atom == false; Atom == null ->
    {jsx, {literal, Atom}, fun() -> ?ENDJSON end};
%% second parameter is a stack to match end_foos to start_foos
start(Forms, Opts) -> list_or_object(Forms, [], Opts).


list_or_object([start_object|Forms], Stack, Opts) ->
    {jsx, start_object, fun() -> key(Forms, [object] ++ Stack, Opts) end};
list_or_object([start_array|Forms], Stack, Opts) ->
    {jsx, start_array, fun() -> value(Forms, [array] ++ Stack, Opts) end};
list_or_object([], Stack, Opts) ->
    {jsx, incomplete, fun(end_stream) -> 
            {error, {badjson, []}}
        ; (Stream) -> 
            list_or_object(Stream, Stack, Opts) 
    end};
list_or_object(Forms, _, _) -> {error, {badjson, Forms}}.

 
key([{key, Key}|Forms], Stack, Opts) when is_binary(Key) ->
    {jsx, {key, json_escape(Key)}, fun() -> value(Forms, Stack, Opts) end};
key([end_object|Forms], [object|Stack], Opts) ->
    {jsx, end_object, fun() -> maybe_done(Forms, Stack, Opts) end};
key([], Stack, Opts) ->
    {jsx, incomplete, fun(end_stream) -> 
            {error, {badjson, []}}
        ; (Stream) -> 
            key(Stream, Stack, Opts) 
    end};
key(Forms, _, _) -> {error, {badjson, Forms}}.


value([{string, S}|Forms], Stack, Opts) when is_binary(S) ->
    {jsx, {string, json_escape(S)}, fun() -> maybe_done(Forms, Stack, Opts) end};
value([{float, F}|Forms],  Stack, Opts) when is_float(F) ->
    {jsx, {float, F}, fun() -> maybe_done(Forms, Stack, Opts) end};
value([{integer, I}|Forms], Stack, Opts) when is_integer(I) ->
    {jsx, {integer, I}, fun() -> maybe_done(Forms, Stack, Opts) end};
value([{literal, L}|Forms], Stack, Opts)
        when L == true; L == false; L == null ->
    {jsx, {literal, L}, fun() -> maybe_done(Forms, Stack, Opts) end};
value([start_object|Forms], Stack, Opts) ->
    {jsx, start_object, fun() -> key(Forms, [object] ++ Stack, Opts) end};
value([start_array|Forms], Stack, Opts) ->
    {jsx, start_array, fun() -> value(Forms, [array] ++ Stack, Opts) end};
value([end_array|Forms], [array|Stack], Opts) ->
    {jsx, end_array, fun() -> maybe_done(Forms, Stack, Opts) end};
value([], Stack, Opts) ->
    {jsx, incomplete, fun(end_stream) -> 
            {error, {badjson, []}}
        ; (Stream) -> 
            value(Stream, Stack, Opts) 
    end};
value(Forms, _, _) -> {error, {badjson, Forms}}.


maybe_done([], [], _) -> ?ENDJSON;
maybe_done([end_json], [], _) -> ?ENDJSON;
maybe_done([end_json|Forms], [], #opts{multi_term=true}=Opts) ->
    {jsx, end_json, fun() -> start(Forms, Opts) end};
maybe_done([end_object|Forms], [object|Stack], Opts) ->
    {jsx, end_object, fun() -> maybe_done(Forms, Stack, Opts) end};
maybe_done([end_array|Forms], [array|Stack], Opts) ->
    {jsx, end_array, fun() -> maybe_done(Forms, Stack, Opts) end};
maybe_done(Forms, [object|_] = Stack, Opts) -> key(Forms, Stack, Opts);
maybe_done(Forms, [array|_] = Stack, Opts) -> value(Forms, Stack, Opts);
maybe_done([], Stack, Opts) ->
    {jsx, incomplete, fun(end_stream) -> 
            {error, {badjson, []}}
        ; (Stream) -> 
            maybe_done(Stream, Stack, Opts) 
    end};
maybe_done(Forms, _, _) -> {error, {badjson, Forms}}.



%% json string escaping, for utf8 binaries. escape the json control sequences to 
%%  their json equivalent, escape other control characters to \uXXXX sequences, 
%%  everything else should be a legal json string component
json_escape(String) ->
    json_escape(String, <<>>).

%% double quote    
json_escape(<<$\", Rest/binary>>, Acc) -> 
    json_escape(Rest, <<Acc/binary, $\\, $\">>);
%% backslash \ reverse solidus
json_escape(<<$\\, Rest/binary>>, Acc) -> 
    json_escape(Rest, <<Acc/binary, $\\, $\\>>);
%% backspace
json_escape(<<$\b, Rest/binary>>, Acc) -> 
    json_escape(Rest, <<Acc/binary, $\\, $b>>);
%% form feed
json_escape(<<$\f, Rest/binary>>, Acc) -> 
    json_escape(Rest, <<Acc/binary, $\\, $f>>);
%% newline
json_escape(<<$\n, Rest/binary>>, Acc) -> 
    json_escape(Rest, <<Acc/binary, $\\, $n>>);
%% cr
json_escape(<<$\r, Rest/binary>>, Acc) -> 
    json_escape(Rest, <<Acc/binary, $\\, $r>>);
%% tab
json_escape(<<$\t, Rest/binary>>, Acc) -> 
    json_escape(Rest, <<Acc/binary, $\\, $t>>);
%% other control characters
json_escape(<<C/utf8, Rest/binary>>, Acc) when C >= 0, C < $\s -> 
    json_escape(Rest, <<Acc/binary, (json_escape_sequence(C))/binary>>);
%% escape u+2028 and u+2029 to avoid problems with jsonp
json_escape(<<C/utf8, Rest/binary>>, Acc) when C == 16#2028; C == 16#2029 ->
    json_escape(Rest, <<Acc/binary, (json_escape_sequence(C))/binary>>);
%% any other legal codepoint
json_escape(<<C/utf8, Rest/binary>>, Acc) ->
    json_escape(Rest, <<Acc/binary, C/utf8>>);
json_escape(<<>>, Acc) ->
    Acc;
json_escape(_, _) ->
    erlang:error(badarg).


%% convert a codepoint to it's \uXXXX equiv.
json_escape_sequence(X) ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    <<$\\, $u, (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))>>.


to_hex(15) -> $f;
to_hex(14) -> $e;
to_hex(13) -> $d;
to_hex(12) -> $c;
to_hex(11) -> $b;
to_hex(10) -> $a;
to_hex(X) -> X + $0.




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").



encode(Terms) -> encode_whole(Terms) andalso encode_incremental(Terms).


encode_whole(Terms) ->
    case loop((encoder([]))(Terms), []) of
        %% unwrap naked values
        {ok, [Terms]} -> true
        ; {ok, Terms} -> true
        ; _ -> false
    end.


encode_incremental(Terms) when is_list(Terms) ->
    encode_incremental(Terms, encoder([]), Terms, []);
%% we could feed naked terms to the regular encoder, but we already do that, so
%%  cheat instead
encode_incremental(_) -> true.

encode_incremental([Term], F, Expected, Acc) ->
    case loop(F([Term]), []) of
        {ok, R} -> Expected =:= Acc ++ R
        ; _ -> false
    end;
encode_incremental([Term|Terms], F, Expected, Acc) ->
    case loop(F([Term]), []) of
        {jsx, incomplete, Next, R} ->
            encode_incremental(Terms, Next, Expected, Acc ++ R)
        ; _ ->
            false
    end.


loop({error, _}, _) -> error;
loop({jsx, incomplete, Next}, Acc) -> {jsx, incomplete, Next, lists:reverse(Acc)};
loop({jsx, end_json, Next}, Acc) ->
    {jsx, incomplete, F} = Next(),
    {error, {badjson, []}} = F([]),
    {ok, lists:reverse(Acc)};
loop({jsx, Event, Next}, Acc) -> loop(Next(), [Event] ++ Acc).


encode_test_() ->    
    [
        {"empty object", ?_assert(encode([start_object, end_object]))},
        {"empty array", ?_assert(encode([start_array, end_array]) =:= true)},
        {"nested empty objects", ?_assert(encode([start_object,
            {key, <<"empty object">>},
            start_object,
            {key, <<"empty object">>},
            start_object,
            end_object,
            end_object,
            end_object
        ]))},
        {"nested empty arrays", ?_assert(encode([start_array,
            start_array,
            start_array,
            end_array,
            end_array,
            end_array
        ]))},
        {"simple object", ?_assert(encode([start_object, 
            {key, <<"a">>},
            {string, <<"hello">>},
            {key, <<"b">>},
            {integer, 1},
            {key, <<"c">>},
            {float, 1.0},
            {key, <<"d">>},
            {literal, true},
            end_object
        ]))},
        {"simple array", ?_assert(encode([start_array,
            {string, <<"hello">>},
            {integer, 1},
            {float, 1.0},
            {literal, true},
            end_array
        ]))},
        {"unbalanced array", ?_assertNot(encode([start_array,
            end_array,
            end_array
        ]))},
        {"naked string", ?_assert(encode({string, <<"hello">>}))},
        {"naked literal", ?_assert(encode({literal, true}))},
        {"naked integer", ?_assert(encode({integer, 1}))},
        {"naked float", ?_assert(encode({float, 1.0}))}
    ].


escape_test_() ->
    [
        {"json string escaping", 
            ?_assert(json_escape(
                    <<"\"\\\b\f\n\r\t">>
                ) =:= <<"\\\"\\\\\\b\\f\\n\\r\\t">>
            )
        },
        {"json string hex escape", 
            ?_assert(json_escape(
                    <<1, 2, 3, 11, 26, 30, 31>>
                ) =:= <<"\\u0001\\u0002\\u0003\\u000b\\u001a\\u001e\\u001f">>
            )
        },
        {"jsonp protection",
            ?_assert(json_escape(
                    <<226, 128, 168, 226, 128, 169>>
                ) =:= <<"\\u2028\\u2029">>
            )
        }
    ].

-endif.
    
