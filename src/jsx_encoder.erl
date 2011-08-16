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


-include("jsx_common.hrl").


-export([start/3,
    list_or_object/4,
    key/4,
    value/4,
    maybe_done/4,
    bad_json/2
]).

-export([encoder/1]).


-spec encoder(Opts::#opts{}) -> jsx_encoder().

encoder(Opts) ->
    case Opts#opts.iterate of
        true ->
            fun(Forms) -> start(Forms, iterate, Opts) end
        ; false ->
            fun(Forms) -> start(Forms, [], Opts) end
    end.



%% emit takes a list of `events` to present to client code and formats them
%%    appropriately
emit([], {State, Rest, T, Args}) ->
    erlang:apply(?MODULE, State, [Rest, T] ++ Args);
emit([incomplete], {State, Rest, T, Args}) ->
    {jsx, incomplete, fun(Stream)
        when is_binary(Stream) ->
            erlang:apply(?MODULE,
                State,
                [Rest ++ Stream, T] ++ Args
            )
        ; (Else) -> {error, {badjson, Else}}
    end};
emit([Event|Events], {_State, _Rest, iterate, _Args} = Next) ->
    {jsx, Event, fun() -> emit(Events, Next) end};
emit([end_json|Events], {_State, _Rest, T, _Args} = Next) ->
    {jsx, lists:reverse([end_json] ++ T), fun() -> emit(Events, Next) end};
emit([Event|Events], {State, Rest, T, Args}) ->
    emit(Events, {State, Rest, [Event] ++ T, Args}).


bad_json(Stream, _) -> {error, {badjson, Stream}}.

    
start({string, String}, T, Opts) when is_binary(String); is_list(String) ->
    emit([{string, unicode:characters_to_list(json_escape(String, Opts))},
            end_json,
            incomplete
        ],
        {bad_json, [], T, []}
    );
start({float, Float}, T, _Opts) when is_float(Float) ->
    emit([{float, Float}, end_json, incomplete], {bad_json, [], T, []});
start({integer, Int}, T, _Opts) when is_integer(Int) ->
    emit([{integer, Int}, end_json, incomplete], {bad_json, [], T, []});
start({literal, Atom}, T, _Opts) when Atom == true; Atom == false; Atom == null ->
    emit([{literal, Atom}, end_json, incomplete], {bad_json, [], T, []});
%% third parameter is a stack to match end_foos to start_foos
start(Forms, T, Opts) -> list_or_object(Forms, T, [], Opts).


list_or_object([start_object|Forms], T, Stack, Opts) ->
    emit([start_object], {key, Forms, T, [[object] ++ Stack, Opts]});
list_or_object([start_array|Forms], T, Stack, Opts) ->
    emit([start_array], {value, Forms, T, [[array] ++ Stack, Opts]});
list_or_object([], T, Stack, Opts) ->
    emit([incomplete], {list_or_object, [], T, [Stack, Opts]});
list_or_object(Forms, _, _, _) -> {error, {badjson, Forms}}.

 
key([{key, Key}|Forms], T, Stack, Opts) when is_binary(Key); is_list(Key) ->
    emit([{key, unicode:characters_to_list(json_escape(Key, Opts))}],
        {value, Forms, T, [Stack, Opts]}
    );
key([end_object|Forms], T, [object|Stack], Opts) ->
    emit([end_object], {maybe_done, Forms, T, [Stack, Opts]});
key([], T, Stack, Opts) ->
    emit([incomplete], {key, [], T, [Stack, Opts]});
key(Forms, _, _, _) -> {error, {badjson, Forms}}.


value([{string, S}|Forms], T, Stack, Opts) when is_binary(S); is_list(S) ->
    emit([{string, unicode:characters_to_list(json_escape(S, Opts))}],
        {maybe_done, Forms, T, [Stack, Opts]}
    );
value([{float, F}|Forms], T, Stack, Opts) when is_float(F) ->
    emit([{float, F}], {maybe_done, Forms, T, [Stack, Opts]});
value([{integer, I}|Forms], T, Stack, Opts) when is_integer(I) ->
    emit([{integer, I}], {maybe_done, Forms, T, [Stack, Opts]});
value([{literal, L}|Forms], T, Stack, Opts)
        when L == true; L == false; L == null ->
    emit([{literal, L}], {maybe_done, Forms, T, [Stack, Opts]});
value([start_object|Forms], T, Stack, Opts) ->
    emit([start_object], {key, Forms, T, [[object] ++ Stack, Opts]});
value([start_array|Forms], T, Stack, Opts) ->
    emit([start_array], {value, Forms, T, [[array] ++ Stack, Opts]});
value([end_array|Forms], T, [array|Stack], Opts) ->
    emit([end_array], {maybe_done, Forms, T, [Stack, Opts]});
value([], T, Stack, Opts) ->
    emit([incomplete], {value, [], T, [Stack, Opts]});
value(Forms, _, _, _) -> {error, {badjson, Forms}}.


maybe_done([], T, [], _Opts) ->
    emit([end_json, incomplete], {bad_json, [], T, []});
maybe_done([end_json], T, [], _Opts) ->
    emit([end_json, incomplete], {bad_json, [], T, []});
maybe_done([end_object|Forms], T, [object|Stack], Opts) ->
    emit([end_object], {maybe_done, Forms, T, [Stack, Opts]});
maybe_done([end_array|Forms], T, [array|Stack], Opts) ->
    emit([end_array], {maybe_done, Forms, T, [Stack, Opts]});
maybe_done(Forms, T, [object|_] = Stack, Opts) -> key(Forms, T, Stack, Opts);
maybe_done(Forms, T, [array|_] = Stack, Opts) -> value(Forms, T, Stack, Opts);
maybe_done([], T, Stack, Opts) ->
    emit([incomplete], {maybe_done, [], T, [Stack, Opts]});
maybe_done(Forms, _, _, _) -> {error, {badjson, Forms}}.



%% json string escaping, for utf8 binaries. escape the json control sequences to 
%%  their json equivalent, escape other control characters to \uXXXX sequences, 
%%  everything else should be a legal json string component
json_escape(String, Opts) when is_binary(String) ->
    json_escape(String, Opts, <<>>);
json_escape(String, Opts) when is_list(String) ->
    json_escape(String, Opts, []).

%% double quote    
json_escape(<<$\", Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $\">>);
json_escape([$\"|Rest], Opts, Acc) ->
    json_escape(Rest, Opts, [$\", $\\] ++ Acc);
%% backslash \ reverse solidus
json_escape(<<$\\, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $\\>>);
json_escape([$\\|Rest], Opts, Acc) ->
    json_escape(Rest, Opts, [$\\, $\\] ++ Acc);
%% backspace
json_escape(<<$\b, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $b>>);
json_escape([$\b|Rest], Opts, Acc) ->
    json_escape(Rest, Opts, [$b, $\\] ++ Acc);
%% form feed
json_escape(<<$\f, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $f>>);
json_escape([$\f|Rest], Opts, Acc) ->
    json_escape(Rest, Opts, [$f, $\\] ++ Acc);
%% newline
json_escape(<<$\n, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $n>>);
json_escape([$\n|Rest], Opts, Acc) ->
    json_escape(Rest, Opts, [$n, $\\] ++ Acc);
%% cr
json_escape(<<$\r, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $r>>);
json_escape([$\r|Rest], Opts, Acc) ->
    json_escape(Rest, Opts, [$r, $\\] ++ Acc);
%% tab
json_escape(<<$\t, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $t>>);
json_escape([$\t|Rest], Opts, Acc) ->
    json_escape(Rest, Opts, [$t, $\\] ++ Acc);
%% other control characters
json_escape(<<C/utf8, Rest/binary>>, Opts, Acc) when C >= 0, C < $\s -> 
    json_escape(Rest,
        Opts,
        <<Acc/binary,
            (unicode:characters_to_binary(json_escape_sequence(C)))/binary
        >>
    );
json_escape([C|Rest], Opts, Acc) when C >= 0, C < $\s ->
    json_escape(Rest, Opts, lists:reverse(json_escape_sequence(C)) ++ Acc);
%% escape forward slashes -- optionally -- to faciliate microsoft's retarded
%%   date format
json_escape(<<$/, Rest/binary>>, Opts=#opts{escape_forward_slash=true}, Acc) ->
    json_escape(Rest, Opts, <<Acc/binary, $\\, $/>>);
json_escape([$/|Rest], Opts=#opts{escape_forward_slash=true}, Acc) ->
    json_escape(Rest, Opts, [$/, $\\] ++ Acc);
%% escape u+2028 and u+2029 to avoid problems with jsonp
json_escape(<<C/utf8, Rest/binary>>, Opts, Acc)
        when C == 16#2028; C == 16#2029 ->
    json_escape(Rest,
        Opts,
        <<Acc/binary,
            (unicode:characters_to_binary(json_escape_sequence(C)))/binary
        >>
    );
json_escape([C|Rest], Opts, Acc) when C =:= 16#2028; C =:= 16#2029 ->
    json_escape(Rest, Opts, lists:reverse(json_escape_sequence(C)) ++ Acc);
%% any other legal codepoint
json_escape(<<C/utf8, Rest/binary>>, Opts, Acc) ->
    json_escape(Rest, Opts, <<Acc/binary, C/utf8>>);
json_escape([C|Rest], Opts, Acc) ->
    json_escape(Rest, Opts, [C] ++ Acc);
json_escape(<<>>, _Opts, Acc) ->
    Acc;
json_escape([], _Opts, Acc) ->
    lists:reverse(Acc);
json_escape(_, _, _) ->
    erlang:error(badarg).


%% convert a codepoint to it's \uXXXX equiv.
json_escape_sequence(X) ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    [$\\, $u, (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))].


to_hex(15) -> $f;
to_hex(14) -> $e;
to_hex(13) -> $d;
to_hex(12) -> $c;
to_hex(11) -> $b;
to_hex(10) -> $a;
to_hex(X) -> X + $0.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


encode(Terms) ->
    encode_simple(Terms) andalso encode_iterative(Terms).


encode_simple(Terms) ->    
    case (jsx:encoder([]))(Terms) of
        {jsx, Terms, _} ->
            true
        %% matches [foo, end_json], aka naked terms
        ; {jsx, [Terms, end_json], _} ->
            true
        ; {error, _} ->
            false
    end.


encode_iterative(Terms) ->
    case loop((jsx:encoder([iterate]))(Terms), []) of
        {ok, Terms} ->
            true
        %% matches naked terms
        ; {ok, [Terms, end_json]} ->
            true
        ; {error, _} ->
            false
    end.

loop({jsx, end_json, Next}, Acc) ->
    {jsx, incomplete, F} = Next(),
    {error, _} = F([]),
    {ok, lists:reverse([end_json] ++ Acc)};
loop({jsx, Event, Next}, Acc) ->
    loop(Next(), [Event] ++ Acc).


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
            end_array,
            end_json
        ]))},
        {"naked string", ?_assert(encode({string, "hello"}))},
        {"naked literal", ?_assert(encode({literal, true}))},
        {"naked integer", ?_assert(encode({integer, 1}))},
        {"naked float", ?_assert(encode({float, 1.0}))}
    ].


binary_escape_test_() ->
    [
        {"json string escaping", 
            ?_assert(json_escape(
                    <<"\"\\\b\f\n\r\t">>, #opts{}
                ) =:= <<"\\\"\\\\\\b\\f\\n\\r\\t">>
            )
        },
        {"json string hex escape", 
            ?_assert(json_escape(
                    <<1, 2, 3, 11, 26, 30, 31>>, #opts{}
                ) =:= <<"\\u0001\\u0002\\u0003\\u000b\\u001a\\u001e\\u001f">>
            )
        },
        {"jsonp protection",
            ?_assert(json_escape(
                    <<226, 128, 168, 226, 128, 169>>, #opts{}
                ) =:= <<"\\u2028\\u2029">>
            )
        },
        {"microsoft i hate your date format",
            ?_assert(json_escape(<<"/Date(1303502009425)/">>,
                    #opts{escape_forward_slash=true}
                ) =:= <<"\\/Date(1303502009425)\\/">>
            )
        }
    ].


string_escape_test_() ->
    [
        {"json string escaping", 
            ?_assert(json_escape(
                    "\"\\\b\f\n\r\t", #opts{}
                ) =:= "\\\"\\\\\\b\\f\\n\\r\\t"
            )
        },
        {"json string hex escape", 
            ?_assert(json_escape(
                    [1, 2, 3, 11, 26, 30, 31], #opts{}
                ) =:= "\\u0001\\u0002\\u0003\\u000b\\u001a\\u001e\\u001f"
            )
        },
        {"jsonp protection",
            ?_assert(json_escape(
                    [16#2028, 16#2029], #opts{}
                ) =:= "\\u2028\\u2029"
            )
        },
        {"microsoft i hate your date format",
            ?_assert(json_escape("/Date(1303502009425)/",
                    #opts{escape_forward_slash=true}
                ) =:= "\\/Date(1303502009425)\\/"
            )
        }
    ].

-endif.