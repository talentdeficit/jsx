%% The MIT License

%% Copyright (c) 2010 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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


-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


-ifndef(incomplete).
-define(incomplete(State, T, Stack, Opts),
    {ok, lists:reverse(T), fun(Stream) when is_list(Stream) ->
            State(Stream, [], Stack, Opts)
        end
    }
).
-endif.


-ifndef(event).
-define(event(Event, State, Rest, T, Stack, Opts),
    State(Rest, Event ++ T, Stack, Opts)
).
-endif.




start({string, String}, [], [], Opts) when is_binary(String); is_list(String) ->
    {ok,
        [{string, unicode:characters_to_list(json_escape(String, Opts))}, end_json],
        fun(X) when is_list(X) -> ?error([X, [], [], Opts]) end
    };
start({float, Float}, [], [], Opts) when is_float(Float) ->
    {ok,
        [{float, Float}, end_json],
        fun(X) when is_list(X) -> ?error([X, [], [], Opts]) end
    };
start({integer, Int}, [], [], Opts) when is_integer(Int) ->
    {ok,
        [{integer, Int}, end_json],
        fun(X) when is_list(X) -> ?error([X, [], [], Opts]) end
    };
start({literal, Atom}, [], [], Opts) when Atom == true; Atom == false; Atom == null ->
    {ok,
        [{literal, Atom}, end_json],
        fun(X) when is_list(X) -> ?error([X, [], [], Opts]) end
    };
%% third parameter is a stack to match end_foos to start_foos
start(Forms, [], [], Opts) -> list_or_object(Forms, [], [], Opts).


list_or_object([start_object|Forms], T, Stack, Opts) ->
    ?event([start_object], key, Forms, T, [object] ++ Stack, Opts);
list_or_object([start_array|Forms], T, Stack, Opts) ->
    ?event([start_array], value, Forms, T, [array] ++ Stack, Opts);
list_or_object([], T, Stack, Opts) -> ?incomplete(list_or_object, T, Stack, Opts);
list_or_object(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).

 
key([{key, Key}|Forms], T, Stack, Opts) when is_binary(Key); is_list(Key) ->
    ?event([{key, unicode:characters_to_list(json_escape(Key, Opts))}],
        value, Forms, T, Stack, Opts
    );
key([end_object|Forms], T, [object|Stack], Opts) ->
    ?event([end_object], maybe_done, Forms, T, Stack, Opts);
key([], T, Stack, Opts) -> ?incomplete(key, T, Stack, Opts);
key(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).


value([{string, S}|Forms], T, Stack, Opts) when is_binary(S); is_list(S) ->
    ?event([{string, unicode:characters_to_list(json_escape(S, Opts))}],
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


done([], T, [], Opts) ->
    {ok, lists:reverse(T), fun(X) when is_list(X) ->
            done(X, T, [], Opts)
        end
    };
done(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).


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
json_escape(Rest, Opts, Acc) ->
    erlang:error(badarg, [Rest, Opts, Acc]).


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