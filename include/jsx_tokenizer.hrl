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
        [{string, unicode:characters_to_list(jsx_utils:json_escape(String, Opts))}, end_json],
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
    ?event([{key, unicode:characters_to_list(jsx_utils:json_escape(Key, Opts))}],
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


done([], T, [], Opts) ->
    {ok, lists:reverse(T), fun(X) when is_list(X) ->
            done(X, T, [], Opts)
        end
    };
done(Forms, T, Stack, Opts) -> ?error([Forms, T, Stack, Opts]).