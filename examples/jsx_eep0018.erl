-module(jsx_eep0018).

-export([json_to_term/1, json_to_term/2]).


json_to_term(JSON) ->
    json_to_term(JSON, []).
    
json_to_term(JSON, Opts) ->
    Encoding = proplists:get_value(encoding, Opts, utf8),
    P = jsx:parser([{encoding, Encoding}]),
    collect(P(JSON), [[]], Opts).
  
  
collect({event, Start, Next}, Acc, Opts) when Start =:= start_object; Start =:= start_array ->
    collect(Next(), [[]|Acc], Opts);

%% special case for empty object
collect({event, end_object, Next}, [[], Parent|Rest], Opts) ->
    collect(Next(), [[[{}]] ++ Parent] ++ Rest, Opts);
%% reverse the array/object accumulator before prepending it to it's parent
collect({event, end_object, Next}, [Current, Parent|Rest], Opts) when is_list(Parent) ->
    collect(Next(), [[lists:reverse(Current)] ++ Parent] ++ Rest, Opts);
collect({event, end_array, Next}, [Current, Parent|Rest], Opts) when is_list(Parent) ->
    collect(Next(), [[lists:reverse(Current)] ++ Parent] ++ Rest, Opts);
collect({event, Start, Next}, [Current, Key, Parent|Rest], Opts)
        when Start =:= end_object; Start =:= end_array ->
    collect(Next(), [[{Key, lists:reverse(Current)}] ++ Parent] ++ Rest, Opts);
    
%% end of json is emitted asap (at close of array/object), calling Next() a final 
%%   time and then Force() ensures the tail of the json binary is clean (whitespace only)     
collect({event, end_json, Next}, Acc, _Opts) ->
    collect(Next(), Acc, _Opts);
    
%% key can only be emitted inside of a json object, so just insert it directly into
%%   the head of the accumulator, deal with it when we receive it's paired value    
collect({event, {key, _} = PreKey, Next}, [Current|_] = Acc, Opts) ->
    Key = event(PreKey, Opts),
    case key_repeats(Key, Current) of
        true -> erlang:throw(badarg)
        ; false -> collect(Next(), [Key] ++ Acc, Opts)
    end;

%% check acc to see if we're inside an object or an array. because inside an object
%%   context the events that fall this far are always preceded by a key (which are
%%   binaries or atoms), if Current is a list, we're inside an array, else, an
%%   object
collect({event, Event, Next}, [Current|Rest], Opts) when is_list(Current) ->
    collect(Next(), [[event(Event, Opts)] ++ Current] ++ Rest, Opts);
collect({event, Event, Next}, [Key, Current|Rest], Opts) ->
    collect(Next(), [[{Key, event(Event, Opts)}] ++ Current] ++ Rest, Opts);

%% 
collect({incomplete, _Next, Force}, [[Acc]], _Opts) when is_list(Acc) ->
    case Force() of
        {incomplete, _, _} -> Acc
        ; _ -> erlang:throw(badarg)
    end;
collect(_, _, _) -> erlang:throw(badarg).
    
        
event({string, String}, _Opts) ->
    unicode:characters_to_binary(String);
event({key, Key}, Opts) ->
    case proplists:get_value(label, Opts, binary) of
        binary -> unicode:characters_to_binary(Key)
        ; atom -> 
            try list_to_atom(Key) 
            catch error:badarg -> unicode:characters_to_binary(Key) end
        ; existing_atom -> 
            try list_to_existing_atom(Key) 
            catch error:badarg -> unicode:characters_to_binary(Key) end
    end;
%% special case for negative zero
event({integer, "-0"}, _Opts) ->
    erlang:float(erlang:list_to_integer("-0"));
event({integer, Integer}, Opts) ->
    case proplists:get_value(float, Opts, false) of
        true -> erlang:float(erlang:list_to_integer(Integer))
        ; false -> erlang:list_to_integer(Integer)
    end;
event({float, Float}, _Opts) ->
    erlang:list_to_float(Float);
event({literal, Literal}, _Opts) ->
    Literal.
    

key_repeats(Key, [{Key, _Value}|_Rest]) -> true;
key_repeats(Key, [_|Rest]) -> key_repeats(Key, Rest);
key_repeats(_Key, []) -> false.