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


-module(jsx_eep0018).
-author("alisdairsullivan@yahoo.ca").

-export([json_to_term/1, json_to_term/2]).
-export([term_to_json/1, term_to_json/2]).


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



term_to_json(JSON) ->
    term_to_json(JSON, []).
    
term_to_json(JSON, Opts) ->
    Encoding = proplists:get_value(encoding, Opts, utf8),
    Events = lists:reverse([end_json] ++ term_to_events(JSON, Encoding), Opts),
    pretty_print(Events, []).
    
pretty_print(Events, _Opts) ->
    io:format("~p~n", [Events]).
 
term_to_events([{}], _) ->
    [start_object, end_object];
term_to_events([First|_] = JSON, Encoding) when is_tuple(First) ->
    proplist_to_events(JSON, [start_object], Encoding);
term_to_events(JSON, Encoding) ->
    list_to_events(JSON, [start_array], Encoding).    
    
proplist_to_events([{Key, Term}|Rest], Acc, Encoding) ->
    proplist_to_events(Rest,
        [term_to_event(Term, Encoding), key_to_event(Key, Encoding)] ++ Acc,
        Encoding
    );
proplist_to_events([], Acc, _Opts) ->
    [end_object] ++ Acc.
    
    
list_to_events([Term|Rest], Acc, Encoding) ->
    list_to_events(Rest, [term_to_event(Term, Encoding)] ++ Acc, Encoding);
list_to_events([], Acc, _Opts) ->
    [end_array] ++ Acc.


term_to_event(List, Encoding) when is_list(List) ->
    term_to_events(List, Encoding);
term_to_event(Float, Encoding) when is_float(Float) ->
    {float, unicode:characters_to_binary(float_to_decimal(Float), latin1, Encoding)};
term_to_event(Integer, Encoding) when is_integer(Integer) ->
    {integer, unicode:characters_to_binary(erlang:integer_to_list(Integer), latin1, Encoding)};
term_to_event(String, Encoding) when is_binary(String) -> 
    {string, unicode:characters_to_binary(String, utf8, Encoding)};
term_to_event(true, _Encoding) -> {literal, true};
term_to_event(false, _Encoding) -> {literal, false};
term_to_event(null, _Encoding) -> {literal, null}.


key_to_event(Key, Encoding) when is_atom(Key) ->
    {key, unicode:characters_to_binary(erlang:atom_to_list(Key), latin1, Encoding)};
key_to_event(Key, Encoding) when is_binary(Key) ->
    {key, unicode:characters_to_binary(Key, utf8, Encoding)}.
    


%% converting floats to printable ascii is a lot harder than you think, this is
%%   temporary

float_to_decimal(Float) ->
    float_to_list(Float).    