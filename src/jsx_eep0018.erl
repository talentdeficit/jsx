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

-export([json_to_term/2]).

-include("./include/jsx_types.hrl").



-spec json_to_term(JSON::binary(), Opts::decoder_opts()) -> json().

json_to_term(JSON, Opts) ->
    P = jsx:parser(opts_to_jsx_opts(Opts)),
    case proplists:get_value(strict, Opts, true) of
        true -> collect_strict(P(JSON), [[]], Opts)
        ; false -> collect(P(JSON), [[]], Opts)
    end.
    

opts_to_jsx_opts(Opts) ->
    opts_to_jsx_opts(Opts, []).
    
opts_to_jsx_opts([{encoding, Val}|Rest], Acc) ->
    case lists:member(Val, [auto, utf8, utf16, {utf16, little}, utf32, {utf32, little}]) of
        true -> opts_to_jsx_opts(Rest, [{encoding, Val}] ++ Acc)
        ; false -> opts_to_jsx_opts(Rest, Acc)
    end;
opts_to_jsx_opts([{comments, Val}|Rest], Acc) ->
    case Val of
        true -> opts_to_jsx_opts(Rest, [{comments, true}] ++ Acc)
        ; false -> opts_to_jsx_opts(Rest, [{comments, false}] ++ Acc)
        ; _ -> opts_to_jsx_opts(Rest, Acc)
    end;
opts_to_jsx_opts([_|Rest], Acc) ->
    opts_to_jsx_opts(Rest, Acc);
opts_to_jsx_opts([], Acc) ->
    Acc.
  
  
collect_strict({event, Start, Next}, Acc, Opts) when Start =:= start_object; Start =:= start_array ->
    collect(Next(), [[]|Acc], Opts);
collect_strict(_, _, _) ->
    erlang:error(badarg).
    
    
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
    
%% end of json is emitted asap (at close of array/object), calling Next() until {incomplete, More}
%%   and then More(end_stream) ensures the tail of the json binary is clean (whitespace only)     
collect({event, end_json, Next}, [[Acc]], _Opts) ->
    case Next() of
        {incomplete, More} -> case More(end_stream) of
            ok -> Acc
            ; _ -> erlang:error(badarg)
        end
        ; _ -> erlang:error(badarg)
    end;
    
%% key can only be emitted inside of a json object, so just insert it directly into
%%   the head of the accumulator and deal with it when we receive it's paired value    
collect({event, {key, _} = PreKey, Next}, [Current|_] = Acc, Opts) ->
    Key = event(PreKey, Opts),
    case key_repeats(Key, Current) of
        true -> erlang:error(badarg)
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

%% any other event is an error
collect(_, _, _) -> erlang:error(badarg).
    
        
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