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


%% this module is an example of how to use the raw parser api 

-module(jsx_parser).
-author("alisdairsullivan@yahoo.ca").

-export([decode/1, event/2]).

%% export to allow the dirty hack below
-export([literal/1, string/1, float/1, integer/1]).


%% this is a strict parser, no comments, no naked values and only one key per object. it
%%   also is not streaming, though it could be modified to parse partial objects/lists.

%% event takes two arguments, the result of calling the parser on a json argument (or the
%%   generator returned by the parser) and a term that holds the erlang representation of
%%   the json.

decode(JSON) ->
    F = jsx:parser(),   
    try event(F(JSON), [])
    catch error:badjson -> {error, badjson}
    end.
 
 
%% erlang representation is dicts for objects and lists for arrays. 
    
event({start_object, Next}, Stack) ->
    event(Next(), [dict:new()] ++ Stack);
event({start_array, Next}, Stack) ->
    event(Next(), [[]] ++ Stack);
    
event({end_object, Next}, [Object, {key, Key}, Parent|Stack]) when is_tuple(Parent) ->
    event(Next(), [insert(Key, Object, Parent)] ++ Stack);
event({end_array, Next}, [Array, {key, Key}, Parent|Stack]) when is_tuple(Parent) ->
    event(Next(), [insert(Key, lists:reverse(Array), Parent)] ++ Stack);    
event({end_object, Next}, [Object, Parent|Stack]) when is_list(Parent) ->
    event(Next(), [[Object] ++ Parent] ++ Stack);
event({end_array, Next}, [Array, Parent|Stack]) when is_list(Parent) ->
    event(Next(), [[lists:reverse(Array)] ++ Parent] ++ Stack);

%% special cases for closing the root objects

event({end_object, Next}, [Object]) ->
    event(Next(), [Object]);
event({end_array, Next}, [Array]) ->
    event(Next(), [lists:reverse(Array)]);    
    
%% keys are just pushed onto the stack until their corresponding value is
%%   encountered    
    
event({{key, Key}, Next}, [Stack]) ->
    event(Next(), [{key, Key}] ++ Stack);

%% reject values that aren't wrapped by an array or object

event({{_Type, _Value}, _Next}, []) ->
    {error, badjson};

%% this is kind of a dirty hack, but erlang will interpret atoms when applied to (Args)
%%   as a function. so naming our formatting functions string, integer, float and literal will
%%   allow the following shortcut

event({{Type, Value}, Next}, [{key, Key}, Object|Stack]) ->
    event(Next(), [insert(Key, ?MODULE:Type(Value), Object)] ++ Stack);
event({{Type, Value}, Next}, [Array|Stack]) when is_list(Array) ->
    event(Next(), [[?MODULE:Type(Value)] ++ Array] ++ Stack);
       
event({end_json, _}, [Stack]) ->
    Stack.
    
    
%% we're restricting keys to one occurence per object, as the spec implies.    
    
insert(Key, Val, Dict) ->
    case dict:is_key(Key, Dict) of
        false -> dict:store(Key, Val, Dict)
        ; true -> erlang:error(badjson)
    end.
    

%% strings and literals we just return with no post-processing, numbers we convert
%%   from strings to integers/floats as appropriate
   
string(String) ->
    String.
integer(Number) ->
    list_to_integer(Number).
float(Number) ->
    list_to_float(Number).
literal(Literal) ->
    Literal.