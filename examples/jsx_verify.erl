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


-module(jsx_verify).
-author("alisdairsullivan@yahoo.ca").

-export([is_json/1, event/2]).



%% this is a strict parser, no comments, no naked values and only one key per object. it
%%   also is not streaming, though it could be modified to parse partial objects/lists.

is_json(JSON) ->
    P = jsx:parser({jsx_verify, event, ok}, []),   
    case P(JSON) of
        {incomplete, _} ->
            false
        ; {error, badjson} ->
            false
        ; _ ->
            true
    end.
 
 
%% erlang representation is dicts for objects and lists for arrays. these are pushed
%%   onto a stack, the top of which is our current level, deeper levels represent parent
%%   and grandparent levels in the json structure. keys are also stored on top of the array
%%   during parsing of their associated values.    
    
event(_, ok) ->
    ok.
