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

-export([is_json/2]).

-include("./include/jsx_types.hrl").



-spec is_json(JSON::binary(), Opts::verify_opts()) -> true | false.

is_json(JSON, Opts) ->
    Encoding = proplists:get_value(encoding, Opts, utf8),
    P = jsx:parser([{encoding, Encoding}]),
    case proplists:get_value(strict, Opts, true) of
        true -> collect_strict(P(JSON), Opts)
        ; false -> collect(P(JSON), Opts)
    end.
    

%% enforce only arrays and objects at top level
collect_strict({event, start_object, Next}, Keys) ->
    collect(Next(), Keys);
collect_strict({event, start_array, Next}, Keys) ->
    collect(Next(), Keys);
collect_strict(_, _) ->
    false.
    

collect({event, end_json, _Next}, _Keys) ->
    true;
        
%% check to see if key has already been encountered, if not add it to the key accumulator
%%   and continue, else return false 
collect({event, {key, Key}, Next}, Keys) ->
    case lists:member(Key, Keys) of
        true -> false
        ; false -> collect(Next(), [Key] ++ Keys)
    end;
                
collect({event, _, Next}, Keys) ->
    collect(Next(), Keys);
collect(_, _) ->
    false.

    
    
