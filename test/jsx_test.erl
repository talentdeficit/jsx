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


-module(jsx_test).
-author("alisdairsullivan@yahoo.ca").

-export([test/1]).

-include_lib("eunit/include/eunit.hrl").


test(Dir) ->
    Tests = gen_tests(Dir),
    eunit:test(Tests, [verbose]).

gen_tests(Dir) ->
    TestSpecs = filelib:wildcard("*.test", Dir),
    gen_tests(TestSpecs, Dir, []).
    
gen_tests([], _, Acc) ->
    lists:reverse(Acc); 
    
gen_tests([Test|Rest], Dir, Acc) ->
    gen_tests(Rest, Dir, test_body(Test, Dir) ++ Acc).
    
test_body(TestSpec, Dir) ->
    try
        TestName = filename:basename(TestSpec, ".test"),
        {ok, JSON} = file:read_file(Dir ++ "/" ++ TestName ++ ".json"),
        case file:consult(Dir ++ "/" ++ TestSpec) of
            {ok, [Events]} ->
                Decoder = jsx:decoder(),
                [{TestName ++ "_incremental", ?_assertEqual(incremental_decode(Decoder, JSON), Events)}] ++
                [{TestName, ?_assertEqual(decode(Decoder, JSON), Events)}]
            ; {ok, [Events, Flags]} ->
                Decoder = jsx:decoder(Flags),
                [{TestName ++ "_incremental", ?_assertEqual(incremental_decode(Decoder, JSON), Events)}] ++
                [{TestName, ?_assertEqual(decode(Decoder, JSON), Events)}]
        end
    catch _:_ -> []
    end.
    
incremental_decode(F, <<>>) ->
    case F(<<>>) of
        {incomplete, G} -> G
        ; {Result, _} -> Result
    end;
incremental_decode(F, <<A/utf8, Rest/binary>>) ->
    {_, G} = F(<<A>>),
    incremental_decode(G, Rest).
    
decode(F, JSON) ->
    case F(JSON) of
        {incomplete, G} when is_function(G) -> 
            throw(badjson)
        ; {Result, _} ->
            Result
    end.
    