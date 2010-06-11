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

-export([test/0, test/1, test_event/2]).

test() ->
    F = decoder([]),
    incremental_decode(F, unicode:characters_to_binary(<<"0">>, utf8, utf16)).

test(Dir) ->
    ValidJSONTests = load_tests(Dir),
    
    etap:plan(length(ValidJSONTests) * 10),
    run_tests(ValidJSONTests),
    etap:end_tests().


decoder(Flags) ->
    jsx:decoder({jsx_test, test_event, []}, Flags).

test_event(end_json, Acc) ->
    lists:reverse(Acc);
test_event(Event, Acc) ->
    [Event] ++ Acc.


load_tests(Dir) ->
    TestSpecs = filelib:wildcard("*.test", Dir),
    load_tests(TestSpecs, Dir, []).
    
load_tests([], _Dir, Acc) ->
    lists:reverse(Acc);
load_tests([Test|Rest], Dir, Acc) ->
    try
        TestName = filename:basename(Test, ".test"),
        {ok, JSON} = file:read_file(Dir ++ "/" ++ TestName ++ ".json"),
        case file:consult(Dir ++ "/" ++ Test) of
            {ok, [Events]} ->
                load_tests(Rest, Dir, [{TestName, JSON, Events, []}] ++ Acc)
            ; {ok, [Events, Flags]} ->
                load_tests(Rest, Dir, [{TestName, JSON, Events, Flags}] ++ Acc)
        end
    catch _:_ -> load_tests(Rest, Dir, Acc) end.
    
run_tests([]) ->
    ok;
run_tests([{TestName, JSON, Events, Flags}|Rest]) ->
    F = decoder(Flags),
    etap:is(decode(F, JSON), Events, TestName ++ ": utf8"),
    etap:is(incremental_decode(F, JSON), Events, TestName ++ ": incremental utf8"),
    etap:is(decode(F, to_utf16(JSON)), Events, TestName ++ ": utf16"),
    etap:is(incremental_decode(F, to_utf16(JSON)), Events, TestName ++ ": incremental utf16"),
    etap:is(decode(F, to_utf16le(JSON)), Events, TestName ++ ": utf16le"),
    etap:is(incremental_decode(F, to_utf16le(JSON)), Events, TestName ++ ": incremental utf16le"),
    etap:is(decode(F, to_utf32(JSON)), Events, TestName ++ ": utf32"),
    etap:is(incremental_decode(F, to_utf32(JSON)), Events, TestName ++ ": incremental utf32"),
    etap:is(decode(F, to_utf32le(JSON)), Events, TestName ++ ": utf32le"),
    etap:is(incremental_decode(F, to_utf32le(JSON)), Events, TestName ++ ": incremental utf32le"),
    run_tests(Rest).
    
    
incremental_decode(F, <<>>) ->
    case F(<<>>) of
        {incomplete, G} -> G
        ; {Result, _} -> Result
    end;
incremental_decode(F, <<A, Rest/binary>>) ->
    {_, G} = F(<<A>>),
    incremental_decode(G, Rest).
    
decode(F, JSON) ->
    case F(JSON) of
        {incomplete, G} when is_function(G) -> 
            throw(badjson)
        ; {Result, _} ->
            Result
    end.
    
to_utf16(Bin) -> unicode:characters_to_binary(Bin, utf8, utf16).
to_utf16le(Bin) -> unicode:characters_to_binary(Bin, utf8, {utf16,little}).
to_utf32(Bin) -> unicode:characters_to_binary(Bin, utf8, utf32).
to_utf32le(Bin) -> unicode:characters_to_binary(Bin, utf8, {utf32,little}).

    