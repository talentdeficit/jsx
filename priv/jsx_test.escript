#!/usr/bin/env escript

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

-mode(compile).

main([Path]) ->
    test(Path).


test(Dir) ->
	code:add_path("ebin"),

    ValidJSONTests = load_tests(Dir),
    
    etap:plan(length(ValidJSONTests) * 5),
    Before = erlang:now(),
    run_tests(ValidJSONTests),
    After = erlang:now(),
    etap:end_tests(),
    
    io:format("Elapsed Time:  ~p~n", [timer:now_diff(After, Before)]).


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
    etap:is(decode(JSON, Flags), Events, TestName ++ ": utf8"),
    etap:is(incremental_decode(JSON, Flags), Events, TestName ++ ": incremental utf8"),
    etap:is(decode(to_utf16(JSON), Flags), Events, TestName ++ ": utf16"),
    etap:is(incremental_decode(to_utf16(JSON), Flags), Events, TestName ++ ": incremental utf16"),
    etap:is(decode(to_utf16le(JSON), Flags), Events, TestName ++ ": utf16le"),
    etap:is(incremental_decode(to_utf16le(JSON), Flags), Events, TestName ++ ": incremental utf16le"),
    etap:is(decode(to_utf32(JSON), Flags), Events, TestName ++ ": utf32"),
    etap:is(incremental_decode(to_utf32(JSON), Flags), Events, TestName ++ ": incremental utf32"),
    etap:is(decode(to_utf32le(JSON), Flags), Events, TestName ++ ": utf32le"),
    etap:is(incremental_decode(to_utf32le(JSON), Flags), Events, TestName ++ ": incremental utf32le"),
    run_tests(Rest).
    
incremental_decode(<<C:1/binary, Rest/binary>>, Flags) ->
	P = jsx:parser(Flags),
	incremental_decode(P(C), Rest, []).
	
incremental_decode({incomplete, Next, _}, <<C:1/binary, Rest/binary>>, Acc) ->
	incremental_decode(Next(C), Rest, Acc);
incremental_decode({incomplete, _, Force}, <<>>, Acc) ->
	incremental_decode(Force(), <<>>, Acc);
incremental_decode({event, end_json, Next}, <<C:1/binary, Rest/binary>>, Acc) ->
    incremental_decode(Next(C), Rest, Acc);
incremental_decode({event, end_json, _}, <<>>, Acc) ->
    lists:reverse(Acc);
incremental_decode({event, Event, F}, Rest, Acc) ->
	incremental_decode(F(), Rest, [Event] ++ Acc).


decode(JSON, Flags) ->
    {ok, Result} = jsx:fold(fun(end_json, S) -> 
				lists:reverse(S) 
			;(E, S) -> 
				[E] ++ S 
		end, [], JSON, Flags),
	Result.
    
to_utf16(Bin) -> unicode:characters_to_binary(Bin, utf8, utf16).
to_utf16le(Bin) -> unicode:characters_to_binary(Bin, utf8, {utf16,little}).
to_utf32(Bin) -> unicode:characters_to_binary(Bin, utf8, utf32).
to_utf32le(Bin) -> unicode:characters_to_binary(Bin, utf8, {utf32,little}).

    