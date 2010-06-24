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
	code:load_file(jsx),
	code:load_file(jsx_utf8),
	code:load_file(jsx_utf16),
	code:load_file(jsx_utf16le),
	code:load_file(jsx_utf32),
	code:load_file(jsx_utf32le),

    ValidJSONTests = load_tests(Dir),
    
    etap:plan(length(ValidJSONTests) * 5),
    Before = erlang:now(),
    run_tests(ValidJSONTests),
    etap:is(multi_decode(multi_json_body(), []), multi_test_result(), "multi terms"),
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
    
    
decode(JSON, Flags) ->
    P = jsx:parser(Flags),
    decode_loop(P(JSON), []).

decode_loop({incomplete, _Next, Force}, Acc) ->
    decode_loop(Force(), Acc);
decode_loop({event, end_json, _}, Acc) ->
    lists:reverse(Acc);
decode_loop({event, E, Next}, Acc) ->
    decode_loop(Next(), [E] ++ Acc).


incremental_decode(<<C:1/binary, Rest/binary>>, Flags) ->
	P = jsx:parser(Flags),
	incremental_decode_loop(P(C), Rest, []).
	
incremental_decode_loop({incomplete, Next, _}, <<C:1/binary, Rest/binary>>, Acc) ->
	incremental_decode_loop(Next(C), Rest, Acc);
incremental_decode_loop({incomplete, _, Force}, <<>>, Acc) ->
	incremental_decode_loop(Force(), <<>>, Acc);
incremental_decode_loop({event, end_json, Next}, <<C:1/binary, Rest/binary>>, Acc) ->
    incremental_decode_loop(Next(C), Rest, Acc);
incremental_decode_loop({event, end_json, _}, <<>>, Acc) ->
    lists:reverse(Acc);
incremental_decode_loop({event, Event, F}, Rest, Acc) ->
	incremental_decode_loop(F(), Rest, [Event] ++ Acc).


multi_decode(JSON, Flags) ->
    P = jsx:parser(Flags ++ [{multi_term, true}]),
    multi_decode_loop(P(JSON), [[]]).
    
multi_decode_loop({incomplete, _Next, Force}, [[]|Acc]) ->
    lists:reverse(Acc);
multi_decode_loop({event, end_json, Next}, [S|Acc]) ->
    multi_decode_loop(Next(), [[]|[lists:reverse(S)] ++ Acc]);
multi_decode_loop({event, E, Next}, [S|Acc]) ->
    multi_decode_loop(Next(), [[E] ++ S] ++ Acc).


to_utf16(Bin) -> unicode:characters_to_binary(Bin, utf8, utf16).
to_utf16le(Bin) -> unicode:characters_to_binary(Bin, utf8, {utf16,little}).
to_utf32(Bin) -> unicode:characters_to_binary(Bin, utf8, utf32).
to_utf32le(Bin) -> unicode:characters_to_binary(Bin, utf8, {utf32,little}).


multi_json_body() ->
    <<"0 1 -1 1e1 0.7 0.7e-1 true false null {} [] [1, 2, 3] \"hope this works\"">>.
    
multi_test_result() ->
    [ [{integer, "0"}],
        [{integer, "1"}],
        [{integer, "-1"}],
        [{float, "1.0e1"}],
        [{float, "0.7"}],
        [{float, "0.7e-1"}],
        [{literal, true}],
        [{literal, false}],
        [{literal, null}],
        [start_object, end_object],
        [start_array, end_array],
        [start_array, {integer, "1"}, {integer, "2"}, {integer, "3"}, end_array],
        [{string, "hope this works"}]
    ].

    