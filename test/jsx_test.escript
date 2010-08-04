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

-module(jsx_test).

-export([main/1]).

-define(to_json(X, Y, N),
    etap:is(jsx:term_to_json(X), Y, N)
).

-define(to_erep(X, Y, N),
    etap:is(jsx:json_to_term(X), Y, N)
).

main([]) ->
    test("./test/cases");

main([Path]) ->
    test(Path).


test(Dir) ->
	code:add_path("ebin"),

    ValidJSONTests = load_tests(Dir),
    
    etap:plan((length(ValidJSONTests) * 10) + 13),
    run_jsx_tests(ValidJSONTests),
    
    etap:is(multi_decode(multi_json_body(), []), multi_test_result(), "multi terms"),
    
    ?to_erep(<<"{}">>, [{}], "empty object to erep"),
    ?to_json([{}], <<"{}">>, "empty object to json"),
    ?to_erep(<<"[]">>, [], "empty array to erep"),
    ?to_json([], <<"[]">>, "empty array to json"),
    ?to_erep(<<"{ \"key\": \"value\", \"another key\": [] }">>, [{<<"key">>, <<"value">>}, {<<"another key">>, []}], "object to erep"),
    ?to_json([{<<"key">>, <<"value">>}, {<<"another key">>, []}], <<"{\"key\":\"value\",\"another key\":[]}">>, "object to json"),
    ?to_erep(<<"[true, 1, -0.5e7, \"hello world\"]">>, [true, 1, -0.5e7, <<"hello world">>], "array to erep"),
    ?to_json([true, 1, -0.5e7, <<"hello world">>], <<"[true,1,-5000000.0,\"hello world\"]">>, "array to json"),
    ?to_erep(<<"[[[]]]">>, [[[]]], "deep array to erep"),
    ?to_json([[[]]], <<"[[[]]]">>, "deep array to json"),
    ?to_erep(<<"{\"a\":{\"a\":{\"a\":{}}}}">>, [{<<"a">>, [{<<"a">>, [{<<"a">>, [{}]}]}]}], "deep object to erep"),
    ?to_json([{<<"a">>, [{<<"a">>, [{<<"a">>, [{}]}]}]}], <<"{\"a\":{\"a\":{\"a\":{}}}}">>, "deep object to json"),

    
    etap:end_tests().


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
    
run_jsx_tests([]) ->
    ok;
run_jsx_tests([{TestName, JSON, Events, Flags}|Rest]) ->
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
    run_jsx_tests(Rest).
    
    
decode(JSON, Flags) ->
    P = jsx:parser(Flags),
    decode_loop(P(JSON), []).

decode_loop({event, end_json, _Next}, Acc) ->
    lists:reverse([end_json] ++ Acc);
decode_loop({incomplete, More}, Acc) ->
    decode_loop(More(end_stream), Acc);
decode_loop({event, E, Next}, Acc) ->
    decode_loop(Next(), [E] ++ Acc).


incremental_decode(<<C:1/binary, Rest/binary>>, Flags) ->
	P = jsx:parser(Flags),
	incremental_decode_loop(P(C), Rest, []).
	
incremental_decode_loop({incomplete, Next}, <<>>, Acc) ->
    incremental_decode_loop(Next(end_stream), <<>>, Acc);	
incremental_decode_loop({incomplete, Next}, <<C:1/binary, Rest/binary>>, Acc) ->
	incremental_decode_loop(Next(C), Rest, Acc);	
incremental_decode_loop({event, end_json, _Next}, _Rest, Acc) ->
    lists:reverse([end_json] ++ Acc);
incremental_decode_loop({event, Event, Next}, Rest, Acc) ->
	incremental_decode_loop(Next(), Rest, [Event] ++ Acc).


multi_decode(JSON, Flags) ->
    P = jsx:parser(Flags ++ [{multi_term, true}]),
    multi_decode_loop(P(JSON), [[]]).
    
multi_decode_loop({incomplete, _Next}, [[]|Acc]) ->
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