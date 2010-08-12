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

-ifndef(test).
-export([test/0]).
-endif.

-ifdef(test).
-include_lib("eunit/include/eunit.hrl").
-endif.

    
%% if not compiled with test support

-ifndef(test).

test() -> erlang:error(notest).

-else.

jsx_decoder_test_() ->
    jsx_decoder_gen(load_tests(?eunit_test_path)).
    
    
jsx_decoder_gen([]) -> [];    
jsx_decoder_gen(Tests) -> jsx_decoder_gen(Tests, [utf8, utf16, {utf16, little}, utf32, {utf32, little}]).    
    
jsx_decoder_gen([_Test|Rest], []) ->
    jsx_decoder_gen(Rest);
jsx_decoder_gen([Test|_] = Tests, [Encoding|Encodings]) ->
    Name = lists:flatten(proplists:get_value(name, Test) ++ " :: " ++ io_lib:format("~p", [Encoding])),
    JSON = unicode:characters_to_binary(proplists:get_value(json, Test), unicode, Encoding),
    JSX = proplists:get_value(jsx, Test),
    Flags = proplists:get_value(jsx_flags, Test, []),
    {generator,
        fun() ->
            [{Name, ?_assert(decode(JSON, Flags) =:= JSX)} 
                | {generator, 
                        fun() -> [{Name ++ " incremental", ?_assert(incremental_decode(JSON, Flags) =:= JSX)}
                            | jsx_decoder_gen(Tests, Encodings)]
                        end
                }
            ]
        end
    }.


load_tests(Path) ->
    %% search the specified directory for any files with the .test ending
    TestSpecs = filelib:wildcard("*.test", Path),
    load_tests(TestSpecs, Path, []).

load_tests([], _Dir, Acc) ->
    lists:reverse(Acc);
load_tests([Test|Rest], Dir, Acc) ->
    %% should alert about badly formed tests eventually, but for now just skip over them
    case file:consult(Dir ++ "/" ++ Test) of
        {ok, TestSpec} ->
            try
                ParsedTest = parse_tests(TestSpec, Dir),
                load_tests(Rest, Dir, [ParsedTest] ++ Acc)
            catch _:_ ->
                load_tests(Rest, Dir, Acc)
            end
        ; {error, _Reason} ->
            load_tests(Rest, Dir, Acc)
    end.


parse_tests(TestSpec, Dir) ->
    parse_tests(TestSpec, Dir, []).
    
parse_tests([{json, Path}|Rest], Dir, Acc) when is_list(Path) ->
    case file:read_file(Dir ++ "/" ++ Path) of
        {ok, Bin} -> parse_tests(Rest, Dir, [{json, Bin}] ++ Acc)
        ; _ -> erlang:error(badarg)
    end;
parse_tests([KV|Rest], Dir, Acc) ->
    parse_tests(Rest, Dir, [KV] ++ Acc);
parse_tests([], _Dir, Acc) ->
    Acc.


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


multi_decode_test_() ->
    [
        {"multiple values in a single stream", ?_assert(multi_decode(multi_json_body(), []) =:= multi_test_result())}
    ].

	
multi_decode(JSON, Flags) ->
    P = jsx:parser(Flags ++ [{multi_term, true}]),
    multi_decode_loop(P(JSON), [[]]).

multi_decode_loop({incomplete, _Next}, [[]|Acc]) ->
    lists:reverse(Acc);
multi_decode_loop({event, end_json, Next}, [S|Acc]) ->
    multi_decode_loop(Next(), [[]|[lists:reverse(S)] ++ Acc]);
multi_decode_loop({event, E, Next}, [S|Acc]) ->
    multi_decode_loop(Next(), [[E] ++ S] ++ Acc).
	
	
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

-endif.