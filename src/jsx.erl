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


-module(jsx).


%% the core parser api
-export([scanner/0, scanner/1]).

-include("../include/jsx_types.hrl").


-spec scanner() -> jsx_scanner().
scanner() -> scanner([]).

-spec scanner(OptsList::jsx_opts()) -> jsx_scanner().
scanner(OptsList) ->
    fun(Stream) when is_binary(Stream) ->
            (jsx_decoder:decoder(OptsList))(Stream)
        ; (Stream) when is_list(Stream); is_tuple(Stream) ->
            (jsx_tokenizer:tokenizer(OptsList))(Stream)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


jsx_decoder_test_() ->
    jsx_decoder_gen(load_tests(?eunit_test_path)).


encoder_decoder_equiv_test_() ->
    [
        {"encoder/decoder equivalency",
            ?_assert(begin {ok, X, _} = (jsx:scanner())(
                    <<"[\"a\", 17, 3.14, true, {\"k\":false}, []]">>
                ), X end =:= begin {ok, Y, _} = (jsx:scanner())(
                    [start_array,
                        {string, <<"a">>},
                        {integer, 17},
                        {float, 3.14},
                        {literal, true},
                        start_object,
                        {key, <<"k">>},
                        {literal, false},
                        end_object,
                        start_array,
                        end_array,
                        end_array,
                        end_json]
                ), Y end
            )
        }
    ].
    
    
jsx_decoder_gen([]) -> [];    
jsx_decoder_gen([Test|Rest]) ->
    Name = proplists:get_value(name, Test),
    JSON = proplists:get_value(json, Test),
    JSX = proplists:get_value(jsx, Test),
    Flags = proplists:get_value(jsx_flags, Test, []),
    {generator, fun() ->
        [{Name, ?_assertEqual(decode(JSON, Flags), JSX)},
            {Name ++ " (incremental)",
                ?_assertEqual(incremental_decode(JSON, Flags), JSX)
            }
            | jsx_decoder_gen(Rest)
        ]
    end}.


load_tests(Path) ->
    %% search the specified directory for any files with the .test ending
    TestSpecs = filelib:wildcard("*.test", Path),
    load_tests(TestSpecs, Path, []).

load_tests([], _Dir, Acc) ->
    lists:reverse(Acc);
load_tests([Test|Rest], Dir, Acc) ->
    case file:consult(Dir ++ "/" ++ Test) of
        {ok, TestSpec} ->
            ParsedTest = parse_tests(TestSpec, Dir),
            load_tests(Rest, Dir, [ParsedTest] ++ Acc)
        ; {error, _Reason} ->
            erlang:error(badarg, [Test|Rest], Dir, Acc)
    end.


parse_tests(TestSpec, Dir) ->
    parse_tests(TestSpec, Dir, []).
    
parse_tests([{json, Path}|Rest], Dir, Acc) when is_list(Path) ->
    case file:read_file(Dir ++ "/" ++ Path) of
        {ok, Bin} -> parse_tests(Rest, Dir, [{json, Bin}] ++ Acc)
        ; _ -> erlang:error(badarg, [[{json, Path}|Rest], Dir, Acc])
    end;
parse_tests([KV|Rest], Dir, Acc) ->
    parse_tests(Rest, Dir, [KV] ++ Acc);
parse_tests([], _Dir, Acc) ->
    Acc.


decode(JSON, Flags) ->
    try
        P = jsx:scanner(Flags), 
        {ok, X, More} = P(JSON),
        {ok, Y, _More} = More(<<" ">>),
        V = X ++ Y,
        case lists:reverse(V) of
            [end_json|_] -> V
            ; _ -> {error, badjson}
        end
    catch
        error:badarg -> {error, badjson}
    end.

    
incremental_decode(<<C:1/binary, Rest/binary>>, Flags) ->
	P = jsx:scanner(Flags),
	try incremental_decode_loop(P(C), Rest, [])
	catch error:badarg -> io:format("~p~n", [erlang:get_stacktrace()]), {error, badjson}
	end.

incremental_decode_loop({ok, X, More}, <<>>, Acc) ->
    {ok, Y, _} = More(<<" ">>),     %% clear any naked numbers
    V = Acc ++ X ++ Y,
    case lists:reverse(V) of
        [end_json|_] -> V
        ; _ -> {error, badjson}
    end;
incremental_decode_loop({ok, T, More}, <<C:1/binary, Rest/binary>>, Acc) ->
    incremental_decode_loop(More(C), Rest, Acc ++ T).

    
-endif.