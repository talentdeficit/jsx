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

-export([scanner/0, scanner/1]).
-export([encoder/0, encoder/1]).
-export([decoder/0, decoder/1]).
-export([fold/3, fold/4]).
-export([format/1, format/2]).


%% various semi-useful types with nowhere else to hang out

-type events() :: [event()].
-type event() :: start_object
    | end_object
    | start_array
    | end_array
    | end_json
    | {key, list()}
    | {string, list()}
    | {integer, integer()}
    | {float, float()}
    | {literal, true}
    | {literal, false}
    | {literal, null}.

%% definition of the opts record for the encoder and decoder 
-include("../include/jsx_opts.hrl").

-type opts() :: [opt()].
-type opt() :: loose_unicode | escape_forward_slashes | explicit_end.

-type scanner() :: decoder() | encoder().


-spec scanner() -> scanner().
-spec scanner(OptsList::opts()) -> scanner().

scanner() -> scanner([]).

scanner(OptsList) when is_list(OptsList) ->
    fun(JSON) when is_binary(JSON) -> (decoder(OptsList))(JSON)
        ; (Terms) when is_list(Terms) -> (encoder(OptsList))(Terms)
    end.


-type decoder() :: fun((binary()) -> {ok, events()} | {incomplete, decoder()}).

-spec decoder() -> decoder().
-spec decoder(OptsList::opts()) -> decoder().

decoder() -> decoder([]).

decoder(OptsList) when is_list(OptsList) -> jsx_decoder:decoder(OptsList).


-type encoder() :: fun((list()) ->
    {ok, events()} | {incomplete, decoder()}).

-spec encoder() -> encoder().
-spec encoder(OptsList::opts()) -> encoder().

encoder() -> encoder([]).

encoder(OptsList) when is_list(OptsList) -> jsx_encoder:encoder(OptsList).


-spec fold(fun((Elem::event(), AccIn::any()) -> AccOut::any()),
        AccInitial::any(),
        Source::binary()) ->
    {ok, AccFinal::any()} | {incomplete, decoder()}
    ; (fun((Elem::event(), AccIn::any()) -> AccOut::any()),
        AccInitial::any(),
        Source::list()) ->
    {ok, AccFinal::any()} | {incomplete, encoder()}.
-spec fold(fun((Elem::event(), AccIn::any()) -> AccOut::any()),
        AccInitial::any(),
        Source::binary(),
        OptsLists::opts()) ->
    {ok, AccFinal::any()} | {incomplete, decoder()}
    ; (fun((Elem::event(), AccIn::any()) -> AccOut::any()),
        AccInitial::any(),
        Source::list(),
        OptsList::opts()) ->
    {ok, AccFinal::any()} | {incomplete, encoder()}.

fold(Fun, Acc, Source) -> fold(Fun, Acc, Source, []).

fold(Fun, Acc, Source, Opts) ->
    case (scanner(Opts))(Source) of
        {ok, Events} -> lists:foldl(Fun, Acc, Events)
        ; {incomplete, F} -> {incomplete, F}
    end.


-spec format(Source::binary()) -> binary().
-spec format(Source::binary() | list(), Opts::jsx_format:opts()) -> binary().

format(Source) -> format(Source, []).

format(Source, Opts) -> jsx_format:format(Source, Opts).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


jsx_decoder_test_() ->
    jsx_decoder_gen(load_tests(?eunit_test_path)).


encoder_decoder_equiv_test_() ->
    [
        {"encoder/decoder equivalency",
            ?_assert(begin {ok, X} = (jsx:decoder())(
                    <<"[\"a\", 17, 3.14, true, {\"k\":false}, []]">>
                ), X end =:= begin {ok, Y} = (jsx:encoder())(
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


fold_test_() ->
    [{"fold test",
        ?_assert(fold(fun(_, true) -> true end,
            true,
            <<"[\"a\", 17, 3.14, true, {\"k\":false}, []]">>,
            []
        ))
    }].
    
    
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
        case (jsx:scanner(Flags))(JSON) of
            {ok, Events} -> Events
            ; {incomplete, More} ->
                case More(<<" ">>) of
                    {ok, Events} -> Events
                    ; _ -> {error, badjson}
                end
        end
    catch
        error:badarg -> {error, badjson}
    end.

    
incremental_decode(<<C:1/binary, Rest/binary>>, Flags) ->
	P = jsx:scanner(Flags ++ [explicit_end]),
	try incremental_decode_loop(P(C), Rest)
	catch error:badarg -> io:format("~p~n", [erlang:get_stacktrace()]), {error, badjson}
	end.

incremental_decode_loop({incomplete, More}, <<>>) ->
    case More(end_stream) of
        {ok, X} -> X
        ; _ -> {error, badjson}
    end;
incremental_decode_loop({incomplete, More}, <<C:1/binary, Rest/binary>>) ->
    incremental_decode_loop(More(C), Rest).

    
-endif.