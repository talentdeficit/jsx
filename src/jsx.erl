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

-export([encode/1, encode/2, decode/1, decode/2]).
-export([is_json/1, is_json/2, is_term/1, is_term/2]).
-export([format/1, format/2, minify/1, prettify/1]).
-export([encoder/3, decoder/3, parser/3]).
%% old api
-export([term_to_json/1, term_to_json/2, json_to_term/1, json_to_term/2]).
-export([to_json/1, to_json/2]).
-export([to_term/1, to_term/2]).

-export_type([json_term/0, json_text/0]).

%% test handler
-ifdef(TEST).
-export([init/1, handle_event/2]).
-endif.


-type json_term() :: list({binary(), json_term()})
    | list(json_term())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().

-type json_text() :: binary().


-spec encode(Source::json_term()) -> json_text().
-spec encode(Source::json_term(), Opts::jsx_to_json:opts()) -> json_text().

encode(Source) -> encode(Source, []).

encode(Source, Opts) -> jsx_to_json:to_json(Source, Opts).

%% old api, alias for encode/x

to_json(Source) -> encode(Source, []).
to_json(Source, Opts) -> encode(Source, Opts).
term_to_json(Source) -> encode(Source, []).
term_to_json(Source, Opts) -> encode(Source, Opts).


-spec format(Source::json_text()) -> json_text().
-spec format(Source::json_text(), Opts::jsx_to_json:opts()) -> json_text().

format(Source) -> format(Source, []).

format(Source, Opts) -> jsx_to_json:format(Source, Opts).


-spec minify(Source::json_text()) -> json_text().

minify(Source) -> format(Source, []).


-spec prettify(Source::json_text()) -> json_text().

prettify(Source) -> format(Source, [space, {indent, 2}]).


-spec decode(Source::json_text()) -> json_term().
-spec decode(Source::json_text(), Opts::jsx_to_term:opts()) -> json_term().

decode(Source) -> decode(Source, []).

decode(Source, Opts) -> jsx_to_term:to_term(Source, Opts).

%% old api, alias for to_term/x

to_term(Source) -> decode(Source, []).
to_term(Source, Opts) -> decode(Source, Opts).
json_to_term(Source) -> decode(Source, []).
json_to_term(Source, Opts) -> decode(Source, Opts).


-spec is_json(Source::any()) -> true | false.
-spec is_json(Source::any(), Opts::jsx_verify:opts()) -> true | false.

is_json(Source) -> is_json(Source, []).

is_json(Source, Opts) -> jsx_verify:is_json(Source, Opts).


-spec is_term(Source::any()) -> true | false.
-spec is_term(Source::any(), Opts::jsx_verify:opts()) -> true | false.

is_term(Source) -> is_term(Source, []).

is_term(Source, Opts) -> jsx_verify:is_term(Source, Opts).


-type decoder() :: fun((json_text() | end_stream) -> any()).

-spec decoder(Handler::module(), State::any(), Opts::list()) -> decoder().

decoder(Handler, State, Opts) -> jsx_decoder:decoder(Handler, State, Opts).


-type encoder() :: fun((json_term() | end_stream) -> any()).

-spec encoder(Handler::module(), State::any(), Opts::list()) -> encoder().

encoder(Handler, State, Opts) -> jsx_encoder:encoder(Handler, State, Opts).


-type token() :: [token()]
    | start_object
    | end_object
    | start_array
    | end_array
    | {key, binary()}
    | {string, binary()}
    | binary()
    | {number, integer() | float()}
    | {integer, integer()}
    | {float, float()}
    | integer()
    | float()
    | {literal, true}
    | {literal, false}
    | {literal, null}
    | true
    | false
    | null
    | end_json.


-type parser() :: fun((token() | end_stream) -> any()).

-spec parser(Handler::module(), State::any(), Opts::list()) -> parser().

parser(Handler, State, Opts) -> jsx_parser:parser(Handler, State, Opts).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


jsx_decoder_test_() ->
    jsx_decoder_gen(load_tests(code:lib_dir(jsx, priv) ++ "/test_cases/")).


encoder_decoder_equiv_test_() ->
    [
        {"encoder/decoder equivalency",
            ?_assert((jsx_decoder:decoder(?MODULE, [], []))(
                    <<"[\"a\", 17, 3.14, true, {\"k\":false}, []]">>
                ) =:= (jsx_encoder:encoder(?MODULE, [], []))([<<"a">>, 17, 3.14, true, [{<<"k">>, false}], []])
            )
        }
    ].


single_quoted_strings_test_() ->
    [
        {"single quoted keys",
            ?_assertEqual(
                to_term(<<"{'key':true}">>, [single_quoted_strings]),
                [{<<"key">>, true}]
            )
        },
        {"multiple single quoted keys",
            ?_assertEqual(
                to_term(<<"{'key':true, 'another key':true}">>, [single_quoted_strings]),
                [{<<"key">>, true}, {<<"another key">>, true}]
            )
        },
        {"nested single quoted keys",
            ?_assertEqual(
                to_term(<<"{'key': {'key':true, 'another key':true}}">>, [single_quoted_strings]),
                [{<<"key">>, [{<<"key">>, true}, {<<"another key">>, true}]}]
            )
        },
        {"single quoted string",
            ?_assertEqual(
                to_term(<<"['string']">>, [single_quoted_strings]),
                [<<"string">>]
            )
        },
        {"single quote in double quoted string",
            ?_assertEqual(
                to_term(<<"[\"a single quote: '\"]">>, [single_quoted_strings]),
                [<<"a single quote: '">>]
            )
        },
        {"escaped single quote in single quoted string",
            ?_assertEqual(
                to_term(<<"['a single quote: \\'']">>, [single_quoted_strings]),
                [<<"a single quote: '">>]
            )
        },
        {"escaped single quote when single quotes are disallowed",
            ?_assertError(
                badarg,
                to_term(<<"[\"a single quote: \\'\"]">>)
            )
        },
        {"mismatched quotes",
            ?_assertError(
                badarg,
                to_term(<<"['mismatched\"]">>, [single_quoted_strings])
            )
        }
    ].


%% test handler
init([]) -> [].

handle_event(end_json, State) -> lists:reverse([end_json] ++ State);
handle_event(Event, State) -> [Event] ++ State.



jsx_decoder_gen([]) -> [];
jsx_decoder_gen([Test|Rest]) ->
    Name = proplists:get_value(name, Test),
    JSON = proplists:get_value(json, Test),
    JSX = proplists:get_value(jsx, Test),
    Flags = proplists:get_value(jsx_flags, Test, []),
    {generator, fun() ->
        [{Name, ?_assertEqual(test_decode(JSON, Flags), JSX)},
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


test_decode(JSON, Flags) ->
    try
        case (jsx_decoder:decoder(?MODULE, [], Flags))(JSON) of
            {incomplete, More} ->
                case More(<<" ">>) of
                    {incomplete, _} -> {error, badarg}
                    ; Events -> Events
                end
            ; Events -> Events
        end
    catch
        error:badarg -> {error, badarg}
    end.


incremental_decode(<<C:1/binary, Rest/binary>>, Flags) ->
	P = jsx_decoder:decoder(?MODULE, [], Flags ++ [explicit_end]),
	try incremental_decode_loop(P(C), Rest)
	catch error:badarg -> {error, badarg}
	end.

incremental_decode_loop({incomplete, More}, <<>>) ->
    case More(end_stream) of
        {incomplete, _} -> {error, badarg}
        ; X -> X
    end;
incremental_decode_loop({incomplete, More}, <<C:1/binary, Rest/binary>>) ->
    incremental_decode_loop(More(C), Rest).


-endif.
