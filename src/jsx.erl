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
-export([parser/0, parser/1]).
-export([term_to_json/1, term_to_json/2]).
-export([json_to_term/1, json_to_term/2]).
-export([is_json/1, is_json/2]).
-export([format/1, format/2]).
-export([eventify/1]).


-include("./include/jsx_common.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec parser() -> jsx_parser().

parser() ->
    parser([]).
    

-spec parser(OptsList::jsx_opts()) -> jsx_parser().

parser(OptsList) ->
    case proplists:get_value(encoding, OptsList, auto) of
        utf8 -> jsx_utf8:parser(OptsList)
        ; utf16 -> jsx_utf16:parser(OptsList)
        ; utf32 -> jsx_utf32:parser(OptsList)
        ; {utf16, little} -> jsx_utf16le:parser(OptsList)
        ; {utf32, little} -> jsx_utf32le:parser(OptsList)
        ; auto -> detect_encoding(OptsList)
    end.


-spec json_to_term(JSON::binary()) -> eep0018().

json_to_term(JSON) ->
    try json_to_term(JSON, [])
    %% rethrow exception so internals aren't confusingly exposed to users
    catch error:badarg -> erlang:error(badarg)
    end.
    

-spec json_to_term(JSON::binary(), Opts::decoder_opts()) -> eep0018(). 
    
json_to_term(JSON, Opts) ->
    jsx_eep0018:json_to_term(JSON, Opts).


-spec term_to_json(JSON::eep0018()) -> binary().

term_to_json(JSON) ->
    try term_to_json(JSON, [])
    %% rethrow exception so internals aren't confusingly exposed to users
    catch error:badarg -> erlang:error(badarg)
    end.


-spec term_to_json(JSON::eep0018(), Opts::encoder_opts()) -> binary().        

term_to_json(JSON, Opts) ->
    try jsx_eep0018:term_to_json(JSON, Opts)
    %% rethrow exception so internals aren't confusingly exposed to users
    catch error:badarg -> erlang:error(badarg)
    end.


-spec is_json(JSON::binary()) -> true | false.

is_json(JSON) ->
    is_json(JSON, []).
    

-spec is_json(JSON::binary(), Opts::verify_opts()) -> true | false.

is_json(JSON, Opts) ->
    jsx_verify:is_json(JSON, Opts).


-spec format(JSON::binary()) -> binary() | iolist().

format(JSON) ->
    format(JSON, []).


-spec format(JSON::binary(), Opts::format_opts()) -> binary() | iolist().

format(JSON, Opts) ->
    jsx_format:format(JSON, Opts).


-spec eventify(List::list()) -> jsx_parser_result().

eventify([]) ->
    fun() -> 
        {incomplete, fun(List) when is_list(List) -> 
                eventify(List)
            ; (_) ->
                erlang:error(badarg) 
        end}
    end;    
eventify([Next|Rest]) ->
    fun() -> {event, Next, eventify(Rest)} end.  



%% internal functions

   
%% encoding detection   
%% first check to see if there's a bom, if not, use the rfc4627 method for 
%%   determining encoding. this function makes some assumptions about the 
%%   validity of the stream which may delay failure later than if an encoding is 
%%   explicitly provided

detect_encoding(OptsList) ->
    fun(Stream) -> detect_encoding(Stream, OptsList) end.
    
%% utf8 bom detection    
detect_encoding(<<16#ef, 16#bb, 16#bf, Rest/binary>>, Opts) -> 
    (jsx_utf8:parser(Opts))(Rest);    
%% utf32-little bom detection (this has to come before utf16-little or it'll 
%%   match that)
detect_encoding(<<16#ff, 16#fe, 0, 0, Rest/binary>>, Opts) -> 
    (jsx_utf32le:parser(Opts))(Rest);        
%% utf16-big bom detection
detect_encoding(<<16#fe, 16#ff, Rest/binary>>, Opts) -> 
    (jsx_utf16:parser(Opts))(Rest);
%% utf16-little bom detection
detect_encoding(<<16#ff, 16#fe, Rest/binary>>, Opts) -> 
    (jsx_utf16le:parser(Opts))(Rest);
%% utf32-big bom detection
detect_encoding(<<0, 0, 16#fe, 16#ff, Rest/binary>>, Opts) -> 
    (jsx_utf32:parser(Opts))(Rest);
    
%% utf32-little null order detection
detect_encoding(<<X, 0, 0, 0, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    (jsx_utf32le:parser(Opts))(JSON);
%% utf32-big null order detection
detect_encoding(<<0, 0, 0, X, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    (jsx_utf32:parser(Opts))(JSON);
%% utf16-little null order detection
detect_encoding(<<X, 0, _, 0, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    (jsx_utf16le:parser(Opts))(JSON);
%% utf16-big null order detection
detect_encoding(<<0, X, 0, _, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    (jsx_utf16:parser(Opts))(JSON);
%% utf8 null order detection
detect_encoding(<<X, Y, _Rest/binary>> = JSON, Opts) when X =/= 0, Y =/= 0 ->
    (jsx_utf8:parser(Opts))(JSON);
    
%% a problem, to autodetect naked single digits' encoding, there is not enough 
%%   data to conclusively determine the encoding correctly. below is an attempt 
%%   to solve the problem
detect_encoding(<<X>>, Opts) when X =/= 0 ->
    {incomplete,
        fun(end_stream) ->
                try
                    {incomplete, Next} = (jsx_utf8:parser(Opts))(<<X>>),
                    Next(end_stream)
                    catch error:function_clause -> {error, {badjson, <<X>>}}
                end
            ; (Stream) -> detect_encoding(<<X, Stream/binary>>, Opts) 
        end
    };
detect_encoding(<<0, X>>, Opts) when X =/= 0 ->
    {incomplete,
        fun(end_stream) ->
                try
                    {incomplete, Next} = (jsx_utf16:parser(Opts))(<<0, X>>),
                    Next(end_stream)
                    catch error:function_clause -> {error, {badjson, <<0, X>>}}
                end
            ; (Stream) -> detect_encoding(<<0, X, Stream/binary>>, Opts) 
        end
    };
detect_encoding(<<X, 0>>, Opts) when X =/= 0 ->
    {incomplete,
        fun(end_stream) ->
                try
                    {incomplete, Next} = (jsx_utf16le:parser(Opts))(<<X, 0>>),
                    Next(end_stream)
                    catch error:function_clause -> {error, {badjson, <<X, 0>>}}
                end
            ; (Stream) -> detect_encoding(<<X, 0, Stream/binary>>, Opts)
        end
    };
    
%% not enough input, request more
detect_encoding(Bin, Opts) ->
    {incomplete,
        fun(end_stream) -> {error, {badjson, Bin}}
            ; (Stream) -> detect_encoding(<<Bin/binary, Stream/binary>>, Opts) 
        end
    }.
    

-ifdef(TEST).

jsx_decoder_test_() ->
    jsx_decoder_gen(load_tests(?eunit_test_path)).
    
    
jsx_decoder_gen([]) -> [];    
jsx_decoder_gen(Tests) -> 
    jsx_decoder_gen(Tests, [utf8,
        utf16,
        {utf16, little},
        utf32,
        {utf32, little}
    ]).    
    
jsx_decoder_gen([_Test|Rest], []) ->
    jsx_decoder_gen(Rest);
jsx_decoder_gen([Test|_] = Tests, [Encoding|Encodings]) ->
    Name = lists:flatten(proplists:get_value(name, Test) ++ " :: " ++
        io_lib:format("~p", [Encoding])
    ),
    JSON = unicode:characters_to_binary(proplists:get_value(json, Test),
        unicode,
        Encoding
    ),
    JSX = proplists:get_value(jsx, Test),
    Flags = proplists:get_value(jsx_flags, Test, []),
    {generator,
        fun() ->
            [{Name, ?_assert(decode(JSON, Flags) =:= JSX)} 
                | {generator, 
                        fun() -> [{Name ++ " incremental", ?_assert(
                                incremental_decode(JSON, Flags) =:= JSX)
                            } | jsx_decoder_gen(Tests, Encodings)]
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
    %% should alert about badly formed tests eventually, but for now just skip
    %% over them
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
        {"multiple values in a single stream", ?_assert(
            multi_decode(multi_json_body(), []) =:= multi_test_result()
        )}
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
    <<"0 1 -1 1e1 0.7 0.7e-1 truefalsenull {} {\"key\": \"value\"}[] [1, 2, 3]\"hope this works\"">>.

multi_test_result() ->
    [[{integer, "0"}],
        [{integer, "1"}],
        [{integer, "-1"}],
        [{float, "1.0e1"}],
        [{float, "0.7"}],
        [{float, "0.7e-1"}],
        [{literal, true}],
        [{literal, false}],
        [{literal, null}],
        [start_object, end_object],
        [start_object, {key, "key"}, {string, "value"}, end_object],
        [start_array, end_array],
        [start_array, {integer, "1"}, {integer, "2"}, {integer, "3"}, end_array],
        [{string, "hope this works"}]
    ].


    
-endif.