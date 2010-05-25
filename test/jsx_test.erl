-module(jsx_test).

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
                [{TestName, ?_assertEqual(decode(Decoder, JSON), Events)}]
            ; {ok, [Events, Flags]} ->
                Decoder = jsx:decoder(none, Flags),
                [{TestName, ?_assertEqual(decode(Decoder, JSON), Events)}]
        end
    catch _:_ -> []
    end.
    
decode(F, <<>>) ->
    {Result, _} = F(<<16#FDEF/utf8>>),
    Result;    
decode(F, <<A/utf8, Rest/binary>>) ->
    case F(<<A/utf8>>) of
        G when is_function(G) ->
            decode(G, Rest)
        ; {Result, _} ->
            Result
    end.
    