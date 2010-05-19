-module(jsx_test).

-export([test/2]).

test(TestsDir, Opts) ->
    {ok, MaybeTests} = file:list_dir(TestsDir),
    
    TestSpecs = lists:map(fun(X) -> 
            filename:basename(X, ".test") end, 
            lists:filter(fun(X) -> 
                    case filename:extension(X) of 
                        ".test" -> true
                        ; _ -> false 
                    end 
                end, 
                MaybeTests)),
      
    Results = test(TestSpecs, TestsDir, {[], [], []}),
    
    output(Results, Opts).
    
test([], _, Results) ->
    Results;    
test([Test|Rest], Dir, {Pass, Fail, Discard}) ->
    {DecoderOpts, Expected} = case file:consult(filename:nativename(Dir ++ "/" ++ Test ++ ".test")) of
        {ok, [Result]} when is_list(Result) -> {[], Result}
        ; {ok, {Opts, Result}} -> {Opts, Result}
    end,
    
    try decode(Test, Dir, DecoderOpts) of
        Object ->
            case Object == Expected of
                true -> test(Rest, Dir, {[Test] ++ Pass, Fail, Discard})
                ; false -> test(Rest, Dir, {Pass, [Test] ++ Fail, Discard})
            end
    catch
        error:function_clause ->
            test(Rest, Dir, {Pass, Fail, [Test] ++ Discard})
        ; error:enoent ->
            test(Rest, Dir, {Pass, Fail, [Test] ++ Discard})
    end.  


decode(Test, Dir, DecoderOpts) ->
    Decoder = jsx:decoder(DecoderOpts),
    {ok, JSON} = file:read_file(filename:nativename(Dir ++ "/" ++ Test ++ ".json")),
    
    Decoder(JSON).
        
    
output({Pass, Fail, Discard}, Opts) ->
    Passes = length(Pass),
    Failures = length(Fail),
    Discards = length(Discard),
    
    Total = Passes + Failures + Discards,
    
    io:format("***~p total tests run, ~p passes, ~p failures, ~p discarded~n    Failed:~n", [Total, Passes, Failures, Discards]),
    lists:foreach(fun(X) -> io:format("       ~p~n", [X]) end, Fail).
    

          
        
        
        
        