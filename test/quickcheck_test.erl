-module(quickcheck_test).
-compile({parse_transform, ct_expand}).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

word_list() ->
    {ok, Data} = file:read_file("/usr/share/dict/american-english"),
    List       = binary:split(Data, <<"\n">>, [global]),
    List.

word() ->    
    oneof(ct_expand:term(word_list())).

json_value() ->
   oneof([null, 
          bool(), 
          int(), 
          real(),  
          word()
         ]).

json(0) -> json_value();
json(Size) ->
    ?LAZY(
       oneof([json_value(),
              list(json(Size - 1 )),
              list({word(),json(Size  -1 )})])).

json() ->
    ?SIZED(Size, 
           begin
               json(Size)
           end
                   ).


prop_json_encode() ->

    application:ensure_all_started(erlang_js),
    {ok, JS} = js_driver:new(),
    ?FORALL(JSON_TERM,
            json(),
            begin
                JSON = jsx:encode(JSON_TERM),
                case js:call(JS, <<"JSON.parse">>, [JSON]) of
                    {ok,_} ->
                        jsx:is_json(JSON);
                    {error, _} ->
                        not(jsx:is_json(JSON))
                end
            end).
                    
prop_json_identity() ->
    ?FORALL(JSON_TERM,
            json(),
            begin
                JSON = jsx:encode(JSON_TERM),
                true = jsx:is_json(JSON),
                JSON_TERM =:= jsx:decode(JSON)
            end).
format(JSON,[]) ->
    JSON;
format(JSON, [Op|Rest]) ->

    format(jsx:Op(JSON),Rest).

prop_format() ->
    application:ensure_all_started(erlang_js),
    {ok, JS} = js_driver:new(),

    ?FORALL({JSON_TERM, Operations},
            {json(), list(oneof([minify,prettify]))},
            begin
                JSON      = jsx:encode(JSON_TERM),
                Result    = format(JSON, Operations),
                case js:call(JS, <<"JSON.parse">>, [Result]) of
                    {ok,_} ->
                        jsx:is_json(Result) andalso                 
                            JSON_TERM =:= jsx:decode(Result);
                    {error, _} ->
                        false
                end 

            end).

run_prop_test_() ->
    Props = [prop_json_identity(), prop_json_encode(),prop_format()],
    {timeout,3600,
     [?_assert(quickcheck(P,[{max_size, 5},
                             {spec_timeout, infinity},
                             {to_file,user},
                             250]))
      || P <- Props]

    }.
