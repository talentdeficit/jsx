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


-module(jsx_verify).

-export([is_json/2]).

-include("jsx_common.hrl").
-include("jsx_verify.hrl").

    
-spec is_json(JSON::binary(), Opts::verify_opts()) -> true | false
        ; (Terms::list(jsx_encodeable()), Opts::verify_opts()) -> true | false
        ; (F::jsx_iterator(), Opts::verify_opts()) -> true | false.
    
is_json(JSON, OptsList) when is_binary(JSON) ->
        F = jsx:decoder(extract_parser_opts(OptsList)),
        verify(F(JSON), parse_opts(OptsList));
is_json(JSON, OptsList) when is_list(JSON) ->
    try
        F = jsx:encoder(extract_parser_opts(OptsList)),
        verify(F(JSON), parse_opts(OptsList))
    catch
        _:_ -> erlang:error(badarg)
    end;
is_json(F, OptsList) when is_function(F) ->
    try
        verify(jsx_utils:collect(F), parse_opts(OptsList))
    catch
        _:_ -> erlang:error(badarg)
    end.


extract_parser_opts(Opts) ->
    extract_parser_opts(Opts, []).

extract_parser_opts([], Acc) -> Acc;    
extract_parser_opts([{K,V}|Rest], Acc) ->
    case lists:member(K, [encoding]) of
        true -> [{K,V}] ++ Acc
        ; false -> extract_parser_opts(Rest, Acc)
    end;
extract_parser_opts([K|Rest], Acc) ->
    case lists:member(K, [encoding]) of
        true -> [K] ++ Acc
        ; false -> extract_parser_opts(Rest, Acc)
    end.


parse_opts(Opts) -> parse_opts(Opts, #verify_opts{}).

parse_opts([{repeated_keys, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#verify_opts{repeated_keys = Val});
parse_opts([repeated_keys|Rest], Opts) ->
    parse_opts(Rest, Opts#verify_opts{repeated_keys = true});
parse_opts([{naked_values, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#verify_opts{naked_values = Val});
parse_opts([naked_values|Rest], Opts) ->
    parse_opts(Rest, Opts#verify_opts{naked_values = true});
parse_opts([{encoding, _}|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts([encoding|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts([], Opts) ->
    Opts.


verify({error, {badjson, _}}, _Opts) -> false;
verify({jsx, incomplete, More}, Opts) -> verify(More(end_stream), Opts);
verify({jsx, [First|Rest], _}, Opts=#verify_opts{naked_values=false}) ->
    case First of
         start_object -> verify(Rest, Opts, [])
         ; start_array -> verify(Rest, Opts, [])
         ; _ -> false
    end;
verify({jsx, Terms, _}, Opts) -> verify(Terms, Opts, []).

verify([end_json], _Opts, _Keys) -> true;

%% allocate new key accumulator at start_object, discard it at end_object    
verify([start_object|Rest], Opts=#verify_opts{repeated_keys=false}, Keys) ->
    verify(Rest, Opts, [[]] ++ Keys);
verify([end_object|Rest], Opts=#verify_opts{repeated_keys=false}, [_|Keys]) ->
    verify(Rest, Opts, Keys);

%% check to see if key has already been encountered, if not add it to the key 
%%   accumulator and continue, else return false 
verify([{key, Key}|Rest], Opts=#verify_opts{repeated_keys=false}, [Current|Keys]) ->
    case lists:member(Key, Current) of
        true -> false
        ; false -> verify(Rest, Opts, [[Key] ++ Current] ++ Keys)
    end;
                
verify([_|Rest], Opts, Keys) -> verify(Rest, Opts, Keys);

verify(_, _, _) -> false.
    
    

%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

true_test_() ->
    [
        {"empty object", ?_assert(is_json(<<"{}">>, []) =:= true)},
        {"empty array", ?_assert(is_json(<<"[]">>, []) =:= true)},
        {"whitespace", 
            ?_assert(is_json(<<" \n    \t   \r   [true]   \t    \n\r  ">>, 
                    []
                ) =:= true
            )
        },
        {"nested terms", 
            ?_assert(is_json(
                    <<"[{ \"x\": [ {}, {}, {} ], \"y\": [{}] }, {}, [[[]]]]">>, 
                    []
                ) =:= true
            )
        },
        {"numbers", 
            ?_assert(is_json(
                    <<"[ -1.0, -1, -0, 0, 1e-1, 1, 1.0, 1e1 ]">>, 
                    []
                ) =:= true
            )
        },
        {"strings", 
            ?_assert(is_json(
                    <<"[ \"a\", \"string\", \"in\", \"multiple\", \"acts\" ]">>, 
                    []
                ) =:= true
            )
        },
        {"literals", 
            ?_assert(is_json(<<"[ true, false, null ]">>, []) =:= true)
        },
        {"nested objects", 
            ?_assert(is_json(<<"{\"key\": { \"key\": true}}">>, []) =:= true)
        },
        {"naked true", ?_assert(is_json(<<"true">>, []) =:= true)},
        {"naked number", ?_assert(is_json(<<"1">>, []) =:= true)},
        {"naked string", 
            ?_assert(is_json(<<"\"i am not really json\"">>, []) =:= true)
        }
    ].

false_test_() ->
    [
        {"unbalanced list", ?_assert(is_json(<<"[[[]]">>, []) =:= false)},
        {"trailing comma", 
            ?_assert(is_json(<<"[ true, false, null, ]">>, []) =:= false)
        }
    ].

repeated_keys_test_() ->
    [
        {"repeated key forbidden", 
            ?_assert(is_json(
                    <<"{\"key\": true, \"key\": true}">>, 
                    [{repeated_keys, false}]
                ) =:= false
            )
        },
        {"repeated key allowed", 
            ?_assert(is_json(
                    <<"{\"key\": true, \"key\": true}">>, 
                    [{repeated_keys, true}]
                ) =:= true
            )
        },
        {"repeated key nested",
            ?_assert(is_json(
                    <<"{\"a\": {\"a\": {\"a\": true, \"a\":false}}}">>,
                    [{repeated_keys, false}]
                ) =:= false
            )
        }
    ].
    
naked_value_test_() ->
    [
        {"naked true", 
            ?_assert(is_json(<<"true">>, []) =:= true)
        },
        {"naked number", 
            ?_assert(is_json(<<"1">>, []) =:= true)
        },
        {"naked string", 
            ?_assert(is_json(<<"\"i am not json\"">>, []) =:= true)
        },
        {"naked true", 
            ?_assert(is_json(<<"true">>, [{naked_values, false}]) =:= false)
        },
        {"naked number", 
            ?_assert(is_json(<<"1">>, [{naked_values, false}]) =:= false)
        },
        {"naked string", 
            ?_assert(is_json(
                <<"\"i am not json\"">>, 
                [{naked_values, false}]
                ) =:= false
            )
        }
    ].
    
terms_test_() ->
    [
        {"terms",
            ?_assert(is_json([start_object,
                {key, <<"key">>},
                {string, <<"value">>},
                end_object
            ], []) =:= true
        )}
    ].   
    
-endif.