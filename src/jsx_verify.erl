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
    P = jsx:decoder([iterate] ++ extract_parser_opts(OptsList)),
    is_json(fun() -> P(JSON) end, OptsList);
is_json(Terms, OptsList) when is_list(Terms) ->
    P = jsx:encoder([iterate]),
    is_json(fun() -> P(Terms) end, OptsList);
is_json(F, OptsList) when is_function(F) ->
    Opts = parse_opts(OptsList, #verify_opts{}),
    case Opts#verify_opts.naked_values of
        true -> collect(F(), Opts, [[]])
        ; false ->
            case F() of
                {jsx, start_object, Next} -> collect(Next(), Opts, [[]])
                ; {jsx, start_array, Next} -> collect(Next(), Opts, [[]])
                ; _ -> false
            end
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


parse_opts([{repeated_keys, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#verify_opts{repeated_keys = Val});
parse_opts([repeated_keys|Rest], Opts) ->
    parse_opts(Rest, Opts#verify_opts{repeated_keys = true});
parse_opts([{naked_values, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#verify_opts{naked_values = Val});
parse_opts([naked_values|Rest], Opts) ->
    parse_opts(Rest, Opts#verify_opts{naked_values = true});
parse_opts([_|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts([], Opts) ->
    Opts.



collect({jsx, end_json, _Next}, _Opts, _Keys) ->
    true;


%% allocate new key accumulator at start_object, discard it at end_object    
collect({jsx, start_object, Next},
        Opts = #verify_opts{repeated_keys = false},
        Keys) ->
    collect(Next(), Opts, [[]|Keys]);
collect({jsx, end_object, Next},
        Opts = #verify_opts{repeated_keys = false},
        [_|Keys]) ->
    collect(Next(), Opts, [Keys]);


%% check to see if key has already been encountered, if not add it to the key 
%%   accumulator and continue, else return false 
collect({jsx, {key, Key}, Next},
        Opts = #verify_opts{repeated_keys = false},
        [Current|Keys]) ->
    case lists:member(Key, Current) of
        true -> false
        ; false -> collect(Next(), Opts, [[Key] ++ Current] ++ Keys)
    end;


%% needed to parse numbers that don't have trailing whitespace in less strict 
%%   mode    
collect({jsx, incomplete, More}, Opts, Keys) ->
    collect(More(end_stream), Opts, Keys);

                
collect({jsx, _, Next}, Opts, Keys) ->
    collect(Next(), Opts, Keys);

    
collect(_, _, _) ->
    false.
    
    

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
        }
    ].

false_test_() ->
    [
        {"naked true", ?_assert(is_json(<<"true">>, []) =:= true)},
        {"naked number", ?_assert(is_json(<<"1">>, []) =:= true)},
        {"naked string", 
            ?_assert(is_json(<<"\"i am not really json\"">>, []) =:= true)
        },
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

    
    
