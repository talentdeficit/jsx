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
-export([init/1, handle_event/2]).


-record(opts, {
    repeated_keys = true
}).

-type opts() :: [].


-spec is_json(Source::(binary() | list()), Opts::opts()) -> binary().
    
is_json(Source, Opts) when (is_binary(Source) andalso is_list(Opts))
        orelse (is_list(Source) andalso is_list(Opts)) ->
    try (gen_json:parser(?MODULE, Opts, jsx_utils:extract_opts(Opts)))(Source)
    catch error:badarg -> false
    end.



parse_opts(Opts) -> parse_opts(Opts, #opts{}).

parse_opts([{repeated_keys, Val}|Rest], Opts) when Val == true; Val == false ->
    parse_opts(Rest, Opts#opts{repeated_keys = Val});
parse_opts([repeated_keys|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{repeated_keys = true});
parse_opts([_|Rest], Opts) ->
    parse_opts(Rest, Opts);
parse_opts([], Opts) ->
    Opts.



init(Opts) -> {parse_opts(Opts), []}.



handle_event(end_json, _) -> true;

handle_event(_, {Opts, _} = State) when Opts#opts.repeated_keys == true -> State;

handle_event(start_object, {Opts, Keys}) -> {Opts, [dict:new()] ++ Keys};
handle_event(end_object, {Opts, [_|Keys]}) -> {Opts, Keys};

handle_event({key, Key}, {Opts, [CurrentKeys|Keys]}) ->
    case dict:is_key(Key, CurrentKeys) of
        true -> erlang:error(badarg)
        ; false -> {Opts, [dict:store(Key, blah, CurrentKeys)|Keys]}
    end;

handle_event(_, State) -> State.



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
        {"repeated key ok", ?_assert(is_json(<<"{\"key\": true, \"key\": true}">>, []) =:= true)},
        {"nested repeated key", ?_assert(is_json(<<"{\"key\": { \"key\": true }}">>,
            [{repeated_keys, false}]) =:= true)
        }
    ].

false_test_() ->
    [
        {"unbalanced list", ?_assert(is_json(<<"[]]">>, []) =:= false)},
        {"trailing comma", 
            ?_assert(is_json(<<"[ true, false, null, ]">>, []) =:= false)
        },
        {"unquoted key", ?_assert(is_json(<<"{ key: false }">>, []) =:= false)},
        {"repeated key", ?_assert(is_json(<<"{\"key\": true, \"key\": true}">>,
            [{repeated_keys, false}]) =:= false)
        },
        {"nested repeated key", ?_assert(is_json(<<"{\"key\": { \"a\": true, \"a\": false }}">>,
            [{repeated_keys, false}]) =:= false)
        }
    ].

incomplete_test_() ->
    [
        {"incomplete test", ?_assertMatch({incomplete, _}, is_json(<<"[">>, []))}
    ].
        
    
-endif.