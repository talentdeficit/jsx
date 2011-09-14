%% The MIT License

%% Copyright (c) 2011 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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


-module(jsx_tokenizer).


-include("../include/jsx_types.hrl").


-export([tokenizer/1]).


-spec tokenizer(OptsList::jsx_opts()) -> jsx_tokenizer().
tokenizer(OptsList) ->
    fun(Forms) -> start(Forms, [], [], parse_opts(OptsList)) end.

-include("../include/jsx_opts.hrl").

-include("../include/jsx_opts_parser.hrl").

-include("../include/jsx_tokenizer.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode(Terms) ->    
    try case (jsx:scanner([]))(Terms) of
            {ok, Terms, _} ->
                true
            %% matches [foo, end_json], aka naked terms
            ; {ok, [Terms, end_json], _} ->
                true
        end
    catch
        error:badarg -> false
    end.


encode_test_() ->    
    [
        {"empty object", ?_assert(encode([start_object, end_object, end_json]))},
        {"empty array", ?_assert(encode([start_array, end_array, end_json]))},
        {"nested empty objects", ?_assert(encode([start_object,
            {key, "empty object"},
            start_object,
            {key, "empty object"},
            start_object,
            end_object,
            end_object,
            end_object,
            end_json
        ]))},
        {"nested empty arrays", ?_assert(encode([start_array,
            start_array,
            start_array,
            end_array,
            end_array,
            end_array,
            end_json
        ]))},
        {"simple object", ?_assert(encode([start_object, 
            {key, "a"},
            {string, "hello"},
            {key, "b"},
            {integer, 1},
            {key, "c"},
            {float, 1.0},
            {key, "d"},
            {literal, true},
            end_object,
            end_json
        ]))},
        {"simple array", ?_assert(encode([start_array,
            {string, "hello"},
            {integer, 1},
            {float, 1.0},
            {literal, true},
            end_array,
            end_json
        ]))},
        {"unbalanced array", ?_assertNot(encode([start_array,
            end_array,
            end_array,
            end_json
        ]))},
        {"naked string", ?_assert(encode({string, "hello"}))},
        {"naked literal", ?_assert(encode({literal, true}))},
        {"naked integer", ?_assert(encode({integer, 1}))},
        {"naked float", ?_assert(encode({float, 1.0}))}
    ].

-endif.