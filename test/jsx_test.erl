-module(jsx_test).

-export([test/0]).

-define(assert(A, B),
    case A == B of true -> ok ; false -> erlang:error(failed_assert, ?LINE) end
).

test() ->
    Strict = jsx:decoder(),
    Comments = jsx:decoder([{comments, true}]),
    Naked = jsx:decoder([{naked_values, true}]),
    NakedComments = jsx:decoder([{comments, true}, {naked_values, true}]),
    
    %% empty objects and arrays
    ?assert([start_array, end_array], Strict(<<"[]">>)),
    ?assert([start_object, end_object], Strict(<<"{}">>)),
    
    %% really deep array
    ?assert([start_array, start_array, start_array, start_array, start_array, start_array, start_array, start_array, start_array, start_array, end_array, end_array, end_array, end_array, end_array, end_array, end_array, end_array, end_array, end_array], Strict(<<"[[[[[[[[[[]]]]]]]]]]">>)),
    
    %% naked values
    ?assert([{literal, true}], Naked(<<"true">>)),
    ?assert([{literal, false}], Naked(<<"false">>)),
    ?assert([{literal, null}], Naked(<<"null">>)),
    ?assert([{string, "blah blah blah"}], Naked(<<"\"blah blah blah\"">>)),
    ?assert([{number, "1"}], Naked(<<"1">>)),
    ?assert([{number, "0.51323412"}], Naked(<<"0.51323412">>)),
    ?assert([{number, "-1.1"}], Naked(<<"-1.1">>)),
    
    %% comments
    ?assert([start_array, end_array], Comments(<<"[ /* this is ignored */ ]">>)),
    ?assert([start_object, end_object], Comments(<<"{ /* this too */ }">>)),
    
    %% strings and unicode
    ?assert([start_array, {string, "a test string"}, end_array], Strict(<<"[ \"a test string\" ]">>)),
    ?assert([start_array, {string, "a high unicode value: " ++ [100000]}, end_array], Strict(<<"[ \"a high unicode value: ", 100000/utf8, "\" ]">>)),
    ?assert([start_object, {key, "key"}, {string, "value"}, end_object], Strict(<<"{ \"key\": \"value\" }">>)),
    
    %% numbers
    ?assert([start_array, {number, "1"}, end_array], Strict(<<"[ 1 ]">>)),
    ?assert([start_array, {number, "0"}, end_array], Strict(<<"[ 0 ]">>)),
    ?assert([start_array, {number, "1e1"}, end_array], Strict(<<"[ 1e1 ]">>)),
    ?assert([start_array, {number, "-1.0e5123"}, end_array], Strict(<<"[ -1.0e5123 ]">>)),
    ?assert([start_array, {number, "324523452345256234455"}, end_array], Strict(<<"[ 324523452345256234455 ]">>)), 
    
    ok.
    