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


-module(jsx_scanner).

-export([scanner/1]).

-include("../include/jsx_types.hrl").

-spec scanner(Opts::jsx_opts()) -> jsx_scanner().
scanner(Opts) ->
    fun(JSON) -> start(JSON, [], [], parse_opts(Opts)) end.

-include("../include/jsx_opts.hrl").

-include("../include/jsx_scanner.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


noncharacters_test_() ->
    [
        {"noncharacters - badjson",
            ?_assertEqual(check_bad(noncharacters()), [])
        },
        {"noncharacters - replaced",
            ?_assertEqual(check_replaced(noncharacters()), [])
        }
    ].

extended_noncharacters_test_() ->
    [
        {"extended noncharacters - badjson",
            ?_assertEqual(check_bad(extended_noncharacters()), [])
        },
        {"extended noncharacters - replaced",
            ?_assertEqual(check_replaced(extended_noncharacters()), [])
        }
    ].

surrogates_test_() ->
    [
        {"surrogates - badjson",
            ?_assertEqual(check_bad(surrogates()), [])
        },
        {"surrogates - replaced",
            ?_assertEqual(check_replaced(surrogates()), [])
        }
    ].

control_test_() ->
    [
        {"control characters - badjson",
            ?_assertEqual(check_bad(control_characters()), [])
        }
    ].

reserved_test_() ->
    [
        {"reserved noncharacters - badjson",
            ?_assertEqual(check_bad(reserved_space()), [])
        },
        {"reserved noncharacters - replaced",
            ?_assertEqual(check_replaced(reserved_space()), [])
        }
    ].

zero_test_() ->
    [
        {"nullbyte - badjson",
            ?_assertEqual(check_bad(zero()), [])
        }
    ].
    
good_characters_test_() ->
    [
        {"acceptable codepoints",
            ?_assertEqual(check_good(good()), [])
        },
        {"acceptable extended",
            ?_assertEqual(check_good(good_extended()), [])
        }
    ].
    

check_bad(List) ->
    lists:dropwhile(fun({_, {error, badjson}}) -> true ; (_) -> false end,
        check(List, [], [])
    ).

check_replaced(List) ->
    lists:dropwhile(fun({_, [{string, [16#fffd]}|_]}) ->
                true
            ; (_) -> 
                false 
        end,
        check(List, [loose_unicode], [])
    ).

check_good(List) ->
    lists:dropwhile(fun({_, [{string, _}]}) -> true ; (_) -> false end,
        check(List, [], [])
    ).

check([], _Opts, Acc) -> Acc;
check([H|T], Opts, Acc) ->
    R = decode(to_fake_utf(H, utf8), Opts),
    check(T, Opts, [{H, R}] ++ Acc).


decode(JSON, Opts) ->
    try
        {ok, Events, _} = (scanner(Opts))(JSON),
        loop(Events, [])
    catch
        error:badarg -> {error, badjson}
    end.


loop([end_json], Acc) -> lists:reverse(Acc);
loop([Event|Events], Acc) -> loop(Events, [Event] ++ Acc);
loop(_, _) -> {error, badjson}.
    


noncharacters() -> lists:seq(16#fffe, 16#ffff).
    
extended_noncharacters() ->
    [16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff]
        ++ [16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff]
        ++ [16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff]
        ++ [16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff]
        ++ [16#9fffe, 16#9ffff, 16#afffe, 16#affff]
        ++ [16#bfffe, 16#bffff, 16#cfffe, 16#cffff]
        ++ [16#dfffe, 16#dffff, 16#efffe, 16#effff]
        ++ [16#ffffe, 16#fffff, 16#10fffe, 16#10ffff].

surrogates() -> lists:seq(16#d800, 16#dfff).

control_characters() -> lists:seq(1, 31).

reserved_space() -> lists:seq(16#fdd0, 16#fdef).

zero() -> [0].

good() -> [32, 33]
            ++ lists:seq(16#23, 16#5b)
            ++ lists:seq(16#5d, 16#d7ff)
            ++ lists:seq(16#e000, 16#fdcf)
            ++ lists:seq(16#fdf0, 16#fffd).
            
good_extended() -> lists:seq(16#100000, 16#10fffd).

%% erlang refuses to encode certain codepoints, so fake them all
to_fake_utf(N, utf8) when N < 16#0080 -> <<34/utf8, N:8, 34/utf8>>;
to_fake_utf(N, utf8) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<34/utf8, 2#110:3, Y:5, 2#10:2, X:6, 34/utf8>>; 
to_fake_utf(N, utf8) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<34/utf8, 2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6, 34/utf8>>;
to_fake_utf(N, utf8) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<34/utf8, 2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6, 34/utf8>>.


-endif.