%% data and helper functions for tests

-export([init/1, handle_event/2]).
-export([test_cases/0]).


-include_lib("eunit/include/eunit.hrl").


%% test handler
init([]) -> [].

handle_event(end_json, State) -> lists:reverse([end_json] ++ State);
handle_event(Event, State) -> [Event] ++ State.


test_cases() ->
    empty_array()
    ++ nested_array()
    ++ empty_object()
    ++ nested_object()
    ++ strings()
    ++ literals()
    ++ integers()
    ++ floats()
    ++ compound_object().


empty_array() -> [{"[]", <<"[]">>, [], [start_array, end_array]}].

nested_array() ->
    [{
        "[[[]]]",
        <<"[[[]]]">>,
        [[[]]],
        [start_array, start_array, start_array, end_array, end_array, end_array]
    }].


empty_object() -> [{"{}", <<"{}">>, [{}], [start_object, end_object]}].

nested_object() ->
    [{
        "{\"key\":{\"key\":{}}}",
        <<"{\"key\":{\"key\":{}}}">>,
        [{<<"key">>, [{<<"key">>, [{}]}]}],
        [
            start_object,
                {key, <<"key">>},
                start_object,
                    {key, <<"key">>},
                    start_object,
                    end_object,
                end_object,
            end_object
        ]
    }].


naked_strings() ->
    Raw = [
        "",
        "hello world"
    ],
    [
        {
            String,
            <<"\"", (list_to_binary(String))/binary, "\"">>,
            list_to_binary(String),
            [{string, list_to_binary(String)}]
        }
        || String <- Raw
    ].

strings() ->
    naked_strings()
    ++ [ wrap_with_array(Test) || Test <- naked_strings() ]
    ++ [ wrap_with_object(Test) || Test <- naked_strings() ].


naked_integers() ->
    Raw = [
        1, 2, 3,
        127, 128, 129,
        255, 256, 257,
        65534, 65535, 65536,
        18446744073709551616,
        18446744073709551617
    ],
    [
        {
            integer_to_list(X),
            list_to_binary(integer_to_list(X)),
            X,
            [{integer, X}]
        }
        || X <- Raw ++ [ -1 * Y || Y <- Raw ] ++ [0]
    ].

integers() ->
    naked_integers()
    ++ [ wrap_with_array(Test) || Test <- naked_integers() ]
    ++ [ wrap_with_object(Test) || Test <- naked_integers() ].


naked_floats() ->
    Raw = [
        0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
        1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9,
        1234567890.0987654321,
        0.0e0,
        1234567890.0987654321e16,
        0.1e0, 0.1e1, 0.1e2, 0.1e4, 0.1e8, 0.1e16, 0.1e308,
        1.0e0, 1.0e1, 1.0e2, 1.0e4, 1.0e8, 1.0e16, 1.0e308,
        2.2250738585072014e-308,    %% min normalized float
        1.7976931348623157e308,     %% max normalized float
        5.0e-324,                   %% min denormalized float
        2.225073858507201e-308      %% max denormalized float
    ],
    [
        {
            sane_float_to_list(X),
            list_to_binary(sane_float_to_list(X)),
            X,
            [{float, X}]
        }
        || X <- Raw ++ [ -1 * Y || Y <- Raw ]
    ].

floats() ->
    naked_floats()
    ++ [ wrap_with_array(Test) || Test <- naked_floats() ]
    ++ [ wrap_with_object(Test) || Test <- naked_floats() ].


naked_literals() ->
    [
        {
            atom_to_list(Literal),
            atom_to_binary(Literal, unicode),
            Literal,
            [{literal, Literal}]
        }
        || Literal <- [true, false, null]
    ].

literals() ->
    naked_literals()
    ++ [ wrap_with_array(Test) || Test <- naked_literals() ]
    ++ [ wrap_with_object(Test) || Test <- naked_literals() ].


compound_object() ->
    [{
        "[{\"alpha\":[1,2,3],\"beta\":{\"alpha\":[1.0,2.0,3.0],\"beta\":[true,false]}},[{}]]",
        <<"[{\"alpha\":[1,2,3],\"beta\":{\"alpha\":[1.0,2.0,3.0],\"beta\":[true,false]}},[{}]]">>,
        [[{<<"alpha">>, [1, 2, 3]}, {<<"beta">>, [{<<"alpha">>, [1.0, 2.0, 3.0]}, {<<"beta">>, [true, false]}]}], [[{}]]],
        [
            start_array,
                start_object,
                    {key, <<"alpha">>},
                    start_array,
                        {integer, 1},
                        {integer, 2},
                        {integer, 3},
                    end_array,
                    {key, <<"beta">>},
                    start_object,
                        {key, <<"alpha">>},
                        start_array,
                            {float, 1.0},
                            {float, 2.0},
                            {float, 3.0},
                        end_array,
                        {key, <<"beta">>},
                        start_array,
                            {literal, true},
                            {literal, false},
                        end_array,
                    end_object,
                end_object,
                start_array,
                    start_object,
                    end_object,
                end_array,
            end_array
        ]
    }].


wrap_with_array({Title, JSON, Term, Events}) ->
    {
        "[" ++ Title ++ "]",
        <<"[", JSON/binary, "]">>,
        [Term],
        [start_array] ++ Events ++ [end_array]
    }.


wrap_with_object({Title, JSON, Term, Events}) ->
    {
        "{\"key\":" ++ Title ++ "}",
        <<"{\"key\":", JSON/binary, "}">>,
        [{<<"key">>, Term}],
        [start_object, {key, <<"key">>}] ++ Events ++ [end_object]
    }.


sane_float_to_list(X) ->
    [Output] = io_lib:format("~p", [X]),
    Output.

-include("jsx_config.hrl").
-include("jsx_strings.hrl").


%% erlang refuses to encode certain codepoints, so fake them
to_fake_utf8(N) when N < 16#0080 -> <<N:8>>;
to_fake_utf8(N) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<2#110:3, Y:5, 2#10:2, X:6>>;
to_fake_utf8(N) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6>>;
to_fake_utf8(N) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6>>.


codepoints() ->
    unicode:characters_to_binary(
        [32, 33]
        ++ lists:seq(35, 46)
        ++ lists:seq(48, 91)
        ++ lists:seq(93, 16#2027)
        ++ lists:seq(16#202a, 16#d7ff)
        ++ lists:seq(16#e000, 16#fdcf)
        ++ lists:seq(16#fdf0, 16#fffd)
    ).

extended_codepoints() ->
    unicode:characters_to_binary(
        lists:seq(16#10000, 16#1fffd) ++ [
            16#20000, 16#30000, 16#40000, 16#50000, 16#60000,
            16#70000, 16#80000, 16#90000, 16#a0000, 16#b0000,
            16#c0000, 16#d0000, 16#e0000, 16#f0000, 16#100000
        ]
    ).

reserved_space() -> [ to_fake_utf8(N) || N <- lists:seq(16#fdd0, 16#fdef) ].

surrogates() -> [ to_fake_utf8(N) || N <- lists:seq(16#d800, 16#dfff) ].

noncharacters() -> [ to_fake_utf8(N) || N <- lists:seq(16#fffe, 16#ffff) ].

extended_noncharacters() ->
    [ to_fake_utf8(N) || N <- [16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff]
        ++ [16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff]
        ++ [16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff]
        ++ [16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff]
        ++ [16#9fffe, 16#9ffff, 16#afffe, 16#affff]
        ++ [16#bfffe, 16#bffff, 16#cfffe, 16#cffff]
        ++ [16#dfffe, 16#dffff, 16#efffe, 16#effff]
        ++ [16#ffffe, 16#fffff, 16#10fffe, 16#10ffff]
    ].


clean_string_test_() ->
    [
        {"clean codepoints", ?_assertEqual(
            codepoints(),
            clean_string(codepoints(), #config{})
        )},
        {"clean extended codepoints", ?_assertEqual(
            extended_codepoints(),
            clean_string(extended_codepoints(), #config{})
        )},
        {"escape path codepoints", ?_assertEqual(
            codepoints(),
            clean_string(codepoints(), #config{escaped_strings=true})
        )},
        {"escape path extended codepoints", ?_assertEqual(
            extended_codepoints(),
            clean_string(extended_codepoints(), #config{escaped_strings=true})
        )},
        {"error reserved space", ?_assertEqual(
            lists:duplicate(length(reserved_space()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, reserved_space())
        )},
        {"error surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, surrogates())
        )},
        {"error noncharacters", ?_assertEqual(
            lists:duplicate(length(noncharacters()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, noncharacters())
        )},
        {"error extended noncharacters", ?_assertEqual(
            lists:duplicate(length(extended_noncharacters()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, extended_noncharacters())
        )},
        {"clean reserved space", ?_assertEqual(
            lists:duplicate(length(reserved_space()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{replaced_bad_utf8=true}) end, reserved_space())
        )},
        {"clean surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{replaced_bad_utf8=true}) end, surrogates())
        )},
        {"clean noncharacters", ?_assertEqual(
            lists:duplicate(length(noncharacters()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{replaced_bad_utf8=true}) end, noncharacters())
        )},
        {"clean extended noncharacters", ?_assertEqual(
            lists:duplicate(length(extended_noncharacters()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{replaced_bad_utf8=true}) end, extended_noncharacters())
        )}
    ].


maybe_escape(Bin, Config) -> clean_string(Bin, Config).

escape_test_() ->
    [
        {"maybe_escape backspace", ?_assertEqual(
            <<"\\b">>,
            maybe_escape(<<16#0008/utf8>>, #config{escaped_strings=true})
        )},
        {"don't escape backspace", ?_assertEqual(
            <<"\b">>,
            maybe_escape(<<16#0008/utf8>>, #config{})
        )},
        {"maybe_escape tab", ?_assertEqual(
            <<"\\t">>,
            maybe_escape(<<16#0009/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape newline", ?_assertEqual(
            <<"\\n">>,
            maybe_escape(<<16#000a/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape formfeed", ?_assertEqual(
            <<"\\f">>,
            maybe_escape(<<16#000c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape carriage return", ?_assertEqual(
            <<"\\r">>,
            maybe_escape(<<16#000d/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape quote", ?_assertEqual(
            <<"\\\"">>,
            maybe_escape(<<16#0022/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape forward slash", ?_assertEqual(
            <<"\\/">>,
            maybe_escape(<<16#002f/utf8>>, #config{escaped_strings=true, escaped_forward_slashes=true})
        )},
        {"do not maybe_escape forward slash", ?_assertEqual(
            <<"/">>,
            maybe_escape(<<16#002f/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape backslash", ?_assertEqual(
            <<"\\\\">>,
            maybe_escape(<<16#005c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape jsonp (u2028)", ?_assertEqual(
            <<"\\u2028">>,
            maybe_escape(<<16#2028/utf8>>, #config{escaped_strings=true})
        )},
        {"do not maybe_escape jsonp (u2028)", ?_assertEqual(
            <<16#2028/utf8>>,
            maybe_escape(<<16#2028/utf8>>, #config{escaped_strings=true, unescaped_jsonp=true})
        )},
        {"maybe_escape jsonp (u2029)", ?_assertEqual(
            <<"\\u2029">>,
            maybe_escape(<<16#2029/utf8>>, #config{escaped_strings=true})
        )},
        {"do not maybe_escape jsonp (u2029)", ?_assertEqual(
            <<16#2029/utf8>>,
            maybe_escape(<<16#2029/utf8>>, #config{escaped_strings=true, unescaped_jsonp=true})
        )},
        {"maybe_escape u0000", ?_assertEqual(
            <<"\\u0000">>,
            maybe_escape(<<16#0000/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0001", ?_assertEqual(
            <<"\\u0001">>,
            maybe_escape(<<16#0001/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0002", ?_assertEqual(
            <<"\\u0002">>,
            maybe_escape(<<16#0002/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0003", ?_assertEqual(
            <<"\\u0003">>,
            maybe_escape(<<16#0003/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0004", ?_assertEqual(
            <<"\\u0004">>,
            maybe_escape(<<16#0004/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0005", ?_assertEqual(
            <<"\\u0005">>,
            maybe_escape(<<16#0005/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0006", ?_assertEqual(
            <<"\\u0006">>,
            maybe_escape(<<16#0006/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0007", ?_assertEqual(
            <<"\\u0007">>,
            maybe_escape(<<16#0007/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000b", ?_assertEqual(
            <<"\\u000b">>,
            maybe_escape(<<16#000b/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000e", ?_assertEqual(
            <<"\\u000e">>,
            maybe_escape(<<16#000e/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000f", ?_assertEqual(
            <<"\\u000f">>,
            maybe_escape(<<16#000f/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0010", ?_assertEqual(
            <<"\\u0010">>,
            maybe_escape(<<16#0010/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0011", ?_assertEqual(
            <<"\\u0011">>,
            maybe_escape(<<16#0011/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0012", ?_assertEqual(
            <<"\\u0012">>,
            maybe_escape(<<16#0012/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0013", ?_assertEqual(
            <<"\\u0013">>,
            maybe_escape(<<16#0013/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0014", ?_assertEqual(
            <<"\\u0014">>,
            maybe_escape(<<16#0014/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0015", ?_assertEqual(
            <<"\\u0015">>,
            maybe_escape(<<16#0015/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0016", ?_assertEqual(
            <<"\\u0016">>,
            maybe_escape(<<16#0016/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0017", ?_assertEqual(
            <<"\\u0017">>,
            maybe_escape(<<16#0017/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0018", ?_assertEqual(
            <<"\\u0018">>,
            maybe_escape(<<16#0018/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0019", ?_assertEqual(
            <<"\\u0019">>,
            maybe_escape(<<16#0019/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001a", ?_assertEqual(
            <<"\\u001a">>,
            maybe_escape(<<16#001a/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001b", ?_assertEqual(
            <<"\\u001b">>,
            maybe_escape(<<16#001b/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001c", ?_assertEqual(
            <<"\\u001c">>,
            maybe_escape(<<16#001c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001d", ?_assertEqual(
            <<"\\u001d">>,
            maybe_escape(<<16#001d/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001e", ?_assertEqual(
            <<"\\u001e">>,
            maybe_escape(<<16#001e/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001f", ?_assertEqual(
            <<"\\u001f">>,
            maybe_escape(<<16#001f/utf8>>, #config{escaped_strings=true})
        )}
    ].


bad_utf8_test_() ->
    [
        {"noncharacter u+fffe", ?_assertEqual(
            {error, badarg},
            clean_string(to_fake_utf8(16#fffe), #config{})
        )},
        {"noncharacter u+fffe replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(to_fake_utf8(16#fffe), #config{replaced_bad_utf8=true})
        )},
        {"noncharacter u+ffff", ?_assertEqual(
            {error, badarg},
            clean_string(to_fake_utf8(16#ffff), #config{})
        )},
        {"noncharacter u+ffff replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(to_fake_utf8(16#ffff), #config{replaced_bad_utf8=true})
        )},
        {"orphan continuation byte u+0080", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#0080>>, #config{})
        )},
        {"orphan continuation byte u+0080 replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#0080>>, #config{replaced_bad_utf8=true})
        )},
        {"orphan continuation byte u+00bf", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00bf>>, #config{})
        )},
        {"orphan continuation byte u+00bf replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00bf>>, #config{replaced_bad_utf8=true})
        )},
        {"2 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #config{})
        )},
        {"2 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 2),
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"3 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #config{})
        )},
        {"3 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 3),
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"4 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #config{})
        )},
        {"4 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 4),
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"5 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #config{})
        )},
        {"5 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 5),
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"6 continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #config{})
        )},
        {"6 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 6),
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"all continuation bytes", ?_assertEqual(
            {error, badarg},
            clean_string(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, #config{})
        )},
        {"all continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf))),
            clean_string(
                <<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>,
                #config{replaced_bad_utf8=true}
            )
        )},
        {"lonely start byte", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00c0>>, #config{})
        )},
        {"lonely start byte replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00c0>>, #config{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (2 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00c0, 32, 16#00df>>, #config{})
        )},
        {"lonely start bytes (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00c0, 32, 16#00df>>, #config{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (3 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00e0, 32, 16#00ef>>, #config{})
        )},
        {"lonely start bytes (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00e0, 32, 16#00ef>>, #config{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (4 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#00f0, 32, 16#00f7>>, #config{})
        )},
        {"lonely start bytes (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00f0, 32, 16#00f7>>, #config{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (3 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<224, 160, 32>>, #config{})
        )},
        {"missing continuation byte (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<224, 160, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (4 byte missing one)", ?_assertEqual(
            {error, badarg},
            clean_string(<<240, 144, 128, 32>>, #config{})
        )},
        {"missing continuation byte (4 byte missing one) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 128, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (4 byte missing two)", ?_assertEqual(
            {error, badarg},
            clean_string(<<240, 144, 32>>, #config{})
        )},
        {"missing continuation byte (4 byte missing two) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (2 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#c0, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c0, 16#af, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (3 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (4 byte)", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"highest overlong 2 byte sequence", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#c1, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 2 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c1, 16#bf, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"highest overlong 3 byte sequence", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 3 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"highest overlong 4 byte sequence", ?_assertEqual(
            {error, badarg},
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 4 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #config{replaced_bad_utf8=true})
        )}
    ].


json_escape_sequence_test_() ->
    [
        {"json escape sequence test - 16#0000", ?_assertEqual(json_escape_sequence(16#0000), "\\u0000")},
        {"json escape sequence test - 16#abc", ?_assertEqual(json_escape_sequence(16#abc), "\\u0abc")},
        {"json escape sequence test - 16#def", ?_assertEqual(json_escape_sequence(16#def), "\\u0def")}
    ].