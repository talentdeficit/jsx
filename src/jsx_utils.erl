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


-module(jsx_utils).

-export([parse_config/1]).
-export([extract_config/1, valid_flags/0]).
-export([json_escape_sequence/1]).
-export([clean_string/2]).

-include("jsx_config.hrl").


%% parsing of jsx config
parse_config(Config) ->
    parse_config(Config, #config{}).

parse_config([], Config) ->
    Config;
parse_config([replaced_bad_utf8|Rest], Config) ->
    parse_config(Rest, Config#config{replaced_bad_utf8=true});
parse_config([escaped_forward_slashes|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_forward_slashes=true});
parse_config([explicit_end|Rest], Config) ->
    parse_config(Rest, Config#config{explicit_end=true});
parse_config([single_quoted_strings|Rest], Config) ->
    parse_config(Rest, Config#config{single_quoted_strings=true});
parse_config([unescaped_jsonp|Rest], Config) ->
    parse_config(Rest, Config#config{unescaped_jsonp=true});
parse_config([comments|Rest], Config) ->
    parse_config(Rest, Config#config{comments=true});
parse_config([escaped_strings|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_strings=true});
parse_config([dirty_strings|Rest], Config) ->
    parse_config(Rest, Config#config{dirty_strings=true});
parse_config([ignored_bad_escapes|Rest], Config) ->
    parse_config(Rest, Config#config{ignored_bad_escapes=true});
parse_config([relax|Rest], Config) ->
    parse_config(Rest, Config#config{
        replaced_bad_utf8 = true,
        single_quoted_strings = true,
        comments = true,
        ignored_bad_escapes = true
    });
parse_config([{pre_encode, Encoder}|Rest] = Options, Config) when is_function(Encoder, 1) ->
    case Config#config.pre_encode of
        false -> parse_config(Rest, Config#config{pre_encode=Encoder})
        ; _ -> erlang:error(badarg, [Options, Config])
    end;
%% deprecated flags
parse_config([{pre_encoder, Encoder}|Rest] = Options, Config) when is_function(Encoder, 1) ->
    case Config#config.pre_encode of
        false -> parse_config(Rest, Config#config{pre_encode=Encoder})
        ; _ -> erlang:error(badarg, [Options, Config])
    end;
parse_config([loose_unicode|Rest], Config) ->
    parse_config(Rest, Config#config{replaced_bad_utf8=true});
parse_config([escape_forward_slash|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_forward_slashes=true});
parse_config([single_quotes|Rest], Config) ->
    parse_config(Rest, Config#config{single_quoted_strings=true});
parse_config([no_jsonp_escapes|Rest], Config) ->
    parse_config(Rest, Config#config{unescaped_jsonp=true});
parse_config([json_escape|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_strings=true});
parse_config([ignore_bad_escapes|Rest], Config) ->
    parse_config(Rest, Config#config{ignored_bad_escapes=true});
parse_config(Options, Config) ->
    erlang:error(badarg, [Options, Config]).


valid_flags() ->
    [
        replaced_bad_utf8,
        escaped_forward_slashes,
        single_quoted_strings,
        unescaped_jsonp,
        comments,
        escaped_strings,
        dirty_strings,
        ignored_bad_escapes,
        explicit_end,
        relax,
        pre_encode,
        %% deprecated flags
        pre_encoder,            %% pre_encode
        loose_unicode,          %% replaced_bad_utf8
        escape_forward_slash,   %% escaped_forward_slashes
        single_quotes,          %% single_quoted_strings
        no_jsonp_escapes,       %% unescaped_jsonp
        json_escape,            %% escaped_strings
        ignore_bad_escapes      %% ignored_bad_escapes
    ].


extract_config(Config) ->
    extract_parser_config(Config, []).

extract_parser_config([], Acc) -> Acc;
extract_parser_config([{K,V}|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_config(Rest, [{K,V}] ++ Acc)
        ; false -> extract_parser_config(Rest, Acc)
    end;
extract_parser_config([K|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_config(Rest, [K] ++ Acc)
        ; false -> extract_parser_config(Rest, Acc)
    end.


%% convert a codepoint to it's \uXXXX equiv.
json_escape_sequence(X) ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    [$\\, $u, (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))].


to_hex(10) -> $a;
to_hex(11) -> $b;
to_hex(12) -> $c;
to_hex(13) -> $d;
to_hex(14) -> $e;
to_hex(15) -> $f;
to_hex(X) -> X + 48.    %% ascii "1" is [49], "2" is [50], etc...


clean_string(Bin, #config{dirty_strings=true}) -> Bin;
clean_string(Bin, Config) -> clean_string(Bin, <<>>, Config).

clean_string(Bin, Acc, Config) ->
    Length = cut(Bin),
    <<String:Length/binary, Rest/binary>> = Bin,
    case Rest of
        <<>> -> <<Acc/binary, String/binary>>;
        _ -> maybe_escape(Rest, <<Acc/binary, String/binary>>, Config)
    end.


cut(Bin) -> cut(Bin, 0).

cut(<<32, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<33, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<35, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<36, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<37, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<38, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<39, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<40, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<41, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<42, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<43, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<44, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<45, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<46, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<48, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<49, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<50, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<51, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<52, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<53, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<54, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<55, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<56, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<57, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<58, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<59, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<60, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<61, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<62, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<63, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<64, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<65, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<66, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<67, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<68, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<69, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<70, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<71, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<72, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<73, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<74, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<75, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<76, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<77, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<78, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<79, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<80, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<81, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<82, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<83, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<84, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<85, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<86, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<87, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<88, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<89, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<90, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<91, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<93, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<94, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<95, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<96, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<97, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<98, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<99, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<100, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<101, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<102, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<103, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<104, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<105, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<106, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<107, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<108, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<109, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<110, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<111, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<112, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<113, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<114, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<115, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<116, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<117, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<118, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<119, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<120, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<121, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<122, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<123, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<124, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<125, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<126, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<127, Rest/binary>>, N) -> cut(Rest, N + 1);
cut(<<X/utf8, Rest/binary>>, N) when X >= 128, X < 16#0800 -> cut(Rest, N + 2);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#0800, X < 16#2028 -> cut(Rest, N + 3);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#202a, X < 16#d800 -> cut(Rest, N + 3);
cut(<<X/utf8, Rest/binary>>, N) when X > 16#dfff, X < 16#fdd0 -> cut(Rest, N + 3);
cut(<<X/utf8, Rest/binary>>, N) when X > 16#fdef, X < 16#fffe -> cut(Rest, N + 3);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#10000, X < 16#1fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#20000, X < 16#2fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#30000, X < 16#3fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#40000, X < 16#4fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#50000, X < 16#5fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#60000, X < 16#6fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#70000, X < 16#7fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#80000, X < 16#8fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#90000, X < 16#9fffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#a0000, X < 16#afffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#b0000, X < 16#bfffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#c0000, X < 16#cfffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#d0000, X < 16#dfffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#e0000, X < 16#efffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#f0000, X < 16#ffffe -> cut(Rest, N + 4);
cut(<<X/utf8, Rest/binary>>, N) when X >= 16#100000, X < 16#10fffe -> cut(Rest, N + 4);
cut(_, N) -> N.


strip_continuations(Bin, 0) -> Bin;
strip_continuations(<<X, Rest/binary>>, N) when X >= 128, X =< 191 ->
    strip_continuations(Rest, N - 1);
%% not a continuation byte
strip_continuations(Bin, _) -> Bin.


maybe_escape(<<Codepoint/utf8, Rest/binary>>, Acc, #config{escaped_strings=true} = Config) ->
    case escape(Codepoint, Config) of
        inescapable -> noncharacter(<<Codepoint/utf8, Rest/binary>>, Acc, Config);
        Escaped -> clean_string(Rest, <<Acc/binary, Escaped/binary>>, Config)
    end;
maybe_escape(<<Codepoint/utf8, Rest/binary>>, Acc, Config) ->
    case escape(Codepoint, Config) of
        inescapable -> noncharacter(<<Codepoint/utf8, Rest/binary>>, Acc, Config);
        _ -> clean_string(Rest, <<Acc/binary, Codepoint/utf8>>, Config)
    end;
maybe_escape(Bin, Acc, Config) -> noncharacter(Bin, Acc, Config).


escape($\b, _) -> <<"\\b">>;
escape($\t, _) -> <<"\\t">>;
escape($\n, _) -> <<"\\n">>;
escape($\f, _) -> <<"\\f">>;
escape($\r, _) -> <<"\\r">>;
escape($\", _) -> <<"\\\"">>;
escape($\\, _) -> <<"\\\\">>;
escape($/, #config{escaped_forward_slashes=true}) -> <<"\\/">>;
escape($/, _) -> <<"/">>;
escape(16#2028, #config{unescaped_jsonp=true}) -> <<16#2028/utf8>>;
escape(16#2028, _) -> <<"\\u2028">>;
escape(16#2029, #config{unescaped_jsonp=true}) -> <<16#2029/utf8>>;
escape(16#2029, _) -> <<"\\u2029">>;
escape(X, _) when X < 32 ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    <<"\\u", (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))>>;
escape(_, _) -> inescapable.


%% noncharacters and reserved space
noncharacter(<<_/utf8, Rest/binary>>, Acc, Config) ->
    maybe_replace(Rest, Acc, Config);
%% surrogates
noncharacter(<<237, X, _, Rest/binary>>, Acc, Config) when X >= 160 ->
    maybe_replace(Rest, Acc, Config);
%% u+fffe and u+ffff for R14BXX
noncharacter(<<239, 191, X, Rest/binary>>, Acc, Config) when X == 190; X == 191 ->
    maybe_replace(Rest, Acc, Config);
%% overlong encodings and missing continuations of a 2 byte sequence
noncharacter(<<X, Rest/binary>>, Acc, Config) when X >= 192, X =< 223 ->
    maybe_replace(strip_continuations(Rest, 1), Acc, Config);
%% overlong encodings and missing continuations of a 3 byte sequence
noncharacter(<<X, Rest/binary>>, Acc, Config) when X >= 224, X =< 239 ->
    maybe_replace(strip_continuations(Rest, 2), Acc, Config);
%% overlong encodings and missing continuations of a 4 byte sequence
noncharacter(<<X, Rest/binary>>, Acc, Config) when X >= 240, X =< 247 ->
    maybe_replace(strip_continuations(Rest, 3), Acc, Config);
noncharacter(<<_, Rest/binary>>, Acc, Config) ->
    maybe_replace(Rest, Acc, Config).


maybe_replace(Bin, Acc, #config{replaced_bad_utf8=true} = Config) ->
    clean_string(Bin, <<Acc/binary, 16#fffd/utf8>>, Config);
maybe_replace(_, _, _) -> {error, badarg}.



%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


json_escape_sequence_test_() ->
    [
        {"json escape sequence test - 16#0000", ?_assertEqual(json_escape_sequence(16#0000), "\\u0000")},
        {"json escape sequence test - 16#abc", ?_assertEqual(json_escape_sequence(16#abc), "\\u0abc")},
        {"json escape sequence test - 16#def", ?_assertEqual(json_escape_sequence(16#def), "\\u0def")}
    ].


config_test_() ->
    [
        {"all flags",
            ?_assertEqual(
                #config{
                    replaced_bad_utf8=true,
                    escaped_forward_slashes=true,
                    explicit_end=true,
                    single_quoted_strings=true,
                    unescaped_jsonp=true,
                    comments=true,
                    dirty_strings=true,
                    ignored_bad_escapes=true
                },
                parse_config([
                    replaced_bad_utf8,
                    escaped_forward_slashes,
                    explicit_end,
                    single_quoted_strings,
                    unescaped_jsonp,
                    comments,
                    dirty_strings,
                    ignored_bad_escapes
                ])
            )
        },
        {"relax flag",
            ?_assertEqual(
                #config{
                    replaced_bad_utf8=true,
                    single_quoted_strings=true,
                    comments=true,
                    ignored_bad_escapes=true
                },
                parse_config([relax])
            )
        },
        {"deprecated flags", ?_assertEqual(
            #config{
                pre_encode=fun lists:length/1,
                replaced_bad_utf8=true,
                escaped_forward_slashes=true,
                single_quoted_strings=true,
                unescaped_jsonp=true,
                escaped_strings=true,
                ignored_bad_escapes=true
            },
            parse_config([
                {pre_encoder, fun lists:length/1},
                loose_unicode,
                escape_forward_slash,
                single_quotes,
                no_jsonp_escapes,
                json_escape,
                ignore_bad_escapes
            ])
        )},
        {"pre_encode flag", ?_assertEqual(
            #config{pre_encode=fun lists:length/1},
            parse_config([{pre_encode, fun lists:length/1}])
        )},
        {"two pre_encoders defined", ?_assertError(
            badarg,
            parse_config([
                {pre_encode, fun(_) -> true end},
                {pre_encode, fun(_) -> false end}
            ])
        )},
        {"bad option flag", ?_assertError(badarg, parse_config([error]))}
    ].


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
        {"clean codepoints test", ?_assertEqual(
            codepoints(),
            clean_string(codepoints(), #config{})
        )},
        {"clean extended codepoints test", ?_assertEqual(
            extended_codepoints(),
            clean_string(extended_codepoints(), #config{})
        )}
    ] ++ [
        {
            "reserved character: " ++ lists:flatten(io_lib:format("~p", [Codepoint])),
                ?_assertEqual(
                    {error, badarg},
                    clean_string(Codepoint, #config{})
                )
        } || Codepoint <- reserved_space()
    ] ++ [
        {
            "reserved character: " ++ lists:flatten(io_lib:format("~p", [Codepoint])) ++ " (replaced)",
                ?_assertEqual(
                    <<16#fffd/utf8>>,
                    clean_string(Codepoint, #config{replaced_bad_utf8=true})
                )
        } || Codepoint <- reserved_space()
    ] ++ [
        {
            "surrogate: " ++ lists:flatten(io_lib:format("~p", [Codepoint])),
                ?_assertEqual(
                    {error, badarg},
                    clean_string(Codepoint, #config{})
                )
        } || Codepoint <- surrogates()
    ] ++ [
        {
            "surrogate: " ++ lists:flatten(io_lib:format("~p", [Codepoint])) ++ " (replaced)",
                ?_assertEqual(
                    <<16#fffd/utf8>>,
                    clean_string(Codepoint, #config{replaced_bad_utf8=true})
                )
        } || Codepoint <- surrogates()
    ] ++ [
        {
            "noncharacter: " ++ lists:flatten(io_lib:format("~p", [Codepoint])),
                ?_assertEqual(
                    {error, badarg},
                    clean_string(Codepoint, #config{})
                )
        } || Codepoint <- noncharacters()
    ] ++ [
        {
            "noncharacter: " ++ lists:flatten(io_lib:format("~p", [Codepoint])) ++ " (replaced)",
                ?_assertEqual(
                    <<16#fffd/utf8>>,
                    clean_string(Codepoint, #config{replaced_bad_utf8=true})
                )
        } || Codepoint <- noncharacters()
    ] ++ [
        {
            "extended noncharacter: " ++ lists:flatten(io_lib:format("~p", [Codepoint])),
                ?_assertEqual(
                    {error, badarg},
                    clean_string(Codepoint, #config{})
                )
        } || Codepoint <- extended_noncharacters()
    ] ++ [
        {
            "extended noncharacter: " ++ lists:flatten(io_lib:format("~p", [Codepoint])) ++ " (replaced)",
                ?_assertEqual(
                    <<16#fffd/utf8>>,
                    clean_string(Codepoint, #config{replaced_bad_utf8=true})
                )
        } || Codepoint <- extended_noncharacters()
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


-endif.