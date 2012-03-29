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

-export([parse_opts/1]).
-export([extract_opts/1]).
-export([json_escape/2]).

-include("jsx_opts.hrl").


%% parsing of jsx opts

parse_opts(Opts) ->
    parse_opts(Opts, #opts{}).

parse_opts([], Opts) ->
    Opts;
parse_opts([loose_unicode|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{loose_unicode=true});
parse_opts([escape_forward_slash|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escape_forward_slash=true});
parse_opts([explicit_end|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{explicit_end=true});
parse_opts([single_quotes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{single_quotes=true});
parse_opts([no_jsonp_escapes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{no_jsonp_escapes=true});
parse_opts([comments|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{comments=true});
parse_opts([json_escape|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{json_escape=true});
parse_opts([dirty_strings|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{dirty_strings=true});
parse_opts([ignore_bad_escapes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{ignore_bad_escapes=true});
parse_opts([relax|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{
        loose_unicode = true,
        single_quotes = true,
        comments = true,
        ignore_bad_escapes = true
    });
parse_opts(_, _) ->
    {error, badarg}.


valid_flags() ->
    [
        loose_unicode,
        escape_forward_slash,
        explicit_end,
        single_quotes,
        no_jsonp_escapes,
        comments,
        json_escape,
        dirty_strings,
        ignore_bad_escapes,
        relax
    ].


extract_opts(Opts) ->
    extract_parser_opts(Opts, []).

extract_parser_opts([], Acc) -> Acc;
extract_parser_opts([{K,V}|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_opts(Rest, [{K,V}] ++ Acc)
        ; false -> extract_parser_opts(Rest, Acc)
    end;
extract_parser_opts([K|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_opts(Rest, [K] ++ Acc)
        ; false -> extract_parser_opts(Rest, Acc)
    end.


%% json string escaping, for utf8 binaries. escape the json control sequences to 
%%  their json equivalent, escape other control characters to \uXXXX sequences, 
%%  everything else should be a legal json string component

json_escape(String, Opts) when is_binary(String) ->
    case Opts#opts.dirty_strings of
        true -> String
        ; false -> json_escape(String, Opts, 0, size(String))
    end.


-define(control_character(X),
        <<H:L/binary, X, T/binary>> ->
            json_escape(
                <<H/binary, (unicode:characters_to_binary(json_escape_sequence(X)))/binary, T/binary>>,
                Opts,
                L + 6,
                Len + 5
            )
).

json_escape(Str, Opts, L, Len) when L < Len ->
    case Str of
        ?control_character(0);
        ?control_character(1);
        ?control_character(2);
        ?control_character(3);
        ?control_character(4);
        ?control_character(5);
        ?control_character(6);
        ?control_character(7);
        <<H:L/binary, $\b, T/binary>> -> json_escape(<<H/binary, $\\, $b, T/binary>>, Opts, L + 2, Len + 1);
        <<H:L/binary, $\t, T/binary>> -> json_escape(<<H/binary, $\\, $t, T/binary>>, Opts, L + 2, Len + 1);
        <<H:L/binary, $\n, T/binary>> -> json_escape(<<H/binary, $\\, $n, T/binary>>, Opts, L + 2, Len + 1);
        ?control_character(11);
        <<H:L/binary, $\f, T/binary>> -> json_escape(<<H/binary, $\\, $f, T/binary>>, Opts, L + 2, Len + 1);
        <<H:L/binary, $\r, T/binary>> -> json_escape(<<H/binary, $\\, $r, T/binary>>, Opts, L + 2, Len + 1);
        ?control_character(14);
        ?control_character(15);
        ?control_character(16);
        ?control_character(17);
        ?control_character(18);
        ?control_character(19);
        ?control_character(20);
        ?control_character(21);
        ?control_character(22);
        ?control_character(23);
        ?control_character(24);
        ?control_character(25);
        ?control_character(26);
        ?control_character(27);
        ?control_character(28);
        ?control_character(29);
        ?control_character(30);
        ?control_character(31);
        <<_:L/binary, 32, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 33, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<H:L/binary, $\", T/binary>> -> json_escape(<<H/binary, $\\, $", T/binary>>, Opts, L + 2, Len + 1);
        <<_:L/binary, 35, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 36, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 37, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 38, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 39, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 40, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 41, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 42, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 43, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 44, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 45, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 46, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<H:L/binary, $/, T/binary>> ->
            case Opts#opts.escape_forward_slash of
                true ->
                    json_escape(<<H/binary, $\\, $/, T/binary>>, Opts, L + 2, Len + 1);
                false ->
                    json_escape(<<H/binary, $/, T/binary>>, Opts, L + 1, Len)
            end;
        <<_:L/binary, 48, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 49, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 50, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 51, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 52, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 53, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 54, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 55, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 56, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 57, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 58, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 59, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 60, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 61, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 62, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 63, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 64, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 65, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 66, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 67, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 68, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 69, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 70, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 71, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 72, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 73, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 74, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 75, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 76, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 77, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 78, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 79, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 80, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 81, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 82, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 83, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 84, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 85, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 86, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 87, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 88, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 89, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 90, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 91, _/binary>> -> json_escape(Str, Opts, L + 1, Len);       
        <<H:L/binary, $\\, T/binary>> -> json_escape(<<H/binary, $\\, $\\, T/binary>>, Opts, L + 2, Len + 1);
        <<_:L/binary, 93, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 94, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 95, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 96, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 97, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 98, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 99, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 100, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 101, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 102, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 103, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 104, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 105, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 106, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 107, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 108, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 109, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 110, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 111, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 112, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 113, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 114, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 115, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 116, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 117, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 118, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 119, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 120, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 121, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 122, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 123, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 124, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 125, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 126, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, 127, _/binary>> -> json_escape(Str, Opts, L + 1, Len);
        <<H:L/binary, 16#2028/utf8, T/binary>> ->
            case Opts#opts.no_jsonp_escapes of
                true ->
                    json_escape(<<H/binary, 16#2028/utf8, T/binary>>, Opts, L + 3, Len);
                false ->
                    B = unicode:characters_to_binary(json_escape_sequence(16#2028)),
                    json_escape(<<H/binary, B/binary, T/binary>>, Opts, L + 6, Len + 3)
            end;
        <<H:L/binary, 16#2029/utf8, T/binary>> ->
            case Opts#opts.no_jsonp_escapes of
                true ->
                    json_escape(<<H/binary, 16#2029/utf8, T/binary>>, Opts, L + 3, Len);
                false ->
                    B = unicode:characters_to_binary(json_escape_sequence(16#2029)),
                    json_escape(<<H/binary, B/binary, T/binary>>, Opts, L + 6, Len + 3)
            end;
        <<_:L/binary, X/utf8, _/binary>> when X < 16#0080 ->   
            json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, X/utf8, _/binary>> when X < 16#0800 ->
            json_escape(Str, Opts, L + 2, Len);
        <<_:L/binary, X/utf8, _/binary>> when X < 16#dcff ->
            json_escape(Str, Opts, L + 3, Len);
        <<_:L/binary, X/utf8, _/binary>> when X > 16#dfff, X < 16#fdd0 ->
            json_escape(Str, Opts, L + 3, Len);
        <<_:L/binary, X/utf8, _/binary>> when X > 16#fdef, X < 16#fffe ->
            json_escape(Str, Opts, L + 3, Len);
        <<H:L/binary, X/utf8, T/binary>> when X < 16#10000 ->
            case Opts#opts.loose_unicode of
                true -> json_escape(<<H/binary, 16#fffd/utf8, T/binary>>, Opts, L + 3, Len);
                false -> erlang:error(badarg, [Str, Opts])
            end;
        <<H:L/binary, X/utf8, T/binary>>
                when X == 16#1fffe; X == 16#1ffff;
                X == 16#2fffe; X == 16#2ffff;
                X == 16#3fffe; X == 16#3ffff;
                X == 16#4fffe; X == 16#4ffff;
                X == 16#5fffe; X == 16#5ffff;
                X == 16#6fffe; X == 16#6ffff;
                X == 16#7fffe; X == 16#7ffff;
                X == 16#8fffe; X == 16#8ffff;
                X == 16#9fffe; X == 16#9ffff;
                X == 16#afffe; X == 16#affff;
                X == 16#bfffe; X == 16#bffff;
                X == 16#cfffe; X == 16#cffff;
                X == 16#dfffe; X == 16#dffff;
                X == 16#efffe; X == 16#effff;
                X == 16#ffffe; X == 16#fffff;
                X == 16#10fffe; X == 16#10ffff ->    
            case Opts#opts.loose_unicode of
                true -> json_escape(<<H/binary, 16#fffd/utf8, T/binary>>, Opts, L + 3, Len - 1);
                false -> erlang:error(badarg, [Str, Opts])
            end;
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#10000 ->
            json_escape(Str, Opts, L + 4, Len);
        <<H:L/binary, 237, X, _, T/binary>> when X >= 160 ->
            case Opts#opts.loose_unicode of
                true -> json_escape(<<H/binary, 16#fffd/utf8, T/binary>>, Opts, L + 3, Len);
                false -> erlang:error(badarg, [Str, Opts])
            end;
        <<H:L/binary, 239, 191, X, T/binary>> when X == 190; X == 191 ->
            case Opts#opts.loose_unicode of
                true -> json_escape(<<H/binary, 16#fffd/utf8, T/binary>>, Opts, L + 3, Len);
                false -> erlang:error(badarg, [Str, Opts])
            end;
        <<H:L/binary, X, T/binary>> when X >= 192, X =< 223 ->
            case Opts#opts.loose_unicode of
                true ->
                    {Rest, Stripped} = strip_continuations(T, 1, 0),
                    json_escape(<<H:L/binary, 16#fffd/utf8, Rest/binary>>, Opts, L + 3, Len + 2 - Stripped);
                false -> erlang:error(badarg, [Str, Opts])
            end;
        <<H:L/binary, X, T/binary>> when X >= 224, X =< 239 ->
            case Opts#opts.loose_unicode of
                true ->
                    {Rest, Stripped} = strip_continuations(T, 2, 0),
                    json_escape(<<H:L/binary, 16#fffd/utf8, Rest/binary>>, Opts, L + 3, Len + 2 - Stripped);
                false -> erlang:error(badarg, [Str, Opts])
            end;
        <<H:L/binary, X, T/binary>> when X >= 240, X =< 247 ->
            case Opts#opts.loose_unicode of
                true ->
                    {Rest, Stripped} = strip_continuations(T, 3, 0),
                    json_escape(<<H:L/binary, 16#fffd/utf8, Rest/binary>>, Opts, L + 3, Len + 2 - Stripped);
                false -> erlang:error(badarg, [Str, Opts])
            end;
        <<H:L/binary, _, T/binary>> ->
            case Opts#opts.loose_unicode of
                true -> json_escape(<<H/binary, 16#fffd/utf8, T/binary>>, Opts, L + 3, Len + 2);
                false -> erlang:error(badarg, [Str, Opts])
            end            
    end;
json_escape(Str, _, L, Len) when L =:= Len ->
    Str.


%% convert a codepoint to it's \uXXXX equiv.
json_escape_sequence(X) ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    unicode:characters_to_binary([$\\, $u, (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))]).


to_hex(10) -> $a;
to_hex(11) -> $b;
to_hex(12) -> $c;
to_hex(13) -> $d;
to_hex(14) -> $e;
to_hex(15) -> $f;
to_hex(X) -> X + 48.    %% ascii "1" is [49], "2" is [50], etc...


strip_continuations(Bin, 0, N) -> {Bin, N};
strip_continuations(<<X, Rest/binary>>, N, M) when X >= 128, X =< 191 ->
    strip_continuations(Rest, N - 1, M + 1);
%% not a continuation byte
strip_continuations(Bin, _, N) -> {Bin, N}. 


%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


xcode(Bin) -> xcode(Bin, #opts{}).

xcode(Bin, [loose_unicode]) -> xcode(Bin, #opts{loose_unicode=true});
xcode(Bin, Opts) ->
    try json_escape(Bin, Opts)
    catch error:badarg -> {error, badarg}
    end.


is_bad({error, badarg}) -> true;
is_bad(_) -> false.


bad_utf8_test_() ->
    [
        {"orphan continuation byte u+0080",
            ?_assert(is_bad(xcode(<<16#0080>>)))
        },
        {"orphan continuation byte u+0080 replaced",
            ?_assertEqual(xcode(<<16#0080>>, [loose_unicode]), <<16#fffd/utf8>>)
        },
        {"orphan continuation byte u+00bf",
            ?_assert(is_bad(xcode(<<16#00bf>>)))
        },
        {"orphan continuation byte u+00bf replaced",
            ?_assertEqual(xcode(<<16#00bf>>, [loose_unicode]), <<16#fffd/utf8>>)
        },
        {"2 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 2))/binary>>)))
        },
        {"2 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 2))/binary>>, [loose_unicode]),
                binary:copy(<<16#fffd/utf8>>, 2)
            )
        },
        {"3 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 3))/binary>>)))
        },
        {"3 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 3))/binary>>, [loose_unicode]),
                binary:copy(<<16#fffd/utf8>>, 3)
            )
        },
        {"4 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 4))/binary>>)))
        },
        {"4 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 4))/binary>>, [loose_unicode]),
                binary:copy(<<16#fffd/utf8>>, 4)
            )
        },
        {"5 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 5))/binary>>)))
        },
        {"5 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 5))/binary>>, [loose_unicode]),
                binary:copy(<<16#fffd/utf8>>, 5)
            )
        },
        {"6 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 6))/binary>>)))
        },
        {"6 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 6))/binary>>, [loose_unicode]),
                binary:copy(<<16#fffd/utf8>>, 6)
            )
        },
        {"all continuation bytes",
            ?_assert(is_bad(xcode(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>)))
        },        
        {"all continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, [loose_unicode]),
                binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf)))
            )
        },
        {"lonely start byte",
            ?_assert(is_bad(xcode(<<16#00c0>>)))
        },
        {"lonely start byte replaced",
            ?_assertEqual(
                xcode(<<16#00c0>>, [loose_unicode]),
                <<16#fffd/utf8>>
            )
        },
        {"lonely start bytes (2 byte)",
            ?_assert(is_bad(xcode(<<16#00c0, 32, 16#00df>>)))
        },
        {"lonely start bytes (2 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00c0, 32, 16#00df>>, [loose_unicode]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"lonely start bytes (3 byte)",
            ?_assert(is_bad(xcode(<<16#00e0, 32, 16#00ef>>)))
        },
        {"lonely start bytes (3 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00e0, 32, 16#00ef>>, [loose_unicode]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"lonely start bytes (4 byte)",
            ?_assert(is_bad(xcode(<<16#00f0, 32, 16#00f7>>)))
        },
        {"lonely start bytes (4 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00f0, 32, 16#00f7>>, [loose_unicode]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"missing continuation byte (3 byte)",
            ?_assert(is_bad(xcode(<<224, 160, 32>>)))
        },
        {"missing continuation byte (3 byte) replaced",
            ?_assertEqual(
                xcode(<<224, 160, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"missing continuation byte (4 byte missing one)",
            ?_assert(is_bad(xcode(<<240, 144, 128, 32>>)))
        },
        {"missing continuation byte2 (4 byte missing one) replaced",
            ?_assertEqual(
                xcode(<<240, 144, 128, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"missing continuation byte (4 byte missing two)",
            ?_assert(is_bad(xcode(<<240, 144, 32>>)))
        },
        {"missing continuation byte2 (4 byte missing two) replaced",
            ?_assertEqual(
                xcode(<<240, 144, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (2 byte)",
            ?_assert(is_bad(xcode(<<16#c0, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (2 byte) replaced",
            ?_assertEqual(
                xcode(<<16#c0, 16#af, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (3 byte)",
            ?_assert(is_bad(xcode(<<16#e0, 16#80, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (3 byte) replaced",
            ?_assertEqual(
                xcode(<<16#e0, 16#80, 16#af, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (4 byte)",
            ?_assert(is_bad(xcode(<<16#f0, 16#80, 16#80, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (4 byte) replaced",
            ?_assertEqual(
                xcode(<<16#f0, 16#80, 16#80, 16#af, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 2 byte sequence",
            ?_assert(is_bad(xcode(<<16#c1, 16#bf, 32>>)))
        },
        {"highest overlong 2 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#c1, 16#bf, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 3 byte sequence",
            ?_assert(is_bad(xcode(<<16#e0, 16#9f, 16#bf, 32>>)))
        },
        {"highest overlong 3 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#e0, 16#9f, 16#bf, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 4 byte sequence",
            ?_assert(is_bad(xcode(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>)))
        },
        {"highest overlong 4 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, [loose_unicode]),
                <<16#fffd/utf8, 32>>
            )
        }
    ].


binary_escape_test_() ->
    [
        {"json string escaping", 
            ?_assertEqual(
                json_escape(<<"\"\\\b\f\n\r\t">>, #opts{}),
                <<"\\\"\\\\\\b\\f\\n\\r\\t">>
            )
        },
        {"json string hex escape", 
            ?_assertEqual(
                json_escape(<<0, 1, 2, 3, 11, 26, 30, 31>>, #opts{}),
                <<"\\u0000\\u0001\\u0002\\u0003\\u000b\\u001a\\u001e\\u001f">>
            )
        },
        {"jsonp protection",
            ?_assertEqual(
                json_escape(<<226, 128, 168, 226, 128, 169>>, #opts{}),
                <<"\\u2028\\u2029">>
            )
        },
        {"no jsonp escapes",
            ?_assertEqual(
                json_escape(<<226, 128, 168, 226, 128, 169>>, #opts{no_jsonp_escapes=true}),
                <<226, 128, 168, 226, 128, 169>>
            )
        },
        {"microsoft i hate your date format",
            ?_assertEqual(
                json_escape(<<"/Date(1303502009425)/">>, #opts{escape_forward_slash=true}),
                <<"\\/Date(1303502009425)\\/">>
            )
        },
        {"dirty strings",
            ?_assertEqual(
                json_escape(<<"\\x25\\uffff">>, #opts{dirty_strings=true}),
                <<"\\x25\\uffff">>
            )
        }
    ].


opts_test_() ->
    [
        {"all flags",
            ?_assertEqual(
                parse_opts([
                    loose_unicode,
                    escape_forward_slash,
                    explicit_end,
                    single_quotes,
                    no_jsonp_escapes,
                    comments,
                    json_escape,
                    dirty_strings,
                    ignore_bad_escapes
                ]),
                #opts{
                    loose_unicode=true,
                    escape_forward_slash=true,
                    explicit_end=true,
                    single_quotes=true,
                    no_jsonp_escapes=true,
                    comments=true,
                    json_escape=true,
                    dirty_strings=true,
                    ignore_bad_escapes=true
                }
            )
        },
        {"relax flag",
            ?_assertEqual(
                parse_opts([relax]),
                #opts{
                    loose_unicode=true,
                    single_quotes=true,
                    comments=true,
                    ignore_bad_escapes=true
                }
            )
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


good_characters_test_() ->
    [
        {"acceptable codepoints",
            ?_assertEqual(check_good(good()), [])
        },
        {"acceptable extended",
            ?_assertEqual(check_good(good_extended()), [])
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


check_bad(List) ->
    lists:dropwhile(fun({_, {error, badjson}}) -> true ; (_) -> false end,
        check(List, #opts{}, [])
    ).


check_replaced(List) ->
    lists:dropwhile(fun({_, <<16#fffd/utf8>>}) -> true
            ; (_) -> false 
        end,
        check(List, #opts{loose_unicode=true}, [])
    ).


check_good(List) ->
    lists:dropwhile(fun({_, _}) -> true ; (_) -> false end,
        check(List, #opts{}, [])
    ).


check([], _Opts, Acc) -> Acc;
check([H|T], Opts, Acc) ->
    R = escape(to_fake_utf(H, utf8), Opts),
    check(T, Opts, [{H, R}] ++ Acc).


escape(JSON, Opts) ->
    try json_escape(JSON, Opts)
    catch error:badarg -> {error, badjson}
    end.
    

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


reserved_space() -> lists:seq(16#fdd0, 16#fdef).


good() -> lists:seq(16#0000, 16#d7ff) ++ lists:seq(16#e000, 16#fdcf) ++ lists:seq(16#fdf0, 16#fffd).
            
            
good_extended() -> lists:seq(16#100000, 16#10fffd).


%% erlang refuses to encode certain codepoints, so fake them all
to_fake_utf(N, utf8) when N < 16#0080 -> <<N:8>>;
to_fake_utf(N, utf8) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<2#110:3, Y:5, 2#10:2, X:6>>; 
to_fake_utf(N, utf8) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6>>;
to_fake_utf(N, utf8) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6>>.


-endif.