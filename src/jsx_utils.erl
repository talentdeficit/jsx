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
-export([json_escape/2, json_escape_sequence/1]).

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
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#10000, X < 16#1fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#20000, X < 16#2fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#30000, X < 16#3fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#40000, X < 16#4fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#50000, X < 16#5fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#60000, X < 16#6fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#70000, X < 16#7fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#80000, X < 16#8fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#90000, X < 16#9fffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#a0000, X < 16#afffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#b0000, X < 16#bfffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#c0000, X < 16#cfffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#d0000, X < 16#dfffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#e0000, X < 16#efffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#f0000, X < 16#ffffe ->
            json_escape(Str, Opts, L + 4, Len);
        <<_:L/binary, X/utf8, _/binary>> when X >= 16#100000, X < 16#10fffe ->
            json_escape(Str, Opts, L + 4, Len);
        _ -> erlang:error(badarg, [Str, Opts])
    end;
json_escape(Str, _, L, Len) when L =:= Len ->
    Str.


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


%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


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


-endif.