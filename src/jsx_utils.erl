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
        json_escape
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
    json_escape(String, Opts, 0, size(String)).

json_escape(Str, Opts, L, Len) when L < Len ->
    case Str of
        <<H:L/binary, $\", T/binary>> -> %"  
            json_escape(<<H/binary, $\\, $", T/binary>>, Opts, L + 2, Len + 1);
        <<H:L/binary, $\\, T/binary>> ->
            json_escape(<<H/binary, $\\, $\\, T/binary>>, Opts, L + 2, Len + 1);
        <<H:L/binary, $\b, T/binary>> ->
            json_escape(<<H/binary, $\\, $b, T/binary>>, Opts, L + 2, Len + 1);
        <<H:L/binary, $\f, T/binary>> ->
            json_escape(<<H/binary, $\\, $f, T/binary>>, Opts, L + 2, Len + 1);
        <<H:L/binary, $\n, T/binary>> ->
            json_escape(<<H/binary, $\\, $n, T/binary>>, Opts, L + 2, Len + 1); 
        <<H:L/binary, $\r, T/binary>> ->
            json_escape(<<H/binary, $\\, $r, T/binary>>, Opts, L + 2, Len + 1); 
        <<H:L/binary, $\t, T/binary>> ->
            json_escape(<<H/binary, $\\, $t, T/binary>>, Opts, L + 2, Len + 1);
        <<H:L/binary, $/, T/binary>> ->
            case Opts#opts.escape_forward_slash of
                true ->
                    json_escape(<<H/binary, $\\, $/, T/binary>>, Opts, L + 2, Len + 1);
                false ->
                    json_escape(<<H/binary, $/, T/binary>>, Opts, L + 1, Len)
            end;
        <<H:L/binary, 16#2028/utf8, T/binary>> ->
            case Opts#opts.no_jsonp_escapes of
                true ->
                    json_escape(<<H/binary, 16#2028/utf8, T/binary>>, Opts, L + 3, Len);
                false ->
                    B = unicode:characters_to_binary(json_escape_sequence(16#2028)),
                    json_escape(<<H/binary, B/binary, T/binary>>, Opts, L + size(B), Len + size(B) - size(<<16#2028/utf8>>))
            end;
        <<H:L/binary, 16#2029/utf8, T/binary>> ->
            case Opts#opts.no_jsonp_escapes of
                true ->
                    json_escape(<<H/binary, 16#2029/utf8, T/binary>>, Opts, L + 3, Len);
                false ->
                    B = unicode:characters_to_binary(json_escape_sequence(16#2029)),
                    json_escape(<<H/binary, B/binary, T/binary>>, Opts, L + size(B), Len + size(B) - size(<<16#2029/utf8>>))
            end;
        <<H:L/binary, X/utf8, T/binary>> when X < 32 ->
            B = unicode:characters_to_binary(json_escape_sequence(X)),
            json_escape(<<H/binary, B/binary, T/binary>>, Opts, L + size(B), Len + size(B) - size(<<X/utf8>>));
        <<_:L/binary, X/utf8, _/binary>> when X < 16#0080 ->   
            json_escape(Str, Opts, L + 1, Len);
        <<_:L/binary, X/utf8, _/binary>> when X < 16#0800 ->
            json_escape(Str, Opts, L + 2, Len);
        <<_:L/binary, X/utf8, _/binary>> when X < 16#10000 ->
            json_escape(Str, Opts, L + 3, Len);
        <<_:L/binary, _/utf8, _/binary>> ->
            json_escape(Str, Opts, L + 4, Len);
        <<H:L/binary, X, T/binary>> ->
            erlang:error(badarg, [[<<H:L/binary, X, T/binary>>, Opts]])            
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
        {"bad utf8",
            ?_assertError(badarg, json_escape(<<32, 64, 128, 256>>, #opts{}))
        },
        {"all sizes of codepoints",
            ?_assertEqual(
                json_escape(unicode:characters_to_binary([0, 32, 16#80, 16#800, 16#10000]), #opts{}),
                <<"\\u0000", 32/utf8, 16#80/utf8, 16#800/utf8, 16#10000/utf8>>
            )
        }
    ].

-endif.