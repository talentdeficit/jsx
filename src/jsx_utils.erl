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
-export([json_escape_sequence/1]).

-include("jsx_opts.hrl").


%% parsing of jsx opts
parse_opts(Opts) ->
    parse_opts(Opts, #opts{}).

parse_opts([], Opts) ->
    Opts;
parse_opts([replaced_bad_utf8|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{replaced_bad_utf8=true});
parse_opts([escaped_forward_slashes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escaped_forward_slashes=true});
parse_opts([explicit_end|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{explicit_end=true});
parse_opts([single_quoted_strings|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{single_quoted_strings=true});
parse_opts([unescaped_jsonp|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{unescaped_jsonp=true});
parse_opts([comments|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{comments=true});
parse_opts([escaped_strings|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escaped_strings=true});
parse_opts([dirty_strings|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{dirty_strings=true});
parse_opts([ignored_bad_escapes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{ignored_bad_escapes=true});
parse_opts([relax|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{
        replaced_bad_utf8 = true,
        single_quoted_strings = true,
        comments = true,
        ignored_bad_escapes = true
    });
parse_opts([{pre_encode, Encoder}|Rest] = Options, Opts) when is_function(Encoder, 1) ->
    case Opts#opts.pre_encode of
        false -> parse_opts(Rest, Opts#opts{pre_encode=Encoder})
        ; _ -> erlang:error(badarg, [Options, Opts])
    end;
%% deprecated flags
parse_opts([{pre_encoder, Encoder}|Rest] = Options, Opts) when is_function(Encoder, 1) ->
    case Opts#opts.pre_encode of
        false -> parse_opts(Rest, Opts#opts{pre_encode=Encoder})
        ; _ -> erlang:error(badarg, [Options, Opts])
    end;
parse_opts([loose_unicode|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{replaced_bad_utf8=true});
parse_opts([escape_forward_slash|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escaped_forward_slashes=true});
parse_opts([single_quotes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{single_quoted_strings=true});
parse_opts([no_jsonp_escapes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{unescaped_jsonp=true});
parse_opts([json_escape|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escaped_strings=true});
parse_opts([ignore_bad_escapes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{ignored_bad_escapes=true});
parse_opts(Options, Opts) ->
    erlang:error(badarg, [Options, Opts]).


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
        single_quotes,          %% single_quotes_strings
        no_jsonp_escapes,       %% unescaped_jsonp
        json_escape,            %% escaped_strings
        ignore_bad_escapes      %% ignored_bad_escapes
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


json_escape_sequence_test_() ->
    [
        {"json escape sequence test - 16#0000", ?_assertEqual(json_escape_sequence(16#0000), "\\u0000")},
        {"json escape sequence test - 16#abc", ?_assertEqual(json_escape_sequence(16#abc), "\\u0abc")},
        {"json escape sequence test - 16#def", ?_assertEqual(json_escape_sequence(16#def), "\\u0def")}
    ].

opts_test_() ->
    [
        {"all flags",
            ?_assertEqual(
                parse_opts([
                    replaced_bad_utf8,
                    escaped_forward_slashes,
                    explicit_end,
                    single_quoted_strings,
                    unescaped_jsonp,
                    comments,
                    dirty_strings,
                    ignored_bad_escapes
                ]),
                #opts{
                    replaced_bad_utf8=true,
                    escaped_forward_slashes=true,
                    explicit_end=true,
                    single_quoted_strings=true,
                    unescaped_jsonp=true,
                    comments=true,
                    dirty_strings=true,
                    ignored_bad_escapes=true
                }
            )
        },
        {"relax flag",
            ?_assertEqual(
                parse_opts([relax]),
                #opts{
                    replaced_bad_utf8=true,
                    single_quoted_strings=true,
                    comments=true,
                    ignored_bad_escapes=true
                }
            )
        }
    ].


-endif.