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
parse_opts(_, _) ->
    {error, badarg}.


extract_opts(Opts) ->
    extract_parser_opts(Opts, []).

extract_parser_opts([], Acc) -> Acc;
extract_parser_opts([{K,V}|Rest], Acc) ->
    case lists:member(K, [loose_unicode, escape_forward_slash, explicit_end]) of
        true -> extract_parser_opts(Rest, [{K,V}] ++ Acc)
        ; false -> extract_parser_opts(Rest, Acc)
    end;
extract_parser_opts([K|Rest], Acc) ->
    case lists:member(K, [loose_unicode, escape_forward_slash, explicit_end]) of
        true -> extract_parser_opts(Rest, [K] ++ Acc)
        ; false -> extract_parser_opts(Rest, Acc)
    end.


%% json string escaping, for utf8 binaries. escape the json control sequences to 
%%  their json equivalent, escape other control characters to \uXXXX sequences, 
%%  everything else should be a legal json string component

json_escape(String, Opts) when is_binary(String) ->
    json_escape(String, Opts, <<>>).

%% double quote    
json_escape(<<$\", Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $\">>);
%% backslash \ reverse solidus
json_escape(<<$\\, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $\\>>);
%% backspace
json_escape(<<$\b, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $b>>);
%% form feed
json_escape(<<$\f, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $f>>);
%% newline
json_escape(<<$\n, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $n>>);
%% cr
json_escape(<<$\r, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $r>>);
%% tab
json_escape(<<$\t, Rest/binary>>, Opts, Acc) -> 
    json_escape(Rest, Opts, <<Acc/binary, $\\, $t>>);
%% other control characters
json_escape(<<C/utf8, Rest/binary>>, Opts, Acc) when C >= 0, C < $\s -> 
    json_escape(Rest,
        Opts,
        <<Acc/binary, (unicode:characters_to_binary(json_escape_sequence(C)))/binary>>
    );
%% escape forward slashes -- optionally -- to faciliate microsoft's retarded
%%   date format
json_escape(<<$/, Rest/binary>>, Opts=#opts{escape_forward_slash=true}, Acc) ->
    json_escape(Rest, Opts, <<Acc/binary, $\\, $/>>);
%% escape u+2028 and u+2029 to avoid problems with jsonp
json_escape(<<C/utf8, Rest/binary>>, Opts, Acc)
        when C == 16#2028; C == 16#2029 ->
    json_escape(Rest,
        Opts,
        <<Acc/binary, (unicode:characters_to_binary(json_escape_sequence(C)))/binary>>
    );
%% any other legal codepoint
json_escape(<<C/utf8, Rest/binary>>, Opts, Acc) ->
    json_escape(Rest, Opts, <<Acc/binary, C/utf8>>);
json_escape(<<>>, _Opts, Acc) ->
    Acc;
json_escape(Rest, Opts, Acc) ->
    erlang:error(badarg, [Rest, Opts, Acc]).


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
            ?_assert(json_escape(
                    <<"\"\\\b\f\n\r\t">>, #opts{}
                ) =:= <<"\\\"\\\\\\b\\f\\n\\r\\t">>
            )
        },
        {"json string hex escape", 
            ?_assert(json_escape(
                    <<1, 2, 3, 11, 26, 30, 31>>, #opts{}
                ) =:= <<"\\u0001\\u0002\\u0003\\u000b\\u001a\\u001e\\u001f">>
            )
        },
        {"jsonp protection",
            ?_assert(json_escape(
                    <<226, 128, 168, 226, 128, 169>>, #opts{}
                ) =:= <<"\\u2028\\u2029">>
            )
        },
        {"microsoft i hate your date format",
            ?_assert(json_escape(<<"/Date(1303502009425)/">>,
                    #opts{escape_forward_slash=true}
                ) =:= <<"\\/Date(1303502009425)\\/">>
            )
        }
    ].

-endif.