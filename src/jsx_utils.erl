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

-export([parse_opts/1, extract_opts/1]).
-export([nice_decimal/1]).
-export([json_escape/2]).

-include("../include/jsx_opts.hrl").


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
parse_opts([{parser, Mode}|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{parser=Mode});
parse_opts(_, _) ->
    {error, badarg}.


extract_opts(Opts) ->
    extract_parser_opts(Opts, []).

extract_parser_opts([], Acc) -> Acc;     
extract_parser_opts([{K,V}|Rest], Acc) ->
    case lists:member(K, [loose_unicode, escape_forward_slash, explicit_end, parser]) of
        true -> extract_parser_opts(Rest, [{K,V}] ++ Acc)
        ; false -> extract_parser_opts(Rest, Acc)
    end;
extract_parser_opts([K|Rest], Acc) ->
    case lists:member(K, [loose_unicode, escape_forward_slash, explicit_end]) of
        true -> extract_parser_opts(Rest, [K] ++ Acc)
        ; false -> extract_parser_opts(Rest, Acc)
    end.



%% conversion of floats to 'nice' decimal output. erlang's float implementation 
%%   is almost but not quite ieee 754. it converts negative zero to plain zero 
%%   silently, and throws exceptions for any operations that would produce NaN 
%%   or infinity. as far as I can tell that is. trying to match against NaN or 
%%   infinity binary patterns produces nomatch exceptions, and arithmetic 
%%   operations produce badarg exceptions. with that in mind, this function 
%%   makes no attempt to handle special values (except for zero)

%% algorithm from "Printing Floating-Point Numbers Quickly and Accurately" by 
%%   Burger & Dybvig

-spec nice_decimal(Float::float()) -> string().

nice_decimal(0.0) -> "0.0";
nice_decimal(Num) ->
    {F, E} = extract(<<Num:64/float>>),
    {R, S, MP, MM} = initial_vals(F, E),
    K = ceiling((math:log(abs(Num)) / math:log(10)) - 1.0e-10),
    Round = F band 1 =:= 0,
    {Dpoint, Digits} = scale(R, S, MP, MM, K, 10, Round),
    if Num >= 0 -> digits_to_list(Dpoint, Digits)
        ; Num < 0 -> "-" ++ digits_to_list(Dpoint, Digits)
    end.


%% internal functions

extract(<<_:1, 0:11, Frac:52>>) -> {Frac, -1074};
extract(<<_:1, Exp:11, Frac:52>>) -> {Frac + (1 bsl 52), Exp - 1075}.


initial_vals(F, E) when E >= 0, F /= 1 bsl 52 ->
    BE = 1 bsl E,
    {F * BE * 2, 2, BE, BE};    
initial_vals(F, E) when E >= 0 ->
    BE = 1 bsl E,
    {F * BE * 4, 4, BE * 2, BE};
initial_vals(F, E) when E == -1074; F /= 1 bsl 52 ->
    {F * 2, 1 bsl (-E + 1), 1, 1};
initial_vals(F, E) ->
    {F * 4, 1 bsl (-E + 2), 2, 1}.


ceiling(X) ->
    Y = erlang:trunc(X),
    case X - Y of 
        Z when Z > 0 -> Y + 1 
        ; _ -> Y 
    end.


scale(R, S, MP, MM, K, B, Round) ->
    case K >= 0 of
        true -> fixup(R, S * pow(B, K), MP, MM, K, B, Round)
        ; false -> 
            Scale = pow(B, -1 * K),
            fixup(R * Scale, S, MP * Scale, MM * Scale, K, B, Round)
    end.


fixup(R, S, MP, MM, K, B, true) ->
    case (R + MP >= S) of
        true -> {K + 1, generate(R, S, MP, MM, B, true)}
        ; false -> {K, generate(R * B, S, MP * B, MM * B, B, true)}
    end;
fixup(R, S, MP, MM, K, B, false) ->
    case (R + MP > S) of
        true -> {K + 1, generate(R, S, MP, MM, B, true)}
        ; false -> {K, generate(R * B, S, MP * B, MM * B, B, true)}
    end.


generate(RT, S, MP, MM, B, Round) ->
    D = RT div S,
    R = RT rem S,
    TC1 = case Round of true -> (R =< MM); false -> (R < MM) end,
    TC2 = case Round of true -> (R + MP >= S); false -> (R + MP > S) end,
    case TC1 of
        false -> case TC2 of
                false -> [D | generate(R * B, S, MP * B, MM * B, B, Round)]
                ; true -> [D + 1]
            end
        ; true -> case TC2 of
                false -> [D]
                ; true -> case R * 2 < S of
                    true -> [D]
                    ; false -> [D + 1]
                end
            end
    end.


%% this is not efficient at all and should be replaced with a lookup table 
%%   probably
pow(_B, 0) -> 1;
pow(B, E) when E > 0 -> pow(B, E, 1).

pow(B, E, Acc) when E < 2 -> B * Acc;
pow(B, E, Acc) when E band 1 == 1 -> pow(B * B, E bsr 1, B * Acc);
pow(B, E, Acc) -> pow(B * B, E bsr 1, Acc).


digits_to_list(0, Digits) ->
    digits_to_list(Digits, ignore, ".0");
digits_to_list(Dpoint, Digits) when Dpoint =< length(Digits), Dpoint > 0 ->
    digits_to_list(Digits, Dpoint, []);
digits_to_list(Dpoint, Digits) when Dpoint > 0 ->
    Pad = Dpoint - length(Digits),
    case Pad of
        X when X > 6 -> 
            digits_to_list(Digits, 1, []) ++ "e" ++ integer_to_list(Dpoint - 1)
        ; _ -> 
            digits_to_list(Digits ++ [ 0 || _ <- lists:seq(1, Pad)], Dpoint, [])
    end;
digits_to_list(Dpoint, Digits) when Dpoint < 0 ->
    digits_to_list(Digits, 1, []) ++ "e" ++ integer_to_list(Dpoint - 1).

digits_to_list([], 0, Acc) ->
    lists:reverse("0." ++ Acc);
digits_to_list([], ignore, Acc) ->
    lists:reverse(Acc);
digits_to_list(Digits, 0, Acc) ->
    digits_to_list(Digits, ignore, "." ++ Acc); 
digits_to_list([Digit|Digits], Dpoint, Acc) ->
    digits_to_list(Digits, 
        case Dpoint of ignore -> ignore; X -> X - 1 end, [to_hex(Digit)] ++ Acc
    ).



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
    
nice_decimal_test_() ->
    [
        {"0.0", ?_assert(nice_decimal(0.0) =:= "0.0")},
        {"1.0", ?_assert(nice_decimal(1.0) =:= "1.0")},
        {"-1.0", ?_assert(nice_decimal(-1.0) =:= "-1.0")},
        {"3.1234567890987654321", 
            ?_assert(
                nice_decimal(3.1234567890987654321) =:= "3.1234567890987655")
        },
        {"1.0e23", ?_assert(nice_decimal(1.0e23) =:= "1.0e23")},
        {"0.3", ?_assert(nice_decimal(3.0/10.0) =:= "0.3")},
        {"0.0001", ?_assert(nice_decimal(0.0001) =:= "1.0e-4")},
        {"0.00000001", ?_assert(nice_decimal(0.00000001) =:= "1.0e-8")},
        {"1.0e-323", ?_assert(nice_decimal(1.0e-323) =:= "1.0e-323")},
        {"1.0e308", ?_assert(nice_decimal(1.0e308) =:= "1.0e308")},
        {"min normalized float", 
            ?_assert(
                nice_decimal(math:pow(2, -1022)) =:= "2.2250738585072014e-308"
            )
        },
        {"max normalized float", 
            ?_assert(
                nice_decimal((2 - math:pow(2, -52)) * math:pow(2, 1023)) 
                    =:= "1.7976931348623157e308"
            )
        },
        {"min denormalized float", 
            ?_assert(nice_decimal(math:pow(2, -1074)) =:= "5.0e-324")
        },
        {"max denormalized float", 
            ?_assert(
                nice_decimal((1 - math:pow(2, -52)) * math:pow(2, -1022)) 
                    =:= "2.225073858507201e-308"
            )
        }
    ].


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