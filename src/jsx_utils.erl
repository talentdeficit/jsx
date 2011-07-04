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

-export([nice_decimal/1, detect_encoding/1, detect_encoding/2, pow/2]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



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
        case Dpoint of ignore -> ignore; X -> X - 1 end, to_ascii(Digit) ++ Acc
    ).


to_ascii(10) -> "a";
to_ascii(11) -> "b";
to_ascii(12) -> "c";
to_ascii(13) -> "d";
to_ascii(14) -> "e";
to_ascii(15) -> "f";
to_ascii(X) -> [X + 48].    %% ascii "1" is [49], "2" is [50], etc...


%% encoding detection   
%% first check to see if there's a bom, if not, use the rfc4627 method for 
%%   determining encoding. this function makes some assumptions about the 
%%   validity of the stream which may delay failure later than if an encoding is 
%%   explicitly provided

detect_encoding(OptsList) ->
    fun(Stream) -> detect_encoding(Stream, OptsList) end.
    
%% utf8 bom detection    
detect_encoding(<<16#ef, 16#bb, 16#bf, Rest/binary>>, Opts) -> 
    (jsx_utf8:decoder(Opts))(Rest);    
%% utf32-little bom detection (this has to come before utf16-little or it'll 
%%   match that)
detect_encoding(<<16#ff, 16#fe, 0, 0, Rest/binary>>, Opts) -> 
    (jsx_utf32le:decoder(Opts))(Rest);        
%% utf16-big bom detection
detect_encoding(<<16#fe, 16#ff, Rest/binary>>, Opts) -> 
    (jsx_utf16:decoder(Opts))(Rest);
%% utf16-little bom detection
detect_encoding(<<16#ff, 16#fe, Rest/binary>>, Opts) -> 
    (jsx_utf16le:decoder(Opts))(Rest);
%% utf32-big bom detection
detect_encoding(<<0, 0, 16#fe, 16#ff, Rest/binary>>, Opts) -> 
    (jsx_utf32:decoder(Opts))(Rest);
    
%% utf32-little null order detection
detect_encoding(<<X, 0, 0, 0, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    (jsx_utf32le:decoder(Opts))(JSON);
%% utf32-big null order detection
detect_encoding(<<0, 0, 0, X, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    (jsx_utf32:decoder(Opts))(JSON);
%% utf16-little null order detection
detect_encoding(<<X, 0, _, 0, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    (jsx_utf16le:decoder(Opts))(JSON);
%% utf16-big null order detection
detect_encoding(<<0, X, 0, _, _Rest/binary>> = JSON, Opts) when X =/= 0 ->
    (jsx_utf16:decoder(Opts))(JSON);
%% utf8 null order detection
detect_encoding(<<X, Y, _Rest/binary>> = JSON, Opts) when X =/= 0, Y =/= 0 ->
    (jsx_utf8:decoder(Opts))(JSON);
    
%% a problem, to autodetect naked single digits' encoding, there is not enough 
%%   data to conclusively determine the encoding correctly. below is an attempt 
%%   to solve the problem
detect_encoding(<<X>>, Opts) when X =/= 0 ->
    {incomplete,
        fun(end_stream) ->
                try
                    {incomplete, Next} = (jsx_utf8:decoder(Opts))(<<X>>),
                    Next(end_stream)
                catch
                    error:function_clause -> {error, {badjson, <<X>>}}
                    ; error:{badmatch, _} -> {error, {badjson, <<X>>}}
                end
            ; (Stream) -> detect_encoding(<<X, Stream/binary>>, Opts) 
        end
    };
detect_encoding(<<0, X>>, Opts) when X =/= 0 ->
    {incomplete,
        fun(end_stream) ->
                try
                    {incomplete, Next} = (jsx_utf16:decoder(Opts))(<<0, X>>),
                    Next(end_stream)
                catch
                    error:function_clause -> {error, {badjson, <<0, X>>}}
                    ; error:{badmatch, _} -> {error, {badjson, <<X>>}}
                end
            ; (Stream) -> detect_encoding(<<0, X, Stream/binary>>, Opts) 
        end
    };
detect_encoding(<<X, 0>>, Opts) when X =/= 0 ->
    {incomplete,
        fun(end_stream) ->
                try
                    {incomplete, Next} = (jsx_utf16le:decoder(Opts))(<<X, 0>>),
                    Next(end_stream)
                catch
                    error:function_clause -> {error, {badjson, <<X, 0>>}}
                    ; error:{badmatch, _} -> {error, {badjson, <<X>>}}
                end
            ; (Stream) -> detect_encoding(<<X, 0, Stream/binary>>, Opts)
        end
    };
    
%% not enough input, request more
detect_encoding(Bin, Opts) ->
    {incomplete,
        fun(end_stream) -> {error, {badjson, Bin}}
            ; (Stream) -> detect_encoding(<<Bin/binary, Stream/binary>>, Opts) 
        end
    }.


%% eunit tests
-ifdef(TEST).
    
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

-endif.