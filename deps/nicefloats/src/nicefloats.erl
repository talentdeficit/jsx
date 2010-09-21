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


%% conversion of floats to 'nice' decimal output. erlang's float implementation 
%%   is almost but not quite ieee 754. it converts negative zero to plain zero 
%%   silently, and throws exceptions for any operations that would produce NaN 
%%   or infinity. as far as I can tell that is. trying to match against NaN or 
%%   infinity binary patterns produces nomatch exceptions, and arithmetic 
%%   operations produce badarg exceptions. with that in mind, this function 
%%   makes no attempt to handle special values (except for zero)


-module(nicefloats).

-export([format/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec format(Float::float()) -> string().

format(Float) when is_float(Float) ->
    nice_decimal(Float).


%% algorithm from "Printing FLoating-Point Numbers Quickly and Accurately" by 
%%   Burger & Dybvig
nice_decimal(0.0) -> "0.0";
nice_decimal(Num) ->
    {F, E} = extract(<<Num:64/float>>),
    {R, S, MP, MM} = initial_vals(F, E),
    K = ceiling(math:log10(abs(Num)) - 1.0e-10),
    Round = F band 1 =:= 0,
    {Dpoint, Digits} = scale(R, S, MP, MM, K, Round),
    if Num >= 0 -> format(Dpoint, Digits)
        ; Num < 0 -> "-" ++ format(Dpoint, Digits)
    end.


extract(<<_:1, 0:11, Frac:52>>) -> {Frac, -1074};
extract(<<_:1, Exp:11, Frac:52>>) -> {Frac + (1 bsl 52), Exp - 1075}.


ceiling(X) ->
    Y = trunc(X),
    case X - Y of 
        Z when Z > 0 -> Y + 1 
        ; _ -> Y 
    end.


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


scale(R, S, MP, MM, K, Round) ->
    case K >= 0 of
        true -> fixup(R, S * pow(10, K), MP, MM, K, Round)
        ; false -> 
            Scale = pow(10, -1 * K),
            fixup(R * Scale, S, MP * Scale, MM * Scale, K, Round)
    end.


fixup(R, S, MP, MM, K, true) ->
    case (R + MP >= S) of
        true -> {K + 1, generate(R, S, MP, MM, true)}
        ; false -> {K, generate(R * 10, S, MP * 10, MM * 10, true)}
    end;
fixup(R, S, MP, MM, K, false) ->
    case (R + MP > S) of
        true -> {K + 1, generate(R, S, MP, MM, true)}
        ; false -> {K, generate(R * 10, S, MP * 10, MM * 10, true)}
    end.


generate(RT, S, MP, MM, Round) ->
    D = RT div S,
    R = RT rem S,
    TC1 = case Round of true -> (R =< MM); false -> (R < MM) end,
    TC2 = case Round of true -> (R + MP >= S); false -> (R + MP > S) end,
    case TC1 of
        false -> case TC2 of
                false -> [D | generate(R * 10, S, MP * 10, MM * 10, Round)]
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


format(0, Digits) ->
    format(Digits, ignore, ".0");
format(Dpoint, Digits) when Dpoint =< length(Digits), Dpoint > 0 ->
    format(Digits, Dpoint, []);
format(Dpoint, Digits) when Dpoint > 0 ->
    Pad = Dpoint - length(Digits),
    case Pad of
        X when X > 6 -> 
            format(Digits, 1, []) ++ "e" ++ integer_to_list(Dpoint - 1)
        ; _ -> 
            format(Digits ++ [ 0 || _ <- lists:seq(1, Pad)], Dpoint, [])
    end;
format(Dpoint, Digits) when Dpoint < 0 ->
    format(Digits, 1, []) ++ "e" ++ integer_to_list(Dpoint - 1).

format([], 0, Acc) ->
    lists:reverse("0." ++ Acc);
format([], ignore, Acc) ->
    lists:reverse(Acc);
format(Digits, 0, Acc) ->
    format(Digits, ignore, "." ++ Acc); 
format([Digit|Digits], Dpoint, Acc) ->
    format(Digits, 
        case Dpoint of ignore -> ignore; X -> X - 1 end, to_ascii(Digit) ++ Acc
    ).


to_ascii(X) -> [X + 48].    %% ascii "1" is [49], "2" is [50], etc...



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