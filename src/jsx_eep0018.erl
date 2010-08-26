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


%% @hidden hide this module from edoc, exported functions are internal to jsx
%%   and may be altered or removed without notice


-module(jsx_eep0018).
-author("alisdairsullivan@yahoo.ca").


-export([json_to_term/2, term_to_json/2]).


-include("./include/jsx_common.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



json_to_term(JSON, Opts) ->
    P = jsx:parser(opts_to_jsx_opts(Opts)),
    case proplists:get_value(strict, Opts, true) of
        true -> collect_strict(P(JSON), [[]], Opts)
        ; false -> collect(P(JSON), [[]], Opts)
    end.
    

%% the jsx formatter (pretty printer) can do most of the heavy lifting in converting erlang
%%  terms to json strings, but it expects a jsx event iterator. luckily, the mapping from
%%  erlang terms to jsx events is straightforward and the iterator can be faked with an
%%  anonymous function
term_to_json(List, Opts) ->
    case proplists:get_value(strict, Opts, true) of
        true when is_list(List) -> continue
        ; true -> erlang:error(badarg)
        ; false -> continue
    end,
    Encoding = proplists:get_value(encoding, Opts, utf8),
    jsx:format(jsx:eventify(lists:reverse([end_json] ++ term_to_events(List))), [{output_encoding, Encoding}] ++ Opts).
    

%% parse opts for the decoder
opts_to_jsx_opts(Opts) ->
    opts_to_jsx_opts(Opts, []).
    
opts_to_jsx_opts([{encoding, Val}|Rest], Acc) ->
    case lists:member(Val, [auto, utf8, utf16, {utf16, little}, utf32, {utf32, little}]) of
        true -> opts_to_jsx_opts(Rest, [{encoding, Val}] ++ Acc)
        ; false -> opts_to_jsx_opts(Rest, Acc)
    end;
opts_to_jsx_opts([{comments, Val}|Rest], Acc) ->
    case Val of
        true -> opts_to_jsx_opts(Rest, [{comments, true}] ++ Acc)
        ; false -> opts_to_jsx_opts(Rest, [{comments, false}] ++ Acc)
        ; _ -> opts_to_jsx_opts(Rest, Acc)
    end;
opts_to_jsx_opts([_|Rest], Acc) ->
    opts_to_jsx_opts(Rest, Acc);
opts_to_jsx_opts([], Acc) ->
    Acc.


%% ensure the first jsx event we get is start_object or start_array when running
%%  in strict mode
collect_strict({event, Start, Next}, Acc, Opts) when Start =:= start_object; Start =:= start_array ->
    collect(Next(), [[]|Acc], Opts);
collect_strict(_, _, _) ->
    erlang:error(badarg).
    
    
%% collect decoder events and convert to eep0018 format     
collect({event, Start, Next}, Acc, Opts) when Start =:= start_object; Start =:= start_array ->
    collect(Next(), [[]|Acc], Opts);
%% special case for empty object
collect({event, end_object, Next}, [[], Parent|Rest], Opts) when is_list(Parent) ->
    collect(Next(), [[[{}]] ++ Parent] ++ Rest, Opts);
%% reverse the array/object accumulator before prepending it to it's parent
collect({event, end_object, Next}, [Current, Parent|Rest], Opts) when is_list(Parent) ->
    collect(Next(), [[lists:reverse(Current)] ++ Parent] ++ Rest, Opts);
collect({event, end_array, Next}, [Current, Parent|Rest], Opts) when is_list(Parent) ->
    collect(Next(), [[lists:reverse(Current)] ++ Parent] ++ Rest, Opts);
%% special case for empty object
collect({event, end_object, Next}, [[], Key, Parent|Rest], Opts) ->
    collect(Next(), [[{Key, [{}]}] ++ Parent] ++ Rest, Opts);
collect({event, End, Next}, [Current, Key, Parent|Rest], Opts)
        when End =:= end_object; End =:= end_array ->
    collect(Next(), [[{Key, lists:reverse(Current)}] ++ Parent] ++ Rest, Opts);      
collect({event, end_json, _Next}, [[Acc]], _Opts) ->
    Acc;  
%% key can only be emitted inside of a json object, so just insert it directly into
%%   the head of the accumulator and deal with it when we receive it's paired value    
collect({event, {key, _} = PreKey, Next}, [Current|_] = Acc, Opts) ->
    Key = event(PreKey, Opts),
    case decode_key_repeats(Key, Current) of
        true -> erlang:error(badarg)
        ; false -> collect(Next(), [Key] ++ Acc, Opts)
    end;
%% check acc to see if we're inside an object or an array. because inside an object
%%   context the events that fall this far are always preceded by a key (which are
%%   binaries or atoms), if Current is a list, we're inside an array, else, an
%%   object
collect({event, Event, Next}, [Current|Rest], Opts) when is_list(Current) ->
    collect(Next(), [[event(Event, Opts)] ++ Current] ++ Rest, Opts);
collect({event, Event, Next}, [Key, Current|Rest], Opts) ->
    collect(Next(), [[{Key, event(Event, Opts)}] ++ Current] ++ Rest, Opts);
%% if our first returned event is {incomplete, ...} try to force end and return the
%%   Event if one is returned    
collect({incomplete, More}, [[]], Opts) ->
    case More(end_stream) of
        {event, Event, _Next} -> event(Event, Opts)
        ; _ -> erlang:error(badarg)
    end;
%% any other event is an error
collect(_, _, _) -> erlang:error(badarg).
    

%% helper functions for converting jsx events to eep0018 formats 
event({string, String}, _Opts) ->
    unicode:characters_to_binary(String);
event({key, Key}, Opts) ->
    case proplists:get_value(label, Opts, binary) of
        binary -> unicode:characters_to_binary(Key)
        ; atom -> 
            try list_to_atom(Key) 
            catch error:badarg -> unicode:characters_to_binary(Key) end
        ; existing_atom -> 
            try list_to_existing_atom(Key) 
            catch error:badarg -> unicode:characters_to_binary(Key) end
    end;
%% special case for negative zero
event({integer, "-0"}, _Opts) ->
    erlang:float(erlang:list_to_integer("-0"));
event({integer, Integer}, Opts) ->
    case proplists:get_value(float, Opts, false) of
        true -> erlang:float(erlang:list_to_integer(Integer))
        ; false -> erlang:list_to_integer(Integer)
    end;
event({float, Float}, _Opts) ->
    erlang:list_to_float(Float);
event({literal, Literal}, _Opts) ->
    Literal.
    

decode_key_repeats(Key, [{Key, _Value}|_Rest]) -> true;
decode_key_repeats(Key, [_|Rest]) -> decode_key_repeats(Key, Rest);
decode_key_repeats(_Key, []) -> false.

    
    
%% convert eep0018 representation to jsx events. note special casing for the empty object
term_to_events([{}]) ->
    [end_object, start_object];
term_to_events([First|_] = List) when is_tuple(First) ->
    proplist_to_events(List, [start_object]);
term_to_events(List) when is_list(List) ->
    list_to_events(List, [start_array]);
term_to_events(Term) ->
    term_to_event(Term). 
       
    
proplist_to_events([{Key, Term}|Rest], Acc) ->
    Event = term_to_event(Term),
    EncodedKey = key_to_event(Key),
    case encode_key_repeats(EncodedKey, Acc) of
        false -> proplist_to_events(Rest, Event ++ EncodedKey ++ Acc)
        ; true -> erlang:error(badarg)
    end;
proplist_to_events([], Acc) ->
    [end_object] ++ Acc;
proplist_to_events(_, _) ->
    erlang:throw(badarg).
    
    
list_to_events([Term|Rest], Acc) ->
    list_to_events(Rest, term_to_event(Term) ++ Acc);
list_to_events([], Acc) ->
    [end_array] ++ Acc.


term_to_event(List) when is_list(List) ->
    term_to_events(List);
term_to_event(Float) when is_float(Float) ->
    [{float, float_to_decimal(Float)}];
term_to_event(Integer) when is_integer(Integer) ->
    [{integer, erlang:integer_to_list(Integer)}];
term_to_event(String) when is_binary(String) -> 
    [{string, json_escape(String)}];
term_to_event(true) -> [{literal, true}];
term_to_event(false) -> [{literal, false}];
term_to_event(null) -> [{literal, null}];
term_to_event(_) -> erlang:error(badarg).


key_to_event(Key) when is_atom(Key) ->
    [{key, json_escape(erlang:atom_to_binary(Key, utf8))}];
key_to_event(Key) when is_binary(Key) ->
    [{key, json_escape(Key)}].


encode_key_repeats([Key], SoFar) -> encode_key_repeats(Key, SoFar, 0).

encode_key_repeats(Key, [Key|_], 0) -> true;
encode_key_repeats(Key, [end_object|Rest], Level) -> encode_key_repeats(Key, Rest, Level + 1);
encode_key_repeats(_, [start_object|_], 0) -> false;
encode_key_repeats(Key, [start_object|Rest], Level) -> encode_key_repeats(Key, Rest, Level - 1);
encode_key_repeats(Key, [_|Rest], Level) -> encode_key_repeats(Key, Rest, Level);
encode_key_repeats(_, [], 0) -> false.

    
%% conversion of floats to 'nice' decimal output. erlang's float implementation is almost
%%   but not quite ieee 754. it converts negative zero to plain zero silently, and throws
%%   exceptions for any operations that would produce NaN or infinity. as far as I can
%%   tell that is. trying to match against NaN or infinity binary patterns produces nomatch 
%%   exceptions, and arithmetic operations produce badarg exceptions. with that in mind, this 
%%   function makes no attempt to handle special values (except for zero)

%% algorithm from "Printing FLoating-Point Numbers Quickly and Accurately" by Burger & Dybvig
float_to_decimal(0.0) -> "0.0";
float_to_decimal(Num) when is_float(Num) ->
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


%% this is not efficient at all and should be replaced with a lookup table probably
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
        X when X > 6 -> format(Digits, 1, []) ++ "e" ++ integer_to_list(Dpoint - 1)
        ; _ -> format(Digits ++ [ 0 || _ <- lists:seq(1, Pad)], Dpoint, [])
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
    format(Digits, case Dpoint of ignore -> ignore; X -> X - 1 end, to_ascii(Digit) ++ Acc).


to_ascii(X) -> [X + 48].    %% ascii "1" is [49], "2" is [50], etc...


%% json string escaping, for utf8 binaries. escape the json control sequences to their
%%  json equivalent, escape other control characters to \uXXXX sequences, everything
%%  else should be a legal json string component
json_escape(String) ->
    json_escape(String, <<>>).

%% double quote    
json_escape(<<$\", Rest/binary>>, Acc) -> json_escape(Rest, <<Acc/binary, $\\, $\">>);
%% backslash \ reverse solidus
json_escape(<<$\\, Rest/binary>>, Acc) -> json_escape(Rest, <<Acc/binary, $\\, $\\>>);
%% backspace
json_escape(<<$\b, Rest/binary>>, Acc) -> json_escape(Rest, <<Acc/binary, $\\, $b>>);
%% form feed
json_escape(<<$\f, Rest/binary>>, Acc) -> json_escape(Rest, <<Acc/binary, $\\, $f>>);
%% newline
json_escape(<<$\n, Rest/binary>>, Acc) -> json_escape(Rest, <<Acc/binary, $\\, $n>>);
%% cr
json_escape(<<$\r, Rest/binary>>, Acc) -> json_escape(Rest, <<Acc/binary, $\\, $r>>);
%% tab
json_escape(<<$\t, Rest/binary>>, Acc) -> json_escape(Rest, <<Acc/binary, $\\, $t>>);
%% other control characters
json_escape(<<C/utf8, Rest/binary>>, Acc) when C >= 0, C < $\s -> 
    json_escape(Rest, <<Acc/binary, (json_escape_sequence(C))/binary>>);
%% any other legal codepoint
json_escape(<<C/utf8, Rest/binary>>, Acc) ->
    json_escape(Rest, <<Acc/binary, C/utf8>>);
json_escape(<<>>, Acc) ->
    Acc;
json_escape(_, _) ->
    erlang:error(badarg).


%% convert a codepoint to it's \uXXXX equiv. for laziness, this only handles codepoints
%%  this module might escape, ie, control characters
json_escape_sequence(C) when C < 16#20 ->
    <<_:8, A:4, B:4>> = <<C:16>>,   % first two hex digits are always zero
    <<$\\, $u, $0, $0, (to_hex(A)), (to_hex(B))>>.


to_hex(15) -> $f;
to_hex(14) -> $e;
to_hex(13) -> $d;
to_hex(12) -> $c;
to_hex(11) -> $b;
to_hex(10) -> $a;
to_hex(X) -> X + $0.


%% eunit tests
-ifdef(TEST).

decode_test_() ->
    [
        {"empty object", ?_assert(json_to_term(<<"{}">>, []) =:= [{}])},
        {"empty array", ?_assert(json_to_term(<<"[]">>, []) =:= [])},
        {"simple object", ?_assert(json_to_term(<<"{\"a\": true, \"b\": true, \"c\": true}">>, [{label, atom}]) =:= [{a, true}, {b, true}, {c, true}])},
        {"simple array", ?_assert(json_to_term(<<"[true,true,true]">>, []) =:= [true, true, true])},
        {"nested structures", ?_assert(json_to_term(<<"{\"list\":[{\"list\":[{}, {}],\"object\":{}}, []],\"object\":{}}">>, [{label, atom}]) =:= [{list, [[{list, [[{}], [{}]]}, {object, [{}]}],[]]}, {object, [{}]}])},
        {"numbers", ?_assert(json_to_term(<<"[-10000000000.0, -1, 0.0, 0, 1, 10000000000, 1000000000.0]">>, []) =:= [-10000000000.0, -1, 0.0, 0, 1, 10000000000, 1000000000.0])},
        {"numbers (all floats)", ?_assert(json_to_term(<<"[-10000000000.0, -1, 0.0, 0, 1, 10000000000, 1000000000.0]">>, [{float, true}]) =:= [-10000000000.0, -1.0, 0.0, 0.0, 1.0, 10000000000.0, 1000000000.0])},
        {"strings", ?_assert(json_to_term(<<"[\"a string\"]">>, []) =:= [<<"a string">>])},
        {"literals", ?_assert(json_to_term(<<"[true,false,null]">>, []) =:= [true,false,null])},
        {"naked true", ?_assert(json_to_term(<<"true">>, [{strict, false}]) =:= true)},
        {"naked short number", ?_assert(json_to_term(<<"1">>, [{strict, false}]) =:= 1)},
        {"float", ?_assert(json_to_term(<<"1.0">>, [{strict, false}]) =:= 1.0)},
        {"naked string", ?_assert(json_to_term(<<"\"hello world\"">>, [{strict, false}]) =:= <<"hello world">>)},
        {"comments", ?_assert(json_to_term(<<"[ /* a comment in an empty array */ ]">>, [{comments, true}]) =:= [])}
    ].
    
encode_test_() ->
    [
        {"empty object", ?_assert(term_to_json([{}], []) =:= <<"{}">>)},
        {"empty array", ?_assert(term_to_json([], []) =:= <<"[]">>)},
        {"simple object", ?_assert(term_to_json([{a, true}, {b, true}, {c, true}], []) =:= <<"{\"a\":true,\"b\":true,\"c\":true}">>)},
        {"simple array", ?_assert(term_to_json([true, true, true], []) =:= <<"[true,true,true]">>)},
        {"nested structures", ?_assert(term_to_json([{list, [[{list, [[{}], [{}]]}, {object, [{}]}],[]]}, {object, [{}]}], []) =:= <<"{\"list\":[{\"list\":[{},{}],\"object\":{}},[]],\"object\":{}}">>)},
        {"numbers", ?_assert(term_to_json([-10000000000.0, -1, 0.0, 0, 1, 10000000000, 1000000000.0], []) =:= <<"[-1.0e10,-1,0.0,0,1,10000000000,1.0e9]">>)},
        {"strings", ?_assert(term_to_json([<<"a string">>], []) =:= <<"[\"a string\"]">>)},
        {"literals", ?_assert(term_to_json([true,false,null], []) =:= <<"[true,false,null]">>)},
        {"naked true", ?_assert(term_to_json(true, [{strict, false}]) =:= <<"true">>)},
        {"naked number", ?_assert(term_to_json(1, [{strict, false}]) =:= <<"1">>)},
        {"float", ?_assert(term_to_json(1.0, [{strict, false}]) =:= <<"1.0">>)},
        {"naked string", ?_assert(term_to_json(<<"hello world">>, [{strict, false}]) =:= <<"\"hello world\"">>)}
    ].
    
repeated_keys_test_() ->
    [
        {"encode", ?_assertError(badarg, term_to_json([{k, true}, {k, false}], []))},
        {"decode", ?_assertError(badarg, json_to_term(<<"{\"k\": true, \"k\": false}">>, []))}
    ].

escape_test_() ->
    [
        {"json string escaping", ?_assert(json_escape(<<"\"\\\b\f\n\r\t">>) =:= <<"\\\"\\\\\\b\\f\\n\\r\\t">>)},
        {"json string hex escape", ?_assert(json_escape(<<1, 2, 3, 11, 26, 30, 31>>) =:= <<"\\u0001\\u0002\\u0003\\u000b\\u001a\\u001e\\u001f">>)}
    ].
    
nice_decimal_test_() ->
    [
        {"0.0", ?_assert(float_to_decimal(0.0) =:= "0.0")},
        {"1.0", ?_assert(float_to_decimal(1.0) =:= "1.0")},
        {"-1.0", ?_assert(float_to_decimal(-1.0) =:= "-1.0")},
        {"3.1234567890987654321", ?_assert(float_to_decimal(3.1234567890987654321) =:= "3.1234567890987655")},
        {"1.0e23", ?_assert(float_to_decimal(1.0e23) =:= "1.0e23")},
        {"0.3", ?_assert(float_to_decimal(3.0/10.0) =:= "0.3")},
        {"0.0001", ?_assert(float_to_decimal(0.0001) =:= "1.0e-4")},
        {"0.00000001", ?_assert(float_to_decimal(0.00000001) =:= "1.0e-8")},
        {"1.0e-323", ?_assert(float_to_decimal(1.0e-323) =:= "1.0e-323")},
        {"1.0e308", ?_assert(float_to_decimal(1.0e308) =:= "1.0e308")},
        {"min normalized float", ?_assert(float_to_decimal(math:pow(2, -1022)) =:= "2.2250738585072014e-308")},
        {"max normalized float", ?_assert(float_to_decimal((2 - math:pow(2, -52)) * math:pow(2, 1023)) =:= "1.7976931348623157e308")},
        {"min denormalized float", ?_assert(float_to_decimal(math:pow(2, -1074)) =:= "5.0e-324")},
        {"max denormalized float", ?_assert(float_to_decimal((1 - math:pow(2, -52)) * math:pow(2, -1022)) =:= "2.225073858507201e-308")}
    ].

-endif.