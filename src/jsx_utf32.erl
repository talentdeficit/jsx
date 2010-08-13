-file("priv/jsx_decoder_template.erl", 1).

-module(jsx_utf32).

-author("alisdairsullivan@yahoo.ca").

-export([parse/2]).

-file("./include/jsx_decoder.hrl", 1).

-file("priv/jsx_decoder_template.erl", 35).

-file("./include/jsx_types.hrl", 1).

-type jsx_opts() :: [jsx_opt()].

-type jsx_opt() :: {comments, true | false}
                 | {escaped_unicode, ascii | codepoint | none}
                 | {multi_term, true | false}
                 | {encoding,
                    auto |
                    utf8 |
                    utf16 |
                    {utf16, little} |
                    utf32 |
                    {utf32, little}}.

-type unicode_codepoint() :: 0..1114111.

-type unicode_string() :: [unicode_codepoint()].

-type jsx_event() :: start_object
                   | end_object
                   | start_array
                   | end_array
                   | end_json
                   | {key, unicode_string()}
                   | {string, unicode_string()}
                   | {integer, unicode_string()}
                   | {float, unicode_string()}
                   | {literal, true}
                   | {literal, false}
                   | {literal, null}.

-type jsx_parser() :: fun((binary()) -> jsx_parser_result()).

-type jsx_parser_result() :: {event,
                              jsx_event(),
                              fun(() -> jsx_parser_result())}
                           | {incomplete, jsx_parser()}
                           | {error, badjson}
                           | ok.

-type eep0018() :: eep0018_object() | eep0018_array().

-type eep0018_array() :: [eep0018_term()].

-type eep0018_object() :: [{eep0018_key(), eep0018_term()}].

-type eep0018_key() :: binary() | atom().

-type eep0018_term() :: eep0018_array()
                      | eep0018_object()
                      | eep0018_string()
                      | eep0018_number()
                      | true
                      | false
                      | null.

-type eep0018_string() :: binary().

-type eep0018_number() :: float() | integer().

-type supported_utf() :: utf8
                       | utf16
                       | {utf16, little}
                       | utf32
                       | {utf32, little}.

-type encoder_opts() :: [encoder_opt()].

-type encoder_opt() :: {strict, true | false}
                     | {encoding, supported_utf()}
                     | {space, integer()}
                     | space
                     | {indent, integer()}
                     | indent.

-type decoder_opts() :: [decoder_opt()].

-type decoder_opt() :: {strict, true | false}
                     | {comments, true | false}
                     | {encoding, supported_utf()}
                     | {label, atom | binary | existing_atom}
                     | {float, true | false}.

-type verify_opts() :: [verify_opt()].

-type verify_opt() :: {strict, true | false}
                    | {encoding, auto | supported_utf()}
                    | {comments, true | false}.

-type format_opts() :: [format_opt()].

-type format_opt() :: {strict, true | false}
                    | {encoding, auto | supported_utf()}
                    | {comments, true | false}
                    | {space, integer()}
                    | space
                    | {indent, integer()}
                    | indent
                    | {output_encoding, supported_utf()}.

-file("priv/jsx_decoder_template.erl", 36).

-spec parse(JSON :: eep0018(), Opts :: jsx_opts()) ->
               jsx_parser_result().

parse(JSON, Opts) ->
    start(JSON, [], Opts).

start(<<S/utf32,Rest/binary>>, Stack, Opts)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    start(Rest, Stack, Opts);
start(<<123/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     start_object,
     fun() ->
            object(Rest, [key|Stack], Opts)
     end};
start(<<91/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     start_array,
     fun() ->
            array(Rest, [array|Stack], Opts)
     end};
start(<<34/utf32,Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
start(<<$t/utf32,Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
start(<<$f/utf32,Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
start(<<$n/utf32,Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
start(<<45/utf32,Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
start(<<48/utf32,Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
start(<<S/utf32,Rest/binary>>, Stack, Opts)
    when
        S >= $1
        andalso
        S =< $9 ->
    integer(Rest, Stack, Opts, [S]);
start(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         start(Resume, Stack, Opts)
                  end);
start(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    start(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

maybe_done(<<S/utf32,Rest/binary>>, Stack, Opts)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    maybe_done(Rest, Stack, Opts);
maybe_done(<<125/utf32,Rest/binary>>, [object|Stack], Opts) ->
    {event,
     end_object,
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
maybe_done(<<93/utf32,Rest/binary>>, [array|Stack], Opts) ->
    {event,
     end_array,
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
maybe_done(<<44/utf32,Rest/binary>>, [object|Stack], Opts) ->
    key(Rest, [key|Stack], Opts);
maybe_done(<<44/utf32,Rest/binary>>, [array|_] = Stack, Opts) ->
    value(Rest, Stack, Opts);
maybe_done(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         maybe_done(Resume, Stack, Opts)
                  end);
maybe_done(Rest, [], {_,_,_,true,_} = Opts) ->
    {event,
     end_json,
     fun() ->
            start(Rest, [], Opts)
     end};
maybe_done(Rest, [], Opts) ->
    done(Rest, Opts);
maybe_done(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    maybe_done(<<Bin/binary,Stream/binary>>,
                               Stack,
                               Opts)
             end};
        false ->
            {error,badjson}
    end.

done(<<S/utf32,Rest/binary>>, Opts)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    done(Rest, Opts);
done(<<47/utf32,Rest/binary>>, {_,true,_,_,_} = Opts) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         done(Resume, Opts)
                  end);
done(<<>>, Opts) ->
    {event,
     end_json,
     fun() ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    done(Stream, Opts)
             end}
     end};
done(Bin, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    done(<<Bin/binary,Stream/binary>>, Opts)
             end};
        false ->
            {error,badjson}
    end.

object(<<S/utf32,Rest/binary>>, Stack, Opts)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    object(Rest, Stack, Opts);
object(<<34/utf32,Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
object(<<125/utf32,Rest/binary>>, [key|Stack], Opts) ->
    {event,
     end_object,
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
object(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         object(Resume, Stack, Opts)
                  end);
object(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    object(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

array(<<S/utf32,Rest/binary>>, Stack, Opts)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    array(Rest, Stack, Opts);
array(<<34/utf32,Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
array(<<$t/utf32,Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
array(<<$f/utf32,Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
array(<<$n/utf32,Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
array(<<45/utf32,Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
array(<<48/utf32,Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
array(<<S/utf32,Rest/binary>>, Stack, Opts)
    when
        S >= $1
        andalso
        S =< $9 ->
    integer(Rest, Stack, Opts, [S]);
array(<<123/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     start_object,
     fun() ->
            object(Rest, [key|Stack], Opts)
     end};
array(<<91/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     start_array,
     fun() ->
            array(Rest, [array|Stack], Opts)
     end};
array(<<93/utf32,Rest/binary>>, [array|Stack], Opts) ->
    {event,
     end_array,
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
array(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         array(Resume, Stack, Opts)
                  end);
array(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    array(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

value(<<S/utf32,Rest/binary>>, Stack, Opts)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    value(Rest, Stack, Opts);
value(<<34/utf32,Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
value(<<$t/utf32,Rest/binary>>, Stack, Opts) ->
    tr(Rest, Stack, Opts);
value(<<$f/utf32,Rest/binary>>, Stack, Opts) ->
    fa(Rest, Stack, Opts);
value(<<$n/utf32,Rest/binary>>, Stack, Opts) ->
    nu(Rest, Stack, Opts);
value(<<45/utf32,Rest/binary>>, Stack, Opts) ->
    negative(Rest, Stack, Opts, "-");
value(<<48/utf32,Rest/binary>>, Stack, Opts) ->
    zero(Rest, Stack, Opts, "0");
value(<<S/utf32,Rest/binary>>, Stack, Opts)
    when
        S >= $1
        andalso
        S =< $9 ->
    integer(Rest, Stack, Opts, [S]);
value(<<123/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     start_object,
     fun() ->
            object(Rest, [key|Stack], Opts)
     end};
value(<<91/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     start_array,
     fun() ->
            array(Rest, [array|Stack], Opts)
     end};
value(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         value(Resume, Stack, Opts)
                  end);
value(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    value(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

colon(<<S/utf32,Rest/binary>>, Stack, Opts)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    colon(Rest, Stack, Opts);
colon(<<58/utf32,Rest/binary>>, [key|Stack], Opts) ->
    value(Rest, [object|Stack], Opts);
colon(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         colon(Resume, Stack, Opts)
                  end);
colon(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    colon(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

key(<<S/utf32,Rest/binary>>, Stack, Opts)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    key(Rest, Stack, Opts);
key(<<34/utf32,Rest/binary>>, Stack, Opts) ->
    string(Rest, Stack, Opts, []);
key(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         key(Resume, Stack, Opts)
                  end);
key(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    key(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

string(<<34/utf32,Rest/binary>>, [key|_] = Stack, Opts, Acc) ->
    {event,
     {key,lists:reverse(Acc)},
     fun() ->
            colon(Rest, Stack, Opts)
     end};
string(<<34/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    {event,
     {string,lists:reverse(Acc)},
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
string(<<92/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    escape(Rest, Stack, Opts, Acc);
string(<<S/utf32,Rest/binary>>, Stack, Opts, Acc) when S >= 32 ->
    string(Rest, Stack, Opts, [S] ++ Acc);
string(Bin, Stack, Opts, Acc) ->
    case partial_utf(Bin) of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    string(<<Bin/binary,Stream/binary>>,
                           Stack,
                           Opts,
                           Acc)
             end};
        false ->
            {error,badjson}
    end.

partial_utf(<<_:32>>) ->
    false;
partial_utf(_) ->
    true.

escape(<<$b/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\b" ++ Acc);
escape(<<$f/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\f" ++ Acc);
escape(<<$n/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\n" ++ Acc);
escape(<<$r/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\r" ++ Acc);
escape(<<$t/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    string(Rest, Stack, Opts, "\t" ++ Acc);
escape(<<$u/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    escaped_unicode(Rest, Stack, Opts, Acc, []);
escape(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when S =:= 34; S =:= 47; S =:= 92 ->
    string(Rest, Stack, Opts, [S] ++ Acc);
escape(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    escape(<<Bin/binary,Stream/binary>>,
                           Stack,
                           Opts,
                           Acc)
             end};
        false ->
            {error,badjson}
    end.

escaped_unicode(<<D/utf32,Rest/binary>>,
                Stack,
                {_,_,ascii,_,_} = Opts,
                String,
                [C,B,A])
    when
        D >= $a
        andalso
        D =< $z;
        D >= $A
        andalso
        D =< $Z;
        D >= $0
        andalso
        D =< $9 ->
    case erlang:list_to_integer([A,B,C,D], 16) of
        X when X < 128 ->
            string(Rest, Stack, Opts, [X] ++ String);
        _ ->
            string(Rest, Stack, Opts, [D,C,B,A,$u,92] ++ String)
    end;
escaped_unicode(<<D/utf32,Rest/binary>>,
                Stack,
                {_,_,codepoint,_,_} = Opts,
                String,
                [C,B,A])
    when
        D >= $a
        andalso
        D =< $z;
        D >= $A
        andalso
        D =< $Z;
        D >= $0
        andalso
        D =< $9 ->
    case erlang:list_to_integer([A,B,C,D], 16) of
        X when X >= 56320, X =< 57343 ->
            case check_acc_for_surrogate(String) of
                false ->
                    string(Rest, Stack, Opts, [D,C,B,A,$u,92] ++ String);
                {Y,NewString} ->
                    string(Rest,
                           Stack,
                           Opts,
                           [surrogate_to_codepoint(Y, X)] ++ NewString)
            end;
        X when X < 55296; X > 57343, X < 65534 ->
            string(Rest, Stack, Opts, [X] ++ String);
        _ ->
            string(Rest, Stack, Opts, [D,C,B,A,$u,92] ++ String)
    end;
escaped_unicode(<<D/utf32,Rest/binary>>, Stack, Opts, String, [C,B,A])
    when
        D >= $a
        andalso
        D =< $z;
        D >= $A
        andalso
        D =< $Z;
        D >= $0
        andalso
        D =< $9 ->
    string(Rest, Stack, Opts, [D,C,B,A,$u,92] ++ String);
escaped_unicode(<<S/utf32,Rest/binary>>, Stack, Opts, String, Acc)
    when
        S >= $a
        andalso
        S =< $z;
        S >= $A
        andalso
        S =< $Z;
        S >= $0
        andalso
        S =< $9 ->
    escaped_unicode(Rest, Stack, Opts, String, [S] ++ Acc);
escaped_unicode(Bin, Stack, Opts, String, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    escaped_unicode(<<Bin/binary,Stream/binary>>,
                                    Stack,
                                    Opts,
                                    String,
                                    Acc)
             end};
        false ->
            {error,badjson}
    end.

check_acc_for_surrogate([D,C,B,A,$u,92|Rest])
    when
        D >= $a
        andalso
        D =< $z;
        D >= $A
        andalso
        D =< $Z;
        D >= $0
        andalso
        D =< $9,
        C >= $a
        andalso
        C =< $z;
        C >= $A
        andalso
        C =< $Z;
        C >= $0
        andalso
        C =< $9,
        B >= $a
        andalso
        B =< $z;
        B >= $A
        andalso
        B =< $Z;
        B >= $0
        andalso
        B =< $9,
        A >= $a
        andalso
        A =< $z;
        A >= $A
        andalso
        A =< $Z;
        A >= $0
        andalso
        A =< $9 ->
    case erlang:list_to_integer([A,B,C,D], 16) of
        X when X >= 55296, X =< 56319 ->
            {X,Rest};
        _ ->
            false
    end;
check_acc_for_surrogate(_) ->
    false.

surrogate_to_codepoint(High, Low) ->
    (High - 55296) * 1024 + (Low - 56320) + 65536.

negative(<<$0/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    zero(Rest, Stack, Opts, "0" ++ Acc);
negative(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when
        S >= $1
        andalso
        S =< $9 ->
    integer(Rest, Stack, Opts, [S] ++ Acc);
negative(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    negative(<<Bin/binary,Stream/binary>>,
                             Stack,
                             Opts,
                             Acc)
             end};
        false ->
            {error,badjson}
    end.

zero(<<125/utf32,Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            {event,
             end_object,
             fun() ->
                    maybe_done(Rest, Stack, Opts)
             end}
     end};
zero(<<93/utf32,Rest/binary>>, [array|Stack], Opts, Acc) ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            {event,
             end_array,
             fun() ->
                    maybe_done(Rest, Stack, Opts)
             end}
     end};
zero(<<44/utf32,Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            key(Rest, [key|Stack], Opts)
     end};
zero(<<44/utf32,Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            value(Rest, Stack, Opts)
     end};
zero(<<46/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    initial_decimal(Rest, Stack, Opts, [46] ++ Acc);
zero(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
zero(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts, Acc) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         zero(Resume, Stack, Opts, Acc)
                  end);
zero(<<>>, [], Opts, Acc) ->
    {incomplete,
     fun(end_stream) ->
            {event,
             {integer,lists:reverse(Acc)},
             fun() ->
                    {event,
                     end_json,
                     fun() ->
                            zero(<<>>, [], Opts, Acc)
                     end}
             end};
        (Stream) ->
            zero(Stream, [], Opts, Acc)
     end};
zero(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    zero(<<Bin/binary,Stream/binary>>, Stack, Opts, Acc)
             end};
        false ->
            {error,badjson}
    end.

integer(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when
        S >= $1
        andalso
        S =< $9 ->
    integer(Rest, Stack, Opts, [S] ++ Acc);
integer(<<125/utf32,Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            {event,
             end_object,
             fun() ->
                    maybe_done(Rest, Stack, Opts)
             end}
     end};
integer(<<93/utf32,Rest/binary>>, [array|Stack], Opts, Acc) ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            {event,
             end_array,
             fun() ->
                    maybe_done(Rest, Stack, Opts)
             end}
     end};
integer(<<44/utf32,Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            key(Rest, [key|Stack], Opts)
     end};
integer(<<44/utf32,Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            value(Rest, Stack, Opts)
     end};
integer(<<46/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    initial_decimal(Rest, Stack, Opts, [46] ++ Acc);
integer(<<48/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    integer(Rest, Stack, Opts, [48] ++ Acc);
integer(<<$e/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    e(Rest, Stack, Opts, "e0." ++ Acc);
integer(<<$E/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    e(Rest, Stack, Opts, "e0." ++ Acc);
integer(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    {event,
     {integer,lists:reverse(Acc)},
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
integer(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts, Acc) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         integer(Resume, Stack, Opts, Acc)
                  end);
integer(<<>>, [], Opts, Acc) ->
    {incomplete,
     fun(end_stream) ->
            {event,
             {integer,lists:reverse(Acc)},
             fun() ->
                    {event,
                     end_json,
                     fun() ->
                            integer(<<>>, [], Opts, Acc)
                     end}
             end};
        (Stream) ->
            integer(Stream, [], Opts, Acc)
     end};
integer(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    integer(<<Bin/binary,Stream/binary>>,
                            Stack,
                            Opts,
                            Acc)
             end};
        false ->
            {error,badjson}
    end.

initial_decimal(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when
        S >= $1
        andalso
        S =< $9 ->
    decimal(Rest, Stack, Opts, [S] ++ Acc);
initial_decimal(<<48/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    decimal(Rest, Stack, Opts, [48] ++ Acc);
initial_decimal(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    initial_decimal(<<Bin/binary,Stream/binary>>,
                                    Stack,
                                    Opts,
                                    Acc)
             end};
        false ->
            {error,badjson}
    end.

decimal(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when
        S >= $1
        andalso
        S =< $9 ->
    decimal(Rest, Stack, Opts, [S] ++ Acc);
decimal(<<125/utf32,Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            {event,
             end_object,
             fun() ->
                    maybe_done(Rest, Stack, Opts)
             end}
     end};
decimal(<<93/utf32,Rest/binary>>, [array|Stack], Opts, Acc) ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            {event,
             end_array,
             fun() ->
                    maybe_done(Rest, Stack, Opts)
             end}
     end};
decimal(<<44/utf32,Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            key(Rest, [key|Stack], Opts)
     end};
decimal(<<44/utf32,Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            value(Rest, Stack, Opts)
     end};
decimal(<<48/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    decimal(Rest, Stack, Opts, [48] ++ Acc);
decimal(<<$e/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    e(Rest, Stack, Opts, "e" ++ Acc);
decimal(<<$E/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    e(Rest, Stack, Opts, "e" ++ Acc);
decimal(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
decimal(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts, Acc) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         decimal(Resume, Stack, Opts, Acc)
                  end);
decimal(<<>>, [], Opts, Acc) ->
    {incomplete,
     fun(end_stream) ->
            {event,
             {float,lists:reverse(Acc)},
             fun() ->
                    {event,
                     end_json,
                     fun() ->
                            decimal(<<>>, [], Opts, Acc)
                     end}
             end};
        (Stream) ->
            decimal(Stream, [], Opts, Acc)
     end};
decimal(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    decimal(<<Bin/binary,Stream/binary>>,
                            Stack,
                            Opts,
                            Acc)
             end};
        false ->
            {error,badjson}
    end.

e(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when
        S =:= 48;
        S >= $1
        andalso
        S =< $9 ->
    exp(Rest, Stack, Opts, [S] ++ Acc);
e(<<S/utf32,Rest/binary>>, Stack, Opts, Acc) when S =:= 43; S =:= 45 ->
    ex(Rest, Stack, Opts, [S] ++ Acc);
e(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    e(<<Bin/binary,Stream/binary>>, Stack, Opts, Acc)
             end};
        false ->
            {error,badjson}
    end.

ex(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when
        S =:= 48;
        S >= $1
        andalso
        S =< $9 ->
    exp(Rest, Stack, Opts, [S] ++ Acc);
ex(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    ex(<<Bin/binary,Stream/binary>>, Stack, Opts, Acc)
             end};
        false ->
            {error,badjson}
    end.

exp(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when
        S >= $1
        andalso
        S =< $9 ->
    exp(Rest, Stack, Opts, [S] ++ Acc);
exp(<<125/utf32,Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            {event,
             end_object,
             fun() ->
                    maybe_done(Rest, Stack, Opts)
             end}
     end};
exp(<<93/utf32,Rest/binary>>, [array|Stack], Opts, Acc) ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            {event,
             end_array,
             fun() ->
                    maybe_done(Rest, Stack, Opts)
             end}
     end};
exp(<<44/utf32,Rest/binary>>, [object|Stack], Opts, Acc) ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            key(Rest, [key|Stack], Opts)
     end};
exp(<<44/utf32,Rest/binary>>, [array|_] = Stack, Opts, Acc) ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            value(Rest, Stack, Opts)
     end};
exp(<<48/utf32,Rest/binary>>, Stack, Opts, Acc) ->
    exp(Rest, Stack, Opts, [48] ++ Acc);
exp(<<S/utf32,Rest/binary>>, Stack, Opts, Acc)
    when S =:= 32; S =:= 9; S =:= 13; S =:= 10 ->
    {event,
     {float,lists:reverse(Acc)},
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
exp(<<47/utf32,Rest/binary>>, Stack, {_,true,_,_,_} = Opts, Acc) ->
    maybe_comment(Rest,
                  fun(Resume) ->
                         exp(Resume, Stack, Opts, Acc)
                  end);
exp(<<>>, [], Opts, Acc) ->
    {incomplete,
     fun(end_stream) ->
            {event,
             {float,lists:reverse(Acc)},
             fun() ->
                    {event,
                     end_json,
                     fun() ->
                            exp(<<>>, [], Opts, Acc)
                     end}
             end};
        (Stream) ->
            exp(Stream, [], Opts, Acc)
     end};
exp(Bin, Stack, Opts, Acc) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    exp(<<Bin/binary,Stream/binary>>, Stack, Opts, Acc)
             end};
        false ->
            {error,badjson}
    end.

tr(<<$r/utf32,Rest/binary>>, Stack, Opts) ->
    tru(Rest, Stack, Opts);
tr(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    tr(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

tru(<<$u/utf32,Rest/binary>>, Stack, Opts) ->
    true(Rest, Stack, Opts);
tru(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    tru(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

true(<<$e/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     {literal,true},
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
true(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    true(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

fa(<<$a/utf32,Rest/binary>>, Stack, Opts) ->
    fal(Rest, Stack, Opts);
fa(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    fa(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

fal(<<$l/utf32,Rest/binary>>, Stack, Opts) ->
    fals(Rest, Stack, Opts);
fal(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    fal(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

fals(<<$s/utf32,Rest/binary>>, Stack, Opts) ->
    false(Rest, Stack, Opts);
fals(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    fals(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

false(<<$e/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     {literal,false},
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
false(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    false(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

nu(<<$u/utf32,Rest/binary>>, Stack, Opts) ->
    nul(Rest, Stack, Opts);
nu(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    nu(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

nul(<<$l/utf32,Rest/binary>>, Stack, Opts) ->
    null(Rest, Stack, Opts);
nul(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    nul(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

null(<<$l/utf32,Rest/binary>>, Stack, Opts) ->
    {event,
     {literal,null},
     fun() ->
            maybe_done(Rest, Stack, Opts)
     end};
null(Bin, Stack, Opts) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    null(<<Bin/binary,Stream/binary>>, Stack, Opts)
             end};
        false ->
            {error,badjson}
    end.

maybe_comment(<<42/utf32,Rest/binary>>, Resume) ->
    comment(Rest, Resume);
maybe_comment(Bin, Resume) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    maybe_comment(<<Bin/binary,Stream/binary>>, Resume)
             end};
        false ->
            {error,badjson}
    end.

comment(<<42/utf32,Rest/binary>>, Resume) ->
    maybe_comment_done(Rest, Resume);
comment(<<_/utf32,Rest/binary>>, Resume) ->
    comment(Rest, Resume);
comment(Bin, Resume) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    comment(<<Bin/binary,Stream/binary>>, Resume)
             end};
        false ->
            {error,badjson}
    end.

maybe_comment_done(<<47/utf32,Rest/binary>>, Resume) ->
    Resume(Rest);
maybe_comment_done(<<_/utf32,Rest/binary>>, Resume) ->
    comment(Rest, Resume);
maybe_comment_done(Bin, Resume) ->
    case byte_size(Bin) < 4 of
        true ->
            {incomplete,
             fun(end_stream) ->
                    {error,badjson};
                (Stream) ->
                    maybe_comment_done(<<Bin/binary,Stream/binary>>,
                                       Resume)
             end};
        false ->
            {error,badjson}
    end.



