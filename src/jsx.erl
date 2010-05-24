-module(jsx).

-export([decoder/0, decoder/2]).

-include("jsx_common.hrl").

decoder() ->
    decoder(none, []).

decoder(Callbacks, OptsList) when is_list(OptsList) ->
    Opts = parse_opts(OptsList),
    decoder(Callbacks, Opts);
decoder(Callbacks, Opts) ->
    case Opts#opts.encoding of
        utf8 ->
            fun(Stream) -> jsx_utf8:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; utf16b ->
            fun(Stream) -> jsx_utf16b:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; utf16l ->
            fun(Stream) -> jsx_utf16l:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; utf32b ->
            fun(Stream) -> jsx_utf32b:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; utf32l ->
            fun(Stream) -> jsx_utf32l:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; auto ->
            fun(Stream) -> detect_encoding(Stream, Callbacks, Opts) end
    end.

    
parse_opts(Opts) ->
    parse_opts(Opts, #opts{}).

parse_opts([], Opts) ->
    Opts;    
parse_opts([{comments, Value}|Rest], Opts) ->
    true = lists:member(Value, [true, false]),
    parse_opts(Rest, Opts#opts{comments = Value});
parse_opts([{escaped_unicode, Value}|Rest], Opts) ->
    true = lists:member(Value, [ascii, codepoint, none]),
    parse_opts(Rest, Opts#opts{escaped_unicode = Value});
parse_opts([{naked_values, Value}|Rest], Opts) ->
    true = lists:member(Value, [true, false]),
    parse_opts(Rest, Opts#opts{naked_values = Value});
parse_opts([{encoding, Value}|Rest], Opts) ->
    true = lists:member(Value, [auto, utf8, utf16b, utf16l, utf32b, utf32l]),
    parse_opts(Rest, Opts#opts{encoding = Value}).
    
init_callbacks(none) ->
    {none, []};
init_callbacks({M, S}) when is_atom(M) ->
    {M, S};
init_callbacks({F, S}) when is_function(F) ->
    {F, S}.
    
detect_encoding(<<A:8, B:8, C:8, D:8, _/binary>> = Stream, Callbacks, Opts) ->
    Encoding = case [A, B, C, D] of
        [0, 0, 0, _] -> utf32b
        ; [0, _, 0, _] -> utf16b
        ; [_, 0, 0, 0] -> utf32l
        ; [_, 0, _, 0] -> utf16l
        ; _ -> utf8
    end,
    (decoder(Callbacks, Opts#opts{encoding = Encoding}))(Stream);
detect_encoding(Else, Callbacks, Opts) ->
    fun(Stream) -> detect_encoding(<<Else/binary, Stream/binary>>, Callbacks, Opts) end.

