-module(jsx).

-export([decoder/0, decoder/2]).

-include("jsx_common.hrl").

decoder() ->
    decoder(none, []).

decoder(Callbacks, OptsList) ->
    Opts = parse_opts(OptsList),   
    case Opts#opts.encoding of
        utf8 -> 
            fun(Stream) -> jsx_utf8:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; utf16-big ->
            fun(Stream) -> jsx_utf16b:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; utf16-little ->
            fun(Stream) -> jsx_utf16l:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; utf32-big ->
            fun(Stream) -> jsx_utf32b:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; utf32-little ->
            fun(Stream) -> jsx_utf32l:start(Stream, [], init_callbacks(Callbacks), Opts) end
        ; 
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
    true = lists:member(Value, [auto, utf8, utf16-big, utf16-little, utf32-big, utf32-little]),
    parse_opts(Rest, Opts#opts{encoding = Value}).
    
init_callbacks(none) ->
    {none, []};
init_callbacks({M, S}) when is_atom(M) ->
    {M, S};
init_callbacks({F, S}) when is_function(F) ->
    {F, S}.

