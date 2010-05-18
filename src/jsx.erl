-module(jsx).

-export([decoder/0, decoder/1]).

-include("jsx_common.hrl").

decoder() ->
    decoder([]).

decoder(OptsList) ->
    OptsRec = parse_opts(OptsList),
    case OptsRec#opts.encoding of
        utf8 -> 
            fun(Stream) -> jsx_utf8:start(Stream, [], [], OptsRec) end
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
    true = lists:member(Value, [utf8]),
    parse_opts(Rest, Opts#opts{encoding = Value}).