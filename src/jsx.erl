-module(jsx).

-export([decoder/0, decoder/2, decode/3]).

-include("jsx_common.hrl").

decode(JSON, Callbacks, Opts) ->
    (decoder(Callbacks, Opts))(JSON). 

decoder() ->
    decoder(none, []).

decoder(Callbacks, OptsList) when is_list(OptsList) ->
    Opts = parse_opts(OptsList),
    decoder(Callbacks, Opts);
decoder(Callbacks, Opts) ->
    case Opts#opts.encoding of
        utf8 ->
            fun(Stream) -> jsx_decoder:start(Stream, [], init_callbacks(Callbacks), Opts) end
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
    parse_opts(Rest, Opts#opts{naked_values = Value, explicit_termination = true});
parse_opts([{encoding, Value}|Rest], Opts) ->
    true = lists:member(Value, [utf8]),
    parse_opts(Rest, Opts#opts{encoding = Value});
parse_opts([{explicit_termination, Value}|Rest], Opts) ->
    true = lists:member(Value, [true, false]),
    parse_opts(Rest, Opts#opts{explicit_termination = Value}).
    
init_callbacks(none) ->
    {none, []};
init_callbacks({M, S}) when is_atom(M) ->
    {M, S};
init_callbacks({F, S}) when is_function(F) ->
    {F, S}.

