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


-module(jsx).
-author("alisdairsullivan@yahoo.ca").

-export([decoder/0, decoder/1, decoder/2, tail_clean/1]).

-include("jsx_common.hrl").


decoder() ->
    decoder([]).

decoder(Opts) ->
    F = fun(completed_parse, State) -> lists:reverse(State) ;(Event, State) -> [Event] ++ State  end,
    decoder({F, []}, Opts).

decoder({F, _} = Callbacks, OptsList) when is_list(OptsList), is_function(F) ->
    Opts = parse_opts(OptsList),
    decoder(Callbacks, Opts);
decoder({{Mod, Fun}, State}, OptsList) when is_list(OptsList), is_atom(Mod), is_atom(Fun) ->
    Opts = parse_opts(OptsList),
    decoder({fun(E, S) -> Mod:Fun(E, S) end, State}, Opts);
decoder(Callbacks, Opts) ->
    case Opts#opts.encoding of
        utf8 ->
            fun(Stream) -> jsx_decoder:start(Stream, [], Callbacks, Opts) end
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


%% ensures there's no invalid characters left in the stream upon completion of parsing

tail_clean(<<X/utf8, Rest/binary>>) when ?is_whitespace(X) ->
    tail_clean(Rest);
tail_clean(<<>>) ->
    true;
tail_clean(_) ->
    false.
