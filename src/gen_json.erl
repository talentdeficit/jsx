%% The MIT License

%% Copyright (c) 2011 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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


-module(gen_json).

-export([behaviour_info/1]).
-export([parser/1, parser/2, parser/3]).
-export([handle_event/2, init/1]).


-type events() :: [event()].
-type event() :: start_object
    | end_object
    | start_array
    | end_array
    | end_json
    | {key, list()}
    | {string, list()}
    | {integer, integer()}
    | {float, float()}
    | {literal, true}
    | {literal, false}
    | {literal, null}.

-type opts() :: [opt()].
-type opt() :: loose_unicode
        | escape_forward_slashes
        | explicit_end
        | {parser, auto} | {parser, encoder} | {parser, decoder} | {parser, function()}.

-export_type([events/0, event/0, opts/0, opt/0]).


behaviour_info(callbacks) -> [{init, 0}, {handle_event, 2}];
behaviour_info(_) -> undefined.


parser(F) -> parser(F, []).

parser(F, Opts) when is_function(F, 1) -> parser(?MODULE, {F, undefined}, Opts);
parser({F, State}, Opts) when is_function(F, 2) -> parser(?MODULE, {F, State}, Opts);
parser(Mod, Args) -> parser(Mod, Args, []).

parser(Mod, Args, Opts) when is_atom(Mod), is_list(Opts) ->
    case proplists:get_value(parser, Opts, auto) of
        auto ->
            fun(Input) when is_list(Input) -> (jsx_encoder:encoder(Mod, Mod:init(Args), Opts))(Input)
                ; (Input) when is_binary(Input) -> (jsx_decoder:decoder(Mod, Mod:init(Args), Opts))(Input)
            end
        ; encoder ->
            fun(Input) -> (jsx_encoder:encoder(Mod, Mod:init(Args), Opts))(Input) end
        ; decoder ->
            fun(Input) -> (jsx_decoder:decoder(Mod, Mod:init(Args), Opts))(Input) end
    end.


handle_event(Event, {F, undefined}) -> F(Event), {F, undefined};
handle_event(Event, {F, State}) -> {F, F(Event, State)}.

init(State) -> State.