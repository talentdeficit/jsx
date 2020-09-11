%% The MIT License

%% Copyright (c) 2010-2015 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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


-module(jsx_consult).

-export([consult/2]).
-export([init/1, reset/1, handle_event/2]).


-record(config, {
    labels = binary,
    return_maps = false
}).

-type config() :: proplists:proplist().
-export_type([config/0]).

-type json_value() :: list(json_value())
    | map()
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
-export_type([json_value/0]).

opts(Opts) -> [return_maps, multi_term] ++ Opts.

-spec consult(File::file:name_all(), Config::config()) -> [json_value()].

consult(File, Config) when is_list(Config) ->
    case file:read_file(File) of
      {ok, Bin}  ->
          {Final, _, _} = (jsx:decoder(
              ?MODULE,
              opts(Config),
              jsx_config:extract_config(opts(Config))
          ))(Bin),
          lists:reverse(Final);
      {error, _} -> erlang:error(badarg)
    end.


-type state() :: {[], config(), {list(), #config{}}}.
-spec init(Config::config()) -> state().

init(Config) -> {[], Config, jsx_to_term:start_term(Config)}.


-spec reset(State::state()) -> state().

reset({Acc, Config, _}) -> {Acc, Config, jsx_to_term:start_term(Config)}.


-spec handle_event(Event::any(), State::state()) -> state().

handle_event(end_json, {Acc, Config, State}) ->
    {[jsx_to_term:get_value(State)] ++ Acc, Config, State};
handle_event(Event, {Acc, Config, State}) ->
    {Acc, Config, jsx_to_term:handle_event(Event, State)}.
