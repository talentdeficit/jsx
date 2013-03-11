%% The MIT License

%% Copyright (c) 2010-2013 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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


-module(jsx_to_term).

-export([to_term/2]).
-export([init/1, handle_event/2]).


-record(config, {
    labels = binary
}).

-type config() :: list().

-type json_value() :: list({binary(), json_value()})
    | list(json_value())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().


-spec to_term(Source::binary(), Config::config()) -> json_value().

to_term(Source, Config) when is_list(Config) ->
    (jsx:decoder(?MODULE, Config, jsx_config:extract_config(Config)))(Source).


parse_config(Config) -> parse_config(Config, #config{}).

parse_config([{labels, Val}|Rest], Config)
        when Val == binary; Val == atom; Val == existing_atom; Val == attempt_atom ->
    parse_config(Rest, Config#config{labels = Val});
parse_config([labels|Rest], Config) ->
    parse_config(Rest, Config#config{labels = binary});
parse_config([{K, _}|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config)
        ; false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([K|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config)
        ; false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([], Config) ->
    Config.


init(Config) -> {[[]], parse_config(Config)}.


handle_event(end_json, {[[Terms]], _Config}) -> Terms;

handle_event(start_object, {Terms, Config}) -> {[[]|Terms], Config};
handle_event(end_object, {[[], {key, Key}, Last|Terms], Config}) ->
    {[[{Key, [{}]}] ++ Last] ++ Terms, Config};
handle_event(end_object, {[Object, {key, Key}, Last|Terms], Config}) ->
    {[[{Key, lists:reverse(Object)}] ++ Last] ++ Terms, Config};
handle_event(end_object, {[[], Last|Terms], Config}) ->
    {[[[{}]] ++ Last] ++ Terms, Config};
handle_event(end_object, {[Object, Last|Terms], Config}) ->
    {[[lists:reverse(Object)] ++ Last] ++ Terms, Config};

handle_event(start_array, {Terms, Config}) -> {[[]|Terms], Config};
handle_event(end_array, {[List, {key, Key}, Last|Terms], Config}) ->
    {[[{Key, lists:reverse(List)}] ++ Last] ++ Terms, Config};
handle_event(end_array, {[List, Last|Terms], Config}) ->
    {[[lists:reverse(List)] ++ Last] ++ Terms, Config};

handle_event({key, Key}, {Terms, Config}) -> {[{key, format_key(Key, Config)}] ++ Terms, Config};

handle_event({_, Event}, {[{key, Key}, Last|Terms], Config}) ->
    {[[{Key, Event}] ++ Last] ++ Terms, Config};
handle_event({_, Event}, {[Last|Terms], Config}) ->
    {[[Event] ++ Last] ++ Terms, Config}.


format_key(Key, Config) ->
    case Config#config.labels of
        binary -> Key
        ; atom -> binary_to_atom(Key, utf8)
        ; existing_atom -> binary_to_existing_atom(Key, utf8)
        ; attempt_atom ->
            try binary_to_existing_atom(Key, utf8) of
                Result -> Result
            catch
                error:badarg -> Key
            end
    end.



%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"empty config", ?_assertEqual(#config{}, parse_config([]))},
        {"implicit binary labels", ?_assertEqual(#config{}, parse_config([labels]))},
        {"binary labels", ?_assertEqual(#config{}, parse_config([{labels, binary}]))},
        {"atom labels", ?_assertEqual(#config{labels=atom}, parse_config([{labels, atom}]))},
        {"existing atom labels", ?_assertEqual(
            #config{labels=existing_atom},
            parse_config([{labels, existing_atom}])
        )},
        {"invalid opt flag", ?_assertError(badarg, parse_config([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_config([{error, true}]))}
    ].


format_key_test_() ->
    [
        {"binary key", ?_assertEqual(<<"key">>, format_key(<<"key">>, #config{labels=binary}))},
        {"atom key", ?_assertEqual(key, format_key(<<"key">>, #config{labels=atom}))},
        {"existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #config{labels=existing_atom})
        )},
        {"nonexisting atom key", ?_assertError(
            badarg,
            format_key(<<"nonexistentatom">>, #config{labels=existing_atom})
        )},
        {"sloppy existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #config{labels=attempt_atom})
        )},
        {"nonexisting atom key", ?_assertEqual(
            <<"nonexistentatom">>,
            format_key(<<"nonexistentatom">>, #config{labels=attempt_atom})
        )}
    ].


handle_event_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                Term,
                lists:foldl(fun handle_event/2, {[[]], #config{}}, Events ++ [end_json])
            )
        } || {Title, _, Term, Events} <- Data
    ].


-endif.
