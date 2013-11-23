%% The MIT License

%% Copyright (c) 2010-2013 alisdair sullivan <alisdairsullivan@yahoo.ca>

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


-module(jsx_config).

-export([parse_config/1]).
-export([config_to_list/1]).
-export([extract_config/1, valid_flags/0]).

-ifdef(TEST).
-export([fake_error_handler/3]).
-endif.

-include("jsx_config.hrl").

-type handler_type(Handler) ::
    fun((jsx:json_text() | end_stream |
         jsx:json_term(),
         {decoder, any(), module(), null | list(), list()} |
         {parser, any(), module(), list()} |
         {encoder, any(), module()},
         list({pre_encode, fun((any()) -> any())} |
              {error_handler, Handler} |
              {incomplete_handler, Handler} |
              atom())) -> any()).
-type handler() :: handler_type(handler()).
-export_type([handler/0]).

%% parsing of jsx config
-spec parse_config(Config::proplists:proplist()) -> jsx:config().

parse_config(Config) ->
    parse_config(Config, #config{}).

parse_config([], Config) ->
    Config;
parse_config([replaced_bad_utf8|Rest], Config) ->
    parse_config(Rest, Config#config{replaced_bad_utf8=true});
parse_config([escaped_forward_slashes|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_forward_slashes=true});
parse_config([explicit_end|Rest], Config) ->
    parse_config(Rest, Config#config{explicit_end=true});
parse_config([single_quoted_strings|Rest], Config) ->
    parse_config(Rest, Config#config{single_quoted_strings=true});
parse_config([unescaped_jsonp|Rest], Config) ->
    parse_config(Rest, Config#config{unescaped_jsonp=true});
parse_config([comments|Rest], Config) ->
    parse_config(Rest, Config#config{comments=true});
parse_config([escaped_strings|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_strings=true});
parse_config([dirty_strings|Rest], Config) ->
    parse_config(Rest, Config#config{dirty_strings=true});
parse_config([ignored_bad_escapes|Rest], Config) ->
    parse_config(Rest, Config#config{ignored_bad_escapes=true});
parse_config([relax|Rest], Config) ->
    parse_config(Rest, Config#config{
        replaced_bad_utf8 = true,
        single_quoted_strings = true,
        comments = true,
        ignored_bad_escapes = true
    });
parse_config([{pre_encode, Encoder}|Rest] = Options, Config) when is_function(Encoder, 1) ->
    case Config#config.pre_encode of
        false -> parse_config(Rest, Config#config{pre_encode=Encoder})
        ; _ -> erlang:error(badarg, [Options, Config])
    end;
parse_config([{error_handler, ErrorHandler}|Rest] = Options, Config) when is_function(ErrorHandler, 3) ->
    case Config#config.error_handler of
        false -> parse_config(Rest, Config#config{error_handler=ErrorHandler})
        ; _ -> erlang:error(badarg, [Options, Config])
    end;
parse_config([{incomplete_handler, IncompleteHandler}|Rest] = Options, Config) when is_function(IncompleteHandler, 3) ->
    case Config#config.incomplete_handler of
        false -> parse_config(Rest, Config#config{incomplete_handler=IncompleteHandler})
        ; _ -> erlang:error(badarg, [Options, Config])
    end;
%% deprecated flags
parse_config([{pre_encoder, Encoder}|Rest] = Options, Config) when is_function(Encoder, 1) ->
    case Config#config.pre_encode of
        false -> parse_config(Rest, Config#config{pre_encode=Encoder})
        ; _ -> erlang:error(badarg, [Options, Config])
    end;
parse_config([loose_unicode|Rest], Config) ->
    parse_config(Rest, Config#config{replaced_bad_utf8=true});
parse_config([escape_forward_slash|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_forward_slashes=true});
parse_config([single_quotes|Rest], Config) ->
    parse_config(Rest, Config#config{single_quoted_strings=true});
parse_config([no_jsonp_escapes|Rest], Config) ->
    parse_config(Rest, Config#config{unescaped_jsonp=true});
parse_config([json_escape|Rest], Config) ->
    parse_config(Rest, Config#config{escaped_strings=true});
parse_config([ignore_bad_escapes|Rest], Config) ->
    parse_config(Rest, Config#config{ignored_bad_escapes=true});
parse_config(Options, Config) ->
    erlang:error(badarg, [Options, Config]).


-spec config_to_list(Config::jsx:config()) -> proplists:proplist().

config_to_list(Config) ->
    lists:map(
        fun ({pre_encode, F}) -> {pre_encode, F};
            ({error_handler, F}) -> {error_handler, F};
            ({incomplete_handler, F}) -> {incomplete_handler, F};
            ({Key, true}) -> Key
        end,
        lists:filter(
            fun({_, false}) -> false; (_) -> true end,
            lists:zip(record_info(fields, config), tl(tuple_to_list(Config)))
        )
    ).


-spec valid_flags() -> [atom()].

valid_flags() ->
    [
        replaced_bad_utf8,
        escaped_forward_slashes,
        single_quoted_strings,
        unescaped_jsonp,
        comments,
        escaped_strings,
        dirty_strings,
        ignored_bad_escapes,
        explicit_end,
        relax,
        pre_encode,
        error_handler,
        incomplete_handler,
        %% deprecated flags
        pre_encoder,            %% pre_encode
        loose_unicode,          %% replaced_bad_utf8
        escape_forward_slash,   %% escaped_forward_slashes
        single_quotes,          %% single_quoted_strings
        no_jsonp_escapes,       %% unescaped_jsonp
        json_escape,            %% escaped_strings
        ignore_bad_escapes      %% ignored_bad_escapes
    ].


-spec extract_config(Config::proplists:proplist()) -> proplists:proplist().

extract_config(Config) ->
    extract_parser_config(Config, []).

extract_parser_config([], Acc) -> Acc;
extract_parser_config([{K,V}|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_config(Rest, [{K,V}] ++ Acc)
        ; false -> extract_parser_config(Rest, Acc)
    end;
extract_parser_config([K|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_config(Rest, [K] ++ Acc)
        ; false -> extract_parser_config(Rest, Acc)
    end.


%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"all flags",
            ?_assertEqual(
                #config{
                    replaced_bad_utf8=true,
                    escaped_forward_slashes=true,
                    explicit_end=true,
                    single_quoted_strings=true,
                    unescaped_jsonp=true,
                    comments=true,
                    dirty_strings=true,
                    ignored_bad_escapes=true
                },
                parse_config([
                    replaced_bad_utf8,
                    escaped_forward_slashes,
                    explicit_end,
                    single_quoted_strings,
                    unescaped_jsonp,
                    comments,
                    dirty_strings,
                    ignored_bad_escapes
                ])
            )
        },
        {"relax flag",
            ?_assertEqual(
                #config{
                    replaced_bad_utf8=true,
                    single_quoted_strings=true,
                    comments=true,
                    ignored_bad_escapes=true
                },
                parse_config([relax])
            )
        },
        {"deprecated flags", ?_assertEqual(
            #config{
                pre_encode=fun lists:length/1,
                replaced_bad_utf8=true,
                escaped_forward_slashes=true,
                single_quoted_strings=true,
                unescaped_jsonp=true,
                escaped_strings=true,
                ignored_bad_escapes=true
            },
            parse_config([
                {pre_encoder, fun lists:length/1},
                loose_unicode,
                escape_forward_slash,
                single_quotes,
                no_jsonp_escapes,
                json_escape,
                ignore_bad_escapes
            ])
        )},
        {"pre_encode flag", ?_assertEqual(
            #config{pre_encode=fun lists:length/1},
            parse_config([{pre_encode, fun lists:length/1}])
        )},
        {"two pre_encoders defined", ?_assertError(
            badarg,
            parse_config([
                {pre_encode, fun(_) -> true end},
                {pre_encode, fun(_) -> false end}
            ])
        )},
        {"error_handler flag", ?_assertEqual(
            #config{error_handler=fun ?MODULE:fake_error_handler/3},
            parse_config([{error_handler, fun ?MODULE:fake_error_handler/3}])
        )},
        {"two error_handlers defined", ?_assertError(
            badarg,
            parse_config([
                {error_handler, fun(_) -> true end},
                {error_handler, fun(_) -> false end}
            ])
        )},
        {"incomplete_handler flag", ?_assertEqual(
            #config{incomplete_handler=fun ?MODULE:fake_error_handler/3},
            parse_config([{incomplete_handler, fun ?MODULE:fake_error_handler/3}])
        )},
        {"two incomplete_handlers defined", ?_assertError(
            badarg,
            parse_config([
                {incomplete_handler, fun(_) -> true end},
                {incomplete_handler, fun(_) -> false end}
            ])
        )},
        {"bad option flag", ?_assertError(badarg, parse_config([error]))}
    ].


config_to_list_test_() ->
    [
        {"empty config", ?_assertEqual(
            [],
            config_to_list(#config{})
        )},
        {"all flags", ?_assertEqual(
            [
                replaced_bad_utf8,
                escaped_forward_slashes,
                single_quoted_strings,
                unescaped_jsonp,
                comments,
                dirty_strings,
                ignored_bad_escapes,
                explicit_end
            ],
            config_to_list(
                #config{
                    replaced_bad_utf8=true,
                    escaped_forward_slashes=true,
                    explicit_end=true,
                    single_quoted_strings=true,
                    unescaped_jsonp=true,
                    comments=true,
                    dirty_strings=true,
                    ignored_bad_escapes=true
                }
            )
        )},
        {"pre_encode", ?_assertEqual(
            [{pre_encode, fun lists:length/1}],
            config_to_list(#config{pre_encode=fun lists:length/1})
        )},
        {"error handler", ?_assertEqual(
            [{error_handler, fun ?MODULE:fake_error_handler/3}],
            config_to_list(#config{error_handler=fun ?MODULE:fake_error_handler/3})
        )},
        {"incomplete handler", ?_assertEqual(
            [{incomplete_handler, fun ?MODULE:fake_error_handler/3}],
            config_to_list(#config{incomplete_handler=fun ?MODULE:fake_error_handler/3})
        )}
    ].


fake_error_handler(_, _, _) -> ok.


-endif.
