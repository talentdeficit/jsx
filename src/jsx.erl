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


-module(jsx).

-export([encode/1, encode/2, decode/1, decode/2]).
-export([is_json/1, is_json/2, is_term/1, is_term/2]).
-export([format/1, format/2, minify/1, prettify/1]).
-export([encoder/3, decoder/3, parser/3]).
-export([resume/3]).
%% old api
-export([term_to_json/1, term_to_json/2, json_to_term/1, json_to_term/2]).
-export([to_json/1, to_json/2]).
-export([to_term/1, to_term/2]).

-export_type([json_term/0, json_text/0, token/0]).
-export_type([config/0, encoder/0, decoder/0, parser/0, internal_state/0]).


-ifdef(TEST).
-include("jsx_tests.hrl").
-else.
-include("jsx_config.hrl").
-endif.

-type config() :: #config{}.

-type json_term()
   :: [{binary() | atom(), json_term()}]
    | [json_term()]
    | true
    | false
    | null
    | integer()
    | float()
    | binary().

-type json_text() :: binary().

-spec encode(Source::json_term()) -> json_text() | {incomplete, encoder()}.
-spec encode(Source::json_term(), Config::jsx_to_json:config()) -> json_text() | {incomplete, encoder()}.

encode(Source) -> encode(Source, []).
encode(Source, Config) -> jsx_to_json:to_json(Source, Config).

%% old api, alias for encode/x

-spec to_json(Source::json_term()) -> json_text() | {incomplete, encoder()}.
-spec to_json(Source::json_term(), Config::jsx_to_json:config()) -> json_text() | {incomplete, encoder()}.

to_json(Source) -> encode(Source, []).
to_json(Source, Config) -> encode(Source, Config).

-spec term_to_json(Source::json_term()) -> json_text() | {incomplete, encoder()}.
-spec term_to_json(Source::json_term(), Config::jsx_to_json:config()) -> json_text() | {incomplete, encoder()}.

term_to_json(Source) -> encode(Source, []).
term_to_json(Source, Config) -> encode(Source, Config).


-spec format(Source::json_text()) -> json_text() | {incomplete, decoder()}.
-spec format(Source::json_text(), Config::jsx_to_json:config()) -> json_text() | {incomplete, decoder()}.

format(Source) -> format(Source, []).
format(Source, Config) -> jsx_to_json:format(Source, Config).


-spec minify(Source::json_text()) -> json_text()  | {incomplete, decoder()}.

minify(Source) -> format(Source, []).


-spec prettify(Source::json_text()) -> json_text() | {incomplete, decoder()}.

prettify(Source) -> format(Source, [space, {indent, 2}]).


-spec decode(Source::json_text()) -> json_term() | {incomplete, decoder()}.
-spec decode(Source::json_text(), Config::jsx_to_term:config()) -> json_term()  | {incomplete, decoder()}.

decode(Source) -> decode(Source, []).
decode(Source, Config) -> jsx_to_term:to_term(Source, Config).

%% old api, alias for to_term/x

-spec to_term(Source::json_text()) -> json_term() | {incomplete, decoder()}.
-spec to_term(Source::json_text(), Config::jsx_to_term:config()) -> json_term()  | {incomplete, decoder()}.

to_term(Source) -> decode(Source, []).
to_term(Source, Config) -> decode(Source, Config).

-spec json_to_term(Source::json_text()) -> json_term() | {incomplete, decoder()}.
-spec json_to_term(Source::json_text(), Config::jsx_to_term:config()) -> json_term()  | {incomplete, decoder()}.

json_to_term(Source) -> decode(Source, []).
json_to_term(Source, Config) -> decode(Source, Config).


-spec is_json(Source::any()) -> true | false.
-spec is_json(Source::any(), Config::jsx_verify:config()) -> true | false.

is_json(Source) -> is_json(Source, []).
is_json(Source, Config) -> jsx_verify:is_json(Source, Config).


-spec is_term(Source::any()) -> true | false.
-spec is_term(Source::any(), Config::jsx_verify:config()) -> true | false.

is_term(Source) -> is_term(Source, []).
is_term(Source, Config) -> jsx_verify:is_term(Source, Config).


-type decoder() :: fun((json_text() | end_stream) -> any()).

-spec decoder(Handler::module(), State::any(), Config::list()) -> decoder().

decoder(Handler, State, Config) -> jsx_decoder:decoder(Handler, State, Config).


-type encoder() :: fun((json_term() | end_stream) -> any()).

-spec encoder(Handler::module(), State::any(), Config::list()) -> encoder().

encoder(Handler, State, Config) -> jsx_encoder:encoder(Handler, State, Config).


-type token() :: [token()]
    | start_object
    | end_object
    | start_array
    | end_array
    | {key, binary()}
    | {string, binary()}
    | binary()
    | {number, integer() | float()}
    | {integer, integer()}
    | {float, float()}
    | integer()
    | float()
    | {literal, true}
    | {literal, false}
    | {literal, null}
    | true
    | false
    | null
    | end_json.


-type parser() :: fun((token() | end_stream) -> any()).

-spec parser(Handler::module(), State::any(), Config::list()) -> parser().

parser(Handler, State, Config) -> jsx_parser:parser(Handler, State, Config).

-opaque internal_state() :: tuple().

-spec resume(Term::json_text() | token(), InternalState::internal_state(), Config::list()) -> any().

resume(Term, {decoder, State, Handler, Acc, Stack}, Config) ->
    jsx_decoder:resume(Term, State, Handler, Acc, Stack, jsx_config:parse_config(Config));
resume(Term, {parser, State, Handler, Stack}, Config) ->
    jsx_parser:resume(Term, State, Handler, Stack, jsx_config:parse_config(Config)).

