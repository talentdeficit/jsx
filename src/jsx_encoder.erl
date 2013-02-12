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


-module(jsx_encoder).

-export([encoder/3, pre_encode/2]).

-spec encoder(Handler::module(), State::any(), Opts::jsx:opts()) -> jsx:encoder().

encoder(Handler, State, Opts) ->
    fun(JSON) ->
        start(
            JSON,
            {Handler, Handler:init(State)},
            jsx_utils:parse_opts(Opts)
        )
    end.



-include("jsx_opts.hrl").


-ifndef(error).
-define(error(Args),
    erlang:error(badarg, Args)
).
-endif.


start(Term, {Handler, State}, Opts) ->
    Handler:handle_event(end_json, value(pre_encode(Term, Opts), {Handler, State}, Opts)).


value(String, {Handler, State}, Opts) when is_binary(String) ->
    Handler:handle_event({string, clean_string(String, Opts)}, State);
value(Float, {Handler, State}, _Opts) when is_float(Float) ->
    Handler:handle_event({float, Float}, State);
value(Int, {Handler, State}, _Opts) when is_integer(Int) ->
    Handler:handle_event({integer, Int}, State);
value(Literal, {Handler, State}, _Opts)
        when Literal == true; Literal == false; Literal == null ->
    Handler:handle_event({literal, Literal}, State);
value([{}], {Handler, State}, _Opts) ->
    Handler:handle_event(end_object, Handler:handle_event(start_object, State));
value([], {Handler, State}, _Opts) ->
    Handler:handle_event(end_array, Handler:handle_event(start_array, State));
value([Tuple|_] = List, Handler, Opts) when is_tuple(Tuple) ->
    list_or_object(List, Handler, Opts);
value(List, Handler, Opts) when is_list(List) ->
    list_or_object(List, Handler, Opts);
value(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


list_or_object([Term|Rest], {Handler, State}, Opts) ->
    case pre_encode(Term, Opts) of
        {K, V} ->
            object([{K, V}|Rest], {Handler, Handler:handle_event(start_object, State)}, Opts)
        ; T ->
            list([T|Rest], {Handler, Handler:handle_event(start_array, State)}, Opts)
    end.


object([{Key, Value}, Next|Rest], {Handler, State}, Opts) when is_atom(Key); is_binary(Key) ->
    V = pre_encode(Value, Opts),
    object(
        [pre_encode(Next, Opts)|Rest],
        {
            Handler,
            value(
                V,
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), Opts)}, State)},
                Opts
            )
        },
        Opts
    );
object([{Key, Value}], {Handler, State}, Opts) when is_atom(Key); is_binary(Key) ->
    object(
        [],
        {
            Handler,
            value(
                pre_encode(Value, Opts),
                {Handler, Handler:handle_event({key, clean_string(fix_key(Key), Opts)}, State)},
                Opts
            )
        },
        Opts
    );
object([], {Handler, State}, _Opts) -> Handler:handle_event(end_object, State);
object(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


list([Value, Next|Rest], {Handler, State}, Opts) ->
    list([pre_encode(Next, Opts)|Rest], {Handler, value(Value, {Handler, State}, Opts)}, Opts);
list([Value], {Handler, State}, Opts) ->
	list([], {Handler, value(Value, {Handler, State}, Opts)}, Opts);
list([], {Handler, State}, _Opts) -> Handler:handle_event(end_array, State);
list(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


pre_encode(Value, #opts{pre_encode=false}) -> io:format("~p~n", [Value]), Value;
pre_encode(Value, Opts) -> (Opts#opts.pre_encode)(Value).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Opts) -> jsx_utils:clean_string(Bin, Opts).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


encode_test_() ->
    Data = jsx:empty_array()
        ++ jsx:deep_array()
        ++ jsx:really_deep_array()
        ++ jsx:empty_object()
        ++ jsx:literals()
        ++ jsx:naked_literals()
        ++ jsx:integers()
        ++ jsx:naked_integers()
        ++ jsx:floats()
        ++ jsx:naked_floats(),
    [
        {
            Title, ?_assertEqual(
                Events ++ [end_json],
                start(Term, {jsx, []}, #opts{})
            )
        } || {Title, _, Term, Events} <- Data
    ].


-endif.