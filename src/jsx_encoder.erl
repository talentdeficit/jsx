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

-export([encoder/3]).

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
value(List, {Handler, State}, Opts) when is_list(List) ->
    list_or_object(List, {Handler, State}, Opts);
value(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


list_or_object([Tuple|_] = List, {Handler, State}, Opts) when is_tuple(Tuple) ->
    object(List, {Handler, Handler:handle_event(start_object, State)}, Opts);
list_or_object(List, {Handler, State}, Opts) ->
    list(List, {Handler, Handler:handle_event(start_array, State)}, Opts).


object([{Key, Value}|Rest], {Handler, State}, Opts) ->
    object(
        Rest,
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


list([Value|Rest], {Handler, State}, Opts) ->
    list(Rest, {Handler, value(pre_encode(Value, Opts), {Handler, State}, Opts)}, Opts);
list([], {Handler, State}, _Opts) -> Handler:handle_event(end_array, State);
list(Term, Handler, Opts) -> ?error([Term, Handler, Opts]).


pre_encode(Value, #opts{pre_encode=false}) -> Value;
pre_encode(Value, Opts) -> (Opts#opts.pre_encode)(Value).


fix_key(Key) when is_atom(Key) -> fix_key(atom_to_binary(Key, utf8));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, Opts) ->
    case Opts#opts.replaced_bad_utf8 orelse Opts#opts.escaped_strings of
        true -> clean(Bin, [], Opts)
        ; false -> ensure_clean(Bin), Bin
    end.


%% fast path for no escaping and no correcting, throws error if string is 'bad'
ensure_clean(<<>>) -> ok;
ensure_clean(<<0, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<1, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<2, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<3, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<4, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<5, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<6, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<7, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<8, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<9, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<10, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<11, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<12, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<13, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<14, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<15, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<16, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<17, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<18, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<19, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<20, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<21, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<22, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<23, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<24, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<25, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<26, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<27, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<28, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<29, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<30, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<31, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<32, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<33, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<34, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<35, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<36, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<37, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<38, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<39, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<40, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<41, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<42, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<43, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<44, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<45, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<46, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<47, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<48, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<49, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<50, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<51, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<52, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<53, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<54, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<55, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<56, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<57, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<58, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<59, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<60, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<61, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<62, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<63, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<64, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<65, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<66, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<67, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<68, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<69, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<70, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<71, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<72, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<73, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<74, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<75, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<76, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<77, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<78, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<79, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<80, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<81, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<82, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<83, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<84, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<85, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<86, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<87, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<88, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<89, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<90, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<91, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<92, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<93, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<94, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<95, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<96, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<97, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<98, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<99, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<100, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<101, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<102, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<103, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<104, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<105, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<106, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<107, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<108, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<109, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<110, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<111, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<112, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<113, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<114, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<115, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<116, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<117, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<118, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<119, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<120, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<121, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<122, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<123, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<124, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<125, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<126, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<127, Rest/binary>>) -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X < 16#800 -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X < 16#dcff -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X > 16#dfff, X < 16#fdd0 -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X > 16#fdef, X < 16#fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#10000, X < 16#1fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#20000, X < 16#2fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#30000, X < 16#3fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#40000, X < 16#4fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#50000, X < 16#5fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#60000, X < 16#6fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#70000, X < 16#7fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#80000, X < 16#8fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#90000, X < 16#9fffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#a0000, X < 16#afffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#b0000, X < 16#bfffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#c0000, X < 16#cfffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#d0000, X < 16#dfffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#e0000, X < 16#efffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#f0000, X < 16#ffffe -> ensure_clean(Rest);
ensure_clean(<<X/utf8, Rest/binary>>) when X >= 16#100000, X < 16#10fffe -> ensure_clean(Rest);
ensure_clean(Bin) -> erlang:error(badarg, [Bin]).


%% escape and/or replace bad codepoints if requested
clean(<<>>, Acc, _Opts) -> unicode:characters_to_binary(lists:reverse(Acc));
clean(<<0, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(0, Opts) ++ Acc, Opts);
clean(<<1, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(1, Opts) ++ Acc, Opts);
clean(<<2, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(2, Opts) ++ Acc, Opts);
clean(<<3, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(3, Opts) ++ Acc, Opts);
clean(<<4, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(4, Opts) ++ Acc, Opts);
clean(<<5, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(5, Opts) ++ Acc, Opts);
clean(<<6, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(6, Opts) ++ Acc, Opts);
clean(<<7, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(7, Opts) ++ Acc, Opts);
clean(<<8, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(8, Opts) ++ Acc, Opts);
clean(<<9, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(9, Opts) ++ Acc, Opts);
clean(<<10, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(10, Opts) ++ Acc, Opts);
clean(<<11, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(11, Opts) ++ Acc, Opts);
clean(<<12, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(12, Opts) ++ Acc, Opts);
clean(<<13, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(13, Opts) ++ Acc, Opts);
clean(<<14, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(14, Opts) ++ Acc, Opts);
clean(<<15, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(15, Opts) ++ Acc, Opts);
clean(<<16, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(16, Opts) ++ Acc, Opts);
clean(<<17, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(17, Opts) ++ Acc, Opts);
clean(<<18, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(18, Opts) ++ Acc, Opts);
clean(<<19, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(19, Opts) ++ Acc, Opts);
clean(<<20, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(20, Opts) ++ Acc, Opts);
clean(<<21, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(21, Opts) ++ Acc, Opts);
clean(<<22, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(22, Opts) ++ Acc, Opts);
clean(<<23, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(23, Opts) ++ Acc, Opts);
clean(<<24, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(24, Opts) ++ Acc, Opts);
clean(<<25, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(25, Opts) ++ Acc, Opts);
clean(<<26, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(26, Opts) ++ Acc, Opts);
clean(<<27, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(27, Opts) ++ Acc, Opts);
clean(<<28, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(28, Opts) ++ Acc, Opts);
clean(<<29, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(29, Opts) ++ Acc, Opts);
clean(<<30, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(30, Opts) ++ Acc, Opts);
clean(<<31, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(31, Opts) ++ Acc, Opts);
clean(<<32, Rest/binary>>, Acc, Opts) -> clean(Rest, [32] ++ Acc, Opts);
clean(<<33, Rest/binary>>, Acc, Opts) -> clean(Rest, [33] ++ Acc, Opts);
clean(<<34, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(34, Opts) ++ Acc, Opts);
clean(<<35, Rest/binary>>, Acc, Opts) -> clean(Rest, [35] ++ Acc, Opts);
clean(<<36, Rest/binary>>, Acc, Opts) -> clean(Rest, [36] ++ Acc, Opts);
clean(<<37, Rest/binary>>, Acc, Opts) -> clean(Rest, [37] ++ Acc, Opts);
clean(<<38, Rest/binary>>, Acc, Opts) -> clean(Rest, [38] ++ Acc, Opts);
clean(<<39, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(39, Opts) ++ Acc, Opts);
clean(<<40, Rest/binary>>, Acc, Opts) -> clean(Rest, [40] ++ Acc, Opts);
clean(<<41, Rest/binary>>, Acc, Opts) -> clean(Rest, [41] ++ Acc, Opts);
clean(<<42, Rest/binary>>, Acc, Opts) -> clean(Rest, [42] ++ Acc, Opts);
clean(<<43, Rest/binary>>, Acc, Opts) -> clean(Rest, [43] ++ Acc, Opts);
clean(<<44, Rest/binary>>, Acc, Opts) -> clean(Rest, [44] ++ Acc, Opts);
clean(<<45, Rest/binary>>, Acc, Opts) -> clean(Rest, [45] ++ Acc, Opts);
clean(<<46, Rest/binary>>, Acc, Opts) -> clean(Rest, [46] ++ Acc, Opts);
clean(<<47, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(47, Opts) ++ Acc, Opts);
clean(<<48, Rest/binary>>, Acc, Opts) -> clean(Rest, [48] ++ Acc, Opts);
clean(<<49, Rest/binary>>, Acc, Opts) -> clean(Rest, [49] ++ Acc, Opts);
clean(<<50, Rest/binary>>, Acc, Opts) -> clean(Rest, [50] ++ Acc, Opts);
clean(<<51, Rest/binary>>, Acc, Opts) -> clean(Rest, [51] ++ Acc, Opts);
clean(<<52, Rest/binary>>, Acc, Opts) -> clean(Rest, [52] ++ Acc, Opts);
clean(<<53, Rest/binary>>, Acc, Opts) -> clean(Rest, [53] ++ Acc, Opts);
clean(<<54, Rest/binary>>, Acc, Opts) -> clean(Rest, [54] ++ Acc, Opts);
clean(<<55, Rest/binary>>, Acc, Opts) -> clean(Rest, [55] ++ Acc, Opts);
clean(<<56, Rest/binary>>, Acc, Opts) -> clean(Rest, [56] ++ Acc, Opts);
clean(<<57, Rest/binary>>, Acc, Opts) -> clean(Rest, [57] ++ Acc, Opts);
clean(<<58, Rest/binary>>, Acc, Opts) -> clean(Rest, [58] ++ Acc, Opts);
clean(<<59, Rest/binary>>, Acc, Opts) -> clean(Rest, [59] ++ Acc, Opts);
clean(<<60, Rest/binary>>, Acc, Opts) -> clean(Rest, [60] ++ Acc, Opts);
clean(<<61, Rest/binary>>, Acc, Opts) -> clean(Rest, [61] ++ Acc, Opts);
clean(<<62, Rest/binary>>, Acc, Opts) -> clean(Rest, [62] ++ Acc, Opts);
clean(<<63, Rest/binary>>, Acc, Opts) -> clean(Rest, [63] ++ Acc, Opts);
clean(<<64, Rest/binary>>, Acc, Opts) -> clean(Rest, [64] ++ Acc, Opts);
clean(<<65, Rest/binary>>, Acc, Opts) -> clean(Rest, [65] ++ Acc, Opts);
clean(<<66, Rest/binary>>, Acc, Opts) -> clean(Rest, [66] ++ Acc, Opts);
clean(<<67, Rest/binary>>, Acc, Opts) -> clean(Rest, [67] ++ Acc, Opts);
clean(<<68, Rest/binary>>, Acc, Opts) -> clean(Rest, [68] ++ Acc, Opts);
clean(<<69, Rest/binary>>, Acc, Opts) -> clean(Rest, [69] ++ Acc, Opts);
clean(<<70, Rest/binary>>, Acc, Opts) -> clean(Rest, [70] ++ Acc, Opts);
clean(<<71, Rest/binary>>, Acc, Opts) -> clean(Rest, [71] ++ Acc, Opts);
clean(<<72, Rest/binary>>, Acc, Opts) -> clean(Rest, [72] ++ Acc, Opts);
clean(<<73, Rest/binary>>, Acc, Opts) -> clean(Rest, [73] ++ Acc, Opts);
clean(<<74, Rest/binary>>, Acc, Opts) -> clean(Rest, [74] ++ Acc, Opts);
clean(<<75, Rest/binary>>, Acc, Opts) -> clean(Rest, [75] ++ Acc, Opts);
clean(<<76, Rest/binary>>, Acc, Opts) -> clean(Rest, [76] ++ Acc, Opts);
clean(<<77, Rest/binary>>, Acc, Opts) -> clean(Rest, [77] ++ Acc, Opts);
clean(<<78, Rest/binary>>, Acc, Opts) -> clean(Rest, [78] ++ Acc, Opts);
clean(<<79, Rest/binary>>, Acc, Opts) -> clean(Rest, [79] ++ Acc, Opts);
clean(<<80, Rest/binary>>, Acc, Opts) -> clean(Rest, [80] ++ Acc, Opts);
clean(<<81, Rest/binary>>, Acc, Opts) -> clean(Rest, [81] ++ Acc, Opts);
clean(<<82, Rest/binary>>, Acc, Opts) -> clean(Rest, [82] ++ Acc, Opts);
clean(<<83, Rest/binary>>, Acc, Opts) -> clean(Rest, [83] ++ Acc, Opts);
clean(<<84, Rest/binary>>, Acc, Opts) -> clean(Rest, [84] ++ Acc, Opts);
clean(<<85, Rest/binary>>, Acc, Opts) -> clean(Rest, [85] ++ Acc, Opts);
clean(<<86, Rest/binary>>, Acc, Opts) -> clean(Rest, [86] ++ Acc, Opts);
clean(<<87, Rest/binary>>, Acc, Opts) -> clean(Rest, [87] ++ Acc, Opts);
clean(<<88, Rest/binary>>, Acc, Opts) -> clean(Rest, [88] ++ Acc, Opts);
clean(<<89, Rest/binary>>, Acc, Opts) -> clean(Rest, [89] ++ Acc, Opts);
clean(<<90, Rest/binary>>, Acc, Opts) -> clean(Rest, [90] ++ Acc, Opts);
clean(<<91, Rest/binary>>, Acc, Opts) -> clean(Rest, [91] ++ Acc, Opts);
clean(<<92, Rest/binary>>, Acc, Opts) -> clean(Rest, maybe_replace(92, Opts) ++ Acc, Opts);
clean(<<93, Rest/binary>>, Acc, Opts) -> clean(Rest, [93] ++ Acc, Opts);
clean(<<94, Rest/binary>>, Acc, Opts) -> clean(Rest, [94] ++ Acc, Opts);
clean(<<95, Rest/binary>>, Acc, Opts) -> clean(Rest, [95] ++ Acc, Opts);
clean(<<96, Rest/binary>>, Acc, Opts) -> clean(Rest, [96] ++ Acc, Opts);
clean(<<97, Rest/binary>>, Acc, Opts) -> clean(Rest, [97] ++ Acc, Opts);
clean(<<98, Rest/binary>>, Acc, Opts) -> clean(Rest, [98] ++ Acc, Opts);
clean(<<99, Rest/binary>>, Acc, Opts) -> clean(Rest, [99] ++ Acc, Opts);
clean(<<100, Rest/binary>>, Acc, Opts) -> clean(Rest, [100] ++ Acc, Opts);
clean(<<101, Rest/binary>>, Acc, Opts) -> clean(Rest, [101] ++ Acc, Opts);
clean(<<102, Rest/binary>>, Acc, Opts) -> clean(Rest, [102] ++ Acc, Opts);
clean(<<103, Rest/binary>>, Acc, Opts) -> clean(Rest, [103] ++ Acc, Opts);
clean(<<104, Rest/binary>>, Acc, Opts) -> clean(Rest, [104] ++ Acc, Opts);
clean(<<105, Rest/binary>>, Acc, Opts) -> clean(Rest, [105] ++ Acc, Opts);
clean(<<106, Rest/binary>>, Acc, Opts) -> clean(Rest, [106] ++ Acc, Opts);
clean(<<107, Rest/binary>>, Acc, Opts) -> clean(Rest, [107] ++ Acc, Opts);
clean(<<108, Rest/binary>>, Acc, Opts) -> clean(Rest, [108] ++ Acc, Opts);
clean(<<109, Rest/binary>>, Acc, Opts) -> clean(Rest, [109] ++ Acc, Opts);
clean(<<110, Rest/binary>>, Acc, Opts) -> clean(Rest, [110] ++ Acc, Opts);
clean(<<111, Rest/binary>>, Acc, Opts) -> clean(Rest, [111] ++ Acc, Opts);
clean(<<112, Rest/binary>>, Acc, Opts) -> clean(Rest, [112] ++ Acc, Opts);
clean(<<113, Rest/binary>>, Acc, Opts) -> clean(Rest, [113] ++ Acc, Opts);
clean(<<114, Rest/binary>>, Acc, Opts) -> clean(Rest, [114] ++ Acc, Opts);
clean(<<115, Rest/binary>>, Acc, Opts) -> clean(Rest, [115] ++ Acc, Opts);
clean(<<116, Rest/binary>>, Acc, Opts) -> clean(Rest, [116] ++ Acc, Opts);
clean(<<117, Rest/binary>>, Acc, Opts) -> clean(Rest, [117] ++ Acc, Opts);
clean(<<118, Rest/binary>>, Acc, Opts) -> clean(Rest, [118] ++ Acc, Opts);
clean(<<119, Rest/binary>>, Acc, Opts) -> clean(Rest, [119] ++ Acc, Opts);
clean(<<120, Rest/binary>>, Acc, Opts) -> clean(Rest, [120] ++ Acc, Opts);
clean(<<121, Rest/binary>>, Acc, Opts) -> clean(Rest, [121] ++ Acc, Opts);
clean(<<122, Rest/binary>>, Acc, Opts) -> clean(Rest, [122] ++ Acc, Opts);
clean(<<123, Rest/binary>>, Acc, Opts) -> clean(Rest, [123] ++ Acc, Opts);
clean(<<124, Rest/binary>>, Acc, Opts) -> clean(Rest, [124] ++ Acc, Opts);
clean(<<125, Rest/binary>>, Acc, Opts) -> clean(Rest, [125] ++ Acc, Opts);
clean(<<126, Rest/binary>>, Acc, Opts) -> clean(Rest, [126] ++ Acc, Opts);
clean(<<127, Rest/binary>>, Acc, Opts) -> clean(Rest, [127] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X < 16#800 ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X == 16#2028; X == 16#2029 ->
    clean(Rest, maybe_replace(X, Opts) ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X < 16#dcff ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X > 16#dfff, X < 16#fdd0 ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X > 16#fdef, X < 16#fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#10000, X < 16#1fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#20000, X < 16#2fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#30000, X < 16#3fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#40000, X < 16#4fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#50000, X < 16#5fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#60000, X < 16#6fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#70000, X < 16#7fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#80000, X < 16#8fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#90000, X < 16#9fffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#a0000, X < 16#afffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#b0000, X < 16#bfffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#c0000, X < 16#cfffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#d0000, X < 16#dfffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#e0000, X < 16#efffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#f0000, X < 16#ffffe ->
    clean(Rest, [X] ++ Acc, Opts);
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X >= 16#100000, X < 16#10fffe ->
    clean(Rest, [X] ++ Acc, Opts);
%% noncharacters
clean(<<_/utf8, Rest/binary>>, Acc, Opts) ->
    clean(Rest, maybe_replace(noncharacter, Opts) ++ Acc, Opts);
%% surrogates
clean(<<237, X, _, Rest/binary>>, Acc, Opts) when X >= 160 ->
    clean(Rest, maybe_replace(surrogate, Opts) ++ Acc, Opts);
%% u+fffe and u+ffff for R14BXX
clean(<<239, 191, X, Rest/binary>>, Acc, Opts) when X == 190; X == 191 ->
    clean(Rest, maybe_replace(noncharacter, Opts) ++ Acc, Opts);
%% overlong encodings and missing continuations of a 2 byte sequence
clean(<<X, Rest/binary>>, Acc, Opts) when X >= 192, X =< 223 ->
    clean(strip_continuations(Rest, 1), maybe_replace(badutf, Opts) ++ Acc, Opts);
%% overlong encodings and missing continuations of a 3 byte sequence
clean(<<X, Rest/binary>>, Acc, Opts) when X >= 224, X =< 239 ->
    clean(strip_continuations(Rest, 2), maybe_replace(badutf, Opts) ++ Acc, Opts);
%% overlong encodings and missing continuations of a 4 byte sequence
clean(<<X, Rest/binary>>, Acc, Opts) when X >= 240, X =< 247 ->
    clean(strip_continuations(Rest, 3), maybe_replace(badutf, Opts) ++ Acc, Opts);
clean(<<_, Rest/binary>>, Acc, Opts) ->
    clean(Rest, maybe_replace(badutf, Opts) ++ Acc, Opts).


strip_continuations(Bin, 0) -> Bin;
strip_continuations(<<X, Rest/binary>>, N) when X >= 128, X =< 191 ->
    strip_continuations(Rest, N - 1);
%% not a continuation byte
strip_continuations(Bin, _) -> Bin.


maybe_replace(X, #opts{dirty_strings=true}) when is_integer(X) -> [X];
maybe_replace($\b, #opts{escaped_strings=true}) -> [$b, $\\];
maybe_replace($\t, #opts{escaped_strings=true}) -> [$t, $\\];
maybe_replace($\n, #opts{escaped_strings=true}) -> [$n, $\\];
maybe_replace($\f, #opts{escaped_strings=true}) -> [$f, $\\];
maybe_replace($\r, #opts{escaped_strings=true}) -> [$r, $\\];
maybe_replace($\", #opts{escaped_strings=true}) -> [$\", $\\];
maybe_replace($', Opts=#opts{escaped_strings=true}) ->
    case Opts#opts.single_quoted_strings of
        true -> [$', $\\]
        ; false -> [$']
    end;
maybe_replace($/, Opts=#opts{escaped_strings=true}) ->
    case Opts#opts.escaped_forward_slashes of
        true -> [$/, $\\]
        ; false -> [$/]
    end;
maybe_replace($\\, #opts{escaped_strings=true}) -> [$\\, $\\];
maybe_replace(X, Opts=#opts{escaped_strings=true})  when X == 16#2028; X == 16#2029 ->
    case Opts#opts.unescaped_jsonp of
        true -> [X]
        ; false -> lists:reverse(jsx_utils:json_escape_sequence(X))
    end;
maybe_replace(X, #opts{escaped_strings=true}) when X < 32 ->
    lists:reverse(jsx_utils:json_escape_sequence(X));
maybe_replace(noncharacter, #opts{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(surrogate, #opts{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(badutf, #opts{replaced_bad_utf8=true}) -> [16#fffd].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


xcode(Bin) -> xcode(Bin, #opts{}).

xcode(Bin, [replaced_bad_utf8]) -> xcode(Bin, #opts{replaced_bad_utf8=true});
xcode(Bin, Opts) ->
    try clean_string(Bin, Opts)
    catch error:badarg -> {error, badarg}
    end.


is_bad({error, badarg}) -> true;
is_bad(_) -> false.


bad_utf8_test_() ->
    [
        {"orphan continuation byte u+0080",
            ?_assert(is_bad(xcode(<<16#0080>>)))
        },
        {"orphan continuation byte u+0080 replaced",
            ?_assertEqual(xcode(<<16#0080>>, [replaced_bad_utf8]), <<16#fffd/utf8>>)
        },
        {"orphan continuation byte u+00bf",
            ?_assert(is_bad(xcode(<<16#00bf>>)))
        },
        {"orphan continuation byte u+00bf replaced",
            ?_assertEqual(xcode(<<16#00bf>>, [replaced_bad_utf8]), <<16#fffd/utf8>>)
        },
        {"2 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 2))/binary>>)))
        },
        {"2 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 2))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 2)
            )
        },
        {"3 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 3))/binary>>)))
        },
        {"3 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 3))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 3)
            )
        },
        {"4 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 4))/binary>>)))
        },
        {"4 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 4))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 4)
            )
        },
        {"5 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 5))/binary>>)))
        },
        {"5 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 5))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 5)
            )
        },
        {"6 continuation bytes",
            ?_assert(is_bad(xcode(<<(binary:copy(<<16#0080>>, 6))/binary>>)))
        },
        {"6 continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(binary:copy(<<16#0080>>, 6))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, 6)
            )
        },
        {"all continuation bytes",
            ?_assert(is_bad(xcode(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>)))
        },        
        {"all continuation bytes replaced",
            ?_assertEqual(
                xcode(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, [replaced_bad_utf8]),
                binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf)))
            )
        },
        {"lonely start byte",
            ?_assert(is_bad(xcode(<<16#00c0>>)))
        },
        {"lonely start byte replaced",
            ?_assertEqual(
                xcode(<<16#00c0>>, [replaced_bad_utf8]),
                <<16#fffd/utf8>>
            )
        },
        {"lonely start bytes (2 byte)",
            ?_assert(is_bad(xcode(<<16#00c0, 32, 16#00df>>)))
        },
        {"lonely start bytes (2 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00c0, 32, 16#00df>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"lonely start bytes (3 byte)",
            ?_assert(is_bad(xcode(<<16#00e0, 32, 16#00ef>>)))
        },
        {"lonely start bytes (3 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00e0, 32, 16#00ef>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"lonely start bytes (4 byte)",
            ?_assert(is_bad(xcode(<<16#00f0, 32, 16#00f7>>)))
        },
        {"lonely start bytes (4 byte) replaced",
            ?_assertEqual(
                xcode(<<16#00f0, 32, 16#00f7>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32, 16#fffd/utf8>>
            )
        },
        {"missing continuation byte (3 byte)",
            ?_assert(is_bad(xcode(<<224, 160, 32>>)))
        },
        {"missing continuation byte (3 byte) replaced",
            ?_assertEqual(
                xcode(<<224, 160, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"missing continuation byte (4 byte missing one)",
            ?_assert(is_bad(xcode(<<240, 144, 128, 32>>)))
        },
        {"missing continuation byte (4 byte missing one) replaced",
            ?_assertEqual(
                xcode(<<240, 144, 128, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"missing continuation byte (4 byte missing two)",
            ?_assert(is_bad(xcode(<<240, 144, 32>>)))
        },
        {"missing continuation byte (4 byte missing two) replaced",
            ?_assertEqual(
                xcode(<<240, 144, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (2 byte)",
            ?_assert(is_bad(xcode(<<16#c0, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (2 byte) replaced",
            ?_assertEqual(
                xcode(<<16#c0, 16#af, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (3 byte)",
            ?_assert(is_bad(xcode(<<16#e0, 16#80, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (3 byte) replaced",
            ?_assertEqual(
                xcode(<<16#e0, 16#80, 16#af, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"overlong encoding of u+002f (4 byte)",
            ?_assert(is_bad(xcode(<<16#f0, 16#80, 16#80, 16#af, 32>>)))
        },
        {"overlong encoding of u+002f (4 byte) replaced",
            ?_assertEqual(
                xcode(<<16#f0, 16#80, 16#80, 16#af, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 2 byte sequence",
            ?_assert(is_bad(xcode(<<16#c1, 16#bf, 32>>)))
        },
        {"highest overlong 2 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#c1, 16#bf, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 3 byte sequence",
            ?_assert(is_bad(xcode(<<16#e0, 16#9f, 16#bf, 32>>)))
        },
        {"highest overlong 3 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#e0, 16#9f, 16#bf, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        },
        {"highest overlong 4 byte sequence",
            ?_assert(is_bad(xcode(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>)))
        },
        {"highest overlong 4 byte sequence replaced",
            ?_assertEqual(
                xcode(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, [replaced_bad_utf8]),
                <<16#fffd/utf8, 32>>
            )
        }
    ].


encode(Term) -> encode(Term, []).

encode(Term, Opts) ->
    try (encoder(jsx, [], Opts))(Term)
    catch _:_ -> {error, badjson}
    end.


encode_test_() ->    
    [
        {"naked string", ?_assertEqual(encode(<<"a string\n">>), [{string, <<"a string\n">>}, end_json])},
        {"escaped naked string", ?_assertEqual(encode(<<"a string\n">>, [escaped_strings]), [{string, <<"a string\\n">>}, end_json])},
        {"naked integer", ?_assertEqual(encode(123), [{integer, 123}, end_json])},
        {"naked float", ?_assertEqual(encode(1.23), [{float, 1.23}, end_json])},
        {"naked literal", ?_assertEqual(encode(null), [{literal, null}, end_json])},
        {"empty object", ?_assertEqual(encode([{}]), [start_object, end_object, end_json])},
        {"empty list", ?_assertEqual(encode([]), [start_array, end_array, end_json])},
        {"simple list", ?_assertEqual(
                encode([1,2,3,true,false]),
                [
                    start_array,
                    {integer, 1},
                    {integer, 2},
                    {integer, 3},
                    {literal, true},
                    {literal, false},
                    end_array,
                    end_json
                ]
            )
        },
        {"simple object", ?_assertEqual(
                encode([{<<"a">>, true}, {<<"b">>, false}]),
                [
                    start_object,
                    {key, <<"a">>},
                    {literal, true},
                    {key, <<"b">>},
                    {literal, false},
                    end_object,
                    end_json
                ]
            )
        },
        {"complex term", ?_assertEqual(
                encode([
                    {<<"a">>, true},
                    {<<"b">>, false},
                    {<<"c">>, [1,2,3]},
                    {<<"d">>, [{<<"key">>, <<"value">>}]}
                ]),
                [
                    start_object,
                    {key, <<"a">>},
                    {literal, true},
                    {key, <<"b">>},
                    {literal, false},
                    {key, <<"c">>},
                    start_array,
                        {integer, 1},
                        {integer, 2},
                        {integer, 3},
                    end_array,
                    {key, <<"d">>},
                    start_object,
                        {key, <<"key">>},
                        {string, <<"value">>},
                    end_object,
                    end_object,
                    end_json
                ]
            )
        },
        {"atom keys", ?_assertEqual(
                encode([{key, <<"value">>}]),
                [start_object, {key, <<"key">>}, {string, <<"value">>}, end_object, end_json]
            )
        }
    ].


pre_encoders_test_() ->
    Term = [
        {<<"object">>, [
            {<<"literals">>, [true, false, null]},
            {<<"strings">>, [<<"foo">>, <<"bar">>, <<"baz">>]},
            {<<"numbers">>, [1, 1.0, 1.0e0]}
        ]}
    ],
    [
        {"no pre encode", ?_assertEqual(
            encode(Term, []),
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array,
                            {literal, true}, {literal, false}, {literal, null},
                        end_array,
                        {key, <<"strings">>}, start_array,
                            {string, <<"foo">>}, {string, <<"bar">>}, {string, <<"baz">>},
                        end_array,
                        {key, <<"numbers">>}, start_array,
                            {integer, 1}, {float, 1.0}, {float, 1.0},
                        end_array,
                    end_object,
                end_object,
                end_json
            ]
        )},
        {"replace lists with empty lists", ?_assertEqual(
            encode(Term, [{pre_encode, fun(V) -> case V of [{_,_}|_] -> V; [{}] -> V; V when is_list(V) -> []; _ -> V end end}]),
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array, end_array,
                        {key, <<"strings">>}, start_array, end_array,
                        {key, <<"numbers">>}, start_array, end_array,
                    end_object,
                end_object,
                end_json
            ]
        )},
        {"replace objects with empty objects", ?_assertEqual(
            encode(Term, [{pre_encode, fun(V) -> case V of [{_,_}|_] -> [{}]; _ -> V end end}]),
            [
                start_object,
                end_object,
                end_json
            ]
        )},
        {"replace all non-list values with false", ?_assertEqual(
            encode(Term, [{pre_encode, fun(V) when is_list(V) -> V; (_) -> false end}]),
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array,
                            {literal, false}, {literal, false}, {literal, false},
                        end_array,
                        {key, <<"strings">>}, start_array,
                            {literal, false}, {literal, false}, {literal, false},
                        end_array,
                        {key, <<"numbers">>}, start_array,
                            {literal, false}, {literal, false}, {literal, false},
                        end_array,
                    end_object,
                end_object,
                end_json
            ]
        )},
        {"replace all atoms with atom_to_list", ?_assertEqual(
            encode(Term, [{pre_encode, fun(V) when is_atom(V) -> unicode:characters_to_binary(atom_to_list(V)); (V) -> V end}]),
            [
                start_object,
                    {key, <<"object">>}, start_object,
                        {key, <<"literals">>}, start_array,
                            {string, <<"true">>}, {string, <<"false">>}, {string, <<"null">>},
                        end_array,
                        {key, <<"strings">>}, start_array,
                            {string, <<"foo">>}, {string, <<"bar">>}, {string, <<"baz">>},
                        end_array,
                        {key, <<"numbers">>}, start_array,
                            {integer, 1}, {float, 1.0}, {float, 1.0},
                        end_array,
                    end_object,
                end_object,
                end_json
            ]
        )}
    ].


escapes_test_() ->
    [
        {"backspace escape", ?_assertEqual(encode(<<"\b">>, [escaped_strings]), [{string, <<"\\b">>}, end_json])},
        {"formfeed escape", ?_assertEqual(encode(<<"\f">>, [escaped_strings]), [{string, <<"\\f">>}, end_json])},
        {"newline escape", ?_assertEqual(encode(<<"\n">>, [escaped_strings]), [{string, <<"\\n">>}, end_json])},
        {"carriage return escape", ?_assertEqual(encode(<<"\r">>, [escaped_strings]), [{string, <<"\\r">>}, end_json])},
        {"tab escape", ?_assertEqual(encode(<<"\t">>, [escaped_strings]), [{string, <<"\\t">>}, end_json])},
        {"quote escape", ?_assertEqual(encode(<<"\"">>, [escaped_strings]), [{string, <<"\\\"">>}, end_json])},
        {"single quote escape", ?_assertEqual(encode(<<"'">>, [escaped_strings, single_quoted_strings]), [{string, <<"\\'">>}, end_json])},
        {"no single quote escape", ?_assertEqual(encode(<<"'">>, [escaped_strings]), [{string, <<"'">>}, end_json])},
        {"forward slash escape", ?_assertEqual(encode(<<"/">>, [escaped_strings, escaped_forward_slashes]), [{string, <<"\\/">>}, end_json])},
        {"no forward slash escape", ?_assertEqual(encode(<<"/">>, [escaped_strings]), [{string, <<"/">>}, end_json])},
        {"back slash escape", ?_assertEqual(encode(<<"\\">>, [escaped_strings]), [{string, <<"\\\\">>}, end_json])},
        {"jsonp escape", ?_assertEqual(
            encode(<<16#2028/utf8, 16#2029/utf8>>, [escaped_strings]),
            [{string, <<"\\u2028\\u2029">>}, end_json]
        )},
        {"no jsonp escape", ?_assertEqual(
            encode(<<16#2028/utf8, 16#2029/utf8>>, [escaped_strings, unescaped_jsonp]),
            [{string, <<16#2028/utf8, 16#2029/utf8>>}, end_json]
        )},
        {"control escape", ?_assertEqual(encode(<<0>>, [escaped_strings]), [{string, <<"\\u0000">>}, end_json])},
        {"dirty strings", ?_assertEqual(encode(<<"\n">>, [escaped_strings, dirty_strings]), [{string, <<"\n">>}, end_json])},
        {"ignore bad escapes", ?_assertEqual(encode(<<"\\x25">>, [escaped_strings, ignored_bad_escapes]), [{string, <<"\\\\x25">>}, end_json])}
    ].


surrogates_test_() ->
    [
        {"surrogates - badjson",
            ?_assert(check_bad(surrogates()))
        },
        {"surrogates - replaced",
            ?_assert(check_replaced(surrogates()))
        }
    ].


good_characters_test_() ->
    [
        {"acceptable codepoints",
            ?_assert(check_good(good()))
        },
        {"acceptable codepoints - escaped_strings",
            ?_assert(check_good(good(), [escaped_strings]))
        },
        {"acceptable codepoints - replaced_bad_utf8",
            ?_assert(check_good(good(), [escaped_strings]))
        },
        {"acceptable codepoints - escaped_strings + replaced_bad_utf8",
            ?_assert(check_good(good(), [escaped_strings, replaced_bad_utf8]))
        },
        {"acceptable extended",
            ?_assert(check_good(good_extended()))
        },
        {"acceptable extended - escaped_strings",
            ?_assert(check_good(good_extended(), [escaped_strings]))
        },
        {"acceptable extended - escaped_strings",
            ?_assert(check_good(good_extended(), [replaced_bad_utf8]))
        }
    ].


reserved_test_() ->
    [
        {"reserved noncharacters - badjson",
            ?_assert(check_bad(reserved_space()))
        },
        {"reserved noncharacters - replaced",
            ?_assert(check_replaced(reserved_space()))
        }
    ].


noncharacters_test_() ->
    [
        {"noncharacters - badjson",
            ?_assert(check_bad(noncharacters()))
        },
        {"noncharacters - replaced",
            ?_assert(check_replaced(noncharacters()))
        }
    ].


extended_noncharacters_test_() ->
    [
        {"extended noncharacters - badjson",
            ?_assert(check_bad(extended_noncharacters()))
        },
        {"extended noncharacters - replaced",
            ?_assert(check_replaced(extended_noncharacters()))
        }
    ].


check_bad(List) ->
    [] == lists:dropwhile(fun({_, {error, badjson}}) -> true ; (_) -> false end,
        check(List, [], [])
    ).


check_replaced(List) ->
    [] == lists:dropwhile(fun({_, [{string, <<16#fffd/utf8>>}|_]}) -> true ; (_) -> false 
        end,
        check(List, [replaced_bad_utf8], [])
    ).


check_good(List) -> check_good(List, []).

check_good(List, Opts) ->
    [] == lists:dropwhile(fun({_, [{string, _}|_]}) -> true ; (_) -> false end,
        check(List, Opts, [])
    ).


check([], _Opts, Acc) -> Acc;
check([H|T], Opts, Acc) ->
    R = encode(to_fake_utf(H, utf8), Opts),
    check(T, Opts, [{H, R}] ++ Acc).
    

noncharacters() -> lists:seq(16#fffe, 16#ffff).

extended_noncharacters() ->
    [16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff]
        ++ [16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff]
        ++ [16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff]
        ++ [16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff]
        ++ [16#9fffe, 16#9ffff, 16#afffe, 16#affff]
        ++ [16#bfffe, 16#bffff, 16#cfffe, 16#cffff]
        ++ [16#dfffe, 16#dffff, 16#efffe, 16#effff]
        ++ [16#ffffe, 16#fffff, 16#10fffe, 16#10ffff].

surrogates() -> lists:seq(16#d800, 16#dfff).

reserved_space() -> lists:seq(16#fdd0, 16#fdef).

good() -> lists:seq(16#0000, 16#d7ff) ++ lists:seq(16#e000, 16#fdcf) ++ lists:seq(16#fdf0, 16#fffd).

good_extended() -> [16#10000, 16#20000, 16#30000, 16#40000, 16#50000,
        16#60000, 16#70000, 16#80000, 16#90000, 16#a0000, 
        16#b0000, 16#c0000, 16#d0000, 16#e0000, 16#f0000
    ] ++ lists:seq(16#100000, 16#10fffd).


%% erlang refuses to encode certain codepoints, so fake them all
to_fake_utf(N, utf8) when N < 16#0080 -> <<N:8>>;
to_fake_utf(N, utf8) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<2#110:3, Y:5, 2#10:2, X:6>>; 
to_fake_utf(N, utf8) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6>>;
to_fake_utf(N, utf8) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6>>.


-endif.