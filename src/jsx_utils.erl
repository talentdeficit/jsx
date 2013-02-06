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


-module(jsx_utils).

-export([parse_opts/1]).
-export([extract_opts/1, valid_flags/0]).
-export([json_escape_sequence/1]).
-export([clean_string/2]).

-include("jsx_opts.hrl").


%% parsing of jsx opts
parse_opts(Opts) ->
    parse_opts(Opts, #opts{}).

parse_opts([], Opts) ->
    Opts;
parse_opts([replaced_bad_utf8|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{replaced_bad_utf8=true});
parse_opts([escaped_forward_slashes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escaped_forward_slashes=true});
parse_opts([explicit_end|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{explicit_end=true});
parse_opts([single_quoted_strings|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{single_quoted_strings=true});
parse_opts([unescaped_jsonp|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{unescaped_jsonp=true});
parse_opts([comments|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{comments=true});
parse_opts([escaped_strings|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escaped_strings=true});
parse_opts([dirty_strings|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{dirty_strings=true});
parse_opts([ignored_bad_escapes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{ignored_bad_escapes=true});
parse_opts([relax|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{
        replaced_bad_utf8 = true,
        single_quoted_strings = true,
        comments = true,
        ignored_bad_escapes = true
    });
parse_opts([{pre_encode, Encoder}|Rest] = Options, Opts) when is_function(Encoder, 1) ->
    case Opts#opts.pre_encode of
        false -> parse_opts(Rest, Opts#opts{pre_encode=Encoder})
        ; _ -> erlang:error(badarg, [Options, Opts])
    end;
%% deprecated flags
parse_opts([{pre_encoder, Encoder}|Rest] = Options, Opts) when is_function(Encoder, 1) ->
    case Opts#opts.pre_encode of
        false -> parse_opts(Rest, Opts#opts{pre_encode=Encoder})
        ; _ -> erlang:error(badarg, [Options, Opts])
    end;
parse_opts([loose_unicode|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{replaced_bad_utf8=true});
parse_opts([escape_forward_slash|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escaped_forward_slashes=true});
parse_opts([single_quotes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{single_quoted_strings=true});
parse_opts([no_jsonp_escapes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{unescaped_jsonp=true});
parse_opts([json_escape|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escaped_strings=true});
parse_opts([ignore_bad_escapes|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{ignored_bad_escapes=true});
parse_opts(Options, Opts) ->
    erlang:error(badarg, [Options, Opts]).


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
        %% deprecated flags
        pre_encoder,            %% pre_encode
        loose_unicode,          %% replaced_bad_utf8
        escape_forward_slash,   %% escaped_forward_slashes
        single_quotes,          %% single_quotes_strings
        no_jsonp_escapes,       %% unescaped_jsonp
        json_escape,            %% escaped_strings
        ignore_bad_escapes      %% ignored_bad_escapes
    ].


extract_opts(Opts) ->
    extract_parser_opts(Opts, []).

extract_parser_opts([], Acc) -> Acc;
extract_parser_opts([{K,V}|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_opts(Rest, [{K,V}] ++ Acc)
        ; false -> extract_parser_opts(Rest, Acc)
    end;
extract_parser_opts([K|Rest], Acc) ->
    case lists:member(K, valid_flags()) of
        true -> extract_parser_opts(Rest, [K] ++ Acc)
        ; false -> extract_parser_opts(Rest, Acc)
    end.


%% convert a codepoint to it's \uXXXX equiv.
json_escape_sequence(X) ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    [$\\, $u, (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))].


to_hex(10) -> $a;
to_hex(11) -> $b;
to_hex(12) -> $c;
to_hex(13) -> $d;
to_hex(14) -> $e;
to_hex(15) -> $f;
to_hex(X) -> X + 48.    %% ascii "1" is [49], "2" is [50], etc...


clean_string(Bin, #opts{dirty_strings=true}) -> Bin;
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
ensure_clean(<<X/utf8, Rest/binary>>) when X < 16#d800 -> ensure_clean(Rest);
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
clean(<<39, Rest/binary>>, Acc, Opts) -> clean(Rest, [39] ++ Acc, Opts);
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
clean(<<X/utf8, Rest/binary>>, Acc, Opts) when X < 16#d800 ->
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


maybe_replace($\b, #opts{escaped_strings=true}) -> [$b, $\\];
maybe_replace($\t, #opts{escaped_strings=true}) -> [$t, $\\];
maybe_replace($\n, #opts{escaped_strings=true}) -> [$n, $\\];
maybe_replace($\f, #opts{escaped_strings=true}) -> [$f, $\\];
maybe_replace($\r, #opts{escaped_strings=true}) -> [$r, $\\];
maybe_replace($\", #opts{escaped_strings=true}) -> [$\", $\\];
maybe_replace($\\, #opts{escaped_strings=true}) -> [$\\, $\\];
maybe_replace($/, Opts) ->
    case Opts#opts.escaped_forward_slashes of
        true -> [$/, $\\]
        ; false -> [$/]
    end;
maybe_replace(X, Opts=#opts{escaped_strings=true})  when X == 16#2028; X == 16#2029 ->
    case Opts#opts.unescaped_jsonp of
        true -> [X]
        ; false -> lists:reverse(jsx_utils:json_escape_sequence(X))
    end;
maybe_replace(X, #opts{escaped_strings=true}) when X < 32 ->
    lists:reverse(jsx_utils:json_escape_sequence(X));
maybe_replace(noncharacter, #opts{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(surrogate, #opts{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(badutf, #opts{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(_, _) -> erlang:error(badarg).



%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


json_escape_sequence_test_() ->
    [
        {"json escape sequence test - 16#0000", ?_assertEqual(json_escape_sequence(16#0000), "\\u0000")},
        {"json escape sequence test - 16#abc", ?_assertEqual(json_escape_sequence(16#abc), "\\u0abc")},
        {"json escape sequence test - 16#def", ?_assertEqual(json_escape_sequence(16#def), "\\u0def")}
    ].


opts_test_() ->
    [
        {"all flags",
            ?_assertEqual(
                parse_opts([
                    replaced_bad_utf8,
                    escaped_forward_slashes,
                    explicit_end,
                    single_quoted_strings,
                    unescaped_jsonp,
                    comments,
                    dirty_strings,
                    ignored_bad_escapes
                ]),
                #opts{
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
        },
        {"relax flag",
            ?_assertEqual(
                parse_opts([relax]),
                #opts{
                    replaced_bad_utf8=true,
                    single_quoted_strings=true,
                    comments=true,
                    ignored_bad_escapes=true
                }
            )
        }
    ].


%% erlang refuses to encode certain codepoints, so fake them
to_fake_utf8(N) when N < 16#0080 -> <<N:8>>;
to_fake_utf8(N) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<2#110:3, Y:5, 2#10:2, X:6>>;
to_fake_utf8(N) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6>>;
to_fake_utf8(N) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6>>.


codepoints() ->
    unicode:characters_to_binary(
        [32, 33]
        ++ lists:seq(35, 91)
        ++ lists:seq(93, 16#2027)
        ++ lists:seq(16#202a, 16#d7ff)
        ++ lists:seq(16#e000, 16#fdcf)
        ++ lists:seq(16#fdf0, 16#fffd)
    ).

escapables() ->
    [ to_fake_utf8(N) || N <-
        lists:seq(0, 31) ++ [34, 92, 16#2028, 16#2029] 
    ].

extended_codepoints() ->
    unicode:characters_to_binary(
        lists:seq(16#10000, 16#1fffd) ++ [
            16#20000, 16#30000, 16#40000, 16#50000, 16#60000,
            16#70000, 16#80000, 16#90000, 16#a0000, 16#b0000,
            16#c0000, 16#d0000, 16#e0000, 16#f0000
        ] ++ lists:seq(16#100000, 16#10fffd)
    ).

noncharacters() -> [ to_fake_utf8(N) || N <- lists:seq(16#fffe, 16#ffff) ].

extended_noncharacters() ->
    [ to_fake_utf8(N) || N <- [16#1fffe, 16#1ffff, 16#2fffe, 16#2ffff]
        ++ [16#3fffe, 16#3ffff, 16#4fffe, 16#4ffff]
        ++ [16#5fffe, 16#5ffff, 16#6fffe, 16#6ffff]
        ++ [16#7fffe, 16#7ffff, 16#8fffe, 16#8ffff]
        ++ [16#9fffe, 16#9ffff, 16#afffe, 16#affff]
        ++ [16#bfffe, 16#bffff, 16#cfffe, 16#cffff]
        ++ [16#dfffe, 16#dffff, 16#efffe, 16#effff]
        ++ [16#ffffe, 16#fffff, 16#10fffe, 16#10ffff]
    ].

surrogates() -> [ to_fake_utf8(N) || N <- lists:seq(16#d800, 16#dfff) ].

reserved_space() -> [ to_fake_utf8(N) || N <- lists:seq(16#fdd0, 16#fdef) ].


fail_ensure(Codepoints) ->
    {generator,
        fun() -> case Codepoints of
            [N|Rest] ->
                [ ?_assertError(badarg, ensure_clean(N)) | fail_clean(Rest) ]
            ; [] -> []
        end end
    }.

fail_clean(Codepoints) ->
    {generator,
        fun() -> case Codepoints of
            [N|Rest] ->
                [ ?_assertError(badarg, clean(N, [], #opts{})) | fail_clean(Rest) ]
            ; [] -> []
        end end
    }.

fail_bad(Codepoints) ->
    {generator,
        fun() -> case Codepoints of
            [N|Rest] ->
                [ ?_assertError(badarg, clean(N, [], #opts{})) | fail_bad(Rest) ]
            ; [] -> []
        end end
    }.

replace_bad(Codepoints) ->
    {generator,
        fun() -> case Codepoints of
            [N|Rest] ->
                [ ?_assertEqual(
                        <<16#fffd/utf8>>,
                        clean(N, [], #opts{replaced_bad_utf8=true}))
                    | fail_bad(Rest)
                ]
            ; [] -> []
        end end
    }.


ensure_clean_test_() ->
    [
        {"basic codepoints", ?_assertEqual(ok, ensure_clean(codepoints()))},
        {"escapables", ?_assertEqual(ok, ensure_clean(unicode:characters_to_binary(escapables())))},
        {"extended codepoints", ?_assertEqual(ok, ensure_clean(extended_codepoints()))},
        {"noncharacters", fail_ensure(noncharacters())},
        {"extended noncharacter", fail_ensure(extended_noncharacters())},
        {"surrogates", fail_ensure(surrogates())},
        {"reserved_space", fail_ensure(reserved_space())}
    ].


clean_test_() ->
    [
        {"basic codepoints", ?_assertEqual(
            codepoints(),
            clean(codepoints(), [], #opts{})
        )},
        {"escapables", fail_clean(escapables())},
        {"extended codepoints", ?_assertEqual(
            extended_codepoints(),
            clean(extended_codepoints(), [], #opts{})
        )},
        {"noncharacters", fail_clean(noncharacters())},
        {"extended noncharacter", fail_clean(extended_noncharacters())},
        {"surrogates", fail_clean(surrogates())},
        {"reserved_space", fail_clean(reserved_space())}
    ].


escape_test_() ->
    [
        {"escape u0000", ?_assertEqual(
            <<"\\u0000">>,
            clean_string(to_fake_utf8(16#0000), #opts{escaped_strings=true})
        )},
        {"escape u0001", ?_assertEqual(
            <<"\\u0001">>,
            clean_string(to_fake_utf8(16#0001), #opts{escaped_strings=true})
        )},
        {"escape u0002", ?_assertEqual(
            <<"\\u0002">>,
            clean_string(to_fake_utf8(16#0002), #opts{escaped_strings=true})
        )},
        {"escape u0003", ?_assertEqual(
            <<"\\u0003">>,
            clean_string(to_fake_utf8(16#0003), #opts{escaped_strings=true})
        )},
        {"escape u0004", ?_assertEqual(
            <<"\\u0004">>,
            clean_string(to_fake_utf8(16#0004), #opts{escaped_strings=true})
        )},
        {"escape u0005", ?_assertEqual(
            <<"\\u0005">>,
            clean_string(to_fake_utf8(16#0005), #opts{escaped_strings=true})
        )},
        {"escape u0006", ?_assertEqual(
            <<"\\u0006">>,
            clean_string(to_fake_utf8(16#0006), #opts{escaped_strings=true})
        )},
        {"escape u0007", ?_assertEqual(
            <<"\\u0007">>,
            clean_string(to_fake_utf8(16#0007), #opts{escaped_strings=true})
        )},
        {"escape u0008", ?_assertEqual(
            <<"\\b">>,
            clean_string(to_fake_utf8(16#0008), #opts{escaped_strings=true})
        )},
        {"escape u0009", ?_assertEqual(
            <<"\\t">>,
            clean_string(to_fake_utf8(16#0009), #opts{escaped_strings=true})
        )},
        {"escape u000a", ?_assertEqual(
            <<"\\n">>,
            clean_string(to_fake_utf8(16#000a), #opts{escaped_strings=true})
        )},
        {"escape u000b", ?_assertEqual(
            <<"\\u000b">>,
            clean_string(to_fake_utf8(16#000b), #opts{escaped_strings=true})
        )},
        {"escape u000c", ?_assertEqual(
            <<"\\f">>,
            clean_string(to_fake_utf8(16#000c), #opts{escaped_strings=true})
        )},
        {"escape u000d", ?_assertEqual(
            <<"\\r">>,
            clean_string(to_fake_utf8(16#000d), #opts{escaped_strings=true})
        )},
        {"escape u000e", ?_assertEqual(
            <<"\\u000e">>,
            clean_string(to_fake_utf8(16#000e), #opts{escaped_strings=true})
        )},
        {"escape u000f", ?_assertEqual(
            <<"\\u000f">>,
            clean_string(to_fake_utf8(16#000f), #opts{escaped_strings=true})
        )},
        {"escape u0010", ?_assertEqual(
            <<"\\u0010">>,
            clean_string(to_fake_utf8(16#0010), #opts{escaped_strings=true})
        )},
        {"escape u0011", ?_assertEqual(
            <<"\\u0011">>,
            clean_string(to_fake_utf8(16#0011), #opts{escaped_strings=true})
        )},
        {"escape u0012", ?_assertEqual(
            <<"\\u0012">>,
            clean_string(to_fake_utf8(16#0012), #opts{escaped_strings=true})
        )},
        {"escape u0013", ?_assertEqual(
            <<"\\u0013">>,
            clean_string(to_fake_utf8(16#0013), #opts{escaped_strings=true})
        )},
        {"escape u0014", ?_assertEqual(
            <<"\\u0014">>,
            clean_string(to_fake_utf8(16#0014), #opts{escaped_strings=true})
        )},
        {"escape u0015", ?_assertEqual(
            <<"\\u0015">>,
            clean_string(to_fake_utf8(16#0015), #opts{escaped_strings=true})
        )},
        {"escape u0016", ?_assertEqual(
            <<"\\u0016">>,
            clean_string(to_fake_utf8(16#0016), #opts{escaped_strings=true})
        )},
        {"escape u0017", ?_assertEqual(
            <<"\\u0017">>,
            clean_string(to_fake_utf8(16#0017), #opts{escaped_strings=true})
        )},
        {"escape u0018", ?_assertEqual(
            <<"\\u0018">>,
            clean_string(to_fake_utf8(16#0018), #opts{escaped_strings=true})
        )},
        {"escape u0019", ?_assertEqual(
            <<"\\u0019">>,
            clean_string(to_fake_utf8(16#0019), #opts{escaped_strings=true})
        )},
        {"escape u001a", ?_assertEqual(
            <<"\\u001a">>,
            clean_string(to_fake_utf8(16#001a), #opts{escaped_strings=true})
        )},
        {"escape u001b", ?_assertEqual(
            <<"\\u001b">>,
            clean_string(to_fake_utf8(16#001b), #opts{escaped_strings=true})
        )},
        {"escape u001c", ?_assertEqual(
            <<"\\u001c">>,
            clean_string(to_fake_utf8(16#001c), #opts{escaped_strings=true})
        )},
        {"escape u001d", ?_assertEqual(
            <<"\\u001d">>,
            clean_string(to_fake_utf8(16#001d), #opts{escaped_strings=true})
        )},
        {"escape u001e", ?_assertEqual(
            <<"\\u001e">>,
            clean_string(to_fake_utf8(16#001e), #opts{escaped_strings=true})
        )},
        {"escape u001f", ?_assertEqual(
            <<"\\u001f">>,
            clean_string(to_fake_utf8(16#001f), #opts{escaped_strings=true})
        )},
        {"escape u0022", ?_assertEqual(
            <<"\\\"">>,
            clean_string(to_fake_utf8(16#0022), #opts{escaped_strings=true})
        )},
        {"escape u002f", ?_assertEqual(
            <<"\\/">>,
            clean_string(to_fake_utf8(16#002f), #opts{escaped_strings=true, escaped_forward_slashes=true})
        )},
        {"escape u005c", ?_assertEqual(
            <<"\\\\">>,
            clean_string(to_fake_utf8(16#005c), #opts{escaped_strings=true})
        )},
        {"escape u2028", ?_assertEqual(
            <<"\\u2028">>,
            clean_string(to_fake_utf8(16#2028), #opts{escaped_strings=true})
        )},
        {"escape u2029", ?_assertEqual(
            <<"\\u2029">>,
            clean_string(to_fake_utf8(16#2029), #opts{escaped_strings=true})
        )}
    ].


bad_utf8_test_() ->
    [
        {"noncharacter u+fffe", ?_assertError(badarg, clean_string(to_fake_utf8(16#fffe), #opts{}))},
        {"noncharacter u+fffe replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(to_fake_utf8(16#fffe), #opts{replaced_bad_utf8=true})
        )},
        {"noncharacter u+ffff", ?_assertError(badarg, clean_string(to_fake_utf8(16#ffff), #opts{}))},
        {"noncharacter u+ffff replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(to_fake_utf8(16#ffff), #opts{replaced_bad_utf8=true})
        )},
        {"extended noncharacters", fail_bad(extended_noncharacters())},
        {"extended noncharacters replaced", replace_bad(extended_noncharacters())},
        {"surrogates", fail_bad(surrogates())},
        {"surrogates replaced", replace_bad(surrogates())},
        {"reserved_space", fail_bad(reserved_space())},
        {"reserved_space replaced", replace_bad(reserved_space())},
        {"orphan continuation byte u+0080", ?_assertError(
            badarg,
            clean_string(<<16#0080>>, #opts{})
        )},
        {"orphan continuation byte u+0080 replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#0080>>, #opts{replaced_bad_utf8=true})
        )},
        {"orphan continuation byte u+00bf", ?_assertError(
            badarg,
            clean_string(<<16#00bf>>, #opts{})
        )},
        {"orphan continuation byte u+00bf replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00bf>>, #opts{replaced_bad_utf8=true})
        )},
        {"2 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #opts{})
        )},
        {"2 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 2),
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #opts{replaced_bad_utf8=true})
        )},
        {"3 continuation bytes",
            ?_assertError(badarg, clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #opts{}))
        },
        {"3 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 3),
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #opts{replaced_bad_utf8=true})
        )},
        {"4 continuation bytes",
            ?_assertError(badarg, clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #opts{}))
        },
        {"4 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 4),
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #opts{replaced_bad_utf8=true})
        )},
        {"5 continuation bytes",
            ?_assertError(badarg, clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #opts{}))
        },
        {"5 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 5),
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #opts{replaced_bad_utf8=true})
        )},
        {"6 continuation bytes",
            ?_assertError(badarg, clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #opts{}))
        },
        {"6 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 6),
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #opts{replaced_bad_utf8=true})
        )},
        {"all continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, #opts{})
        )},
        {"all continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf))),
            clean_string(
                <<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>,
                #opts{replaced_bad_utf8=true}
            )
        )},
        {"lonely start byte", ?_assertError(badarg, clean_string(<<16#00c0>>, #opts{}))},
        {"lonely start byte replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00c0>>, #opts{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (2 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00c0, 32, 16#00df>>, #opts{})
        )},
        {"lonely start bytes (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00c0, 32, 16#00df>>, #opts{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (3 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00e0, 32, 16#00ef>>, #opts{})
        )},
        {"lonely start bytes (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00e0, 32, 16#00ef>>, #opts{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (4 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00f0, 32, 16#00f7>>, #opts{})
        )},
        {"lonely start bytes (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00f0, 32, 16#00f7>>, #opts{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (3 byte)", ?_assertError(
            badarg,
            clean_string(<<224, 160, 32>>, #opts{})
        )},
        {"missing continuation byte (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<224, 160, 32>>, #opts{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (4 byte missing one)", ?_assertError(
            badarg,
            clean_string(<<240, 144, 128, 32>>, #opts{})
        )},
        {"missing continuation byte (4 byte missing one) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 128, 32>>, #opts{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (4 byte missing two)", ?_assertError(
            badarg,
            clean_string(<<240, 144, 32>>, #opts{})
        )},
        {"missing continuation byte (4 byte missing two) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 32>>, #opts{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (2 byte)", ?_assertError(
            badarg,
            clean_string(<<16#c0, 16#af, 32>>, #opts{})
        )},
        {"overlong encoding of u+002f (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c0, 16#af, 32>>, #opts{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (3 byte)", ?_assertError(
            badarg,
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #opts{})
        )},
        {"overlong encoding of u+002f (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #opts{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (4 byte)", ?_assertError(
            badarg,
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #opts{})
        )},
        {"overlong encoding of u+002f (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #opts{replaced_bad_utf8=true})
        )},
        {"highest overlong 2 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#c1, 16#bf, 32>>, #opts{})
        )},
        {"highest overlong 2 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c1, 16#bf, 32>>, #opts{replaced_bad_utf8=true})
        )},
        {"highest overlong 3 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #opts{})
        )},
        {"highest overlong 3 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #opts{replaced_bad_utf8=true})
        )},
        {"highest overlong 4 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #opts{})
        )},
        {"highest overlong 4 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #opts{replaced_bad_utf8=true})
        )}
    ].


-endif.