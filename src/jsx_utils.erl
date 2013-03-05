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

-export([parse_config/1]).
-export([config_to_list/1]).
-export([extract_config/1, valid_flags/0]).
-export([json_escape_sequence/1]).
-export([clean_string/2]).

-ifdef(TEST).
-export([fake_error_handler/3]).
-endif.

-include("jsx_config.hrl").


%% parsing of jsx config
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


clean_string(Bin, #config{dirty_strings=true}) -> Bin;
clean_string(Bin, Config) ->
    case Config#config.replaced_bad_utf8 orelse Config#config.escaped_strings of
        true -> clean(Bin, [], Config)
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
clean(<<>>, Acc, _Config) -> unicode:characters_to_binary(lists:reverse(Acc));
clean(<<0, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(0, Config) ++ Acc, Config);
clean(<<1, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(1, Config) ++ Acc, Config);
clean(<<2, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(2, Config) ++ Acc, Config);
clean(<<3, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(3, Config) ++ Acc, Config);
clean(<<4, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(4, Config) ++ Acc, Config);
clean(<<5, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(5, Config) ++ Acc, Config);
clean(<<6, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(6, Config) ++ Acc, Config);
clean(<<7, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(7, Config) ++ Acc, Config);
clean(<<8, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(8, Config) ++ Acc, Config);
clean(<<9, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(9, Config) ++ Acc, Config);
clean(<<10, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(10, Config) ++ Acc, Config);
clean(<<11, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(11, Config) ++ Acc, Config);
clean(<<12, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(12, Config) ++ Acc, Config);
clean(<<13, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(13, Config) ++ Acc, Config);
clean(<<14, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(14, Config) ++ Acc, Config);
clean(<<15, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(15, Config) ++ Acc, Config);
clean(<<16, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(16, Config) ++ Acc, Config);
clean(<<17, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(17, Config) ++ Acc, Config);
clean(<<18, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(18, Config) ++ Acc, Config);
clean(<<19, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(19, Config) ++ Acc, Config);
clean(<<20, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(20, Config) ++ Acc, Config);
clean(<<21, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(21, Config) ++ Acc, Config);
clean(<<22, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(22, Config) ++ Acc, Config);
clean(<<23, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(23, Config) ++ Acc, Config);
clean(<<24, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(24, Config) ++ Acc, Config);
clean(<<25, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(25, Config) ++ Acc, Config);
clean(<<26, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(26, Config) ++ Acc, Config);
clean(<<27, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(27, Config) ++ Acc, Config);
clean(<<28, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(28, Config) ++ Acc, Config);
clean(<<29, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(29, Config) ++ Acc, Config);
clean(<<30, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(30, Config) ++ Acc, Config);
clean(<<31, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(31, Config) ++ Acc, Config);
clean(<<32, Rest/binary>>, Acc, Config) -> clean(Rest, [32] ++ Acc, Config);
clean(<<33, Rest/binary>>, Acc, Config) -> clean(Rest, [33] ++ Acc, Config);
clean(<<34, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(34, Config) ++ Acc, Config);
clean(<<35, Rest/binary>>, Acc, Config) -> clean(Rest, [35] ++ Acc, Config);
clean(<<36, Rest/binary>>, Acc, Config) -> clean(Rest, [36] ++ Acc, Config);
clean(<<37, Rest/binary>>, Acc, Config) -> clean(Rest, [37] ++ Acc, Config);
clean(<<38, Rest/binary>>, Acc, Config) -> clean(Rest, [38] ++ Acc, Config);
clean(<<39, Rest/binary>>, Acc, Config) -> clean(Rest, [39] ++ Acc, Config);
clean(<<40, Rest/binary>>, Acc, Config) -> clean(Rest, [40] ++ Acc, Config);
clean(<<41, Rest/binary>>, Acc, Config) -> clean(Rest, [41] ++ Acc, Config);
clean(<<42, Rest/binary>>, Acc, Config) -> clean(Rest, [42] ++ Acc, Config);
clean(<<43, Rest/binary>>, Acc, Config) -> clean(Rest, [43] ++ Acc, Config);
clean(<<44, Rest/binary>>, Acc, Config) -> clean(Rest, [44] ++ Acc, Config);
clean(<<45, Rest/binary>>, Acc, Config) -> clean(Rest, [45] ++ Acc, Config);
clean(<<46, Rest/binary>>, Acc, Config) -> clean(Rest, [46] ++ Acc, Config);
clean(<<47, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(47, Config) ++ Acc, Config);
clean(<<48, Rest/binary>>, Acc, Config) -> clean(Rest, [48] ++ Acc, Config);
clean(<<49, Rest/binary>>, Acc, Config) -> clean(Rest, [49] ++ Acc, Config);
clean(<<50, Rest/binary>>, Acc, Config) -> clean(Rest, [50] ++ Acc, Config);
clean(<<51, Rest/binary>>, Acc, Config) -> clean(Rest, [51] ++ Acc, Config);
clean(<<52, Rest/binary>>, Acc, Config) -> clean(Rest, [52] ++ Acc, Config);
clean(<<53, Rest/binary>>, Acc, Config) -> clean(Rest, [53] ++ Acc, Config);
clean(<<54, Rest/binary>>, Acc, Config) -> clean(Rest, [54] ++ Acc, Config);
clean(<<55, Rest/binary>>, Acc, Config) -> clean(Rest, [55] ++ Acc, Config);
clean(<<56, Rest/binary>>, Acc, Config) -> clean(Rest, [56] ++ Acc, Config);
clean(<<57, Rest/binary>>, Acc, Config) -> clean(Rest, [57] ++ Acc, Config);
clean(<<58, Rest/binary>>, Acc, Config) -> clean(Rest, [58] ++ Acc, Config);
clean(<<59, Rest/binary>>, Acc, Config) -> clean(Rest, [59] ++ Acc, Config);
clean(<<60, Rest/binary>>, Acc, Config) -> clean(Rest, [60] ++ Acc, Config);
clean(<<61, Rest/binary>>, Acc, Config) -> clean(Rest, [61] ++ Acc, Config);
clean(<<62, Rest/binary>>, Acc, Config) -> clean(Rest, [62] ++ Acc, Config);
clean(<<63, Rest/binary>>, Acc, Config) -> clean(Rest, [63] ++ Acc, Config);
clean(<<64, Rest/binary>>, Acc, Config) -> clean(Rest, [64] ++ Acc, Config);
clean(<<65, Rest/binary>>, Acc, Config) -> clean(Rest, [65] ++ Acc, Config);
clean(<<66, Rest/binary>>, Acc, Config) -> clean(Rest, [66] ++ Acc, Config);
clean(<<67, Rest/binary>>, Acc, Config) -> clean(Rest, [67] ++ Acc, Config);
clean(<<68, Rest/binary>>, Acc, Config) -> clean(Rest, [68] ++ Acc, Config);
clean(<<69, Rest/binary>>, Acc, Config) -> clean(Rest, [69] ++ Acc, Config);
clean(<<70, Rest/binary>>, Acc, Config) -> clean(Rest, [70] ++ Acc, Config);
clean(<<71, Rest/binary>>, Acc, Config) -> clean(Rest, [71] ++ Acc, Config);
clean(<<72, Rest/binary>>, Acc, Config) -> clean(Rest, [72] ++ Acc, Config);
clean(<<73, Rest/binary>>, Acc, Config) -> clean(Rest, [73] ++ Acc, Config);
clean(<<74, Rest/binary>>, Acc, Config) -> clean(Rest, [74] ++ Acc, Config);
clean(<<75, Rest/binary>>, Acc, Config) -> clean(Rest, [75] ++ Acc, Config);
clean(<<76, Rest/binary>>, Acc, Config) -> clean(Rest, [76] ++ Acc, Config);
clean(<<77, Rest/binary>>, Acc, Config) -> clean(Rest, [77] ++ Acc, Config);
clean(<<78, Rest/binary>>, Acc, Config) -> clean(Rest, [78] ++ Acc, Config);
clean(<<79, Rest/binary>>, Acc, Config) -> clean(Rest, [79] ++ Acc, Config);
clean(<<80, Rest/binary>>, Acc, Config) -> clean(Rest, [80] ++ Acc, Config);
clean(<<81, Rest/binary>>, Acc, Config) -> clean(Rest, [81] ++ Acc, Config);
clean(<<82, Rest/binary>>, Acc, Config) -> clean(Rest, [82] ++ Acc, Config);
clean(<<83, Rest/binary>>, Acc, Config) -> clean(Rest, [83] ++ Acc, Config);
clean(<<84, Rest/binary>>, Acc, Config) -> clean(Rest, [84] ++ Acc, Config);
clean(<<85, Rest/binary>>, Acc, Config) -> clean(Rest, [85] ++ Acc, Config);
clean(<<86, Rest/binary>>, Acc, Config) -> clean(Rest, [86] ++ Acc, Config);
clean(<<87, Rest/binary>>, Acc, Config) -> clean(Rest, [87] ++ Acc, Config);
clean(<<88, Rest/binary>>, Acc, Config) -> clean(Rest, [88] ++ Acc, Config);
clean(<<89, Rest/binary>>, Acc, Config) -> clean(Rest, [89] ++ Acc, Config);
clean(<<90, Rest/binary>>, Acc, Config) -> clean(Rest, [90] ++ Acc, Config);
clean(<<91, Rest/binary>>, Acc, Config) -> clean(Rest, [91] ++ Acc, Config);
clean(<<92, Rest/binary>>, Acc, Config) -> clean(Rest, maybe_replace(92, Config) ++ Acc, Config);
clean(<<93, Rest/binary>>, Acc, Config) -> clean(Rest, [93] ++ Acc, Config);
clean(<<94, Rest/binary>>, Acc, Config) -> clean(Rest, [94] ++ Acc, Config);
clean(<<95, Rest/binary>>, Acc, Config) -> clean(Rest, [95] ++ Acc, Config);
clean(<<96, Rest/binary>>, Acc, Config) -> clean(Rest, [96] ++ Acc, Config);
clean(<<97, Rest/binary>>, Acc, Config) -> clean(Rest, [97] ++ Acc, Config);
clean(<<98, Rest/binary>>, Acc, Config) -> clean(Rest, [98] ++ Acc, Config);
clean(<<99, Rest/binary>>, Acc, Config) -> clean(Rest, [99] ++ Acc, Config);
clean(<<100, Rest/binary>>, Acc, Config) -> clean(Rest, [100] ++ Acc, Config);
clean(<<101, Rest/binary>>, Acc, Config) -> clean(Rest, [101] ++ Acc, Config);
clean(<<102, Rest/binary>>, Acc, Config) -> clean(Rest, [102] ++ Acc, Config);
clean(<<103, Rest/binary>>, Acc, Config) -> clean(Rest, [103] ++ Acc, Config);
clean(<<104, Rest/binary>>, Acc, Config) -> clean(Rest, [104] ++ Acc, Config);
clean(<<105, Rest/binary>>, Acc, Config) -> clean(Rest, [105] ++ Acc, Config);
clean(<<106, Rest/binary>>, Acc, Config) -> clean(Rest, [106] ++ Acc, Config);
clean(<<107, Rest/binary>>, Acc, Config) -> clean(Rest, [107] ++ Acc, Config);
clean(<<108, Rest/binary>>, Acc, Config) -> clean(Rest, [108] ++ Acc, Config);
clean(<<109, Rest/binary>>, Acc, Config) -> clean(Rest, [109] ++ Acc, Config);
clean(<<110, Rest/binary>>, Acc, Config) -> clean(Rest, [110] ++ Acc, Config);
clean(<<111, Rest/binary>>, Acc, Config) -> clean(Rest, [111] ++ Acc, Config);
clean(<<112, Rest/binary>>, Acc, Config) -> clean(Rest, [112] ++ Acc, Config);
clean(<<113, Rest/binary>>, Acc, Config) -> clean(Rest, [113] ++ Acc, Config);
clean(<<114, Rest/binary>>, Acc, Config) -> clean(Rest, [114] ++ Acc, Config);
clean(<<115, Rest/binary>>, Acc, Config) -> clean(Rest, [115] ++ Acc, Config);
clean(<<116, Rest/binary>>, Acc, Config) -> clean(Rest, [116] ++ Acc, Config);
clean(<<117, Rest/binary>>, Acc, Config) -> clean(Rest, [117] ++ Acc, Config);
clean(<<118, Rest/binary>>, Acc, Config) -> clean(Rest, [118] ++ Acc, Config);
clean(<<119, Rest/binary>>, Acc, Config) -> clean(Rest, [119] ++ Acc, Config);
clean(<<120, Rest/binary>>, Acc, Config) -> clean(Rest, [120] ++ Acc, Config);
clean(<<121, Rest/binary>>, Acc, Config) -> clean(Rest, [121] ++ Acc, Config);
clean(<<122, Rest/binary>>, Acc, Config) -> clean(Rest, [122] ++ Acc, Config);
clean(<<123, Rest/binary>>, Acc, Config) -> clean(Rest, [123] ++ Acc, Config);
clean(<<124, Rest/binary>>, Acc, Config) -> clean(Rest, [124] ++ Acc, Config);
clean(<<125, Rest/binary>>, Acc, Config) -> clean(Rest, [125] ++ Acc, Config);
clean(<<126, Rest/binary>>, Acc, Config) -> clean(Rest, [126] ++ Acc, Config);
clean(<<127, Rest/binary>>, Acc, Config) -> clean(Rest, [127] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X == 16#2028; X == 16#2029 ->
    clean(Rest, maybe_replace(X, Config) ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X < 16#dcff ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X > 16#dfff, X < 16#fdd0 ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X > 16#fdef, X < 16#fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#10000, X < 16#1fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#20000, X < 16#2fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#30000, X < 16#3fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#40000, X < 16#4fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#50000, X < 16#5fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#60000, X < 16#6fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#70000, X < 16#7fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#80000, X < 16#8fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#90000, X < 16#9fffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#a0000, X < 16#afffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#b0000, X < 16#bfffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#c0000, X < 16#cfffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#d0000, X < 16#dfffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#e0000, X < 16#efffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#f0000, X < 16#ffffe ->
    clean(Rest, [X] ++ Acc, Config);
clean(<<X/utf8, Rest/binary>>, Acc, Config) when X >= 16#100000, X < 16#10fffe ->
    clean(Rest, [X] ++ Acc, Config);
%% noncharacters
clean(<<_/utf8, Rest/binary>>, Acc, Config) ->
    clean(Rest, maybe_replace(noncharacter, Config) ++ Acc, Config);
%% surrogates
clean(<<237, X, _, Rest/binary>>, Acc, Config) when X >= 160 ->
    clean(Rest, maybe_replace(surrogate, Config) ++ Acc, Config);
%% u+fffe and u+ffff for R14BXX
clean(<<239, 191, X, Rest/binary>>, Acc, Config) when X == 190; X == 191 ->
    clean(Rest, maybe_replace(noncharacter, Config) ++ Acc, Config);
%% overlong encodings and missing continuations of a 2 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 192, X =< 223 ->
    clean(strip_continuations(Rest, 1), maybe_replace(badutf, Config) ++ Acc, Config);
%% overlong encodings and missing continuations of a 3 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 224, X =< 239 ->
    clean(strip_continuations(Rest, 2), maybe_replace(badutf, Config) ++ Acc, Config);
%% overlong encodings and missing continuations of a 4 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 240, X =< 247 ->
    clean(strip_continuations(Rest, 3), maybe_replace(badutf, Config) ++ Acc, Config);
clean(<<_, Rest/binary>>, Acc, Config) ->
    clean(Rest, maybe_replace(badutf, Config) ++ Acc, Config).


strip_continuations(Bin, 0) -> Bin;
strip_continuations(<<X, Rest/binary>>, N) when X >= 128, X =< 191 ->
    strip_continuations(Rest, N - 1);
%% not a continuation byte
strip_continuations(Bin, _) -> Bin.


maybe_replace($\b, #config{escaped_strings=true}) -> [$b, $\\];
maybe_replace($\t, #config{escaped_strings=true}) -> [$t, $\\];
maybe_replace($\n, #config{escaped_strings=true}) -> [$n, $\\];
maybe_replace($\f, #config{escaped_strings=true}) -> [$f, $\\];
maybe_replace($\r, #config{escaped_strings=true}) -> [$r, $\\];
maybe_replace($\", #config{escaped_strings=true}) -> [$\", $\\];
maybe_replace($/, Config=#config{escaped_strings=true}) ->
    case Config#config.escaped_forward_slashes of
        true -> [$/, $\\]
        ; false -> [$/]
    end;
maybe_replace($\\, #config{escaped_strings=true}) -> [$\\, $\\];
maybe_replace(X, Config=#config{escaped_strings=true})  when X == 16#2028; X == 16#2029 ->
    case Config#config.unescaped_jsonp of
        true -> [X]
        ; false -> lists:reverse(jsx_utils:json_escape_sequence(X))
    end;
maybe_replace(X, #config{escaped_strings=true}) when X < 32 ->
    lists:reverse(jsx_utils:json_escape_sequence(X));
maybe_replace(noncharacter, #config{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(surrogate, #config{replaced_bad_utf8=true}) -> [16#fffd];
maybe_replace(badutf, #config{replaced_bad_utf8=true}) -> [16#fffd];
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
        ++ lists:seq(35, 46)
        ++ lists:seq(48, 91)
        ++ lists:seq(93, 16#2027)
        ++ lists:seq(16#202a, 16#d7ff)
        ++ lists:seq(16#e000, 16#fdcf)
        ++ lists:seq(16#fdf0, 16#fffd)
    ).

extended_codepoints() ->
    unicode:characters_to_binary(
        lists:seq(16#10000, 16#1fffd) ++ [
            16#20000, 16#30000, 16#40000, 16#50000, 16#60000,
            16#70000, 16#80000, 16#90000, 16#a0000, 16#b0000,
            16#c0000, 16#d0000, 16#e0000, 16#f0000, 16#100000
        ]
    ).

reserved_space() -> [ to_fake_utf8(N) || N <- lists:seq(16#fdd0, 16#fdef) ].

surrogates() -> [ to_fake_utf8(N) || N <- lists:seq(16#d800, 16#dfff) ].

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


decode(String, Config) ->
    try
        [{string, clean_string(String, Config)}, end_json]
    catch
        error:badarg -> {error, badarg}
    end.


clean_string_test_() ->
    [
        {"clean codepoints", ?_assertEqual(
            [{string, codepoints()}, end_json],
            decode(codepoints(), #config{})
        )},
        {"clean extended codepoints", ?_assertEqual(
            [{string, extended_codepoints()}, end_json],
            decode(extended_codepoints(), #config{})
        )},
        {"escape path codepoints", ?_assertEqual(
            [{string, codepoints()}, end_json],
            decode(codepoints(), #config{escaped_strings=true})
        )},
        {"escape path extended codepoints", ?_assertEqual(
            [{string, extended_codepoints()}, end_json],
            decode(extended_codepoints(), #config{escaped_strings=true})
        )},
        {"error reserved space", ?_assertEqual(
            lists:duplicate(length(reserved_space()), {error, badarg}),
            lists:map(fun(Codepoint) -> decode(Codepoint, #config{}) end, reserved_space())
        )},
        {"error surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), {error, badarg}),
            lists:map(fun(Codepoint) -> decode(Codepoint, #config{}) end, surrogates())
        )},
        {"error noncharacters", ?_assertEqual(
            lists:duplicate(length(noncharacters()), {error, badarg}),
            lists:map(fun(Codepoint) -> decode(Codepoint, #config{}) end, noncharacters())
        )},
        {"error extended noncharacters", ?_assertEqual(
            lists:duplicate(length(extended_noncharacters()), {error, badarg}),
            lists:map(fun(Codepoint) -> decode(Codepoint, #config{}) end, extended_noncharacters())
        )},
        {"clean reserved space", ?_assertEqual(
            lists:duplicate(length(reserved_space()), [{string, <<16#fffd/utf8>>}, end_json]),
            lists:map(fun(Codepoint) -> decode(Codepoint, #config{replaced_bad_utf8=true}) end, reserved_space())
        )},
        {"clean surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), [{string, <<16#fffd/utf8>>}, end_json]),
            lists:map(fun(Codepoint) -> decode(Codepoint, #config{replaced_bad_utf8=true}) end, surrogates())
        )},
        {"clean noncharacters", ?_assertEqual(
            lists:duplicate(length(noncharacters()), [{string, <<16#fffd/utf8>>}, end_json]),
            lists:map(fun(Codepoint) -> decode(Codepoint, #config{replaced_bad_utf8=true}) end, noncharacters())
        )},
        {"clean extended noncharacters", ?_assertEqual(
            lists:duplicate(length(extended_noncharacters()), [{string, <<16#fffd/utf8>>}, end_json]),
            lists:map(fun(Codepoint) -> decode(Codepoint, #config{replaced_bad_utf8=true}) end, extended_noncharacters())
        )}
    ].


maybe_escape(Bin, Config) -> clean_string(Bin, Config).

escape_test_() ->
    [
        {"maybe_escape backspace", ?_assertEqual(
            <<"\\b">>,
            maybe_escape(<<16#0008/utf8>>, #config{escaped_strings=true})
        )},
        {"don't escape backspace", ?_assertEqual(
            <<"\b">>,
            maybe_escape(<<16#0008/utf8>>, #config{})
        )},
        {"maybe_escape tab", ?_assertEqual(
            <<"\\t">>,
            maybe_escape(<<16#0009/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape newline", ?_assertEqual(
            <<"\\n">>,
            maybe_escape(<<16#000a/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape formfeed", ?_assertEqual(
            <<"\\f">>,
            maybe_escape(<<16#000c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape carriage return", ?_assertEqual(
            <<"\\r">>,
            maybe_escape(<<16#000d/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape quote", ?_assertEqual(
            <<"\\\"">>,
            maybe_escape(<<16#0022/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape forward slash", ?_assertEqual(
            <<"\\/">>,
            maybe_escape(<<16#002f/utf8>>, #config{escaped_strings=true, escaped_forward_slashes=true})
        )},
        {"do not maybe_escape forward slash", ?_assertEqual(
            <<"/">>,
            maybe_escape(<<16#002f/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape backslash", ?_assertEqual(
            <<"\\\\">>,
            maybe_escape(<<16#005c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape jsonp (u2028)", ?_assertEqual(
            <<"\\u2028">>,
            maybe_escape(<<16#2028/utf8>>, #config{escaped_strings=true})
        )},
        {"do not maybe_escape jsonp (u2028)", ?_assertEqual(
            <<16#2028/utf8>>,
            maybe_escape(<<16#2028/utf8>>, #config{escaped_strings=true, unescaped_jsonp=true})
        )},
        {"maybe_escape jsonp (u2029)", ?_assertEqual(
            <<"\\u2029">>,
            maybe_escape(<<16#2029/utf8>>, #config{escaped_strings=true})
        )},
        {"do not maybe_escape jsonp (u2029)", ?_assertEqual(
            <<16#2029/utf8>>,
            maybe_escape(<<16#2029/utf8>>, #config{escaped_strings=true, unescaped_jsonp=true})
        )},
        {"maybe_escape u0000", ?_assertEqual(
            <<"\\u0000">>,
            maybe_escape(<<16#0000/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0001", ?_assertEqual(
            <<"\\u0001">>,
            maybe_escape(<<16#0001/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0002", ?_assertEqual(
            <<"\\u0002">>,
            maybe_escape(<<16#0002/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0003", ?_assertEqual(
            <<"\\u0003">>,
            maybe_escape(<<16#0003/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0004", ?_assertEqual(
            <<"\\u0004">>,
            maybe_escape(<<16#0004/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0005", ?_assertEqual(
            <<"\\u0005">>,
            maybe_escape(<<16#0005/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0006", ?_assertEqual(
            <<"\\u0006">>,
            maybe_escape(<<16#0006/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0007", ?_assertEqual(
            <<"\\u0007">>,
            maybe_escape(<<16#0007/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000b", ?_assertEqual(
            <<"\\u000b">>,
            maybe_escape(<<16#000b/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000e", ?_assertEqual(
            <<"\\u000e">>,
            maybe_escape(<<16#000e/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000f", ?_assertEqual(
            <<"\\u000f">>,
            maybe_escape(<<16#000f/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0010", ?_assertEqual(
            <<"\\u0010">>,
            maybe_escape(<<16#0010/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0011", ?_assertEqual(
            <<"\\u0011">>,
            maybe_escape(<<16#0011/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0012", ?_assertEqual(
            <<"\\u0012">>,
            maybe_escape(<<16#0012/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0013", ?_assertEqual(
            <<"\\u0013">>,
            maybe_escape(<<16#0013/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0014", ?_assertEqual(
            <<"\\u0014">>,
            maybe_escape(<<16#0014/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0015", ?_assertEqual(
            <<"\\u0015">>,
            maybe_escape(<<16#0015/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0016", ?_assertEqual(
            <<"\\u0016">>,
            maybe_escape(<<16#0016/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0017", ?_assertEqual(
            <<"\\u0017">>,
            maybe_escape(<<16#0017/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0018", ?_assertEqual(
            <<"\\u0018">>,
            maybe_escape(<<16#0018/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0019", ?_assertEqual(
            <<"\\u0019">>,
            maybe_escape(<<16#0019/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001a", ?_assertEqual(
            <<"\\u001a">>,
            maybe_escape(<<16#001a/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001b", ?_assertEqual(
            <<"\\u001b">>,
            maybe_escape(<<16#001b/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001c", ?_assertEqual(
            <<"\\u001c">>,
            maybe_escape(<<16#001c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001d", ?_assertEqual(
            <<"\\u001d">>,
            maybe_escape(<<16#001d/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001e", ?_assertEqual(
            <<"\\u001e">>,
            maybe_escape(<<16#001e/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001f", ?_assertEqual(
            <<"\\u001f">>,
            maybe_escape(<<16#001f/utf8>>, #config{escaped_strings=true})
        )}
    ].


bad_utf8_test_() ->
    [
        {"noncharacter u+fffe", ?_assertError(
            badarg,
            clean_string(to_fake_utf8(16#fffe), #config{})
        )},
        {"noncharacter u+fffe replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(to_fake_utf8(16#fffe), #config{replaced_bad_utf8=true})
        )},
        {"noncharacter u+ffff", ?_assertError(
            badarg,
            clean_string(to_fake_utf8(16#ffff), #config{})
        )},
        {"noncharacter u+ffff replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(to_fake_utf8(16#ffff), #config{replaced_bad_utf8=true})
        )},
        {"orphan continuation byte u+0080", ?_assertError(
            badarg,
            clean_string(<<16#0080>>, #config{})
        )},
        {"orphan continuation byte u+0080 replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#0080>>, #config{replaced_bad_utf8=true})
        )},
        {"orphan continuation byte u+00bf", ?_assertError(
            badarg,
            clean_string(<<16#00bf>>, #config{})
        )},
        {"orphan continuation byte u+00bf replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00bf>>, #config{replaced_bad_utf8=true})
        )},
        {"2 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #config{})
        )},
        {"2 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 2),
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"3 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #config{})
        )},
        {"3 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 3),
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"4 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #config{})
        )},
        {"4 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 4),
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"5 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #config{})
        )},
        {"5 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 5),
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"6 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #config{})
        )},
        {"6 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 6),
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #config{replaced_bad_utf8=true})
        )},
        {"all continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, #config{})
        )},
        {"all continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf))),
            clean_string(
                <<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>,
                #config{replaced_bad_utf8=true}
            )
        )},
        {"lonely start byte", ?_assertError(
            badarg,
            clean_string(<<16#00c0>>, #config{})
        )},
        {"lonely start byte replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00c0>>, #config{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (2 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00c0, 32, 16#00df>>, #config{})
        )},
        {"lonely start bytes (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00c0, 32, 16#00df>>, #config{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (3 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00e0, 32, 16#00ef>>, #config{})
        )},
        {"lonely start bytes (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00e0, 32, 16#00ef>>, #config{replaced_bad_utf8=true})
        )},
        {"lonely start bytes (4 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00f0, 32, 16#00f7>>, #config{})
        )},
        {"lonely start bytes (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00f0, 32, 16#00f7>>, #config{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (3 byte)", ?_assertError(
            badarg,
            clean_string(<<224, 160, 32>>, #config{})
        )},
        {"missing continuation byte (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<224, 160, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (4 byte missing one)", ?_assertError(
            badarg,
            clean_string(<<240, 144, 128, 32>>, #config{})
        )},
        {"missing continuation byte (4 byte missing one) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 128, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"missing continuation byte (4 byte missing two)", ?_assertError(
            badarg,
            clean_string(<<240, 144, 32>>, #config{})
        )},
        {"missing continuation byte (4 byte missing two) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (2 byte)", ?_assertError(
            badarg,
            clean_string(<<16#c0, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c0, 16#af, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (3 byte)", ?_assertError(
            badarg,
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"overlong encoding of u+002f (4 byte)", ?_assertError(
            badarg,
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"highest overlong 2 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#c1, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 2 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c1, 16#bf, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"highest overlong 3 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 3 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #config{replaced_bad_utf8=true})
        )},
        {"highest overlong 4 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 4 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #config{replaced_bad_utf8=true})
        )}
    ].


-endif.