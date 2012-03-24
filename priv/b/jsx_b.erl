%% The MIT License

%% Copyright (c) 2012 Dmitry Kolesnikov <fogfish@ovi.com>

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
-module(jsx_b).

-export([run/2, hot/0]).

-define(LEN_KEY,  32).  %% upper bound of object attribute
-define(LEN_STR, 256).  %% upper bound of string value
-define(LEN_INT,   7).  %% upper bound of digits
-define(JSON,     20).  %% number of attributes
-define(ALPHA,"qwertyuiopasdfghjklzxcvbnm").
-define(TEXT,"qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890\"\\\b\f\n\r\t").
-define(DIGIT,"123456789").


run(Set, Loop) ->
   Json = lists:map(fun(_) -> gen_json(?JSON) end, lists:seq(1, Set)),
   Term = lists:map(fun(_) -> gen_term(?JSON) end, lists:seq(1, Set)),
   [
      {
         b_jsx_json(Json, Loop)%, 
         %b_mochi_json(Json, Loop)
      },
      {
         b_jsx_term(Term, Loop)%, 
         %b_mochi_term(Term, Loop)
      }
   ].
   
hot() ->   
   b_jsx_term([gen_term(?JSON)], 100).
   
   
b_jsx_json(Set, Loop) ->
   {T, _} = timer:tc(
      fun() ->
         lists:foreach(
            fun(_) -> 
               lists:map(fun(X) -> jsx:to_term(X) end, Set)
            end,
            lists:seq(1, Loop)
         )
      end,
      []
   ),
   {jsx, to_term, T / 1000, T / (Loop * length(Set) * 1000)}.
   
b_jsx_term(Set, Loop) ->
   erlang:garbage_collect(),
   {T, _} = timer:tc(
      fun() ->
         lists:foreach(
            fun(_) -> 
               %error_logger:info_report([{mem_jsx, erlang:memory(processes)}]),
               lists:map(fun(X) -> jsx:to_json(X) end, Set)
            end,
            lists:seq(1, Loop)
         )
      end,
      []
   ),
   {jsx, to_json, T / 1000, T / (Loop * length(Set) * 1000)}.   

   
b_mochi_json(Set, Loop) ->
   {T, _} = timer:tc(
      fun() ->
         lists:foreach(
            fun(_) -> 
               lists:map(fun(X) -> mochijson2:decode(X) end, Set)
            end,
            lists:seq(1, Loop)
         )
      end,
      []
   ),
   {mochi, to_term, T / 1000, T / (Loop * length(Set) * 1000)}.   
   
b_mochi_term(Set, Loop) ->
   erlang:garbage_collect(),
   {T, _} = timer:tc(
      fun() ->
         lists:foreach(
            fun(_) -> 
               %error_logger:info_report([{mem_mochi, erlang:memory(processes)}]),
               lists:map(fun(X) -> mochijson2:encode({struct, X})end, Set)
            end,
            lists:seq(1, Loop)
         )
      end,
      []
   ),
   {mochi, to_json, T / 1000, T / (Loop * length(Set) * 1000)}.   
   
   
%%
%% generates a json object
gen_json(Len) ->
   list_to_binary(
      io_lib:format("{~s}", [
         string:join(
            lists:map(
               fun(_) ->
                  case random:uniform(2) of
                  1 -> 
                     io_lib:format("\"~s\":\"~s\"", 
                        [rstring(?LEN_KEY, ?ALPHA), rstring(?LEN_STR, ?ALPHA)]
                     );
                  2 ->
                     io_lib:format("\"~s\":~s", 
                        [rstring(?LEN_KEY, ?ALPHA), rstring(?LEN_INT, ?DIGIT)]
                     )
                  end
               end,
               lists:seq(1,Len)
            ),
            ","
         )
      ])
   ).

gen_term(Len) ->
   lists:map(
      fun(_) ->
         case random:uniform(2) of
         1 -> {
              list_to_binary(rstring(?LEN_KEY, ?ALPHA)), 
              list_to_binary(rstring(?LEN_STR, ?ALPHA))
              };
         2 -> {
              list_to_binary(rstring(?LEN_KEY, ?ALPHA)), 
              list_to_integer(rstring(?LEN_INT, ?DIGIT))
              }
         end
      end,
      lists:seq(1,Len)
   ).
   
%%
%%
rstring(Length, Alphabet) ->
   ustring(random:uniform(Length), Alphabet).
   
%%
%% from http://blog.teemu.im/2009/11/07/generating-random-strings-in-erlang/
ustring(Length, AllowedChars) ->
   lists:foldl(
      fun(_, Acc) ->
         [lists:nth(
            random:uniform(length(AllowedChars)),
            AllowedChars
         )] ++ Acc
      end, 
      [], 
      lists:seq(1, Length)
   ).