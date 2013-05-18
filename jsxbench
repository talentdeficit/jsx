#!/usr/bin/env escript

%% The MIT License

%% Copyright (c) 2012 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

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

-mode(compile).


-define(averageN(Test, N),
    {average, {repeat, N, Test}}
).


main([]) ->
    %% preload jsx mods
    jsx:to_term(<<"{}">>),
    format(frequency:profile({"empty object to term", ?averageN({jsx, to_term, [<<"{}">>]}, 1000)})),
    format(frequency:profile({"empty object to json", ?averageN({jsx, to_json, [[{}]]}, 1000)})),
    format(frequency:profile({"empty array to term", ?averageN({jsx, to_term, [<<"[]">>]}, 1000)})),
    format(frequency:profile({"empty array to json", ?averageN({jsx, to_json, [[]]}, 1000)})),
    format(frequency:profile({"sample tweet to term", ?averageN({jsx, to_term, [sample_tweet()]}, 1000)})),
    format(frequency:profile({"sample tweet to json", ?averageN({jsx, to_json, [jsx:to_term(sample_tweet())]}, 1000)})),
    format(frequency:profile({"sample github user to term", ?averageN({jsx, to_term, [sample_github_user()]}, 1000)})),
    format(frequency:profile({"sample github user to json", ?averageN({jsx, to_json, [jsx:to_term(sample_github_user())]}, 1000)})).


format([]) -> ok;
format([{name, Name}|Rest]) ->
    io:format("name    : ~p~n", [Name]),
    format(Rest);
format([{time, Time}|Rest]) ->
    io:format("time    : ~p~n", [Time]),
    format(Rest);
format([{error, Error}|Rest]) ->
    io:format("error   : ~p~n", [Error]),
    format(Rest);
format([Result|Rest]) when is_list(Result) -> format(Result), format(Rest);
format([_|Rest]) -> format(Rest).


sample_tweet() -> 
    <<"{
        \"coordinates\": null,
        \"created_at\": \"Sat Sep 10 22:23:38 +0000 2011\",
        \"truncated\": false,
        \"favorited\": false,
        \"id_str\": \"112652479837110273\",
        \"entities\": {
            \"urls\": [{
                \"expanded_url\": \"http://instagr.am/p/MuW67/\",
                \"url\": \"http://t.co/6J2EgYM\",
                \"indices\": [67, 86],
                \"display_url\": \"instagr.am/p/MuW67/\"
            }],
            \"hashtags\": [{
                \"text\": \"tcdisrupt\",
                \"indices\": [32,42]
            }],
            \"user_mentions\": [
                {
                    \"name\": \"Twitter\",
                    \"id_str\": \"783214\",
                    \"id\": 783214,
                    \"indices\": [0, 8],
                    \"screen_name\": \"twitter\"
                },
                {
                    \"name\": \"Picture.ly\",
                    \"id_str\": \"334715534\",
                    \"id\": 334715534,
                    \"indices\": [15, 28],
                    \"screen_name\": \"SeePicturely\"
                },
                {
                    \"name\": \"Bosco So\",
                    \"id_str\": \"14792670\",
                    \"id\": 14792670,
                    \"indices\": [46, 58],
                    \"screen_name\": \"boscomonkey\"
                },
                {
                    \"name\": \"Taylor Singletary\",
                    \"id_str\": \"819797\",
                    \"id\": 819797,
                    \"indices\": [59, 66],
                    \"screen_name\": \"episod\"
                }
            ]
        },
        \"in_reply_to_user_id_str\": \"783214\",
        \"text\": \"@twitter meets @seepicturely at #tcdisrupt cc.@boscomonkey @episod http://t.co/6J2EgYM\",
        \"contributors\": null,
        \"id\": 112652479837110273,
        \"retweet_count\": 0,
        \"in_reply_to_status_id_str\": null,
        \"geo\": null,
        \"retweeted\": false,
        \"possibly_sensitive\": false,
        \"in_reply_to_user_id\": 783214,
        \"place\": null,
        \"source\": \"<a href=\\\"http://instagr.am\\\" rel=\\\"nofollow\\\">Instagram</a>\",
        \"user\": {
            \"profile_sidebar_border_color\": \"eeeeee\",
            \"profile_background_tile\": true,
            \"profile_sidebar_fill_color\": \"efefef\",
            \"name\": \"Eoin McMillan \",
            \"profile_image_url\": \"http://a1.twimg.com/profile_images/1380912173/Screen_shot_2011-06-03_at_7.35.36_PM_normal.png\",
            \"created_at\": \"Mon May 16 20:07:59 +0000 2011\",
            \"location\": \"Twitter\",
            \"profile_link_color\": \"009999\",
            \"follow_request_sent\": null,
            \"is_translator\": false,
            \"id_str\": \"299862462\",
            \"favourites_count\": 0,
            \"default_profile\": false,
            \"url\": \"http://www.eoin.me\",
            \"contributors_enabled\": false,
            \"id\": 299862462,
            \"utc_offset\": null,
            \"profile_image_url_https\": \"https://si0.twimg.com/profile_images/1380912173/Screen_shot_2011-06-03_at_7.35.36_PM_normal.png\",
            \"profile_use_background_image\": true,
            \"listed_count\": 0,
            \"followers_count\": 9,
            \"lang\": \"en\",
            \"profile_text_color\": \"333333\",
            \"protected\": false,
            \"profile_background_image_url_https\": \"https://si0.twimg.com/images/themes/theme14/bg.gif\",
            \"description\": \"Eoin's photography account. See @mceoin for tweets.\",
            \"geo_enabled\": false,
            \"verified\": false,
            \"profile_background_color\": \"131516\",
            \"time_zone\": null,
            \"notifications\": null,
            \"statuses_count\": 255,
            \"friends_count\": 0,
            \"default_profile_image\": false,
            \"profile_background_image_url\": \"http://a1.twimg.com/images/themes/theme14/bg.gif\",
            \"screen_name\": \"imeoin\",
            \"following\": null,
            \"show_all_inline_media\": false
        },
        \"in_reply_to_screen_name\": \"twitter\",
        \"in_reply_to_status_id\": null
    }">>.


sample_github_user() ->
    <<"{
        \"user\": {
            \"gravatar_id\": \"b8dbb1987e8e5318584865f880036796\",
            \"company\": \"GitHub\",
            \"name\": \"Chris Wanstrath\",
            \"created_at\": \"2007/10/19 22:24:19 -0700\",
            \"location\": \"San Francisco, CA\",
            \"public_repo_count\": 98,
            \"public_gist_count\": 270,
            \"blog\": \"http://chriswanstrath.com/\",
            \"following_count\": 196,
            \"id\": 2,
            \"type\": \"User\",
            \"permission\": null,
            \"followers_count\": 1692,
            \"login\": \"defunkt\",
            \"email\": \"chris@wanstrath.com\"
        }
    }">>.
