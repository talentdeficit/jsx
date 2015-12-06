defmodule Twitter.Encode.Map do
  use Benchfella

  @tweet %{
    "contributors" => [14927800],
    "coordinates" => :null,
    "created_at" => "Wed Jun 06 20:07:10 +0000 2012",
    "entities" => %{
      "hashtags" => [%{"indices" => [19,31],"text" => "Twitterbird"}],
      "urls" => [%{
        "display_url" => "dev.twitter.com/terms/display-…",
        "expanded_url" => "https://dev.twitter.com/terms/display-guidelines",
        "indices" => "La",
        "url" => "https://t.co/Ed4omjYs"
      }],
      "user_mentions" => []
    },
    "favorited" => false,
    "geo" => :null,
    "id" => 210462857140252672,
    "id_str" => "210462857140252672",
    "in_reply_to_screen_name" => :null,
    "in_reply_to_status_id" => :null,
    "in_reply_to_status_id_str" => :null,
    "in_reply_to_user_id" => :null,
    "in_reply_to_user_id_str" => :null,
    "place" => :null,
    "possibly_sensitive" => false,
    "retweet_count" => 66,
    "retweeted" => true,
    "source" => "web",
    "text" => "Along with our new %Twitterbird, we've also updated our Display Guidelines: https://t.co/Ed4omjYs  ^JC",
    "truncated" => false,
    "user" => %{
      "protected" => false,
      "id_str" => "6253282",
      "show_all_inline_media" => false,
      "friends_count" => 31,
      "followers_count" => 1212963,
      "following" => true,
      "default_profile" => true,
      "profile_sidebar_fill_color" => "DDEEF6",
      "id" => 6253282,
      "profile_image_url" => "http://a0.twimg.com/profile_images/2284174872/7df3h38zabcvjylnyfe3_normal.png",
      "profile_link_color" => "0084B4",
      "verified" => true,
      "utc_offset" => -28800,
      "profile_sidebar_border_color" => "C0DEED",
      "statuses_count" => 3333,
      "profile_text_color" => "333333",
      "is_translator" => false,
      "lang" => "en",
      "profile_background_image_url_https" => "https://si0.twimg.com/images/themes/theme1/bg.png",
      "listed_count" => 10774,
      "location" => "San Francisco, CA",
      "contributors_enabled" => true,
      "profile_background_image_url" => "http://a0.twimg.com/images/themes/theme1/bg.png",
      "created_at" => "Wed May 23 06:01:13 +0000 2007",
      "name" => "Twitter API",
      "profile_background_color" => "C0DEED",
      "notifications" => :null,
      "entities" => %{
        "description" => %{"urls" => []},
        "url" => %{"urls" => [%{"expanded_url" => :null, "indices" => [0,22], "url" => "http://dev.twitter.com"}]}
      },
      "url" => "http://dev.twitter.com",
      "profile_background_tile" => false,
      "default_profile_image" => false,
      "description" => "The Real Twitter API. I tweet about API changes, service issues and happily answer questions about Twitter and our API. Don't get an answer? It's on my website.",
      "favourites_count" => 24,
      "geo_enabled" => true,
      "profile_image_url_https" => "https://si0.twimg.com/profile_images/2284174872/7df3h38zabcvjylnyfe3_normal.png",
      "profile_use_background_image" => true,
      "time_zone" => "Pacific Time (US & Canada)",
      "follow_request_sent" => false,
      "screen_name" => "twitterapi"
    }
  }

  bench "map     :: twitter" do
    :jsx.encode @tweet
  end
end