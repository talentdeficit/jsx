-record(config, {
    escaped_forward_slashes = false     :: boolean(),
    escaped_strings = false             :: boolean(),
    unescaped_jsonp = false             :: boolean(),
    dirty_strings = false               :: boolean(),
    strict_comments = false             :: boolean(),
    strict_commas = false               :: boolean(),
    strict_utf8 = false                 :: boolean(),
    strict_single_quotes = false        :: boolean(),
    strict_escapes = false              :: boolean(),
    stream = false                      :: boolean(),
    uescape = false                     :: boolean(),
    error_handler = false               :: false | jsx_config:handler(),
    incomplete_handler = false          :: false | jsx_config:handler()
}).
