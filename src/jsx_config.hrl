-record(config, {
    replaced_bad_utf8 = false         :: boolean(),
    escaped_forward_slashes = false   :: boolean(),
    single_quoted_strings = false     :: boolean(),
    unescaped_jsonp = false           :: boolean(),
    comments = false                  :: boolean(),
    escaped_strings = false           :: boolean(),
    dirty_strings = false             :: boolean(),
    ignored_bad_escapes = false       :: boolean(),
    explicit_end = false              :: boolean(),
    pre_encode = false                :: false | fun((any()) -> any()),
    error_handler = false             :: false | jsx_config:handler(),
    incomplete_handler = false        :: false | jsx_config:handler()
}).

