parse_opts(Opts) ->
    parse_opts(Opts, #opts{}).

parse_opts([], Opts) ->
    Opts;
parse_opts([loose_unicode|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{loose_unicode=true});
parse_opts([escape_forward_slash|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{escape_forward_slash=true});
parse_opts(_, _) ->
    {error, badarg}.