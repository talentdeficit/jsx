-module(jsx_prettify).

-export([pretty/2, jsx_event/2]).

-record(opts, {
    indent = 4
}).


pretty(JSON, Opts) ->
    Init = init(parse_opts(Opts, #opts{})),
    {{_, Result}, Rest} = (jsx:decoder({{pretty_printer, jsx_event}, Init}, []))(JSON),
    case jsx:tail_clean(Rest) of
        true -> Result
        ; _ -> exit(badarg)
    end.
    
parse_opts([{indent, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{indent = Val});
parse_opts([], Opts) ->
    Opts.
    
init(Opts) ->
    {[], Opts#opts.indent, 0, new}.
    
    
jsx_event(start_object, {Acc, Indent, Level, value}) ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ "{", Indent, Level + 1, new};
jsx_event(start_object, {Acc, Indent, Level, _}) ->
    {Acc ++ "{", Indent, Level + 1, new};
    
jsx_event(start_array, {Acc, Indent, Level, value}) ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ "[", Indent, Level + 1, new};
jsx_event(start_array, {Acc, Indent, Level, _}) ->
    {Acc ++ "[", Indent, Level + 1, new};


jsx_event(end_object, {Acc, Indent, Level, value}) ->
    {Acc ++ "\n" ++ indent(Indent, Level - 1) ++ "}", Indent, Level - 1, value};
jsx_event(end_object, {Acc, Indent, Level, new}) ->
    {Acc ++ "}", Indent, Level - 1, value};


jsx_event(end_array, {Acc, Indent, Level, value}) ->
    {Acc ++ "\n" ++ indent(Indent, Level - 1) ++ "]", Indent, Level - 1, value};
jsx_event(end_array, {Acc, Indent, Level, new}) ->
    {Acc ++ "]", Indent, Level - 1, value};


jsx_event({key, Key}, {Acc, Indent, Level, value}) ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ "\"" ++ Key ++ "\": ", Indent, Level, key};
jsx_event({key, Key}, {Acc, Indent, Level, _}) ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ "\"" ++ Key ++ "\": ", Indent, Level, key};


jsx_event({Type, Value}, {Acc, Indent, Level, value})  ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ format(Type, Value), Indent, Level, value};
jsx_event({Type, Value}, {Acc, Indent, Level, new})  ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ format(Type, Value), Indent, Level, value};
jsx_event({Type, Value}, {Acc, Indent, Level, key}) ->
    {Acc ++ format(Type, Value), Indent, Level, value};


jsx_event(eof, {Acc, _, _, _}) ->
    Acc.
    

format(number, Number) ->
    Number;
format(string, String) ->
    "\"" ++ String ++ "\"";
format(literal, Literal) ->
    erlang:atom_to_list(Literal).
    

indent(Indent, Level) ->
    [ 16#20 || _ <- lists:seq(1, Indent * Level) ].
    