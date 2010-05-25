-module(pretty_printer).

-export([print/2, jsx_event/2]).

-record(opts, {
    indent = 4
}).

print(JSON, Opts) ->
    Init = init(parse_opts(Opts, #opts{})),
    {{pretty_printer, Result}, _} = (jsx:decoder({pretty_printer, Init}, []))(JSON),
    Result.
    
parse_opts([{indent, Val}|Rest], Opts) ->
    parse_opts(Rest, Opts#opts{indent = Val});
parse_opts([], Opts) ->
    Opts.
    
init(Opts) ->
    {[], Opts#opts.indent, 0, false}.
    

jsx_event(start_object, {Acc, Indent, Level, _}) ->
    {Acc ++ "{", Indent, Level + 1, false};
jsx_event(start_array, {Acc, Indent, Level, _}) ->
    {Acc ++ "[", Indent, Level + 1, false};

jsx_event({key, Key}, {Acc, Indent, Level, true}) ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ "\"" ++ Key ++ "\": ", Indent, Level, false};
jsx_event({key, Key}, {Acc, Indent, Level, false}) ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ "\"" ++ Key ++ "\": ", Indent, Level, false};

jsx_event({number, Number}, {Acc, Indent, Level, true})  ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ Number, Indent, Level, true};
jsx_event({number, Number}, {Acc, Indent, Level, false})  ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ Number, Indent, Level, true};

jsx_event({string, String}, {Acc, Indent, Level, true})  ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ "\"" ++ String ++ "\"", Indent, Level, true};
jsx_event({string, String}, {Acc, Indent, Level, false})  ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ "\"" ++ String ++ "\"", Indent, Level, true};

jsx_event({literal, Literal}, {Acc, Indent, Level, true})  ->
    {Acc ++ ",\n" ++ indent(Indent, Level) ++ atom_to_list(Literal), Indent, Level, true};
jsx_event({literal, Literal}, {Acc, Indent, Level, false})  ->
    {Acc ++ "\n" ++ indent(Indent, Level) ++ atom_to_list(Literal), Indent, Level, true};

jsx_event(end_object, {Acc, Indent, Level, true}) ->
    {Acc ++ "\n" ++ indent(Indent, Level - 1) ++ "}", Indent, Level - 1, true};
jsx_event(end_object, {Acc, Indent, Level, false}) ->
    {Acc ++ "}", Indent, Level - 1, true};
    
jsx_event(end_array, {Acc, Indent, Level, true}) ->
    {Acc ++ "\n" ++ indent(Indent, Level - 1) ++ "]", Indent, Level - 1, true};
jsx_event(end_array, {Acc, Indent, Level, false}) ->
    {Acc ++ "]", Indent, Level - 1, true};
    
jsx_event(eof, {Acc, _, _, _}) ->
    Acc.
    

indent(Indent, Level) ->
    [ 16#20 || _ <- lists:seq(1, Indent * Level) ].
    