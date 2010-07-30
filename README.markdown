jsx
===

basically [yajl][1], but in erlang. born from a need for a stream based, incremental parser capable of outputting a number of representations



overview
--------

jsx is a json parsing toolkit, to be used to convert json streams to arbitrary output representations. see:


an eep0018 (erlang extension proposal for json decoding) json decoder
    
    %% this is nearly complete, ignoring only the options json_to_term should accept
    
    %% [true,[{<<"key">>, <<"value">>}],1] = json_to_term(<<"[ true, { \"key\": \"value\" }, 1 ]">>).
    
    json_to_term(JSON) ->
        P = jsx:parser(),
        collect_start(P(JSON), [[]]).
    
	collect_start({event, Event, Next}, Acc) when Event =:= start_object; Event =:= start_array ->
	    collect(Next(), [[]|Acc]);
	collect_start(_, _) ->
	    erlang:error(badarg).
    
	collect({event, Event, Next}, Acc) when Event =:= start_object; Event =:= start_array ->
	    collect(Next(), [[]|Acc]);

	%% special case for empty object
	collect({event, end_object, Next}, [[], Parent|Rest]) ->
	    collect(Next(), [[[{}]] ++ Parent] ++ Rest);
	%% reverse the array/object accumulator before prepending it to it's parent
	collect({event, end_object, Next}, [Current, Parent|Rest]) when is_list(Parent) ->
	    collect(Next(), [[lists:reverse(Current)] ++ Parent] ++ Rest);
	collect({event, end_array, Next}, [Current, Parent|Rest]) when is_list(Parent) ->
	    collect(Next(), [[lists:reverse(Current)] ++ Parent] ++ Rest);
	collect({event, Start, Next}, [Current, Key, Parent|Rest])
	        when Start =:= end_object; Start =:= end_array ->
	    collect(Next(), [[{Key, lists:reverse(Current)}] ++ Parent] ++ Rest);
    
	%% end of json is emitted asap (at close of array/object), Next() should return {incomplete, More}
	%%   and then More(end_stream) ensures the tail of the json binary is clean (whitespace only)     
	collect({event, end_json, Next}, [[Acc]]) ->
	    case Next() of
	        {incomplete, More} -> case More(end_stream) of
	            ok -> Acc
	            ; _ -> erlang:error(badarg)
	        end
	        ; _ -> erlang:error(badarg)
	    end;
    
	%% key can only be emitted inside of a json object, so just insert it directly into
	%%   the head of the accumulator and deal with it when we receive it's paired value    
	collect({event, {key, _} = PreKey, Next}, [Current|_] = Acc) ->
	    Key = event(PreKey),
	    case key_repeats(Key, Current) of
	        true -> erlang:error(badarg)
	        ; false -> collect(Next(), [Key] ++ Acc)
	    end;
    
	%% check acc to see if we're inside an object or an array. because inside an object
	%%   context the events that fall this far are always preceded by a key (which are
	%%   binaries or atoms), if Current is a list, we're inside an array, else, an
	%%   object
	collect({event, Event, Next}, [Current|Rest]) when is_list(Current) ->
	    collect(Next(), [[event(Event)] ++ Current] ++ Rest);
	collect({event, Event, Next}, [Key, Current|Rest]) ->
	    collect(Next(), [[{Key, event(Event)}] ++ Current] ++ Rest);
    
	%% any other event is an error
	collect(_, _) -> erlang:error(badarg).
    
    
	event({string, String}) ->
	    unicode:characters_to_binary(String);
	event({key, Key}) ->
	    unicode:characters_to_binary(Key);
	%% special case for negative zero
	event({integer, "-0"}) ->
	    erlang:float(erlang:list_to_integer("-0"));
	event({integer, Integer}) ->
	    erlang:list_to_integer(Integer);
	event({float, Float}) ->
	    erlang:list_to_float(Float);
	event({literal, Literal}) ->
	    Literal.
    
    
	key_repeats(Key, [{Key, _Value}|_Rest]) -> true;
	key_repeats(Key, [_|Rest]) -> key_repeats(Key, Rest);
	key_repeats(_Key, []) -> false.



the twitter json API has a ton of detail in it's stream, most of which is ignorable. this parser extracts just the user screenname, the text and the time and throws the rest away
    
    %% {"talentdeficit", "use jsx!", "Fri Jul 30 04:25:50 +0000 2010"} = tweet_to_tuple(...).
    
    tweet_to_tuple(Tweet) ->
        P = jsx:parser(),
        scan(P(Tweet), {name, text, time}).
    
    scan({event, {key, "screen_name"}, Next}, Acc) ->
        collect_name(Next(), Acc);
    scan({event, {key, "created_at"}, Next}, Acc) ->
        collect_time(Next(), Acc);
    scan({event, {key, "text"}, Next}, Acc) ->
        collect_text(Next(), Acc);
    scan({event, end_json, Next}, Acc) ->
        Acc;
    scan({_, _, Next}, Acc) ->
        scan(Next(), Acc).
    
    collect_name({event, {string, Name}, Next}, {_, Text, Time}) ->
        scan(Next(), {Name, Text, Time}).
    collect_time({event, {string, Time}, Next}, {Name, Text, _}) ->
        scan(Next(), {Name, Text, Time}).
    collect_text({event, {string, Text}, Next}, {Name, _, Time}) ->
        scan(Next(), {Name, Text, Time}).



api
---

`jsx:parser/0` returns a function with arity 1. call it with a binary containing a properly encoded json stream and it returns one of the following values:

* `{event, Event, Next}`: Event is described below and Next is a zero arity function that returns the next value
* `{incomplete, More}`: More is described below
* `{error, badjson}`: the parser has decided the json stream is not a valid json document

`jsx:parser/1` has the same return signature as `jsx:parser/0` but accepts a proplist containing the following options:

* `{comments, true | false}`: determines whether (\/\* c style \*\/) comments are allowed. default is false
* `{escaped_unicode, ascii | codepoint | none}`: determines what escaped unicode sequences like `"\ua123"` are converted to in key/string events. ascii converts any sequences in the range 0-127 to their ascii value (`"\u0021"` would become 33), codepoint converts all valid sequences to their unicode codepoint value (`"\uabcd"` would become 43981), none does no conversion (`"\u0021"` would become the sequence 92, 117, 48, 48, 50, 49)
* `{encoding, utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} | auto}`: determines which encoding to expect the binary to use. a contrary encoding will result in an error. auto will auto detect the encoding. auto is the default
* `{multi_term, true | false}`: normally, after the end of a json document only whitespace is allowed, passing this option with true alters parsing so after the end of a json document, another json document is permitted, with any amount of whitespace in between. default is false

More is a new parser returned when parsing encounters the end of the stream supplied to the parser. calling it with a new stream resumes parsing as if the stream was not interrupted. because json documents may be followed by arbitrary whitespace, there is no unambiguous ending to a json stream. Next will always eventually return `{incomplete, More}`. to ensure the stream is clean and contains no garbage in the tail, call `More(end_stream)`. if `ok` is returned, parsing is complete. otherwise `{error, badjson}` will be returned



events
------

a complete list of events. Next is described under *api* above:

* `{event, start_object | end_object | start_array | end_array, Next}`: emitted when `{`, `}`, `[`, `]` are encountered by the parser in a legal position
* `{event, end_json, Next}`: emitted when the json document has been completed (the root object/array has been closed). note that this does NOT ensure the json document is valid, the tail must be checked to be free of invalid characters as described above under *api*
* `{event, {key, Key}, Next}`: an object key has been encountered. Key has the same format as String below
* `{event, {string, String}, Next}`: a string has been encountered. String is a list of unicode codepoints
* `{event, {integer, Integer}, Next}`: an integer had been encountered. Integer is a list of unicode codepoints that can be passed to `erlang:list_to_integer/1` to convert to an integer
* `{event, {float, Float}, Next}`: a float has been encountered. Float is a list of unicode codepoints that can be passed to `erlang:list_to_float/1` to convert to a float
* `{event, {literal, true | false | null}, Next}`: a json literal has been encountered. it will be the atom `true`, the atom `false` or the atom `null`


notes
-----

to compile and install, run `make && make install` from the root of the project directory

jsx supports utf8, utf16 (little and big endian) and utf32 (little and big endian). future support is planned for erlang iolists







[1]: http://github.com/lloyd/yajl
