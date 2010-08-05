jsx
===

jsx is an event based json parser. basically [yajl][1], but in erlang. born from a need for a stream based, incremental parser capable of outputting a number of representations. see [the homepage][2] for examples of what it can do.


### usage ###

jsx provides an iterator based api that returns tuples of the form `{event, Event, Next}` where `Event` is an atom or tuple (see below) representing the json structure or value encountered. `Next` is a zero arity function that returns the next tuple in the sequence when called. it is stream based, and can also return the tuple `{incomplete, More}` to signify that input is exhausted. `More` is an arity one function that, when called with another binary, attempts to continue parsing treating the new binary as the tail of the preceding binary. errors in the json document are represented by the tuple `{error, badjson}`

    Parser = jsx:parser(),
    {event, start_array, A} = Parser(<<"[ true, 1, "hello world" ]">>),
    {event, {literal, true}, B} = A(),
    {event, {integer, "1"}, C} = B(),
    {event, {string, "hello world"}, D} = C(),
    {event, end_array, E} = D(),
    {event, end_json, F} = E(),
    {incomplete, More} = F().

jsx is stream based and allows the parsing of naked, unwrapped json values. together, this presents a problem with streams that contain numbers ie: "123". returning at end of input means clients need to be able to invalidate the `{integer, ...}` and `end_json` events and replace them in case of more input of the form "456", for example. instead, jsx doesn't return those events until an unambiguous end of value or input is reached. instead, `{incomplete, More}` will be returned. parsing can be explicitly terminated with `More(end_stream)` or by ending all naked numbers with whitespace. note that this is only a problem with json numbers not wrapped in a containing object or array and that calling `More(end_stream)` in any other context will result in an error 


### types ###

    unicode_codepoint() = 0..16#10ffff
    
    jsx_event() = start_object
	  | start_array
	  | end_object
	  | end_array
	  | end_json
	  | {key, [unicode_codepoint]}
	  | {string, [unicode_codepoint]}
	  | {integer, [unicode_codepoint]}
	  | {float, [unicode_codepoint]}
	
	jsx_result() = {error, badjson}
	  | {incomplete, fun((binary()) -> jsx_result())}
	  | {event, jsx_event(), fun(() -> jsx_result)}
	
	jsx_parser(binary()) -> jsx_result()


### functions ###

    parser() -> jsx_result() | {error, Reason}
    parser(Options) -> jsx_result() | {error, Reason}
        Options = [Opt]
            Opt -- see below
        Reason = badopt
        
        returns a function that takes a binary and attempts to parse it as an encoded 
        json document
        
        the available options are:
        
          {comments, true | false}
            if true, json documents that contain c style (/* ... */) comments
            will be parsed as if they did not contain any comments. default is
            false
            
          {encoded_unicode, ascii | codepoint | none}
            if a \uXXXX escape sequence is encountered within a key or string,
            this option controls how it is interpreted. none makes no attempt
            to interpret the value, leaving it unconverted. ascii will convert
            any value that falls within the ascii range. codepoint will convert
            any value that is a valid unicode codepoint. note that unicode
            non-characters (including badly formed surrogates) will never be
            converted. codepoint is the default

          {encoding, auto | utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
            attempt to parse the binary using the specified encoding. auto will
            auto detect any supported encoding and is the default

          {multi_term, true | false}
            usually, documents will be parsed in full before the end_json
            event is emitted. setting this option to true will instead emit
            the end_json event as soon as a valid document is parsed and then
            reset the parser to it's initial state and attempt to parse the
            remainder as a new json document. this allows streams containing
            multiple documents to be parsed correctly


### installation ###

`make` to build jsx
`make install` to install into `code:root_dir()`


### notes ###

don't edit the various jsx\_utfx.erl files in the src dir directly, see /priv/jsx\_decoder.erl for why

jsx supports utf8, utf16 (little and big endian) and utf32 (little and big endian). future support is planned for erlang iolists







[1]: http://lloyd.github.com/yajl
[2]: http://talentdeficit.github.com/jsx
