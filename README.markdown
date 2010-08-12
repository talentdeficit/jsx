jsx
===

jsx is an event based json parser. basically [yajl][1], but in erlang. born from a need for a stream based, incremental parser capable of outputting a number of representations. see [the homepage][2] for examples of what it can do.

it also includes an implementation of [eep0018][3], a pretty printer, a verifier and a few stray functions to help you write your own json gizmos.


### usage ###

jsx provides an iterator based api that returns tuples of the form `{event, Event, Next}` where `Event` is an atom or tuple (see below) representing the json structure or value encountered. `Next` is a zero arity function that returns the next tuple in the sequence when called. it is stream based, and can also return the tuple `{incomplete, More}` to signify that input is exhausted. `More` is an arity one function that, when called with another binary, attempts to continue parsing treating the new binary as the tail of the preceding binary. errors in the json document are represented by the tuple `{error, badjson}`

the following module

    -module(jsx_ex).

    -export([simple_decode/1]).

    simple_decode(JSON) when is_binary(JSON) ->
        P = jsx:parser(),
        decode(P(JSON), []).

    decode({event, end_json, _Next}, Acc) -> 
        lists:reverse(Acc);    
    decode({event, Event, Next}, Acc) -> 
        decode(Next(), [Event] ++ Acc).
        
produces the following output:

    1> jsx_ex:simple_decode(
        <<"{
            \"dead rock stars\": [\"kurt cobain\", \"elliott smith\", \"nicky wire\"], 
            \"total\": 3.0, 
            \"courtney killed kurt\": true
        }">>).
    [start_object,
     {key,"dead rock stars"},
     start_array,
     {string,"kurt cobain"},
     {string,"elliott smith"},
     {string,"nicky wire"},
     end_array,
     {key,"total"},
     {float,"3.0"},
     {key,"courtney killed kurt"},
     {literal,true},
     end_object]

    

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
	
	
	eep0018() = eep0018_object() | eep0018_array()

    eep0018_array() = [eep0018_term()]
    eep0018_object() = [{eep0018_key(), eep0018_term()}]

    eep0018_key() = binary() | atom()

    eep0018_term() = eep0018_array() 
      | eep0018_object() 
      | eep0018_string() 
      | eep0018_number() 
      | true 
      | false 
      | null

    eep0018_string() = binary()

    eep0018_number() = float() | integer()


### functions ###

    parser() -> jsx_result() | {error, Reason}
    parser(Options) -> jsx_result() | {error, Reason}
        Options = [Opt]
            Opt -- see below
        Reason = badopt
        
        returns a function that takes a binary and attempts to parse it as an encoded 
        json document
        
        options:
        
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
            
            
    term_to_json(eep0018()) -> binary()
    term_to_json(eep0018(), Options) -> binary()
        Options = [Opt]
            Opt -- see below
        
        takes the erlang representation of a json object (as defined in eep0018) and
        returns a (binary encoded) json string
        
        options:
          
          {strict, true | false}
            by default, attempting to convert unwrapped json values (numbers, 
            strings and the atoms true, false and null) result in a badarg exception. 
            if strict equals false, these are instead json encoded. default is false
            
          {encoding, utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
            the encoding of the resulting binary. default is utf8
            
          space
          {space, N}
            place N spaces after each colon and comma in the resulting binary. space 
            implies {space, 1}. default is zero
            
          indent 
          {indent, N}
            indent each 'level' of the json structure by N spaces. indent implies 
            {indent, 1}. default is zero
            
        
    json_to_term(binary()) -> eep0018()
    json_to_term(binary(), Options) -> eep0018()
        Options = [Opt]
            Opt -- see below
            
        takes a binary and attempts to decode it into the eep0018 representation
        
        options:
        
          {strict, true | false}
            by default, attempting to convert unwrapped json values (numbers, strings and
            the atoms true, false and null) result in a badarg exception. if strict equals
            false, these are instead decoded to their equivalent eep0018 value. default is
            false
            
          {encoding, auto | utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
            assume the binary is encoded using the specified binary. default is auto, which
            attempts to autodetect the encoding
            
          {comments, true | false}
            if true, json documents that contain c style (/* ... */) comments
            will be parsed as if they did not contain any comments. default is
            false
            
          {label, atom | existing_atom | binary}
            json keys (labels) are decoded to utf8 encoded binaries, atoms or 
            existing_atoms (atom if it exists, binary otherwise) as specified by 
            this option. default is binary
            
          {float, true | false}
            return all numbers as floats. default is false
            
    is_json(binary()) -> true | false
    is_json(binary(), Options) -> true | false
        Options = [Opt]
            Opt -- see below
            
        returns true if the binary is an encoded json string, false otherwise
        
        options:
        
          {strict, true | false}
            by default,  unwrapped json values (numbers, strings and the atoms 
            true, false and null) return false. if strict equals true, is_json
            returns true. default is false
          
          {encoding, auto | utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
            assume the binary is encoded using the specified binary. default is auto, 
            which attempts to autodetect the encoding
          
          {comments, true | false}
            if true, json documents that contain c style (/* ... */) comments
            will be parsed as if they did not contain any comments. default is
            false
            
    format(binary()) -> binary()
    format(binary(), Options) -> binary()
        Options = [Opt]
            Opt -- see below
            
        formats a binary encoded json string according to the options chose. the
        defaults will produced a string stripped of all whitespace
        
        options:
        
          {strict, true | false}
            by default,  unwrapped json values (numbers, strings and the atoms 
            true, false and null) result in an error. if strict equals true, they
            are treated as valid json. default is false
          
          {encoding, auto | utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
            assume the binary is encoded using the specified binary. default is auto, 
            which attempts to autodetect the encoding
            
          {output_encoding, utf8 | utf16 | {utf16, little} | utf32 | {utf32, little} }
            the encoding of the resulting binary. default is utf8
            
          {comments, true | false}
            if true, json documents that contain c style (/* ... */) comments
            will be parsed as if they did not contain any comments. default is
            false 
            
          space
          {space, N}
            place N spaces after each colon and comma in the resulting binary. space 
            implies {space, 1}. default is zero

          indent 
          {indent, N}
            indent each 'level' of the json structure by N spaces. indent implies 
            {indent, 1}. default is zero
            
            
          


### installation ###

`make` to build jsx
`make install` to install into `code:root_dir()`


### notes ###

don't edit the various jsx\_utfx.erl files in the src dir directly, see /priv/jsx\_decoder.erl for why

jsx supports utf8, utf16 (little and big endian) and utf32 (little and big endian). future support is planned for erlang iolists







[1]: http://lloyd.github.com/yajl
[2]: http://talentdeficit.github.com/jsx
[3]: http://eep0018
