jsx
===

*jsx* is an evented, streaming json tokenizer that can be used to build a variety of json tools. the api and a few example tools are described below.

currently it is limited to utf8, but this is temporary. it is probably buggy as hell, feel free to contribute test cases. it also needs to be specify'd properly, but that can wait until the api is stable. (it is currently sorta stable. there are no planned changes but...).


the api
-------

there's only one real function in jsx. decode/2. it takes a callback handler and some options and returns a function which can be applied to utf8 encoded binaries which produce events passed to the callback structure. in the case of premature end of input, it returns a new function which can be applied to future input to continue parsing. in the case of complete input (a complete json object, array or maybe a value if you pass it the {naked_values, true} option) it returns the result of the callback being applied consecutively to the events and any remaining unparsed input. there's a function supplied to ensure the remaining stream is json clean (ie, whitespace). there's also a default callback handler that can be applied to events (it's {none, []} and returns a list of events in the order they occured).

    jsx:decoder(Callbacks, options()) -> function() | {term(), binary()}
    
    [options()] :: {comments, true | false} 
        | {naked_values, true | false} 
        | {escaped_unicode, ascii | codepoint | none}
        | {explicit_termination, true | false}
        | []
        
Callbacks should be a tuple containing the function to be applied to events (either a tuple of {module, function} where function is an arity 2 function exported from module or an anonymous function of arity 2) and a term that will also be passed to the function along with the event. the callback can do whatever it wants with the events and term. events are described below.

the following are all atoms representing "{", "[", "}" and "]":

    start_object
    start_array
    end_object
    end_array

the following are json primitive values, list() is an erlang list of unicode codepoints representing the string/number (basically copied directly from the json string):

    {number, list()}
    {string, list()}
    {literal, true | false | null}
    
this is the end of input marker which will be emitted when parsing finishes (sucessfully)

    eof
    
    
typical usage examples follows:

    1> F = jsx:decoder().
    #Fun<jsx.0.69981956>
    2> F(<<"[ true, false, null, [], {}, \"hi there\", 1 ]">>).
    {[start_array,
      {literal,true},
      {literal,false},
      {literal,null},
      start_array,end_array,start_object,end_object,
      {string,"hi there"},
      {number,"1"},
      end_array],
     <<>>}
     
here is the result of the default parser applied to some boring json
     
     1> F = jsx:decoder({none, []}, []).
     #Fun<jsx.0.69981956>
     2> F(<<"[ true, false, null, [], {}, \"hi there\", 1 ]">>).
     {[start_array,
       {literal,true},
       {literal,false},
       {literal,null},
       start_array,end_array,start_object,end_object,
       {string,"hi there"},
       {number,"1"},
       end_array],
      <<>>}
     
and here it is when invoked with decode/2

    1> F = jsx:decoder({none, []}, []).                    
    #Fun<jsx.0.69981956>
    2> G = F(<<"{">>).                 
    #Fun<jsx_decoder.6.104510278>
    3> G(<<"}">>).
    {[start_object,end_object],<<>>}
    
here's an example of partial application

    1> F = jsx:decoder({none, []}, [{comments, true}]).        
    #Fun<jsx.0.69981956>
    2> F(<<"[ true /* this is a comment */ ]">>).              
    {[start_array,{literal,true},end_array],<<>>}

an example with a comment

    1> F = jsx:decoder({none, []}, [{naked_values, true}]).
    #Fun<jsx.0.69981956>
    2> G = F(<<"true">>).
    #Fun<jsx_decoder.3.63048828>
    3> G(<<>>).
    {[{literal,true}],<<>>}
    
parsing a naked value. note that naked values enable the explicit_termination option, because otherwise parsing numbers is ambiguous. explicit_termination, when true, will parse until end of input, then return a function that will either take more input, or take <<>> which will signal the parser to terminate. when explicit_termination is false (the default) the parser will terminate as soon as possible.
     

a simple decoder
----------------

*jsx_parser* (included in the examples directory) is a very primitive json decoder. it does no processing of strings, numbers or literals, returning the first two as erlang strings and the last as the corresponding atom. objects are stdlib dicts, arrays are lists and keys are strings. it is strict, allowing no comments, no naked values and only one occurence of each key per object. it swallows all errors, instead throwing a badarg exception should it fail parsing. it is a fairly complete example of how such a thing would be written however.

the api is extremely simple:

    jsx_parser:decode(binary()) -> dict() | list()
    
where the binary is a json string encoded in utf8. 


a pretty printer
----------------

*jsx_prettify* takes a json string (encoded as a utf8 binary) and returns a json string (encoded as an erlang list/string) correctly indented. it throws an error if passed invalid json.

the api:

    jsx_prettify:pretty(binary(), options()) -> list()
    
    [options()] :: {indentation, integer()} | []
    
the option is simply a number of spaces to indent per level. the default is 4.


a stream parser
---------------

*jsx_stream_parser* is an example of a stream parser. it scans the stream for the key "_id" and then captures and returns the value that follows if and only if it is a string. if passed a json array or an object without the key "_id" it terminates at the earliest possible opportunity. it uses exceptions for flow control, which is probably subpar, but works.

the api:

    jsx_stream_parser:decoder(options()) -> function(binary()) -> string() | not_found
    
    [options()] :: {comments, true | false} | []
    
the only relevant option is comments, controlling whether comments are allowed in the stream or not. you may pass any other option that would be valid for jsx:decode/2, but most will result in immediate failure of the parser, making them of questionable value.


jsx is &copy; 2010 Alisdair Sullivan (alisdairsullivan@yahoo.ca).

