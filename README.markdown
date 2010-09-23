jsx (v0.9.0)
============

jsx is an event based json parser. basically [yajl][1], but in erlang. born from a need for a stream based, incremental parser capable of outputting a number of representations. see [the wiki][2] for examples of what it can do

it also includes an implementation of [eep0018][3], a pretty printer, a verifier and a few stray functions to help you write your own json gizmos


### i just want to encode/decode json maaaaaan ###

`jsx:term\_to\_json` and `jsx:json\_to\_term` are what you want. see [the wiki][2] or [eep0018][3] for usage examples 


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


### installation ###

`make` to build jsx


### notes ###

don't edit the various jsx\_utfxx.erl files in the src dir directly, see comments in those files for why

jsx supports utf8, utf16 (little and big endian) and utf32 (little and big endian). future support is planned for erlang iolists (maybe)







[1]: http://lloyd.github.com/yajl
[2]: http://www.github.com/talentdeficit/jsx/wiki
[3]: http://www.erlang.org/eeps/eep-0018.html
