# jsx: a streaming, event driven json parser library #


## why another json parser written in erlang? ##

none of the existing parsers support incremental parsing of json streams. none of the existing parsers are output representation neutral. none of the existing parsers are as fast as they could be.


## how do I use it? ##

first, install it:

    chmod u+x rebar
    ./rebar compile
    ./rebar install
    
next, start up an interactive session:

    1> F = jsx:decoder().
    #Fun<jsx.2.95981374>
    2> F(<<"[ \"some json\" ]">>).
    {[start_array,{string,"some json"},end_array],
     #Fun<jsx_utf8.3.21708423>}
    
that's it!


## that doesn't seem like a very friendly (or neutral...) output representation ##

that's not meant for human consumption, or even use in your programs. it's a list of json events output by the parser that can be processed by a callback module to do whatever you want. there's a simple parser in examples you can check out to see what I mean:

    1. jsx_parser:decode(<<"[ \"some json\" ]">>).
    {ok,["some json"]}
    
jsx_parser converts the events into a list containing a list of unicode codepoints (that conveniently is identical to what erlang considers a string). another example:

    2. {ok, Dict} = jsx_parser:decode(<<"{ \"key\": 42 }">>).
    {ok,{dict,1,16,16,8,80,48,
              {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
              {{[],[],[],[],[],[],[],[],[],[],[],[],
                [["key"|42]],
                [],[],[]}}}}
    3. dict:fetch("key", Dict).
    42
    
the code that builds this representation from the list of events is less than 55 lines, not including comments. here's the fun that builds the default representation:

    fun(end_of_stream, State) -> lists:reverse(State) ;(Event, State) -> [Event] ++ State end
    
that just collects the events into a list and returns them when finished. that's not all jsx can do:

    4. Pretty = jsx_prettify:pretty(<<"[ \"life\", \"the universe\", \"everything\", { \"answer\": 42 } ]">>, []).
    "[\n    \"life\",\n    \"the universe\",\n    \"everything\",\n    {\n        \"answer\": 42\n    }\n]"
    5. io:format("~s~n", [Pretty]).
    [                     
        "life",
        "the universe",
        "everything",
        {
            "answer": 42
        }
    ]
    

## the API ##

there are three functions exported from jsx.erl (well, four, but don't use detect_encoding/4 directly):

1. jsx:decoder/0
2. jsx:decoder/1
3. jsx:decoder/2

decoder/0 returns the default decoder (the one that just returns a list of events) while decoder/1 returns a decoder with the given options and the default callback handler. decoder/2 is the interesting one. it takes a callback handler and a proplist of options as specified below.

return values are {Value, F}, {error, Error} or {incomplete, F}. see 'incremental parsing' below for an explanation of F. the only error currently returned is badjson, representing a failure to parse the json. other errors are unintentional, but may currently be encountered.


## callback handlers ##

callback handlers are essentially a fold over the list of events. a function is called with the json event and an arbitrary term and a new term is returned to be passed to the function along with the next event. they're passed to decoder/2 either as {F, Term} where F is an anonymous function of arity 2 or {Module, Function, Term} where Module is a module that exports Function. Here's an example using the default callback function seen above:

    F = jsx:decoder({fun(end_of_stream, State) -> lists:reverse(State) ;(Event, State) -> [Event] ++ State end, []}, [])
    
and an example from jsx_parser:

    F = jsx:decoder({jsx_parser, event, []}, [])
    

## options ##

the second, mystery, argument seen above is a proplist of options used to configure the parser. currently there are only three possible options:

#### comments ####

possible values are true or false, the default is false. this allows c style comments in the json input stream in between any pair of events.

#### escaped_unicode ####

possible values are ascii, unicode or none. default is codepoint. this option handles how json unicode escapes are handled. you know, those \uXXXX things. none doesn't do anything with them, leaving them in the string intact. ascii will convert any that represent ascii codepoints (including control characters) into their corresponding codepoint and treats all other escapes as none. codepoint will convert any that represent any unicode codepoint into it's corresponding codepoint and leave unicode non-characters as strings. because json is silly, it will also attempt to convert utf16 surrogates encoded as json escapes into the correct codepoints. codepoint codepoint codepoint.

#### encoding ####

possible values are utf8, utf16, {utf16,little}, utf32, {utf32,little} and auto. default is auto. this just forces the parser to attempt to interpret the json as the chosen encoding. auto will autodetect the encoding.


## events ##

the things the parser emits to your callback function.

    start_array, start_object, end_array, end_object
    
these denote [, {, ] and } respectively.

    {key, [Codepoints]}
    {string, [Codepoints]}
    {integer, [Codepoints]}
    {float, [Codepoints]}
    
keys, strings, integers and floats are all returned as lists of unicode codepoints. if your json input is ascii, these are identical to erlang strings. otherwise, they can have non-printable characters in them and should be handled with the unicode module in the stdlib. if your input is utf8, occaisonally you can get lists which appear to be valid latin-1, this is just coincidence. 

floats and integers are always ascii printable, and should always be passable to erlang:list\_to\_float and erlang:list\_to\_integer, respectively. 

    {literal, true}
    {literal, false}
    {literal, null}
    
atoms representing the json boolean values. (and null, javascript is terrible).

    end_of_stream
    
the json stream has been successfully parsed completely.


## incremental parsing ##

jsx is a stream parser. it can parse partial json input:

    6. F = jsx:decoder().
    #Fun<jsx.2.95981374>
    7. {_, G} = F(<<"[ tr">>).
    {incomplete,#Fun<jsx_utf8.35.98972787>}
    8. {_, H} = G(<<"ue ]">>).
    {[start_array,{literal,true},end_array],
     #Fun<jsx_utf8.3.21708423>}
    
even upon returning a value, the parser still returns a new decoder that can be called on subsequent input in the stream:

    9. H(<<"   ">>).
    {[start_array,{literal,true},end_array],
     #Fun<jsx_utf8.3.21708423>}
    
this can be used to make sure the tails of json input are clean or when parsing naked json numbers:

    10. {_, I} = F(<<"1">>).
    {[{integer,"1"}],#Fun<jsx.3.78701932>}
    11. {_, J} = I(<<"2">>).
    {[{integer,"12"}],#Fun<jsx_utf8.23.42619789>}
    12. J(<<"3">>).
    {[{integer,"123"}],#Fun<jsx_utf8.23.3431536}
    

## notes ##

yes, jsx parses naked (unwrapped) json values. that doesn't mean you need to accept naked json values as valid json. just make sure any value is preceded, at some point, but either start\_array or start\_object.


    
    

    
    