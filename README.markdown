# <a name="introduction">jsx (v1.2.1)</a> #

a sane [json][json] implementation for erlang, inspired by [yajl][yajl]

copyright 2011, 2012 alisdair sullivan

jsx is released under the terms of the [MIT][MIT] license

jsx uses [rebar][rebar] for it's build chain

[![Build Status](https://secure.travis-ci.org/talentdeficit/jsx.png?branch=master)](http://travis-ci.org/talentdeficit/jsx)


## index ##

* [introduction](#intro)
* [quickstart](#quickstart)
* [the api](#api)
  - [json <-> erlang mapping](#mapping)
  - [options](#options)
  - [incomplete input](#incompletes)
  - [the encoder and decoder](#core)
  - [handler callbacks](#handler)
  - [converting json to erlang and vice versa](#convert)
  - [formatting and minifying json text](#format)
  - [verifying json and terms are valid input](#verify)
* [acknowledgments](#thanks)



## <a name="quickstart">quickstart</a> ##

to build the library: `rebar compile`

to convert a utf8 binary containing a json string into an erlang term: `jsx:to_term(JSON)`

to convert an erlang term into a utf8 binary containing a json string: `jsx:to_json(Term)`

to check if a binary is valid json: `jsx:is_json(JSON)`

to check if a term is valid json: `jsx:is_term(Term)`

to minify a json string: `jsx:format(JSON)`


## <a name="api">api</a> ##


### <a name="mapping">json &lt;-> erlang mapping</a> ###

**json**                        | **erlang**
--------------------------------|--------------------------------
`number`                        | `integer()` and `float()`
`string`                        | `binary()`
`true`, `false` and `null`      | `true`, `false` and `null`
`array`                         | `[]` and `[JSON]`
`object`                        | `[{}]` and `[{binary() OR atom(), JSON}]`

#### json ####

json must be a binary encoded in `utf8`. if it's invalid `utf8` or invalid json, it probably won't parse without errors. there are a few non-standard extensions to the parser available that may change that, they are detailed in the options section below

jsx also supports json fragments; valid json values that are not complete json. that means jsx will parse things like `<<"1">>`, `<<"true">>` and `<<"\"hello world\"">>` without complaint

#### erlang ####

only the erlang terms in the table above are supported. non supported terms result in badarg errors. jsx is never going to support erlang lists instead of binaries, mostly because you can't discriminate between lists of integers and strings without hinting, and hinting is silly

#### numbers ####

javascript and thus json represent all numeric values with floats. as this is woefully insufficient for many uses, **jsx**, just like erlang, supports bigints. whenever possible, this library will interpret json numbers that look like integers as integers. other numbers will be converted to erlang's floating point type, which is nearly but not quite iee754. negative zero is not representable in erlang (zero is unsigned in erlang and `0` is equivalent to `-0`) and will be interpreted as regular zero. numbers not representable are beyond the concern of this implementation, and will result in parsing errors

when converting from erlang to json, numbers are represented with their shortest representation that will round trip without loss of precision. this means that some floats may be superficially dissimilar (although functionally equivalent). for example, `1.0000000000000001` will be represented by `1.0`

#### strings ####

the [json spec][rfc4627] is frustratingly vague on the exact details of json strings. json must be unicode, but no encoding is specified. javascript explicitly allows strings containing codepoints explicitly disallowed by unicode. json allows implementations to set limits on the content of strings and other implementations attempt to resolve this in various ways. this implementation, in default operation, only accepts strings that meet the constraints set out in the json spec (strings are sequences of unicode codepoints deliminated by `"` (`u+0022`) that may not contain control codes unless properly escaped with `\` (`u+005c`)) and that are encoded in `utf8`

the utf8 restriction means improperly paired surrogates are explicitly disallowed. `u+d800` to `u+dfff` are allowed, but only when they form valid surrogate pairs. surrogates that appear otherwise are an error

json string escapes of the form `\uXXXX` will be converted to their equivalent codepoint during parsing. this means control characters and other codepoints disallowed by the json spec may be encountered in resulting strings, but codepoints disallowed by the unicode spec (like the two cases above) will not be

in the interests of pragmatism, there is an option for looser parsing, see options below

all erlang strings are represented by *valid* `utf8` encoded binaries. the encoder will check strings for conformance. noncharacters (like `u+ffff`) are allowed in erlang utf8 encoded binaries, but not in strings passed to the encoder (although see options below)

this implementation performs no normalization on strings beyond that detailed here. be careful when comparing strings as equivalent strings may have different `utf8` encodings

#### true, false and null ####

the json primitives `true`, `false` and `null` are represented by the erlang atoms `true`, `false` and `null`. surprise

#### arrays ####

json arrays are represented with erlang lists of json values as described in this section

#### objects ####

json objects are represented by erlang proplists. the empty object has the special representation `[{}]` to differentiate it from the empty list. ambiguities like `[true, false]` prevent using the shorthand form of property lists using atoms as properties so all properties must be tuples. all keys must be encoded as in `string`, above, or as atoms (which will be escaped and converted to binaries for presentation to handlers). values should be valid json values


### <a name="options">options</a> ###

jsx functions all take a common set of options. not all flags have meaning in all contexts, but they are always valid options. flags are always atoms or {atom, Term} tuples. functions may have additional options beyond these, see individual function documentation for details

#### `replaced_bad_utf8` ####

json text input and json strings SHOULD be utf8 encoded binaries, appropriately escaped as per the json spec. if this option is present attempts are made to replace invalid codepoints with `u+FFFD` as per the unicode spec. this applies both to malformed unicode and disallowed codepoints

#### `escaped_forward_slashes` ####

json strings are escaped according to the json spec. this means forward slashes (solidus) are optionally escaped. this option is only relevant for encoding, you may want to use this if you are embedding JSON directly into a HTML or XML document. See: [html4-non-html-data]

#### `single_quoted_strings` ####
 
some parsers allow double quotes (`u+0022`) to be replaced by single quotes (`u+0027`) to deliminate keys and strings. this option allows json containing single quotes as structural (deliminator) characters to be parsed without errors. note that the parser expects strings to be terminated by the same quote type that opened it and that single quotes must, obviously, be escaped within strings deliminated by single quotes

double quotes must ALWAYS be escaped, regardless of what kind of quotes deliminate the string they are found in

the parser will never emit json with keys or strings deliminated by single quotes

#### `unescaped_jsonp` ####

javascript interpreters treat the codepoints `u+2028` and `u+2029` as significant whitespace. json strings that contain either of these codepoints will be parsed incorrectly by some javascript interpreters. by default, these codepoints are escaped (to `\u2028` and `\u2029`, respectively) to retain compatibility. this option simply removes that escaping if, for some reason, you object to this

#### `comments` ####

json has no official comments but some parsers allow c style comments. this flag allows comments (both `// ...` and `/* ... */` style) anywhere whitespace is allowed

#### `escaped_strings` ####

by default, both the encoder and decoder return strings as utf8 binaries appropriate for use in erlang. escape sequences that were present in decoded terms are converted into the appropriate codepoint and encoded terms are unaltered. this flag escapes strings as if for output in json, removing control codes and problematic codepoints and replacing them with the appropriate escapes

#### `dirty_strings` ####

json escaping is lossy, it mutates the json string and repeated application can result in unwanted behaviour. if your strings are already escaped (or you'd like to force invalid strings into "json") use this flag to bypass escaping

#### `ignored_bad_escapes` ####

during decoding, ignore unrecognized escape sequences and leave them as is in the stream. note that if you combine this option with `escaped_strings` the escape character itself will be escaped

#### `explicit_end` ####

this option treats all exhausted inputs as incomplete, as explained below. the parser will not attempt to return a final state until the function is called with the value `end_stream`

#### `relax` ####

relax is a synonym for `[replaced_bad_utf8, single_quoted_strings, comments, ignored_bad_escapes]` for when you don't care how janky and awful your json input is, you just want the parser to do the best it can

#### `{pre_encode, F}` ####

`F` is a function of arity 1 that pre-process input to the encoder. only input evaluated in a *value* context is pre-processed in this manner (so keys are not pre-processed, but objects and arrays are). if more than one pre encoder is declared, a `badarg` exception will occur

input can be any term, but output from the function must be a valid type for input


### <a name="incompletes">incomplete input</a> ###

jsx handles incomplete json texts. if a partial json text is parsed, rather than returning a term from your callback handler, jsx returns `{incomplete, F}` where `F` is a function with an identical API to the anonymous fun returned from `decoder/3`. it retains the internal state of the parser at the point where input was exhausted. this allows you to parse as you stream json over a socket or file descriptor or to parse large json texts without needing to keep them entirely in memory

however, it is important to recognize that jsx is greedy by default. if input is exhausted and the json text is not unambiguously incomplete jsx will consider the parsing complete. this is mostly relevant when parsing bare numbers like `<<"1234">>`. this could be a complete json integer or just the beginning of a json integer that is being parsed incrementally. jsx will treat it as a whole integer. the option `explicit_end` can be used to modify this behaviour, see above


### <a name="core">the encoder and decoder</a> ###

jsx is built on top of two finite state automata, one that handles json texts and one that handles erlang terms. both take a callback module as an argument that acts similar to a fold over a list of json 'events'. these events and the handler module's callbacks are detailed in the next section

`jsx:decoder/3` and `jsx:encoder/3` are the entry points for the decoder and encoder, respectively

`decoder(Handler, InitialState, Opts)` -> `Fun((JSON) -> Any)`

`encoder(Handler, InitialState, Opts)` -> `Fun((Term) -> Any)`

types:

- `Handler` = `atom()`, should be the name of a callback module, see below
- `InitialState` = `term()`, passed as is to `Handler:init/1`
- `Opts` = see above
- `JSON` = `utf8` encoded json text
- `Term` = an erlang term as specified above in the mapping section
- `Any` = `term()`

decoder returns an anonymous function that handles binary json input and encoder returns an anonymous function that handles erlang term input. these are safe to reuse for multiple inputs


### <a name="handler">handler callbacks</a> ###

`Handler` should export the following pair of functions

`Handler:init(InitialState)` -> `State`

`Handler:handle_event(Event, State)` -> `NewState`

types:

- `InitialState`, `State`, `NewState` = any erlang term
- `Event` =
  * `start_object`
  * `end_object`
  * `start_array`
  * `end_array`
  * `end_json`
  * `{key, binary()}`
  * `{string, binary()}`
  * `{integer, integer()}`
  * `{float, float()}`
  * `{literal, true}`
  * `{literal, false}`
  * `{literal, null}`

`init/1` is called with the `initialState` argument from `decoder/3` or `encoder/3` and should take care of any initialization your handler requires and return a new state

`handle_event/2` is called for each `Event` emitted by the decoder/encoder with the output of the previous `handle_event/2` call (or `init/1` call, if `handle_event/2` has not yet been called)

the event `end_json` will always be the last event emitted, you should take care of any cleanup in `handle_event/2` when encountering `end_json`. the state returned from this call will be returned as the final result of the anonymous function

both `key` and `string` are `utf8` encoded binaries with all escaped values converted into the appropriate codepoints


### <a name="convert">converting json to erlang and vice versa</a> ###

#### converting json to erlang terms ####

`to_term` parses a JSON text (a utf8 encoded binary) and produces an erlang term (see json <-> erlang mapping details above)

`to_term(JSON)` -> `Term`

`to_term(JSON, Opts)` -> `Term`

types:

* `JSON` = as above in the mapping section
* `Term` = as above in the mapping section
* `Opts` = as above in the opts section, but see also additional opts below
* `Opt` =
    - `labels`
    - `{labels, Label}`
    - `Label` =
        * `binary`
        * `atom`
        * `existing_atom`
    - `{post_decode, F}`

the option `labels` controls how keys are converted from json to erlang terms. `binary` does no conversion beyond normal escaping. `atom` converts keys to erlang atoms, and results in a badarg error if keys fall outside the range of erlang atoms. `existing_atom` is identical to `atom`, except it will not add new atoms to the atom table

`{post_decode, F}` is a user defined function of arity 1 that is called on each output value (objects, arrays, strings, numbers and literals). it may return any value to be substituted in the returned term. for example:

```erlang
    1> F = fun(V) when is_list(V) -> V; (V) -> false end.
    2> jsx:to_term(<<"{\"a list\": [true, \"a string\", 1]}">>, [{post_decode, F}]).
      [{<<"a list">>, [false, false, false]}]
```

if more than one decoder is declared a badarg exception will result

#### converting erlang terms to json ####
    
`to_json` parses an erlang term and produces a JSON text (see json <-> erlang mapping details below)

`to_json(Term)` -> `JSON`

`to_json(Term, Opts)` -> `JSON`

types:
        
* `JSON` = as above in the mapping section
* `Term` = as above in the mapping section
* `Opts` = as above in the opts section, but see also additional opts below
* `Opt` =
    - `space`
    - `{space, N}`
    - `indent`
    - `{indent, N}`

the option `{space, N}` inserts `N` spaces after every comma and colon in your json output. `space` is an alias for `{space, 1}`. the default is `{space, 0}`

the option `{indent, N}` inserts a newline and `N` spaces for each level of indentation in your json output. note that this overrides spaces inserted after a comma. `indent` is an alias for `{indent, 1}`. the default is `{indent, 0}`


### <a name="format">formatting and minifying json text</a> ###

#### formatting json texts ####
    
produces a JSON text from JSON text, reformatted

`format(JSON)` -> `JSON`

`format(JSON, Opts)` -> `JSON`

types:

* `JSON` = as above in the mapping section
* `Opts` = as above in the opts section, but see also additional opts below
* `Opt` =
    - `space`
    - `{space, N}`
    - `indent`
    - `{indent, N}`

the option `{space, N}` inserts `N` spaces after every comma and colon in your json output. `space` is an alias for `{space, 1}`. the default is `{space, 0}`

the option `{indent, N}` inserts a newline and `N` spaces for each level of indentation in your json output. note that this overrides spaces inserted after a comma. `indent` is an alias for `{indent, 1}`. the default is `{indent, 0}`

calling `format` with no options results in minified json text


### <a name="verify">verifying json and terms are valid input</a> ###

#### verifying json texts ####
    
returns true if input is a valid JSON text, false if not

`is_json(MaybeJSON)` -> `true` | `false` | `{incomplete, Fun}`

`is_json(MaybeJSON, Opts)` -> `true` | `false` | `{incomplete, Fun}`

types:

* `MaybeJSON` = `any()`
* `Opts` = as above


#### verifying terms ####
    
returns true if input is a valid erlang term that represents a JSON text, false if not

`is_term(MaybeJSON)` -> `true` | `false`

`is_term(MaybeJSON, Opts)` -> `true` | `false`

types:

* `MaybeJSON` = `any()`
* `Opts` = as above


## <a name="thanks">acknowledgements</a> ##

jsx wouldn't be what it is without the contributions of paul davis, lloyd hilaiel, john engelhart, bob ippolito, fernando benavides, alex kropivny, steve strong, michael truog and dmitry kolesnikov

[json]: http://json.org
[yajl]: http://lloyd.github.com/yajl
[MIT]: http://www.opensource.org/licenses/mit-license.html
[rebar]: https://github.com/basho/rebar
[meck]: https://github.com/eproxus/meck
[rfc4627]: http://tools.ietf.org/html/rfc4627[html4-non-html-data]: http://www.w3.org/TR/html4/appendix/notes.html#h-B.3.2
