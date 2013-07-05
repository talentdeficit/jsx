# jsx (v1.4.2) #

an erlang application for consuming, producing and manipulating [json][json]. 
inspired by [yajl][yajl]

jsx is built via [rebar][rebar] and continuous integration testing provided courtesy [travis][travis]

current status: [![Build Status](https://secure.travis-ci.org/talentdeficit/jsx.png?branch=develop)](http://travis-ci.org/talentdeficit/jsx)

jsx is released under the terms of the [MIT][MIT] license

copyright 2010-2013 alisdair sullivan

## index ##

* [quickstart](#quickstart)
* [description](#description)
  - [json <-> erlang mapping](#json---erlang-mapping)
  - [incomplete input](#incomplete-input)
* [data types](#data-types)
  - [`json_term()`](#json_term)
  - [`json_text()`](#json_text)
  - [`event()`](#event)
  - [`token()`](#token)
  - [`option()`](#option)
* [exports](#exports)
  - [`encoder/3`, `decoder/3` & `parser/3`](#encoder3-decoder3--parser3)
  - [`decode/1,2`](#decode12)
  - [`encode/1,2`](#encode12)
  - [`format/1,2`](#format12)
  - [`minify/1`](#minify1)
  - [`prettify/1`](#prettify1)
  - [`is_json/1,2`](#is_json12)
  - [`is_term/1,2`](#is_term12)
* [callback exports](#callback_exports)
  - [`Module:init/1`](#moduleinit1)
  - [`Module:handle_event/2`](#modulehandle_event2)
* [acknowledgements](#acknowledgements)


## quickstart ##

#### to build the library and run tests ####

```bash
$ rebar compile
$ rebar eunit
```
or, to build using hipe
```bash
$ rebar -C hipe.cfg compile
$ rebar -C hipe.cfg eunit
```

#### to convert a utf8 binary containing a json string into an erlang term ####

```erlang
1> jsx:decode(<<"{\"library\": \"jsx\", \"awesome\": true}">>).
[{<<"library">>,<<"jsx">>},{<<"awesome">>,true}]
2> jsx:decode(<<"[\"a\",\"list\",\"of\",\"words\"]">>).
[<<"a">>, <<"list">>, <<"of">>, <<"words">>]
```

#### to convert an erlang term into a utf8 binary containing a json string ####

```erlang
1> jsx:encode([{<<"library">>,<<"jsx">>},{<<"awesome">>,true}]).
<<"{\"library\": \"jsx\", \"awesome\": true}">>
2> jsx:encode([<<"a">>, <<"list">>, <<"of">>, <<"words">>]).
<<"[\"a\",\"list\",\"of\",\"words\"]">>
```

#### to check if a binary or a term is valid json ####

```erlang
1> jsx:is_json(<<"[\"this is json\"]">>).
true
2> jsx:is_json("[\"this is not\"]").
false
3> jsx:is_term([<<"this is a term">>]).
true
4> jsx:is_term([this, is, not]).
false
```

#### to minify some json ####

```erlang
1> jsx:minify(<<"{
  \"a list\": [
    1,
    2,
    3
  ]
}">>).
<<"{\"a list\":[1,2,3]}">>
```

#### to prettify some json ####

```erlang
1> jsx:prettify(<<"{\"a list\":[1,2,3]}">>).
<<"{
  \"a list\": [
    1,
    2,
    3
  ]
}">>
```


## description ##


jsx is an erlang application for consuming, producing and manipulating 
[json][json]

json has a [spec][rfc4627] but common usage differs subtly. it's common 
usage jsx attempts to address, with guidance from the spec

all json produced and consumed by jsx should be `utf8` encoded text or a 
reasonable approximation thereof. ascii works too, but anything beyond that 
i'm not going to make any promises. **especially** not latin1

the [spec][rfc4627] thinks json values must be wrapped in a json array or 
object but everyone else disagrees so jsx allows naked json values by default. 
if you're a curmudgeon who's offended by this deviation here is a wrapper for 
you:

```erlang
%% usage: `real_json(jsx:decode(JSON))`
real_json(Result) when is_list(Result) -> Result;
real_json(Result) when is_tuple(Result, 2) -> Result;
real_json(_) -> erlang:error(badarg).
```

here is a table of how various json values map to erlang:

### json &lt;-> erlang mapping ###

**json**                        | **erlang**
--------------------------------|--------------------------------
`number`                        | `integer()` and `float()`
`string`                        | `binary()`
`true`, `false` and `null`      | `true`, `false` and `null`
`array`                         | `[]` and `[JSON]`
`object`                        | `[{}]` and `[{binary() OR atom(), JSON}]`

*   numbers

    javascript and thus json represent all numeric values with floats. as 
    this is woefully insufficient for many uses, **jsx**, just like erlang, 
    supports bigints. whenever possible, this library will interpret json 
    numbers that look like integers as integers. other numbers will be converted 
    to erlang's floating point type, which is nearly but not quite iee754. 
    negative zero is not representable in erlang (zero is unsigned in erlang and 
    `0` is equivalent to `-0`) and will be interpreted as regular zero. numbers 
    not representable are beyond the concern of this implementation, and will 
    result in parsing errors

    when converting from erlang to json, numbers are represented with their 
    shortest representation that will round trip without loss of precision. this 
    means that some floats may be superficially dissimilar (although 
    functionally equivalent). for example, `1.0000000000000001` will be 
    represented by `1.0`

*   strings

    the json [spec][rfc4627] is frustratingly vague on the exact details of json 
    strings. json must be unicode, but no encoding is specified. javascript 
    explicitly allows strings containing codepoints explicitly disallowed by 
    unicode. json allows implementations to set limits on the content of 
    strings. other implementations attempt to resolve this in various ways. this 
    implementation, in default operation, only accepts strings that meet the 
    constraints set out in the json spec (strings are sequences of unicode 
    codepoints deliminated by `"` (`u+0022`) that may not contain control codes 
    unless properly escaped with `\` (`u+005c`)) and that are encoded in `utf8`

    the utf8 restriction means improperly paired surrogates are explicitly 
    disallowed. `u+d800` to `u+dfff` are allowed, but only when they form valid 
    surrogate pairs. surrogates encountered otherwise result in errors

    json string escapes of the form `\uXXXX` will be converted to their 
    equivalent codepoints during parsing. this means control characters and 
    other codepoints disallowed by the json spec may be encountered in resulting 
    strings, but codepoints disallowed by the unicode spec will not be. in the 
    interest of pragmatism there is an [option](#option) for looser parsing

    all erlang strings are represented by **valid** `utf8` encoded binaries. the 
    encoder will check strings for conformance. noncharacters (like `u+ffff`) 
    are allowed in erlang utf8 encoded binaries, but not in strings passed to 
    the encoder (although, again, see [options](#option))

    this implementation performs no normalization on strings beyond that 
    detailed here. be careful when comparing strings as equivalent strings 
    may have different `utf8` encodings

*   true, false and null

    the json primitives `true`, `false` and `null` are represented by the 
    erlang atoms `true`, `false` and `null`. surprise

*   arrays

    json arrays are represented with erlang lists of json values as described 
    in this section

*   objects

    json objects are represented by erlang proplists. the empty object has the 
    special representation `[{}]` to differentiate it from the empty list. 
    ambiguities like `[true, false]` prevent the use of the shorthand form of 
    property lists using atoms as properties so all properties must be tuples. 
    all keys must be encoded as in `string` or as atoms (which will be escaped 
    and converted to binaries for presentation to handlers). values should be 
    valid json values


### incomplete input ###

jsx handles incomplete json texts. if a partial json text is parsed, rather than 
returning a term from your callback handler, jsx returns `{incomplete, F}` where 
`F` is a function with an identical API to the anonymous fun returned from 
`decoder/3`, `encoder/3` or `parser/3`. it retains the internal state of the 
parser at the point where input was exhausted. this allows you to parse as you 
stream json over a socket or file descriptor, or to parse large json texts 
without needing to keep them entirely in memory

however, it is important to recognize that jsx is greedy by default. jsx will 
consider the parsing complete if input is exhausted and the json text is not 
unambiguously incomplete. this is mostly relevant when parsing bare numbers like 
`<<"1234">>`. this could be a complete json integer or just the beginning of a 
json integer that is being parsed incrementally. jsx will treat it as a whole 
integer. calling jsx with the [option](#options) `explicit_end` reverses this 
behavior and never considers parsing complete until the `incomplete` function is 
called with the argument `end_stream`


## data types ##

#### `json_term()` ####

```erlang
json_term() = [json_term()]
    | [{binary() | atom(), json_term()}]
    | true
    | false
    | null
    | integer()
    | float()
    | binary()
```

the erlang representation of json. binaries should be `utf8` encoded, or close 
at least

#### `json_text()` ####

```erlang
json_text() = binary()
```

a utf8 encoded binary containing a json string

#### `event()` ####

```erlang
event() = start_object
    | end_object
    | start_array
    | end_array
    | {key, binary()}
    | {string, binary()}
    | {integer, integer()}
    | {float, float()}
    | {literal, true}
    | {literal, false}
    | {literal, null}
    | end_json
```

#### `token()` ####

```erlang
token() = event()
    | binary()
    | {number, integer() | float()}
    | integer()
    | float()
    | true
    | false
    | null
```

the representation used during syntactic analysis. you can generate this 
yourself and feed it to `jsx:parser/3` if you'd like to define your own 
representations

#### `option()` ####

```erlang
option() = replaced_bad_utf8
    | escaped_forward_slashes
    | single_quoted_strings
    | unescaped_jsonp
    | comments
    | escaped_strings
    | dirty_strings
    | ignored_bad_escapes
    | relax
    | explicit_end
```

jsx functions all take a common set of options. not all flags have meaning 
in all contexts, but they are always valid options. functions may have 
additional options beyond these. see 
[individual function documentation](#exports) for details

- `replaced_bad_utf8`

    json text input and json strings SHOULD be utf8 encoded binaries, 
    appropriately escaped as per the json spec. attempts are made to replace 
    invalid codepoints with `u+FFFD` as per the unicode spec when this option is 
    present. this applies both to malformed unicode and disallowed codepoints

- `escaped_forward_slashes`

    json strings are escaped according to the json spec. this means forward 
    slashes (solidus) are only escaped when this flag is present. otherwise they 
    are left unescaped. you may want to use this if you are embedding json 
    directly into a html or xml document

- `single_quoted_strings`

    some parsers allow double quotes (`u+0022`) to be replaced by single quotes 
    (`u+0027`) to delimit keys and strings. this option allows json containing 
    single quotes as structural characters to be parsed without errors. note 
    that the parser expects strings to be terminated by the same quote type that 
    opened it and that single quotes must, obviously, be escaped within strings 
    delimited by single quotes

    double quotes must **always** be escaped, regardless of what kind of quotes 
    delimit the string they are found in

    the parser will never emit json with keys or strings delimited by single 
    quotes

- `unescaped_jsonp`

    javascript interpreters treat the codepoints `u+2028` and `u+2029` as 
    significant whitespace. json strings that contain either of these codepoints 
    will be parsed incorrectly by some javascript interpreters. by default, 
    these codepoints are escaped (to `\u2028` and `\u2029`, respectively) to 
    retain compatibility. this option simply removes that escaping

- `comments`

    json has no official comments but some parsers allow c/c++ style comments. 
    anywhere whitespace is allowed this flag allows comments (both `// ...` and 
    `/* ... */`)

- `escaped_strings`

    by default both the encoder and decoder return strings as utf8 binaries 
    appropriate for use in erlang. escape sequences that were present in decoded 
    terms are converted into the appropriate codepoint while encoded terms are 
    unaltered. this flag escapes strings as if for output in json, removing 
    control codes and problematic codepoints and replacing them with the 
    appropriate escapes

- `ignored_bad_escapes`

    during decoding ignore unrecognized escape sequences and leave them as is in 
    the stream. note that combining this option with `escaped_strings` will 
    result in the escape character itself being escaped

- `dirty_strings`

    json escaping is lossy; it mutates the json string and repeated application 
    can result in unwanted behaviour. if your strings are already escaped (or 
    you'd like to force invalid strings into "json" you monster) use this flag 
    to bypass escaping. this can also be used to read in **really** invalid json 
    strings. everything but escaped quotes are passed as is to the resulting 
    string term. note that this overrides `ignored_bad_escapes`, 
    `unescaped_jsonp` and `escaped_strings`

- `explicit_end`

    see [incomplete input](#incomplete-input)

- `relax`

    relax is a synonym for `[replaced_bad_utf8, single_quoted_strings, comments, 
    ignored_bad_escapes]` for when you don't care how absolutely terrible your 
    json input is, you just want the parser to do the best it can

- `incomplete_handler` & `error_handler`

    the default incomplete and error handlers can be replaced with user defined 
    handlers. if options include `{error_handler, F}` and/or 
    `{incomplete_handler, F}` where `F` is a function of arity 3 they will be 
    called instead of the default handler. the spec for `F` is as follows
    ```erlang
    F(Remaining, InternalState, Config) -> any()
    
      Remaining = binary() | term()
      InternalState = opaque()
      Config = list()
    ```
    `Remaining` is the binary fragment or term that caused the error
    
    `InternalState` is an opaque structure containing the internal state of the 
    parser/decoder/encoder
    
    `Config` is a list of options/flags in use by the parser/decoder/encoder
    
    these functions should be considered experimental for now


## exports ##


#### `encoder/3`, `decoder/3` & `parser/3` ####

```erlang
decoder(Module, Args, Opts) -> Fun((JSONText) -> any())
encoder(Module, Args, Opts) -> Fun((JSONTerm) -> any())
parser(Module, Args, Opts) -> Fun((Tokens) -> any())

  Module = atom()
  Args = any()
  Opts = [option()]
  JSONText = json_text()
  JSONTerm = json_term()
  Tokens = token() | [token()]
```

jsx is a json compiler with interleaved tokenizing, syntactic analysis and 
semantic analysis stages. included are two tokenizers; one that handles json 
texts (`decoder/3`) and one that handles erlang terms (`encoder/3`). there is 
also an entry point to the syntactic analysis stage for use with user-defined 
tokenizers (`parser/3`)

all three functions return an anonymous function that takes the appropriate type 
of input and returns the result of performing semantic analysis, the tuple 
`{incomplete, F}` where `F` is a new anonymous function (see 
[incomplete input](#incomplete_input)) or a `badarg` error exception if 
syntactic analysis fails

`Module` is the name of the callback module

`Args` is any term that will be passed to `Module:init/1` prior to syntactic 
analysis to produce an initial state

`Opts` are detailed [here](#option)

check out [callback module documentation](#callback_exports) for details of 
the callback module interface

#### `decode/1,2` ####

```erlang
decode(JSON) -> Term
decode(JSON, Opts) -> Term

  JSON = json_text()
  Term = json_term()
  Opts = [option() | labels | {labels, Label} | {post_decode, F}]
    Label = binary | atom | existing_atom | attempt_atom
    F = fun((any()) -> any())
```

`decode` parses a json text (a `utf8` encoded binary) and produces an erlang 
term

the option `labels` controls how keys are converted from json to
erlang terms.  `binary` (the default behavior) does no conversion
beyond normal escaping. `atom` converts keys to erlang atoms and
results in a `badarg` error if the keys fall outside the range of erlang
atoms. `existing_atom` is identical to `atom` except it will not add
new atoms to the atom table and will result in a `badarg` error if the atom
does not exist. `attempt_atom` will convert keys to atoms when they exist,
and leave them as binary otherwise

`{post_decode, F}` is a user defined function of arity 1 that is called on each 
output value (objects, arrays, strings, numbers and literals). it may return any 
value to be substituted in the returned term. for example:

```erlang
1> F = fun(V) when is_list(V) -> V; (V) -> false end.
2> jsx:decode(<<"{\"a list\": [true, \"a string\", 1]}">>, [{post_decode, F}]).
[{<<"a list">>, [false, false, false]}]
```

declaring more than one post-decoder will result in a `badarg` error exception

raises a `badarg` error exception if input is not valid json


#### `encode/1,2` ####

```erlang
encode(Term) -> JSON
encode(Term, Opts) -> JSON

  Term = json_term()
  JSON = json_text()
  Opts = [option() | {pre_encode, F} | space | {space, N} | indent | {indent, N}]
    F = fun((any()) -> any())
    N = pos_integer()
```

`encode` converts an erlang term into json text (a `utf8` encoded binary)

the option `{space, N}` inserts `N` spaces after every comma and colon in your 
json output. `space` is an alias for `{space, 1}`. the default is `{space, 0}`

the option `{indent, N}` inserts a newline and `N` spaces for each level of 
indentation in your json output. note that this overrides spaces inserted after 
a comma. `indent` is an alias for `{indent, 1}`. the default is `{indent, 0}`

`{pre_encode, F}` is a user defined function of arity 1 that is called on each 
input value. it may return any valid json value to be substituted in the 
returned json. for example:

```erlang
1> F = fun(V) when is_list(V) -> V; (V) -> false end.
2> jsx:encode([{<<"a list">>, [true, <<"a string">>, 1]}], [{pre_encode, F}]).
<<"{\"a list\": [false, false, false]}">>
```

declaring more than one pre-encoder will result in a `badarg` error exception

raises a `badarg` error exception if input is not a valid 
[erlang representation of json](#json---erlang-mapping)


#### `format/1,2` ####

```erlang
format(JSON) -> JSON
format(JSON, Opts) -> JSON

  JSON = json_text()
  Opts = [option() | space | {space, N} | indent | {indent, N}]
    N = pos_integer()
```

`format` parses a json text (a `utf8` encoded binary) and produces a new json 
text according to the format rules specified by `Opts`

the option `{space, N}` inserts `N` spaces after every comma and colon in your 
json output. `space` is an alias for `{space, 1}`. the default is `{space, 0}`

the option `{indent, N}` inserts a newline and `N` spaces for each level of 
indentation in your json output. note that this overrides spaces inserted after 
a comma. `indent` is an alias for `{indent, 1}`. the default is `{indent, 0}`

raises a `badarg` error exception if input is not valid json


#### `minify/1` ####

```erlang
minify(JSON) -> JSON

  JSON = json_text()
```

`minify` parses a json text (a `utf8` encoded binary) and produces a new json 
text stripped of whitespace

raises a `badarg` error exception if input is not valid json


#### `prettify/1` ####

```erlang
prettify(JSON) -> JSON

  JSON = json_text()
```

`prettify` parses a json text (a `utf8` encoded binary) and produces a new json 
text equivalent to `format(JSON, [{space, 1}, {indent, 2}])`

raises a `badarg` error exception if input is not valid json


#### `is_json/1,2` ####

```erlang
is_json(MaybeJSON) -> true | false
is_json(MaybeJSON, Opts) -> true | false

  MaybeJSON = any()
  Opts = options()
```

returns true if input is a valid json text, false if not

what exactly constitutes valid json may be [altered](#option)


#### `is_term/1,2` ####

```erlang
is_term(MaybeJSON) -> true | false
is_term(MaybeJSON, Opts) -> true | false

  MaybeJSON = any()
  Opts = options()
```

returns true if input is a valid erlang representation of json, false if not

what exactly constitutes valid json may be altered via [options](#option)

## callback exports ##

the following functions should be exported from a jsx callback module

#### `Module:init/1` ####

```erlang
Module:init(Args) -> InitialState

  Args = any()
  InitialState = any()
```

whenever any of `encoder/3`, `decoder/3` or `parser/3` are called, this function 
is called with the `Args` argument provided in the calling function to obtain 
`InitialState`

#### `Module:handle_event/2` ####

```erlang
Module:handle_event(Event, State) -> NewState

  Event = [event()]
  State = any()
  NewState = any()
```

semantic analysis is performed by repeatedly calling `handle_event/2` with a 
stream of events emitted by the tokenizer and the current state. the new state 
returned is used as the input to the next call to `handle_event/2`. the 
following events must be handled:

-   `start_object`

    the start of a json object

-   `end_object`

    the end of a json object

-   `start_array`

    the start of a json array

-   `end_array`

    the end of a json array

-   `{key, binary()}`

    a key in a json object. this is guaranteed to follow either `start_object` 
    or a json value. it will usually be a `utf8` encoded binary. see the 
    [options](#option) for possible exceptions

-   `{string, binary()}`

    a json string. it will usually be a `utf8` encoded binary. see the 
    [options](#option) for possible exceptions

-   `{integer, integer()}`

    an erlang integer (bignum)

-   `{float, float()}`

    an erlang float

-   `{literal, true}`

    the atom `true`

-   `{literal, false}`

    the atom `false`

-   `{literal, null}`

    the atom `null`

-   `end_json`

    this event is emitted when syntactic analysis is completed. you should 
    do any cleanup and return the result of your semantic analysis


## acknowledgements ##

jsx wouldn't be what it is without the contributions of [paul davis](https://github.com/davisp), [lloyd hilaiel](https://github.com/lloyd), [john engelhart](https://github.com/johnezang), [bob ippolito](https://github.com/etrepum), [fernando benavides](https://github.com/elbrujohalcon), [alex kropivny](https://github.com/amtal), [steve strong](https://github.com/srstrong), [michael truog](https://github.com/okeuday), [dmitry kolesnikov](https://github.com/fogfish) and [emptytea](https://github.com/emptytea)

[json]: http://json.org
[yajl]: http://lloyd.github.com/yajl
[MIT]: http://www.opensource.org/licenses/mit-license.html
[rebar]: https://github.com/rebar/rebar
[meck]: https://github.com/eproxus/meck
[rfc4627]: http://tools.ietf.org/html/rfc4627
[travis]: https://travis-ci.org/
