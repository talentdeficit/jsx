## jsx (v1.0.1) ##

a sane json implementation for erlang, inspired by [yajl][yajl]

copyright 2011, 2012 alisdair sullivan

jsx is released under the terms of the [MIT][MIT] license

jsx uses [rebar][rebar] and [meck][meck]

[![Build Status](https://secure.travis-ci.org/talentdeficit/jsx.png?branch=master)](http://travis-ci.org/talentdeficit/jsx)


## api ##


**converting json to erlang terms**

parses a JSON text (a utf8 encoded binary) and produces an erlang term (see json <-> erlang mapping details below)

`to_term(JSON)` -> `Term`

`to_term(JSON, Opts)` -> `Term`

types:

* `JSON` = `binary()`
* `Term` = `[]` | `[{}]` | `[Value]` | `[{Label, Value}]` | `{incomplete, Fun}`
* `Value` = `binary()` | `integer()` | `float()` | `true` | `false` | `null`
* `Label` = `binary()` | `atom()`
* `Fun` = `fun(JSON)` -> `Term`
* `Opts` = `[]` | `[Opt]`
* `Opt` =
    - `loose_unicode`
    - `labels`
    - `{labels, Label}`
    - `Label` =
        * `binary`
        * `atom`
        * `existing_atom`
    - `explicit_end`

`JSON` SHOULD be a utf8 encoded binary. if the option `loose_unicode` is present attempts are made to replace invalid codepoints with `u+FFFD` but badly encoded binaries may, in either case, result in `badarg` errors 

the option `labels` controls how keys are converted from json to erlang terms. `binary` does no conversion beyond normal escaping. `atom` converts keys to erlang atoms, and results in a badarg error if keys fall outside the range of erlang atoms. `existing_atom` is identical to `atom`, except it will not add new atoms to the atom table

see the note below about streaming mode for details of `explicit_end`

**converting erlang terms to json**
    
produces a JSON text from an erlang term (see json <-> erlang mapping details below)

`to_json(Term)` -> `JSON`

`to_json(Term, Opts)` -> `JSON`

types:
        
* `JSON` = `binary()`
* `Term` = `[]` | `[{}]` | `[Value]` | `[{Label, Value}]` | `{incomplete, Fun}`
* `Value` = `binary()` | `integer()` | `float()` | `true` | `false` | `null`
* `Label` = `binary()` | `atom()`
* `Opts` = `[]` | `[Opt]`
* `Opt` =
    - `space`
    - `{space, N}`
    - `indent`
    - `{indent, N}`
    - `escape_forward_slash`

the option `{space, N}` inserts `N` spaces after every comma and colon in your json output. `space` is an alias for `{space, 1}`. the default is `{space, 0}`

the option `{indent, N}` inserts a newline and `N` spaces for each level of indentation in your json output. note that this overrides spaces inserted after a comma. `indent` is an alias for `{indent, 1}`. the default is `{indent, 0}`
      
if the option `escape_forward_slash` is enabled, `$/` is escaped. this is not normally required but is necessary for compatibility with microsoft's json date format


**formatting json texts**
    
produces a JSON text from JSON text, reformatted

`format(JSON)` -> `JSON`

`format(JSON, Opts)` -> `JSON`

types:

* `JSON` = `binary()`
* `Term` = `[]` | `[{}]` | `[Value]` | `[{Label, Value}]` | `{incomplete, Fun}`
* `Value` = `binary()` | `integer()` | `float()` | `true` | `false` | `null`
* `Label` = `binary()` | `atom()`
* `Fun` = `fun(JSON)` -> `Term`
* `Opts` = `[]` | `[Opt]`
* `Opt` =
    - `space`
    - `{space, N}`
    - `indent`
    - `{indent, N}`
    - `loose_unicode`
    - `escape_forward_slash`
    - `explicit_end`

`JSON` SHOULD be a utf8 encoded binary. if the option `loose_unicode` is present attempts are made to replace invalid codepoints with `u+FFFD` but badly encoded binaries may, in either case, result in `badarg` errors 

the option `{space, N}` inserts `N` spaces after every comma and colon in your json output. `space` is an alias for `{space, 1}`. the default is `{space, 0}`

the option `{indent, N}` inserts a newline and `N` spaces for each level of indentation in your json output. note that this overrides spaces inserted after a comma. `indent` is an alias for `{indent, 1}`. the default is `{indent, 0}`

if the option `escape_forward_slash` is enabled, `$/` is escaped. this is not normally required but is necessary for compatibility with microsoft's json date format

see the note below about streaming mode for details of `explicit_end`


**verifying json texts**
    
returns true if input is a valid JSON text, false if not

`is_json(MaybeJSON)` -> `true` | `false` | `{incomplete, Fun}`

`is_json(MaybeJSON, Opts)` -> `true` | `false` | `{incomplete, Fun}`

types:

* `MaybeJSON` = `any()`
* `Opts` = `[]` | `[Opt]`
* `Opt` =
    - `loose_unicode`
    - `explicit_end`

see `json_to_term` for details of options


**verifying json texts**
    
returns true if input is a valid erlang term that represents a JSON text, false if not

`is_term(MaybeJSON)` -> `true` | `false`

types:

* `MaybeJSON` = `any()`


**streaming mode**

this implementation is interruptable and reentrant and may be used to incrementally parse json texts. it's greedy and will exhaust input, returning when the stream buffer is empty. if the json text is so far valid, but incomplete (or if the option `explicit_end` has been selected), `{incomplete, Fun}` will be returned. `Fun/1` may be called with additional input (or the atom `end_stream` to force the end of parsing)

`explicit_end` is of use when parsing bare numbers (like `123` or `-0.987` for example) as they may have no unambiguous end when encountered in a stream. it is also of use when reading from a socket or file and there may be unprocessed white space (or errors) left in the stream


## json <-> erlang ##

**json**                        | **erlang**
--------------------------------|--------------------------------
`number`                        | `integer()` OR `float()`
`string`                        | `binary()`
`true`, `false` and `null`      | `true`, `false` and `null`
`array`                         | `[]` OR `[JSON]`
`object`                        | `[{}]` OR `[{binary(), JSON}]`

**json**

json must be encoded in `utf8`. if it's invalid `utf8`, it probably won't parse without errors. one optional exception is made for json strings that are otherwise `utf8`, see under `strings` below.

**numbers**

javascript and thus json represent all numeric values with floats. as this is woefully insufficient for many uses, **jsx**, just like erlang, supports bigints. whenever possible, this library will interpret json numbers that look like integers as integers. other numbers will be converted to erlang's floating point type, which is nearly but not quite iee754. negative zero is not representable in erlang (zero is unsigned in erlang and `0` is equivalent to `-0`) and will be interpreted as regular zero. numbers not representable are beyond the concern of this implementation, and will result in parsing errors

when converting from erlang to json, numbers are represented with their shortest representation that will round trip without loss of precision. this means that some floats may be superficially dissimilar (although functionally equivalent). for example, `1.0000000000000001` will be represented by `1.0`

**strings**

the [json spec][rfc4627] is frustratingly vague on the exact details of json strings. json must be unicode, but no encoding is specified. javascript explicitly allows strings containing codepoints explicitly disallowed by unicode. json allows implementations to set limits on the content of strings and other implementations attempt to resolve this in various ways. this implementation, in default operation, only accepts strings that meet the constraints set out in the json spec (properly escaped control characters and quotes) and that are encoded in `utf8`. in the interests of pragmatism, however, the parser option `loose_unicode` attempts to replace invalid `utf8` sequences with the replacement codepoint `u+fffd` when possible

all erlang strings are represented by *valid* `utf8` encoded binaries

this implementation performs no normalization on strings beyond that detailed here. be careful when comparing strings as equivalent strings may have different `utf8` encodings

**true, false and null**

the json primitives `true`, `false` and `null` are represented by the erlang atoms `true`, `false` and `null`. surprise

**arrays**

json arrays are represented with erlang lists of json values as described in this document

**objects**

json objects are represented by erlang proplists. the empty object has the special representation `[{}]` to differentiate it from the empty list. ambiguities like `[true, false]` prevent using the shorthand form of property lists using atoms as properties. all properties must be tuples. all keys must be encoded as in `string`, above, or as atoms (which will be escaped and converted to binaries for presentation to handlers)


## acknowledgements ##

paul davis, lloyd hilaiel, john engelhart, bob ippolito, fernando benavides and alex kropivny have all contributed to the development of jsx, whether they know it or not


[yajl]: http://lloyd.github.com/yajl
[MIT]: http://www.opensource.org/licenses/mit-license.html
[rebar]: https://github.com/basho/rebar
[meck]: https://github.com/eproxus/meck
[json]: http://json.org
[rfc4627]: http://tools.ietf.org/html/rfc4627