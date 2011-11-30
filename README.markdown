jsx (v0.10.0)
=============

a sane json implementation for erlang


quickstart
----------

`make` or `./rebar compile`

`jsx:json_to_term/1,2` takes a utf8 encoded (binary) json text and produces a term as detailed below

`jsx:term_to_json/1,2` takes a term conforming to the mapping detailed below and produces a utf8 encoded (binary) json text

options for either are:
    * `loose_unicode`
    * `explicit_end`
    * `escape_forward_slash`

use `loose_unicode` to replace invalid utf8 sequences with u+FFFD instead of throwing an error

use `explicit_end` when parsing binary streams with indeterminate ends, like naked integers or json values with trailing whitespace

use `escape_forward_slash` to escape forward slashes, which is required to produce microsoft's silly date format


`jsx:format/1,2` takes a utf8 encoded (binary) json text and produces the same, reformatted.

options are:
    * {`space`, N}
    * {`indent`, N}
    
the opt `space` will insert `N` spaces after every colon and comma, `indent` will insert a newline after every comma and `N` preceding spaces for every level of indent


erlang to json mapping
----------------------

json                       | erlang
---------------------------|---------------------------
`true`, `false` and `null` | the atoms `true`, `false` and `null`
Number                     | integers if possible, floats if not
String (also Key)          | utf8 encoded binaries
Array                      | [] or [Value, ...]
Object                     | [{}] or [{Key, Value}, ...]


license
-------

jsx is copyright alisdair sullivan (alisdairsullivan@yahoo.ca) and released under the MIT license

