jsx
===

basically [yajl][1], but in erlang. born from a need for a stream based, incremental parser capable of outputting a number of representations. more of a json toolkit than a complete json parser. see [the homepage][2] for examples of what it can do.


installation
------------

`make` to build jsx
`make install` to install into `code:root_dir()`


notes
-----

jsx supports utf8, utf16 (little and big endian) and utf32 (little and big endian). future support is planned for erlang iolists







[1]: http://lloyd.github.com/yajl
[2]: http://talentdeficit.github.com/jsx
