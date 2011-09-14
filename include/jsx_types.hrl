-type jsx_opts() :: [jsx_opt()].
-type jsx_opt() :: multi_term
    | loose_unicode
    | escape_forward_slashes
    | {encoding, auto 
        | utf8
        | utf16
        | {utf16, little}
        | utf32
        | {utf32, little}
    }.
    

-type jsx_event() :: start_object
    | end_object
    | start_array
    | end_array
    | end_json
    | {key, list()}
    | {string, list()}
    | {integer, integer()}
    | {float, float()}
    | {literal, true}
    | {literal, false}
    | {literal, null}.


-type jsx_encodeable() :: jsx_event() | [jsx_encodeable()].


-type jsx_iterator() :: jsx_scanner() | jsx_tokenizer().


-type jsx_scanner() :: fun((binary()) -> jsx_iterator_result()).


-type jsx_tokenizer() :: fun((jsx_encodeable()) -> jsx_iterator_result()).


-type jsx_iterator_result() :: 
    {jsx, jsx_event(), fun(() -> jsx_iterator_result())}
    | {jsx, [jsx_event()], fun(() -> jsx_iterator_result())}
    | {jsx, incomplete, jsx_iterator()}
    | {error, {badjson, any()}}.