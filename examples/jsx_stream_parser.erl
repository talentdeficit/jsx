-module(jsx_stream_parser).

-export([decoder/1, event/2]).

decoder(Opts) ->
    Decoder = jsx:decoder({{jsx_stream_parser, event}, 0}, Opts),
    fun(Stream) -> 
        try Decoder(Stream) of
            F when is_function(F) -> F
        catch
            throw:{ok, Result} -> Result
            ; throw:not_found -> not_found
        end
    end. 
    
event(start_object, Level) ->
    Level + 1;
    
event(start_array, 0) ->
    throw(not_found);    
event(start_array, Level) ->
    Level + 1;
    
event(end_object, Level) ->
    Level - 1;
event(end_array, Level) ->
    Level - 1;
    
event({key, "_id"}, 1) ->
    capture;
    
event({string, String}, capture) ->
    throw({ok, String});

event(eof, _) ->
    throw(not_found);    
    
event(_, Level) ->
    Level.