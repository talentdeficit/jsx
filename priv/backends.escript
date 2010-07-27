#!/usr/bin/env escript

%% The MIT License

%% Copyright (c) 2010 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

main(["create"]) ->
	lists:foreach(
	    fun(X) -> to_src(X) end, 
	    [ to_abf(Backend) || Backend <- ["utf8", "utf16", "utf16le", "utf32", "utf32le"] ]
	);
	
main(["clean"]) ->
    [ file:delete(Filename) || Filename <- ["src/jsx_utf8.erl", "src/jsx_utf16.erl", "src/jsx_utf16le.erl", "src/jsx_utf32.erl", "src/jsx_utf32le.erl"] ].
    
to_abf(Backend) ->
    case os:getenv("TMPDIR") of
        false -> Out = "."
        ; Out -> Out
    end,
    Name = to_modname(Backend),
    {ok, _, ABF} = compile:file(
        "priv/jsx_decoder.erl", 
        [binary, 'P', {outdir, Out}, {d, list_to_atom(Backend)}, {d, name, Name}]
    ),
	{Name, ABF}.
	
to_src({Name, ABF}) ->
	{ok, Outfile} = file:open("src/" ++ atom_to_list(Name) ++ ".erl", [write, delayed_write]),
	lists:foreach(fun(Form) -> io:put_chars(Outfile, [erl_pp:form(Form), "\n"]) end, ABF),
	file:close(Outfile).
    
to_modname(Name) ->
    list_to_atom("jsx_" ++ Name).
    
        
    