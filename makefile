compile:
	./rebar compile
	
expand:
	./priv/backends.escript create
    
test: compile
	./rebar eunit

clean:
	./rebar clean
	
install: compile
	./rebar -f install