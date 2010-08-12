compile:
	./rebar compile
	
expand:
	./priv/backends.escript create
    
test:
	./rebar eunit

clean:
	./rebar clean
	
install: compile
	./rebar -f install