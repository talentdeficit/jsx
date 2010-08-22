compile:
	./rebar compile
    
test: force
	./rebar eunit

clean:
	./rebar clean
	
install: compile
	./rebar -f install
	
force: