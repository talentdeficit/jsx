compile:
	./rebar compile
    
test: force
	./rebar eunit

clean:
	./rebar clean
	
deps:
    git submodule init
    git submodule update