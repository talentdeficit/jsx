compile:
	./rebar compile
	
expand:
	./priv/backends.escript create
    
test: compile
	./test/jsx_test.escript test/cases
	
prove: compile
	prove ./test/jsx_test.escript

clean:
	./rebar clean
	
install: compile
	./rebar -f install