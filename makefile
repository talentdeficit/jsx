compile: expand
	./rebar compile
	./priv/backends.escript clean
	
expand:
	./priv/backends.escript create
    
test: compile
	./priv/jsx_test.escript test/cases
	
prove: compile
	prove ./priv/jsx_test.escript

clean:
	./rebar clean
	./priv/backends.escript clean
	
package: compile
	./rebar install target=.