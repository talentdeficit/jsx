compile: clean get-deps
	./rebar --jobs=1 compile

compile-test: get-test-deps
	./rebar --config rebar.test.config --jobs 1 compile

clean:
	./rebar clean

clean-test:
	./rebar --config rebar.test.config clean

get-deps:
	./rebar get-deps

get-test-deps:
	./rebar --config rebar.test.config get-deps

test: clean-test compile-test
	./rebar --config rebar.test.config --jobs 1 skip_deps=true eunit