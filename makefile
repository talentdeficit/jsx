compile: get-deps
	./rebar compile

compile-test: get-test-deps
	./rebar --config rebar.test.config compile

clean:
	./rebar clean

clean-test:
	./rebar --config rebar.test.config clean

get-deps:
	./rebar recursive=true get-deps

get-test-deps:
	./rebar --config rebar.test.config get-deps

test: clean-test compile-test
	./rebar --config rebar.test.config skip_deps=true eunit