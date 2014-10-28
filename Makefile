all:
	rebar compile

test:
	rebar eunit

deps:
	rebar get-deps