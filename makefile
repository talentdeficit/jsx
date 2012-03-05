compile:
	./rebar --jobs=1 compile

clean:
	./rebar clean

test: clean
	./rebar --jobs=1 eunit