
all:
	test -d ebin || mkdir ebin
	erlc -b beam -o ebin src/*.erl
	cp src/jsx.app.src ebin/jsx.app

clean:
	rm -Rf ebin

test:
	erlc -b beam -o ebin priv/b/*.erl

run:
	erl -pa ./ebin -pa ./*/ebin
