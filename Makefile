all: compile

compile:
	./rebar compile

clean:
	./rebar clean

test: compile
	./rebar eunit
