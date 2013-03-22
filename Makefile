all: compile

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

clean:
	./rebar clean

test: clean
	./rebar --config "rebar.test.config" get-deps
	./rebar --config "rebar.test.config" compile
	./rebar --config "rebar.test.config" skip_deps=true ct
