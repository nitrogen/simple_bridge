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

run_yaws:
	(make run BACKEND=yaws)

run:
	erl -pa ebin/ -pa deps/*/ebin \
		-config etc/yaws.config \
		-config etc/simple_bridge.config \
		-eval "simple_bridge:start($(BACKEND))"
