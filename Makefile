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

platform: clean
	(escript rebar_deps/merge_deps.escript rebar.config rebar_deps/$(BACKEND).deps rebar.$(BACKEND).config)
	(./rebar --config "rebar.$(BACKEND).config" get-deps)
	(./rebar --config "rebar.$(BACKEND).config" compile)

run_cowboy: platform
	(make platform run BACKEND=cowboy)

run_elli: platform
	(make platform run BACKEND=elli)

run_inets: platform
	(make platform run BACKEND=inets)

run_mochiweb: platform
	(make platform run BACKEND=mochiweb)

run_webmachine: platform
	(make platform run BACKEND=webmachine)

run_yaws: platform
	(make platform run BACKEND=yaws)

run:
	erl -pa ebin/ -pa deps/*/ebin \
		-config etc/$(BACKEND).config \
		-config etc/simple_bridge.config \
		-eval "simple_bridge:start($(BACKEND))"
