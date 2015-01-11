BACKEND:=inets

all: compile

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

clean: clean-deps
	./rebar --config "rebar.$(BACKEND).config" clean

clean-deps:
	rm -fr deps/*

platform: clean
	(escript rebar_deps/merge_deps.escript rebar.config rebar_deps/$(BACKEND).deps rebar.$(BACKEND).config)
	(./rebar --config "rebar.$(BACKEND).config" get-deps)
	(./rebar --config "rebar.$(BACKEND).config" compile)
	(rm -f rebar.$(BACKEND).config)

run_cowboy:
	(make platform run BACKEND=cowboy)

run_elli:
	(make platform run BACKEND=elli)

run_inets:
	(make platform run BACKEND=inets)

run_mochiweb:
	(make platform run BACKEND=mochiweb)

run_webmachine:
	(make platform run BACKEND=webmachine)

run_yaws:
	(make platform run BACKEND=yaws)

run:
	erl -pa ebin/ -pa deps/*/ebin \
		-config etc/simple_bridge.config \
		-simple_bridge backend $(BACKEND) \
		-eval "application:start(simple_bridge)"




##### COMMON TEST

test: test_cowboy test_inets test_webmachine test_yaws
#test: test_cowboy test_inets test_mochiweb test_webmachine test_yaws

test_cowboy:
	(make test_core BACKEND=cowboy)

test_inets:
	(make test_core BACKEND=inets)

test_mochiweb:
	(make test_core BACKEND=mochiweb)

test_webmachine:
	(make test_core BACKEND=webmachine)

test_yaws:
	(make test_core BACKEND=yaws)


test_core: clean
	(escript rebar_deps/merge_deps.escript rebar.test.config rebar_deps/$(BACKEND).deps rebar.test.$(BACKEND).config)
	(cd test; sed "s/BACKEND/$(BACKEND)/" < app.config.src > app.config)
	./rebar --config "rebar.test.$(BACKEND).config" get-deps
	./rebar --config "rebar.test.$(BACKEND).config" compile
	./rebar --config "rebar.test.$(BACKEND).config" skip_deps=true ct


