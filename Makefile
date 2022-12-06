BACKEND:=inets

all: compile

get-deps: rebar
	./rebar get-deps

compile: rebar get-deps
	./rebar compile

clean: rebar clean-deps
	./rebar --config "rebar.$(BACKEND).config" clean

clean-deps:
	rm -fr deps/*

platform: rebar clean
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


rebar:
	@(echo "Building rebar2 for your platform...")
	@(mkdir -p tmp)
	@(cd tmp && \
	git clone https://github.com/choptastic/rebar && \
	cd rebar && \
	./bootstrap)
	@(echo "Moving rebar executable into thge NitrogenProject directory")
	@(mv tmp/rebar/rebar .)
	@(echo "Cleaning up rebar remnants")
	@(rm -fr tmp)

##### COMMON TEST

test: test_cowboy test_nocowboy

test_nocowboy: test_yaws test_mochiweb test_inets test_webmachine



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

clean_test:
	(rm -f rebar.test.*.config)
	(rm -f test/*.beam)

test_quick:
	./rebar --config "rebar.test.$(BACKEND).config" clean
	./rebar --config "rebar.test.$(BACKEND).config" get-deps
	./rebar --config "rebar.test.$(BACKEND).config" compile
	./rebar --config "rebar.test.$(BACKEND).config" skip_deps=true ct

test_core: clean clean_test
	(escript rebar_deps/merge_deps.escript rebar.test.config rebar_deps/$(BACKEND).deps rebar.test.$(BACKEND).config)
	(cd test; sed "s/BACKEND/$(BACKEND)/" < app.config.src > app.config)
	./rebar --config "rebar.test.$(BACKEND).config" get-deps
	./rebar --config "rebar.test.$(BACKEND).config" compile
	./rebar --config "rebar.test.$(BACKEND).config" skip_deps=true ct


DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib crypto sasl
# removed 'sasl' in attempt to minimize memory usage for Travis

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo 
	@## We don't do -r ./deps for this at least until we have a separate
	@## dialyzer test for each backend, mostly because webmachine's mochiweb
	@## version and mochiweb 2.9 conflict.
	@(dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS))

dialyzer: $(DEPS_PLT)
	@(dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin)

dialyzer-no-race: $(DEPS_PLT)
	@(dialyzer --fullpath --plt $(DEPS_PLT) -r ./ebin)

# TRAVIS-CI STUFF

ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
ERTS_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(version)]), halt()."  -noshell
ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))
ERTS_VERSION = $(shell $(ERTS_VERSION_CHECK))

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis: compile $(ERLANG_VERSION)

19: test dialyzer
20: test dialyzer
21: test dialyzer
22: test dialyzer
23:
ifeq ($(ERTS_VERSION), 11.0)
	@(echo "Skipping Inets. Inets should not be used with Erlang 23.0 (ERTS=$(ERTS_VERSION))")
else
	@(echo "Inets is fine with Erlang 23 (ERTS=$(ERTS_VERSION))")
	(make test_inets)
endif
	(make test_cowboy test_yaws test_mochiweb test_webmachine)



