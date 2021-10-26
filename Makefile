BACKEND:=inets

REBAR:=./rebar3

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

platform: clean
	(escript rebar_deps/merge_deps.escript rebar.config rebar_deps/$(BACKEND).deps rebar.$(BACKEND).config)
	echo "-simple_bridge backend $(BACKEND)" > vm.args
	REBAR_CONFIG="rebar.$(BACKEND).config" $(REBAR) compile
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
	ERL_FLAGS="-args_file ${PWD}/vm.args" $(REBAR) shell \
		--apps simple_bridge \
		--config etc/simple_bridge.config

publish:
	$(REBAR) hex publish

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
	$(REBAR) --config "rebar.test.$(BACKEND).config" clean
	$(REBAR) --config "rebar.test.$(BACKEND).config" compile
	$(REBAR) --config "rebar.test.$(BACKEND).config" skip_deps=true ct

test_core: clean clean_test
	(escript rebar_deps/merge_deps.escript rebar.test.config rebar_deps/$(BACKEND).deps rebar.test.$(BACKEND).config)
	(cd test; sed "s/BACKEND/$(BACKEND)/" < app.config.src > app.config)
	$(REBAR) --config "rebar.test.$(BACKEND).config" compile
	$(REBAR) --config "rebar.test.$(BACKEND).config" skip_deps=true ct


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



