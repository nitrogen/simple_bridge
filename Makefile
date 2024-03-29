BACKEND:=inets

REBAR:=./rebar3

all: compile

compile: rebar3 platform
	$(REBAR) compile

clean:
	rm -fr _build rebar.lock

platform: clean
	echo "-simple_bridge backend $(BACKEND)" > vm.args

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

run: platform
	ERL_FLAGS="-args_file ${PWD}/vm.args" $(REBAR) as $(BACKEND) shell \
		--apps simple_bridge \
		--config etc/simple_bridge.config

publish:
	$(REBAR) hex publish

dialyzer:
	$(REBAR) as inets dialyzer
	$(REBAR) as cowboy dialyzer
	$(REBAR) as mochiweb dialyzer
	$(REBAR) as webmachine dialyzer
	$(REBAR) as yaws dialyzer

rebar3:
	@(echo "Building rebar3 for your platform...")
	@(rm -fr tmp)
	@(mkdir -p tmp)
	@(cd tmp && \
		git clone https://github.com/erlang/rebar3 && \
		cd rebar3 && \
		./bootstrap)
	@(echo "Moving rebar3 executable into simple_bridge")
	@(mv tmp/rebar3/rebar3 .)
	@(echo "Cleaning up rebar3 remnants")
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

clean_test: clean
	(rm -f test/*.beam)

test_quick: rebar3 clean_test
	$(REBAR) as $(BACKEND) ct

test_core: rebar3 clean clean_test
	(cd test; sed "s/BACKEND/$(BACKEND)/" < app.config.src > app.config)
	$(REBAR) as $(BACKEND) ct --sys_config test/app.config


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
24: test dialyzer
