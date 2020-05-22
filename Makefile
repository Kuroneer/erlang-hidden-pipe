#############################################
# "Alias" makefile
#############################################

# Targets to run things:
.PHONY: build test test-verbose shell clean purge

build:
	rebar3 compile

test:
	rebar3 ct

test-verbose:
	rebar3 ct --verbose

shell:
	rebar3 shell

clean:
	rebar3 clean

purge:
	rm -rf _build/* _build ebin/* _ebin

