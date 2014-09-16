.PHONY: deps compile test clean start tags
REBAR=`which rebar || ./rebar`
all: deps compile
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
ct:
	@$(REBAR) ct
clean:
	@$(REBAR) clean
start:
	erl -pa ebin -pa deps/*/ebin -sname freyr -mnesia dir '"./data"'
tags:
	ctags --file-scope=no -R --languages=erlang --exclude=.eunit .
