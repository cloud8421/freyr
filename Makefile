.PHONY: deps compile test clean start
REBAR=`which rebar || ./rebar`
all: deps compile
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
start:
	erl -pa ebin -pa deps/*/ebin -sname freyr -mnesia dir '"./data"'
