ERLC   = erlc
ERL    = erl


all: compile

compile:
	rebar -v compile

docs:
	rebar doc skip_deps=true

clean:
	rebar clean

otp.plt:
	dialyzer --build_plt --output_plt otp.plt --apps \
		erts kernel stdlib compiler crypto eunit \
			| fgrep -v dialyzer.ignore

efuse.plt: otp.plt
	dialyzer --add_to_plt --plt otp.plt --output_plt efuse.plt ebin

dialyzer: compile efuse.plt
	dialyzer -o dialyzer.log --add_to_plt --plt otp.plt --output_plt efuse.plt ebin
	dialyzer --plt efuse.plt -o dialyzer.log ebin

test:
	rebar skip_deps=true eunit ct

node: compile
	rebar generate -fv

.PHONY: test
