ERLC   = erlc
ERL    = erl

all: compile

compile:
	rebar3 compile

docs:
	rebar3 edoc

clean:
	rebar3 clean
	rm -rf _build priv/efuse

otp.plt: Makefile
	dialyzer --build_plt --output_plt otp.plt --apps \
		erts kernel stdlib compiler crypto eunit \
			| fgrep -v dialyzer.ignore

efuse.plt: otp.plt
	dialyzer --add_to_plt --plt otp.plt --output_plt efuse.plt ebin

dialyzer: compile efuse.plt
	dialyzer -o dialyzer.log --add_to_plt --plt otp.plt --output_plt efuse.plt ebin
	dialyzer --plt efuse.plt -o dialyzer.log ebin

test:
	mkdir -p deps
	rm -f deps/efuse
	ln -s .. deps/efuse
	rebar3 ct
	rm deps/efuse

.PHONY: test
