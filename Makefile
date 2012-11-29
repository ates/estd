REBAR = rebar

compile:
	@$(REBAR) compile

test:
	@$(REBAR) xref eunit

clean:
	@$(REBAR) clean

doc:
	@$(REBAR) doc

.PHONY: doc test
