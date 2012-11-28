REBAR = rebar

compile:
	@$(REBAR) compile

test:
	@$(REBAR) xref eunit

.PHONY: test
