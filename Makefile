.PHONY: all compile clean check test cover doc help

REBAR3 := rebar3

#==============================================================================
# Core targets
#==============================================================================

all: compile

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

check:
	$(REBAR3) check

test:
	$(REBAR3) test

cover:
	$(REBAR3) test
	$(REBAR3) cover --verbose
	@echo ""
	@echo "Quality gate: Coverage must be >= 75%"

doc:
	$(REBAR3) doc

#==============================================================================
# Help
#==============================================================================

help:
	@echo "ncalendar Makefile targets:"
	@echo ""
	@echo "  Core:"
	@echo "    make              - Build the project"
	@echo "    make compile      - Build the project"
	@echo "    make clean        - Clean build artifacts"
	@echo "    make check        - Run fmt check, xref, dialyzer, hank"
	@echo "    make test         - Run all tests (CT + triq)"
	@echo "    make cover        - Run tests with coverage (>= 75% required)"
	@echo "    make doc          - Generate ex_doc documentation"
	@echo ""
	@echo "  Quality gates:"
	@echo "    - All checks pass (make check)"
	@echo "    - Coverage >= 75% (raised to 85% once the test suites grow)"
	@echo ""
