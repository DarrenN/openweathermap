PROJECT := openweathermap
UNIT_SYSTEM := $(PROJECT)-tests
INTEGRATION_SYSTEM := $(PROJECT)-integration-tests
REDOCLY := ./node_modules/.bin/redocly

.PHONY: help test unit integration test-all check lint spec-check repl tree prepare-cache

help:
	@echo "Available targets:"
	@echo "  make unit           Run unit tests"
	@echo "  make integration    Run integration tests (requires OPENWEATHER_API_KEY)"
	@echo "  make test           Alias for unit"
	@echo "  make test-all       Run unit + integration"
	@echo "  make check          Run lint + unit tests"
	@echo "  make lint           Placeholder lint target"
	@echo "  make spec-check     Validate OpenAPI spec with Redocly CLI"
	@echo "  make repl           Start SBCL REPL"
	@echo "  make tree           Show top-level project tree"

prepare-cache:
	@mkdir -p .cache

unit test: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-tests.asd"))' \
		--eval '(asdf:test-system :$(UNIT_SYSTEM))'

integration: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))' \
		--eval '(asdf:test-system :$(INTEGRATION_SYSTEM))'

test-all:
	@$(MAKE) unit
	@$(MAKE) integration

lint:
	@echo "No linter configured yet. Add one in Phase 5."

check:
	@$(MAKE) lint
	@$(MAKE) unit

spec-check:
	@if [ -x $(REDOCLY) ]; then \
		$(REDOCLY) lint spec/*.yaml; \
	else \
		echo "Redocly CLI not found. Run: npm install"; \
		exit 1; \
	fi

repl: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))'

tree:
	@find . -maxdepth 2 -type f | sort
