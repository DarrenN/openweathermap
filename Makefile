# SPDX-License-Identifier: MIT
PROJECT := openweathermap
UNIT_SYSTEM := $(PROJECT)-tests
INTEGRATION_SYSTEM := $(PROJECT)-integration-tests
REDOCLY := ./node_modules/.bin/redocly

.PHONY: help test unit integration integration-onecall integration-current integration-forecast integration-geocoding integration-air-pollution integration-maps test-all check lint spec-check update-weather-conditions repl repl-tests repl-integration tree prepare-cache

help:
	@echo "Available targets:"
	@echo "  make unit           Run unit tests"
	@echo "  make integration    Run integration tests (requires OPENWEATHER_API_KEY and OPENWEATHERMAP_RUN_LIVE_TESTS=1)"
	@echo "  make integration-onecall       Run One Call integration smoke tests"
	@echo "  make integration-current       Run Current Weather integration smoke tests"
	@echo "  make integration-forecast      Run Forecast integration smoke tests"
	@echo "  make integration-geocoding     Run Geocoding integration smoke tests"
	@echo "  make integration-air-pollution Run Air Pollution integration smoke tests"
	@echo "  make integration-maps          Run Maps integration smoke tests"
	@echo "  make test           Alias for unit"
	@echo "  make test-all       Run unit + integration"
	@echo "  make check          Run lint + unit tests"
	@echo "  make lint           Placeholder lint target"
	@echo "  make spec-check     Validate OpenAPI spec with Redocly CLI"
	@echo "  make update-weather-conditions Refresh weather condition/icon dataset artifacts"
	@echo "  make repl           Start SBCL REPL"
	@echo "  make repl-tests     Start SBCL REPL with unit test system loaded"
	@echo "  make repl-integration Start SBCL REPL with integration test system loaded"
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

integration-onecall: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))' \
		--eval '(asdf:load-system :openweathermap-integration-tests)' \
		--eval '(uiop:symbol-call :openweathermap/integration-tests :run-onecall-integration-tests)'

integration-current: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))' \
		--eval '(asdf:load-system :openweathermap-integration-tests)' \
		--eval '(uiop:symbol-call :openweathermap/integration-tests :run-current-integration-tests)'

integration-forecast: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))' \
		--eval '(asdf:load-system :openweathermap-integration-tests)' \
		--eval '(uiop:symbol-call :openweathermap/integration-tests :run-forecast-integration-tests)'

integration-geocoding: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))' \
		--eval '(asdf:load-system :openweathermap-integration-tests)' \
		--eval '(uiop:symbol-call :openweathermap/integration-tests :run-geocoding-integration-tests)'

integration-air-pollution: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))' \
		--eval '(asdf:load-system :openweathermap-integration-tests)' \
		--eval '(uiop:symbol-call :openweathermap/integration-tests :run-air-pollution-integration-tests)'

integration-maps: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))' \
		--eval '(asdf:load-system :openweathermap-integration-tests)' \
		--eval '(uiop:symbol-call :openweathermap/integration-tests :run-maps-integration-tests)'

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

update-weather-conditions:
	node scripts/update-weather-conditions.mjs

repl: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-system :openweathermap)' \
		--eval '(in-package :openweathermap)'

repl-tests: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-tests.asd"))' \
		--eval '(asdf:load-system :openweathermap-tests)' \
		--eval '(in-package :openweathermap/tests)'

repl-integration: prepare-cache
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "openweathermap.asd"))' \
		--eval '(asdf:load-asd (truename "openweathermap-integration-tests.asd"))' \
		--eval '(asdf:load-system :openweathermap-integration-tests)' \
		--eval '(in-package :openweathermap/integration-tests)'

tree:
	@find . -maxdepth 2 -type f | sort
