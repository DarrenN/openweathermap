<!-- SPDX-License-Identifier: MIT -->

# OpenWeatherMap Agent Guide

## Purpose

Common Lisp client library for multiple OpenWeatherMap API families.  
Current scope includes:

- One Call 3.0
- Current Weather
- 5 Day / 3 Hour Forecast
- Geocoding (direct, reverse, zip)
- Air Pollution (current, forecast, history)
- Maps (weather tiles)

This repository also maintains OpenAPI specs for each supported family under `spec/`.

## Current Repo Structure

- `src/`
  - `package.lisp`: exported public API
  - `config.lisp`: API key/client config
  - `client.lisp`: shared HTTP + decode + error pipeline
  - `errors.lisp`: error conditions
  - `apis/*.lisp`: per-family builders/requests/fetchers
- `tests/`
  - Unit tests (FiveAM)
  - Includes `query-param-conformance-test.lisp` for URL/query correctness
- `integration-tests/`
  - Live API smoke tests, gated by env var
  - Runner prints detailed failure reports before signaling failure
- `spec/`
  - OpenAPI YAML files (one per API family)
- `data/`
  - Generated weather condition/icon mapping artifacts
- `scripts/`
  - Project maintenance generators (e.g. weather-condition dataset updater)
- `examples/`
  - URL-only examples, live fetch examples, all-APIs script, REPL workflow loader
- `plans/`
  - Working plans and phase/task notes

## Tooling and Dependencies

- SBCL + ASDF + Quicklisp
- Node.js + npm
- `@redocly/cli` for OpenAPI linting (installed via `npm install`)

## Canonical Commands

- `make unit`: run unit tests
- `make integration`: run full integration suite
- `make integration-onecall`
- `make integration-current`
- `make integration-forecast`
- `make integration-geocoding`
- `make integration-air-pollution`
- `make integration-maps`
- `make spec-check`: lint `spec/*.yaml` with Redocly
- `make update-weather-conditions`: regenerate weather condition/icon mapping artifacts
- `make check`: lint placeholder + unit tests
- `make repl`: REPL with library loaded
- `make repl-tests`: REPL with unit test system loaded
- `make repl-integration`: REPL with integration system loaded

## Environment Variables

- `OPENWEATHER_API_KEY`: required for live API calls
- `OPENWEATHERMAP_RUN_LIVE_TESTS=1` (or `true`/`yes`): enables live integration tests
- `XDG_CACHE_HOME=$(pwd)/.cache`: used by Makefile and examples to keep cache local

## Public API Conventions

Per endpoint family, keep this pattern:

- `build-*`: URL construction only
- `make-*`: request metadata plist (method/url)
- `fetch-*`: executes HTTP request and returns decoded plist (maps tile returns payload bytes/string)

## Rules for Changes

- Keep `README.org` in org-mode and update it when behavior/workflow changes.
- Keep plans in `plans/` as markdown files.
- Keep `CHANGELOG.md` updated for each session.
- Preserve idiomatic Common Lisp style and clear package exports.
- Prefer deterministic unit tests for URL/parameter behavior; keep live checks in `integration-tests/`.
- If adding or modifying API behavior, update:
  - `src/apis/*.lisp`
  - `src/package.lisp` exports (if public surface changes)
  - unit tests in `tests/`
  - integration smoke tests in `integration-tests/` (if applicable)
  - `spec/*.yaml` for relevant API family
  - `README.org` and/or `examples/` when user-facing usage changes

## Query Parameter and URL Safety Checklist

When touching URL builders:

- Verify required params are present and correctly named (`appid`, `lat`, `lon`, etc.).
- Verify optional params are passed with exact documented names (no truncation/misspelling).
- Verify values are URL-encoded correctly (spaces, commas, slashes, plus signs).
- Add/update tests in `tests/query-param-conformance-test.lisp`.
- For maps, verify layer tokens and tile URL path format against OpenWeatherMap docs.

## API Documentation Targets

Primary docs to keep aligned with implementation and `spec/*.yaml`:

- https://openweathermap.org/api/one-call-3?collection=one_call_api_3.0
- https://openweathermap.org/current?collection=current_forecast
- https://openweathermap.org/forecast5?collection=current_forecast
- https://openweathermap.org/api/air-pollution?collection=environmental
- https://openweathermap.org/api/weathermaps?collection=maps
- https://openweathermap.org/api/geocoding-api?collection=other
