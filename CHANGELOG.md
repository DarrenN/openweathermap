# Changelog

All notable changes to this project are tracked here.

## 2026-02-15
### Added
- Initial repository bootstrap for OpenWeatherMap One Call 3.0 Common Lisp client.
- Project skeleton directories: `src/`, `tests/`, `integration-tests/`, `spec/`, `examples/`.
- ASDF system definitions for library, unit tests, and integration tests.
- Minimal client/config/error source stubs and package exports.
- Unit test scaffold (FiveAM) with test runner.
- Integration test scaffold (FiveAM) with environment-gated smoke test.
- `Makefile` with test/check/spec targets.
- Baseline `README.org` with setup and development workflow.
- Bootstrap task plan in `plans/2026-02-15-bootstrap-task-plan.md`.
- Draft OpenAPI 3.0 spec in `spec/onecall.yaml` covering:
  - `/data/3.0/onecall`
  - `/data/3.0/onecall/timemachine`
  - `/data/3.0/onecall/day_summary`
  - `/data/3.0/onecall/overview`
- Node tooling manifest `package.json` with `@redocly/cli` dev dependency.
- Phase 3 client API functions:
  - URL builders for all core endpoints (`build-onecall-url`, `build-timemachine-url`, `build-day-summary-url`, `build-overview-url`)
  - Request builders (`make-client-weather-request`, `make-timemachine-request`, `make-day-summary-request`, `make-overview-request`)
  - Fetch functions with JSON decoding (`fetch-onecall`, `fetch-timemachine`, `fetch-day-summary`, `fetch-overview`)
- Runtime configuration knobs for timeout and retry behavior (`configure-client`, `*request-timeout-seconds*`, `*max-retries*`, `*retry-backoff-seconds*`).
- Additional structured error conditions for network and parse failures (`api-network-error`, `api-response-parse-error`).
- Expanded unit test coverage for endpoint URL building, fetch/retry behavior, and error signaling.
- Multi-API spec placeholders:
  - `spec/current.yaml`
  - `spec/forecast.yaml`
  - `spec/air-pollution.yaml`
  - `spec/maps.yaml`
  - `spec/geocoding.yaml`
- Phase B API surface design document: `plans/2026-02-15-phase-b-api-surface.md`.
- API-family module stubs:
  - `src/apis/current.lisp`
  - `src/apis/forecast.lisp`
  - `src/apis/geocoding.lisp`
  - `src/apis/air-pollution.lisp`
  - `src/apis/maps.lisp`
- Expanded client configuration with `*maps-base-url*`.
- Current Weather endpoint implementation with validated location selectors (`lat/lon`, `q`, `id`, `zip`) in `src/apis/current.lisp`.
- New validation condition `invalid-parameters-error` for local request contract checks.
- Dedicated Current Weather unit tests in `tests/current-test.lisp`.
- Forecast endpoint implementation with validated location selectors (`lat/lon`, `q`, `id`, `zip`) in `src/apis/forecast.lisp`.
- Dedicated Forecast unit tests in `tests/forecast-test.lisp`.
- Geocoding endpoint implementation in `src/apis/geocoding.lisp`:
  - direct geocoding (`/geo/1.0/direct`)
  - reverse geocoding (`/geo/1.0/reverse`)
  - ZIP geocoding (`/geo/1.0/zip`)
- Dedicated geocoding unit tests in `tests/geocoding-test.lisp`.

### Changed
- Replaced placeholder OpenAPI file with reusable components for parameters, shared weather schemas, and standard error responses.
- Updated bootstrap plan to mark Phase 1 complete and Phase 2 draft/spec-capture items complete.
- Switched `make spec-check` from `swagger-cli` to Redocly CLI (`redocly lint spec/*.yaml`).
- Updated `README.org` prerequisites and setup instructions to include Redocly CLI installation via `npm install`.
- Added `info.license` metadata to OpenAPI spec to satisfy Redocly recommended rules.
- Updated core ASDF system dependencies to include `dexador` (HTTP) and `jonathan` (JSON).
- Expanded `README.org` with public API usage examples for fetch/build functions and runtime configuration.
- Renamed package and ASDF systems from `openweathermap-onecall*` to `openweathermap*` (breaking rename; no compatibility aliases).
- Updated Makefile, tests, and source namespaces to the new `openweathermap` package identity.
- Standardized multi-API public naming with `build-*`, `make-*`, and `fetch-*` functions across API families.
- Updated system definition to load API-family modules from `src/apis/`.
- Updated `README.org` with API surface conventions and module layout guidance.
- Updated unit test system definition to include `tests/current-test.lisp`.
- Updated unit test system definition to include `tests/forecast-test.lisp`.
- Updated unit test system definition to include `tests/geocoding-test.lisp`.

### Notes
- Redocly validation passes in this environment; current Node (`v20.10.0`) shows a runtime version warning from Redocly, which recommends `>=20.19.0` or `>=22.12.0`.
