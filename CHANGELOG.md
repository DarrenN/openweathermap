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
- Draft OpenAPI 3.0 spec in `spec/openapi.yaml` covering:
  - `/data/3.0/onecall`
  - `/data/3.0/onecall/timemachine`
  - `/data/3.0/onecall/day_summary`
  - `/data/3.0/onecall/overview`

### Changed
- Replaced placeholder OpenAPI file with reusable components for parameters, shared weather schemas, and standard error responses.
- Updated bootstrap plan to mark Phase 1 complete and Phase 2 draft/spec-capture items complete.

### Notes
- `make spec-check` currently skips semantic validation because `swagger-cli` is not installed in this environment.
