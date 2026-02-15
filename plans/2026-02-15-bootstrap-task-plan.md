# Bootstrap Task Plan (2026-02-15)

> Superseded by `plans/2026-02-15-multi-api-replan.md` after scope expanded to multiple OpenWeatherMap APIs and package rename to `openweathermap`.

## Scope
Set up a production-quality Common Lisp client for OpenWeatherMap One Call 3.0 with strong testing, good developer ergonomics, and generated API surfaces from a project-local OpenAPI spec.

## Phase 1: Foundation
- [x] Create initial project structure (`src/`, `tests/`, `integration-tests/`, `spec/`, `examples/`).
- [x] Choose and configure system/tooling files (`*.asd`, package definitions, test system).
- [x] Add `README.org` baseline (install, quick start, development workflow).
- [x] Add comprehensive `Makefile` targets for setup, lint/check, unit tests, integration tests, and docs/spec checks.
- [x] Add `CHANGELOG.md` and start session entries.

## Phase 2: API Specification
- [x] Read OpenWeatherMap One Call 3.0 docs and draft an OpenAPI/Swagger file under `spec/`.
- [x] Capture endpoints, params, units/lang options, and response schemas.
- [x] Validate the spec with an OpenAPI linter/validator.
- [x] Record assumptions and unresolved doc ambiguities.

## Phase 3: Client Core
- [x] Implement HTTP client layer (timeouts, retries, base URL, API key handling).
- [x] Implement request builders for each One Call operation.
- [x] Implement response decoding into idiomatic Lisp structures.
- [x] Add robust error mapping (HTTP/network/API-level errors).
- [x] Document public API surface and usage examples.

## Phase 4: Testing
- [ ] Add unit tests for request serialization and response parsing.
- [ ] Add tests for error handling branches.
- [ ] Add integration test harness for live API (separate target/suite).
- [ ] Gate live tests by environment variables and mark as opt-in.
- [ ] Ensure CI-friendly non-live test target.

## Phase 5: Quality and Release Readiness
- [ ] Add static/style checks for Lisp code and run through `make` targets.
- [ ] Expand docs for configuration, troubleshooting, and API coverage matrix.
- [ ] Verify semantic versioning and changelog workflow.
- [ ] Add first tagged release checklist in `plans/`.

## Suggested Execution Order (Immediate)
1. [x] Foundation skeleton + tooling + `README.org` + `CHANGELOG.md`.
2. [x] OpenAPI spec draft for core One Call endpoints.
3. [x] Minimal client implementation for one endpoint path.
4. [x] Unit test baseline and integration test scaffolding.

## Phase 2 Notes (Assumptions / Ambiguities)
- The draft spec currently targets the four core `/data/3.0` One Call weather endpoints and does not yet include assistant/chat-style endpoints.
- Error payload shape is modeled from common documented examples (`cod`, `message`, optional `parameters`) and may need tightening with captured live samples.
- Official docs include additional constraints around valid `date` values and timestamp windows; these are partially encoded in descriptions and need stricter schema bounds where feasible.
- OpenAPI semantic validation now runs through Redocly CLI via `make spec-check`.
- Current local Node runtime is `v20.10.0`; Redocly `2.18.1` executes but warns that recommended runtime is `>=20.19.0` or `>=22.12.0`.
