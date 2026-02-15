# Multi-API Replan (2026-02-15)

## Goal Shift
Move from a One Call-only client to a broader OpenWeatherMap client that supports multiple APIs under one package name: `openweathermap`.

## Scope (from AGENTS.md)
- One Call 3.0 (existing baseline)
- Current Weather Data API
- 5 Day / 3 Hour Forecast API
- Air Pollution API
- Weather Maps API
- Geocoding API

## Phase A: Rename and Structural Migration
- [x] Rename package/system namespace from `openweathermap-onecall` to `openweathermap`.
- [x] Rename ASDF systems and file names to reflect the new package identity.
- [x] Update package exports to be API-grouped and extensible.
- [x] Update tests, Makefile targets, and docs to new names.
- [x] Preserve backwards compatibility aliases temporarily (optional) or document explicit breaking change.

## Phase B: Unified API Surface Design
- [x] Define module layout by API family (e.g., `onecall`, `current`, `forecast`, `air-pollution`, `maps`, `geocoding`).
- [x] Standardize shared request pipeline (auth, retries, timeout, error mapping, decoding).
- [x] Define naming conventions for public functions across all APIs.
- [x] Decide response representation strategy (plist baseline now; optionally typed structures later).

## Phase C: OpenAPI Specifications
- [x] Keep and refine existing One Call OpenAPI file.
- [x] Add OpenAPI specs for Current Weather Data API.
- [x] Add OpenAPI specs for 5 Day / 3 Hour Forecast API.
- [x] Add OpenAPI specs for Air Pollution API.
- [x] Add OpenAPI specs for Weather Maps API.
- [x] Add OpenAPI specs for Geocoding API.
- [x] Validate all specs with Redocly (`make spec-check`).

## Phase D: Incremental Client Implementation
- [ ] Implement Current Weather client endpoints + tests.
- [ ] Implement Forecast client endpoints + tests.
- [ ] Implement Geocoding client endpoints + tests.
- [ ] Implement Air Pollution endpoints + tests.
- [ ] Implement Weather Maps endpoints + tests.
- [ ] Integrate One Call module under the renamed package.

## Phase E: Testing and Integration Quality
- [ ] Expand unit test suite per endpoint family.
- [ ] Build integration test suites per API family (opt-in, env-gated).
- [ ] Add smoke targets for each API family in Makefile.
- [ ] Confirm non-live test path is CI-safe and deterministic.

## Phase F: Documentation and Release Readiness
- [ ] Rewrite `README.org` for multi-API usage and migration notes.
- [ ] Add API coverage matrix (documented + implemented + tested).
- [ ] Keep `CHANGELOG.md` comprehensive through migration.
- [ ] Add first pre-release checklist for the renamed package.

## Proposed Execution Order (Next)
1. Perform Phase A rename migration first.
2. Finalize Phase B API surface conventions.
3. Execute Phase C specs for all added APIs.
4. Implement APIs in Phase D in this order: current -> forecast -> geocoding -> air pollution -> maps.
5. Complete Phase E/F and prepare initial release.

## Open Decisions to Confirm
- [x] Keep temporary compatibility symbols (`openweathermap-onecall`) or ship a clean breaking rename.
- [x] Keep one combined OpenAPI file vs. one spec per API family under `spec/`.
- [x] Priority order for API implementations if different from proposed order above.
