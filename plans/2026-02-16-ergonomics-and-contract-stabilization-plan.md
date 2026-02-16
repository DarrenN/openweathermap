# Ergonomics And Contract Stabilization Plan

Date: 2026-02-16

## Goal
Improve first-use ergonomics and API contract stability for the `openweathermap` Common Lisp client while preserving backward-compatible core API naming where practical.

## Phase 1: Contract Stabilization (Completed)
- [x] Validate required One Call-family positional inputs at URL-builder boundary:
  - `build-onecall-url` requires numeric `lat` and `lon`
  - `build-timemachine-url` requires numeric `lat`/`lon` and integer `dt`
  - `build-day-summary-url` requires numeric `lat`/`lon` and `YYYY-MM-DD` `date`
  - `build-overview-url` requires numeric `lat` and `lon`
- [x] Enforce strict query-param plist shape for rest arguments:
  - reject odd-length keyword/value lists
  - signal `invalid-parameters-error` with clear message
- [x] Normalize decoded JSON object keys to stable keyword symbols so callers can rely on straightforward `(getf data :key)` access.
- [x] Add/expand unit tests for all validations and key-normalization behavior.

## Phase 2: Public API Ergonomics (Planned)
- [ ] Export condition slot readers for structured downstream error handling:
  - `api-request-error-status-code`
  - `api-request-error-message`
  - `api-request-error-endpoint`
  - `invalid-parameters-error-message`
- [ ] Add consumer-friendly alias(es) where naming is awkward (e.g. `make-onecall-request` alias for `make-client-weather-request`) while retaining existing names.
- [ ] Evaluate optional per-client configuration model to reduce global mutable state friction.

## Phase 3: Documentation And Examples (Planned)
- [ ] Add a consumer-first quickstart section (Quicklisp + one live fetch + error handling snippet).
- [ ] Document response-key stability contract and validation/error behavior.
- [ ] Add an explicit “Error Handling” section with `handler-case` patterns.
- [ ] Refresh examples to follow the stabilized key-access contract consistently.

## Exit Criteria
- Phase 1:
  - unit tests pass with new validation/normalization assertions
  - no regressions in existing API-family unit suites
- Phase 2:
  - downstream callers can introspect condition details without internal package access
- Phase 3:
  - README and examples support “first 5 minutes” onboarding with minimal ambiguity
