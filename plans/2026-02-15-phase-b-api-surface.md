# Phase B API Surface Design (2026-02-15)

## Decisions
- Package namespace: `openweathermap`
- Breaking rename policy: no compatibility aliases for old `openweathermap-onecall` symbols.
- OpenAPI layout: one spec file per API family under `spec/`.
- Public naming convention:
  - URL builders: `build-<feature>-url`
  - Request metadata: `make-<feature>-request`
  - Executing fetchers: `fetch-<feature>`
- Response model: plist-decoded JSON as baseline.

## Module Layout
- `src/client.lisp`: shared transport, retry, error mapping, JSON decode.
- `src/apis/current.lisp`: Current Weather API surface.
- `src/apis/forecast.lisp`: Forecast API surface.
- `src/apis/geocoding.lisp`: Direct/reverse/zip geocoding API surface.
- `src/apis/air-pollution.lisp`: Current/forecast/history air pollution API surface.
- `src/apis/maps.lisp`: Weather Maps tile URL/request/fetch surface.

## Shared Pipeline Contract
All `fetch-*` functions should:
1. Resolve API key via `ensure-api-key`.
2. Use shared retry/timeout policy from runtime configuration.
3. Signal standardized conditions for request/network/parse failures.
4. Return decoded plist data, except tile endpoints which return raw payload bytes.

## Runtime Configuration
- `*api-base-url*` for API endpoints.
- `*maps-base-url*` for tile endpoints.
- `*request-timeout-seconds*`, `*max-retries*`, `*retry-backoff-seconds*` for transport behavior.

## Next Implementation Order
1. Current Weather
2. Forecast
3. Geocoding
4. Air Pollution
5. Maps
