# Phase G: Weather Code and Icon Resolution (2026-02-16)

## Goal
Make weather condition handling ergonomic and reliable by adding library helpers that resolve:
- condition code (`weather[*].id`)
- icon code (`weather[*].icon`)
- canonical metadata (group/main/default description/day-night icons)
- icon asset URL

This phase should improve developer experience without changing existing raw response behavior.

## Why This Is Needed
- OpenWeather responses include both `id` and `icon`, but app code currently has to manually reconcile them.
- The official weather condition list provides canonical code/icon mapping:
  - https://openweathermap.org/weather-conditions
  - https://docs.openweather.co.uk/weather-conditions
- `description` in API payload may be localized (`:lang`), so callers need clear rules for when to trust payload text vs canonical mapping.

## API Design (Proposed)

### New Public Functions
- `lookup-weather-condition` `(condition-id)`  
  Returns canonical metadata for a weather condition ID, or `NIL` if unknown.
- `weather-icon-url` `(icon-code &key size secure-p)`  
  Builds icon URL from icon code (e.g. `10d`) using OpenWeather format.
- `resolve-weather-condition` `(&key id icon description main)`  
  Returns a normalized plist that merges payload fields with canonical metadata.
- `enrich-weather-entry` `(weather-entry)`  
  Accepts one `weather` object (plist) and returns enriched version.
- `enrich-weather-list` `(weather-list)`  
  Maps enrichment over a list of weather entries.

### Return Shape (Proposed)
Normalized/enriched plist keys:
- `:id` integer
- `:main` string (prefer payload, fallback canonical)
- `:description` string (prefer payload, fallback canonical)
- `:canonical-description` string (English, from mapping)
- `:icon` string (payload icon or derived default)
- `:icon-day` string (e.g. `10d`)
- `:icon-night` string (e.g. `10n`)
- `:icon-url` string
- `:condition-group` string (e.g. `Rain`)

## Source of Truth Strategy

### Canonical Data Artifact
Create and commit one generated dataset:
- `data/weather-conditions.json` (canonical source in repo)

Then generate a Lisp constant file from it for runtime lookup:
- `src/data/weather-conditions.lisp`

This avoids runtime network dependency and keeps lookups deterministic.

### Updater Script
Add script:
- `scripts/update-weather-conditions.*`

Responsibilities:
1. Fetch official weather-conditions page.
2. Parse condition table (ID/main/description/default icon).
3. Parse icon list table (day/night icon pairs + label).
4. Build normalized dataset.
5. Validate schema and write `data/weather-conditions.json`.
6. Generate/update `src/data/weather-conditions.lisp`.

If parsing fails (layout drift), script must fail loudly with a clear error.

## Resolution Rules
- Prefer API payload `weather[*].icon` for display (it is day/night specific and exact for returned context).
- Prefer API payload `description` for user-facing text (it may be localized via `:lang`).
- Use canonical mapping for:
  - fallback description when payload missing
  - canonical group/main normalization
  - default icon derivation when payload icon missing
- Unknown condition IDs should not error; return partial enrichment + `:unknown-condition-p t`.

## Integration Plan

### Phase G1: Data + Codegen
- Add dataset schema and updater script.
- Generate first committed mapping files.
- Add Make target: `make update-weather-conditions`.

### Phase G2: Library API
- Add new module: `src/weather-conditions.lisp`.
- Add lookup, resolve, enrich, and icon URL helpers.
- Export new symbols from `src/package.lisp`.

### Phase G3: Tests
- Unit tests for:
  - known IDs (e.g. 200, 500, 800, 804) map correctly
  - icon URL generation for `d/n` codes
  - fallback behavior when fields are missing
  - unknown ID behavior
  - enrichment preserves payload values where expected
- Add conformance test that all mapped icons are in valid set:
  - `01d/01n`, `02d/02n`, `03d/03n`, `04d/04n`,
  - `09d/09n`, `10d/10n`, `11d/11n`, `13d/13n`, `50d/50n`

### Phase G4: Docs + Examples
- Update `README.org` with:
  - weather condition helper API
  - icon URL usage examples
  - localization/fallback behavior notes
- Add example script:
  - `examples/07-weather-condition-resolution.lisp`

## Non-Goals
- Do not mutate existing `fetch-*` return payloads.
- Do not introduce network calls during normal library operation.
- Do not rely on undocumented/private OpenWeather endpoints.

## Risks and Mitigations
- **Risk:** Docs page markup changes and breaks parser.  
  **Mitigation:** Keep parser strict + test fixtures + clear failure output.
- **Risk:** Canonical descriptions differ from localized API response text.  
  **Mitigation:** Payload description remains primary for display.
- **Risk:** Drift between mapping file and upstream docs.  
  **Mitigation:** Add maintenance command and changelog guidance.

## Acceptance Criteria
- New helper API resolves known weather codes and icons deterministically.
- Unit tests cover lookup, URL generation, fallback paths, unknown IDs.
- Mapping data is reproducible via updater script.
- README and examples document usage clearly.
