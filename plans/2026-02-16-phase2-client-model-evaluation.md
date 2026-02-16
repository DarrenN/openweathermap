# Phase 2: Per-Client Configuration Model Evaluation

Date: 2026-02-16

## Context
Current library behavior is driven by dynamically scoped global configuration variables:
- `*api-key*`
- `*api-base-url*`
- `*maps-base-url*`
- retry/timeout settings

This is simple and idiomatic for many Common Lisp libraries, but can be limiting for multi-tenant usage where different API keys/settings must coexist cleanly.

## Evaluated Options

### Option A: Keep global-only configuration
- Pros:
  - no API changes
  - lowest maintenance overhead
- Cons:
  - awkward for applications using multiple API keys
  - requires careful dynamic binding discipline in concurrent code

### Option B: Add client profile + dynamic binding helper (Recommended)
- Shape:
  - introduce a client profile object (struct/plist) containing key/base URLs/retry config
  - add a macro like `WITH-CLIENT` that dynamically binds current special vars from that profile for call scope
- Pros:
  - non-breaking for existing API
  - preserves current function signatures
  - fits Common Lisp dynamic-variable idioms
- Cons:
  - adds one abstraction layer
  - requires docs and examples for correct usage

### Option C: Thread explicit client object through all public APIs
- Pros:
  - fully explicit state
  - easiest to reason about in large/multi-tenant systems
- Cons:
  - broad API expansion/breaking changes
  - significant migration/documentation burden

## Recommendation
Adopt **Option B** as a backward-compatible enhancement in a follow-up implementation step:
- keep current global API intact
- add optional client-profile APIs for advanced users
- document both simple (global) and advanced (profile-scoped) usage paths

## Suggested Follow-up Implementation Scope
- add profile constructor (`MAKE-CLIENT-PROFILE`) and binder macro (`WITH-CLIENT-PROFILE`)
- add unit tests for isolated profile scopes and nested bindings
- add README examples for:
  - single-key global setup
  - multi-key scoped setup with profile binding
