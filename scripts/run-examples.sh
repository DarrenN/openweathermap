#!/usr/bin/env bash
# SPDX-License-Identifier: MIT

set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CACHE_DIR="${XDG_CACHE_HOME:-$ROOT_DIR/.cache}"
SBCL_BIN="${SBCL_BIN:-sbcl}"
LIVE_MODE="${OPENWEATHERMAP_RUN_LIVE_EXAMPLES:-auto}"

OFFLINE_EXAMPLES=(
  "examples/01-build-urls.lisp"
  "examples/06-repl-workflow.lisp"
  "examples/07-weather-condition-helpers.lisp"
)

LIVE_EXAMPLES=(
  "examples/02-current-weather-fetch.lisp"
  "examples/03-geocoding-fetch.lisp"
  "examples/04-maps-tile-fetch.lisp"
  "examples/05-all-apis.lisp"
  "examples/08-current-weather-enrichment.lisp"
  "examples/09-live-parser-contract-smoke.lisp"
)

mkdir -p "$CACHE_DIR"
cd "$ROOT_DIR" || exit 1

is_true() {
  case "$1" in
    1|true|TRUE|yes|YES|on|ON) return 0 ;;
    *) return 1 ;;
  esac
}

is_false() {
  case "$1" in
    0|false|FALSE|no|NO|off|OFF) return 0 ;;
    *) return 1 ;;
  esac
}

run_example() {
  local example="$1"
  local output_file
  output_file="$(mktemp)"

  printf "=== %s ===\n" "$example"
  if XDG_CACHE_HOME="$CACHE_DIR" "$SBCL_BIN" --script "$example" >"$output_file" 2>&1; then
    echo "[OK]"
    cat "$output_file"
    rm -f "$output_file"
    echo
    return 0
  fi

  echo "[FAIL]"
  cat "$output_file"
  rm -f "$output_file"
  echo
  return 1
}

should_run_live=0
live_required=0

if is_true "$LIVE_MODE"; then
  should_run_live=1
  live_required=1
elif is_false "$LIVE_MODE"; then
  should_run_live=0
else
  if [ -n "${OPENWEATHER_API_KEY:-}" ]; then
    should_run_live=1
  fi
fi

if [ "$should_run_live" -eq 1 ] && [ -z "${OPENWEATHER_API_KEY:-}" ]; then
  echo "OPENWEATHERMAP_RUN_LIVE_EXAMPLES requires OPENWEATHER_API_KEY."
  exit 2
fi

total=0
passed=0
failed=0
skipped=0

for example in "${OFFLINE_EXAMPLES[@]}"; do
  total=$((total + 1))
  if run_example "$example"; then
    passed=$((passed + 1))
  else
    failed=$((failed + 1))
  fi
done

if [ "$should_run_live" -eq 1 ]; then
  echo "Running live examples."
  for example in "${LIVE_EXAMPLES[@]}"; do
    total=$((total + 1))
    if run_example "$example"; then
      passed=$((passed + 1))
    else
      failed=$((failed + 1))
    fi
  done
else
  echo "Skipping live examples (set OPENWEATHER_API_KEY or OPENWEATHERMAP_RUN_LIVE_EXAMPLES=1 to enable)."
  for example in "${LIVE_EXAMPLES[@]}"; do
    total=$((total + 1))
    skipped=$((skipped + 1))
    printf "=== %s ===\n" "$example"
    echo "[SKIP]"
    echo
  done
fi

echo "Summary: total=$total passed=$passed failed=$failed skipped=$skipped"

if [ "$failed" -gt 0 ]; then
  exit 1
fi

if [ "$live_required" -eq 1 ] && [ "$skipped" -gt 0 ]; then
  exit 2
fi
