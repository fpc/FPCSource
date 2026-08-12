#!/usr/bin/env bash
# Debt story D4 — the BLOCKING new-code self-lint gate (NFR10 dogfooding).
#
# DECISION (settled with Michael 2026-06-28): grandfather today's ~245 self-lint
# findings via a committed baseline (config/fpsonar-selflint-baseline.json) and
# fail CI only on NEWLY-introduced findings. Leverages 5-1
# (`analyze --new-code <baseline>`) and the FR23 tixeo config.
#
# This is the SINGLE source of truth for the gate: build-verify.sh's non-blocking
# self-lint, this gate, and the .gitlab-ci `lint` job all run the SAME scope +
# config + unit paths, so the baseline finding set is identical everywhere.
#
# PATH-PORTABILITY (the load-bearing detail): the issue fingerprint (story 1.4)
# bakes in the ABSOLUTE file path, and FPC's ExpandFileName canonicalises it
# (resolving symlinks). A baseline generated under a developer's home dir would
# therefore match NOTHING on a CI runner whose checkout lives elsewhere — every
# finding would look "new" and the gate would red-flag a clean tree. The fix:
# both refresh and gate STAGE the tree into a fixed CANONICAL path
# ($CANON, default /tmp/fpsonar-selflint) and run there, so the committed
# baseline's path prefix is identical on every host.
#
# EXIT-CODE CONTRACT (verified empirically): `analyze --new-code` runs the
# quality gate over ONLY the new findings, and the DEFAULT gate (config has no
# `gate` block) trips on Blocker/Critical only (maxBlocker=0, maxCritical=0; all
# others -1=unlimited). So a new MAJOR/MINOR/INFO finding does NOT by itself make
# the CLI exit non-zero. To make the gate BLOCK on ANY new finding regardless of
# severity, this script ALSO parses the trailing `<N> issue(s).` count and fails
# when N > 0. We honour the CLI exit code too (a new Blocker/Critical, or an
# exit-2 usage/IO error such as an unloadable baseline, also fails the gate).
#
# Usage:
#   tests/self-lint-gate.sh            run the gate (CI mode); exit 0 iff 0 new findings
#   tests/self-lint-gate.sh --refresh  REGENERATE the committed baseline (when new
#                                      debt is intentionally accepted), then verify
#                                      the gate is green against it
set -uo pipefail

REPO="$(cd "$(dirname "$(readlink -f "$0")")/.." && pwd)"
CANON="${FPSONAR_SELFLINT_CANON:-/tmp/fpsonar-selflint}"
BASELINE_REL="config/fpsonar-selflint-baseline.json"

REFRESH=0
if [ "${1:-}" = "--refresh" ]; then
  REFRESH=1
elif [ -n "${1:-}" ]; then
  echo "usage: $0 [--refresh]" >&2
  exit 2
fi

# --- Stage the tree into the canonical path so fingerprints are host-stable. ---
echo "===== self-lint gate: staging tree -> $CANON ====="
rm -rf "$CANON"
mkdir -p "$CANON"
trap 'rm -rf "$CANON"' EXIT
cp -r "$REPO/src" "$REPO/config" "$REPO/third_party" "$CANON/"
cd "$CANON"

# --- Build the CLI exactly as build-verify.sh's non-blocking self-lint does
#     (throwaway unit dir; no stale .ppu from the package build leaks in). ---
echo "===== self-lint gate: building CLI ====="
SLDIR="$(mktemp -d)"
trap 'rm -rf "$CANON" "$SLDIR"' EXIT
fpc -Mobjfpc -Sh -O1 \
  -Futhird_party/fcl-passrc/src \
  -Fusrc/core -Fusrc/core/base -Fusrc/core/rules -Fusrc/core/output -Fusrc/cli \
  -Fithird_party/fcl-passrc/src \
  -FE"$SLDIR" -FU"$SLDIR" \
  src/cli/fpsonar.lpr >"$SLDIR/build.log" 2>&1
if [ ! -x "$SLDIR/fpsonar" ]; then
  echo "self-lint gate: CLI build FAILED" >&2
  tail -30 "$SLDIR/build.log" >&2
  exit 1
fi
FPSONAR="$SLDIR/fpsonar"

# The self-lint scope + config + unit paths — IDENTICAL to build-verify.sh.
# --synthetic-only (story 6.18): opt OUT of the auto-detect ppudump DEFAULT so the
# gate stays deterministic + FPC-version-independent (the committed baseline must
# not depend on the host's .ppu). Auto-detect is host-dependent by design.
SCOPE=(src/core src/cli)
ARGS=(--config config/fpsonar.tixeo.json --synthetic-only
      -Fusrc/core -Fusrc/core/base -Fusrc/core/rules -Fusrc/core/output)

if [ "$REFRESH" = "1" ]; then
  echo "===== self-lint gate: REFRESH baseline -> $BASELINE_REL ====="
  "$FPSONAR" baseline "${SCOPE[@]}" "${ARGS[@]}" -o "$CANON/$BASELINE_REL"
  if [ ! -s "$CANON/$BASELINE_REL" ]; then
    echo "self-lint gate: baseline generation produced no file" >&2
    exit 1
  fi
  cp "$CANON/$BASELINE_REL" "$REPO/$BASELINE_REL"
  COUNT="$(grep -c '"fingerprint"' "$REPO/$BASELINE_REL")"
  echo "self-lint gate: wrote $REPO/$BASELINE_REL ($COUNT fingerprints)"
  echo "(commit this file; then the gate grandfathers exactly these findings)"
fi

# --- Run the gate: report+gate ONLY findings absent from the committed baseline. ---
echo "===== self-lint gate: analyze --new-code $BASELINE_REL ====="
OUT="$("$FPSONAR" analyze --new-code "$CANON/$BASELINE_REL" \
  "${SCOPE[@]}" "${ARGS[@]}" 2>"$SLDIR/gate.err")"
CLI_EXIT=$?
echo "$OUT" | tail -2
# Surface any stderr (gate-failed note, project-load note, baseline error).
[ -s "$SLDIR/gate.err" ] && cat "$SLDIR/gate.err" >&2

# Robust failure on ANY new finding regardless of severity: parse the count.
NEW="$(printf '%s\n' "$OUT" | sed -n 's/^Analyzed [0-9]* file(s): \([0-9]*\) issue(s)\.$/\1/p' | tail -1)"

if [ "$CLI_EXIT" = "2" ]; then
  echo "SELF-LINT GATE: usage/IO error (exit 2) — baseline unloadable or bad args" >&2
  exit 1
fi
if [ -z "$NEW" ]; then
  echo "SELF-LINT GATE: could not parse the new-finding count from CLI output" >&2
  exit 1
fi
if [ "$NEW" != "0" ] || [ "$CLI_EXIT" != "0" ]; then
  echo "SELF-LINT GATE VIOLATION: $NEW NEW finding(s) not in the baseline (CLI exit $CLI_EXIT)." >&2
  echo "Fix the new finding, or — if the debt is intentionally accepted —" >&2
  echo "run 'tests/self-lint-gate.sh --refresh' and commit the updated baseline." >&2
  exit 1
fi

echo "self-lint gate OK: 0 new findings vs the committed baseline."
exit 0
