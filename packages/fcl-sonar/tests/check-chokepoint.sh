#!/usr/bin/env bash
# Story 1.2 (AC #3) — scanner/parser dependency chokepoint guard.
#
# CONVENTION: the vendored fcl-passrc scanner/parser units (PScanner, PParser)
# may be imported by ONLY ONE src/core unit — FpSonar.Ingest.pas (the merged
# scanner+parser adapter). Every other src/core unit must reach the scanner/
# parser exclusively through that adapter. (PasTree / AST types are NOT
# chokepointed; they are shared downstream.)
#
# Story 3.1 (AC #1, #6) — a SECOND chokepoint: the vendored resolver units
# (pasresolver, pasresolveeval) and their result/exception types may be touched
# by ONLY ONE src/core unit — FpSonar.Resolver.pas. Every SEM rule reaches
# resolved facts through that wrapper's FpSonar-owned query API. (PasTree types
# remain shared.)
#
# Exits non-zero (failing CI) if any other src/core unit imports the chokepointed
# scanner/parser/resolver units.
set -uo pipefail
cd "$(dirname "$(readlink -f "$0")")/.."

# --- Guard 1: scanner/parser (story 1.2) ---
SCANPARSE_ALLOWED_RE='src/core/base/FpSonar\.Ingest\.pas'

offenders="$(grep -rilE '\bP(Scanner|Parser)\b' src/core \
  --include='*.pas' --include='*.pp' 2>/dev/null \
  | grep -vE "$SCANPARSE_ALLOWED_RE" || true)"

if [ -n "$offenders" ]; then
  echo "CHOKEPOINT VIOLATION (1.2 AC #3): PScanner/PParser imported outside the adapter:" >&2
  echo "$offenders" >&2
  echo "Only FpSonar.Ingest.pas may import them." >&2
  exit 1
fi

# --- Guard 2: resolver (story 3.1) ---
RESOLVER_ALLOWED_RE='src/core/base/FpSonar\.Resolver\.pas'

resolver_offenders="$(grep -rilE '\bpasresolv(er|eeval)\b' src/core \
  --include='*.pas' --include='*.pp' 2>/dev/null \
  | grep -vE "$RESOLVER_ALLOWED_RE" || true)"

if [ -n "$resolver_offenders" ]; then
  echo "CHOKEPOINT VIOLATION (3.1 AC #1): pasresolver/pasresolveeval touched outside the wrapper:" >&2
  echo "$resolver_offenders" >&2
  echo "Only FpSonar.Resolver.pas may import the vendored resolver units." >&2
  exit 1
fi

echo "chokepoint OK: PScanner/PParser confined to FpSonar.Ingest; pasresolver/pasresolveeval confined to FpSonar.Resolver"
exit 0
