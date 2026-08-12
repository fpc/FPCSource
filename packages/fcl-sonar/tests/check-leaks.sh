#!/usr/bin/env bash
# Story 1.2 (AC #5) — automated memory-leak guard.
#
# AC #5 requires the parser adapter's container engine to own the whole AST via
# AddOwnedElement, so freeing the engine frees the tree with 0 unfreed blocks.
# Story 1-1's review found exactly this leak class slip through because nothing
# checked it automatically. This guard compiles the test program WITH heaptrc
# (-gh) and fails CI if any unfreed memory block is reported after the suite
# runs — the parser ownership path is exercised by TParserTest, and the
# parse-failure ownership path (story 1.3 AC #6) by TFaultIsolationTest.
#
# The main fpmake build does NOT enable -gh (release build), so this is a
# separate, dedicated instrumentation build.
set -uo pipefail
cd "$(dirname "$(readlink -f "$0")")/.."

OUT="$(mktemp -d)"
trap 'rm -rf "$OUT"' EXIT

echo "compiling test program with heaptrc (-gh)..."
fpc -gh -Mobjfpc -Sh -O1 \
  -Futhird_party/fcl-passrc/src \
  -Fusrc/core \
  -Fusrc/core/base \
  -Fusrc/core/rules \
  -Fusrc/core/output \
  -Fusrc/cli \
  -Futests/core \
  -Fithird_party/fcl-passrc/src \
  -FE"$OUT" -FU"$OUT" \
  tests/fpsonar_tests.lpr >"$OUT/build.log" 2>&1
if [ $? -ne 0 ]; then
  echo "LEAK GUARD: heaptrc build failed" >&2
  tail -40 "$OUT/build.log" >&2
  exit 1
fi

# heaptrc writes its dump to stderr at program exit; on a clean run it reports
# no unfreed blocks. Any positive "unfreed memory blocks" count fails the guard.
run_out="$("$OUT/fpsonar_tests" --format=plain --all 2>&1)"
echo "$run_out" | grep -E '^\s*N:[0-9]+ E:[0-9]+ F:[0-9]+' || true

if echo "$run_out" | grep -qiE '[1-9][0-9]* unfreed memory blocks'; then
  echo "LEAK GUARD VIOLATION (AC #5): unfreed memory blocks detected:" >&2
  echo "$run_out" | grep -iE 'unfreed|heap dump|call trace' >&2
  exit 1
fi

echo "leak guard OK: 0 unfreed memory blocks (parser ownership intact)"
exit 0
