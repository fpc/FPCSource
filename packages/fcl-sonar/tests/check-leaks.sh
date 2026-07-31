#!/usr/bin/env bash
# Guard: build the test program with heaptrc (-gh) and fail on any unfreed block.
# Exit: 0 no leaks | 1 leaks reported | 2 the guard could not measure
set -uo pipefail

BV="$(dirname "$(readlink -f "$0")")/build-verify.sh"
if [ ! -r "$BV" ]; then
  echo "LEAK GUARD: cannot read $BV — it owns the unit paths this guard needs." >&2
  exit 2
fi
# shellcheck source=build-verify.sh
source "$BV" || { echo "LEAK GUARD: sourcing $BV failed" >&2; exit 2; }

require_array() {                              # require_array <array name>
  local n="$1" decl flags
  decl="$(declare -p "$n" 2>/dev/null)" || {
    echo "LEAK GUARD: $n is not defined after sourcing $BV." >&2
    echo "  A definition moved below the 'sourceable up to here' barrier; that" >&2
    echo "  barrier is a contract, not a comment." >&2
    exit 2
  }
  flags="${decl#declare -}"; flags="${flags%% *}"
  case "$flags" in
    *a*|*A*) ;;
    *) echo "LEAK GUARD: $n is not an array after sourcing $BV (declare -$flags)." >&2
       echo "  The barrier contract requires an array; a scalar would collapse to one word." >&2
       exit 2 ;;
  esac
  local -n ref="$n"                            # by name: ${!"#n[@]"} is not valid bash
  [ "${#ref[@]}" -gt 0 ] || {
    echo "LEAK GUARD: $n is empty after sourcing $BV" >&2
    exit 2
  }
}

[ -n "${TESTS_DIR:-}" ] || { echo "LEAK GUARD: TESTS_DIR unset after sourcing $BV" >&2; exit 2; }
# Non-empty but not a directory: the cd below would fail, the run would produce
# nothing, and check 1 would blame a lost -gh for a broken path.
[ -d "$TESTS_DIR" ] || { echo "LEAK GUARD: TESTS_DIR is not a directory: $TESTS_DIR" >&2; exit 2; }
require_array FPC_FLAGS
require_array SONAR_TEST_PATHS
require_array SONAR_SENTINELS
require_array SONAR_PASSRC_SENTINELS

HEAP_FLAGS=(-gh)

# heaptrc reads this at startup: log=<file> diverts the dump, disabled turns it
# off entirely. Either would look exactly like a lost -gh.
unset HEAPTRC

OUT="$(mktemp -d)" || { echo "LEAK GUARD: mktemp failed" >&2; exit 2; }

# A failing run's build.log and run.log are the evidence; deleting them leaves
# only whatever tail this script chose to print.
cleanup() {
  local rc=$? f
  if [ "$rc" -ne 0 ]; then
    echo "LEAK GUARD: evidence kept in $OUT:" >&2
    # Only what exists: a build-stage failure has no run log, and naming it sends
    # whoever reads this to a file that was never written.
    for f in build.log run.log; do
      [ -f "$OUT/$f" ] && echo "  $OUT/$f" >&2
    done
  else
    rm -rf "$OUT"
  fi
  return 0
}
trap cleanup EXIT

echo "leak guard: building fpsonar_tests with heaptrc (${HEAP_FLAGS[*]}) in $OUT"
fpc "${FPC_FLAGS[@]}" "${HEAP_FLAGS[@]}" -B "${SONAR_TEST_PATHS[@]}" \
  -FE"$OUT" -FU"$OUT" "$TESTS_DIR/fpsonar_tests.lpr" > "$OUT/build.log" 2>&1
if [ $? -ne 0 ] || [ ! -x "$OUT/fpsonar_tests" ]; then
  echo "LEAK GUARD: heaptrc build failed" >&2
  diag="$(grep -E 'Fatal:|Error:' "$OUT/build.log" | head -15)"
  if [ -n "$diag" ]; then printf '%s\n' "$diag" >&2; else tail -40 "$OUT/build.log" >&2; fi
  exit 2
fi

missing=()
for u in "${SONAR_SENTINELS[@]}" "${SONAR_PASSRC_SENTINELS[@]}"; do
  [ -f "$OUT/$u.ppu" ] || missing+=("$u")
done
if [ ${#missing[@]} -gt 0 ]; then
  echo "LEAK GUARD: stale units — ${missing[*]} were not compiled from the in-tree sources." >&2
  echo "Check the -Fu entries in ~/.fpc.cfg / /etc/fpc.cfg for a staged unit tree." >&2
  exit 3
fi

run_out="$(cd "$TESTS_DIR" && "$OUT/fpsonar_tests" --format=plain --all 2>&1)"
run_rc=$?
printf '%s\n' "$run_out" > "$OUT/run.log"

# summary_field <label> -> the number the suite reported for it.
summary_field() {
  sed -n "s/.*Number of $1:[[:space:]]*\([0-9][0-9]*\).*/\1/p" "$OUT/run.log" | tail -1
}

# 1) instrumentation present: heaptrc prints its dump header at program exit.
if ! grep -qi 'heap dump by heaptrc unit' "$OUT/run.log"; then
  echo "LEAK GUARD: no heaptrc dump in the run output — the binary was not instrumented." >&2
  echo "Nothing was measured, so this is a failure, not a clean heap. Is -gh still in HEAP_FLAGS?" >&2
  tail -5 "$OUT/run.log" >&2
  exit 1
fi

# 2) the suite actually ran something.
ran="$(summary_field 'run tests')"
if [ -z "$ran" ] || [ "$ran" -eq 0 ]; then
  echo "LEAK GUARD: the instrumented binary ran ${ran:-no} tests (exit $run_rc)." >&2
  echo "An empty run frees nothing and would report a clean heap for free." >&2
  tail -10 "$OUT/run.log" >&2
  exit 1
fi

# 3) the suite was green. A run that aborts early both allocates less and leaks
# less, so a red suite must not be allowed to certify the heap.
errs="$(summary_field errors)"
fails="$(summary_field failures)"
if [ "$run_rc" != "0" ] || [ "${errs:-1}" != "0" ] || [ "${fails:-1}" != "0" ]; then
  echo "LEAK GUARD: the instrumented suite was not green — $ran run, ${errs:-?} errors, ${fails:-?} failures (exit $run_rc)." >&2
  echo "A suite that stops early frees less, so its heap says nothing. Fix the suite first." >&2
  grep -E '^\s+(Message|Exception message):' "$OUT/run.log" | head -10 >&2
  exit 1
fi

mapfile -t unfreed_counts < <(grep -hoiE '(^|[[:space:]])[0-9]+ unfreed memory block' \
  "$OUT/run.log" | grep -oE '[0-9]+')
if [ ${#unfreed_counts[@]} -eq 0 ]; then
  echo "LEAK GUARD: heaptrc dumped but its unfreed-block line was not found." >&2
  grep -iE 'unfreed|heap dump' "$OUT/run.log" >&2
  exit 1
fi
unfreed=0
for n in "${unfreed_counts[@]}"; do [ "$n" -eq 0 ] || unfreed="$n"; done
if [ "$unfreed" -ne 0 ]; then
  echo "LEAK GUARD VIOLATION: $unfreed unfreed memory block(s) after $ran tests:" >&2
  grep -iE 'unfreed|heap dump|memory blocks|call trace' "$OUT/run.log" >&2
  sed -n '/Heap dump by heaptrc unit/,$p' "$OUT/run.log" | head -60 >&2
  exit 1
fi

echo "leak guard OK: heaptrc dump seen, $ran tests run green, 0 unfreed memory blocks"
exit 0
