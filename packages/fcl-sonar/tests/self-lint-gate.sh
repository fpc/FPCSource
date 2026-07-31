#!/usr/bin/env bash
# Gate: run fpsonar over its own sources and fail on any finding absent from the
# committed baseline. --refresh regenerates that baseline.
# Exit: 0 clean | 1 new finding | 2 the gate could not measure | 3 stale units
set -uo pipefail
export LC_ALL=C          # byte collation: rule ids and paths are compared with
                         # sort/comm, which must agree with the fixed-string
                         # lookups and with the CLI's own sort order

REFRESH=0
if [ $# -gt 1 ]; then
  echo "SELF-LINT GUARD: too many arguments: $*" >&2
  echo "  Only --refresh is accepted; anything else would be silently ignored." >&2
  exit 2
fi
case "${1:-}" in
  "")         ;;
  --refresh)  REFRESH=1 ;;
  # The header block is the help text; derived from it so the two cannot drift.
  -h|--help)  awk 'NR>1 && /^#/ { sub(/^# ?/, ""); print; next } NR>1 { exit }' "$0"
              exit 0 ;;
  *) echo "usage: $0 [--refresh]" >&2; exit 2 ;;
esac

BV="$(dirname "$(readlink -f "$0")")/build-verify.sh"
if [ ! -r "$BV" ]; then
  echo "SELF-LINT GUARD: cannot read $BV — it owns the layout this gate needs." >&2
  exit 2
fi
# It is both sourced and executed below. Checking only readability turns a
# missing +x into exit 126 from the build step, reported as a build failure.
if [ ! -x "$BV" ]; then
  echo "SELF-LINT GUARD: $BV is not executable; this gate also runs it (cli target)." >&2
  exit 2
fi
if [ -z "${HOME:-}" ] && [ -z "${FPSONAR_BUILD_DIR:-}" ]; then
  echo "SELF-LINT GUARD: HOME is unset and FPSONAR_BUILD_DIR is not set." >&2
  echo "  $BV derives its build dir from \$HOME; sourcing it under set -u would" >&2
  echo "  abort this shell with exit 1, which this gate's contract reads as a new" >&2
  echo "  finding. Set FPSONAR_BUILD_DIR to an absolute path." >&2
  exit 2
fi
# shellcheck source=build-verify.sh
source "$BV" || { echo "SELF-LINT GUARD: sourcing $BV failed" >&2; exit 2; }

require_array() {                              # require_array <array name>
  local n="$1" decl flags
  decl="$(declare -p "$n" 2>/dev/null)" || {
    echo "SELF-LINT GUARD: $n is not defined after sourcing $BV." >&2
    echo "  A definition moved below the 'sourceable up to here' barrier; that" >&2
    echo "  barrier is a contract, not a comment." >&2
    exit 2
  }
  flags="${decl#declare -}"; flags="${flags%% *}"
  case "$flags" in
    *a*|*A*) ;;
    *) echo "SELF-LINT GUARD: $n is not an array after sourcing $BV (declare -$flags)." >&2
       echo "  The barrier contract requires an array; a scalar would collapse to one word." >&2
       exit 2 ;;
  esac
  local -n ref="$n"                            # by name: ${!"#n[@]"} is not valid bash
  [ "${#ref[@]}" -gt 0 ] || {
    echo "SELF-LINT GUARD: $n is empty after sourcing $BV" >&2
    exit 2
  }
}

require_scalar() {                             # require_scalar <name> <what it is>
  local n="$1" what="$2"
  [ -n "${!n:-}" ] || {
    echo "SELF-LINT GUARD: $n unset after sourcing $BV (needed: $what)" >&2
    exit 2
  }
}

require_scalar ROOT "the FPC src root the corpus is staged from"
require_scalar TESTS_DIR "where the config and the committed baseline live"
require_scalar BUILD "where build-verify.sh puts the CLI it builds"
[ -d "$ROOT" ] || { echo "SELF-LINT GUARD: ROOT is not a directory: $ROOT" >&2; exit 2; }
[ -d "$TESTS_DIR" ] || { echo "SELF-LINT GUARD: TESTS_DIR is not a directory: $TESTS_DIR" >&2; exit 2; }
require_array PASSRC_PATHS
require_array JSON_PATHS
export FPSONAR_BUILD_DIR="$BUILD"

CONFIG="$TESTS_DIR/fpsonar.selflint.json"
BASELINE="$TESTS_DIR/fpsonar.selflint.baseline.json"
CANON="${FPSONAR_SELFLINT_CANON:-/tmp/fpsonar-selflint}"
while [ "$CANON" != "/" ] && [ "${CANON%/}" != "$CANON" ]; do CANON="${CANON%/}"; done
STAGE_MARKER=".fpsonar-selflint-stage"

SCOPE_ROOTS=(packages/fcl-sonar/src/base
             packages/fcl-sonar/src/rules
             packages/fcl-sonar/src/output
             utils/fpsonar)
# Never source: stale copies and build output. The CLI's directory walk is
# recursive and has no exclude flag, so pruning has to happen during staging.
PRUNE_NAMES=(backup lib units)
SRC_EXT=(-iname '*.pp' -o -iname '*.pas' -o -iname '*.lpr' -o -iname '*.inc')

#--- Evidence -----------------------------------------------------------------
EV="$(mktemp -d)" || { echo "SELF-LINT GUARD: mktemp failed" >&2; exit 2; }

cleanup() {
  local rc=$? f
  cd / || return 0                             # never rm -rf the cwd out from under us
  if [ "$rc" -ne 0 ]; then
    echo "SELF-LINT GUARD: evidence kept in $EV:" >&2
    # Only what exists: a staging failure has no analyze log, and naming it
    # sends whoever reads this to a file that was never written.
    for f in build.log registry.json analyze.out analyze.err outside.txt gate.out gate.err \
             gate-outside.txt baseline.json src-files.txt staged-files.txt by-rule.txt \
             find.err sibling-dirs.txt cli-dirs.txt stray.txt pkg-stray.txt \
             cfg-facts.txt cfg-rules.txt cfg-rules-all.txt cfg-enabled.txt \
             registry-rules.txt not-enabled.txt unknown-ids.txt dup-keys.txt \
             cfg-extra-keys.txt baseline-rules.txt firing-rules.txt silent-rules.txt \
             baseline-rule-counts.txt shrunk-rules.txt; do
      [ -f "$EV/$f" ] && echo "  $EV/$f" >&2
    done
    [ -d "$CANON" ] && echo "  staged corpus kept at $CANON" >&2
  else
    rm -rf "$EV"
    # Only a directory this run marked as its own staging area, for the same
    # reason the wipe below is guarded: $CANON comes from the environment.
    [ -f "$CANON/$STAGE_MARKER" ] && rm -rf "$CANON"
  fi
  return 0
}
trap cleanup EXIT
trap 'exit 143' TERM
trap 'exit 130' INT
trap 'exit 129' HUP

#--- The CLI ------------------------------------------------------------------
if [ -n "${FPSONAR_SELFLINT_CLI:-}" ]; then
  FPSONAR="$FPSONAR_SELFLINT_CLI"
  # -f as well as -x: a searchable DIRECTORY passes -x, and the failure then
  # surfaces four checks later as "init-config produced no registry listing".
  if [ ! -f "$FPSONAR" ] || [ ! -x "$FPSONAR" ]; then
    echo "SELF-LINT GUARD: FPSONAR_SELFLINT_CLI is not an executable file: $FPSONAR" >&2
    echo "  Unset it to let build-verify.sh build the CLI from the working copy." >&2
    exit 2
  fi
  echo "self-lint gate: using FPSONAR_SELFLINT_CLI=$FPSONAR (freshness is the caller's)"
else
  echo "self-lint gate: building the CLI via build-verify.sh cli"
  "$BV" -q cli > "$EV/build.log" 2>&1
  bv_rc=$?
  if [ "$bv_rc" = "3" ]; then
    echo "SELF-LINT GUARD: build-verify.sh reports stale units — the CLI is not the working copy's." >&2
    sed 's/^/        /' "$EV/build.log" >&2
    exit 3
  fi
  if [ "$bv_rc" != "0" ]; then
    echo "SELF-LINT GUARD: build-verify.sh cli failed (exit $bv_rc)." >&2
    sed 's/^/        /' "$EV/build.log" >&2
    exit 2
  fi
  # Not restated by hand: build-verify.sh's cli target puts it here, and its
  # assert_fresh has just certified it was linked from the in-tree sources.
  FPSONAR="$BUILD/cli/fpsonar"
  [ -x "$FPSONAR" ] || {
    echo "SELF-LINT GUARD: build-verify.sh reported success but $FPSONAR is not executable." >&2
    exit 2
  }
fi

#--- Stage the corpus into the canonical path ---------------------------------
# The path is wiped, so refuse anything that could be someone's working tree.
case "$CANON" in
  /*) ;;
  *) echo "SELF-LINT GUARD: FPSONAR_SELFLINT_CANON must be an absolute path: $CANON" >&2
     exit 2 ;;
esac
refuse_canon() {                               # refuse_canon <why>
  echo "SELF-LINT GUARD: refusing to wipe $CANON — $1." >&2
  echo "  Point FPSONAR_SELFLINT_CANON at a path this gate can own outright." >&2
  echo "  Note the committed baseline only matches findings staged under one fixed" >&2
  echo "  path, so changing it means refreshing the baseline too." >&2
  exit 2
}
[ -n "$CANON" ] && [ "$CANON" != "/" ] || refuse_canon "it is the filesystem root"
[ "$CANON" != "$ROOT" ] || refuse_canon "it is the working copy"
[ "$CANON" != "${HOME:-}" ] || refuse_canon "it is your home directory"
[ "$CANON" != "/tmp" ] && [ "$CANON" != "/var/tmp" ] || refuse_canon "it is a shared temp root"
case "$CANON" in
  "$ROOT"/*) refuse_canon "it is inside the working copy" ;;
esac
[ ! -L "$CANON" ] || refuse_canon "it is a symlink"
if [ -e "$CANON" ]; then
  [ -d "$CANON" ] || refuse_canon "it exists and is not a directory"
  [ ! -e "$CANON/.git" ] || refuse_canon "it holds a .git — that is a real tree"
  [ -f "$CANON/$STAGE_MARKER" ] ||
    refuse_canon "it has no $STAGE_MARKER, so this gate did not create it"
fi

if command -v flock > /dev/null 2>&1; then
  if [ -e "$CANON.lock" ] || [ -L "$CANON.lock" ]; then
    if [ -L "$CANON.lock" ] || [ ! -f "$CANON.lock" ]; then
      echo "SELF-LINT GUARD: $CANON.lock exists and is not a regular file." >&2
      echo "  It is opened for writing, so a symlink there redirects the truncation." >&2
      exit 2
    fi
    if [ ! -O "$CANON.lock" ]; then
      echo "SELF-LINT GUARD: $CANON.lock is not owned by this user." >&2
      echo "  Its path is predictable and its directory is shared, so another user's" >&2
      echo "  file there blocks every run. Remove it, or set FPSONAR_SELFLINT_CANON to" >&2
      echo "  a path you own and refresh a baseline for it." >&2
      exit 2
    fi
  fi
  if ! exec 9> "$CANON.lock"; then
    echo "SELF-LINT GUARD: cannot create the lock file $CANON.lock" >&2
    exit 2
  fi
  flock -n 9 || {
    echo "SELF-LINT GUARD: another run holds $CANON (lock: $CANON.lock)." >&2
    echo "  Runs cannot share the staging path; wait, or give this one its own" >&2
    echo "  FPSONAR_SELFLINT_CANON and refresh a baseline for it." >&2
    exit 2
  }
else
  # Not silent: the comment above states what two concurrent runs do to each
  # other, and without flock nothing prevents it.
  echo "self-lint gate: WARNING flock is not installed — runs sharing $CANON are not serialised."
fi

rm -rf "$CANON" || { echo "SELF-LINT GUARD: cannot clear $CANON" >&2; exit 2; }
mkdir -p "$(dirname "$CANON")" || {
  echo "SELF-LINT GUARD: cannot create the parent of $CANON" >&2; exit 2; }
mkdir "$CANON" || { echo "SELF-LINT GUARD: cannot create $CANON" >&2; exit 2; }
[ ! -L "$CANON" ] || {
  echo "SELF-LINT GUARD: $CANON became a symlink after it was created." >&2; exit 2; }
: > "$CANON/$STAGE_MARKER" || {
  echo "SELF-LINT GUARD: cannot write $CANON/$STAGE_MARKER" >&2; exit 2; }

prune=()
for n in "${PRUNE_NAMES[@]}"; do prune+=(-name "$n" -o); done

# sources_under <root> <outfile> -> its source files, paths relative to $ROOT,
sources_under() {
  ( cd "$ROOT" && find -L "$1" \( "${prune[@]}" -false \) -prune -o \
      -type f \( "${SRC_EXT[@]}" \) -print ) 2> "$EV/find.err" | sort > "$2"
  local st=("${PIPESTATUS[@]}")
  if [ "${st[0]}" != "0" ] || [ "${st[1]}" != "0" ] || [ -s "$EV/find.err" ]; then
    echo "SELF-LINT GUARD: walking $1 failed (find exit ${st[0]}, sort exit ${st[1]})." >&2
    [ -s "$EV/find.err" ] && sed 's/^/        /' "$EV/find.err" >&2
    echo "  Whatever it could not read would be silently dropped from the corpus." >&2
    exit 2
  fi
}

SONAR_SRC="packages/fcl-sonar/src"
SONAR_PKG="packages/fcl-sonar"
CLI_ROOT="utils"
PKG_EXEMPT=(fpmake.pp)
in_scope_roots() {                             # in_scope_roots <path relative to $ROOT>
  printf '%s\n' "${SCOPE_ROOTS[@]}" | grep -qxF "$1"
}
# walk_rel <outfile> <label> <find argument...> -> the walk's output relative to
walk_rel() {
  local out="$1" label="$2"; shift 2
  ( cd "$ROOT" && find -L "$@" ) 2> "$EV/find.err" | sort > "$out"
  local st=("${PIPESTATUS[@]}")
  if [ "${st[0]}" != "0" ] || [ "${st[1]}" != "0" ] || [ -s "$EV/find.err" ]; then
    echo "SELF-LINT GUARD: the $label walk failed (find exit ${st[0]}, sort exit ${st[1]})." >&2
    [ -s "$EV/find.err" ] && sed 's/^/        /' "$EV/find.err" >&2
    echo "  Whatever it could not read would be missing from its result, and an empty" >&2
    echo "  result is what a fully covered tree looks like to the check that reads it." >&2
    exit 2
  fi
}
uncovered=()
walk_rel "$EV/sibling-dirs.txt" "src sibling directory" \
  "$SONAR_SRC" -mindepth 1 -maxdepth 1 -type d
mapfile -t sibling_dirs < "$EV/sibling-dirs.txt"
for d in "${sibling_dirs[@]}"; do
  printf '%s\n' "${PRUNE_NAMES[@]}" | grep -qxF "$(basename "$d")" && continue
  in_scope_roots "$d" || uncovered+=("$d")
done
# A second fpsonar tool tree beside utils/fpsonar is just as invisible.
walk_rel "$EV/cli-dirs.txt" "utils/fpsonar* directory" \
  "$CLI_ROOT" -mindepth 1 -maxdepth 1 -type d -name 'fpsonar*'
mapfile -t cli_dirs < "$EV/cli-dirs.txt"
for d in "${cli_dirs[@]}"; do
  in_scope_roots "$d" || uncovered+=("$d")
done
if [ "${#uncovered[@]}" -gt 0 ]; then
  echo "SELF-LINT GUARD: shipped source director(y/ies) not covered by SCOPE_ROOTS:" >&2
  printf '        %s\n' "${uncovered[@]}" >&2
  echo "  Nothing there would ever be linted, and every count below would agree that" >&2
  echo "  the corpus is complete. Add them here and to check-chokepoint.sh together." >&2
  exit 2
fi
# A unit dropped directly into src/ belongs to no classified root.
walk_rel "$EV/stray.txt" "stray unit under $SONAR_SRC" \
  "$SONAR_SRC" -maxdepth 1 -type f \( "${SRC_EXT[@]}" \)
mapfile -t stray < "$EV/stray.txt"
if [ "${#stray[@]}" -gt 0 ]; then
  echo "SELF-LINT GUARD: unit(s) directly under $SONAR_SRC/, outside every scope root:" >&2
  printf '        %s\n' "${stray[@]}" >&2
  echo "  Move them into base/, rules/ or output/, or add their directory to SCOPE_ROOTS." >&2
  exit 2
fi
walk_rel "$EV/pkg-stray.txt" "stray unit under $SONAR_PKG" \
  "$SONAR_PKG" -maxdepth 1 -type f \( "${SRC_EXT[@]}" \)
pkg_stray=()
while IFS= read -r f; do
  printf '%s\n' "${PKG_EXEMPT[@]}" | grep -qxF "$(basename "$f")" && continue
  pkg_stray+=("$f")
done < "$EV/pkg-stray.txt"
if [ "${#pkg_stray[@]}" -gt 0 ]; then
  echo "SELF-LINT GUARD: unit(s) directly under $SONAR_PKG/, outside every scope root:" >&2
  printf '        %s\n' "${pkg_stray[@]}" >&2
  echo "  Only ${PKG_EXEMPT[*]} is exempt there (a build description, not analyzer code)." >&2
  echo "  Move them under src/, or add their directory to SCOPE_ROOTS and to" >&2
  echo "  check-chokepoint.sh together." >&2
  exit 2
fi

# Per root, not just in aggregate: a root that moved away leaves everything it
# held unanalyzed while the total stays comfortably non-zero.
: > "$EV/src-files.txt"
for r in "${SCOPE_ROOTS[@]}"; do
  if [ ! -d "$ROOT/$r" ]; then
    echo "SELF-LINT GUARD: scope root missing under $ROOT: $r" >&2
    echo "  The corpus would be smaller than the tree. Update SCOPE_ROOTS in $(basename "$0")" >&2
    echo "  and in check-chokepoint.sh together — the two guards share this corpus." >&2
    exit 2
  fi
  sources_under "$r" "$EV/root-files.txt"
  mapfile -t part < "$EV/root-files.txt"
  if [ "${#part[@]}" -eq 0 ]; then
    echo "SELF-LINT GUARD: scope root holds no *.pp/*.pas/*.lpr/*.inc: $r" >&2
    echo "  Whatever belongs there would never be linted, so this is a failure." >&2
    exit 2
  fi
  root_targets="$(grep -ciE '\.(pp|pas|lpr)$' "$EV/root-files.txt")"
  if [ "$root_targets" -eq 0 ]; then
    echo "SELF-LINT GUARD: scope root holds ${#part[@]} file(s) but no analysis target: $r" >&2
    echo "  Only .pp/.pas/.lpr are analyzed; an .inc-only root is never opened." >&2
    exit 2
  fi
  printf '%s\n' "${part[@]}" >> "$EV/src-files.txt"
  echo "self-lint gate: $r — ${#part[@]} source file(s), $root_targets analysis target(s)"
done
sort -o "$EV/src-files.txt" "$EV/src-files.txt"

while IFS= read -r rel; do
  mkdir -p "$CANON/$(dirname "$rel")" || {
    echo "SELF-LINT GUARD: cannot create $CANON/$(dirname "$rel")" >&2; exit 2; }
  cp -p "$ROOT/$rel" "$CANON/$rel" || {
    echo "SELF-LINT GUARD: copying $rel into $CANON failed" >&2; exit 2; }
done < "$EV/src-files.txt"

( cd "$CANON" && find -L . -type f ! -name "$STAGE_MARKER" -printf '%P\n' ) |
  sort > "$EV/staged-files.txt"
if ! diff -q "$EV/src-files.txt" "$EV/staged-files.txt" > /dev/null; then
  echo "SELF-LINT GUARD: the staged corpus does not match the source corpus." >&2
  echo "  < only in $ROOT, > only in $CANON:" >&2
  diff "$EV/src-files.txt" "$EV/staged-files.txt" | sed 's/^/        /' >&2
  exit 2
fi
# Staged copies must match in content: a truncated one analyzes clean.
truncated=()
while IFS= read -r rel; do
  cmp -s "$ROOT/$rel" "$CANON/$rel" || truncated+=("$rel")
done < "$EV/src-files.txt"
if [ "${#truncated[@]}" -gt 0 ]; then
  echo "SELF-LINT GUARD: staged copy differs from the source for:" >&2
  printf '        %s\n' "${truncated[@]}" >&2
  exit 2
fi

# The analysis targets: the CLI's directory walk takes .pas/.pp/.lpr and skips
# .inc, so this is the number its summary line must report back.
EXPECTED_TARGETS="$(grep -ciE '\.(pp|pas|lpr)$' "$EV/src-files.txt")"
STAGED_FILES="$(wc -l < "$EV/src-files.txt")"
[ "$EXPECTED_TARGETS" -gt 0 ] || {
  echo "SELF-LINT GUARD: no .pp/.pas/.lpr among the $STAGED_FILES staged file(s) — nothing to analyze." >&2
  exit 2
}
echo "self-lint gate: staged $STAGED_FILES file(s) ($EXPECTED_TARGETS analysis target(s)) -> $CANON"

#--- The config: resolution on, and every registered rule enabled -------------
[ -r "$CONFIG" ] || {
  echo "SELF-LINT GUARD: cannot read the self-lint config: $CONFIG" >&2
  echo "  Without it the run would fall back to the shipped defaults, which have" >&2
  echo "  resolution off and their own gate thresholds." >&2
  exit 2
}

# json_facts <file> -> the facts this gate needs from a config-shaped JSON:
json_facts() {
  awk '
  { all = all $0 "\n" }
  END {
    n = length(all); i = 1; ntok = 0
    while (i <= n) {
      c = substr(all, i, 1)
      if (c == "\"") {
        j = i + 1; v = ""
        while (j <= n) {
          d = substr(all, j, 1)
          if (d == "\\") {
            e = substr(all, j + 1, 1)
            if (e == "u") badu = 1
            v = v e; j += 2; continue
          }
          if (d == "\"") break
          v = v d; j++
        }
        ntok++; tt[ntok] = "S"; tv[ntok] = v; i = j + 1; continue
      }
      if (c == "{" || c == "}" || c == "[" || c == "]" || c == ":" || c == ",") {
        ntok++; tt[ntok] = "P"; tv[ntok] = c; i++; continue
      }
      if (c == " " || c == "\t" || c == "\n" || c == "\r") { i++; continue }
      j = i; v = ""                            # a bare literal: true/false/null/number
      while (j <= n && substr(all, j, 1) ~ /[A-Za-z0-9_.+-]/) { v = v substr(all, j, 1); j++ }
      if (v == "") { i++; continue }           # anything else cannot start a value
      ntok++; tt[ntok] = "W"; tv[ntok] = v; i = j
    }
    depth = 0
    for (k = 1; k <= ntok; k++) {
      if (tt[k] == "P") {
        if (tv[k] == "{" || tv[k] == "[") {
          depth++; key[depth] = ""
          if (tv[k] == "{" && depth == 3 && key[1] == "suppressions") nsupp++
          continue
        }
        if (tv[k] == "}" || tv[k] == "]") { key[depth] = ""; if (depth > 0) depth--; continue }
        continue
      }
      if (tt[k] != "S") continue
      if (tt[k+1] != "P" || tv[k+1] != ":") continue
      key[depth] = tv[k]                       # depth 1 is the root object body
      if (depth == 2 && key[1] == "rules")
        print "R " tv[k]
      else if (depth == 3 && key[1] == "rules" && tv[k] == "enabled" && tt[k+2] == "W")
        print "E " key[2] " " tv[k+2]
      else if (depth == 3 && key[1] == "rules")
        print "X " key[2] " " tv[k]
      else if (depth == 2 && key[1] == "useTier" && tv[k] == "resolution" && tt[k+2] == "S")
        print "U " tv[k+2]
      else if (depth == 2 && key[1] == "gate" && tt[k+2] == "W")
        print "G " tv[k] " " tv[k+2]
      if (depth <= 3 && key[1] != "suppressions") {
        p = key[1]
        for (dd = 2; dd <= depth; dd++) p = p "/" key[dd]
        print "K " p
      }
    }
    if (nsupp > 0) print "P " nsupp
    if (badu) print "!"
  }' "$1"
}

blind=0                    # exit 2: the gate cannot measure
viol=0                     # exit 1: the policed thing is broken
# Both are accumulated so one run reports every problem; the verdict is taken
# once, below, with "cannot measure" winning over "is broken".

if ! json_facts "$CONFIG" > "$EV/cfg-facts.txt" || [ ! -s "$EV/cfg-facts.txt" ]; then
  echo "SELF-LINT GUARD: no rules/useTier facts parsed out of $CONFIG." >&2
  echo "  Either the file is not the expected config shape, or json_facts is broken;" >&2
  echo "  in both cases the enable-all check below would pass vacuously." >&2
  exit 2
fi
if grep -qx '!' "$EV/cfg-facts.txt"; then
  echo "SELF-LINT GUARD: $CONFIG uses a \\uXXXX escape, which this gate cannot decode." >&2
  echo "  fpjson would read a different string than the checks below, so a correctly" >&2
  echo "  enabled rule could be reported as both not-enabled and unknown. Write rule" >&2
  echo "  ids and values as plain ASCII." >&2
  exit 2
fi
sed -n 's/^R //p' "$EV/cfg-facts.txt" | sort > "$EV/cfg-rules-all.txt"
sort -u "$EV/cfg-rules-all.txt" > "$EV/cfg-rules.txt"
# `\(.*\)`, not `\([^ ]*\)`: an id holding a space would silently drop out of the
# enabled set and be reported as a violation of a rule that is in fact enabled.
sed -n 's/^E \(.*\) true$/\1/p' "$EV/cfg-facts.txt" | sort -u > "$EV/cfg-enabled.txt"
mapfile -t cfg_res < <(sed -n 's/^U //p' "$EV/cfg-facts.txt")
mapfile -t cfg_supp < <(sed -n 's/^P //p' "$EV/cfg-facts.txt")

mapfile -t dup_ids < <(uniq -d "$EV/cfg-rules-all.txt")
if [ "${#dup_ids[@]}" -gt 0 ]; then
  echo "SELF-LINT GUARD: ${#dup_ids[@]} rule id(s) listed twice in $CONFIG:" >&2
  printf '        %s\n' "${dup_ids[@]}" >&2
  echo "  fpjson rejects a duplicate object member, so the CLI would refuse the" >&2
  echo "  entire config and the failure would be blamed on the baseline." >&2
  exit 2
fi

sed -n 's/^K //p' "$EV/cfg-facts.txt" | sort | uniq -d > "$EV/dup-keys.txt"
if [ -s "$EV/dup-keys.txt" ]; then
  echo "SELF-LINT GUARD: duplicate JSON member(s) in $CONFIG:" >&2
  sed 's/^/        /' "$EV/dup-keys.txt" >&2
  exit 2
fi

sed -n 's/^X //p' "$EV/cfg-facts.txt" | sort > "$EV/cfg-extra-keys.txt"
if [ -s "$EV/cfg-extra-keys.txt" ]; then
  echo "SELF-LINT GUARD: rule object(s) in $CONFIG carry keys other than \"enabled\":" >&2
  sed 's/^/        /' "$EV/cfg-extra-keys.txt" >&2
  echo "  A parameter override can silence a rule without disabling it, so the" >&2
  echo "  enable-all check would pass while the rule contributes nothing. Each rule" >&2
  echo "  must keep its registered defaults: { \"enabled\": true } and nothing else." >&2
  exit 2
fi

if [ "${#cfg_supp[@]}" -ne 0 ]; then
  echo "SELF-LINT GUARD: $CONFIG carries ${cfg_supp[0]} suppression entr(y/ies)." >&2
  echo "  Suppressed findings never reach the report, so they are invisible to the" >&2
  echo "  new-code comparison — the self-lint config must keep \"suppressions\": []." >&2
  echo "  Grandfathering belongs in the baseline, where it is visible and diffable." >&2
  blind=1
fi

if [ "${#cfg_res[@]}" -ne 1 ] || [ "${cfg_res[0]:-}" != "prefer" ]; then
  echo "SELF-LINT GUARD: $CONFIG must set useTier.resolution to \"prefer\"" >&2
  echo "  (found: ${cfg_res[*]:-<absent>})." >&2
  echo "  Fix: \"useTier\": { \"resolution\": \"prefer\" }" >&2
  blind=1
fi

GATE_KEYS=(maxBlocker maxCritical maxMajor maxMinor maxInfo maxTotal)
bad_gate=()
for g in "${GATE_KEYS[@]}"; do
  mapfile -t gv < <(sed -n "s/^G $g //p" "$EV/cfg-facts.txt")
  [ "${#gv[@]}" -eq 1 ] && [ "${gv[0]:-}" = "-1" ] || bad_gate+=("$g = ${gv[*]:-<absent>}")
done
if [ "${#bad_gate[@]}" -gt 0 ]; then
  echo "SELF-LINT GUARD: every gate threshold in $CONFIG must be -1 (unlimited):" >&2
  printf '        %s\n' "${bad_gate[@]}" >&2
  echo "  With a threshold that can trip, this script's own new-finding count stops" >&2
  echo "  being the verdict and a clean tree exits 1. Fix: \"gate\": { \"maxBlocker\": -1," >&2
  echo "  \"maxCritical\": -1, \"maxMajor\": -1, \"maxMinor\": -1, \"maxInfo\": -1, \"maxTotal\": -1 }" >&2
  blind=1
fi

# The registry, from the linked CLI: init-config enumerates every registered
# rule and is the only way to do so — there is no --list-rules.
if ! "$FPSONAR" init-config -o "$EV/registry.json" || [ ! -s "$EV/registry.json" ]; then
  echo "SELF-LINT GUARD: '$FPSONAR init-config' produced no registry listing." >&2
  exit 2
fi
json_facts "$EV/registry.json" | sed -n 's/^R //p' | sort -u > "$EV/registry-rules.txt"
REGISTRY_RULES="$(wc -l < "$EV/registry-rules.txt")"
if [ "$REGISTRY_RULES" -eq 0 ]; then
  echo "SELF-LINT GUARD: the CLI's init-config listed no rules at all." >&2
  echo "  Nothing could be compared, so the enable-all check would pass vacuously." >&2
  exit 2
fi

comm -23 "$EV/registry-rules.txt" "$EV/cfg-enabled.txt" > "$EV/not-enabled.txt" || {
  echo "SELF-LINT GUARD: comparing the registry against $CONFIG failed (comm)." >&2
  exit 2
}
mapfile -t not_enabled < "$EV/not-enabled.txt"
if [ "${#not_enabled[@]}" -gt 0 ]; then
  echo "SELF-LINT VIOLATION: ${#not_enabled[@]} registered rule(s) not enabled in $CONFIG:" >&2
  printf '        %s\n' "${not_enabled[@]}" >&2
  echo "  Add to the \"rules\" object (the loader silently ignores an unknown id," >&2
  echo "  so the id must match the registry exactly, case included):" >&2
  printf '        "%s": { "enabled": true },\n' "${not_enabled[@]}" >&2
  viol=1
fi
# The other half of the same hazard: an id the registry does not know is silently
# ignored by the loader, so a typo or a rename leaves the rule at its default.
comm -13 "$EV/registry-rules.txt" "$EV/cfg-rules.txt" > "$EV/unknown-ids.txt" || {
  echo "SELF-LINT GUARD: comparing $CONFIG against the registry failed (comm)." >&2
  exit 2
}
mapfile -t unknown_ids < "$EV/unknown-ids.txt"
if [ "${#unknown_ids[@]}" -gt 0 ]; then
  echo "SELF-LINT VIOLATION: ${#unknown_ids[@]} id(s) in $CONFIG are not in the registry:" >&2
  printf '        %s\n' "${unknown_ids[@]}" >&2
  echo "  The config loader ignores an unknown rule id without complaining, so each" >&2
  echo "  of these configures nothing. Delete it, or fix its spelling/case." >&2
  viol=1
fi

if [ "$blind" = "1" ]; then exit 2; fi
if [ "$viol" = "1" ]; then exit 1; fi
echo "self-lint gate: $REGISTRY_RULES registered rule(s), all enabled, resolution prefer"

#--- Run ----------------------------------------------------------------------
cd "$CANON" || { echo "SELF-LINT GUARD: cannot cd into $CANON" >&2; exit 2; }

ARGS=(--config "$CONFIG" --synthetic-only
      --mode OBJFPC --cpu x86_64 --os linux)
for r in "${SCOPE_ROOTS[@]}"; do ARGS+=(-Fu"$CANON/$r" -Fi"$CANON/$r"); done
ARGS+=("${PASSRC_PATHS[@]}" "${JSON_PATHS[@]}")

missing_paths=()
for a in "${ARGS[@]}"; do
  case "$a" in
    -Fu*|-Fi*) [ -d "${a#-F?}" ] || missing_paths+=("$a") ;;
  esac
done
if [ "${#missing_paths[@]}" -gt 0 ]; then
  echo "SELF-LINT GUARD: ${#missing_paths[@]} search path(s) handed to the analyzer do not exist:" >&2
  printf '        %s\n' "${missing_paths[@]}" >&2
  echo "  Findings would silently disappear rather than the run failing. The vendored" >&2
  echo "  paths come from $BV; the staged ones from SCOPE_ROOTS." >&2
  exit 2
fi
# Not named TARGETS: build-verify.sh uses that name below its barrier, and a
# future move of the declaration upward would silently merge the two lists.
SCOPE_TARGETS=()
for r in "${SCOPE_ROOTS[@]}"; do SCOPE_TARGETS+=("$CANON/$r"); done

# check_summary <analyze stdout file> <label> -> sets SUM_ISSUES to the CLI's own
assert_no_crash() {
  local label="$1"; shift
  grep -qE 'An unhandled exception occurred|Runtime error [0-9]+|^Exception ' "$@" || return 0
  echo "SELF-LINT GUARD: the $label run died from an unhandled exception (and still exited 0)." >&2
  head -q -n 40 "$@" | sed 's/^/        /' >&2
  echo "  Whatever it had not yet reported is missing from the run, so neither the" >&2
  echo "  finding total nor a clean new-code verdict means anything." >&2
  exit 2
}

SUM_ISSUES=""
check_summary() {
  local f="$1" label="$2" s files
  SUM_ISSUES=""
  s="$(sed -n 's/^Analyzed \([0-9][0-9]*\) file(s): \([0-9][0-9]*\) issue(s)\.$/\1 \2/p' "$f" | tail -1)"
  if [ -z "$s" ]; then
    echo "SELF-LINT GUARD: no 'Analyzed N file(s): M issue(s).' line from the $label run." >&2
    echo "  A zero exit without that line means the CLI printed help, or swallowed an" >&2
    echo "  exception — not that there were no findings." >&2
    tail -5 "$f" >&2
    exit 2
  fi
  files="${s%% *}"
  if [ "$files" != "$EXPECTED_TARGETS" ]; then
    echo "SELF-LINT GUARD: the $label run took $files file(s) as targets, not the $EXPECTED_TARGETS staged." >&2
    echo "  The CLI's walk and this script's disagree, so part of the corpus is unlinted." >&2
    exit 2
  fi
  SUM_ISSUES="${s##* }"
}

# report_lines <analyze stdout file> -> the issue lines only, with the $CANON
report_lines() {
  CANON_PREFIX="$CANON/" awk 'index($0, ENVIRON["CANON_PREFIX"]) == 1 {
    print substr($0, length(ENVIRON["CANON_PREFIX"]) + 1) }' "$1"
}

# other_lines <analyze stdout file> -> everything that is neither an issue line
other_lines() {
  CANON_PREFIX="$CANON/" awk '
    index($0, ENVIRON["CANON_PREFIX"]) == 1                  { next }
    /^Fp-Sonar /                                             { next }
    /^[0-9]+ issue\(s\)\.$/                                  { next }
    /^Analyzed [0-9]+ file\(s\): [0-9]+ issue\(s\)\.$/       { next }
    $0 == ""                                                 { next }
    { print }' "$1"
}

# count_fingerprints <baseline file> -> how many fingerprint entries it holds.
count_fingerprints() {
  grep -o '"fingerprint"' "$1" | wc -l
}

"$FPSONAR" analyze "${ARGS[@]}" "${SCOPE_TARGETS[@]}" > "$EV/analyze.out" 2> "$EV/analyze.err"
rc=$?
[ -s "$EV/analyze.err" ] && sed 's/^/        /' "$EV/analyze.err" >&2
if [ "$rc" = "2" ]; then
  echo "SELF-LINT GUARD: the full analyze run failed with a usage/IO error (exit 2)." >&2
  exit 2
fi
other_lines "$EV/analyze.out" > "$EV/outside.txt"
assert_no_crash "full analyze" "$EV/analyze.err" "$EV/outside.txt"
check_summary "$EV/analyze.out" "full analyze"
TOTAL="$SUM_ISSUES"
if [ "$rc" != "0" ]; then
  echo "SELF-LINT GUARD: the full analyze run exited $rc with a summary line present." >&2
  echo "  The config's gate thresholds should all be -1, so nothing should make this" >&2
  echo "  run non-zero; the reported counts cannot be trusted. Check $CONFIG." >&2
  exit 2
fi

outside="$(wc -l < "$EV/outside.txt")"
if [ "$outside" != "0" ]; then
  echo "SELF-LINT GUARD: $outside report line(s) name a file outside $CANON." >&2
  head -10 "$EV/outside.txt" | sed 's/^/        /' >&2
  echo "  Their fingerprints would bake in this checkout's path, so the committed" >&2
  echo "  baseline would only ever match on this machine." >&2
  exit 2
fi

awk 'match($0, /: (info|minor|major|critical|blocker) [A-Za-z0-9_]+: /) {
       s = substr($0, RSTART + 2)                # past ": "
       sub(/^[a-z]+ /, "", s)                    # past the severity
       sub(/:.*$/, "", s)                        # the rule id alone
       print s }' "$EV/analyze.out" | sort | uniq -c | sort -rn > "$EV/by-rule.txt"
BY_RULE_SUM="$(awk '{s+=$1} END {print s+0}' "$EV/by-rule.txt")"
if [ "$BY_RULE_SUM" != "$TOTAL" ]; then
  echo "SELF-LINT GUARD: the per-rule breakdown accounts for $BY_RULE_SUM of $TOTAL finding(s)." >&2
  echo "  The report format changed, so neither the size of this grandfathering event" >&2
  echo "  nor the per-rule numbers below can be trusted." >&2
  exit 2
fi
RULES_FIRING="$(wc -l < "$EV/by-rule.txt")"
echo "self-lint gate: $TOTAL finding(s) over $EXPECTED_TARGETS file(s), $RULES_FIRING distinct rule(s) firing"
echo "self-lint gate: analyzer diagnostics among them:"
for p in ParseError ResolveError ScanError RuleError FileNotFound; do
  printf '        %-14s %s\n' "$p" "$(grep -c ": [a-z]* $p:" "$EV/analyze.out")"
done
if [ "$REFRESH" = "1" ]; then
  echo "self-lint gate: top rules by count:"
  head -15 "$EV/by-rule.txt" | sed 's/^/        /'
fi

# --- Refresh: generate the candidate baseline, but do not install it yet ------
if [ "$REFRESH" = "1" ]; then
  echo "===== self-lint gate: REFRESH ====="
  if [ -s "$BASELINE" ] && ! grep -qF "\"$CANON/" "$BASELINE"; then
    echo "SELF-LINT GUARD: refusing to refresh while staging into $CANON." >&2
    echo "  $BASELINE was taken under a different path, so this would replace a" >&2
    echo "  portable baseline with one that matches only on this host." >&2
    echo "  Unset FPSONAR_SELFLINT_CANON; if the path change is deliberate, delete" >&2
    echo "  $BASELINE first." >&2
    exit 2
  fi
  # The baseline subcommand's exit code is not assigned on this path, so the file
  # is the only thing worth checking.
  "$FPSONAR" baseline "${ARGS[@]}" "${SCOPE_TARGETS[@]}" -o "$EV/baseline.json" 2>&1 |
    sed 's/^/        /'
  if [ ! -s "$EV/baseline.json" ] || ! grep -q '"_fpsonar"' "$EV/baseline.json"; then
    echo "SELF-LINT GUARD: 'baseline -o' did not write a baseline document." >&2
    exit 2
  fi
  FPS="$(count_fingerprints "$EV/baseline.json")"
  if [ "$FPS" -eq 0 ]; then
    echo "SELF-LINT GUARD: the generated baseline holds no fingerprints, but the analyze" >&2
    echo "  run found $TOTAL finding(s). Committing it would grandfather nothing." >&2
    exit 2
  fi
  if [ -s "$BASELINE" ]; then
    OLD_FPS="$(count_fingerprints "$BASELINE")"
    echo "self-lint gate: candidate baseline $FPS fingerprint(s), committed one $OLD_FPS"
    if [ "$OLD_FPS" -gt 0 ] && [ "$((FPS * 2))" -lt "$OLD_FPS" ]; then
      echo "SELF-LINT GUARD: the candidate grandfathers $FPS fingerprint(s), less than half" >&2
      echo "  the $OLD_FPS in $BASELINE. Installing it would silently shrink what the gate" >&2
      echo "  measures forever after. The candidate was NOT installed." >&2
      echo "  If a cleanup this large is real, delete $BASELINE first and refresh again —" >&2
      echo "  deleting it is the deliberate act this check exists to require." >&2
      exit 2
    fi
  fi
  GATE_BASELINE="$EV/baseline.json"
else
  GATE_BASELINE="$BASELINE"
fi

# --- The gate proper: only findings absent from the baseline ------------------
if [ ! -s "$GATE_BASELINE" ]; then
  echo "SELF-LINT GUARD: no committed baseline at $BASELINE." >&2
  echo "  Run '$(basename "$0") --refresh' and commit the file it writes; without a" >&2
  echo "  baseline every pre-existing finding would read as new." >&2
  exit 2
fi
if ! grep -q '"_fpsonar"' "$GATE_BASELINE"; then
  echo "SELF-LINT GUARD: $GATE_BASELINE is not a baseline document (no _fpsonar stamp)." >&2
  exit 2
fi
BASE_FPS="$(count_fingerprints "$GATE_BASELINE")"
if [ "$BASE_FPS" -eq 0 ]; then
  echo "SELF-LINT GUARD: $GATE_BASELINE holds no fingerprints." >&2
  echo "  Every pre-existing finding would read as new. Regenerate it with --refresh." >&2
  exit 2
fi

CANON_MISMATCH=0
if ! grep -qF "\"$CANON/" "$GATE_BASELINE"; then
  BASE_PREFIX="$(grep -o '"file"[[:space:]]*:[[:space:]]*"[^"]*"' "$GATE_BASELINE" |
    head -1 | sed 's/.*"\(.*\)"$/\1/')"
  echo "self-lint gate: WARNING no path in $GATE_BASELINE starts with $CANON/" >&2
  echo "  (it holds e.g. $BASE_PREFIX) — expect every finding to read as new." >&2
  CANON_MISMATCH=1
fi

grep -o '"ruleId"[[:space:]]*:[[:space:]]*"[^"]*"' "$GATE_BASELINE" |
  sed 's/.*"\(.*\)"$/\1/' | sort -u > "$EV/baseline-rules.txt"
if [ ! -s "$EV/baseline-rules.txt" ]; then
  echo "SELF-LINT GUARD: $GATE_BASELINE records no ruleId at all." >&2
  echo "  It is not the document fpsonar.baseline.pp writes, so the coverage check" >&2
  echo "  below would pass vacuously." >&2
  exit 2
fi
awk '{print $2}' "$EV/by-rule.txt" | sort -u > "$EV/firing-rules.txt"
comm -23 "$EV/baseline-rules.txt" "$EV/firing-rules.txt" > "$EV/silent-rules.txt" || {
  echo "SELF-LINT GUARD: comparing the baseline's rules against this run's failed (comm)." >&2
  exit 2
}
if [ -s "$EV/silent-rules.txt" ]; then
  echo "SELF-LINT GUARD: $(wc -l < "$EV/silent-rules.txt") rule(s) in $GATE_BASELINE produced" >&2
  echo "  nothing in this run, so new findings of theirs could not be detected:" >&2
  sed 's/^/        /' "$EV/silent-rules.txt" >&2
  echo "  Either the rule stopped working, or every finding it had was genuinely fixed." >&2
  echo "  The second case is real progress and needs a baseline refresh at the epic" >&2
  echo "  boundary; the first is a regression. This gate cannot tell them apart." >&2
  exit 2
fi

if [ -s "$BASELINE" ]; then
  if ! grep -o '"ruleId"[[:space:]]*:[[:space:]]*"[^"]*"' "$BASELINE" |
       sed 's/.*"\(.*\)"$/\1/' | sort | uniq -c > "$EV/baseline-rule-counts.txt"; then
    echo "SELF-LINT GUARD: $BASELINE records no ruleId at all, so per-rule loss cannot" >&2
    echo "  be measured. It is not the document fpsonar.baseline.pp writes." >&2
    exit 2
  fi
  if ! awk 'NR == FNR { want[$2] = $1; next }
            { have[$2] = $1 }
            END { for (r in want)
                    if ((have[r] + 0) * 2 < want[r])
                      printf "%-32s %5d in the baseline, %5d now\n", r, want[r], have[r] + 0 }' \
         "$EV/baseline-rule-counts.txt" "$EV/by-rule.txt" | sort > "$EV/shrunk-rules.txt"; then
    echo "SELF-LINT GUARD: comparing per-rule counts against $BASELINE failed." >&2
    exit 2
  fi
  if [ -s "$EV/shrunk-rules.txt" ]; then
    echo "SELF-LINT GUARD: $(wc -l < "$EV/shrunk-rules.txt") rule(s) produced less than half" >&2
    echo "  of what $BASELINE records for them:" >&2
    sed 's/^/        /' "$EV/shrunk-rules.txt" >&2
    echo "  --new-code cannot see findings that went away, so this is the only signal" >&2
    echo "  that a rule stopped measuring. Either it is broken, or a cleanup that large" >&2
    echo "  is real — in which case delete $BASELINE and refresh, which is the" >&2
    echo "  deliberate act this check exists to require." >&2
    exit 2
  fi
fi

if [ "$TOTAL" -lt "$BASE_FPS" ]; then
  echo "SELF-LINT GUARD: only $TOTAL finding(s), fewer than the $BASE_FPS fingerprint(s)" >&2
  echo "  the baseline grandfathers. Findings have disappeared wholesale, so a clean" >&2
  echo "  new-code verdict would be meaningless. Either the analyzer is measuring less" >&2
  echo "  than it did (search paths, config, scope), or a very large cleanup landed and" >&2
  echo "  the baseline needs refreshing at the next epic boundary." >&2
  exit 2
fi

"$FPSONAR" analyze --new-code "$GATE_BASELINE" "${ARGS[@]}" "${SCOPE_TARGETS[@]}" \
  > "$EV/gate.out" 2> "$EV/gate.err"
rc=$?
[ -s "$EV/gate.err" ] && sed 's/^/        /' "$EV/gate.err" >&2
if [ "$rc" = "2" ]; then
  echo "SELF-LINT GUARD: the gate run failed with a usage/IO error (exit 2) — an" >&2
  echo "  unloadable or malformed baseline, or a bad argument." >&2
  exit 2
fi
other_lines "$EV/gate.out" > "$EV/gate-outside.txt"
assert_no_crash "gate" "$EV/gate.err" "$EV/gate-outside.txt"
check_summary "$EV/gate.out" "gate"
NEW="$SUM_ISSUES"
gate_outside="$(wc -l < "$EV/gate-outside.txt")"
if [ "$gate_outside" != "0" ]; then
  echo "SELF-LINT GUARD: $gate_outside line(s) of the gate run's report are not findings" >&2
  echo "  under $CANON:" >&2
  head -10 "$EV/gate-outside.txt" | sed 's/^/        /' >&2
  echo "  A finding outside the staging dir bakes this checkout's path into its" >&2
  echo "  fingerprint; anything else means the report is not what this gate parses." >&2
  exit 2
fi

if [ "$NEW" != "0" ]; then
  echo "SELF-LINT VIOLATION: $NEW finding(s) not in $GATE_BASELINE:" >&2
  report_lines "$EV/gate.out" | head -20 | sed 's/^/        /' >&2
  [ "$NEW" -gt 20 ] && echo "        ... $((NEW - 20)) more, all in $EV/gate.out" >&2
  if [ "$CANON_MISMATCH" = "1" ]; then
    echo "  These are not new: $GATE_BASELINE was taken under a different staging path" >&2
    echo "  than $CANON, and a fingerprint bakes the absolute path in, so not one of" >&2
    echo "  them could match. Do NOT fix them and do NOT refresh — unset" >&2
    echo "  FPSONAR_SELFLINT_CANON so the gate stages where the baseline was taken." >&2
  elif [ "$REFRESH" = "1" ]; then
    echo "  These are absent from the baseline this run just generated from the same" >&2
    echo "  tree, so the analysis is not reproducible. The candidate was NOT installed:" >&2
    echo "  $BASELINE is untouched." >&2
  else
    echo "  Fix them. The baseline is refreshed once per epic at the epic boundary," >&2
    echo "  not per change — refreshing it to clear a new finding is a rubber stamp." >&2
  fi
  exit 1
fi
if [ "$CANON_MISMATCH" = "1" ]; then
  echo "SELF-LINT GUARD: 0 new findings, yet no path in $GATE_BASELINE lies under $CANON." >&2
  echo "  A fingerprint bakes the absolute path in, so nothing could have matched and" >&2
  echo "  every finding should have been reported as new. The comparison did not happen." >&2
  exit 2
fi
# The CLI's own gate is neutralised by the all--1 thresholds in the config, so a
# non-zero exit here is something this script has not accounted for.
if [ "$rc" != "0" ]; then
  echo "SELF-LINT GUARD: 0 new findings but the CLI exited $rc." >&2
  echo "  The config's gate thresholds should all be -1; check $CONFIG." >&2
  exit 2
fi

if [ "$REFRESH" = "1" ]; then
  cp "$EV/baseline.json" "$BASELINE" || {
    echo "SELF-LINT GUARD: cannot write $BASELINE" >&2; exit 2; }
  echo "self-lint gate: wrote $BASELINE ($BASE_FPS fingerprint(s) for $TOTAL finding(s))"
  echo "self-lint gate: commit it unmodified — the gate then grandfathers exactly these."
  echo "self-lint gate OK (refreshed): $EXPECTED_TARGETS file(s) analyzed, $REGISTRY_RULES rule(s) enabled, $TOTAL finding(s) grandfathered as $BASE_FPS fingerprint(s), 0 new finding(s)"
else
  echo "self-lint gate OK: $EXPECTED_TARGETS file(s) analyzed, $REGISTRY_RULES rule(s) enabled, $TOTAL finding(s) in $BASE_FPS grandfathered fingerprint(s), 0 new finding(s)"
fi
exit 0
