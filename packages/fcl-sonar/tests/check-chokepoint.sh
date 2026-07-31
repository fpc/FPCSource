#!/usr/bin/env bash
# Guard: the vendored scanner/parser/resolver units may be imported only by their
# adapter unit. Every other fcl-sonar unit reaches them through that adapter.
# Exit: 0 clean | 1 violation | 2 the guard could not measure
set -uo pipefail
export LC_ALL=C          # byte collation: unit names contain dots, and sort
                         # order must match the fixed-string lookups below
cd "$(dirname "$(readlink -f "$0")")/../../.." || exit 2   # the FPC src root

SONAR_SRC="packages/fcl-sonar/src"
CLI_ROOT="utils"
CLI_SRC="$CLI_ROOT/fpsonar"
VENDOR_SRC="packages/fcl-passrc/src"
VENDOR_NS="packages/fcl-passrc/namespaced"

# Everything fpsonar ships: the library and the CLI that links it. Not tests —
# a test unit may legitimately drive the vendored scanner directly.
SEARCH_ROOTS=("$SONAR_SRC/base" "$SONAR_SRC/rules" "$SONAR_SRC/output" "$CLI_SRC")

PRUNE_NAMES=(backup lib units)

declare -A ADAPTER_FILE=(
  [ingest]="$SONAR_SRC/base/fpsonar.ingest.pp"
  [resolver]="$SONAR_SRC/base/fpsonar.resolver.pp"
  [useanalysis]="$SONAR_SRC/base/fpsonar.useanalysis.pp"
  [dataflow]="$SONAR_SRC/base/fpsonar.dataflow.pp"
)
declare -A ADAPTER_UNITS=(
  [ingest]="pscanner pparser pascal.scanner pascal.parser"
  [resolver]="pasresolver pasresolveeval pasnativeresolve
              pascal.resolver pascal.resolveeval pascal.resolver.native"
  [useanalysis]="pasuseanalyzer pascal.useanalyzer"
  [dataflow]="pasdataflow pascfg pascal.dataflow pascal.cfg"
)

# Deliberately shared: the AST is handed across the boundary by design.
SHARED_UNITS="pastree pascal.tree"

# Vendored units fpsonar has no adapter for and does not use. Naming one is a
# new dependency on the vendored tree: classify it above first.
FORBIDDEN_UNITS="paswrite passrcutil pastounittest dpktolpk
                 pascal.writer pascal.utils pascal.tounittest pascal.dpktolpk"

# pruned <path> -> true if its basename is a PRUNE_NAMES directory. Callers pass
# the directory itself, which is what both directory checks below enumerate.
pruned() {
  local b
  b="$(basename "$1")"
  printf '%s\n' "${PRUNE_NAMES[@]}" | grep -qxF "$b"
}

#--- Corpus -------------------------------------------------------------------
missing_roots=()
for r in "${SEARCH_ROOTS[@]}"; do
  [ -d "$r" ] || missing_roots+=("$r")
done
if [ ${#missing_roots[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: search root(s) missing under $PWD: ${missing_roots[*]}" >&2
  echo "The guard would have searched an incomplete corpus and passed vacuously." >&2
  echo "Update SEARCH_ROOTS in check-chokepoint.sh to the current layout." >&2
  exit 2
fi

# A new sibling of src/base is the other half of the same hazard: every root
# exists, nothing looks wrong, and the new directory is simply never searched.
mapfile -t src_dirs < <(find -L "$SONAR_SRC" -mindepth 1 -maxdepth 1 -type d | sort)
unsearched=()
for d in "${src_dirs[@]}"; do
  pruned "$d" && continue
  printf '%s\n' "${SEARCH_ROOTS[@]}" | grep -qxF "$d" || unsearched+=("$d")
done
if [ ${#unsearched[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: directory under $SONAR_SRC/ not covered by SEARCH_ROOTS: ${unsearched[*]}" >&2
  echo "Units there would never be searched, so the guard would pass vacuously for them." >&2
  exit 2
fi

# The CLI half of the corpus names one tree explicitly, so a second fpsonar tool
# tree beside it would be just as invisible as a new src/ sibling.
mapfile -t cli_dirs < <(find -L "$CLI_ROOT" -mindepth 1 -maxdepth 1 -type d -name 'fpsonar*' | sort)
unsearched=()
for d in "${cli_dirs[@]}"; do
  printf '%s\n' "${SEARCH_ROOTS[@]}" | grep -qxF "$d" || unsearched+=("$d")
done
if [ ${#unsearched[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: fpsonar tree under $CLI_ROOT/ not covered by SEARCH_ROOTS: ${unsearched[*]}" >&2
  echo "Shipped code there would never be searched. Add it to SEARCH_ROOTS." >&2
  exit 2
fi

# A unit dropped directly into src/ belongs to no classified root.
mapfile -t stray < <(find -L "$SONAR_SRC" -maxdepth 1 -type f \
  \( -name '*.pp' -o -name '*.pas' -o -name '*.inc' \) | sort)
if [ ${#stray[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: unit(s) directly under $SONAR_SRC/, outside every search root:" >&2
  printf '  %s\n' "${stray[@]}" >&2
  echo "Move them into base/, rules/ or output/, or add their directory to SEARCH_ROOTS." >&2
  exit 2
fi

prune=()
for n in "${PRUNE_NAMES[@]}"; do prune+=(-name "$n" -o); done

# files_under <root> -> the searchable sources under it, pruned.
files_under() {
  find -L "$1" \( "${prune[@]}" -false \) -prune -o \
    -type f \( -name '*.pp' -o -name '*.pas' -o -name '*.inc' \) -print | sort
}

# Per root, not just in aggregate: a root that exists but holds no source leaves
# everything it should have contained unpoliced while the total stays non-zero.
empty_roots=()
CORPUS=()
for r in "${SEARCH_ROOTS[@]}"; do
  mapfile -t part < <(files_under "$r")
  if [ ${#part[@]} -eq 0 ]; then
    empty_roots+=("$r")
  else
    CORPUS+=("${part[@]}")
  fi
done
if [ ${#empty_roots[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: search root(s) hold no *.pp/*.pas/*.inc: ${empty_roots[*]}" >&2
  echo "Whatever belongs there is unpoliced, so this is a failure, not a clean tree." >&2
  exit 2
fi

if [ ${#CORPUS[@]} -eq 0 ]; then
  echo "CHOKEPOINT GUARD: empty search corpus — no *.pp/*.pas/*.inc under ${SEARCH_ROOTS[*]}" >&2
  exit 2
fi

unreadable=()
for f in "${CORPUS[@]}"; do
  [ -r "$f" ] || unreadable+=("$f")
done
if [ ${#unreadable[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: corpus file(s) not readable, so they cannot be policed:" >&2
  printf '  %s\n' "${unreadable[@]}" >&2
  exit 2
fi

for a in "${!ADAPTER_UNITS[@]}"; do
  [ -n "${ADAPTER_FILE[$a]+x}" ] || {
    echo "CHOKEPOINT GUARD: adapter '$a' has ADAPTER_UNITS but no ADAPTER_FILE entry." >&2; exit 2; }
done
for a in "${!ADAPTER_FILE[@]}"; do
  [ -n "${ADAPTER_UNITS[$a]+x}" ] || {
    echo "CHOKEPOINT GUARD: adapter '$a' has ADAPTER_FILE but no ADAPTER_UNITS entry." >&2; exit 2; }
done
# Derived from the table, never restated: a hand-written loop list would let a
# newly added adapter count as classified while never being policed.
mapfile -t ADAPTERS < <(printf '%s\n' "${!ADAPTER_UNITS[@]}" | sort)

for t in "$VENDOR_SRC" "$VENDOR_NS"; do
  [ -d "$t" ] && continue
  echo "CHOKEPOINT GUARD: vendored tree missing: $t" >&2
  echo "Its units would escape the exhaustiveness check, so a rule could name one freely." >&2
  echo "Repoint VENDOR_SRC / VENDOR_NS in check-chokepoint.sh at the current layout." >&2
  exit 2
done

# A vendored unit in a subdirectory is invisible to the -maxdepth 1 scan below.
mapfile -t vendor_dirs < <(find -L "$VENDOR_SRC" "$VENDOR_NS" -mindepth 1 -maxdepth 1 -type d | sort)
unscanned=()
for d in "${vendor_dirs[@]}"; do
  pruned "$d" || unscanned+=("$d")
done
if [ ${#unscanned[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: vendored subdirectory not scanned for units: ${unscanned[*]}" >&2
  echo "Units there are never classified, so naming one would never be a violation." >&2
  echo "Add its name to PRUNE_NAMES if it is not source, or widen the vendored scan." >&2
  exit 2
fi

# Unit names as the compiler sees them: filename minus extension, lowercased.
mapfile -t VENDORED < <(
    find -L "$VENDOR_SRC" "$VENDOR_NS" -maxdepth 1 -type f \
      \( -name '*.pp' -o -name '*.pas' \) -printf '%f\n' \
    | sed 's/\.[^.]*$//' | tr 'A-Z' 'a-z' | sort -u )

if [ ${#VENDORED[@]} -eq 0 ]; then
  echo "CHOKEPOINT GUARD: no vendored units found under $VENDOR_SRC — nothing to police." >&2
  exit 2
fi

CLASSIFIED="$SHARED_UNITS $FORBIDDEN_UNITS"
for a in "${ADAPTERS[@]}"; do CLASSIFIED="$CLASSIFIED ${ADAPTER_UNITS[$a]}"; done

declare -A CLASS_OF=()
dup=()
class_add() {                                  # class_add <class label> <unit...>
  local label="$1" u; shift
  for u in "$@"; do
    [ -z "${CLASS_OF[$u]:-}" ] || dup+=("$u: ${CLASS_OF[$u]} and $label")
    CLASS_OF[$u]="$label"
  done
}
for a in "${ADAPTERS[@]}"; do class_add "adapter $a" ${ADAPTER_UNITS[$a]}; done
class_add "SHARED_UNITS" $SHARED_UNITS
class_add "FORBIDDEN_UNITS" $FORBIDDEN_UNITS
if [ ${#dup[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: vendored unit(s) classified twice in check-chokepoint.sh:" >&2
  printf '  %s\n' "${dup[@]}" >&2
  echo "Each unit belongs to exactly one class; the checks contradict each other otherwise." >&2
  exit 2
fi

unclassified=()
for v in "${VENDORED[@]}"; do
  printf '%s\n' $CLASSIFIED | grep -qxF "$v" || unclassified+=("$v")
done
if [ ${#unclassified[@]} -gt 0 ]; then
  echo "CHOKEPOINT GUARD: vendored unit(s) not classified in check-chokepoint.sh: ${unclassified[*]}" >&2
  echo "Add each to an adapter's ADAPTER_UNITS, to SHARED_UNITS, or to FORBIDDEN_UNITS." >&2
  echo "Until then the guard cannot say whether importing it is allowed." >&2
  exit 2
fi

importers_of() { grep -liwF --binary-files=text "$1" "${CORPUS[@]}"; }

# names_in_code <file> <unit> -> true if the file names the unit outside comments
# and string literals
names_in_code() {
  awk -v want="$(printf '%s' "$2" | tr 'A-Z' 'a-z')" '
  function strip(s,   n,i,c,d,j,out) {
    n=length(s); i=1; out=""
    while (i<=n) {
      c=substr(s,i,1); d=substr(s,i,2)
      if (d=="//") { while (i<=n && substr(s,i,1)!="\n") i++; continue }
      if (c=="{")  { j=index(substr(s,i),"}");    if (j==0) i=n+1; else i=i+j;   out=out " "; continue }
      if (d=="(*") { j=index(substr(s,i+2),"*)"); if (j==0) i=n+1; else i=i+3+j; out=out " "; continue }
      if (c=="\047") {
        i++
        while (i<=n) {
          if (substr(s,i,1)=="\047") {
            if (substr(s,i+1,1)=="\047") { i+=2; continue }
            i++; break
          }
          i++
        }
        out=out " "; continue
      }
      out=out c; i++
    }
    return out
  }
  { all = all $0 "\n" }
  END {
    t = tolower(strip(all))
    gsub(/[^a-z0-9_.]+/, " ", t)               # unit names are dotted words
    exit (index(" " t " ", " " want " ") ? 0 : 1)
  }
  ' "$1"
}

# An adapter may split its vendored usage into a companion include file.
belongs_to_adapter() {                          # belongs_to_adapter <file> <adapter file>
  local f="${1,,}" a="${2,,}"
  [ "$f" = "$a" ] || [ "$f" = "${a%.*}.inc" ]
}

# adapter_on_disk <adapter file> -> the corpus entry for it, whatever its case.
adapter_on_disk() {
  local a="${1,,}" f
  for f in "${CORPUS[@]}"; do
    [ "${f,,}" = "$a" ] && { printf '%s\n' "$f"; return 0; }
  done
  return 1
}

fail=0
for a in "${ADAPTERS[@]}"; do
  adapter="${ADAPTER_FILE[$a]}"
  on_disk="$(adapter_on_disk "$adapter" || true)"
  owned=()
  named=()
  future=()
  offenders=()
  contradictory=()

  for unit in ${ADAPTER_UNITS[$a]}; do
    in_tree=0
    if printf '%s\n' "${VENDORED[@]}" | grep -qxF "$unit"; then
      owned+=("$unit"); in_tree=1
    else
      future+=("$unit")
    fi
    while read -r f; do
      [ -n "$f" ] || continue
      names_in_code "$f" "$unit" || continue
      if belongs_to_adapter "$f" "$adapter"; then
        named+=("$unit")
        [ "$in_tree" = "1" ] || \
          contradictory+=("$unit — imported by $f, absent from the vendored tree")
      else
        offenders+=("$unit -> $f")
      fi
    done < <(importers_of "$unit")
  done

  if [ ${#offenders[@]} -gt 0 ]; then
    echo "CHOKEPOINT VIOLATION: vendored unit named outside the $a adapter:" >&2
    printf '  %s\n' "${offenders[@]}" >&2
    echo "  Only $adapter may name it; everything else goes through its API." >&2
    fail=1
  fi

  # An adapter cannot import a unit that is not in the vendored tree: the source
  # would not compile. Seeing both means the inventory above went half-blind.
  if [ ${#contradictory[@]} -gt 0 ]; then
    echo "CHOKEPOINT GUARD: adapter/inventory contradiction in the $a adapter:" >&2
    printf '  %s\n' "${contradictory[@]}" >&2
    echo "  The vendored inventory is not seeing units that demonstrably exist." >&2
    echo "  Check VENDOR_SRC / VENDOR_NS and PRUNE_NAMES in check-chokepoint.sh." >&2
    fail=1
  fi

  # An adapter that exists must still name at least one unit it owns; if it
  # names none, either the vendored unit was renamed or the adapter dropped it.
  if [ -n "$on_disk" ] && [ ${#named[@]} -eq 0 ]; then
    echo "CHOKEPOINT GUARD STALE: adapter $on_disk names none of ${ADAPTER_UNITS[$a]}." >&2
    echo "  The table in check-chokepoint.sh must be updated to the current unit names." >&2
    fail=1
  fi

  if [ -n "$on_disk" ]; then state="present as $on_disk"; else state="not yet written"; fi
  printf '  %-12s adapter %s (%s), owns %d vendored unit(s), names %d' \
    "$a" "$adapter" "$state" "${#owned[@]}" "${#named[@]}"
  [ ${#future[@]} -eq 0 ] || printf ', %d not yet in the vendored tree: %s' \
    "${#future[@]}" "${future[*]}"
  printf '\n'
done

# Forbidden: no permitted importer at all.
for unit in $FORBIDDEN_UNITS; do
  while read -r f; do
    [ -n "$f" ] || continue
    names_in_code "$f" "$unit" || continue
    echo "CHOKEPOINT VIOLATION: '$unit' has no adapter and must not be named: $f" >&2
    echo "  If fpsonar now needs it, classify it in check-chokepoint.sh first." >&2
    fail=1
  done < <(importers_of "$unit")
done

[ "$fail" = "0" ] || exit 1
echo "chokepoint OK: ${#VENDORED[@]} vendored units classified, ${#CORPUS[@]} files searched under ${SEARCH_ROOTS[*]}"
exit 0
