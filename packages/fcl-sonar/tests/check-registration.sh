#!/usr/bin/env bash
# Guard: test_fpsonar.pp and fpsonar_tests.lpr must list the identical unit set,
# and every test unit on disk must be registered in both.
# Exit: 0 clean | 1 mismatch | 2 the guard could not measure
set -uo pipefail
export LC_ALL=C          # byte collation: unit names contain dots, and comm must
                         # agree with the sort order its inputs were built in
cd "$(dirname "$(readlink -f "$0")")" || exit 2   # .../packages/fcl-sonar/tests

RUNNER_A="test_fpsonar.pp"
RUNNER_B="fpsonar_tests.lpr"
REQUIRED_UNIT="consoletestrunner"             # both runners are fpcunit console runners
# Every registrar fcl-fpcunit's testregistry exports: a unit calling any of them
# contributes tests, so registering in only one runner loses them.
REGISTRAR='Register(Test|Tests|TestDecorator)[[:space:]]*\('

WORK="$(mktemp -d)" || { echo "REGISTRATION GUARD: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$WORK"' EXIT

# stripped <file>: the file as one line with // { } (* *) comments removed,
stripped() {
  awk '
  function strip(s,   n,i,c,d,j,out,marker) {
    n=length(s); i=1; out=""
    while (i<=n) {
      c=substr(s,i,1); d=substr(s,i,2)
      if (d=="//") { while (i<=n && substr(s,i,1)!="\n") i++; continue }
      if (c=="{") {
        marker=(substr(s,i+1,1)=="$")
        j=index(substr(s,i),"}")
        if (j==0) i=n+1; else i=i+j
        out=out (marker ? " @DIRECTIVE@ " : " ")
        continue
      }
      if (d=="(*") {
        j=index(substr(s,i+2),"*)")
        if (j==0) i=n+1; else i=i+3+j
        out=out " "
        continue
      }
      if (c=="\047") {                       # string literal: blank the body
        i++
        while (i<=n) {
          if (substr(s,i,1)=="\047") {
            if (substr(s,i+1,1)=="\047") { i+=2; continue }   # doubled quote
            i++; break
          }
          i++
        }
        out=out "\047@STR@\047"
        continue
      }
      out=out c; i++
    }
    return out
  }
  { all = all $0 "\n" }
  END { t=strip(all); gsub(/[ \t\r\n]+/," ",t); print t }
  ' "$1"
}

# clause_of <file>: prints the file's uses clause(s), one per line.
clause_of() {
  stripped "$1" | awk '{
    t=$0
    while (match(t,/(^|[^A-Za-z0-9_.])[Uu][Ss][Ee][Ss][^A-Za-z0-9_.]/)) {
      t=substr(t,RSTART+RLENGTH-1)
      j=index(t,";")
      if (j==0) { print "@UNTERMINATED@"; exit }
      print substr(t,1,j-1)
      t=substr(t,j+1)
    }
  }'
}

# uses_units <file> -> "lowercase<TAB>as-written", one unit per line, sorted.
uses_units() {
  clause_of "$1" \
  | tr ',' '\n' \
  | sed "s/[[:space:]]\+[Ii][Nn][[:space:]]*'[^']*'//" \
  | sed 's/[[:space:]]//g' \
  | grep -v '^$' \
  | awk '{ print tolower($0) "\t" $0 }' \
  | sort -u
}

# registers <file>: true if it calls a registrar in code — the stripped text, so
registers() { stripped "$1" | grep -qiE "$REGISTRAR"; }

COUNT=0                                        # set by read_runner

read_runner() {                                # read_runner <file> <outprefix>
  local f="$1" p="$2" clauses
  if [ ! -f "$f" ]; then
    echo "REGISTRATION GUARD: runner not found: $PWD/$f" >&2
    return 1
  fi

  clauses="$(clause_of "$f" | grep -c .)"
  if [ "$clauses" != "1" ]; then
    echo "REGISTRATION GUARD: found $clauses uses clauses in $f, expected exactly 1." >&2
    echo "  The guard compares one clause per runner; it cannot see units in another." >&2
    return 1
  fi

  uses_units "$f" > "$WORK/$p.map"
  cut -f1 "$WORK/$p.map" > "$WORK/$p.units"
  COUNT="$(grep -c . "$WORK/$p.units")"

  # Both sentinels are matched lower case: uses_units lowercases column 1.
  if grep -q '@unterminated@' "$WORK/$p.units"; then
    echo "REGISTRATION GUARD: the uses clause of $f has no terminating semicolon." >&2
    return 1
  fi
  if grep -q '@directive@' "$WORK/$p.units"; then
    echo "REGISTRATION GUARD: a compiler directive sits inside the uses clause of $f." >&2
    echo "  Conditionally registered units cannot be compared by text; register them" >&2
    echo "  unconditionally, or teach this guard the condition deliberately." >&2
    return 1
  fi
  if [ "$COUNT" -eq 0 ]; then
    echo "REGISTRATION GUARD: no uses clause extracted from $f — the guard cannot see anything" >&2
    return 1
  fi
  if ! grep -qx "$REQUIRED_UNIT" "$WORK/$p.units"; then
    echo "REGISTRATION GUARD: '$REQUIRED_UNIT' absent from the uses clause read out of $f" >&2
    echo "  Either the runner is no longer an fpcunit console runner, or the clause was misparsed." >&2
    return 1
  fi

  # Every token must look like a unit name; anything else means a misparse.
  local bad
  bad="$(grep -vE '^[a-z_][a-z0-9_.]*$' "$WORK/$p.units" || true)"
  if [ -n "$bad" ]; then
    echo "REGISTRATION GUARD: unparsable token(s) in the uses clause of $f:" >&2
    printf '  %s\n' "$bad" >&2
    return 1
  fi
  return 0
}

read_runner "$RUNNER_A" a || exit 2
NA="$COUNT"
read_runner "$RUNNER_B" b || exit 2
NB="$COUNT"

as_written() {                                 # as_written <mapfile> <lowername>
  awk -F'\t' -v u="$2" '$1 == u { print $2; exit }' "$1"
}

fail=0

#--- A against B --------------------------------------------------------------
only_a="$(comm -23 "$WORK/a.units" "$WORK/b.units")"
only_b="$(comm -13 "$WORK/a.units" "$WORK/b.units")"

if [ -n "$only_a" ]; then
  echo "REGISTRATION ASYMMETRY: registered in $RUNNER_A but MISSING from $RUNNER_B:" >&2
  while read -r u; do
    [ -n "$u" ] && echo "  $(as_written "$WORK/a.map" "$u")  -> add it to $RUNNER_B" >&2
  done <<< "$only_a"
  fail=1
fi
if [ -n "$only_b" ]; then
  echo "REGISTRATION ASYMMETRY: registered in $RUNNER_B but MISSING from $RUNNER_A:" >&2
  while read -r u; do
    [ -n "$u" ] && echo "  $(as_written "$WORK/b.map" "$u")  -> add it to $RUNNER_A" >&2
  done <<< "$only_b"
  fail=1
fi
[ "$fail" = "0" ] || \
  echo "$RUNNER_A lists $NA units, $RUNNER_B lists $NB — a unit in only one never runs in the other." >&2

PRUNE_NAMES=(backup lib units)
prune=()
for n in "${PRUNE_NAMES[@]}"; do prune+=(-name "$n" -o); done

mapfile -t disk_files < <(
  find -L . \( "${prune[@]}" -false \) -prune -o \
    -type f \( -name '*.pp' -o -name '*.pas' \) -print | sed 's|^\./||' | sort)

: > "$WORK/disk.raw"
for f in "${disk_files[@]}"; do
  if [ "$f" = "$RUNNER_A" ] || [ "$f" = "$RUNNER_B" ]; then
    continue
  fi
  # An unreadable unit yields no text, so registers() would call it a fixture
  # and its absence from both runners would never be reported.
  if [ ! -r "$f" ]; then
    echo "REGISTRATION GUARD: test unit not readable, so it cannot be classified: $f" >&2
    echo "  Left alone it would silently count as a non-registering fixture." >&2
    exit 2
  fi
  # The unit name the runners must list is the basename: a Pascal uses clause
  # never carries the directory.
  registers "$f" && { b="${f##*/}"; printf '%s\n' "${b%.*}" >> "$WORK/disk.raw"; }
done
tr 'A-Z' 'a-z' < "$WORK/disk.raw" | sort -u > "$WORK/disk.units"

NDISK="$(grep -c . "$WORK/disk.units")"
if [ "$NDISK" -eq 0 ]; then
  echo "REGISTRATION GUARD: no unit beside the runners calls a registrar —" >&2
  echo "  the cross-check against disk has nothing to compare and would pass vacuously." >&2
  exit 2
fi

unregistered="$(comm -23 "$WORK/disk.units" <(sort -u "$WORK/a.units" "$WORK/b.units"))"
if [ -n "$unregistered" ]; then
  echo "UNREGISTERED TEST UNIT(S): they call a registrar but no runner lists them:" >&2
  while read -r u; do
    [ -n "$u" ] && echo "  $u  -> add it to BOTH $RUNNER_A and $RUNNER_B" >&2
  done <<< "$unregistered"
  echo "A unit missing from both runners is symmetric, so the A/B diff cannot see it." >&2
  fail=1
fi

[ "$fail" = "0" ] || exit 1
echo "registration OK: $NA units listed identically in $RUNNER_A and $RUNNER_B; all $NDISK test units on disk registered"
exit 0
