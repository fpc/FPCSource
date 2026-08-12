#!/bin/sh
# Reproduce FPC compiler AV: a generic from Generics.Collections specialized
# on a procedure-local nested record type produces a broken specialization
# in the PPU; loading that PPU during a recompile crashes the wasm32 backend
# in get_para_push_size (wasm32/wasmdef.pas) with a nil returndef on the
# transitively-specialized TEnumerator<T>.DoGetCurrent.
# See issue 41774
#
# Usage: ./testcycle.sh [compiler]
#   compiler defaults to ppcrosswasm32

set -e

CC="${1:-ppcrosswasm32}"
cd "$(dirname "$0")"

echo "=== Compiler: $CC ==="
"$CC" -iV 2>/dev/null || true

echo
echo "=== Step 1: clean ==="
rm -f *.ppu *.o treload41774 treload41774.wasm
ls *.ppu *.o 2>/dev/null && { echo "FAIL: leftover artifacts"; exit 2; }
echo "OK"

echo
echo "=== Step 2: clean build (expect success) ==="
if "$CC" treload41774.pp; then
  echo "OK: clean build succeeded"
else
  echo "FAIL: clean build failed (exit $?)"
  exit 2
fi

echo
echo "=== Step 3: touch unitc.pp to force recompile ==="
sleep 1
touch unitc.pp
echo "OK"

echo
echo "=== Step 4: recompile (expect AV — this is the bug) ==="
set +e
"$CC" treload41774.pp
rc=$?
set -e

echo
if [ "$rc" -ne 0 ]; then
  echo "Issue 41774 confirmed: recompile failed with exit $rc"
  exit 1
else
  echo "Issue 41774 corrected: recompile succeeded"
  exit 0
fi
