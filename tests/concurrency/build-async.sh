#!/bin/sh
# Збірка модульних тестів fcl-async (Фаза 4)
set -e
ROOT=$(cd "$(dirname "$0")/../.." 2>/dev/null && pwd)
OUT="${1:-$(dirname "$0")/out-async}"
mkdir -p "$OUT"
. "$ROOT/tests/concurrency/fpcenv.sh"
set -x
$FPC_TRUNK @"$FPC_CFG" \
  -FE"$OUT" \
  -FU"$OUT" \
  -Fi"$ROOT/packages/fcl-async/src" \
  -Fu"$ROOT/packages/fcl-async/src" \
  -Fu"$ROOT/packages/fcl-async/tests" \
  -dDEBUG \
  "$ROOT/packages/fcl-async/tests/testasync.lpr"
set +x
echo "Готово: $OUT/testasync"

