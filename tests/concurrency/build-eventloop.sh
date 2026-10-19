#!/bin/sh
# Збирає тестовий раннер fcl-eventloop з поточних джерел гілки.
# Використання: build-eventloop.sh [вихідна_тека] [додаткові опції]
set -e
. "$(dirname "$0")/fpcenv.sh"
OUT="${1:-$ROOT/tests/concurrency/out-eventloop}"
[ $# -gt 0 ] && shift
mkdir -p "$OUT"
rm -f "$OUT"/*.ppu "$OUT"/*.o
"$FPC_TRUNK" @"$FPC_CFG" -gl -Sa "$@" -FU"$OUT" -FE"$OUT" \
  "$ROOT/packages/fcl-eventloop/tests/testeventloop.lpr"
rm -f "$FPC_CFG"
echo "Готово: $OUT/testeventloop"
