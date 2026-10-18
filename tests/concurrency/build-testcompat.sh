#!/bin/sh
# Збирає тестовий раннер vcl-compat (включно з тестами System.Threading)
# з поточних джерел гілки. Використання: build-testcompat.sh [вихідна_тека] [додаткові опції]
set -e
. "$(dirname "$0")/fpcenv.sh"
OUT="${1:-$ROOT/tests/concurrency/out}"
[ $# -gt 0 ] && shift
mkdir -p "$OUT"
rm -f "$OUT"/*.ppu "$OUT"/*.o
"$FPC_TRUNK" @"$FPC_CFG" -gl -Sa "$@" -FU"$OUT" -FE"$OUT" \
  "$ROOT/packages/vcl-compat/tests/testcompat.lpr"
rm -f "$FPC_CFG"
echo "Готово: $OUT/testcompat"
