#!/bin/sh
# Збирає тестовий раннер vcl-compat з анотаціями для valgrind (-dFPC_VALGRIND).
# Анотації змінюють inline-методи TSpinLock (інтерфейс syncobjs), тому
# fcl-base і всі пакети, що від нього залежать, компілюються з джерел.
# Використання: build-testcompat-valgrind.sh [вихідна_тека]
set -e
OUT="${1:-$(dirname "$0")/out-valgrind}"
FPC_SRC_PACKAGES="vcl-compat fcl-base fcl-hash fcl-json fcl-web fcl-xml hash libpcre rtl-extra rtl-generics rtl-objpas wasm-utils fcl-fpcunit" \
  "$(dirname "$0")/build-testcompat.sh" "$OUT" -dFPC_VALGRIND -gw

# FPC (-gw) експортує мітки налагоджувальної інформації як глобальні символи
# секцій .debug_*; їхні значення — зміщення в цих секціях, а не адреси.
# valgrind 3.22 сприймає їх як адреси й, коли значення потрапляє в діапазон
# коду, падає з "Assertion 'in_rx' failed". Прибираємо ці символи з таблиці.
BIN="$OUT/testcompat"
DBG=$(readelf -SW "$BIN" | awk '$2 ~ /^\.debug/ {gsub(/[][]/,"",$1); print $1}' | paste -sd'|')
LIST=$(mktemp)
readelf -sW "$BIN" | awk -v re="^($DBG)\$" '$7 ~ re {print $8}' > "$LIST"
objcopy --strip-symbols="$LIST" "$BIN"
echo "Прибрано символів налагоджувальних секцій: $(wc -l < "$LIST")"
rm -f "$LIST"
