#!/bin/sh
# Збирає тестовий раннер vcl-compat з анотаціями для valgrind (-dFPC_VALGRIND).
# Анотації змінюють inline-методи TSpinLock (інтерфейс syncobjs), тому
# fcl-base і всі пакети, що від нього залежать, компілюються з джерел.
# -gw: valgrind 3.22 падає (Assertion in_rx) на налагоджувальній інформації за замовчуванням.
# Використання: build-testcompat-valgrind.sh [вихідна_тека]
OUT="${1:-$(dirname "$0")/out-valgrind}"
FPC_SRC_PACKAGES="vcl-compat fcl-base fcl-hash fcl-json fcl-web fcl-xml hash libpcre rtl-extra rtl-generics rtl-objpas wasm-utils fcl-fpcunit" \
  exec "$(dirname "$0")/build-testcompat.sh" "$OUT" -dFPC_VALGRIND -gw
