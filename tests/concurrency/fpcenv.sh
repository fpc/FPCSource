#!/bin/sh
# Середовище для компіляції тестів гілки feature/concurrency
# свіжозібраним компілятором trunk і юнітами з дерева джерел.
# Використання: . tests/concurrency/fpcenv.sh ; $FPC_TRUNK @"$FPC_CFG" prog.pp
ROOT=$(cd "$(dirname "$0")/../.." 2>/dev/null && pwd)
[ -x "$ROOT/compiler/ppcx64" ] || ROOT=$(git rev-parse --show-toplevel)
TARGET=x86_64-linux
FPC_SRC_PACKAGES="${FPC_SRC_PACKAGES:-vcl-compat}"
FPC_TRUNK="$ROOT/compiler/ppcx64"
FPC_CFG="${TMPDIR:-/tmp}/fpc-concurrency-$(id -u).cfg"
{
  echo "-n"
  echo "-Fu$ROOT/rtl/units/$TARGET"
  # Пакети, які ми змінюємо, компілюються з джерел (FPC_SRC_PACKAGES),
  # тому їхні старі скомпільовані юніти до шляху пошуку не додаються.
  for d in "$ROOT"/packages/*/units/$TARGET; do
    pkg=$(basename "$(dirname "$(dirname "$d")")")
    case " $FPC_SRC_PACKAGES " in *" $pkg "*) continue ;; esac
    echo "-Fu$d"
  done
  for pkg in $FPC_SRC_PACKAGES; do echo "-Fu$ROOT/packages/$pkg/src"; done
  gccdir=$(dirname "$(gcc -print-libgcc-file-name 2>/dev/null)")
  [ -n "$gccdir" ] && echo "-Fl$gccdir"
} > "$FPC_CFG"
export ROOT FPC_TRUNK FPC_CFG
