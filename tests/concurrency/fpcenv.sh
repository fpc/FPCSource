#!/bin/sh
# Середовище для компіляції тестів гілки feature/concurrency
# свіжозібраним компілятором trunk і юнітами з дерева джерел.
# Використання: . tests/concurrency/fpcenv.sh ; $FPC_TRUNK @"$FPC_CFG" prog.pp
ROOT=$(cd "$(dirname "$0")/../.." 2>/dev/null && pwd)
[ -x "$ROOT/compiler/ppcx64" ] || ROOT=$(git rev-parse --show-toplevel)
TARGET=x86_64-linux
FPC_SRC_PACKAGES="${FPC_SRC_PACKAGES:-fcl-eventloop vcl-compat fcl-base fcl-hash fcl-json fcl-web fcl-xml hash libpcre rtl-generics rtl-objpas wasm-utils fcl-fpcunit}"
FPC_TRUNK="$ROOT/compiler/ppcx64"
# Окремий файл для кожного запуску: паралельні збірки не заважають одна одній.
FPC_CFG=$(mktemp "${TMPDIR:-/tmp}/fpc-concurrency-XXXXXX.cfg")
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
  # Усі підтеки src (деякі пакети, як fcl-web, розкладені по src/base, src/http2...),
  # крім платформних тек інших ОС.
  for pkg in $FPC_SRC_PACKAGES; do
    [ -d "$ROOT/packages/$pkg/src" ] || continue
    find "$ROOT/packages/$pkg/src" -type d \
      ! -path "*/win*" ! -path "*/darwin*" ! -path "*/bsd*" ! -path "*/amiga*" \
      ! -path "*/wasi*" ! -path "*/os2*" ! -path "*/msdos*" ! -path "*/go32v2*" |
    while read -r d; do echo "-Fu$d"; echo "-Fi$d"; done
  done
  gccdir=$(dirname "$(gcc -print-libgcc-file-name 2>/dev/null)")
  [ -n "$gccdir" ] && echo "-Fl$gccdir"
} > "$FPC_CFG"
export ROOT FPC_TRUNK FPC_CFG
