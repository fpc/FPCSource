#!/bin/sh
# Fetches the W3C SVG 1.1 test suite into tests/suites/w3c.
# The suite is not part of this tree; its licence keeps it out.
# Run from the repository root.

set -e

ARCHIVE_URL=https://www.w3.org/Graphics/SVG/Test/20110816/archives/W3C_SVG_11_TestSuite.tar.gz
TARGET=${1:-tests/suites/w3c}
WORK=$TARGET/.download

if [ -d "$TARGET/svg" ] && [ -d "$TARGET/png" ]; then
  echo "The suite is already in $TARGET."
  echo "Remove that directory to fetch it again."
  exit 0
fi

mkdir -p "$WORK"

if command -v curl > /dev/null 2>&1; then
  curl -L -o "$WORK/suite.tar.gz" "$ARCHIVE_URL"
elif command -v wget > /dev/null 2>&1; then
  wget -O "$WORK/suite.tar.gz" "$ARCHIVE_URL"
else
  echo "Neither curl nor wget is installed." >&2
  exit 1
fi

tar -xzf "$WORK/suite.tar.gz" -C "$WORK"

# Every document reaches outside svg/ for an image, a font or another
# document, so the whole tree is kept, not the two directories compared.
ROOT=`find "$WORK" -maxdepth 3 -type d -name svg | head -1`
if [ -z "$ROOT" ]; then
  echo "The archive holds no svg directory." >&2
  exit 1
fi
ROOT=`dirname "$ROOT"`
for PART in "$ROOT"/*; do
  NAME=`basename "$PART"`
  if [ "$NAME" = "suite.tar.gz" ]; then
    continue
  fi
  rm -rf "$TARGET/$NAME"
  mv "$PART" "$TARGET/$NAME"
done

rm -rf "$WORK"

# The archive leaves out the web fonts its font-face rules name, so they are
# fetched one at a time from the same place the archive came from.
FONTBASE=`dirname "$ARCHIVE_URL"`
FONTBASE=`dirname "$FONTBASE"`/svg/woffs
mkdir -p "$TARGET/svg/woffs"
FONTS=0
for NAME in `grep -ohE "url\([^)]*\.woff\)" "$TARGET"/svg/*.svg \
             | sed 's|url(||;s|)||' | tr -d "'\"" | sed 's|.*/||' | sort -u`; do
  if [ -f "$TARGET/svg/woffs/$NAME" ]; then
    FONTS=`expr $FONTS + 1`
    continue
  fi
  if command -v curl > /dev/null 2>&1; then
    curl -sf -o "$TARGET/svg/woffs/$NAME" "$FONTBASE/$NAME" || rm -f "$TARGET/svg/woffs/$NAME"
  else
    wget -q -O "$TARGET/svg/woffs/$NAME" "$FONTBASE/$NAME" || rm -f "$TARGET/svg/woffs/$NAME"
  fi
  if [ -f "$TARGET/svg/woffs/$NAME" ]; then
    FONTS=`expr $FONTS + 1`
  else
    echo "  the web font $NAME could not be fetched; documents using it will"
    echo "  fall back to a system face."
  fi
done

echo "Fetched `ls "$TARGET/svg" | wc -l` documents, `ls "$TARGET/png" | wc -l` references and $FONTS web fonts into $TARGET."
echo "The suite is published by the W3C under its own licence; read the"
echo "terms that came with the archive before redistributing any of it."
